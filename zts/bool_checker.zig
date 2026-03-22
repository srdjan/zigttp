//! Strict Boolean Enforcement
//!
//! Static analysis pass that enforces boolean-typed values in boolean contexts:
//! - if/ternary conditions must be boolean
//! - && and || operands must be boolean
//! - ! operand must be boolean
//! - ?? LHS that is provably non-nullable triggers a warning
//!
//! Modeled on handler_verifier.zig: walks the IR tree inferring expression types.
//! When inferType returns .unknown, no diagnostic is emitted (escape hatch for
//! function calls, parameters, property accesses). Runtime VM assertions in the
//! interpreter catch those cases at execution time.

const std = @import("std");
const ir = @import("parser/ir.zig");
const object = @import("object.zig");
const context = @import("context.zig");

const Node = ir.Node;
const NodeIndex = ir.NodeIndex;
const NodeTag = ir.NodeTag;
const IrView = ir.IrView;
const null_node = ir.null_node;

// ---------------------------------------------------------------------------
// Expression type inference
// ---------------------------------------------------------------------------

pub const ExprType = enum(u8) {
    boolean, // true/false, comparisons, !(bool), &&/|| of bools
    number, // int/float literals, arithmetic, bitwise, unary -/+/~
    string, // string/template literals, typeof
    undefined, // undefined literal
    object, // object/array literals
    function, // function/arrow expressions
    unknown, // cannot determine statically (params, fn calls, let vars, property access)
    // Optional variants: T | undefined
    optional_string, // e.g. env(), parseBearer(), cacheGet()
    optional_object, // e.g. routerMatch()

    /// Returns true if this type is known to never be undefined.
    pub fn isNonNullable(self: ExprType) bool {
        return switch (self) {
            .boolean, .number, .string, .object, .function => true,
            else => false,
        };
    }

    /// Remove optionality from a type: optional_string -> string, etc.
    /// Returns .unknown for types that are purely undefined.
    pub fn removeNullish(self: ExprType) ExprType {
        return switch (self) {
            .optional_string => .string,
            .optional_object => .object,
            .undefined => .unknown,
            else => self,
        };
    }
};

/// Unify two types into a single type. Returns .unknown if they're incompatible.
/// Handles optional promotion: string + undefined -> optional_string, etc.
fn unifyTypes(a: ExprType, b: ExprType) ExprType {
    if (a == b) return a;
    // Optional promotion: T + undefined -> optional_T
    if (a == .undefined) return makeNullable(b);
    if (b == .undefined) return makeNullable(a);
    // optional_T + T -> optional_T
    if (a == .optional_string and b == .string) return .optional_string;
    if (a == .string and b == .optional_string) return .optional_string;
    if (a == .optional_object and b == .object) return .optional_object;
    if (a == .object and b == .optional_object) return .optional_object;
    return .unknown;
}

/// Promote a type to its optional variant. Returns .unknown for types without optional variants.
fn makeNullable(t: ExprType) ExprType {
    return switch (t) {
        .string, .optional_string => .optional_string,
        .object, .optional_object => .optional_object,
        .undefined => .undefined,
        else => .unknown,
    };
}

/// Merge a new return type into an accumulator. Returns null if incompatible.
fn mergeReturnType(current: ?ExprType, new: ExprType) ?ExprType {
    if (current) |c| {
        if (c == new) return c;
        const unified = unifyTypes(c, new);
        return if (unified == .unknown) null else unified;
    }
    return new;
}

pub fn packBindingKey(scope_id: ir.ScopeId, slot: u16) u32 {
    return (@as(u32, scope_id) << 16) | @as(u32, slot);
}

fn bindingKey(binding: ir.BindingRef) u32 {
    return packBindingKey(binding.scope_id, binding.slot);
}

// ---------------------------------------------------------------------------
// Diagnostic types
// ---------------------------------------------------------------------------

pub const Severity = enum {
    err,
    warning,

    pub fn label(self: Severity) []const u8 {
        return switch (self) {
            .err => "error",
            .warning => "warning",
        };
    }
};

pub const DiagnosticKind = enum {
    condition_not_boolean, // if/ternary condition is non-boolean
    logical_operand_not_boolean, // && or || operand is non-boolean
    not_operand_not_boolean, // ! operand is non-boolean
    nullish_on_non_nullable, // ?? LHS is provably non-nullable
};

pub const Diagnostic = struct {
    severity: Severity,
    kind: DiagnosticKind,
    node: NodeIndex,
    message: []const u8,
    help: ?[]const u8,
};

// ---------------------------------------------------------------------------
// BoolChecker
// ---------------------------------------------------------------------------

pub const BoolChecker = struct {
    allocator: std.mem.Allocator,
    ir_view: IrView,
    atoms: ?*context.AtomTable,
    diagnostics: std.ArrayList(Diagnostic),
    /// Tracks inferred type for const/let bindings: packed(scope_id, slot) -> ExprType
    const_types: std.AutoHashMapUnmanaged(u32, ExprType),
    /// Tracks inferred return type for function bindings: packed(scope_id, slot) -> ExprType
    fn_return_types: std.AutoHashMapUnmanaged(u32, ExprType),
    /// Branch-scoped type narrowings from typeof guards.
    /// Checked before const_types in inferType for ALL binding kinds.
    /// Entries are temporary: installed before walking a branch, removed after.
    narrowed_types: std.AutoHashMapUnmanaged(u32, ExprType),
    /// Virtual module import binding tracking: local slot -> return type.
    /// Populated by scanImports before the main walk.
    module_fn_types: std.AutoHashMapUnmanaged(u16, ExprType),
    /// Local slots bound to Result-producing module functions (jwtVerify, etc.).
    /// Used in walkStmt to detect `const r = jwtVerify(...)` and track result_bindings.
    module_result_fn_slots: std.AutoHashMapUnmanaged(u16, void),
    /// Binding slots that hold Result objects (from jwtVerify, validateJson, etc.).
    /// Used to infer result.ok as boolean. Key: packed(scope_id, slot).
    result_bindings: std.AutoHashMapUnmanaged(u32, void),

    const TypeofGuard = struct {
        binding_key: u32,
        narrowed_type: ExprType,
        op: ir.BinaryOp,
    };

    const MAX_NARROWINGS = 4;

    pub fn init(allocator: std.mem.Allocator, ir_view: IrView, atoms: ?*context.AtomTable) BoolChecker {
        return .{
            .allocator = allocator,
            .ir_view = ir_view,
            .atoms = atoms,
            .diagnostics = .empty,
            .const_types = .empty,
            .fn_return_types = .empty,
            .narrowed_types = .empty,
            .module_fn_types = .empty,
            .module_result_fn_slots = .empty,
            .result_bindings = .empty,
        };
    }

    pub fn deinit(self: *BoolChecker) void {
        // Free dynamically allocated diagnostic messages
        for (self.diagnostics.items) |diag| {
            if (isAllocatedMessage(diag.message)) {
                self.allocator.free(diag.message);
            }
        }
        self.diagnostics.deinit(self.allocator);
        self.const_types.deinit(self.allocator);
        self.fn_return_types.deinit(self.allocator);
        self.narrowed_types.deinit(self.allocator);
        self.module_fn_types.deinit(self.allocator);
        self.module_result_fn_slots.deinit(self.allocator);
        self.result_bindings.deinit(self.allocator);
    }

    /// Check if a diagnostic message was dynamically allocated vs static string.
    /// Allocated messages start with "always-truthy value (" and contain "' operator".
    fn isAllocatedMessage(msg: []const u8) bool {
        return msg.len > 30 and
            std.mem.startsWith(u8, msg, "always-truthy value (") and
            std.mem.endsWith(u8, msg, "' operator");
    }

    /// Run the checker on the given root node. Returns the number of errors.
    pub fn check(self: *BoolChecker, root: NodeIndex) !u32 {
        self.scanImports();
        self.walkStmt(root);
        var error_count: u32 = 0;
        for (self.diagnostics.items) |diag| {
            if (diag.severity == .err) error_count += 1;
        }
        return error_count;
    }

    pub fn getDiagnostics(self: *const BoolChecker) []const Diagnostic {
        return self.diagnostics.items;
    }

    // -----------------------------------------------------------------------
    // Diagnostic formatting (mirrors handler_verifier.zig)
    // -----------------------------------------------------------------------

    pub fn formatDiagnostics(
        self: *const BoolChecker,
        source: []const u8,
        writer: anytype,
    ) !void {
        for (self.diagnostics.items) |diag| {
            const loc = self.ir_view.getLoc(diag.node) orelse continue;

            // Header
            try writer.print("{s}: {s}\n", .{ diag.severity.label(), diag.message });

            // Location
            try writer.print("  --> {d}:{d}\n", .{ loc.line, loc.column });

            // Source context line
            if (getSourceLine(source, loc.line)) |line| {
                try writer.print("   |\n", .{});
                try writer.print("{d: >3} | {s}\n", .{ loc.line, line });
                try writer.print("   | ", .{});
                var col: u16 = 1;
                while (col < loc.column) : (col += 1) {
                    try writer.writeByte(' ');
                }
                try writer.writeAll("^\n");
            }

            // Help text
            if (diag.help) |help| {
                try writer.print("   = help: {s}\n", .{help});
            }
            try writer.writeByte('\n');
        }
    }

    // -----------------------------------------------------------------------
    // Statement walking
    // -----------------------------------------------------------------------

    fn walkStmt(self: *BoolChecker, node: NodeIndex) void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;

        switch (tag) {
            .program, .block => {
                const block = self.ir_view.getBlock(node) orelse return;
                for (0..block.stmts_count) |i| {
                    const stmt_idx = self.ir_view.getListIndex(block.stmts_start, @intCast(i));
                    self.walkStmt(stmt_idx);
                }
            },

            .if_stmt => {
                const if_s = self.ir_view.getIfStmt(node) orelse return;
                // S1: condition must produce a value with unambiguous truthiness
                self.requireBoolean(if_s.condition, "if");

                // Extract guards from condition for branch-scoped narrowing
                // (typeof guards, undefined equality guards, and truthiness guards)
                var guards: [MAX_NARROWINGS]TypeofGuard = undefined;
                var saved: [MAX_NARROWINGS]?ExprType = undefined;
                var is_negated = false;
                const guard_count = self.extractTypeofGuards(if_s.condition, &guards, &is_negated);
                const active = guards[0..guard_count];
                const saved_active = saved[0..guard_count];

                if (active.len > 0 and is_negated) {
                    // Negated guard: narrow in else-branch only
                    self.walkStmt(if_s.then_branch);
                    if (if_s.else_branch != null_node) {
                        self.walkStmtWithGuards(if_s.else_branch, active, saved_active);
                    }
                } else {
                    // Positive guard or no guard (install/restore are no-ops on empty slice)
                    self.walkStmtWithGuards(if_s.then_branch, active, saved_active);
                    if (if_s.else_branch != null_node) {
                        self.walkStmt(if_s.else_branch);
                    }
                }
            },

            .var_decl => {
                const vd = self.ir_view.getVarDecl(node) orelse return;
                // Walk the initializer expression for nested checks
                if (vd.init != null_node) {
                    self.walkExpr(vd.init);
                    // Track const and let binding types
                    // Let bindings are invalidated on reassignment (see .assignment handler)
                    if (vd.kind == .@"const" or vd.kind == .let) {
                        const inferred = self.inferType(vd.init);
                        const key = bindingKey(vd.binding);
                        self.const_types.put(self.allocator, key, inferred) catch {};

                        // If binding is a function, try to infer its return type
                        if (inferred == .function) {
                            const ret_type = self.inferFunctionReturnType(vd.init);
                            if (ret_type != .unknown) {
                                self.fn_return_types.put(self.allocator, key, ret_type) catch {};
                            }
                        }

                        // Track Result bindings: const r = jwtVerify(...)
                        if (inferred == .object) {
                            if (self.isResultCall(vd.init)) {
                                self.result_bindings.put(self.allocator, key, {}) catch {};
                            }
                        }
                    }
                }
            },

            .return_stmt => {
                // Walk optional return value expression
                if (self.ir_view.getOptValue(node)) |ret_val| {
                    self.walkExpr(ret_val);
                }
            },

            .expr_stmt => {
                if (self.ir_view.getOptValue(node)) |expr| {
                    self.walkExpr(expr);
                }
            },

            .for_of_stmt, .for_in_stmt => {
                const fi = self.ir_view.getForIter(node) orelse return;
                self.walkExpr(fi.iterable);
                self.walkStmt(fi.body);
            },

            .switch_stmt => {
                const sw = self.ir_view.getSwitchStmt(node) orelse return;
                self.walkExpr(sw.discriminant);
                for (0..sw.cases_count) |i| {
                    const case_idx = self.ir_view.getListIndex(sw.cases_start, @intCast(i));
                    const cc = self.ir_view.getCaseClause(case_idx) orelse continue;
                    if (cc.test_expr != null_node) self.walkExpr(cc.test_expr);
                    for (0..cc.body_count) |j| {
                        const body_stmt = self.ir_view.getListIndex(cc.body_start, @intCast(j));
                        self.walkStmt(body_stmt);
                    }
                }
            },

            .function_decl => {
                const func = self.ir_view.getFunction(node) orelse return;
                // Track function return type for named declarations
                const ret_type = self.inferFunctionReturnType(node);
                if (ret_type != .unknown and func.name_atom != 0) {
                    // Function declarations create a binding in the enclosing scope
                    // Use scope_id=0 + name_atom as key (global function pattern)
                    const key = @as(u32, func.name_atom);
                    self.fn_return_types.put(self.allocator, key, ret_type) catch {};
                    self.const_types.put(self.allocator, key, .function) catch {};
                }
                self.walkStmt(func.body);
            },

            .function_expr, .arrow_function => {
                const func = self.ir_view.getFunction(node) orelse return;
                self.walkStmt(func.body);
            },

            .export_default => {
                if (self.ir_view.getOptValue(node)) |val| {
                    self.walkStmt(val);
                }
            },

            // Expression-position tags that can appear as statements
            .binary_op,
            .unary_op,
            .ternary,
            .call,
            .method_call,
            .assignment,
            .match_expr,
            .template_literal,
            => {
                self.walkExpr(node);
            },

            else => {},
        }
    }

    // -----------------------------------------------------------------------
    // Expression walking (checks boolean contexts + recurses)
    // -----------------------------------------------------------------------

    fn walkExpr(self: *BoolChecker, node: NodeIndex) void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;

        switch (tag) {
            .binary_op => {
                const bin = self.ir_view.getBinary(node) orelse return;
                switch (bin.op) {
                    // S2: && and || operands must be boolean
                    .and_op, .or_op => {
                        const op_name = if (bin.op == .and_op) "&&" else "||";
                        self.requireBoolean(bin.left, op_name);
                        self.requireBoolean(bin.right, op_name);
                    },
                    // S4: ?? LHS warning for non-nullable
                    .nullish => {
                        const lhs_type = self.inferType(bin.left);
                        if (lhs_type.isNonNullable()) {
                            self.addDiagnostic(.{
                                .severity = .warning,
                                .kind = .nullish_on_non_nullable,
                                .node = node,
                                .message = "left side of '??' is never undefined",
                                .help = "remove the '??' fallback; it is unreachable",
                            });
                        }
                    },
                    else => {},
                }
                // Recurse into sub-expressions
                self.walkExpr(bin.left);
                self.walkExpr(bin.right);
            },

            .unary_op => {
                const un = self.ir_view.getUnary(node) orelse return;
                // S3: ! operand must be boolean
                if (un.op == .not) {
                    self.requireBoolean(un.operand, "!");
                }
                self.walkExpr(un.operand);
            },

            .ternary => {
                const t = self.ir_view.getTernary(node) orelse return;
                // S1: ternary condition must produce a value with unambiguous truthiness
                self.requireBoolean(t.condition, "ternary");
                self.walkExpr(t.condition);

                // Extract guards from condition for branch-scoped narrowing
                var guards: [MAX_NARROWINGS]TypeofGuard = undefined;
                var saved: [MAX_NARROWINGS]?ExprType = undefined;
                var is_negated = false;
                const guard_count = self.extractTypeofGuards(t.condition, &guards, &is_negated);
                const active = guards[0..guard_count];
                const saved_active = saved[0..guard_count];

                if (active.len > 0 and is_negated) {
                    self.walkExpr(t.then_branch);
                    self.walkExprWithGuards(t.else_branch, active, saved_active);
                } else {
                    self.walkExprWithGuards(t.then_branch, active, saved_active);
                    self.walkExpr(t.else_branch);
                }
            },

            .call => {
                const c = self.ir_view.getCall(node) orelse return;
                self.walkExpr(c.callee);
                for (0..c.args_count) |i| {
                    const arg = self.ir_view.getListIndex(c.args_start, @intCast(i));
                    self.walkExpr(arg);
                }
            },

            .method_call => {
                // method_call uses CallExpr layout: callee is the object.method member_access node
                const mc = self.ir_view.getCall(node) orelse return;
                self.walkExpr(mc.callee);
                for (0..mc.args_count) |i| {
                    const arg = self.ir_view.getListIndex(mc.args_start, @intCast(i));
                    self.walkExpr(arg);
                }
            },

            .assignment => {
                const asgn = self.ir_view.getAssignment(node) orelse return;
                self.walkExpr(asgn.value);
                // Invalidate let binding type on reassignment, then re-track
                const target_tag = self.ir_view.getTag(asgn.target) orelse return;
                if (target_tag == .identifier) {
                    const binding = self.ir_view.getBinding(asgn.target) orelse return;
                    const key = (@as(u32, binding.scope_id) << 16) | @as(u32, binding.slot);
                    // Invalidate branch-scoped narrowing on reassignment
                    _ = self.narrowed_types.remove(key);
                    // Re-infer from the new value (simple assignment only)
                    if (asgn.op == null) {
                        const new_type = self.inferType(asgn.value);
                        self.const_types.put(self.allocator, key, new_type) catch {};
                    } else {
                        // Compound assignment - invalidate to unknown
                        _ = self.const_types.remove(key);
                    }
                }
            },

            .array_literal => {
                const arr = self.ir_view.getArray(node) orelse return;
                for (0..arr.elements_count) |i| {
                    const elem = self.ir_view.getListIndex(arr.elements_start, @intCast(i));
                    self.walkExpr(elem);
                }
            },

            .object_literal => {
                const obj = self.ir_view.getObject(node) orelse return;
                for (0..obj.properties_count) |i| {
                    const prop = self.ir_view.getListIndex(obj.properties_start, @intCast(i));
                    const prop_data = self.ir_view.getProperty(prop) orelse continue;
                    self.walkExpr(prop_data.value);
                }
            },

            .match_expr => {
                const me = self.ir_view.getMatchExpr(node) orelse return;
                self.walkExpr(me.discriminant);
                for (0..me.arms_count) |i| {
                    const arm_idx = self.ir_view.getListIndex(me.arms_start, @intCast(i));
                    const arm = self.ir_view.getMatchArm(arm_idx) orelse continue;
                    self.walkExpr(arm.body);
                }
            },

            .template_literal => {
                const tpl = self.ir_view.getTemplate(node) orelse return;
                for (0..tpl.parts_count) |i| {
                    const part = self.ir_view.getListIndex(tpl.parts_start, @intCast(i));
                    const part_tag = self.ir_view.getTag(part) orelse continue;
                    if (part_tag == .template_part_expr) {
                        if (self.ir_view.getOptValue(part)) |expr| {
                            self.walkExpr(expr);
                        }
                    }
                }
            },

            // Function expressions: walk body for boolean checks
            .function_expr, .arrow_function => {
                const func = self.ir_view.getFunction(node) orelse return;
                self.walkStmt(func.body);
            },

            // Leaf expressions - no children to walk
            .lit_int,
            .lit_float,
            .lit_string,
            .lit_bool,
            .lit_null,
            .lit_undefined,
            .identifier,
            .member_access,
            .computed_access,
            .optional_chain,
            .spread,
            => {},

            else => {},
        }
    }

    // -----------------------------------------------------------------------
    // Type inference
    // -----------------------------------------------------------------------

    fn inferType(self: *BoolChecker, node: NodeIndex) ExprType {
        if (node == null_node) return .unknown;
        const tag = self.ir_view.getTag(node) orelse return .unknown;

        return switch (tag) {
            // Literals
            .lit_bool => .boolean,
            .lit_int, .lit_float => .number,
            .lit_string, .template_literal => .string,
            .lit_null => .undefined, // parser rejects null, but map defensively
            .lit_undefined => .undefined,
            .object_literal, .array_literal => .object,
            .function_expr, .arrow_function, .function_decl => .function,

            .binary_op => self.inferBinaryType(node),
            .unary_op => self.inferUnaryType(node),

            .ternary => {
                const t = self.ir_view.getTernary(node) orelse return .unknown;
                const then_type = self.inferType(t.then_branch);
                const else_type = self.inferType(t.else_branch);
                return unifyTypes(then_type, else_type);
            },

            .identifier => {
                const binding = self.ir_view.getBinding(node) orelse return .unknown;
                const key = bindingKey(binding);
                // Branch-scoped narrowing (applies to ALL binding kinds including .argument)
                if (self.narrowed_types.get(key)) |t| return t;
                // Existing: const/let tracking (local/global only)
                if (binding.kind == .local or binding.kind == .global) {
                    if (self.const_types.get(key)) |t| return t;
                }
                return .unknown;
            },

            .match_expr => self.inferMatchType(node),

            .call => self.inferCallReturnType(node),

            .member_access => self.inferMemberAccessType(node),

            // Method calls, computed access - cannot determine statically
            .method_call,
            .computed_access,
            .optional_chain,
            .optional_call,
            .spread,
            => .unknown,

            else => .unknown,
        };
    }

    fn inferBinaryType(self: *BoolChecker, node: NodeIndex) ExprType {
        const bin = self.ir_view.getBinary(node) orelse return .unknown;

        return switch (bin.op) {
            // Comparisons always produce boolean
            .strict_eq, .strict_neq, .lt, .lte, .gt, .gte, .in_op => .boolean,

            // Logical ops: both sides boolean -> boolean
            .and_op, .or_op => .boolean,

            // Arithmetic
            .sub, .mul, .div, .mod, .pow => .number,

            // Add: string if either side is string, number if both number, else unknown
            .add => {
                const left_type = self.inferType(bin.left);
                const right_type = self.inferType(bin.right);
                if (left_type == .string or right_type == .string) return .string;
                if (left_type == .number and right_type == .number) return .number;
                return .unknown;
            },

            // Bitwise ops
            .bit_and, .bit_or, .bit_xor, .shl, .shr, .ushr => .number,

            // Nullish coalescing: Join(RemoveNullish(LHS), RHS)
            .nullish => {
                const left_type = self.inferType(bin.left);
                const right_type = self.inferType(bin.right);
                // optional_string ?? string -> string
                if (left_type == .optional_string and right_type == .string) return .string;
                if (left_type == .optional_object and right_type == .object) return .object;
                // optional_string ?? number -> unknown (mismatched base types)
                if (left_type == .optional_string and right_type != .string and right_type != .unknown) return .unknown;
                if (left_type == .optional_object and right_type != .object and right_type != .unknown) return .unknown;
                // Non-nullable ?? anything -> non-nullable (the RHS is dead code, warned above)
                if (left_type.isNonNullable()) return left_type;
                // undefined ?? T -> T
                if (left_type == .undefined) return right_type;
                // unknown ?? T -> unknown (conservative)
                return .unknown;
            },

            // Legacy eq/neq (banned by parser, but handle gracefully)
            .eq, .neq => .boolean,
        };
    }

    fn inferUnaryType(self: *const BoolChecker, node: NodeIndex) ExprType {
        const un = self.ir_view.getUnary(node) orelse return .unknown;

        return switch (un.op) {
            .not => .boolean,
            .neg, .pos, .bit_not => .number,
            .typeof_op => .string,
            .void_op => .undefined,
        };
    }

    /// Infer the type of a match expression by unifying arm body types.
    fn inferMatchType(self: *BoolChecker, node: NodeIndex) ExprType {
        const me = self.ir_view.getMatchExpr(node) orelse return .unknown;
        if (me.arms_count == 0) return .unknown;
        var result: ?ExprType = null;
        for (0..me.arms_count) |i| {
            const arm_idx = self.ir_view.getListIndex(me.arms_start, @intCast(i));
            const arm = self.ir_view.getMatchArm(arm_idx) orelse return .unknown;
            const arm_type = self.inferType(arm.body);
            result = mergeReturnType(result, arm_type) orelse return .unknown;
        }
        return result orelse .unknown;
    }

    // -----------------------------------------------------------------------
    // Function return type inference
    // -----------------------------------------------------------------------

    /// Infer the return type of a function expression or arrow function.
    /// Handles: single-expression arrows `(x) => x > 0` and block-bodied
    /// functions with a uniform return type across all return statements.
    fn inferFunctionReturnType(self: *BoolChecker, node: NodeIndex) ExprType {
        const tag = self.ir_view.getTag(node) orelse return .unknown;
        if (tag != .arrow_function and tag != .function_expr and tag != .function_decl) return .unknown;

        const func = self.ir_view.getFunction(node) orelse return .unknown;
        if (func.body == null_node) return .unknown;

        const body_tag = self.ir_view.getTag(func.body) orelse return .unknown;

        // Single-expression arrow: body is wrapped in a return_stmt by the parser
        if (body_tag == .return_stmt) {
            const ret_val = self.ir_view.getOptValue(func.body) orelse return .undefined;
            return self.inferType(ret_val);
        }

        // Other non-block body (shouldn't happen normally, but handle gracefully)
        if (body_tag != .block and body_tag != .program) {
            return self.inferType(func.body);
        }

        // Block body: collect return types
        return self.inferBlockReturnType(func.body);
    }

    /// Scan a block for return statements and infer a uniform return type.
    fn inferBlockReturnType(self: *BoolChecker, block_node: NodeIndex) ExprType {
        const block = self.ir_view.getBlock(block_node) orelse return .unknown;
        var return_type: ?ExprType = null;

        for (0..block.stmts_count) |i| {
            const stmt_idx = self.ir_view.getListIndex(block.stmts_start, @intCast(i));
            const stmt_tag = self.ir_view.getTag(stmt_idx) orelse continue;

            switch (stmt_tag) {
                .return_stmt => {
                    const ret_val = self.ir_view.getOptValue(stmt_idx) orelse {
                        // bare return -> undefined
                        return_type = mergeReturnType(return_type, .undefined) orelse return .unknown;
                        continue;
                    };
                    const this_type = self.inferType(ret_val);
                    return_type = mergeReturnType(return_type, this_type) orelse return .unknown;
                },
                .if_stmt => {
                    // Recurse into if branches
                    const if_s = self.ir_view.getIfStmt(stmt_idx) orelse continue;
                    if (self.inferBranchReturnType(if_s.then_branch)) |tt| {
                        return_type = mergeReturnType(return_type, tt) orelse return .unknown;
                    }
                    if (if_s.else_branch != null_node) {
                        if (self.inferBranchReturnType(if_s.else_branch)) |et| {
                            return_type = mergeReturnType(return_type, et) orelse return .unknown;
                        }
                    }
                },
                else => {},
            }
        }

        return return_type orelse .unknown;
    }

    /// Infer return type from a branch (block or single statement).
    fn inferBranchReturnType(self: *BoolChecker, node: NodeIndex) ?ExprType {
        const tag = self.ir_view.getTag(node) orelse return null;
        if (tag == .block or tag == .program) {
            const t = self.inferBlockReturnType(node);
            return if (t == .unknown) null else t;
        }
        if (tag == .return_stmt) {
            const ret_val = self.ir_view.getOptValue(node) orelse return .undefined;
            return self.inferType(ret_val);
        }
        return null;
    }

    /// Infer the return type of a call expression by looking up the callee.
    fn inferCallReturnType(self: *BoolChecker, node: NodeIndex) ExprType {
        const call = self.ir_view.getCall(node) orelse return .unknown;
        const callee_tag = self.ir_view.getTag(call.callee) orelse return .unknown;

        if (callee_tag == .identifier) {
            const binding = self.ir_view.getBinding(call.callee) orelse return .unknown;

            // Check virtual module import bindings first
            if (self.module_fn_types.get(binding.slot)) |ret_type| return ret_type;

            // Then check locally-inferred function return types
            const key = bindingKey(binding);
            if (self.fn_return_types.get(key)) |ret_type| return ret_type;
        }

        return .unknown;
    }

    /// Check if a call node invokes a Result-producing function.
    fn isResultCall(self: *const BoolChecker, node: NodeIndex) bool {
        const call_tag = self.ir_view.getTag(node) orelse return false;
        if (call_tag != .call) return false;
        const call = self.ir_view.getCall(node) orelse return false;
        const callee_tag = self.ir_view.getTag(call.callee) orelse return false;
        if (callee_tag != .identifier) return false;
        const binding = self.ir_view.getBinding(call.callee) orelse return false;
        return self.module_result_fn_slots.contains(binding.slot);
    }

    // -----------------------------------------------------------------------
    // Property access type inference (Phase 4)
    // -----------------------------------------------------------------------

    /// Known property types for Result objects: { ok: boolean, value: unknown, error: string }
    fn resultPropertyType(self: *const BoolChecker, prop_atom: u16) ExprType {
        const name = self.resolveAtomName(prop_atom) orelse return .unknown;
        if (std.mem.eql(u8, name, "ok")) return .boolean;
        if (std.mem.eql(u8, name, "error")) return .string;
        if (std.mem.eql(u8, name, "value")) return .unknown;
        return .unknown;
    }

    /// Infer the type of a member access expression (obj.prop).
    fn inferMemberAccessType(self: *BoolChecker, node: NodeIndex) ExprType {
        const member = self.ir_view.getMember(node) orelse return .unknown;

        // Check if the object is an identifier with known shape
        const obj_tag = self.ir_view.getTag(member.object) orelse return .unknown;
        if (obj_tag != .identifier) return .unknown;

        const binding = self.ir_view.getBinding(member.object) orelse return .unknown;
        const key = bindingKey(binding);

        // Pattern B: Result object property access (result.ok -> boolean)
        if (self.result_bindings.contains(key)) {
            return self.resultPropertyType(member.property);
        }

        return .unknown;
    }

    // -----------------------------------------------------------------------
    // Boolean requirement check
    // -----------------------------------------------------------------------

    fn requireBoolean(self: *BoolChecker, node: NodeIndex, context_name: []const u8) void {
        const inferred = self.inferType(node);

        // Type-directed truthiness: accept types with unambiguous falsy states.
        // boolean, number, string, optional_string, optional_object: accepted
        // unknown: accepted (runtime handles it)
        // undefined: WARNING (always false - dead branch)
        // object, function: ERROR (always truthy - pointless condition)
        switch (inferred) {
            .boolean, .number, .string, .optional_string, .optional_object, .unknown => return,
            .undefined => {
                self.addDiagnostic(.{
                    .severity = .warning,
                    .kind = .condition_not_boolean,
                    .node = node,
                    .message = "condition is always false (undefined)",
                    .help = "this branch is dead code",
                });
                return;
            },
            .object, .function => {},
        }

        const help: []const u8 = switch (inferred) {
            .object => "objects are always truthy; this condition is pointless",
            .function => "functions are always truthy; this condition is pointless",
            else => unreachable,
        };

        const type_name: []const u8 = switch (inferred) {
            .object => "object",
            .function => "function",
            else => unreachable,
        };

        // Select diagnostic kind based on operator context
        const kind: DiagnosticKind = if (std.mem.eql(u8, context_name, "&&") or std.mem.eql(u8, context_name, "||"))
            .logical_operand_not_boolean
        else if (std.mem.eql(u8, context_name, "!"))
            .not_operand_not_boolean
        else
            .condition_not_boolean;

        // Build message: "always-truthy value (object) in 'if' operator"
        var msg_buf: [80]u8 = undefined;
        const prefix = "always-truthy value (";
        const mid = ") in '";
        const suffix = "' operator";
        const msg_len = prefix.len + type_name.len + mid.len + context_name.len + suffix.len;
        if (msg_len <= msg_buf.len) {
            var pos: usize = 0;
            @memcpy(msg_buf[pos..][0..prefix.len], prefix);
            pos += prefix.len;
            @memcpy(msg_buf[pos..][0..type_name.len], type_name);
            pos += type_name.len;
            @memcpy(msg_buf[pos..][0..mid.len], mid);
            pos += mid.len;
            @memcpy(msg_buf[pos..][0..context_name.len], context_name);
            pos += context_name.len;
            @memcpy(msg_buf[pos..][0..suffix.len], suffix);
            pos += suffix.len;
            const message = self.allocator.dupe(u8, msg_buf[0..pos]) catch
                "always-truthy value used in boolean context";
            self.addDiagnostic(.{
                .severity = .err,
                .kind = kind,
                .node = node,
                .message = message,
                .help = help,
            });
        } else {
            self.addDiagnostic(.{
                .severity = .err,
                .kind = kind,
                .node = node,
                .message = "always-truthy value used in boolean context",
                .help = help,
            });
        }
    }

    // -----------------------------------------------------------------------
    // Truthiness guard extraction (type-directed narrowing)
    // -----------------------------------------------------------------------

    /// Extract a truthiness guard from `if (x)` or `if (!x)` where x has an optional type.
    /// if (x): narrow to non-optional in then-branch (encoded as strict_eq)
    /// if (!x): narrow to non-optional in else-branch (encoded as strict_neq)
    /// Returns null if condition is not an identifier with an optional type.
    fn extractTruthinessGuard(self: *BoolChecker, cond: NodeIndex) ?TypeofGuard {
        const tag = self.ir_view.getTag(cond) orelse return null;

        // Pattern 1: direct identifier - if (x)
        if (tag == .identifier) {
            return self.makeTruthinessGuard(cond, false);
        }

        // Pattern 2: negated identifier - if (!x)
        if (tag == .unary_op) {
            const un = self.ir_view.getUnary(cond) orelse return null;
            if (un.op != .not) return null;
            const operand_tag = self.ir_view.getTag(un.operand) orelse return null;
            if (operand_tag != .identifier) return null;
            return self.makeTruthinessGuard(un.operand, true);
        }

        return null;
    }

    fn makeTruthinessGuard(self: *BoolChecker, ident_node: NodeIndex, negated: bool) ?TypeofGuard {
        const binding = self.ir_view.getBinding(ident_node) orelse return null;
        const key = packBindingKey(binding.scope_id, binding.slot);
        const current_type = self.lookupBindingType(key);

        // Only narrow optional types - non-optional types don't benefit
        const narrowed = current_type.removeNullish();
        if (narrowed == current_type) return null; // Not optional
        if (current_type == .undefined) return null; // Pure undefined has no non-optional variant

        // if (x) -> strict_eq (narrowing in then-branch), if (!x) -> strict_neq (narrowing in else-branch)
        return .{
            .binding_key = key,
            .narrowed_type = narrowed,
            .op = if (negated) .strict_neq else .strict_eq,
        };
    }

    // -----------------------------------------------------------------------
    // Typeof guard extraction
    // -----------------------------------------------------------------------

    /// Map a typeof result string to an ExprType.
    fn typeStringToExprType(s: []const u8) ?ExprType {
        if (std.mem.eql(u8, s, "boolean")) return .boolean;
        if (std.mem.eql(u8, s, "number")) return .number;
        if (std.mem.eql(u8, s, "string")) return .string;
        if (std.mem.eql(u8, s, "object")) return .object;
        if (std.mem.eql(u8, s, "function")) return .function;
        if (std.mem.eql(u8, s, "undefined")) return .undefined;
        return null;
    }

    /// Extract a single typeof guard from a `typeof x === "T"` or `"T" === typeof x` pattern.
    /// Also extracts null/undefined equality guards: `x === null`, `x !== null`,
    /// `x === undefined`, `x !== undefined` (and reversed operand forms).
    /// Returns null if the node does not match any guard pattern.
    fn extractTypeofGuard(self: *BoolChecker, node: NodeIndex) ?TypeofGuard {
        const tag = self.ir_view.getTag(node) orelse return null;
        if (tag != .binary_op) return null;

        const bin = self.ir_view.getBinary(node) orelse return null;
        if (bin.op != .strict_eq and bin.op != .strict_neq) return null;

        const left_tag = self.ir_view.getTag(bin.left) orelse return null;
        const right_tag = self.ir_view.getTag(bin.right) orelse return null;

        // Pattern 1: typeof x === "T" or "T" === typeof x
        if ((left_tag == .unary_op and right_tag == .lit_string) or
            (left_tag == .lit_string and right_tag == .unary_op))
        {
            const typeof_node = if (left_tag == .unary_op) bin.left else bin.right;
            const string_node = if (left_tag == .lit_string) bin.left else bin.right;

            // Verify the unary op is typeof
            const un = self.ir_view.getUnary(typeof_node) orelse return null;
            if (un.op != .typeof_op) return null;

            // Operand must be an identifier
            const operand_tag = self.ir_view.getTag(un.operand) orelse return null;
            if (operand_tag != .identifier) return null;

            const binding = self.ir_view.getBinding(un.operand) orelse return null;
            const key = packBindingKey(binding.scope_id, binding.slot);

            // Get the type string
            const str_idx = self.ir_view.getStringIdx(string_node) orelse return null;
            const type_str = self.ir_view.getString(str_idx) orelse return null;
            const narrowed = typeStringToExprType(type_str) orelse return null;

            return .{
                .binding_key = key,
                .narrowed_type = narrowed,
                .op = bin.op,
            };
        }

        // Pattern 2: x === undefined / undefined === x
        // For x !== undefined: narrow optional_T -> T (remove optionality)
        // For x === undefined: narrow to undefined in then-branch
        const ident_node = blk: {
            if (left_tag == .identifier and (right_tag == .lit_null or right_tag == .lit_undefined)) {
                break :blk bin.left;
            } else if ((left_tag == .lit_null or left_tag == .lit_undefined) and right_tag == .identifier) {
                break :blk bin.right;
            } else {
                return null;
            }
        };

        const binding = self.ir_view.getBinding(ident_node) orelse return null;
        const key = packBindingKey(binding.scope_id, binding.slot);

        // For undefined guards, the narrowed_type is what the variable becomes
        // in the "positive" (===) branch. The guard system uses `op` to decide:
        //   op == strict_eq -> install narrowing in then-branch
        //   op == strict_neq -> install narrowing in else-branch
        //
        // x === undefined: then-branch -> undefined, else-branch -> removeNullish
        // x !== undefined: then-branch -> removeNullish, else-branch -> undefined
        //
        // We always store the "then-branch" narrowing and encode the op.
        if (bin.op == .strict_eq) {
            // x === undefined: then-branch narrowing is undefined
            return .{
                .binding_key = key,
                .narrowed_type = .undefined,
                .op = bin.op,
            };
        } else {
            // x !== undefined: then-branch narrowing is removeNullish (non-undefined)
            const current_type = self.lookupBindingType(key);
            const narrowed = current_type.removeNullish();
            // For x !== undefined with unknown type, narrow to unknown (still useful for flow)
            // For x !== undefined with non-nullable type, no narrowing needed
            if (narrowed == current_type and current_type.isNonNullable()) return null;
            return .{
                .binding_key = key,
                .narrowed_type = narrowed,
                // Encode as strict_eq so narrowing applies to then-branch
                // (x !== undefined -> then-branch has non-undefined x)
                .op = .strict_eq,
            };
        }
    }

    /// Look up the current type of a binding by key.
    fn lookupBindingType(self: *const BoolChecker, key: u32) ExprType {
        if (self.narrowed_types.get(key)) |t| return t;
        if (self.const_types.get(key)) |t| return t;
        return .unknown;
    }

    /// Extract typeof guards from a condition, handling &&-chained guards.
    /// Sets is_negated to true if a single !== guard is found.
    /// For && chains, only collects === guards (not !==) to stay sound.
    /// Returns the number of guards collected.
    fn extractTypeofGuards(
        self: *BoolChecker,
        node: NodeIndex,
        guards: *[MAX_NARROWINGS]TypeofGuard,
        is_negated: *bool,
    ) usize {
        is_negated.* = false;

        // Try single typeof/equality guard first
        if (self.extractTypeofGuard(node)) |guard| {
            guards[0] = guard;
            is_negated.* = (guard.op == .strict_neq);
            return 1;
        }

        // Try && chain of typeof guards
        const tag = self.ir_view.getTag(node) orelse return 0;
        if (tag == .binary_op) {
            const bin = self.ir_view.getBinary(node) orelse return 0;
            if (bin.op == .and_op) {
                var count: usize = 0;
                self.collectAndGuards(bin.left, guards, &count);
                self.collectAndGuards(bin.right, guards, &count);
                if (count > 0) return count;
            }
        }

        // Fallback: truthiness guard for if (x) / if (!x) with optional types
        return self.tryTruthinessGuard(node, guards, is_negated);
    }

    fn tryTruthinessGuard(
        self: *BoolChecker,
        node: NodeIndex,
        guards: *[MAX_NARROWINGS]TypeofGuard,
        is_negated: *bool,
    ) usize {
        const guard = self.extractTruthinessGuard(node) orelse return 0;
        guards[0] = guard;
        is_negated.* = (guard.op == .strict_neq);
        return 1;
    }

    /// Recursively collect === typeof guards from an && chain.
    fn collectAndGuards(
        self: *BoolChecker,
        node: NodeIndex,
        guards: *[MAX_NARROWINGS]TypeofGuard,
        count: *usize,
    ) void {
        if (count.* >= MAX_NARROWINGS) return;

        // Check if this node is itself a typeof === guard
        if (self.extractTypeofGuard(node)) |guard| {
            if (guard.op == .strict_eq) {
                guards[count.*] = guard;
                count.* += 1;
            }
            return;
        }

        // Check if this is another && to recurse into
        const tag = self.ir_view.getTag(node) orelse return;
        if (tag != .binary_op) return;
        const bin = self.ir_view.getBinary(node) orelse return;
        if (bin.op != .and_op) return;

        self.collectAndGuards(bin.left, guards, count);
        if (count.* >= MAX_NARROWINGS) return;
        self.collectAndGuards(bin.right, guards, count);
    }

    // -----------------------------------------------------------------------
    // Narrowing scope helpers
    // -----------------------------------------------------------------------

    /// Save current narrowed_types values for the guard keys, then install the narrowings.
    fn installGuards(self: *BoolChecker, guards: []const TypeofGuard, saved: []?ExprType) void {
        for (guards, 0..) |g, i| {
            saved[i] = self.narrowed_types.get(g.binding_key);
            self.narrowed_types.put(self.allocator, g.binding_key, g.narrowed_type) catch {};
        }
    }

    /// Restore narrowed_types to saved values, removing entries that had no prior value.
    fn restoreGuards(self: *BoolChecker, guards: []const TypeofGuard, saved: []const ?ExprType) void {
        for (guards, 0..) |g, i| {
            if (saved[i]) |prev| {
                self.narrowed_types.put(self.allocator, g.binding_key, prev) catch {};
            } else {
                _ = self.narrowed_types.remove(g.binding_key);
            }
        }
    }

    fn walkStmtWithGuards(
        self: *BoolChecker,
        node: NodeIndex,
        guards: []const TypeofGuard,
        saved: []?ExprType,
    ) void {
        self.installGuards(guards, saved);
        self.walkStmt(node);
        self.restoreGuards(guards, saved);
    }

    fn walkExprWithGuards(
        self: *BoolChecker,
        node: NodeIndex,
        guards: []const TypeofGuard,
        saved: []?ExprType,
    ) void {
        self.installGuards(guards, saved);
        self.walkExpr(node);
        self.restoreGuards(guards, saved);
    }

    // -----------------------------------------------------------------------
    // Import scanning for virtual module return types
    // -----------------------------------------------------------------------

    // -----------------------------------------------------------------------
    // Virtual module return type table
    // -----------------------------------------------------------------------

    const builtin_modules = @import("builtin_modules.zig");
    const mb = @import("module_binding.zig");

    /// Return type entry derived from the module binding registry.
    const ModuleReturnEntry = struct {
        module: []const u8,
        name: []const u8,
        ret: ExprType,
        is_result: bool = false,
    };

    /// Look up a function's return type from the module binding registry.
    fn findModuleReturnEntry(module_str: []const u8, func_name: []const u8) ?ModuleReturnEntry {
        for (builtin_modules.all) |binding| {
            if (!std.mem.eql(u8, binding.specifier, module_str)) continue;
            for (binding.exports) |func| {
                if (std.mem.eql(u8, func.name, func_name)) {
                    return .{
                        .module = binding.specifier,
                        .name = func.name,
                        .ret = returnKindToExprType(func.returns),
                        .is_result = func.returns == .result,
                    };
                }
            }
        }
        return null;
    }

    fn returnKindToExprType(kind: mb.ReturnKind) ExprType {
        return switch (kind) {
            .boolean => .boolean,
            .number => .number,
            .string => .string,
            .object => .object,
            .undefined => .undefined,
            .unknown => .unknown,
            .optional_string => .optional_string,
            .optional_object => .optional_object,
            .result => .object, // Result objects are typed as object in ExprType
        };
    }

    /// Scan all import declarations to map local binding slots to known return types.
    fn scanImports(self: *BoolChecker) void {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            if (tag != .import_decl) continue;

            const import_decl = self.ir_view.getImportDecl(idx) orelse continue;
            const module_str = self.ir_view.getString(import_decl.module_idx) orelse continue;

            // Only process zigttp:* virtual modules
            if (!std.mem.startsWith(u8, module_str, "zigttp:")) continue;

            var j: u8 = 0;
            while (j < import_decl.specifiers_count) : (j += 1) {
                const spec_idx = self.ir_view.getListIndex(import_decl.specifiers_start, j);
                const spec = self.ir_view.getImportSpec(spec_idx) orelse continue;
                const imported_name = self.resolveAtomName(spec.imported_atom) orelse continue;
                const entry = findModuleReturnEntry(module_str, imported_name) orelse continue;

                self.module_fn_types.put(self.allocator, spec.local_binding.slot, entry.ret) catch {};
                if (entry.is_result) {
                    self.module_result_fn_slots.put(self.allocator, spec.local_binding.slot, {}) catch {};
                }
            }
        }
    }

    fn resolveAtomName(self: *const BoolChecker, atom_idx: u16) ?[]const u8 {
        if (self.atoms) |table| {
            // With atom table: predefined atoms first, then dynamic table
            const atom: object.Atom = @enumFromInt(atom_idx);
            if (atom.toPredefinedName()) |name| return name;
            return table.getName(atom);
        }
        // Without atom table (standalone parser): predefined atoms and string
        // constants share the u16 index space. String constants take priority
        // because import specifiers (the main use case) go through addString.
        // Predefined atom names are keywords/builtins, never import specifier names.
        if (self.ir_view.getString(atom_idx)) |name| return name;
        const atom: object.Atom = @enumFromInt(atom_idx);
        return atom.toPredefinedName();
    }

    fn addDiagnostic(self: *BoolChecker, diag: Diagnostic) void {
        self.diagnostics.append(self.allocator, diag) catch {};
    }
};

// ---------------------------------------------------------------------------
// Utility
// ---------------------------------------------------------------------------

pub fn getSourceLine(source: []const u8, target_line: u32) ?[]const u8 {
    var current_line: u32 = 1;
    var line_start: usize = 0;

    for (source, 0..) |c, i| {
        if (current_line == target_line) {
            var line_end = i;
            while (line_end < source.len and source[line_end] != '\n') {
                line_end += 1;
            }
            return source[line_start..line_end];
        }
        if (c == '\n') {
            current_line += 1;
            line_start = i + 1;
        }
    }

    if (current_line == target_line and line_start < source.len) {
        return source[line_start..];
    }

    return null;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

fn checkSource(source: []const u8, expect_errors: u32) !void {
    return checkSourceFull(source, expect_errors, null);
}

fn checkSourceFull(source: []const u8, expect_errors: u32, expect_warnings: ?u32) !void {
    const allocator = std.testing.allocator;

    var parser = @import("parser/parse.zig").Parser.init(allocator, source);
    defer parser.deinit();

    const root = parser.parse() catch |err| return err;

    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var checker = BoolChecker.init(allocator, ir_view, null);
    defer checker.deinit();

    const errors = try checker.check(root);
    try std.testing.expectEqual(expect_errors, errors);

    if (expect_warnings) |w| {
        var warning_count: u32 = 0;
        for (checker.getDiagnostics()) |diag| {
            if (diag.severity == .warning) warning_count += 1;
        }
        try std.testing.expectEqual(w, warning_count);
    }
}

// S1: if/ternary conditions

test "sound: boolean literal in if passes" {
    try checkSource("if (true) { let x = 1; }", 0);
}

test "sound: comparison in if passes" {
    try checkSource("let x = 5; if (x === 0) { let y = 1; }", 0);
}

test "sound: boolean && boolean in if passes" {
    try checkSource("let a = 1; let b = 2; if (a === 1 && b !== 3) { let c = 1; }", 0);
}

test "sound: tracked const boolean in if passes" {
    try checkSource("const done = 1 > 0; if (done) { let x = 1; }", 0);
}

test "sound: unknown (fn call result) in if passes" {
    try checkSource("const ok = validate(); if (ok) { let x = 1; }", 0);
}

test "sound: unknown (param) in ternary passes" {
    try checkSource("const f = (x) => x ? 1 : 2;", 0);
}

test "sound: number literal in if passes (TDT)" {
    try checkSource("if (0) { let x = 1; }", 0);
}

test "sound: string literal in if passes (TDT)" {
    try checkSource("if (\"hello\") { let x = 1; }", 0);
}

test "sound: undefined literal in if warns (TDT)" {
    try checkSourceFull("if (undefined) { let x = 1; }", 0, 1);
}

test "sound: tracked const number in if passes (TDT)" {
    try checkSource("const count = 42; if (count) { let x = 1; }", 0);
}

// S2: && and || operands

test "sound: number operands for && passes (TDT)" {
    try checkSource("const r = 1 && 2;", 0);
}

test "sound: string operands for || passes (TDT)" {
    try checkSource("const r = \"a\" || \"b\";", 0);
}

// S3: ! operand

test "sound: !0 passes (TDT)" {
    try checkSource("const r = !0;", 0);
}

test "sound: !string passes (TDT)" {
    try checkSource("const r = !\"str\";", 0);
}

test "sound: !boolean passes" {
    try checkSource("const r = !(1 > 0);", 0);
}

// S4: ?? warnings

test "sound: non-nullable LHS for ?? warns" {
    try checkSourceFull("const r = 42 ?? 0;", 0, 1);
}

test "sound: string LHS for ?? warns" {
    try checkSourceFull("const r = \"str\" ?? \"default\";", 0, 1);
}

test "sound: unknown LHS for ?? no warning" {
    try checkSourceFull("const f = (x) => x ?? 0;", 0, 0);
}

// Combined

test "sound: nested if with mixed types passes (TDT)" {
    try checkSource(
        \\const flag = true;
        \\if (flag) {
        \\  const count = 5;
        \\  if (count) { let x = 1; }
        \\}
    , 0);
}

test "sound: complex boolean expression passes" {
    try checkSource(
        \\const a = 1;
        \\const b = 2;
        \\const c = 3;
        \\if (a > 0 && b !== c || a === b) { let x = 1; }
    , 0);
}

// Let variable tracking

test "sound: tracked let number in if passes (TDT)" {
    try checkSource("let count = 0; if (count) { let x = 1; }", 0);
}

test "sound: let reassigned to boolean passes" {
    try checkSource(
        \\let flag = 0;
        \\flag = 1 > 0;
        \\if (flag) { let x = 1; }
    , 0);
}

test "sound: let reassigned to number passes (TDT)" {
    try checkSource(
        \\let flag = true;
        \\flag = 42;
        \\if (flag) { let x = 1; }
    , 0);
}

// Diagnostic context messages

// Function return type inference

test "sound: arrow function returning boolean - call site passes" {
    try checkSource(
        \\const isPositive = (n) => n > 0;
        \\if (isPositive(5)) { let x = 1; }
    , 0);
}

test "sound: arrow function returning number - call site passes (TDT)" {
    try checkSource(
        \\const double = (n) => n * 2;
        \\if (double(5)) { let x = 1; }
    , 0);
}

test "sound: block function returning boolean passes" {
    try checkSource(
        \\const check = (x) => {
        \\  return x > 0;
        \\};
        \\if (check(1)) { let y = 1; }
    , 0);
}

test "sound: block function with mixed return types is unknown" {
    // Mixed return types -> unknown -> passes (runtime catches it)
    try checkSource(
        \\const mixed = (x) => {
        \\  if (x > 0) { return true; }
        \\  return 0;
        \\};
        \\if (mixed(1)) { let y = 1; }
    , 0);
}

test "sound: untracked function call is unknown (passes)" {
    try checkSource("if (someFunc()) { let x = 1; }", 0);
}

// Diagnostic context messages

test "sound: diagnostic includes operator context for object" {
    const allocator = std.testing.allocator;

    var parser = @import("parser/parse.zig").Parser.init(allocator, "if ({}) { let x = 1; }");
    defer parser.deinit();

    const root = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var checker = BoolChecker.init(allocator, ir_view, null);
    defer checker.deinit();

    _ = try checker.check(root);
    const diags = checker.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 1), diags.len);
    // Message should include the operator context
    try std.testing.expect(std.mem.indexOf(u8, diags[0].message, "object") != null);
    try std.testing.expect(std.mem.indexOf(u8, diags[0].message, "if") != null);
}

// Typeof guard narrowing

test "sound: typeof guard narrows to number, used in boolean context (TDT)" {
    try checkSource(
        \\const f = (x) => {
        \\  if (typeof x === "number") {
        \\    if (x) { let y = 1; }
        \\  }
        \\  return true;
        \\};
    , 0);
}

test "sound: typeof guard narrows to boolean, used in boolean context" {
    try checkSource(
        \\const f = (x) => {
        \\  if (typeof x === "boolean") {
        \\    if (x) { let y = 1; }
        \\  }
        \\  return true;
        \\};
    , 0);
}

test "sound: typeof guard reversed operand order (TDT)" {
    try checkSource(
        \\const f = (x) => {
        \\  if ("number" === typeof x) {
        \\    if (x) { let y = 1; }
        \\  }
        \\  return true;
        \\};
    , 0);
}

test "sound: typeof negated guard narrows else-branch (TDT)" {
    try checkSource(
        \\const f = (x) => {
        \\  if (typeof x !== "number") {
        \\    let y = 1;
        \\  } else {
        \\    if (x) { let z = 1; }
        \\  }
        \\  return true;
        \\};
    , 0);
}

test "sound: typeof guard narrows ternary then branch (TDT)" {
    try checkSource(
        \\const f = (x) => {
        \\  typeof x === "number" ? (x ? 1 : 0) : 0;
        \\  return true;
        \\};
    , 0);
}

test "sound: typeof negated guard narrows ternary else branch (TDT)" {
    try checkSource(
        \\const f = (x) => {
        \\  typeof x !== "number" ? 0 : (x ? 1 : 0);
        \\  return true;
        \\};
    , 0);
}

test "sound: typeof narrowing does not leak outside branch" {
    // Outside the typeof branch, x should revert to unknown (passes)
    try checkSource(
        \\const f = (x) => {
        \\  if (typeof x === "number") {
        \\    let y = x;
        \\  }
        \\  if (x) { let z = 1; }
        \\  return true;
        \\};
    , 0);
}

test "sound: typeof compound && guard narrows both (TDT)" {
    try checkSource(
        \\const f = (x, y) => {
        \\  if (typeof x === "number" && typeof y === "string") {
        \\    if (x) { let z = 1; }
        \\  }
        \\  return true;
        \\};
    , 0);
}

test "sound: typeof nested guards compose" {
    try checkSource(
        \\const f = (x, y) => {
        \\  if (typeof x === "boolean") {
        \\    if (typeof y === "boolean") {
        \\      const r = x && y;
        \\    }
        \\  }
        \\  return true;
        \\};
    , 0);
}

test "sound: non-typeof condition does not narrow" {
    // x === true is not a typeof guard - x stays unknown (passes)
    try checkSource(
        \\const f = (x) => {
        \\  if (x === true) {
        \\    if (x) { let y = 1; }
        \\  }
        \\  return true;
        \\};
    , 0);
}

// Virtual module return type inference (Phase 1)

test "sound: virtual module boolean return type catches non-boolean use" {
    // verifyWebhookSignature returns boolean - using it in if is fine
    try checkSource(
        \\import { verifyWebhookSignature } from "zigttp:auth";
        \\const ok = verifyWebhookSignature("payload", "secret", "sig");
        \\if (ok) { let x = 1; }
    , 0);
}

test "sound: virtual module number return type passes (TDT)" {
    // cacheIncr returns number - TDT coerces: != 0
    try checkSource(
        \\import { cacheIncr } from "zigttp:cache";
        \\const count = cacheIncr("ns", "key");
        \\if (count) { let x = 1; }
    , 0);
}

test "sound: virtual module string return type passes (TDT)" {
    // sha256 returns string - TDT coerces: != ""
    try checkSource(
        \\import { sha256 } from "zigttp:crypto";
        \\const hash = sha256("data");
        \\if (hash) { let x = 1; }
    , 0);
}

test "sound: virtual module object return type fails in boolean context" {
    // jwtVerify returns object (Result) - objects are always truthy, still an error
    try checkSource(
        \\import { jwtVerify } from "zigttp:auth";
        \\const result = jwtVerify("token", "secret");
        \\if (result) { let x = 1; }
    , 1);
}

test "sound: virtual module optional return type passes (TDT)" {
    // env returns optional_string - TDT coerces: != undefined, narrows to string
    try checkSource(
        \\import { env } from "zigttp:env";
        \\const val = env("KEY");
        \\if (val) { let x = 1; }
    , 0);
}

test "sound: virtual module direct call in boolean context" {
    // timingSafeEqual returns boolean - direct call in if is fine
    try checkSource(
        \\import { timingSafeEqual } from "zigttp:auth";
        \\if (timingSafeEqual("a", "b")) { let x = 1; }
    , 0);
}

test "sound: virtual module direct number call in boolean context passes (TDT)" {
    // cacheIncr returns number - TDT coerces: != 0
    try checkSource(
        \\import { cacheIncr } from "zigttp:cache";
        \\if (cacheIncr("ns", "key")) { let x = 1; }
    , 0);
}

// Phase 2: Match expression type inference tests must run via `zig build test-zts`
// (standalone `zig test` on bool_checker.zig hangs when parsing match expressions).

// Phase 3: Optional union types

test "sound: optional string from env used with ?? passes (TDT)" {
    // env() ?? "default" is fine - ?? resolves optional, string in boolean context passes
    try checkSourceFull(
        \\import { env } from "zigttp:env";
        \\const val = env("KEY") ?? "default";
        \\if (val) { let x = 1; }
    , 0, 0); // val is string (from ??), string in boolean context passes via TDT; no warnings
}

test "sound: optional with ?? does not warn" {
    // env() ?? "default" - the ?? is legitimate because env() is optional
    try checkSourceFull(
        \\import { env } from "zigttp:env";
        \\const val = env("KEY") ?? "default";
    , 0, 0); // no errors, no warnings
}

test "sound: non-optional with ?? still warns" {
    // sha256() ?? "" - sha256 returns string (never undefined), so ?? is unreachable
    try checkSourceFull(
        \\import { sha256 } from "zigttp:crypto";
        \\const val = sha256("data") ?? "";
    , 0, 1); // no errors, 1 warning
}

test "sound: optional cacheGet in boolean context passes (TDT)" {
    try checkSource(
        \\import { cacheGet } from "zigttp:cache";
        \\const val = cacheGet("ns", "key");
        \\if (val) { let x = 1; }
    , 0);
}

test "sound: function returning string or undefined infers optional_string (TDT)" {
    // Mixed return: string + undefined -> optional_string -> passes via TDT
    try checkSource(
        \\const find = (arr) => {
        \\  if (arr.length > 0) { return "found"; }
        \\  return undefined;
        \\};
        \\const r = find([1]);
        \\if (r) { let x = 1; }
    , 0);
}

// Phase 4: Property access on known shapes

test "sound: result.ok is boolean - passes in boolean context" {
    try checkSource(
        \\import { jwtVerify } from "zigttp:auth";
        \\const result = jwtVerify("token", "secret", "opts");
        \\if (result.ok) { let x = 1; }
    , 0);
}

test "sound: aliased result-producing import preserves result shape" {
    try checkSource(
        \\import { jwtVerify as verify } from "zigttp:auth";
        \\const result = verify("token", "secret", "opts");
        \\if (result.ok) { let x = 1; }
    , 0);
}

test "sound: result.error is string - passes (TDT)" {
    try checkSource(
        \\import { validateJson } from "zigttp:validate";
        \\const result = validateJson("schema", "data");
        \\if (result.error) { let x = 1; }
    , 0);
}

test "sound: non-result object property stays unknown" {
    // Regular object property access - stays unknown (passes)
    try checkSource(
        \\const obj = { x: 1 };
        \\if (obj.x) { let y = 1; }
    , 0);
}

// Undefined equality narrowing

test "sound: x !== undefined narrows optional to non-optional (TDT)" {
    // env() returns optional_string. After !== undefined guard, it should be string.
    // string in boolean context now passes via TDT.
    try checkSource(
        \\import { env } from "zigttp:env";
        \\const val = env("KEY");
        \\if (val !== undefined) {
        \\  if (val) { let x = 1; }
        \\}
    , 0); // val narrowed to string -> string in if -> passes via TDT
}

test "sound: x !== undefined on non-optional is no-op (TDT)" {
    // sha256 returns string (non-optional), !== undefined check is valid but doesn't change type
    try checkSource(
        \\import { sha256 } from "zigttp:crypto";
        \\const hash = sha256("data");
        \\if (hash !== undefined) {
        \\  if (hash) { let x = 1; }
        \\}
    , 0); // hash is string regardless, string in if -> passes via TDT
}

test "sound: undefined === x narrowing works reversed (TDT)" {
    try checkSource(
        \\import { env } from "zigttp:env";
        \\const val = env("KEY");
        \\if (undefined !== val) {
        \\  if (val) { let x = 1; }
        \\}
    , 0); // val narrowed to string -> passes via TDT
}

test "sound: x === undefined narrows to undefined type in then-branch" {
    // x === undefined in then-branch should narrow to undefined
    // undefined in boolean context emits warning (always false)
    try checkSourceFull(
        \\import { env } from "zigttp:env";
        \\const val = env("KEY");
        \\if (val === undefined) {
        \\  if (val) { let x = 1; }
        \\}
    , 0, 1); // val narrowed to undefined -> undefined in if -> warning (dead branch)
}

test "sound: narrowing does not leak outside undefined guard branch (TDT)" {
    // Outside the undefined guard, val should revert to optional_string (passes via TDT)
    try checkSource(
        \\import { env } from "zigttp:env";
        \\const val = env("KEY");
        \\if (val !== undefined) {
        \\  let y = val;
        \\}
        \\if (val) { let z = 1; }
    , 0); // val reverts to optional_string -> passes via TDT
}

// TDT-specific tests

test "sound: if (42) passes - number truthiness" {
    try checkSource("if (42) { let x = 1; }", 0);
}

test "sound: if (0) passes - number zero is falsy" {
    try checkSource("if (0) { let x = 1; }", 0);
}

test "sound: if ('hello') passes - string truthiness" {
    try checkSource("if (\"hello\") { let x = 1; }", 0);
}

test "sound: if ('') passes - empty string is falsy" {
    try checkSource("if (\"\") { let x = 1; }", 0);
}

test "sound: if (undefined) warns - always false" {
    try checkSourceFull("if (undefined) { let x = 1; }", 0, 1);
}

test "sound: if ({}) fails - object always truthy" {
    try checkSource("if ({}) { let x = 1; }", 1);
}

test "sound: if (fn) fails - function always truthy" {
    try checkSource("if (() => 1) { let x = 1; }", 1);
}

test "sound: truthiness narrowing - if (x) narrows optional to string" {
    try checkSource(
        \\import { env } from "zigttp:env";
        \\const x = env("K");
        \\if (x) {
        \\  const upper = x;
        \\}
    , 0);
}

test "sound: truthiness narrowing - if (!x) narrows optional" {
    try checkSource(
        \\import { env } from "zigttp:env";
        \\const x = env("K");
        \\if (!x) {
        \\  let y = 1;
        \\}
    , 0);
}

test "sound: && with number and string passes (TDT)" {
    try checkSource("if (1 && \"ok\") { let x = 1; }", 0);
}

test "sound: optional from function in boolean context passes (TDT)" {
    // Function returning optional_object -> accepted in boolean context
    try checkSource(
        \\const find = (arr) => {
        \\  if (arr.length > 0) { return "found"; }
        \\  return undefined;
        \\};
        \\const r = find([1]);
        \\if (r) { let x = r; }
    , 0);
}
