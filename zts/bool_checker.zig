//! Sound Mode: Strict Boolean Enforcement
//!
//! Static analysis pass that enforces boolean-typed values in boolean contexts:
//! - if/ternary conditions must be boolean
//! - && and || operands must be boolean
//! - ! operand must be boolean
//! - ?? LHS that is provably non-nullable triggers a warning
//!
//! Modeled on handler_verifier.zig: walks the IR tree inferring expression types.
//! When inferType returns .unknown, no diagnostic is emitted (escape hatch for
//! function calls, parameters, property accesses). Runtime VM assertions (sound_mode
//! flag on interpreter) catch those cases at execution time.

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
    null_type, // null literal
    undefined, // undefined literal
    object, // object/array literals
    function, // function/arrow expressions
    unknown, // cannot determine statically (params, fn calls, let vars, property access)
};

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

    pub fn init(allocator: std.mem.Allocator, ir_view: IrView, atoms: ?*context.AtomTable) BoolChecker {
        return .{
            .allocator = allocator,
            .ir_view = ir_view,
            .atoms = atoms,
            .diagnostics = .empty,
            .const_types = .empty,
            .fn_return_types = .empty,
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
    }

    /// Check if a diagnostic message was dynamically allocated vs static string.
    /// Allocated messages start with "non-boolean value (" and contain "' operator".
    fn isAllocatedMessage(msg: []const u8) bool {
        return msg.len > 30 and
            std.mem.startsWith(u8, msg, "non-boolean value (") and
            std.mem.endsWith(u8, msg, "' operator");
    }

    /// Run the checker on the given root node. Returns the number of errors.
    pub fn check(self: *BoolChecker, root: NodeIndex) !u32 {
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
            try writer.print("sound {s}: {s}\n", .{ diag.severity.label(), diag.message });

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
                // S1: condition must be boolean
                self.requireBoolean(if_s.condition, "if");
                self.walkStmt(if_s.then_branch);
                if (if_s.else_branch != null_node) {
                    self.walkStmt(if_s.else_branch);
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
                        // Key: pack scope_id and slot into a single u32
                        const key = (@as(u32, vd.binding.scope_id) << 16) | @as(u32, vd.binding.slot);
                        self.const_types.put(self.allocator, key, inferred) catch {};

                        // If binding is a function, try to infer its return type
                        if (inferred == .function) {
                            const ret_type = self.inferFunctionReturnType(vd.init);
                            if (ret_type != .unknown) {
                                self.fn_return_types.put(self.allocator, key, ret_type) catch {};
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
            .binary_op, .unary_op, .ternary, .call, .method_call,
            .assignment, .match_expr, .template_literal,
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
                        if (lhs_type == .number or lhs_type == .string or
                            lhs_type == .boolean or lhs_type == .object or
                            lhs_type == .function)
                        {
                            self.addDiagnostic(.{
                                .severity = .warning,
                                .kind = .nullish_on_non_nullable,
                                .node = node,
                                .message = "left side of '??' is never null or undefined",
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
                // S1: ternary condition must be boolean
                self.requireBoolean(t.condition, "ternary");
                self.walkExpr(t.condition);
                self.walkExpr(t.then_branch);
                self.walkExpr(t.else_branch);
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

            // Leaf expressions - no children to walk
            .lit_int, .lit_float, .lit_string, .lit_bool, .lit_null,
            .lit_undefined, .identifier, .member_access, .computed_access,
            .optional_chain, .spread,
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
            .lit_null => .null_type,
            .lit_undefined => .undefined,
            .object_literal, .array_literal => .object,
            .function_expr, .arrow_function, .function_decl => .function,

            .binary_op => self.inferBinaryType(node),
            .unary_op => self.inferUnaryType(node),

            .ternary => {
                const t = self.ir_view.getTernary(node) orelse return .unknown;
                const then_type = self.inferType(t.then_branch);
                const else_type = self.inferType(t.else_branch);
                return if (then_type == else_type) then_type else .unknown;
            },

            .identifier => {
                // Look up const binding type if tracked
                const binding = self.ir_view.getBinding(node) orelse return .unknown;
                if (binding.kind == .local or binding.kind == .global) {
                    const key = (@as(u32, binding.scope_id) << 16) | @as(u32, binding.slot);
                    if (self.const_types.get(key)) |t| return t;
                }
                return .unknown;
            },

            .match_expr => .unknown,

            .call => self.inferCallReturnType(node),

            // Method calls, property access - cannot determine statically
            .method_call, .member_access, .computed_access,
            .optional_chain, .optional_call, .spread,
            => .unknown,

            else => .unknown,
        };
    }

    fn inferBinaryType(self: *BoolChecker, node: NodeIndex) ExprType {
        const bin = self.ir_view.getBinary(node) orelse return .unknown;

        return switch (bin.op) {
            // Comparisons always produce boolean
            .strict_eq, .strict_neq, .lt, .lte, .gt, .gte, .in_op => .boolean,

            // Logical ops in sound mode: both sides boolean -> boolean
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

            // Nullish coalescing: depends on operands
            .nullish => .unknown,

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
                        if (return_type) |rt| {
                            if (rt != .undefined) return .unknown;
                        } else {
                            return_type = .undefined;
                        }
                        continue;
                    };
                    const this_type = self.inferType(ret_val);
                    if (return_type) |rt| {
                        if (rt != this_type) return .unknown;
                    } else {
                        return_type = this_type;
                    }
                },
                .if_stmt => {
                    // Recurse into if branches
                    const if_s = self.ir_view.getIfStmt(stmt_idx) orelse continue;
                    const then_type = self.inferBranchReturnType(if_s.then_branch);
                    if (then_type) |tt| {
                        if (return_type) |rt| {
                            if (rt != tt) return .unknown;
                        } else {
                            return_type = tt;
                        }
                    }
                    if (if_s.else_branch != null_node) {
                        const else_type = self.inferBranchReturnType(if_s.else_branch);
                        if (else_type) |et| {
                            if (return_type) |rt| {
                                if (rt != et) return .unknown;
                            } else {
                                return_type = et;
                            }
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
            const key = (@as(u32, binding.scope_id) << 16) | @as(u32, binding.slot);
            if (self.fn_return_types.get(key)) |ret_type| return ret_type;
        }

        return .unknown;
    }

    // -----------------------------------------------------------------------
    // Boolean requirement check
    // -----------------------------------------------------------------------

    fn requireBoolean(self: *BoolChecker, node: NodeIndex, context_name: []const u8) void {
        const inferred = self.inferType(node);

        // unknown passes through - runtime assertions catch these
        if (inferred == .unknown or inferred == .boolean) return;

        const help: []const u8 = switch (inferred) {
            .number => "use explicit comparison: n !== 0",
            .string => "use explicit comparison: s.length > 0 or s !== \"\"",
            .null_type => "null is not boolean; this condition is always false",
            .undefined => "undefined is not boolean; this condition is always false",
            .object => "objects are not boolean; this condition is always true",
            .function => "functions are not boolean; this condition is always true",
            else => unreachable,
        };

        const type_name: []const u8 = switch (inferred) {
            .number => "number",
            .string => "string",
            .null_type => "null",
            .undefined => "undefined",
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

        // Build message with context: "non-boolean value (number) in '&&' operator"
        var msg_buf: [80]u8 = undefined;
        const prefix = "non-boolean value (";
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
            // Allocate to get a stable pointer for the diagnostic
            const message = self.allocator.dupe(u8, msg_buf[0..pos]) catch
                "non-boolean value used in boolean context";
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
                .message = "non-boolean value used in boolean context",
                .help = help,
            });
        }
    }

    fn addDiagnostic(self: *BoolChecker, diag: Diagnostic) void {
        self.diagnostics.append(self.allocator, diag) catch {};
    }
};

// ---------------------------------------------------------------------------
// Utility
// ---------------------------------------------------------------------------

fn getSourceLine(source: []const u8, target_line: u32) ?[]const u8 {
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

    const root = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return err;
    };

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

test "sound: number literal in if fails" {
    try checkSource("if (0) { let x = 1; }", 1);
}

test "sound: string literal in if fails" {
    try checkSource("if (\"hello\") { let x = 1; }", 1);
}

test "sound: null literal in if fails" {
    try checkSource("if (null) { let x = 1; }", 1);
}

test "sound: tracked const number in if fails" {
    try checkSource("const count = 42; if (count) { let x = 1; }", 1);
}

// S2: && and || operands

test "sound: number operands for && fails" {
    try checkSource("const r = 1 && 2;", 2);
}

test "sound: string operands for || fails" {
    try checkSource("const r = \"a\" || \"b\";", 2);
}

// S3: ! operand

test "sound: !0 fails" {
    try checkSource("const r = !0;", 1);
}

test "sound: !string fails" {
    try checkSource("const r = !\"str\";", 1);
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

test "sound: nested if with mixed types" {
    try checkSource(
        \\const flag = true;
        \\if (flag) {
        \\  const count = 5;
        \\  if (count) { let x = 1; }
        \\}
    , 1);
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

test "sound: tracked let number in if fails" {
    try checkSource("let count = 0; if (count) { let x = 1; }", 1);
}

test "sound: let reassigned to boolean passes" {
    try checkSource(
        \\let flag = 0;
        \\flag = 1 > 0;
        \\if (flag) { let x = 1; }
    , 0);
}

test "sound: let reassigned to number fails" {
    try checkSource(
        \\let flag = true;
        \\flag = 42;
        \\if (flag) { let x = 1; }
    , 1);
}

// Diagnostic context messages

// Function return type inference

test "sound: arrow function returning boolean - call site passes" {
    try checkSource(
        \\const isPositive = (n) => n > 0;
        \\if (isPositive(5)) { let x = 1; }
    , 0);
}

test "sound: arrow function returning number - call site fails" {
    try checkSource(
        \\const double = (n) => n * 2;
        \\if (double(5)) { let x = 1; }
    , 1);
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

test "sound: diagnostic includes operator context" {
    const allocator = std.testing.allocator;

    var parser = @import("parser/parse.zig").Parser.init(allocator, "if (0) { let x = 1; }");
    defer parser.deinit();

    const root = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var checker = BoolChecker.init(allocator, ir_view, null);
    defer checker.deinit();

    _ = try checker.check(root);
    const diags = checker.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 1), diags.len);
    // Message should include the operator context
    try std.testing.expect(std.mem.indexOf(u8, diags[0].message, "number") != null);
    try std.testing.expect(std.mem.indexOf(u8, diags[0].message, "if") != null);
}
