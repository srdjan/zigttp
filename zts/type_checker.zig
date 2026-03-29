//! Type Checker: validates types across the IR tree.
//!
//! Walks the IR tree produced by the parser, consulting the TypeEnv (populated
//! from the stripper's TypeMap) to check:
//! - Variable declarations match their type annotations
//! - Function call arguments match parameter types
//! - Property access on known record types
//! - Return values match declared return types
//! - Union types require narrowing before type-specific operations
//!
//! Architecture follows bool_checker.zig: a struct with walkStmt/walkExpr/inferType
//! methods that produces diagnostics.
//!
//! When a type cannot be determined statically, null_type_idx is returned (escape hatch).
//! Only annotated values are checked - unannotated code passes through unchecked.

const std = @import("std");
const ir = @import("parser/ir.zig");
const object = @import("object.zig");
const context = @import("context.zig");
const type_pool_mod = @import("type_pool.zig");
const type_env_mod = @import("type_env.zig");
const bool_checker_mod = @import("bool_checker.zig");
const match_analysis_mod = @import("match_analysis.zig");

const Node = ir.Node;
const NodeIndex = ir.NodeIndex;
const NodeTag = ir.NodeTag;
const IrView = ir.IrView;
const null_node = ir.null_node;
const TypePool = type_pool_mod.TypePool;
const TypeIndex = type_pool_mod.TypeIndex;
const null_type_idx = type_pool_mod.null_type_idx;
const TypeEnv = type_env_mod.TypeEnv;

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
    type_mismatch, // declared type != inferred type
    missing_field, // property access on record missing field
    arg_count_mismatch, // wrong number of arguments
    arg_type_mismatch, // argument type doesn't match parameter
    return_type_mismatch, // return value doesn't match declared return type
    non_exhaustive_match, // match is not provably exhaustive
};

pub const Diagnostic = struct {
    severity: Severity,
    kind: DiagnosticKind,
    node: NodeIndex,
    message: []const u8,
    help: ?[]const u8,
    /// Whether the message was dynamically allocated.
    allocated: bool = false,
};

// ---------------------------------------------------------------------------
// Type Checker
// ---------------------------------------------------------------------------

pub const TypeChecker = struct {
    allocator: std.mem.Allocator,
    ir_view: IrView,
    atoms: ?*context.AtomTable,
    env: *TypeEnv,
    diagnostics: std.ArrayListUnmanaged(Diagnostic),

    /// Inferred types for const/let bindings: packed(scope_id, slot) -> TypeIndex
    binding_types: std.AutoHashMapUnmanaged(u32, TypeIndex),
    /// Flow-sensitive narrowing: binding key -> narrowed TypeIndex
    narrowed: std.AutoHashMapUnmanaged(u32, TypeIndex),
    /// Track current function's declared return type for return statement checking
    current_return_type: TypeIndex = null_type_idx,

    pub fn init(allocator: std.mem.Allocator, ir_view: IrView, atoms: ?*context.AtomTable, env: *TypeEnv) TypeChecker {
        return .{
            .allocator = allocator,
            .ir_view = ir_view,
            .atoms = atoms,
            .env = env,
            .diagnostics = .empty,
            .binding_types = .empty,
            .narrowed = .empty,
        };
    }

    pub fn deinit(self: *TypeChecker) void {
        for (self.diagnostics.items) |diag| {
            if (diag.allocated) {
                self.allocator.free(diag.message);
            }
        }
        self.diagnostics.deinit(self.allocator);
        self.binding_types.deinit(self.allocator);
        self.narrowed.deinit(self.allocator);
    }

    /// Run the checker on the given root node. Returns the number of errors.
    pub fn check(self: *TypeChecker, root: NodeIndex) !u32 {
        self.walkStmt(root);
        var error_count: u32 = 0;
        for (self.diagnostics.items) |diag| {
            if (diag.severity == .err) error_count += 1;
        }
        return error_count;
    }

    pub fn getDiagnostics(self: *const TypeChecker) []const Diagnostic {
        return self.diagnostics.items;
    }

    // -------------------------------------------------------------------
    // Diagnostic formatting
    // -------------------------------------------------------------------

    pub fn formatDiagnostics(
        self: *const TypeChecker,
        source: []const u8,
        writer: anytype,
    ) !void {
        for (self.diagnostics.items) |diag| {
            const loc = self.ir_view.getLoc(diag.node) orelse continue;

            try writer.print("type {s}: {s}\n", .{ diag.severity.label(), diag.message });
            try writer.print("  --> {d}:{d}\n", .{ loc.line, loc.column });

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

            if (diag.help) |help| {
                try writer.print("   = help: {s}\n", .{help});
            }
            try writer.writeByte('\n');
        }
    }

    // -------------------------------------------------------------------
    // Statement walking
    // -------------------------------------------------------------------

    fn walkStmt(self: *TypeChecker, node: NodeIndex) void {
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

            .var_decl => {
                const vd = self.ir_view.getVarDecl(node) orelse return;
                if (vd.init != null_node) {
                    self.walkExpr(vd.init);

                    // Check: does the initializer type match the declared type?
                    const binding = vd.binding;
                    const key = packBindingKey(binding.scope_id, binding.slot);
                    const inferred = self.inferType(vd.init);

                    const loc = self.ir_view.getLoc(node);
                    const declared = if (loc) |l|
                        (self.env.getVarTypeByLoc(l.line, l.column) orelse blk: {
                            const name = self.resolveAtomName(binding.slot);
                            break :blk if (name) |n| self.env.getVarTypeByName(n) orelse null_type_idx else null_type_idx;
                        })
                    else blk: {
                        const name = self.resolveAtomName(binding.slot);
                        break :blk if (name) |n| self.env.getVarTypeByName(n) orelse null_type_idx else null_type_idx;
                    };

                    if (declared != null_type_idx and inferred != null_type_idx) {
                        if (!self.env.pool.isAssignableTo(inferred, declared)) {
                            self.addTypeMismatch(node, declared, inferred);
                        }
                    }

                    // Track inferred type for use in later expressions
                    const effective = if (declared != null_type_idx) declared else inferred;
                    if (effective != null_type_idx) {
                        self.binding_types.put(self.allocator, key, effective) catch {};
                    }
                }
            },

            .if_stmt => {
                const if_s = self.ir_view.getIfStmt(node) orelse return;
                self.walkExpr(if_s.condition);
                self.walkStmt(if_s.then_branch);
                if (if_s.else_branch != null_node) {
                    self.walkStmt(if_s.else_branch);
                }
            },

            .return_stmt => {
                if (self.ir_view.getOptValue(node)) |ret_val| {
                    self.walkExpr(ret_val);
                    // Check return type against declared return type
                    if (self.current_return_type != null_type_idx) {
                        const inferred = self.inferType(ret_val);
                        if (inferred != null_type_idx and !self.env.pool.isAssignableTo(inferred, self.current_return_type)) {
                            self.addDiagnostic(.{
                                .severity = .err,
                                .kind = .return_type_mismatch,
                                .node = node,
                                .message = "return type does not match declared return type",
                                .help = null,
                            });
                        }
                    }
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
                const decl = self.ir_view.getVarDecl(node) orelse return;
                const func = self.ir_view.getFunction(decl.init) orelse return;
                // Look up function signature from TypeEnv
                const loc = self.ir_view.getLoc(node);
                const sig = if (loc) |l| self.env.getFnSigByLoc(l.line) else null;
                const saved_return = self.current_return_type;
                if (sig) |s| {
                    self.current_return_type = s.return_type;
                }
                self.walkStmt(func.body);
                self.current_return_type = saved_return;
            },

            .function_expr, .arrow_function => {
                const func = self.ir_view.getFunction(node) orelse return;
                const saved_return = self.current_return_type;
                self.walkStmt(func.body);
                self.current_return_type = saved_return;
            },

            .export_default => {
                if (self.ir_view.getOptValue(node)) |val| {
                    self.walkStmt(val);
                }
            },

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

    // -------------------------------------------------------------------
    // Expression walking
    // -------------------------------------------------------------------

    fn walkExpr(self: *TypeChecker, node: NodeIndex) void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;

        switch (tag) {
            .call => {
                const c = self.ir_view.getCall(node) orelse return;
                self.walkExpr(c.callee);
                // Check argument types against function signature
                self.checkCallArgs(node, c);
                for (0..c.args_count) |i| {
                    const arg = self.ir_view.getListIndex(c.args_start, @intCast(i));
                    self.walkExpr(arg);
                }
            },

            .binary_op => {
                const bin = self.ir_view.getBinary(node) orelse return;
                self.walkExpr(bin.left);
                self.walkExpr(bin.right);
            },

            .unary_op => {
                const un = self.ir_view.getUnary(node) orelse return;
                self.walkExpr(un.operand);
            },

            .ternary => {
                const t = self.ir_view.getTernary(node) orelse return;
                self.walkExpr(t.condition);
                self.walkExpr(t.then_branch);
                self.walkExpr(t.else_branch);
            },

            .method_call => {
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
                // Check assignment type matches target's declared type
                const target_tag = self.ir_view.getTag(asgn.target) orelse return;
                if (target_tag == .identifier) {
                    const binding = self.ir_view.getBinding(asgn.target) orelse return;
                    const key = packBindingKey(binding.scope_id, binding.slot);
                    if (self.binding_types.get(key)) |declared| {
                        if (asgn.op == null) {
                            const val_type = self.inferType(asgn.value);
                            if (val_type != null_type_idx and !self.env.pool.isAssignableTo(val_type, declared)) {
                                self.addTypeMismatch(node, declared, val_type);
                            }
                        }
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
                self.walkMatchWithNarrowing(me);
                if (!self.isMatchExhaustive(me)) {
                    self.addDiagnostic(.{
                        .severity = .warning,
                        .kind = .non_exhaustive_match,
                        .node = node,
                        .message = "match expression is not provably exhaustive",
                        .help = "add 'default:' or 'when _:' arm, or cover every union variant",
                    });
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

            .function_expr, .arrow_function => {
                const func = self.ir_view.getFunction(node) orelse return;
                self.walkStmt(func.body);
            },

            else => {},
        }
    }

    // -------------------------------------------------------------------
    // Type inference (TypePool-based)
    // -------------------------------------------------------------------

    /// Infer the TypeIndex of an expression. Returns null_type_idx for unknown.
    pub fn inferType(self: *const TypeChecker, node: NodeIndex) TypeIndex {
        if (node == null_node) return null_type_idx;
        const tag = self.ir_view.getTag(node) orelse return null_type_idx;
        const pool = self.env.pool;

        return switch (tag) {
            .lit_bool => blk: {
                const val = self.ir_view.getBoolValue(node) orelse break :blk pool.idx_boolean;
                break :blk pool.addLiteralBool(self.allocator, val);
            },
            .lit_int => blk: {
                const val = self.ir_view.getIntValue(node) orelse break :blk pool.idx_number;
                const int_val = std.math.cast(i16, val) orelse break :blk pool.idx_number;
                break :blk pool.addLiteralNumber(self.allocator, int_val);
            },
            .lit_float => pool.idx_number,
            .lit_string => blk: {
                const str_idx = self.ir_view.getStringIdx(node) orelse break :blk pool.idx_string;
                const str = self.ir_view.getString(str_idx) orelse break :blk pool.idx_string;
                break :blk pool.addLiteralString(self.allocator, str);
            },
            .template_literal => pool.idx_string,
            .lit_null => pool.idx_undefined, // parser rejects null, but map defensively
            .lit_undefined => pool.idx_undefined,
            .object_literal => self.inferObjectLiteralType(node),
            .array_literal => self.inferArrayLiteralType(node),
            .function_expr, .arrow_function, .function_decl => null_type_idx, // Function types handled via signatures

            .binary_op => self.inferBinaryType(node),
            .unary_op => self.inferUnaryType(node),

            .ternary => {
                const t = self.ir_view.getTernary(node) orelse return null_type_idx;
                const then_type = self.inferType(t.then_branch);
                const else_type = self.inferType(t.else_branch);
                if (then_type == else_type) return then_type;
                if (then_type == null_type_idx) return else_type;
                if (else_type == null_type_idx) return then_type;
                // If both are known but different, create a union
                return pool.addUnion(self.allocator, &.{ then_type, else_type });
            },

            .identifier => {
                const binding = self.ir_view.getBinding(node) orelse return null_type_idx;
                const key = packBindingKey(binding.scope_id, binding.slot);
                // Check narrowing first
                if (self.narrowed.get(key)) |t| return t;
                // Check tracked binding types
                if (self.binding_types.get(key)) |t| return t;
                // Check by name in the environment
                const name = self.resolveAtomName(binding.slot);
                if (name) |n| {
                    if (self.env.getVarTypeByName(n)) |t| return t;
                }
                return null_type_idx;
            },

            .member_access => self.inferMemberAccessType(node),

            .call => self.inferCallType(node),

            .match_expr => self.inferMatchType(node),

            else => null_type_idx,
        };
    }

    fn inferBinaryType(self: *const TypeChecker, node: NodeIndex) TypeIndex {
        const bin = self.ir_view.getBinary(node) orelse return null_type_idx;
        const pool = self.env.pool;

        return switch (bin.op) {
            .strict_eq, .strict_neq, .lt, .lte, .gt, .gte, .in_op, .eq, .neq => pool.idx_boolean,
            .and_op, .or_op => pool.idx_boolean,
            .sub, .mul, .div, .mod, .pow => pool.idx_number,
            .bit_and, .bit_or, .bit_xor, .shl, .shr, .ushr => pool.idx_number,
            .add => {
                const lt = self.inferType(bin.left);
                const rt = self.inferType(bin.right);
                if (lt == pool.idx_string or rt == pool.idx_string) return pool.idx_string;
                if (lt == pool.idx_number and rt == pool.idx_number) return pool.idx_number;
                return null_type_idx;
            },
            .nullish => {
                const lt = self.inferType(bin.left);
                const rt = self.inferType(bin.right);
                const lt_tag = pool.getTag(lt);
                if (lt_tag == .t_nullable) {
                    return pool.getNullableInner(lt);
                }
                if (lt_tag == .t_undefined) return rt;
                if (lt != null_type_idx) return lt; // non-nullable ?? anything -> left
                return null_type_idx;
            },
        };
    }

    fn inferUnaryType(self: *const TypeChecker, node: NodeIndex) TypeIndex {
        const un = self.ir_view.getUnary(node) orelse return null_type_idx;
        const pool = self.env.pool;

        return switch (un.op) {
            .not => pool.idx_boolean,
            .neg, .pos, .bit_not => pool.idx_number,
            .typeof_op => pool.idx_string,
            .void_op => pool.idx_undefined,
        };
    }

    fn inferObjectLiteralType(self: *const TypeChecker, node: NodeIndex) TypeIndex {
        const obj = self.ir_view.getObject(node) orelse return null_type_idx;
        if (obj.properties_count == 0) return null_type_idx;

        var fields_buf: [32]type_pool_mod.RecordField = undefined;
        var count: usize = 0;

        for (0..obj.properties_count) |i| {
            if (count >= 32) break;
            const prop_idx = self.ir_view.getListIndex(obj.properties_start, @intCast(i));
            const prop = self.ir_view.getProperty(prop_idx) orelse continue;
            // The key is a node index (identifier, string, or computed); extract atom from it
            const key_tag = self.ir_view.getTag(prop.key) orelse continue;
            const prop_name = if (key_tag == .identifier)
                (if (self.ir_view.getBinding(prop.key)) |b| self.resolveAtomName(b.slot) else null)
            else if (key_tag == .lit_string)
                (if (self.ir_view.getStringIdx(prop.key)) |si| self.ir_view.getString(si) else null)
            else
                null;
            const prop_name_str = prop_name orelse continue;
            const val_type = self.inferType(prop.value);
            const n = self.env.pool.addName(self.allocator, prop_name_str);
            fields_buf[count] = .{
                .name_start = n.start,
                .name_len = n.len,
                .type_idx = val_type,
                .optional = false,
            };
            count += 1;
        }

        if (count == 0) return null_type_idx;
        return self.env.pool.addRecord(self.allocator, fields_buf[0..count]);
    }

    fn inferArrayLiteralType(self: *const TypeChecker, node: NodeIndex) TypeIndex {
        const arr = self.ir_view.getArray(node) orelse return null_type_idx;
        if (arr.elements_count == 0) return null_type_idx;

        var element_types: [32]TypeIndex = undefined;
        var count: usize = 0;

        for (0..arr.elements_count) |i| {
            if (count >= element_types.len) return null_type_idx;
            const elem_idx = self.ir_view.getListIndex(arr.elements_start, @intCast(i));
            const elem_type = self.inferType(elem_idx);
            if (elem_type == null_type_idx) return null_type_idx;
            element_types[count] = elem_type;
            count += 1;
        }

        const first = element_types[0];
        var homogeneous = true;
        for (element_types[1..count]) |elem_type| {
            if (elem_type != first) {
                homogeneous = false;
                break;
            }
        }

        if (homogeneous) {
            return self.env.pool.addArray(self.allocator, first);
        }
        return self.env.pool.addTuple(self.allocator, element_types[0..count]);
    }

    fn inferMemberAccessType(self: *const TypeChecker, node: NodeIndex) TypeIndex {
        const member = self.ir_view.getMember(node) orelse return null_type_idx;
        const obj_type = self.inferType(member.object);
        if (obj_type == null_type_idx) return null_type_idx;

        const prop_name = self.resolveAtomName(member.property) orelse return null_type_idx;

        // Look up field in record type
        const tag = self.env.pool.getTag(obj_type) orelse return null_type_idx;
        if (tag == .t_record) {
            for (self.env.pool.getRecordFields(obj_type)) |field| {
                const field_name = self.env.pool.getName(field.name_start, field.name_len);
                if (std.mem.eql(u8, field_name, prop_name)) {
                    return field.type_idx;
                }
            }
            // Field not found on known record type
            @constCast(self).addDiagnostic(.{
                .severity = .err,
                .kind = .missing_field,
                .node = node,
                .message = "property does not exist on type",
                .help = null,
            });
            return null_type_idx;
        }

        return null_type_idx;
    }

    fn inferCallType(self: *const TypeChecker, node: NodeIndex) TypeIndex {
        const call = self.ir_view.getCall(node) orelse return null_type_idx;
        const callee_tag = self.ir_view.getTag(call.callee) orelse return null_type_idx;
        if (callee_tag != .identifier) return null_type_idx;

        const binding = self.ir_view.getBinding(call.callee) orelse return null_type_idx;
        const name = self.resolveAtomName(binding.slot) orelse return null_type_idx;
        const sig = self.env.getFnSigByName(name) orelse return null_type_idx;
        return sig.return_type;
    }

    fn inferMatchType(self: *const TypeChecker, node: NodeIndex) TypeIndex {
        const me = self.ir_view.getMatchExpr(node) orelse return null_type_idx;
        if (me.arms_count == 0) return null_type_idx;

        var result: TypeIndex = null_type_idx;
        for (0..me.arms_count) |i| {
            const arm_idx = self.ir_view.getListIndex(me.arms_start, @intCast(i));
            const arm = self.ir_view.getMatchArm(arm_idx) orelse continue;
            const arm_type = self.inferType(arm.body);
            if (result == null_type_idx) {
                result = arm_type;
            } else if (arm_type != null_type_idx and arm_type != result) {
                // Different arm types - create union
                result = self.env.pool.addUnion(self.allocator, &.{ result, arm_type });
            }
        }
        return result;
    }

    // -------------------------------------------------------------------
    // Match expression narrowing
    // -------------------------------------------------------------------

    fn walkMatchWithNarrowing(self: *TypeChecker, me: ir.Node.MatchExpr) void {
        // Check if discriminant is an identifier with a union type
        const disc_tag = self.ir_view.getTag(me.discriminant) orelse return;
        const disc_key: ?u32 = if (disc_tag == .identifier) blk: {
            const binding = self.ir_view.getBinding(me.discriminant) orelse break :blk null;
            break :blk packBindingKey(binding.scope_id, binding.slot);
        } else null;

        const disc_type = if (disc_key) |key| (self.narrowed.get(key) orelse self.binding_types.get(key) orelse null_type_idx) else null_type_idx;
        const pool = self.env.pool;
        const is_union = if (pool.getTag(disc_type)) |tag| tag == .t_union else false;

        for (0..me.arms_count) |i| {
            const arm_idx = self.ir_view.getListIndex(me.arms_start, @intCast(i));
            const arm = self.ir_view.getMatchArm(arm_idx) orelse continue;

            if (is_union and disc_key != null and arm.pattern != ir.null_node) {
                const narrowed_type = self.findUnionMemberForPattern(disc_type, arm.pattern);
                if (narrowed_type != null_type_idx) {
                    const saved = self.narrowed.get(disc_key.?);
                    self.narrowed.put(self.allocator, disc_key.?, narrowed_type) catch {};
                    self.walkExpr(arm.body);
                    if (saved) |s| {
                        self.narrowed.put(self.allocator, disc_key.?, s) catch {};
                    } else {
                        _ = self.narrowed.remove(disc_key.?);
                    }
                    continue;
                }
            }
            self.walkExpr(arm.body);
        }
    }

    fn findUnionMemberForPattern(self: *const TypeChecker, union_type: TypeIndex, pattern: ir.NodeIndex) TypeIndex {
        const analysis = match_analysis_mod.MatchAnalysis.init(self.allocator, self.ir_view, self.env.pool);
        return analysis.narrowTypeForPattern(union_type, pattern);
    }

    fn isMatchExhaustive(self: *const TypeChecker, me: ir.Node.MatchExpr) bool {
        const disc_type = self.inferType(me.discriminant);
        if (disc_type == null_type_idx) return false;
        const analysis = match_analysis_mod.MatchAnalysis.init(self.allocator, self.ir_view, self.env.pool);
        return analysis.isMatchExhaustive(disc_type, me);
    }

    // -------------------------------------------------------------------
    // Call argument checking
    // -------------------------------------------------------------------

    fn checkCallArgs(self: *TypeChecker, node: NodeIndex, call: Node.CallExpr) void {
        // Resolve callee to a function signature
        const callee_tag = self.ir_view.getTag(call.callee) orelse return;
        if (callee_tag != .identifier) return;

        const binding = self.ir_view.getBinding(call.callee) orelse return;
        const name = self.resolveAtomName(binding.slot) orelse return;

        const sig = self.env.getFnSigByName(name) orelse return;

        // Check argument count
        if (call.args_count < sig.param_count) {
            // Count required params (non-optional)
            // For now, treat all params as required
            self.addDiagnostic(.{
                .severity = .err,
                .kind = .arg_count_mismatch,
                .node = node,
                .message = "wrong number of arguments",
                .help = null,
            });
            return;
        }

        // Check argument types
        var i: u8 = 0;
        while (i < sig.param_count and i < call.args_count) : (i += 1) {
            const arg_idx = self.ir_view.getListIndex(call.args_start, i);
            const arg_type = self.inferType(arg_idx);
            if (arg_type != null_type_idx and sig.param_types[i] != null_type_idx) {
                if (!self.env.pool.isAssignableTo(arg_type, sig.param_types[i])) {
                    self.addDiagnostic(.{
                        .severity = .err,
                        .kind = .arg_type_mismatch,
                        .node = arg_idx,
                        .message = "argument type does not match parameter type",
                        .help = null,
                    });
                }
            }
        }
    }

    // -------------------------------------------------------------------
    // Diagnostic helpers
    // -------------------------------------------------------------------

    fn addDiagnostic(self: *TypeChecker, diag: Diagnostic) void {
        self.diagnostics.append(self.allocator, diag) catch {};
    }

    fn addTypeMismatch(self: *TypeChecker, node: NodeIndex, expected: TypeIndex, got: TypeIndex) void {
        var buf: [256]u8 = undefined;
        const expected_str = self.env.pool.formatType(expected, buf[0..128]);
        const got_str = self.env.pool.formatType(got, buf[128..256]);

        const msg = std.fmt.allocPrint(self.allocator, "type '{s}' is not assignable to type '{s}'", .{ got_str, expected_str }) catch "type mismatch";

        self.addDiagnostic(.{
            .severity = .err,
            .kind = .type_mismatch,
            .node = node,
            .message = msg,
            .help = null,
            .allocated = true,
        });
    }

    fn resolveAtomName(self: *const TypeChecker, atom_idx: u16) ?[]const u8 {
        if (self.atoms) |table| {
            const atom: object.Atom = @enumFromInt(atom_idx);
            if (atom.toPredefinedName()) |name| return name;
            return table.getName(atom);
        }
        if (self.ir_view.getString(atom_idx)) |name| return name;
        const atom: object.Atom = @enumFromInt(atom_idx);
        return atom.toPredefinedName();
    }
};

const packBindingKey = bool_checker_mod.packBindingKey;
const getSourceLine = bool_checker_mod.getSourceLine;

fn checkTypedSource(source: []const u8, expect_errors: u32, expect_warnings: ?u32) !void {
    const allocator = std.testing.allocator;

    var strip_result = try @import("stripper.zig").strip(allocator, source, .{});
    defer strip_result.deinit();

    var parser = @import("parser/parse.zig").Parser.init(allocator, strip_result.code);
    defer parser.deinit();

    const root = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);

    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();
    @import("modules/root.zig").populateModuleTypes(&env, &pool, allocator);
    env.populateFromTypeMap(&strip_result.type_map);

    var checker = TypeChecker.init(allocator, ir_view, null, &env);
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

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "TypeChecker init and deinit" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    // Create a minimal IR using NodeList
    var node_list = ir.NodeList.init(allocator);
    defer node_list.deinit();
    var constants = ir.ConstantPool.init(allocator);
    defer constants.deinit();

    // Add a program node with an empty block
    _ = node_list.add(.{
        .tag = .program,
        .loc = .{ .line = 1, .column = 1, .offset = 0 },
        .data = .{ .binary = .{ .op = .add, .left = null_node, .right = null_node } },
    }) catch {};

    const view = ir.IrView.fromNodeList(&node_list, &constants);

    var checker = TypeChecker.init(allocator, view, null, &env);
    defer checker.deinit();

    try std.testing.expectEqual(@as(usize, 0), checker.diagnostics.items.len);
}

test "TypeChecker proves literal union match exhaustiveness" {
    try checkTypedSource(
        \\const value: "a" | "b" = "a";
        \\const out = match (value) {
        \\  when "a": 1,
        \\  when "b": 2,
        \\};
    , 0, 0);
}

test "TypeChecker proves discriminated union match exhaustiveness" {
    try checkTypedSource(
        \\const ok = { kind: "ok", value: "x" };
        \\const err = { kind: "err", error: "bad" };
        \\const result = true ? ok : err;
        \\const out = match (result) {
        \\  when { kind: "ok" }: result.value,
        \\  when { kind: "err" }: result.error,
        \\};
    , 0, 0);
}

test "TypeChecker warns on non-exhaustive match over union" {
    try checkTypedSource(
        \\const value: "a" | "b" = "a";
        \\const out = match (value) {
        \\  when "a": 1,
        \\};
    , 0, 1);
}
