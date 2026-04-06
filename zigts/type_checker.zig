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
const json_utils = @import("json_utils.zig");
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

/// Max fields/enum members tracked per schema type. Schemas with more are silently truncated.
const MAX_SCHEMA_FIELDS = 32;

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
    const CompiledSchemaType = struct {
        name: []const u8,
        type_idx: TypeIndex,
    };

    allocator: std.mem.Allocator,
    ir_view: IrView,
    atoms: ?*context.AtomTable,
    env: *TypeEnv,
    diagnostics: std.ArrayListUnmanaged(Diagnostic),
    compiled_schemas: std.ArrayListUnmanaged(CompiledSchemaType),

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
            .compiled_schemas = .empty,
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
        for (self.compiled_schemas.items) |entry| {
            self.allocator.free(entry.name);
        }
        self.compiled_schemas.deinit(self.allocator);
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

                    // Track inferred type for use in later expressions.
                    // When annotation is a base primitive (number, string, boolean) and
                    // the inferred type is the corresponding literal, keep the narrower
                    // literal. This gives `: Type` satisfies-like semantics for primitives.
                    // For unions and compound annotations, keep the declared type since
                    // the annotation carries semantic intent for exhaustiveness checking.
                    // For let bindings without explicit type annotations, widen
                    // literal types to their base type so reassignment works.
                    var effective = blk: {
                        if (declared != null_type_idx and inferred != null_type_idx) {
                            const dt = self.env.pool.getTag(declared) orelse break :blk declared;
                            const it = self.env.pool.getTag(inferred) orelse break :blk declared;
                            const is_literal_of_base =
                                (dt == .t_number and it == .t_literal_number) or
                                (dt == .t_string and it == .t_literal_string) or
                                (dt == .t_boolean and it == .t_literal_bool);
                            break :blk if (is_literal_of_base) inferred else declared;
                        }
                        break :blk if (declared != null_type_idx) declared else inferred;
                    };
                    if (effective != null_type_idx and declared == null_type_idx and vd.kind == .let) {
                        effective = self.env.pool.widenLiteral(effective);
                    }
                    if (effective != null_type_idx) {
                        self.binding_types.put(self.allocator, key, effective) catch {};
                    }
                }
            },

            .if_stmt => {
                const if_s = self.ir_view.getIfStmt(node) orelse return;
                self.walkExpr(if_s.condition);

                // Narrow nullable types in then-branch when condition is a
                // simple guard: if (x), if (!x), if (x !== undefined).
                const narrow = self.extractNarrowingGuard(if_s.condition);
                if (narrow.key != 0 and narrow.narrowed_type != null_type_idx) {
                    const saved = self.binding_types.get(narrow.key);
                    self.binding_types.put(self.allocator, narrow.key, narrow.narrowed_type) catch {};
                    self.walkStmt(if_s.then_branch);
                    if (saved) |s| {
                        self.binding_types.put(self.allocator, narrow.key, s) catch {};
                    } else {
                        _ = self.binding_types.remove(narrow.key);
                    }

                    // Forward narrowing after early return:
                    // if (!x) { return; } narrows x to non-null after the block
                    // if (x.kind === "err") { return; } narrows x to excluded union
                    if (if_s.else_branch == null_node and self.branchAlwaysReturns(if_s.then_branch)) {
                        if (narrow.negated) {
                            self.binding_types.put(self.allocator, narrow.key, narrow.narrowed_type) catch {};
                        } else if (narrow.else_type != null_type_idx) {
                            self.binding_types.put(self.allocator, narrow.key, narrow.else_type) catch {};
                        }
                    }
                } else {
                    self.walkStmt(if_s.then_branch);
                }

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

            .assert_stmt => {
                // Extract narrowing guard from the assert condition and install
                // it as permanent forward narrowing (no restore after).
                const assert = self.ir_view.getAssertStmt(node) orelse return;
                self.walkExpr(assert.condition);
                if (assert.error_expr != null_node) {
                    self.walkExpr(assert.error_expr);
                }
                const narrow = self.extractNarrowingGuard(assert.condition);
                if (narrow.key != 0 and narrow.narrowed_type != null_type_idx and !narrow.negated) {
                    self.binding_types.put(self.allocator, narrow.key, narrow.narrowed_type) catch {};
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
                self.collectSchemaCompileCall(c) catch {};
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
                } else if (target_tag == .member_access) {
                    // Check readonly field assignment
                    const member = self.ir_view.getMember(asgn.target) orelse return;
                    const obj_tag = self.ir_view.getTag(member.object) orelse return;
                    if (obj_tag == .identifier) {
                        const binding = self.ir_view.getBinding(member.object) orelse return;
                        const key = packBindingKey(binding.scope_id, binding.slot);
                        if (self.binding_types.get(key)) |obj_type| {
                            const prop_name = self.resolveAtomName(member.property) orelse return;
                            if (self.env.pool.lookupRecordField(obj_type, prop_name)) |field| {
                                if (field.readonly) {
                                    self.addDiagnostic(.{
                                        .severity = .err,
                                        .kind = .type_mismatch,
                                        .node = node,
                                        .message = "cannot assign to readonly property",
                                        .help = null,
                                    });
                                }
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

    fn collectSchemaCompileCall(self: *TypeChecker, call: Node.CallExpr) !void {
        const callee_tag = self.ir_view.getTag(call.callee) orelse return;
        if (callee_tag != .identifier or call.args_count < 2) return;

        const binding = self.ir_view.getBinding(call.callee) orelse return;
        const name = self.resolveAtomName(binding.slot) orelse return;
        if (!std.mem.eql(u8, name, "schemaCompile")) return;

        const schema_name_node = self.ir_view.getListIndex(call.args_start, 0);
        const schema_name = self.getLiteralString(schema_name_node) orelse return;
        const schema_json_node = self.ir_view.getListIndex(call.args_start, 1);
        const schema_json = (try self.extractSchemaJson(schema_json_node)) orelse return;
        defer self.allocator.free(schema_json);

        const type_idx = try self.schemaJsonToType(schema_json);
        if (type_idx == null_type_idx) return;

        for (self.compiled_schemas.items) |*entry| {
            if (!std.mem.eql(u8, entry.name, schema_name)) continue;
            entry.type_idx = type_idx;
            return;
        }

        try self.compiled_schemas.append(self.allocator, .{
            .name = try self.allocator.dupe(u8, schema_name),
            .type_idx = type_idx,
        });
    }

    fn schemaJsonToType(self: *TypeChecker, schema_json: []const u8) !TypeIndex {
        var parsed = std.json.parseFromSlice(std.json.Value, self.allocator, schema_json, .{}) catch return null_type_idx;
        defer parsed.deinit();
        return self.schemaValueToType(parsed.value);
    }

    fn schemaValueToType(self: *TypeChecker, value_json: std.json.Value) TypeIndex {
        const pool = self.env.pool;
        const obj = switch (value_json) {
            .object => |o| o,
            else => return pool.idx_unknown,
        };

        if (obj.get("enum")) |enum_val| {
            if (enum_val == .array and enum_val.array.items.len > 0) {
                var members: [MAX_SCHEMA_FIELDS]TypeIndex = undefined;
                var count: usize = 0;
                for (enum_val.array.items) |item| {
                    if (count >= members.len) break;
                    const member = switch (item) {
                        .string => |s| pool.addLiteralString(self.allocator, s),
                        .integer => |i| blk: {
                            const lit = std.math.cast(i16, i) orelse break :blk pool.idx_number;
                            break :blk pool.addLiteralNumber(self.allocator, lit);
                        },
                        .float => pool.idx_number,
                        .bool => |b| pool.addLiteralBool(self.allocator, b),
                        else => pool.idx_unknown,
                    };
                    members[count] = member;
                    count += 1;
                }
                if (count > 0) return pool.addUnion(self.allocator, members[0..count]);
            }
        }

        const type_name = if (obj.get("type")) |type_val|
            switch (type_val) {
                .string => |s| s,
                else => "",
            }
        else
            "";

        if (std.mem.eql(u8, type_name, "string")) return pool.idx_string;
        if (std.mem.eql(u8, type_name, "number")) return pool.idx_number;
        if (std.mem.eql(u8, type_name, "integer")) return pool.idx_number;
        if (std.mem.eql(u8, type_name, "boolean")) return pool.idx_boolean;
        if (std.mem.eql(u8, type_name, "array")) {
            if (obj.get("items")) |items| {
                const elem = self.schemaValueToType(items);
                return pool.addArray(self.allocator, if (elem == null_type_idx) pool.idx_unknown else elem);
            }
            return pool.addArray(self.allocator, pool.idx_unknown);
        }

        if (std.mem.eql(u8, type_name, "object") or obj.get("properties") != null) {
            const props = obj.get("properties") orelse return pool.idx_unknown;
            if (props != .object) return pool.idx_unknown;

            var required: std.ArrayList([]const u8) = .empty;
            defer required.deinit(self.allocator);
            if (obj.get("required")) |required_val| {
                if (required_val == .array) {
                    for (required_val.array.items) |item| {
                        if (item != .string) continue;
                        required.append(self.allocator, item.string) catch {};
                    }
                }
            }

            var fields: [MAX_SCHEMA_FIELDS]type_pool_mod.RecordField = undefined;
            var count: usize = 0;
            var it = props.object.iterator();
            while (it.next()) |entry| {
                if (count >= fields.len) break;
                const prop_type = self.schemaValueToType(entry.value_ptr.*);
                const name = pool.addName(self.allocator, entry.key_ptr.*);
                fields[count] = .{
                    .name_start = name.start,
                    .name_len = name.len,
                    .type_idx = if (prop_type == null_type_idx) pool.idx_unknown else prop_type,
                    .optional = !json_utils.containsString(required.items, entry.key_ptr.*),
                };
                count += 1;
            }
            if (count == 0) return pool.idx_unknown;
            return pool.addRecord(self.allocator, fields[0..count]);
        }

        return pool.idx_unknown;
    }

    fn getLiteralString(self: *const TypeChecker, node_idx: NodeIndex) ?[]const u8 {
        const str_idx = self.ir_view.getStringIdx(node_idx) orelse return null;
        return self.ir_view.getString(str_idx);
    }

    fn getJsonStringifyArg(self: *const TypeChecker, node_idx: NodeIndex) ?NodeIndex {
        const call = self.ir_view.getCall(node_idx) orelse return null;
        if (call.args_count != 1) return null;

        const callee_tag = self.ir_view.getTag(call.callee) orelse return null;
        if (callee_tag != .member_access) return null;

        const member = self.ir_view.getMember(call.callee) orelse return null;
        if (member.property != @intFromEnum(object.Atom.stringify)) return null;

        const obj_tag = self.ir_view.getTag(member.object) orelse return null;
        if (obj_tag != .identifier) return null;

        const binding = self.ir_view.getBinding(member.object) orelse return null;
        if (binding.kind != .global and binding.kind != .undeclared_global) return null;
        if (binding.slot != @intFromEnum(object.Atom.JSON)) return null;

        return self.ir_view.getListIndex(call.args_start, 0);
    }

    fn extractSchemaJson(self: *TypeChecker, node_idx: NodeIndex) !?[]u8 {
        const tag = self.ir_view.getTag(node_idx) orelse return null;
        return switch (tag) {
            .lit_string => blk: {
                const raw = self.getLiteralString(node_idx) orelse break :blk null;
                break :blk try self.allocator.dupe(u8, raw);
            },
            .call => blk: {
                const json_arg = self.getJsonStringifyArg(node_idx) orelse break :blk null;
                break :blk try self.serializeJsonLiteral(json_arg);
            },
            else => null,
        };
    }

    fn serializeJsonLiteral(self: *TypeChecker, node_idx: NodeIndex) !?[]u8 {
        var output: std.ArrayList(u8) = .empty;
        errdefer output.deinit(self.allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(self.allocator, &output);
        const ok = try self.writeJsonLiteralNode(node_idx, &aw.writer);
        if (!ok) {
            output.deinit(self.allocator);
            return null;
        }
        output = aw.toArrayList();
        return try output.toOwnedSlice(self.allocator);
    }

    fn writeJsonLiteralNode(self: *TypeChecker, node_idx: NodeIndex, writer: anytype) !bool {
        const tag = self.ir_view.getTag(node_idx) orelse return false;
        switch (tag) {
            .lit_int => {
                const value_int = self.ir_view.getIntValue(node_idx) orelse return false;
                try writer.print("{d}", .{value_int});
                return true;
            },
            .lit_float => {
                const float_idx = self.ir_view.getFloatIdx(node_idx) orelse return false;
                const value_float = self.ir_view.getFloat(float_idx) orelse return false;
                try writer.print("{d}", .{value_float});
                return true;
            },
            .lit_string => {
                const str = self.getLiteralString(node_idx) orelse return false;
                try writeJsonString(writer, str);
                return true;
            },
            .lit_bool => {
                const value_bool = self.ir_view.getBoolValue(node_idx) orelse return false;
                try writer.writeAll(if (value_bool) "true" else "false");
                return true;
            },
            .lit_null => {
                try writer.writeAll("null");
                return true;
            },
            .unary_op => {
                const unary = self.ir_view.getUnary(node_idx) orelse return false;
                if (unary.op != .neg) return false;
                try writer.writeByte('-');
                return self.writeJsonLiteralNode(unary.operand, writer);
            },
            .array_literal => {
                const arr = self.ir_view.getArray(node_idx) orelse return false;
                try writer.writeByte('[');
                for (0..arr.elements_count) |i| {
                    if (i > 0) try writer.writeAll(", ");
                    if (!try self.writeJsonLiteralNode(self.ir_view.getListIndex(arr.elements_start, @intCast(i)), writer)) return false;
                }
                try writer.writeByte(']');
                return true;
            },
            .object_literal => {
                const obj = self.ir_view.getObject(node_idx) orelse return false;
                try writer.writeByte('{');
                for (0..obj.properties_count) |i| {
                    const prop_idx = self.ir_view.getListIndex(obj.properties_start, @intCast(i));
                    const prop = self.ir_view.getProperty(prop_idx) orelse return false;
                    const key = self.getObjectPropertyKey(prop.key) orelse return false;
                    if (i > 0) try writer.writeAll(", ");
                    try writeJsonString(writer, key);
                    try writer.writeAll(": ");
                    if (!try self.writeJsonLiteralNode(prop.value, writer)) return false;
                }
                try writer.writeByte('}');
                return true;
            },
            else => return false,
        }
    }

    fn getObjectPropertyKey(self: *const TypeChecker, node_idx: NodeIndex) ?[]const u8 {
        const tag = self.ir_view.getTag(node_idx) orelse return null;
        return switch (tag) {
            .lit_string => self.getLiteralString(node_idx),
            .identifier => blk: {
                const binding = self.ir_view.getBinding(node_idx) orelse break :blk null;
                break :blk self.resolveAtomName(binding.slot);
            },
            else => null,
        };
    }

    fn schemaTypeByName(self: *const TypeChecker, name: []const u8) ?TypeIndex {
        for (self.compiled_schemas.items) |entry| {
            if (std.mem.eql(u8, entry.name, name)) return entry.type_idx;
        }
        return null;
    }

    fn typedResultType(self: *const TypeChecker, inner: TypeIndex) TypeIndex {
        const pool = self.env.pool;
        const ok_name = pool.addName(self.allocator, "ok");
        const val_name = pool.addName(self.allocator, "value");
        const err_name = pool.addName(self.allocator, "error");
        const errs_name = pool.addName(self.allocator, "errors");
        return pool.addRecord(self.allocator, &.{
            .{ .name_start = ok_name.start, .name_len = ok_name.len, .type_idx = pool.idx_boolean, .optional = false },
            .{ .name_start = val_name.start, .name_len = val_name.len, .type_idx = inner, .optional = true },
            .{ .name_start = err_name.start, .name_len = err_name.len, .type_idx = pool.idx_string, .optional = true },
            .{ .name_start = errs_name.start, .name_len = errs_name.len, .type_idx = pool.idx_unknown, .optional = true },
        });
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

    // -------------------------------------------------------------------
    // If-guard narrowing helpers
    // -------------------------------------------------------------------

    const NarrowingGuard = struct {
        key: u32 = 0,
        narrowed_type: TypeIndex = null_type_idx,
        negated: bool = false,
        /// For discriminated unions: the type to install after the then-branch
        /// returns (the union with the matched member excluded).
        else_type: TypeIndex = null_type_idx,
    };

    /// Extract a narrowing guard from an if-condition.
    /// Handles: if (x), if (!x), if (x !== undefined), if (x === undefined)
    fn extractNarrowingGuard(self: *const TypeChecker, condition: NodeIndex) NarrowingGuard {
        const tag = self.ir_view.getTag(condition) orelse return .{};

        // if (x) - truthiness guard on nullable binding
        if (tag == .identifier) {
            const r = self.resolveNullableBinding(condition) orelse return .{};
            return .{ .key = r.key, .narrowed_type = r.inner, .negated = false };
        }

        // if (x !== undefined), if (x === undefined), or if (x.prop === "literal")
        if (tag == .binary_op) {
            const bin = self.ir_view.getBinary(condition) orelse return .{};
            if (bin.op != .strict_neq and bin.op != .strict_eq) return .{};

            const lhs_tag = self.ir_view.getTag(bin.left) orelse return .{};
            const rhs_tag = self.ir_view.getTag(bin.right) orelse return .{};

            // Pattern: x === undefined / x !== undefined
            if ((lhs_tag == .identifier and rhs_tag == .lit_undefined) or
                (rhs_tag == .identifier and lhs_tag == .lit_undefined))
            {
                const ident_node = if (lhs_tag == .identifier) bin.left else bin.right;
                const r = self.resolveNullableBinding(ident_node) orelse return .{};
                return .{
                    .key = r.key,
                    .narrowed_type = r.inner,
                    .negated = bin.op == .strict_eq, // === undefined is negated (narrows in else)
                };
            }

            // Pattern: x.prop === "literal" (discriminated union narrowing)
            if ((lhs_tag == .member_access and rhs_tag == .lit_string) or
                (rhs_tag == .member_access and lhs_tag == .lit_string))
            {
                const member_node = if (lhs_tag == .member_access) bin.left else bin.right;
                const string_node = if (lhs_tag == .lit_string) bin.left else bin.right;
                if (self.extractDiscriminantGuard(member_node, string_node, bin.op)) |guard| {
                    return guard;
                }
            }
        }

        // if (!x) - negated truthiness guard
        if (tag == .unary_op) {
            const un = self.ir_view.getUnary(condition) orelse return .{};
            if (un.op != .not) return .{};
            const inner_tag = self.ir_view.getTag(un.operand) orelse return .{};
            if (inner_tag != .identifier) return .{};
            const r = self.resolveNullableBinding(un.operand) orelse return .{};
            return .{ .key = r.key, .narrowed_type = r.inner, .negated = true };
        }

        return .{};
    }

    const NullableBinding = struct { key: u32, inner: TypeIndex };

    /// If `ident_node` is an identifier bound to a nullable type, return
    /// its binding key and the unwrapped inner type.
    fn resolveNullableBinding(self: *const TypeChecker, ident_node: NodeIndex) ?NullableBinding {
        const binding = self.ir_view.getBinding(ident_node) orelse return null;
        const key = packBindingKey(binding.scope_id, binding.slot);
        const current = self.binding_types.get(key) orelse return null;
        const current_tag = self.env.pool.getTag(current) orelse return null;
        if (current_tag != .t_nullable) return null;
        return .{ .key = key, .inner = self.env.pool.getNullableInner(current) };
    }

    /// Extract a discriminated union narrowing guard from x.prop === "literal".
    /// Returns the matched union member for the then-branch. The caller handles
    /// else-branch narrowing via the negated flag.
    fn extractDiscriminantGuard(
        self: *const TypeChecker,
        member_node: NodeIndex,
        string_node: NodeIndex,
        op: @import("parser/ir.zig").BinaryOp,
    ) ?NarrowingGuard {
        const member = self.ir_view.getMember(member_node) orelse return null;
        const obj_tag = self.ir_view.getTag(member.object) orelse return null;
        if (obj_tag != .identifier) return null;

        const binding = self.ir_view.getBinding(member.object) orelse return null;
        const key = packBindingKey(binding.scope_id, binding.slot);
        const current = self.binding_types.get(key) orelse return null;
        if (self.env.pool.getTag(current) != .t_union) return null;

        const prop_name = self.resolveAtomName(member.property) orelse return null;
        const str_idx = self.ir_view.getStringIdx(string_node) orelse return null;
        const lit_value = self.ir_view.getString(str_idx) orelse return null;

        const matched = self.env.pool.findUnionMemberByDiscriminant(current, prop_name, lit_value) orelse return null;

        if (op == .strict_eq) {
            // x.prop === "literal": then-branch narrows to matched member,
            // else_type is the union with matched excluded (for after early return)
            const excluded = self.env.pool.excludeUnionMember(self.allocator, current, matched);
            return .{
                .key = key,
                .narrowed_type = matched,
                .negated = false,
                .else_type = excluded,
            };
        } else {
            // x.prop !== "literal": narrow to union excluding the matched member
            const excluded = self.env.pool.excludeUnionMember(self.allocator, current, matched);
            if (excluded == null_type_idx) return null;
            return .{ .key = key, .narrowed_type = excluded, .negated = false };
        }
    }

    /// Check if a branch unconditionally returns (simple check for early-return pattern).
    fn branchAlwaysReturns(self: *const TypeChecker, node: NodeIndex) bool {
        const tag = self.ir_view.getTag(node) orelse return false;
        if (tag == .return_stmt) return true;
        if (tag == .block) {
            const block = self.ir_view.getBlock(node) orelse return false;
            if (block.stmts_count == 0) return false;
            // Check the last statement
            const last = self.ir_view.getListIndex(block.stmts_start, @intCast(block.stmts_count - 1));
            return self.branchAlwaysReturns(last);
        }
        return false;
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
                const lt = pool.widenLiteral(self.inferType(bin.left));
                const rt = pool.widenLiteral(self.inferType(bin.right));
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
        const raw_obj_type = self.inferType(member.object);
        if (raw_obj_type == null_type_idx) return null_type_idx;

        // Unwrap nominal types for member access (operations work on the base type)
        const obj_type = self.env.pool.unwrapNominal(raw_obj_type);

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
        if (std.mem.eql(u8, name, "validateJson") or
            std.mem.eql(u8, name, "validateObject") or
            std.mem.eql(u8, name, "coerceJson") or
            std.mem.eql(u8, name, "decodeJson") or
            std.mem.eql(u8, name, "decodeForm") or
            std.mem.eql(u8, name, "decodeQuery"))
        {
            if (call.args_count > 0) {
                const schema_node = self.ir_view.getListIndex(call.args_start, 0);
                if (self.getLiteralString(schema_node)) |schema_name| {
                    if (self.schemaTypeByName(schema_name)) |schema_type| {
                        return self.typedResultType(schema_type);
                    }
                }
            }
        }
        // Check if this is a nominal type constructor: UserId("str")
        if (self.env.getTypeAlias(name)) |alias_type| {
            if (self.env.pool.isNominal(alias_type)) {
                return alias_type;
            }
        }
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

const writeJsonString = json_utils.writeJsonString;

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

test "TypeChecker: annotation preserves narrow inferred type" {
    // const p: number = 3000 should keep type 3000, not widen to number
    try checkTypedSource(
        \\const p: number = 3000;
    , 0, 0);
}

test "TypeChecker: annotation rejects incompatible type" {
    // const bad: number = "oops" should error
    try checkTypedSource(
        \\const bad: number = "oops";
    , 1, 0);
}

test "TypeChecker: rejects assignment to readonly property" {
    try checkTypedSource(
        \\type Config = { readonly port: number; host: string };
        \\const cfg: Config = { port: 3000, host: "localhost" };
        \\cfg.port = 8080;
    , 1, 0);
}

test "TypeChecker: allows assignment to non-readonly property" {
    try checkTypedSource(
        \\type Config = { readonly port: number; host: string };
        \\const cfg: Config = { port: 3000, host: "localhost" };
        \\cfg.host = "other";
    , 0, 0);
}

test "TypeChecker: distinct type rejects cross-nominal assignment" {
    // SessionId should not be assignable to UserId
    try checkTypedSource(
        \\distinct type UserId = string;
        \\distinct type SessionId = string;
        \\const sid: SessionId = SessionId("sess_456");
        \\const uid: UserId = sid;
    , 1, 0);
}

test "TypeChecker: distinct type constructor returns nominal type" {
    // UserId("str") should produce a UserId, accepted where UserId is expected
    try checkTypedSource(
        \\distinct type UserId = string;
        \\const uid: UserId = UserId("usr_123");
    , 0, 0);
}

test "TypeChecker: distinct type rejects raw base type" {
    // raw string should not be assignable to UserId
    try checkTypedSource(
        \\distinct type UserId = string;
        \\const uid: UserId = "raw_string";
    , 1, 0);
}

test "TypeChecker: template literal type accepts matching string" {
    try checkTypedSource(
        \\type ApiRoute = `/api/${string}`;
        \\const good: ApiRoute = "/api/users";
    , 0, 0);
}

test "TypeChecker: template literal type rejects non-matching string" {
    try checkTypedSource(
        \\type ApiRoute = `/api/${string}`;
        \\const bad: ApiRoute = "/other";
    , 1, 0);
}
