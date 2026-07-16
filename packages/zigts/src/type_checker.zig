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
const service_types_mod = @import("service_types.zig");
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
const ServiceTypeContext = service_types_mod.ServiceTypeContext;

/// Max union members tracked during type inference (member access, return types, etc.).
const MAX_UNION_MEMBERS = 16;

pub const TypeCheckerError = type_pool_mod.TypePoolError || error{UnresolvedTypeBinding};

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

    const ActiveDeclaredType = struct {
        name_atom: u16,
        type_idx: TypeIndex,
    };

    allocator: std.mem.Allocator,
    ir_view: IrView,
    atoms: ?*context.AtomTable,
    env: *TypeEnv,
    service_type_context: ?*const ServiceTypeContext,
    diagnostics: std.ArrayListUnmanaged(Diagnostic),
    compiled_schemas: std.ArrayListUnmanaged(CompiledSchemaType),

    /// Inferred types for const/let bindings: packed(scope_id, slot) -> TypeIndex
    binding_types: std.AutoHashMapUnmanaged(u32, TypeIndex),
    /// Declared parameter types keyed by (scope_id, slot). Kept SEPARATE from
    /// `binding_types` deliberately: feeding parameter types into general
    /// identifier inference would subject every `req: Request` argument to
    /// module-signature assignability checks the pool cannot discharge yet.
    /// Consulted only where a declared parameter type is load-bearing:
    /// match-discriminant exhaustiveness.
    param_types: std.AutoHashMapUnmanaged(u32, TypeIndex),
    /// Next declaration occurrence for each name atom. The stripper records
    /// the same semantic occurrence, so annotation binding is coordinate-free.
    var_name_ordinals: std.AutoHashMapUnmanaged(u16, u32),
    /// Lexically active declarations, including untyped shadows. Upvalues use
    /// this stack to resolve the nearest declaration by name.
    active_declared_types: std.ArrayListUnmanaged(ActiveDeclaredType),
    bound_var_annotations: usize = 0,
    binding_resolution_failed: bool = false,
    /// Flow-sensitive narrowing: binding key -> narrowed TypeIndex
    narrowed: std.AutoHashMapUnmanaged(u32, TypeIndex),
    /// Track current function's declared return type for return statement checking
    current_return_type: TypeIndex = null_type_idx,
    /// Sticky failure for proof-relevant type, narrowing, schema, parameter,
    /// and diagnostic state. Message-only formatting may retain a static
    /// fallback because the core diagnostic is still stored.
    allocation_failed: bool = false,

    pub fn init(
        allocator: std.mem.Allocator,
        ir_view: IrView,
        atoms: ?*context.AtomTable,
        env: *TypeEnv,
        service_type_context: ?*const ServiceTypeContext,
    ) TypeChecker {
        return .{
            .allocator = allocator,
            .ir_view = ir_view,
            .atoms = atoms,
            .env = env,
            .service_type_context = service_type_context,
            .diagnostics = .empty,
            .compiled_schemas = .empty,
            .binding_types = .empty,
            .param_types = .empty,
            .var_name_ordinals = .empty,
            .active_declared_types = .empty,
            .narrowed = .empty,
            .allocation_failed = false,
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
        self.param_types.deinit(self.allocator);
        self.var_name_ordinals.deinit(self.allocator);
        self.active_declared_types.deinit(self.allocator);
        self.narrowed.deinit(self.allocator);
    }

    /// Run the checker on the given root node. Returns the number of errors.
    pub fn check(self: *TypeChecker, root: NodeIndex) !u32 {
        try self.ensureHealthy();
        self.walkStmt(root);
        if (self.bound_var_annotations != self.env.varAnnotationCount()) {
            self.binding_resolution_failed = true;
        }
        try self.ensureHealthy();
        var error_count: u32 = 0;
        for (self.diagnostics.items) |diag| {
            if (diag.severity == .err) error_count += 1;
        }
        return error_count;
    }

    /// Reject proof/type results after either the checker or its shared pool
    /// encounters an operational allocation or compact-capacity failure.
    pub fn ensureHealthy(self: *const TypeChecker) TypeCheckerError!void {
        try self.env.pool.ensureHealthy();
        if (self.allocation_failed) return error.OutOfMemory;
        if (self.binding_resolution_failed) return error.UnresolvedTypeBinding;
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
        if (self.allocation_failed) return;
        self.env.pool.ensureHealthy() catch return;
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;

        switch (tag) {
            .program, .block => {
                const block = self.ir_view.getBlock(node) orelse return;
                const active_start = self.active_declared_types.items.len;
                defer self.active_declared_types.items.len = active_start;
                for (0..block.stmts_count) |i| {
                    const stmt_idx = self.ir_view.getListIndex(block.stmts_start, @intCast(i));
                    self.walkStmt(stmt_idx);
                }
            },

            .var_decl => {
                const vd = self.ir_view.getVarDecl(node) orelse return;
                const declared = self.bindVarDeclaration(vd.binding);
                if (vd.init != null_node) {
                    self.walkExpr(vd.init);

                    // Check: does the initializer type match the declared type?
                    const binding = vd.binding;
                    const key = packBindingKey(binding.scope_id, binding.slot);
                    const inferred = self.inferType(vd.init);

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
                        self.binding_types.put(self.allocator, key, effective) catch self.markAllocationFailure();
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
                    // Only narrow to the non-null type in the then-branch when the
                    // condition is non-negated (i.e. `if (x)` not `if (!x)`).
                    // A negated guard means the then-branch runs when x is falsy,
                    // so we must not install the non-null narrowing there.
                    if (!narrow.negated) {
                        self.binding_types.put(self.allocator, narrow.key, narrow.narrowed_type) catch self.markAllocationFailure();
                    }
                    self.walkStmt(if_s.then_branch);
                    if (saved) |s| {
                        self.binding_types.put(self.allocator, narrow.key, s) catch self.markAllocationFailure();
                    } else {
                        _ = self.binding_types.remove(narrow.key);
                    }

                    // Forward narrowing after early return:
                    // if (!x) { return; } narrows x to non-null after the block
                    // if (x.kind === "err") { return; } narrows x to excluded union
                    if (if_s.else_branch == null_node and self.branchAlwaysReturns(if_s.then_branch)) {
                        if (narrow.negated) {
                            self.binding_types.put(self.allocator, narrow.key, narrow.narrowed_type) catch self.markAllocationFailure();
                        } else if (narrow.else_type != null_type_idx) {
                            self.binding_types.put(self.allocator, narrow.key, narrow.else_type) catch self.markAllocationFailure();
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
                    self.binding_types.put(self.allocator, narrow.key, narrow.narrowed_type) catch self.markAllocationFailure();
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
                const active_start = self.active_declared_types.items.len;
                defer self.active_declared_types.items.len = active_start;
                // Named declarations resolve signatures by binding identity.
                const fn_name = self.resolveAtomName(decl.binding.name_atom);
                const sig = if (fn_name) |name| self.env.getFnSigByName(name) else null;
                const saved_return = self.current_return_type;
                if (sig) |s| {
                    // Proof markers (Spec/Proof/Effects capsules) are
                    // obligations for the verifier, not shapes the returned
                    // value can satisfy; compare returns against the value
                    // type only.
                    self.current_return_type = self.env.stripProofMarkers(s.return_type);
                    self.registerParamTypes(func, s);
                }
                self.walkStmt(func.body);
                self.current_return_type = saved_return;
            },

            .function_expr, .arrow_function => {
                const func = self.ir_view.getFunction(node) orelse return;
                const active_start = self.active_declared_types.items.len;
                defer self.active_declared_types.items.len = active_start;
                const loc = self.ir_view.getLoc(node);
                const sig = if (loc) |l| self.env.getFnSigByLoc(l.line) else null;
                const saved_return = self.current_return_type;
                if (sig) |s| {
                    self.current_return_type = self.env.stripProofMarkers(s.return_type);
                    self.registerParamTypes(func, s);
                }
                self.walkStmt(func.body);
                self.current_return_type = saved_return;
            },

            .export_default => {
                if (self.ir_view.getOptValue(node)) |val| {
                    self.walkStmt(val);
                }
            },

            .export_decl => {
                const export_decl = self.ir_view.getExportDecl(node) orelse return;
                self.walkStmt(export_decl.declaration);
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
        if (self.allocation_failed) return;
        self.env.pool.ensureHealthy() catch return;
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;

        switch (tag) {
            .call => {
                const c = self.ir_view.getCall(node) orelse return;
                self.collectSchemaCompileCall(c) catch self.markAllocationFailure();
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
                const loc = self.ir_view.getLoc(node);
                const sig = if (loc) |l| self.env.getFnSigByLoc(l.line) else null;
                const saved_return = self.current_return_type;
                if (sig) |s| {
                    self.current_return_type = self.env.stripProofMarkers(s.return_type);
                    self.registerParamTypes(func, s);
                }
                self.walkStmt(func.body);
                self.current_return_type = saved_return;
            },

            else => {},
        }
    }

    fn collectSchemaCompileCall(self: *TypeChecker, call: Node.CallExpr) !void {
        const callee_tag = self.ir_view.getTag(call.callee) orelse return;
        if (callee_tag != .identifier or call.args_count < 2) return;

        const binding = self.ir_view.getBinding(call.callee) orelse return;
        const name = self.resolveAtomName(binding.name_atom) orelse return;
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

        const owned_name = try self.allocator.dupe(u8, schema_name);
        errdefer self.allocator.free(owned_name);
        try self.compiled_schemas.append(self.allocator, .{
            .name = owned_name,
            .type_idx = type_idx,
        });
    }

    fn schemaJsonToType(self: *TypeChecker, schema_json: []const u8) !TypeIndex {
        var parsed = std.json.parseFromSlice(std.json.Value, self.allocator, schema_json, .{}) catch |err| switch (err) {
            error.OutOfMemory => {
                self.markAllocationFailure();
                return error.OutOfMemory;
            },
            else => return null_type_idx,
        };
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
                var members: std.ArrayListUnmanaged(TypeIndex) = .empty;
                defer members.deinit(self.allocator);
                for (enum_val.array.items) |item| {
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
                    members.append(self.allocator, member) catch {
                        self.markAllocationFailure();
                        return pool.idx_unknown;
                    };
                }
                if (members.items.len > 0) return pool.addUnion(self.allocator, members.items);
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
                        required.append(self.allocator, item.string) catch self.markAllocationFailure();
                    }
                }
            }

            var fields: std.ArrayListUnmanaged(type_pool_mod.RecordField) = .empty;
            defer fields.deinit(self.allocator);
            var it = props.object.iterator();
            while (it.next()) |entry| {
                const prop_type = self.schemaValueToType(entry.value_ptr.*);
                const name = pool.addName(self.allocator, entry.key_ptr.*);
                fields.append(self.allocator, .{
                    .name_start = name.start,
                    .name_len = name.len,
                    .type_idx = if (prop_type == null_type_idx) pool.idx_unknown else prop_type,
                    .optional = !json_utils.containsString(required.items, entry.key_ptr.*),
                }) catch {
                    self.markAllocationFailure();
                    return pool.idx_unknown;
                };
            }
            if (fields.items.len == 0) return pool.idx_unknown;
            return pool.addRecord(self.allocator, fields.items);
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
        if (binding.name_atom != @intFromEnum(object.Atom.JSON)) return null;

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
                break :blk self.resolveAtomName(binding.name_atom);
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

    const ParsedServiceRoute = @import("system_linker.zig").ParsedServiceRoute;
    const parseServiceRoute = @import("system_linker.zig").parseServiceRoute;

    const ServiceCallInitInfo = struct {
        path_params: std.ArrayList([]const u8) = .empty,
        query_keys: std.ArrayList([]const u8) = .empty,
        header_keys: std.ArrayList([]const u8) = .empty,
        path_params_dynamic: bool = false,
        query_dynamic: bool = false,
        header_dynamic: bool = false,
        has_body: bool = false,
        body_dynamic: bool = false,

        fn deinit(self: *ServiceCallInitInfo, allocator: std.mem.Allocator) void {
            self.path_params.deinit(allocator);
            self.query_keys.deinit(allocator);
            self.header_keys.deinit(allocator);
        }
    };

    fn extractServiceObjectKeys(
        self: *const TypeChecker,
        node_idx: NodeIndex,
        target: *std.ArrayList([]const u8),
        dynamic_flag: *bool,
    ) void {
        const tag = self.ir_view.getTag(node_idx) orelse {
            dynamic_flag.* = true;
            return;
        };
        if (tag != .object_literal) {
            dynamic_flag.* = true;
            return;
        }

        const obj = self.ir_view.getObject(node_idx) orelse {
            dynamic_flag.* = true;
            return;
        };
        var i: u16 = 0;
        while (i < obj.properties_count) : (i += 1) {
            const prop_idx = self.ir_view.getListIndex(obj.properties_start, i);
            const prop = self.ir_view.getProperty(prop_idx) orelse continue;
            const key = self.getObjectPropertyKey(prop.key) orelse {
                dynamic_flag.* = true;
                continue;
            };
            target.append(self.allocator, key) catch @constCast(self).markAllocationFailure();
        }
    }

    fn extractServiceCallInitInfo(self: *const TypeChecker, call: Node.CallExpr) ServiceCallInitInfo {
        var info = ServiceCallInitInfo{};
        if (call.args_count <= 2) return info;

        const init_idx = self.ir_view.getListIndex(call.args_start, 2);
        const tag = self.ir_view.getTag(init_idx) orelse {
            info.path_params_dynamic = true;
            info.query_dynamic = true;
            info.header_dynamic = true;
            info.body_dynamic = true;
            return info;
        };
        if (tag == .lit_null or tag == .lit_undefined) return info;
        if (tag != .object_literal) {
            info.path_params_dynamic = true;
            info.query_dynamic = true;
            info.header_dynamic = true;
            info.body_dynamic = true;
            return info;
        }

        const obj = self.ir_view.getObject(init_idx) orelse return info;
        var i: u16 = 0;
        while (i < obj.properties_count) : (i += 1) {
            const prop_idx = self.ir_view.getListIndex(obj.properties_start, i);
            const prop = self.ir_view.getProperty(prop_idx) orelse continue;
            const key = self.getObjectPropertyKey(prop.key) orelse continue;

            if (std.mem.eql(u8, key, "params")) {
                self.extractServiceObjectKeys(prop.value, &info.path_params, &info.path_params_dynamic);
            } else if (std.mem.eql(u8, key, "query")) {
                self.extractServiceObjectKeys(prop.value, &info.query_keys, &info.query_dynamic);
            } else if (std.mem.eql(u8, key, "headers")) {
                self.extractServiceObjectKeys(prop.value, &info.header_keys, &info.header_dynamic);
            } else if (std.mem.eql(u8, key, "body")) {
                const body_tag = self.ir_view.getTag(prop.value) orelse {
                    info.has_body = true;
                    info.body_dynamic = true;
                    continue;
                };
                if (body_tag == .lit_null or body_tag == .lit_undefined) continue;
                info.has_body = true;
                if (body_tag != .lit_string and body_tag != .object_literal and body_tag != .array_literal) {
                    info.body_dynamic = true;
                }
            }
        }
        return info;
    }

    const containsString = json_utils.containsString;

    fn addAllocatedDiagnostic(
        self: *TypeChecker,
        kind: DiagnosticKind,
        node: NodeIndex,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        const msg = std.fmt.allocPrint(self.allocator, fmt, args) catch {
            self.markAllocationFailure();
            return;
        };
        self.addDiagnostic(.{
            .severity = .err,
            .kind = kind,
            .node = node,
            .message = msg,
            .help = null,
            .allocated = true,
        });
    }

    fn validateServiceCallAgainstRoute(
        self: *TypeChecker,
        node: NodeIndex,
        call: Node.CallExpr,
        route: *const service_types_mod.RouteInfo,
    ) void {
        var init_info = self.extractServiceCallInitInfo(call);
        defer init_info.deinit(self.allocator);

        for (route.required_path_params) |name| {
            if (init_info.path_params_dynamic) {
                self.addAllocatedDiagnostic(.arg_type_mismatch, node, "serviceCall cannot prove path param '{s}'", .{name});
                return;
            }
            if (!containsString(init_info.path_params.items, name)) {
                self.addAllocatedDiagnostic(.arg_type_mismatch, node, "serviceCall is missing path param '{s}'", .{name});
                return;
            }
        }

        if (!route.request_dynamic) {
            for (route.required_query_params) |name| {
                if (init_info.query_dynamic) {
                    self.addAllocatedDiagnostic(.arg_type_mismatch, node, "serviceCall cannot prove query param '{s}'", .{name});
                    return;
                }
                if (!containsString(init_info.query_keys.items, name)) {
                    self.addAllocatedDiagnostic(.arg_type_mismatch, node, "serviceCall is missing query param '{s}'", .{name});
                    return;
                }
            }

            for (route.required_header_params) |name| {
                if (init_info.header_dynamic) {
                    self.addAllocatedDiagnostic(.arg_type_mismatch, node, "serviceCall cannot prove header '{s}'", .{name});
                    return;
                }
                if (!containsString(init_info.header_keys.items, name)) {
                    self.addAllocatedDiagnostic(.arg_type_mismatch, node, "serviceCall is missing header '{s}'", .{name});
                    return;
                }
            }

            if (route.requires_body) {
                if (init_info.body_dynamic) {
                    self.addAllocatedDiagnostic(.arg_type_mismatch, node, "serviceCall cannot prove request body presence", .{});
                    return;
                }
                if (!init_info.has_body) {
                    self.addAllocatedDiagnostic(.arg_type_mismatch, node, "serviceCall is missing required request body", .{});
                    return;
                }
            }
        }
    }

    fn buildServiceResponseMember(
        self: *const TypeChecker,
        response: service_types_mod.ResponseVariant,
    ) TypeIndex {
        const pool = self.env.pool;
        const status_name = pool.addName(self.allocator, "status");
        const ok_name = pool.addName(self.allocator, "ok");
        const json_name = pool.addName(self.allocator, "json");
        const text_name = pool.addName(self.allocator, "text");
        const headers_name = pool.addName(self.allocator, "headers");

        const status_int: i16 = @intCast(@min(response.status, std.math.maxInt(i16)));
        const status_type = pool.addLiteralNumber(self.allocator, status_int);
        const ok_type = pool.addLiteralBool(self.allocator, response.status >= 200 and response.status < 300);
        const json_return_type = blk: {
            if (response.dynamic) break :blk pool.idx_unknown;
            if (response.content_type) |content_type| {
                if (!std.mem.startsWith(u8, content_type, "application/json")) break :blk pool.idx_unknown;
            } else {
                break :blk pool.idx_unknown;
            }
            if (response.schema_json) |schema_json| {
                break :blk @constCast(self).schemaJsonToType(schema_json) catch {
                    @constCast(self).markAllocationFailure();
                    break :blk pool.idx_unknown;
                };
            }
            break :blk pool.idx_unknown;
        };
        const json_fn = pool.addFunction(self.allocator, &.{}, json_return_type);
        const text_fn = pool.addFunction(self.allocator, &.{}, pool.idx_string);

        return pool.addRecord(self.allocator, &.{
            .{ .name_start = status_name.start, .name_len = status_name.len, .type_idx = status_type, .optional = false },
            .{ .name_start = ok_name.start, .name_len = ok_name.len, .type_idx = ok_type, .optional = false },
            .{ .name_start = json_name.start, .name_len = json_name.len, .type_idx = json_fn, .optional = false },
            .{ .name_start = text_name.start, .name_len = text_name.len, .type_idx = text_fn, .optional = false },
            .{ .name_start = headers_name.start, .name_len = headers_name.len, .type_idx = pool.idx_unknown, .optional = false },
        });
    }

    fn inferServiceCallType(self: *const TypeChecker, call: Node.CallExpr) TypeIndex {
        const service_context = self.service_type_context orelse return null_type_idx;
        if (call.args_count < 2) return null_type_idx;

        const service_node = self.ir_view.getListIndex(call.args_start, 0);
        const route_node = self.ir_view.getListIndex(call.args_start, 1);
        const service_name = self.getLiteralString(service_node) orelse return null_type_idx;
        const route_pattern = self.getLiteralString(route_node) orelse return null_type_idx;
        const parsed = parseServiceRoute(route_pattern) orelse return null_type_idx;
        const route = service_context.lookupRoute(service_name, parsed.method, parsed.path) orelse return null_type_idx;
        if (route.responses.len == 0) return null_type_idx;

        var member_types: [MAX_UNION_MEMBERS]TypeIndex = undefined;
        var count: usize = 0;
        for (route.responses) |response| {
            if (count >= member_types.len) break;
            member_types[count] = self.buildServiceResponseMember(response);
            count += 1;
        }
        if (count == 0) return null_type_idx;
        if (count == 1) return member_types[0];
        return self.env.pool.addUnion(self.allocator, member_types[0..count]);
    }

    // -------------------------------------------------------------------
    // Type inference (TypePool-based)
    // -------------------------------------------------------------------

    /// Infer the TypeIndex of an expression. Returns null_type_idx for unknown.
    pub fn inferType(self: *const TypeChecker, node: NodeIndex) TypeIndex {
        self.env.pool.ensureHealthy() catch return null_type_idx;
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
                return self.declaredTypeForBinding(binding);
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

            // Pattern: x.prop === <literal> (discriminated union narrowing)
            const lhs_is_literal = lhs_tag == .lit_string or lhs_tag == .lit_int or lhs_tag == .lit_bool;
            const rhs_is_literal = rhs_tag == .lit_string or rhs_tag == .lit_int or rhs_tag == .lit_bool;
            if ((lhs_tag == .member_access and rhs_is_literal) or
                (rhs_tag == .member_access and lhs_is_literal))
            {
                const member_node = if (lhs_tag == .member_access) bin.left else bin.right;
                const literal_node = if (lhs_is_literal) bin.left else bin.right;
                if (self.extractDiscriminantGuard(member_node, literal_node, bin.op)) |guard| {
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

    /// Extract a discriminated union narrowing guard from x.prop === <literal>.
    /// Returns the matched union member for the then-branch. The caller handles
    /// else-branch narrowing via the negated flag.
    fn extractDiscriminantGuard(
        self: *const TypeChecker,
        member_node: NodeIndex,
        literal_node: NodeIndex,
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
        const literal_type = self.inferType(literal_node);
        const literal_tag = self.env.pool.getTag(literal_type) orelse return null;
        if (literal_tag != .t_literal_string and literal_tag != .t_literal_number and literal_tag != .t_literal_bool) return null;

        const matched = blk: {
            for (self.env.pool.getUnionMembers(current)) |member_type| {
                const field = self.env.pool.lookupRecordField(member_type, prop_name) orelse continue;
                if (self.env.pool.isAssignableTo(field.type_idx, literal_type) and
                    self.env.pool.isAssignableTo(literal_type, field.type_idx))
                {
                    break :blk member_type;
                }
            }
            break :blk null_type_idx;
        };
        if (matched == null_type_idx) return null;

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
            return .{
                .key = key,
                .narrowed_type = excluded,
                .negated = false,
                .else_type = matched,
            };
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

        var fields_buf: std.ArrayListUnmanaged(type_pool_mod.RecordField) = .empty;
        defer fields_buf.deinit(self.allocator);

        for (0..obj.properties_count) |i| {
            const prop_idx = self.ir_view.getListIndex(obj.properties_start, @intCast(i));
            const prop = self.ir_view.getProperty(prop_idx) orelse continue;
            // The key is a node index (identifier, string, or computed); extract atom from it
            const key_tag = self.ir_view.getTag(prop.key) orelse continue;
            const prop_name = if (key_tag == .identifier)
                (if (self.ir_view.getBinding(prop.key)) |b| self.resolveAtomName(b.name_atom) else null)
            else if (key_tag == .lit_string)
                (if (self.ir_view.getStringIdx(prop.key)) |si| self.ir_view.getString(si) else null)
            else
                null;
            const prop_name_str = prop_name orelse continue;
            const val_type = self.inferType(prop.value);
            const n = self.env.pool.addName(self.allocator, prop_name_str);
            fields_buf.append(self.allocator, .{
                .name_start = n.start,
                .name_len = n.len,
                .type_idx = val_type,
                .optional = false,
            }) catch {
                @constCast(self).markAllocationFailure();
                return null_type_idx;
            };
        }

        if (fields_buf.items.len == 0) return null_type_idx;
        return self.env.pool.addRecord(self.allocator, fields_buf.items);
    }

    fn inferArrayLiteralType(self: *const TypeChecker, node: NodeIndex) TypeIndex {
        const arr = self.ir_view.getArray(node) orelse return null_type_idx;
        if (arr.elements_count == 0) return null_type_idx;

        var element_types: [32]TypeIndex = undefined;
        var count: usize = 0;
        var truncated = false;

        for (0..arr.elements_count) |i| {
            // For arrays larger than the buffer, infer the element type from the
            // first 32 elements rather than discarding the whole type as unknown.
            if (count >= element_types.len) {
                truncated = true;
                break;
            }
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
        // A truncated array must not be described as a fixed-length tuple of the
        // first 32 elements (that would understate the real length). Widen to a
        // homogeneous array over the seen element types instead.
        if (truncated) {
            return self.env.pool.addArray(self.allocator, self.env.pool.addUnion(self.allocator, element_types[0..count]));
        }
        return self.env.pool.addTuple(self.allocator, element_types[0..count]);
    }

    fn functionsCompatible(self: *const TypeChecker, field_type: TypeIndex, first_info: anytype) bool {
        if (self.env.pool.getTag(field_type) != .t_function) return false;
        const info = self.env.pool.getFunctionInfo(field_type);
        if (info.params.len != first_info.params.len) return false;
        for (info.params, first_info.params) |param, first_param| {
            if (param.type_idx != first_param.type_idx) return false;
        }
        return info.ret == first_info.ret;
    }

    fn inferMemberAccessType(self: *const TypeChecker, node: NodeIndex) TypeIndex {
        const member = self.ir_view.getMember(node) orelse return null_type_idx;
        const raw_obj_type = self.inferType(member.object);
        if (raw_obj_type == null_type_idx) return null_type_idx;

        // Unwrap nominal types for member access (operations work on the base type)
        const obj_type = self.env.pool.unwrapNominal(raw_obj_type);

        const prop_name = self.resolveAtomName(member.property) orelse return null_type_idx;

        const tag = self.env.pool.getTag(obj_type) orelse return null_type_idx;
        if (tag == .t_record) {
            if (self.env.pool.lookupRecordField(obj_type, prop_name)) |field| {
                return field.type_idx;
            }
            @constCast(self).addDiagnostic(.{
                .severity = .err,
                .kind = .missing_field,
                .node = node,
                .message = "property does not exist on type",
                .help = null,
            });
            return null_type_idx;
        }

        if (tag == .t_union) {
            var field_types: [MAX_UNION_MEMBERS]TypeIndex = undefined;
            var count: usize = 0;
            for (self.env.pool.getUnionMembers(obj_type)) |member_type| {
                const field = self.env.pool.lookupRecordField(member_type, prop_name) orelse return null_type_idx;
                if (count >= field_types.len) return null_type_idx;
                field_types[count] = field.type_idx;
                count += 1;
            }
            if (count == 0) return null_type_idx;
            if (count == 1) return field_types[0];

            const first_tag = self.env.pool.getTag(field_types[0]) orelse return null_type_idx;
            if (first_tag == .t_function) {
                const first_info = self.env.pool.getFunctionInfo(field_types[0]);
                for (field_types[1..count]) |field_type| {
                    if (!self.functionsCompatible(field_type, first_info)) {
                        @constCast(self).addDiagnostic(.{
                            .severity = .err,
                            .kind = .type_mismatch,
                            .node = node,
                            .message = "must narrow union before calling this member",
                            .help = null,
                        });
                        return null_type_idx;
                    }
                }
                return field_types[0];
            }

            return self.env.pool.addUnion(self.allocator, field_types[0..count]);
        }

        // Array (and tuple-typed array literal) receivers. A heterogeneous literal
        // like `[10, 2, 1]` infers as a tuple, so widen it back to a `T[]` result.
        if (tag == .t_array) {
            return self.inferArrayMethodType(obj_type, prop_name);
        }
        if (tag == .t_tuple) {
            return self.inferArrayMethodType(self.tupleToArrayType(obj_type), prop_name);
        }

        return null_type_idx;
    }

    /// Widen a tuple type into a `T[]` whose element is the union of its (widened)
    /// element types. Used so array-method modelling works on array-literal receivers.
    fn tupleToArrayType(self: *const TypeChecker, tuple_type: TypeIndex) TypeIndex {
        const elements = self.env.pool.getTupleElements(tuple_type);
        if (elements.len == 0) return self.env.pool.addArray(self.allocator, self.env.pool.idx_unknown);
        var widened: [32]TypeIndex = undefined;
        var count: usize = 0;
        for (elements) |elem| {
            if (count >= widened.len) break;
            const w = self.env.pool.widenLiteral(elem);
            var seen = false;
            for (widened[0..count]) |existing| {
                if (existing == w) {
                    seen = true;
                    break;
                }
            }
            if (!seen) {
                widened[count] = w;
                count += 1;
            }
        }
        const element_type = if (count == 1) widened[0] else self.env.pool.addUnion(self.allocator, widened[0..count]);
        return self.env.pool.addArray(self.allocator, element_type);
    }

    /// Model the subset of Array.prototype methods that preserve the element type.
    /// `toSorted(compareFn?)` and `toReversed()` both return a fresh `T[]`. Without
    /// this, member access on an array receiver infers `null_type_idx`, leaving the
    /// call's return type unknown and (for `toSorted`) risking a spurious argument
    /// diagnostic against an unmodelled signature. The comparator parameter is typed
    /// `unknown` and optional so any callback is accepted.
    fn inferArrayMethodType(self: *const TypeChecker, array_type: TypeIndex, prop_name: []const u8) TypeIndex {
        if (std.mem.eql(u8, prop_name, "toSorted")) {
            const params = [_]type_pool_mod.FuncParam{.{
                .name_start = 0,
                .name_len = 0,
                .type_idx = self.env.pool.idx_unknown,
                .optional = true,
            }};
            return self.env.pool.addFunction(self.allocator, &params, array_type);
        }
        if (std.mem.eql(u8, prop_name, "toReversed")) {
            return self.env.pool.addFunction(self.allocator, &.{}, array_type);
        }
        return null_type_idx;
    }

    fn inferFunctionReturnType(self: *const TypeChecker, callee_type: TypeIndex) TypeIndex {
        const tag = self.env.pool.getTag(callee_type) orelse return null_type_idx;
        if (tag == .t_function) {
            return self.env.pool.getFunctionInfo(callee_type).ret;
        }
        if (tag == .t_union) {
            var return_types: [MAX_UNION_MEMBERS]TypeIndex = undefined;
            var count: usize = 0;
            for (self.env.pool.getUnionMembers(callee_type)) |member_type| {
                if (self.env.pool.getTag(member_type) != .t_function) return null_type_idx;
                if (count >= return_types.len) return null_type_idx;
                return_types[count] = self.env.pool.getFunctionInfo(member_type).ret;
                count += 1;
            }
            if (count == 0) return null_type_idx;
            if (count == 1) return return_types[0];
            return self.env.pool.addUnion(self.allocator, return_types[0..count]);
        }
        return null_type_idx;
    }

    fn inferCallType(self: *const TypeChecker, node: NodeIndex) TypeIndex {
        const call = self.ir_view.getCall(node) orelse return null_type_idx;
        const callee_tag = self.ir_view.getTag(call.callee) orelse return null_type_idx;

        if (callee_tag == .member_access) {
            return self.inferFunctionReturnType(self.inferType(call.callee));
        }
        if (callee_tag != .identifier) return null_type_idx;

        const binding = self.ir_view.getBinding(call.callee) orelse return null_type_idx;
        const name = self.resolveAtomName(binding.name_atom) orelse return null_type_idx;
        if (std.mem.eql(u8, name, "serviceCall")) {
            const service_call_type = self.inferServiceCallType(call);
            if (service_call_type != null_type_idx) return service_call_type;
        }
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

    fn bindVarDeclaration(self: *TypeChecker, binding: ir.BindingRef) TypeIndex {
        const name = self.resolveAtomName(binding.name_atom) orelse {
            self.pushActiveDeclared(binding.name_atom, null_type_idx);
            return null_type_idx;
        };

        const gop = self.var_name_ordinals.getOrPut(self.allocator, binding.name_atom) catch {
            self.markAllocationFailure();
            return null_type_idx;
        };
        if (!gop.found_existing) gop.value_ptr.* = 0;
        const ordinal = gop.value_ptr.*;
        gop.value_ptr.* += 1;

        const declared = self.env.getVarTypeByNameOrdinal(name, ordinal) orelse null_type_idx;
        if (declared != null_type_idx) {
            self.env.bindVarType(binding.scope_id, binding.name_atom, declared) catch self.markAllocationFailure();
            self.bound_var_annotations += 1;
        }
        self.pushActiveDeclared(binding.name_atom, declared);
        return declared;
    }

    fn pushActiveDeclared(self: *TypeChecker, name_atom: u16, type_idx: TypeIndex) void {
        self.active_declared_types.append(self.allocator, .{
            .name_atom = name_atom,
            .type_idx = type_idx,
        }) catch self.markAllocationFailure();
    }

    fn declaredTypeForBinding(self: *const TypeChecker, binding: ir.BindingRef) TypeIndex {
        if (self.env.getVarTypeByBinding(binding.scope_id, binding.name_atom)) |declared| {
            return declared;
        }

        if (binding.kind == .upvalue) {
            var i = self.active_declared_types.items.len;
            while (i > 0) {
                i -= 1;
                const active = self.active_declared_types.items[i];
                if (active.name_atom == binding.name_atom) return active.type_idx;
            }
            return null_type_idx;
        }

        if (binding.kind == .undeclared_global) {
            const name = self.resolveAtomName(binding.name_atom) orelse return null_type_idx;
            return self.env.getVarTypeByName(name) orelse null_type_idx;
        }
        return null_type_idx;
    }

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
                    self.narrowed.put(self.allocator, disc_key.?, narrowed_type) catch self.markAllocationFailure();
                    self.walkExpr(arm.body);
                    if (saved) |s| {
                        self.narrowed.put(self.allocator, disc_key.?, s) catch self.markAllocationFailure();
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

    /// Register a function's declared parameter types under their binding keys
    /// (into the dedicated `param_types` map; see its doc comment for why this
    /// stays out of general identifier inference). Entries are keyed by
    /// (scope_id, slot), which is unique per function scope, so no
    /// save/restore is needed.
    fn registerParamTypes(self: *TypeChecker, func: ir.Node.FunctionExpr, sig: type_env_mod.FunctionSig) void {
        for (0..func.params_count) |i| {
            const param_idx = self.ir_view.getListIndex(func.params_start, @intCast(i));
            const binding = self.ir_view.paramBinding(param_idx) orelse continue;
            const param_type = if (i < sig.param_count) sig.param_types[i] else null_type_idx;
            self.pushActiveDeclared(binding.name_atom, param_type);
            if (param_type == null_type_idx) continue;
            const key = packBindingKey(binding.scope_id, binding.slot);
            self.param_types.put(self.allocator, key, param_type) catch self.markAllocationFailure();
            self.env.bindVarType(binding.scope_id, binding.name_atom, param_type) catch self.markAllocationFailure();
        }
    }

    /// Declared type of an identifier that is a function parameter, or
    /// null_type_idx. Fallback for sites where the declared type is
    /// load-bearing and general inference cannot see it.
    fn paramDeclaredType(self: *const TypeChecker, node: NodeIndex) TypeIndex {
        const tag = self.ir_view.getTag(node) orelse return null_type_idx;
        if (tag != .identifier) return null_type_idx;
        const binding = self.ir_view.getBinding(node) orelse return null_type_idx;
        const key = packBindingKey(binding.scope_id, binding.slot);
        return self.param_types.get(key) orelse null_type_idx;
    }

    fn isMatchExhaustive(self: *const TypeChecker, me: ir.Node.MatchExpr) bool {
        // A catch-all arm makes the match exhaustive by construction, regardless
        // of whether the discriminant type resolves.
        if (match_analysis_mod.hasDefaultArm(self.ir_view, me)) return true;
        // General inference does not resolve a plain parameter reference; fall
        // back to the declared parameter type so a full-variant
        // `match (param)` without a default is recognized as exhaustive.
        var disc_type = self.inferType(me.discriminant);
        if (disc_type == null_type_idx) disc_type = self.paramDeclaredType(me.discriminant);
        if (disc_type == null_type_idx) return false;
        const analysis = match_analysis_mod.MatchAnalysis.init(self.allocator, self.ir_view, self.env.pool);
        return analysis.isMatchExhaustive(disc_type, me);
    }

    // -------------------------------------------------------------------
    // Call argument checking
    // -------------------------------------------------------------------

    fn checkCallArgs(self: *TypeChecker, node: NodeIndex, call: Node.CallExpr) void {
        const callee_tag = self.ir_view.getTag(call.callee) orelse return;
        if (callee_tag == .member_access) {
            const callee_type = self.inferType(call.callee);
            const tag = self.env.pool.getTag(callee_type) orelse return;
            if (tag == .t_function) {
                const info = self.env.pool.getFunctionInfo(callee_type);
                // Optional (trailing) params may be omitted: the arg count must fall
                // between the required-param count and the total param count.
                var required: usize = 0;
                for (info.params) |param| {
                    if (!param.optional) required += 1;
                }
                if (call.args_count < required or call.args_count > info.params.len) {
                    self.addArgCountMismatch(node, info.params.len, @intCast(call.args_count));
                    return;
                }
                for (info.params, 0..) |param, i| {
                    if (i >= call.args_count) break;
                    const arg_idx = self.ir_view.getListIndex(call.args_start, @intCast(i));
                    const arg_type = self.inferType(arg_idx);
                    if (arg_type != null_type_idx and param.type_idx != null_type_idx and !self.env.pool.isAssignableTo(arg_type, param.type_idx)) {
                        self.addArgTypeMismatch(arg_idx, param.type_idx, arg_type);
                    }
                }
            } else if (tag == .t_union) {
                const members = self.env.pool.getUnionMembers(callee_type);
                if (members.len == 0) return;

                var shared_param_count: ?usize = null;
                var param_types: [MAX_UNION_MEMBERS]TypeIndex = undefined;
                var param_count: usize = 0;

                for (members) |member_type| {
                    if (self.env.pool.getTag(member_type) != .t_function) return;
                    const info = self.env.pool.getFunctionInfo(member_type);
                    if (shared_param_count == null) {
                        shared_param_count = info.params.len;
                        param_count = info.params.len;
                        if (param_count > param_types.len) return;
                        for (info.params, 0..) |param, i| {
                            param_types[i] = param.type_idx;
                        }
                    } else if (shared_param_count.? != info.params.len) {
                        return;
                    } else {
                        for (info.params, 0..) |param, i| {
                            if (param_types[i] != param.type_idx) return;
                        }
                    }
                }

                if (call.args_count != param_count) {
                    self.addArgCountMismatch(node, param_count, @intCast(call.args_count));
                    return;
                }

                for (0..param_count) |i| {
                    const arg_idx = self.ir_view.getListIndex(call.args_start, @intCast(i));
                    const arg_type = self.inferType(arg_idx);
                    if (arg_type != null_type_idx and param_types[i] != null_type_idx and !self.env.pool.isAssignableTo(arg_type, param_types[i])) {
                        self.addArgTypeMismatch(arg_idx, param_types[i], arg_type);
                    }
                }
            }
            return;
        }
        if (callee_tag != .identifier) return;

        const binding = self.ir_view.getBinding(call.callee) orelse return;
        const name = self.resolveAtomName(binding.name_atom) orelse return;

        if (std.mem.eql(u8, name, "serviceCall")) {
            const service_context = self.service_type_context orelse return;
            if (call.args_count >= 2) {
                const service_node = self.ir_view.getListIndex(call.args_start, 0);
                const route_node = self.ir_view.getListIndex(call.args_start, 1);
                if (self.getLiteralString(service_node)) |service_name| {
                    if (self.getLiteralString(route_node)) |route_pattern| {
                        if (parseServiceRoute(route_pattern)) |parsed| {
                            if (service_context.lookupRoute(service_name, parsed.method, parsed.path)) |route| {
                                self.validateServiceCallAgainstRoute(node, call, route);
                            }
                        }
                    }
                }
            }
        }

        const sig = self.env.getFnSigByName(name) orelse return;

        // Check argument count
        const required_param_count = sig.required_param_count orelse sig.param_count;
        if (call.args_count < required_param_count) {
            self.addArgCountMismatch(node, required_param_count, @intCast(call.args_count));
            return;
        }

        // Check argument types
        var i: u8 = 0;
        while (i < sig.param_count and i < call.args_count) : (i += 1) {
            const arg_idx = self.ir_view.getListIndex(call.args_start, i);
            const arg_type = self.inferType(arg_idx);
            if (arg_type != null_type_idx and sig.param_types[i] != null_type_idx) {
                if (!self.env.pool.isAssignableTo(arg_type, sig.param_types[i])) {
                    self.addArgTypeMismatch(arg_idx, sig.param_types[i], arg_type);
                }
            }
        }
    }

    // -------------------------------------------------------------------
    // Diagnostic helpers
    // -------------------------------------------------------------------

    fn addDiagnostic(self: *TypeChecker, diag: Diagnostic) void {
        self.diagnostics.append(self.allocator, diag) catch {
            if (diag.allocated) self.allocator.free(diag.message);
            self.markAllocationFailure();
        };
    }

    fn addTypeMismatch(self: *TypeChecker, node: NodeIndex, expected: TypeIndex, got: TypeIndex) void {
        var buf: [256]u8 = undefined;
        const expected_str = self.env.pool.formatType(expected, buf[0..128]);
        const got_str = self.env.pool.formatType(got, buf[128..256]);

        const msg = std.fmt.allocPrint(self.allocator, "type '{s}' is not assignable to type '{s}'", .{ got_str, expected_str }) catch {
            self.addDiagnostic(.{
                .severity = .err,
                .kind = .type_mismatch,
                .node = node,
                .message = "type mismatch",
                .help = null,
            });
            return;
        };

        self.addDiagnostic(.{
            .severity = .err,
            .kind = .type_mismatch,
            .node = node,
            .message = msg,
            .help = null,
            .allocated = true,
        });
    }

    /// Emit an arg-count mismatch (ZTS202) whose message names the expected and
    /// actual counts, so a caller (human or agent) can fix it without guessing.
    /// The detail rides in `.message` because `deinit` only frees `.message`
    /// when `allocated`; `.help` is never freed, so an owned string there leaks.
    fn addArgCountMismatch(self: *TypeChecker, node: NodeIndex, expected: usize, got: usize) void {
        const msg = std.fmt.allocPrint(
            self.allocator,
            "wrong number of arguments: expected {d}, got {d}",
            .{ expected, got },
        ) catch "wrong number of arguments";
        self.addDiagnostic(.{
            .severity = .err,
            .kind = .arg_count_mismatch,
            .node = node,
            .message = msg,
            .help = null,
            .allocated = !std.mem.eql(u8, msg, "wrong number of arguments"),
        });
    }

    /// Emit an arg-type mismatch (ZTS203) whose message names the expected
    /// parameter type and the actual argument type. Same ownership rule as
    /// `addArgCountMismatch`: the detail goes in `.message`, not `.help`.
    fn addArgTypeMismatch(self: *TypeChecker, node: NodeIndex, expected: TypeIndex, got: TypeIndex) void {
        var buf: [256]u8 = undefined;
        const expected_str = self.env.pool.formatType(expected, buf[0..128]);
        const got_str = self.env.pool.formatType(got, buf[128..256]);
        const msg = std.fmt.allocPrint(
            self.allocator,
            "argument type does not match parameter type: expected {s}, got {s}",
            .{ expected_str, got_str },
        ) catch "argument type does not match parameter type";
        self.addDiagnostic(.{
            .severity = .err,
            .kind = .arg_type_mismatch,
            .node = node,
            .message = msg,
            .help = null,
            .allocated = !std.mem.eql(u8, msg, "argument type does not match parameter type"),
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

    fn markAllocationFailure(self: *TypeChecker) void {
        self.allocation_failed = true;
    }
};

const packBindingKey = bool_checker_mod.packBindingKey;
const getSourceLine = bool_checker_mod.getSourceLine;

const writeJsonString = json_utils.writeJsonString;

test "TypeChecker fails closed when binding analysis cannot allocate" {
    const allocator = std.testing.allocator;
    var parser = @import("parser/parse.zig").Parser.init(allocator, "let count = 1;");
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);

    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);
    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    var failing = std.testing.FailingAllocator.init(allocator, .{ .fail_index = 0 });
    var checker = TypeChecker.init(failing.allocator(), view, null, &env, null);
    defer checker.deinit();
    try std.testing.expectError(error.OutOfMemory, checker.check(root));
}

test "TypeChecker fails closed when diagnostic storage cannot allocate" {
    const allocator = std.testing.allocator;
    var parser = @import("parser/parse.zig").Parser.init(allocator, "");
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);

    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);
    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    var failing = std.testing.FailingAllocator.init(allocator, .{ .fail_index = 0 });
    var checker = TypeChecker.init(failing.allocator(), view, null, &env, null);
    defer checker.deinit();
    checker.addDiagnostic(.{
        .severity = .err,
        .kind = .type_mismatch,
        .node = root,
        .message = "forced diagnostic",
        .help = null,
    });
    try std.testing.expectError(error.OutOfMemory, checker.check(root));
}

test "TypeChecker ensureHealthy rejects a TypePool poisoned after check" {
    const allocator = std.testing.allocator;
    var parser = @import("parser/parse.zig").Parser.init(allocator, "");
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);

    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);
    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();
    var checker = TypeChecker.init(allocator, view, null, &env, null);
    defer checker.deinit();

    try std.testing.expectEqual(@as(u32, 0), try checker.check(root));

    const record = pool.addRecord(allocator, &.{.{
        .name_start = 0,
        .name_len = 0,
        .type_idx = pool.idx_string,
        .optional = false,
    }});
    try pool.ensureHealthy();
    var failing = std.testing.FailingAllocator.init(allocator, .{ .fail_index = 0 });
    _ = pool.makeReadonly(failing.allocator(), record);

    try std.testing.expectError(error.OutOfMemory, checker.ensureHealthy());
}

test "TypeChecker ensureHealthy propagates TypePool capacity exhaustion" {
    const allocator = std.testing.allocator;
    var parser = @import("parser/parse.zig").Parser.init(allocator, "");
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);

    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);
    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();
    var checker = TypeChecker.init(allocator, view, null, &env, null);
    defer checker.deinit();

    try std.testing.expectEqual(@as(u32, 0), try checker.check(root));
    try pool.fields.resize(allocator, @as(usize, std.math.maxInt(u16)) + 1);
    _ = pool.addRecord(allocator, &.{});

    try std.testing.expectError(error.TypePoolCapacityExceeded, checker.ensureHealthy());
}

fn checkTypedSource(source: []const u8, expect_errors: u32, expect_warnings: ?u32) !void {
    try checkTypedSourceWithServiceContext(source, null, expect_errors, expect_warnings);
}

fn checkTypedSourceWithServiceContext(
    source: []const u8,
    service_type_context: ?*const ServiceTypeContext,
    expect_errors: u32,
    expect_warnings: ?u32,
) !void {
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

    var checker = TypeChecker.init(allocator, ir_view, null, &env, service_type_context);
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

const RawTestResponse = struct {
    status: u16,
    content_type: ?[]const u8 = "application/json",
    schema_json: ?[]const u8 = null,
    dynamic: bool = false,
};

fn makeTestServiceContext(
    allocator: std.mem.Allocator,
    required_path_params: []const []const u8,
    responses: []const RawTestResponse,
) !ServiceTypeContext {
    const routes = try allocator.alloc(service_types_mod.RouteInfo, 1);
    errdefer allocator.free(routes);

    const path_params = try allocator.alloc([]const u8, required_path_params.len);
    errdefer allocator.free(path_params);
    for (required_path_params, 0..) |name, i| {
        path_params[i] = try allocator.dupe(u8, name);
    }

    const query_params = try allocator.alloc([]const u8, 0);
    const header_params = try allocator.alloc([]const u8, 0);

    const route_responses = try allocator.alloc(service_types_mod.ResponseVariant, responses.len);
    errdefer allocator.free(route_responses);
    for (responses, 0..) |response, i| {
        route_responses[i] = .{
            .status = response.status,
            .content_type = if (response.content_type) |content_type|
                try allocator.dupe(u8, content_type)
            else
                null,
            .schema_json = if (response.schema_json) |schema_json|
                try allocator.dupe(u8, schema_json)
            else
                null,
            .dynamic = response.dynamic,
        };
    }

    routes[0] = .{
        .service_name = try allocator.dupe(u8, "users"),
        .handler_path = try allocator.dupe(u8, "users.ts"),
        .method = try allocator.dupe(u8, "GET"),
        .path = try allocator.dupe(u8, "/api/users/:id"),
        .required_path_params = path_params,
        .required_query_params = query_params,
        .required_header_params = header_params,
        .request_dynamic = false,
        .response_dynamic = false,
        .requires_body = false,
        .responses = route_responses,
    };

    return .{ .routes = routes };
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

    var checker = TypeChecker.init(allocator, view, null, &env, null);
    defer checker.deinit();

    try std.testing.expectEqual(@as(usize, 0), checker.diagnostics.items.len);
}

test "TypeChecker infers serviceCall json payload with system context" {
    var service_context = try makeTestServiceContext(
        std.testing.allocator,
        &.{"id"},
        &.{
            .{ .status = 200, .schema_json = "{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"}},\"required\":[\"id\"]}" },
        },
    );
    defer service_context.deinit(std.testing.allocator);

    try checkTypedSourceWithServiceContext(
        \\const user = serviceCall("users", "GET /api/users/:id", {
        \\  params: { id: "123" }
        \\});
        \\const status: number = user.status;
        \\const payload = user.json();
        \\const id: string = payload.id;
    , &service_context, 0, 0);
}

test "TypeChecker rejects serviceCall missing required path param" {
    var service_context = try makeTestServiceContext(
        std.testing.allocator,
        &.{"id"},
        &.{
            .{ .status = 200, .schema_json = "{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"}},\"required\":[\"id\"]}" },
        },
    );
    defer service_context.deinit(std.testing.allocator);

    try checkTypedSourceWithServiceContext(
        \\const user = serviceCall("users", "GET /api/users/:id", {});
        \\const payload = user.json();
    , &service_context, 1, 0);
}

test "TypeChecker requires narrowing before multi-status serviceCall json" {
    var service_context = try makeTestServiceContext(
        std.testing.allocator,
        &.{"id"},
        &.{
            .{ .status = 200, .schema_json = "{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"}},\"required\":[\"id\"]}" },
            .{ .status = 404, .schema_json = "{\"type\":\"object\",\"properties\":{\"error\":{\"type\":\"string\"}},\"required\":[\"error\"]}" },
        },
    );
    defer service_context.deinit(std.testing.allocator);

    try checkTypedSourceWithServiceContext(
        \\const user = serviceCall("users", "GET /api/users/:id", {
        \\  params: { id: "123" }
        \\});
        \\const payload = user.json();
    , &service_context, 2, 0);
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

test "TypeChecker: param-discriminant match with default arm is exhaustive (no warning)" {
    // Regression: inferType does not resolve a plain parameter's declared type,
    // so the discriminant was unknown and a `default` arm was not credited,
    // producing a spurious non-exhaustive warning. A catch-all arm is now
    // honored regardless of discriminant type.
    try checkTypedSource(
        \\type C = { kind: "echo", text: string } | { kind: "ping", text: string };
        \\function run(c: C): string {
        \\  return match (c) {
        \\    when { kind: "echo" }: c.text,
        \\    when { kind: "ping" }: "pong",
        \\    default: "u",
        \\  };
        \\}
    , 0, 0);
}

test "TypeChecker: typed return satisfies a marker-carrying declared return type" {
    // `Effects<string, "env">` instantiates to `string & { __zigttp_effect__ }`.
    // The phantom marker is a verifier obligation, never a value shape, so a
    // returned `string` must pass the return-type check. Without
    // stripProofMarkers the intersection comparison spuriously failed for any
    // return expression whose type inference resolves (annotated locals).
    try checkTypedSource(
        \\function digest(): Effects<string, "env"> {
        \\  const s: string = "x";
        \\  return s;
        \\}
    , 0, 0);
}

test "TypeChecker: marker stripping still rejects a genuinely wrong return" {
    try checkTypedSource(
        \\function digest(): Effects<string, "env"> {
        \\  const n: number = 1;
        \\  return n;
        \\}
    , 1, 0);
}

test "TypeChecker: full-variant param-discriminant match without a default is exhaustive" {
    // Option B: declared parameter types are registered into binding_types on
    // function entry (registerParamTypes), so the discriminant of a
    // `match (param)` resolves to the declared union and variant-coverage
    // analysis runs. Full coverage without a default arm is exhaustive - no
    // warning. (The canonical profile still separately requires a default arm
    // on every match; this only fixes the type-checker layer's false warn.)
    try checkTypedSource(
        \\type C = { kind: "echo", text: string } | { kind: "ping", text: string };
        \\function run(c: C): string {
        \\  return match (c) {
        \\    when { kind: "echo" }: c.text,
        \\    when { kind: "ping" }: "pong",
        \\  };
        \\}
    , 0, 0);
}

test "TypeChecker: partial param-discriminant match without a default still warns" {
    // The same parameter-type resolution must not silence REAL gaps: one of
    // two variants covered, no default - warn.
    try checkTypedSource(
        \\type C = { kind: "echo", text: string } | { kind: "ping", text: string };
        \\function run(c: C): string {
        \\  return match (c) {
        \\    when { kind: "echo" }: c.text,
        \\  };
        \\}
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

test "TypeChecker: exported handler local const rejects incompatible initializer" {
    try checkTypedSource(
        \\export function handler(req: Request): Response {
        \\  const n: number = "not a number";
        \\  return Response.json({});
        \\}
    , 1, 0);
}

test "TypeChecker: exported default handler local annotation is checked" {
    try checkTypedSource(
        \\export default function handler(req: Request): Response {
        \\  const n: number = "x";
        \\  return Response.json({});
        \\}
    , 1, 0);
}

test "TypeChecker: exported arrow handler local annotation is checked" {
    try checkTypedSource(
        \\export const handler = () => {
        \\  const n: number = "x";
        \\  return Response.json({});
        \\};
    , 1, 0);
}

test "TypeChecker: exported handler local let annotation is checked" {
    try checkTypedSource(
        \\export function handler(req: Request): Response {
        \\  let n: number = "x";
        \\  return Response.json({});
        \\}
    , 1, 0);
}

test "TypeChecker: nested arrow in exported handler checks local annotation" {
    try checkTypedSource(
        \\export const handler = () => {
        \\  const nested = () => {
        \\    const n: number = "x";
        \\    return n;
        \\  };
        \\  return Response.json({ value: nested() });
        \\};
    , 1, 0);
}

test "TypeChecker: shadowed exported local uses its own scoped annotation" {
    try checkTypedSource(
        \\export const zz: string = "top level";
        \\export function handler(req: Request): Response {
        \\  const zz: number = "not a number";
        \\  return Response.json({ value: zz });
        \\}
    , 1, 0);
}

test "TypeChecker: exported handler argument annotation reaches nested upvalue" {
    try checkTypedSource(
        \\export function handler(req: number): Response {
        \\  const nested = () => {
        \\    const text: string = req;
        \\    return text;
        \\  };
        \\  return Response.json({ value: nested() });
        \\}
    , 1, 0);
}

test "TypeChecker: exported top-level annotation remains checked" {
    try checkTypedSource(
        \\export const top: number = "x";
    , 1, 0);
}

test "TypeChecker: correct local annotation in exported handler passes" {
    try checkTypedSource(
        \\export function handler(req: Request): Response {
        \\  const n: number = 1;
        \\  return Response.json({ value: n });
        \\}
    , 0, 0);
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

test "TypeChecker: toSorted with a comparator type-checks clean" {
    // Regression for ENG-9: a comparator argument must not raise ZTS204/ZTS003.
    // toSorted(compareFn?) is modelled as an optional callback returning T[].
    try checkTypedSource(
        \\const nums = [10, 2, 1, 33, 4];
        \\const sorted = nums.toSorted((a, b) => a - b);
    , 0, 0);
}

test "TypeChecker: jwtVerify algorithm parameter is optional" {
    try checkTypedSource(
        \\import { jwtVerify } from "zigttp:auth";
        \\const result = jwtVerify("token", "secret");
    , 0, 0);
}

test "TypeChecker: jwtVerify algorithm parameter is type-checked when present" {
    try checkTypedSource(
        \\import { jwtVerify } from "zigttp:auth";
        \\const result = jwtVerify("token", "secret", 123);
    , 1, 0);
}

test "TypeChecker tracks object literal fields beyond 32 properties" {
    const allocator = std.testing.allocator;
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();

    try aw.writer.writeAll("const obj = {");
    for (0..33) |i| {
        if (i > 0) try aw.writer.writeAll(",");
        try aw.writer.print("p{d}: \"{d}\"", .{ i, i });
    }
    try aw.writer.writeAll("};\nconst last: string = obj.p32;\n");

    try checkTypedSource(aw.writer.buffered(), 0, 0);
}

test "TypeChecker tracks schema object fields beyond 32 properties" {
    const allocator = std.testing.allocator;
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();

    try aw.writer.writeAll(
        \\import { schemaCompile, validateJson } from "zigttp:validate";
        \\schemaCompile("big", '{"type":"object","properties":{
    );
    for (0..33) |i| {
        if (i > 0) try aw.writer.writeAll(",");
        try aw.writer.print("\"p{d}\":{{\"type\":\"string\"}}", .{i});
    }
    try aw.writer.writeAll(
        \\}}');
        \\const result = validateJson("big", "{}");
        \\const last: string = result.value.p32;
        \\
    );

    try checkTypedSource(aw.writer.buffered(), 0, 0);
}

test "TypeChecker tracks schema enum members beyond 32 values" {
    const allocator = std.testing.allocator;
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();

    try aw.writer.writeAll(
        \\import { schemaCompile, validateJson } from "zigttp:validate";
        \\schemaCompile("choice", '{"enum":[
    );
    for (0..33) |i| {
        if (i > 0) try aw.writer.writeAll(",");
        try aw.writer.print("\"v{d}\"", .{i});
    }
    try aw.writer.writeAll(
        \\]}');
        \\
    );

    var strip_result = try @import("stripper.zig").strip(allocator, aw.writer.buffered(), .{});
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

    var checker = TypeChecker.init(allocator, ir_view, null, &env, null);
    defer checker.deinit();

    try std.testing.expectEqual(@as(u32, 0), try checker.check(root));
    try std.testing.expectEqual(@as(usize, 1), checker.compiled_schemas.items.len);
    const schema_type = checker.compiled_schemas.items[0].type_idx;
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_union, pool.getTag(schema_type).?);
    const members = pool.getUnionMembers(schema_type);
    try std.testing.expectEqual(@as(usize, 33), members.len);
    try std.testing.expectEqualStrings("v32", pool.getLiteralStringValue(members[32]).?);
}

test "TypeChecker: toSorted with no comparator type-checks clean" {
    // The single comparator parameter is optional, so a zero-arg call is valid.
    try checkTypedSource(
        \\const nums = [3, 1, 2];
        \\const sorted = nums.toSorted();
    , 0, 0);
}

test "TypeChecker: toReversed type-checks clean" {
    try checkTypedSource(
        \\const nums = [3, 1, 2];
        \\const rev = nums.toReversed();
    , 0, 0);
}
