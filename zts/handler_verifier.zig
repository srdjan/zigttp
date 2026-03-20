//! Compile-Time Handler Verifier
//!
//! Statically proves handler functions cannot fail in production by verifying:
//! 1. Every code path returns a Response (exhaustive return analysis)
//! 2. Result values are checked before access (result safety)
//! 3. No unreachable code after unconditional returns
//! 4. No unused variable declarations
//! 5. Match expressions have default arms
//! 6. Optional values from virtual modules are checked before use
//!
//! This is possible because zigttp's JS subset bans all non-trivial control flow:
//! no while/do-while (no back-edges), no break/continue (no non-local jumps),
//! no try/catch (no exceptional paths). The IR tree IS the control flow graph.
//! Verification is a recursive tree walk, not a fixpoint dataflow analysis.

const std = @import("std");
const ir = @import("parser/ir.zig");
const object = @import("object.zig");
const context = @import("context.zig");
const modules_resolver = @import("modules/resolver.zig");

const Node = ir.Node;
const NodeIndex = ir.NodeIndex;
const NodeTag = ir.NodeTag;
const IrView = ir.IrView;
const null_node = ir.null_node;

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
    // Return analysis (Check 1)
    missing_return_else,
    missing_return_default,
    missing_return_path,

    // Result checking (Check 2)
    unchecked_result_value,

    // Unreachable code (Check 3)
    unreachable_after_return,

    // Dead variables (Check 4)
    unused_variable,
    unused_import,

    // Match expression (Check 5)
    non_exhaustive_match,

    // Optional checking (Check 6)
    unchecked_optional_use,
    unchecked_optional_access,
};

pub const Diagnostic = struct {
    severity: Severity,
    kind: DiagnosticKind,
    node: NodeIndex,
    message: []const u8,
    help: ?[]const u8,
};

/// Return status for a statement or block - used by exhaustive return analysis.
const ReturnStatus = enum {
    always, // every path through this node returns
    never, // no path through this node returns
    sometimes, // some paths return, some don't
};

/// Tracks a local binding for result checking and dead variable detection.
const BindingState = struct {
    scope_id: ir.ScopeId, // scope that owns this binding
    slot: u16,
    is_result: bool,
    ok_checked: bool,
    ref_count: u16,
    decl_node: NodeIndex,
    name_idx: u16, // string constant index for the name (0 = unknown)

    fn key(self: BindingState) u32 {
        return bindingKey(self.scope_id, self.slot);
    }
};

// ---------------------------------------------------------------------------
// Known tracked functions from virtual modules
// ---------------------------------------------------------------------------

const OptionalKind = enum { optional_string, optional_object };

/// What a virtual module function produces that requires caller-side checking.
const FunctionProduces = enum {
    result, // Result object - must check .ok before .value
    optional_string, // string | undefined - must narrow before use
    optional_object, // object | undefined - must narrow before use

    fn toOptionalKind(self: FunctionProduces) ?OptionalKind {
        return switch (self) {
            .optional_string => .optional_string,
            .optional_object => .optional_object,
            .result => null,
        };
    }
};

/// Unified table of all virtual module functions that need caller-side checking.
const tracked_functions = [_]struct { module: []const u8, name: []const u8, produces: FunctionProduces }{
    // Result-producing (Check 2)
    .{ .module = "zigttp:auth", .name = "jwtVerify", .produces = .result },
    .{ .module = "zigttp:validate", .name = "validateJson", .produces = .result },
    .{ .module = "zigttp:validate", .name = "validateObject", .produces = .result },
    .{ .module = "zigttp:validate", .name = "coerceJson", .produces = .result },
    // Optional-producing (Check 6)
    .{ .module = "zigttp:env", .name = "env", .produces = .optional_string },
    .{ .module = "zigttp:cache", .name = "cacheGet", .produces = .optional_string },
    .{ .module = "zigttp:auth", .name = "parseBearer", .produces = .optional_string },
    .{ .module = "zigttp:router", .name = "routerMatch", .produces = .optional_object },
};

fn lookupTrackedFunction(name: []const u8) ?FunctionProduces {
    for (&tracked_functions) |entry| {
        if (std.mem.eql(u8, entry.name, name)) return entry.produces;
    }
    return null;
}

/// Shared key computation for scope_id + slot binding lookups.
fn bindingKey(scope_id: ir.ScopeId, slot: u16) u32 {
    return (@as(u32, scope_id) << 16) | @as(u32, slot);
}

const OptionalFnSlot = struct {
    slot: u16,
    kind: OptionalKind,
};

const OptionalBindingState = struct {
    scope_id: ir.ScopeId,
    slot: u16,
    kind: OptionalKind,
    narrowed: bool,
    decl_node: NodeIndex,

    fn key(self: OptionalBindingState) u32 {
        return bindingKey(self.scope_id, self.slot);
    }
};

const NarrowingBranch = enum { then, then_returns_early };

const NarrowingInfo = struct {
    slot: u16,
    scope_id: ir.ScopeId,
    branch: NarrowingBranch,
};

// ---------------------------------------------------------------------------
// HandlerVerifier
// ---------------------------------------------------------------------------

pub const HandlerVerifier = struct {
    allocator: std.mem.Allocator,
    ir_view: IrView,
    atoms: ?*context.AtomTable,
    diagnostics: std.ArrayList(Diagnostic),

    // Result checking state
    result_bindings: std.ArrayList(BindingState),
    // Import-to-module mapping: local binding slot -> is from result-producing module
    result_function_slots: std.ArrayList(u16),

    // Optional checking state (Check 6)
    optional_bindings: std.ArrayList(OptionalBindingState),
    optional_function_slots: std.ArrayList(OptionalFnSlot),

    // Dead variable tracking
    all_bindings: std.ArrayList(BindingState),

    pub fn init(allocator: std.mem.Allocator, ir_view: IrView, atoms: ?*context.AtomTable) HandlerVerifier {
        return .{
            .allocator = allocator,
            .ir_view = ir_view,
            .atoms = atoms,
            .diagnostics = .empty,
            .result_bindings = .empty,
            .result_function_slots = .empty,
            .optional_bindings = .empty,
            .optional_function_slots = .empty,
            .all_bindings = .empty,
        };
    }

    pub fn deinit(self: *HandlerVerifier) void {
        self.diagnostics.deinit(self.allocator);
        self.result_bindings.deinit(self.allocator);
        self.result_function_slots.deinit(self.allocator);
        self.optional_bindings.deinit(self.allocator);
        self.optional_function_slots.deinit(self.allocator);
        self.all_bindings.deinit(self.allocator);
    }

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    /// Run all verification checks on a handler function node.
    /// Returns the number of error-severity diagnostics.
    pub fn verify(self: *HandlerVerifier, handler_func: NodeIndex) !u32 {
        // Phase 1: Scan imports for result-producing function bindings
        self.scanImports();

        // Phase 2: Exhaustive return analysis on the handler body
        const func = self.ir_view.getFunction(handler_func) orelse return 0;
        const body_status = self.stmtReturns(func.body);
        if (body_status != .always) {
            self.addDiagnostic(.{
                .severity = .err,
                .kind = .missing_return_path,
                .node = handler_func,
                .message = "not all code paths return a Response",
                .help = "ensure every branch (if/else, switch/default) ends with a return statement",
            });
        }

        // Phase 3: Walk body for result checking and ref counting
        self.walkForResultsAndRefs(func.body);

        // Phase 4: Report unchecked result accesses (already emitted during walk)

        // Phase 5: Report unused variables (scope-aware tracking)
        self.reportUnusedVariables();

        // Count errors
        var error_count: u32 = 0;
        for (self.diagnostics.items) |diag| {
            if (diag.severity == .err) error_count += 1;
        }
        return error_count;
    }

    /// Get all diagnostics.
    pub fn getDiagnostics(self: *const HandlerVerifier) []const Diagnostic {
        return self.diagnostics.items;
    }

    /// Format diagnostics to a writer. Requires source text for context lines.
    pub fn formatDiagnostics(
        self: *const HandlerVerifier,
        source: []const u8,
        writer: anytype,
    ) !void {
        for (self.diagnostics.items) |diag| {
            const loc = self.ir_view.getLoc(diag.node) orelse continue;

            // Header
            try writer.print("verify {s}: {s}\n", .{ diag.severity.label(), diag.message });

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

    /// Returns true if there are any error-severity diagnostics.
    pub fn hasErrors(self: *const HandlerVerifier) bool {
        for (self.diagnostics.items) |diag| {
            if (diag.severity == .err) return true;
        }
        return false;
    }

    // -----------------------------------------------------------------------
    // Check 1: Exhaustive Return Analysis
    // -----------------------------------------------------------------------

    /// Determine whether a statement node always, never, or sometimes returns.
    fn stmtReturns(self: *HandlerVerifier, node: NodeIndex) ReturnStatus {
        const tag = self.ir_view.getTag(node) orelse return .never;
        return switch (tag) {
            .return_stmt => .always,

            .block, .program => self.blockReturns(node),

            .if_stmt => self.ifReturns(node),

            .switch_stmt => self.switchReturns(node),

            .for_of_stmt => self.forOfReturns(node),

            .var_decl,
            .expr_stmt,
            .empty_stmt,
            .import_decl,
            .export_decl,
            .function_decl,
            => .never,

            else => .never,
        };
    }

    /// Analyze return status of a block. Also detects unreachable code.
    fn blockReturns(self: *HandlerVerifier, node: NodeIndex) ReturnStatus {
        const block = self.ir_view.getBlock(node) orelse return .never;
        if (block.stmts_count == 0) return .never;

        var last_status: ReturnStatus = .never;
        var i: u16 = 0;
        while (i < block.stmts_count) : (i += 1) {
            const stmt_idx = self.ir_view.getListIndex(block.stmts_start, i);
            const status = self.stmtReturns(stmt_idx);

            if (status == .always) {
                // Check for unreachable statements after this one
                if (i + 1 < block.stmts_count) {
                    const next_stmt = self.ir_view.getListIndex(block.stmts_start, i + 1);
                    // Skip function declarations - they're hoisted logically
                    const next_tag = self.ir_view.getTag(next_stmt);
                    if (next_tag != null and next_tag.? != .function_decl) {
                        self.addDiagnostic(.{
                            .severity = .warning,
                            .kind = .unreachable_after_return,
                            .node = next_stmt,
                            .message = "unreachable code after return statement",
                            .help = "remove the unreachable code, or restructure the control flow",
                        });
                    }
                }
                return .always;
            }
            last_status = status;
        }

        return last_status;
    }

    /// Analyze return status of an if statement.
    fn ifReturns(self: *HandlerVerifier, node: NodeIndex) ReturnStatus {
        const if_stmt = self.ir_view.getIfStmt(node) orelse return .never;

        const then_status = self.stmtReturns(if_stmt.then_branch);

        // No else clause: can never guarantee return
        if (if_stmt.else_branch == null_node) {
            if (then_status == .always) {
                // The then branch always returns, but there's no else.
                // This is only a problem if this if-stmt is in a terminal position.
                // We report it as "sometimes" and let the parent block handle it.
                return .sometimes;
            }
            return .never;
        }

        // Has else clause: combine both branches
        const else_status = self.stmtReturns(if_stmt.else_branch);

        if (then_status == .always and else_status == .always) return .always;
        if (then_status == .never and else_status == .never) return .never;
        return .sometimes;
    }

    /// Analyze return status of a switch statement.
    fn switchReturns(self: *HandlerVerifier, node: NodeIndex) ReturnStatus {
        const switch_stmt = self.ir_view.getSwitchStmt(node) orelse return .never;
        if (switch_stmt.cases_count == 0) return .never;

        var has_default = false;
        var all_return = true;

        var i: u8 = 0;
        while (i < switch_stmt.cases_count) : (i += 1) {
            const case_idx = self.ir_view.getListIndex(switch_stmt.cases_start, i);
            const case_clause = self.ir_view.getCaseClause(case_idx) orelse continue;

            // Default case has null_node test_expr
            if (case_clause.test_expr == null_node) {
                has_default = true;
            }

            // Check if this case's body returns
            const case_status = self.caseBodyReturns(case_clause);
            if (case_status != .always) {
                all_return = false;
            }
        }

        // Switch only guarantees return if it has a default and all cases return
        if (has_default and all_return) return .always;
        if (!has_default) {
            self.addDiagnostic(.{
                .severity = .err,
                .kind = .missing_return_default,
                .node = node,
                .message = "switch statement without default case may not return a Response",
                .help = "add a default case that returns a Response",
            });
        }
        return .sometimes;
    }

    /// Check if a case clause's body always returns.
    fn caseBodyReturns(self: *HandlerVerifier, case_clause: Node.CaseClause) ReturnStatus {
        if (case_clause.body_count == 0) return .never;

        // Walk case body statements
        var i: u16 = 0;
        while (i < case_clause.body_count) : (i += 1) {
            const stmt_idx = self.ir_view.getListIndex(case_clause.body_start, i);
            const status = self.stmtReturns(stmt_idx);
            if (status == .always) return .always;
        }
        return .never;
    }

    /// for-of body returns .never because the iterable could be empty.
    fn forOfReturns(_: *HandlerVerifier, _: NodeIndex) ReturnStatus {
        return .never;
    }

    // -----------------------------------------------------------------------
    // Check 2: Result Checking
    // -----------------------------------------------------------------------

    /// Scan top-level import declarations to identify which local bindings
    /// map to tracked virtual module functions (result-producing or optional-producing).
    fn scanImports(self: *HandlerVerifier) void {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            if (tag != .import_decl) continue;

            const import_decl = self.ir_view.getImportDecl(idx) orelse continue;
            const module_str = self.ir_view.getString(import_decl.module_idx) orelse continue;

            // Check if this module has any tracked functions
            var module_has_tracked = false;
            for (&tracked_functions) |entry| {
                if (std.mem.eql(u8, entry.module, module_str)) {
                    module_has_tracked = true;
                    break;
                }
            }
            if (!module_has_tracked) continue;

            // Scan specifiers to find which local bindings map to tracked functions
            var j: u8 = 0;
            while (j < import_decl.specifiers_count) : (j += 1) {
                const spec_idx = self.ir_view.getListIndex(import_decl.specifiers_start, j);
                const spec = self.ir_view.getImportSpec(spec_idx) orelse continue;

                const imported_name = self.resolveAtomName(spec.imported_atom) orelse continue;
                const produces = lookupTrackedFunction(imported_name) orelse continue;

                switch (produces) {
                    .result => {
                        self.result_function_slots.append(self.allocator, spec.local_binding.slot) catch continue;
                    },
                    .optional_string, .optional_object => {
                        self.optional_function_slots.append(self.allocator, .{
                            .slot = spec.local_binding.slot,
                            .kind = produces.toOptionalKind().?,
                        }) catch continue;
                    },
                }
            }
        }
    }

    /// Walk the handler body to track result bindings and reference counts.
    fn walkForResultsAndRefs(self: *HandlerVerifier, node: NodeIndex) void {
        const tag = self.ir_view.getTag(node) orelse return;

        switch (tag) {
            .block, .program => {
                const block = self.ir_view.getBlock(node) orelse return;
                var i: u16 = 0;
                while (i < block.stmts_count) : (i += 1) {
                    const stmt_idx = self.ir_view.getListIndex(block.stmts_start, i);
                    self.walkForResultsAndRefs(stmt_idx);
                }
            },
            .var_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return;

                // Track this binding for dead variable detection (scope-aware)
                self.all_bindings.append(self.allocator, .{
                    .scope_id = decl.binding.scope_id,
                    .slot = decl.binding.slot,
                    .is_result = false,
                    .ok_checked = false,
                    .ref_count = 0,
                    .decl_node = node,
                    .name_idx = 0,
                }) catch {};

                // Check if the init expression is a call to a result-producing function
                if (decl.init != null_node) {
                    self.walkExprForRefs(decl.init);

                    if (self.isResultProducingCall(decl.init)) {
                        self.result_bindings.append(self.allocator, .{
                            .scope_id = decl.binding.scope_id,
                            .slot = decl.binding.slot,
                            .is_result = true,
                            .ok_checked = false,
                            .ref_count = 0,
                            .decl_node = node,
                            .name_idx = 0,
                        }) catch {};
                    }

                    // Check 6: track optional-producing calls
                    // Skip if RHS is `optionalCall() ?? default` (nullish coalesce resolves)
                    if (!self.isNullishCoalesceWithOptionalCall(decl.init)) {
                        if (self.isOptionalProducingCall(decl.init)) |kind| {
                            self.optional_bindings.append(self.allocator, .{
                                .scope_id = decl.binding.scope_id,
                                .slot = decl.binding.slot,
                                .kind = kind,
                                .narrowed = false,
                                .decl_node = node,
                            }) catch {};
                        }
                    }
                }
            },
            .if_stmt => {
                const if_stmt = self.ir_view.getIfStmt(node) orelse return;

                // Always walk the condition for ref-counting (identifiers in
                // conditions must be counted regardless of which extraction path
                // matches). The extraction helpers are read-only so order is safe.
                self.walkExprForRefs(if_stmt.condition);

                // Check if condition is a result.ok check
                const checked_slot = self.extractResultOkCheck(if_stmt.condition);

                if (checked_slot) |slot| {
                    // Mark the result as checked in the then-branch scope
                    self.setResultChecked(slot, true);
                    self.walkForResultsAndRefs(if_stmt.then_branch);
                    self.setResultChecked(slot, false);

                    // In the else branch, .ok is known to be false
                    if (if_stmt.else_branch != null_node) {
                        self.walkForResultsAndRefs(if_stmt.else_branch);
                    }
                } else if (self.extractNegatedResultOkCheck(if_stmt.condition)) |slot| {
                    // Negated pattern: if (!result.ok) { return ...; }
                    self.walkForResultsAndRefs(if_stmt.then_branch);

                    // If the then branch always returns, code after is the ok path
                    const then_returns = self.stmtReturnsQuick(if_stmt.then_branch);
                    if (then_returns == .always) {
                        self.setResultChecked(slot, true);
                    }

                    if (if_stmt.else_branch != null_node) {
                        self.setResultChecked(slot, true);
                        self.walkForResultsAndRefs(if_stmt.else_branch);
                        self.setResultChecked(slot, false);
                    }
                } else if (self.extractOptionalNarrowingCheck(if_stmt.condition)) |info| {
                    // Check 6: optional narrowing via if (val) or if (val !== undefined)
                    switch (info.branch) {
                        .then => {
                            // if (val) - narrowed in then-branch only
                            self.setOptionalNarrowed(info.slot, info.scope_id, true);
                            self.walkForResultsAndRefs(if_stmt.then_branch);
                            self.setOptionalNarrowed(info.slot, info.scope_id, false);
                            if (if_stmt.else_branch != null_node) {
                                self.walkForResultsAndRefs(if_stmt.else_branch);
                            }
                        },
                        .then_returns_early => {
                            // if (!val) { return ...; } - code after is narrowed
                            self.walkForResultsAndRefs(if_stmt.then_branch);

                            const then_returns = self.stmtReturnsQuick(if_stmt.then_branch);
                            if (then_returns == .always) {
                                // Then branch always returns, so subsequent code is narrowed
                                self.setOptionalNarrowed(info.slot, info.scope_id, true);
                            }

                            if (if_stmt.else_branch != null_node) {
                                self.setOptionalNarrowed(info.slot, info.scope_id, true);
                                self.walkForResultsAndRefs(if_stmt.else_branch);
                                // Don't restore if then_returns - leave narrowed for subsequent code
                                if (then_returns != .always) {
                                    self.setOptionalNarrowed(info.slot, info.scope_id, false);
                                }
                            }
                        },
                    }
                } else {
                    self.walkForResultsAndRefs(if_stmt.then_branch);
                    if (if_stmt.else_branch != null_node) {
                        self.walkForResultsAndRefs(if_stmt.else_branch);
                    }
                }
            },
            .switch_stmt => {
                const switch_stmt = self.ir_view.getSwitchStmt(node) orelse return;
                self.walkExprForRefs(switch_stmt.discriminant);
                var i: u8 = 0;
                while (i < switch_stmt.cases_count) : (i += 1) {
                    const case_idx = self.ir_view.getListIndex(switch_stmt.cases_start, i);
                    const case_clause = self.ir_view.getCaseClause(case_idx) orelse continue;
                    if (case_clause.test_expr != null_node) {
                        self.walkExprForRefs(case_clause.test_expr);
                    }
                    var j: u16 = 0;
                    while (j < case_clause.body_count) : (j += 1) {
                        const stmt_idx = self.ir_view.getListIndex(case_clause.body_start, j);
                        self.walkForResultsAndRefs(stmt_idx);
                    }
                }
            },
            .for_of_stmt => {
                const for_iter = self.ir_view.getForIter(node) orelse return;
                self.walkExprForRefs(for_iter.iterable);
                self.walkForResultsAndRefs(for_iter.body);
            },
            .return_stmt => {
                const opt_val = self.ir_view.getOptValue(node);
                if (opt_val) |val| {
                    self.walkExprForRefs(val);
                }
            },
            .expr_stmt => {
                const opt_val = self.ir_view.getOptValue(node);
                if (opt_val) |val| {
                    self.walkExprForRefs(val);
                }
            },
            else => {},
        }
    }

    /// Walk an expression for reference counting and result access checking.
    fn walkExprForRefs(self: *HandlerVerifier, node: NodeIndex) void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;

        switch (tag) {
            .identifier => {
                const binding = self.ir_view.getBinding(node) orelse return;
                self.incrementRefCount(binding);
            },
            .member_access, .optional_chain => {
                const member = self.ir_view.getMember(node) orelse return;
                self.walkExprForRefs(member.object);
                self.checkResultValueAccess(member, node);

                // optional_chain (val?.prop) safely handles undefined - skip optional check
                if (tag == .member_access) {
                    self.checkOptionalObjectAccess(member, node);
                }
            },
            .binary_op => {
                const binary = self.ir_view.getBinary(node) orelse return;
                self.walkExprForRefs(binary.left);
                self.walkExprForRefs(binary.right);

                // Check 6: optional used in non-nullish, non-comparison binary ops
                if (binary.op != .nullish and
                    binary.op != .strict_eq and binary.op != .strict_neq and
                    binary.op != .eq and binary.op != .neq)
                {
                    self.checkOptionalUse(binary.left);
                    self.checkOptionalUse(binary.right);
                }
            },
            .unary_op => {
                const unary = self.ir_view.getUnary(node) orelse return;
                self.walkExprForRefs(unary.operand);
            },
            .call, .method_call => {
                const call = self.ir_view.getCall(node) orelse return;
                self.walkExprForRefs(call.callee);
                var i: u8 = 0;
                while (i < call.args_count) : (i += 1) {
                    const arg_idx = self.ir_view.getListIndex(call.args_start, i);
                    self.walkExprForRefs(arg_idx);
                    // Check 6: optional passed as argument
                    self.checkOptionalUse(arg_idx);
                }
            },
            .ternary => {
                const ternary = self.ir_view.getTernary(node) orelse return;
                self.walkExprForRefs(ternary.condition);
                self.walkExprForRefs(ternary.then_branch);
                self.walkExprForRefs(ternary.else_branch);
            },
            .match_expr => {
                const match_e = self.ir_view.getMatchExpr(node) orelse return;
                self.walkExprForRefs(match_e.discriminant);
                var has_default = false;
                var mi: u8 = 0;
                while (mi < match_e.arms_count) : (mi += 1) {
                    const arm_idx = self.ir_view.getListIndex(match_e.arms_start, mi);
                    const arm = self.ir_view.getMatchArm(arm_idx) orelse continue;
                    if (arm.pattern == null_node) has_default = true;
                    self.walkExprForRefs(arm.body);
                }
                if (!has_default) {
                    self.addDiagnostic(.{
                        .severity = .warning,
                        .kind = .non_exhaustive_match,
                        .node = node,
                        .message = "match expression without default arm may not produce a value",
                        .help = "add 'default:' or 'when _:' arm to handle all cases",
                    });
                }
            },
            .assignment => {
                const assign = self.ir_view.getAssignment(node) orelse return;
                self.walkExprForRefs(assign.target);
                self.walkExprForRefs(assign.value);

                // Check 6: reassignment to non-optional clears tracking
                self.handleOptionalReassignment(assign);
            },
            .array_literal => {
                const arr = self.ir_view.getArray(node) orelse return;
                var i: u16 = 0;
                while (i < arr.elements_count) : (i += 1) {
                    const elem = self.ir_view.getListIndex(arr.elements_start, i);
                    self.walkExprForRefs(elem);
                }
            },
            .object_literal => {
                const obj = self.ir_view.getObject(node) orelse return;
                var i: u16 = 0;
                while (i < obj.properties_count) : (i += 1) {
                    const prop_idx = self.ir_view.getListIndex(obj.properties_start, i);
                    const prop = self.ir_view.getProperty(prop_idx) orelse continue;
                    self.walkExprForRefs(prop.value);
                    // Check 6: optional used as property value
                    self.checkOptionalUse(prop.value);
                }
            },
            .template_literal => {
                const tmpl = self.ir_view.getTemplate(node) orelse return;
                var i: u8 = 0;
                while (i < tmpl.parts_count) : (i += 1) {
                    const part_idx = self.ir_view.getListIndex(tmpl.parts_start, i);
                    const part_tag = self.ir_view.getTag(part_idx) orelse continue;
                    if (part_tag == .template_part_expr) {
                        const opt_val = self.ir_view.getOptValue(part_idx);
                        if (opt_val) |val| {
                            self.walkExprForRefs(val);
                            // Check 6: optional in template literal
                            self.checkOptionalUse(val);
                        }
                    }
                }
            },
            .spread => {
                const unary = self.ir_view.getUnary(node) orelse return;
                self.walkExprForRefs(unary.operand);
            },
            .computed_access => {
                const member = self.ir_view.getMember(node) orelse return;
                self.walkExprForRefs(member.object);
                if (member.computed != null_node) {
                    self.walkExprForRefs(member.computed);
                }
            },
            else => {},
        }
    }

    /// Check if a call expression calls a result-producing function.
    fn isResultProducingCall(self: *HandlerVerifier, node: NodeIndex) bool {
        const tag = self.ir_view.getTag(node) orelse return false;
        if (tag != .call and tag != .method_call) return false;

        const call = self.ir_view.getCall(node) orelse return false;
        const callee_tag = self.ir_view.getTag(call.callee) orelse return false;

        if (callee_tag == .identifier) {
            const binding = self.ir_view.getBinding(call.callee) orelse return false;
            // Check if this binding slot is a known result-producing function
            for (self.result_function_slots.items) |slot| {
                if (slot == binding.slot) return true;
            }
        }

        return false;
    }

    /// Extract the binding slot from a `result.ok` condition check.
    /// Recognizes: `result.ok`, `result.ok === true`, `result.ok !== false`,
    ///             `result.isOk()` (method call on member access)
    fn extractResultOkCheck(self: *HandlerVerifier, cond_node: NodeIndex) ?u16 {
        const tag = self.ir_view.getTag(cond_node) orelse return null;

        // Direct: result.ok or result.isOk
        if (tag == .member_access or tag == .optional_chain) {
            return self.extractResultMemberSlot(cond_node, &ok_atoms);
        }

        // Method call: result.isOk()
        if (tag == .call or tag == .method_call) {
            const call = self.ir_view.getCall(cond_node) orelse return null;
            if (call.args_count == 0) {
                return self.extractResultMemberSlot(call.callee, &ok_atoms);
            }
        }

        // Comparison: result.ok === true, etc.
        if (tag == .binary_op) {
            const binary = self.ir_view.getBinary(cond_node) orelse return null;
            if (binary.op == .strict_eq or binary.op == .eq) {
                if (self.extractResultMemberSlot(binary.left, &ok_atoms)) |slot| return slot;
                if (self.extractResultMemberSlot(binary.right, &ok_atoms)) |slot| return slot;
            }
        }

        return null;
    }

    /// Extract the binding slot from `!result.ok`, `result.ok === false`,
    /// or `result.isErr()`.
    fn extractNegatedResultOkCheck(self: *HandlerVerifier, cond_node: NodeIndex) ?u16 {
        const tag = self.ir_view.getTag(cond_node) orelse return null;

        // Negation: !result.ok
        if (tag == .unary_op) {
            const unary = self.ir_view.getUnary(cond_node) orelse return null;
            if (unary.op == .not) {
                return self.extractResultOkCheck(unary.operand);
            }
        }

        // Method call: result.isErr()
        if (tag == .call or tag == .method_call) {
            const call = self.ir_view.getCall(cond_node) orelse return null;
            if (call.args_count == 0) {
                return self.extractResultMemberSlot(call.callee, &err_atoms);
            }
        }

        // Comparison: result.ok === false, result.ok !== true
        if (tag == .binary_op) {
            const binary = self.ir_view.getBinary(cond_node) orelse return null;
            if (binary.op == .strict_neq or binary.op == .neq) {
                // result.ok !== true
                if (self.extractResultMemberSlot(binary.left, &ok_atoms)) |slot| {
                    if (self.isTrueLiteral(binary.right)) return slot;
                }
            }
            if (binary.op == .strict_eq or binary.op == .eq) {
                // result.ok === false
                if (self.extractResultMemberSlot(binary.left, &ok_atoms)) |slot| {
                    if (self.isFalseLiteral(binary.right)) return slot;
                }
            }
        }

        return null;
    }

    /// Find a result binding by slot, or null if not tracked.
    fn findResultBinding(self: *HandlerVerifier, slot: u16) ?*BindingState {
        for (self.result_bindings.items) |*rb| {
            if (rb.slot == slot) return rb;
        }
        return null;
    }

    const ok_atoms = [_]u16{ @intFromEnum(object.Atom.ok), @intFromEnum(object.Atom.isOk) };
    const err_atoms = [_]u16{@intFromEnum(object.Atom.isErr)};

    /// Given a member_access node, return the object's binding slot if the property
    /// matches one of `accepted_atoms` and the object is a tracked result binding.
    fn extractResultMemberSlot(self: *HandlerVerifier, node: NodeIndex, accepted_atoms: []const u16) ?u16 {
        const tag = self.ir_view.getTag(node) orelse return null;
        if (tag != .member_access and tag != .optional_chain) return null;

        const member = self.ir_view.getMember(node) orelse return null;

        // Check property atom against accepted list
        var found = false;
        for (accepted_atoms) |atom| {
            if (member.property == atom) {
                found = true;
                break;
            }
        }
        if (!found) return null;

        // Check object is an identifier that's a tracked result
        const obj_tag = self.ir_view.getTag(member.object) orelse return null;
        if (obj_tag != .identifier) return null;
        const binding = self.ir_view.getBinding(member.object) orelse return null;

        if (self.findResultBinding(binding.slot)) |_| return binding.slot;
        return null;
    }

    fn isTrueLiteral(self: *HandlerVerifier, node: NodeIndex) bool {
        const tag = self.ir_view.getTag(node) orelse return false;
        if (tag != .lit_bool) return false;
        return self.ir_view.getBoolValue(node) orelse false;
    }

    fn isFalseLiteral(self: *HandlerVerifier, node: NodeIndex) bool {
        const tag = self.ir_view.getTag(node) orelse return false;
        if (tag != .lit_bool) return false;
        const val = self.ir_view.getBoolValue(node) orelse return false;
        return !val;
    }

    /// Check if a member access on a result binding accesses .value/.unwrap()
    /// without prior .ok check.
    fn checkResultValueAccess(self: *HandlerVerifier, member: Node.MemberExpr, node: NodeIndex) void {
        // Check property - we care about .value (180), .unwrap (202) access
        const prop = member.property;
        const is_value_access = (prop == @intFromEnum(object.Atom.value) or
            prop == @intFromEnum(object.Atom.unwrap));
        if (!is_value_access) return;

        // Check if the object is a result binding
        const obj_tag = self.ir_view.getTag(member.object) orelse return;
        if (obj_tag != .identifier) return;
        const binding = self.ir_view.getBinding(member.object) orelse return;

        const rb = self.findResultBinding(binding.slot) orelse return;
        if (!rb.ok_checked) {
            self.addDiagnostic(.{
                .severity = .err,
                .kind = .unchecked_result_value,
                .node = node,
                .message = "result.value accessed without checking result.ok first",
                .help = "check result.ok before accessing result.value:\n           if (result.ok) { ... result.value ... }",
            });
        }
    }

    /// Set the ok_checked state for a result binding slot.
    fn setResultChecked(self: *HandlerVerifier, slot: u16, checked: bool) void {
        if (self.findResultBinding(slot)) |rb| {
            rb.ok_checked = checked;
        }
    }

    /// Increment reference count for a binding (scope-aware).
    fn incrementRefCount(self: *HandlerVerifier, binding_ref: ir.BindingRef) void {
        const target_key = bindingKey(binding_ref.scope_id, binding_ref.slot);
        for (self.all_bindings.items) |*binding| {
            if (binding.key() == target_key) {
                binding.ref_count +|= 1;
                return;
            }
        }
    }

    /// Resolve an atom index to its string name.
    /// Uses predefined atom names or the atom table for dynamic atoms.
    fn resolveAtomName(self: *HandlerVerifier, atom_idx: u16) ?[]const u8 {
        const atom: object.Atom = @enumFromInt(atom_idx);
        if (atom.isPredefined()) {
            return atom.toPredefinedName();
        }
        if (self.atoms) |at| {
            return at.getName(atom);
        }
        return null;
    }

    /// Quick return status check (without emitting diagnostics).
    /// Intentionally separate from stmtReturns: that function is the full Check 1 analysis
    /// pass that emits diagnostics (unreachable code, missing default, etc.). This one is a
    /// lightweight predicate used by Check 2 and Check 6 to decide if a then-branch always
    /// returns (enabling post-dominator narrowing). Merging them would require threading a
    /// "suppress diagnostics" flag through the entire Check 1 call tree.
    fn stmtReturnsQuick(self: *HandlerVerifier, node: NodeIndex) ReturnStatus {
        const tag = self.ir_view.getTag(node) orelse return .never;
        return switch (tag) {
            .return_stmt => .always,
            .block, .program => blk: {
                const block = self.ir_view.getBlock(node) orelse break :blk .never;
                if (block.stmts_count == 0) break :blk .never;
                var i: u16 = 0;
                while (i < block.stmts_count) : (i += 1) {
                    const stmt_idx = self.ir_view.getListIndex(block.stmts_start, i);
                    if (self.stmtReturnsQuick(stmt_idx) == .always) break :blk .always;
                }
                break :blk .never;
            },
            .if_stmt => blk: {
                const if_stmt = self.ir_view.getIfStmt(node) orelse break :blk .never;
                const then_s = self.stmtReturnsQuick(if_stmt.then_branch);
                if (if_stmt.else_branch == null_node) break :blk if (then_s == .always) .sometimes else .never;
                const else_s = self.stmtReturnsQuick(if_stmt.else_branch);
                break :blk if (then_s == .always and else_s == .always) .always else .sometimes;
            },
            else => .never,
        };
    }

    // -----------------------------------------------------------------------
    // Check 4: Dead Variables
    // -----------------------------------------------------------------------

    fn reportUnusedVariables(self: *HandlerVerifier) void {
        for (self.all_bindings.items) |binding| {
            if (binding.ref_count > 0) continue;

            // Skip special bindings (slot 0 is typically the module scope)
            if (binding.slot == 0) continue;

            // Skip _-prefixed names (convention for intentionally unused)
            // For global bindings, slot is the atom index
            if (binding.scope_id == 0) {
                if (self.resolveAtomName(binding.slot)) |name| {
                    if (name.len > 0 and name[0] == '_') continue;
                }
            }
            // For named string index
            if (binding.name_idx != 0) {
                if (self.ir_view.getString(binding.name_idx)) |name| {
                    if (name.len > 0 and name[0] == '_') continue;
                }
            }

            self.addDiagnostic(.{
                .severity = .warning,
                .kind = .unused_variable,
                .node = binding.decl_node,
                .message = "declared variable is never used",
                .help = "remove the unused variable, or prefix with '_' to suppress this warning",
            });
        }
    }

    // -----------------------------------------------------------------------
    // Check 6: Optional Value Checking
    // -----------------------------------------------------------------------

    /// Check if a call expression calls an optional-producing function.
    fn isOptionalProducingCall(self: *HandlerVerifier, node: NodeIndex) ?OptionalKind {
        const call_tag = self.ir_view.getTag(node) orelse return null;
        if (call_tag != .call and call_tag != .method_call) return null;

        const call = self.ir_view.getCall(node) orelse return null;
        const callee_tag = self.ir_view.getTag(call.callee) orelse return null;

        if (callee_tag == .identifier) {
            const binding = self.ir_view.getBinding(call.callee) orelse return null;
            for (self.optional_function_slots.items) |opt_fn| {
                if (opt_fn.slot == binding.slot) return opt_fn.kind;
            }
        }

        return null;
    }

    /// Check if the init is `optionalCall() ?? default` (nullish coalesce resolves optionality).
    fn isNullishCoalesceWithOptionalCall(self: *HandlerVerifier, node: NodeIndex) bool {
        const t = self.ir_view.getTag(node) orelse return false;
        if (t != .binary_op) return false;

        const binary = self.ir_view.getBinary(node) orelse return false;
        if (binary.op != .nullish) return false;

        // LHS must be an optional-producing call
        return self.isOptionalProducingCall(binary.left) != null;
    }

    /// Extract optional narrowing info from an if-condition.
    /// Recognizes: if (val), if (!val), if (val !== undefined), if (val === undefined).
    fn extractOptionalNarrowingCheck(self: *HandlerVerifier, cond_node: NodeIndex) ?NarrowingInfo {
        const cond_tag = self.ir_view.getTag(cond_node) orelse return null;

        // if (val) - truthiness check
        if (cond_tag == .identifier) {
            const binding = self.ir_view.getBinding(cond_node) orelse return null;
            if (self.findOptionalBinding(binding.slot, binding.scope_id)) |_| {
                return .{ .slot = binding.slot, .scope_id = binding.scope_id, .branch = .then };
            }
        }

        // if (!val) - negated truthiness
        if (cond_tag == .unary_op) {
            const unary = self.ir_view.getUnary(cond_node) orelse return null;
            if (unary.op == .not) {
                const inner_tag = self.ir_view.getTag(unary.operand) orelse return null;
                if (inner_tag == .identifier) {
                    const binding = self.ir_view.getBinding(unary.operand) orelse return null;
                    if (self.findOptionalBinding(binding.slot, binding.scope_id)) |_| {
                        return .{ .slot = binding.slot, .scope_id = binding.scope_id, .branch = .then_returns_early };
                    }
                }
            }
        }

        // if (val !== undefined) or if (val === undefined)
        if (cond_tag == .binary_op) {
            const binary = self.ir_view.getBinary(cond_node) orelse return null;
            if (binary.op == .strict_neq) {
                if (self.extractUndefinedComparisonSlot(binary)) |info| {
                    return .{ .slot = info.slot, .scope_id = info.scope_id, .branch = .then };
                }
            }
            if (binary.op == .strict_eq) {
                if (self.extractUndefinedComparisonSlot(binary)) |info| {
                    return .{ .slot = info.slot, .scope_id = info.scope_id, .branch = .then_returns_early };
                }
            }
        }

        return null;
    }

    /// Extract slot from `val !== undefined` or `undefined !== val`.
    fn extractUndefinedComparisonSlot(self: *HandlerVerifier, binary: Node.BinaryExpr) ?struct { slot: u16, scope_id: ir.ScopeId } {
        // Check left=identifier, right=undefined
        const left_tag = self.ir_view.getTag(binary.left) orelse return null;
        const right_tag = self.ir_view.getTag(binary.right) orelse return null;

        if (left_tag == .identifier and right_tag == .lit_undefined) {
            const binding = self.ir_view.getBinding(binary.left) orelse return null;
            if (self.findOptionalBinding(binding.slot, binding.scope_id) != null) {
                return .{ .slot = binding.slot, .scope_id = binding.scope_id };
            }
        }

        // Check left=undefined, right=identifier
        if (left_tag == .lit_undefined and right_tag == .identifier) {
            const binding = self.ir_view.getBinding(binary.right) orelse return null;
            if (self.findOptionalBinding(binding.slot, binding.scope_id) != null) {
                return .{ .slot = binding.slot, .scope_id = binding.scope_id };
            }
        }

        return null;
    }

    /// Find an optional binding by slot and scope.
    fn findOptionalBinding(self: *HandlerVerifier, slot: u16, scope_id: ir.ScopeId) ?*OptionalBindingState {
        const target_key = bindingKey(scope_id, slot);
        for (self.optional_bindings.items) |*ob| {
            if (ob.key() == target_key) return ob;
        }
        return null;
    }

    /// Resolve an identifier node to its un-narrowed optional binding, if any.
    fn getUnnarrowedOptional(self: *HandlerVerifier, identifier_node: NodeIndex) ?*OptionalBindingState {
        const binding = self.ir_view.getBinding(identifier_node) orelse return null;
        const ob = self.findOptionalBinding(binding.slot, binding.scope_id) orelse return null;
        if (ob.narrowed) return null;
        return ob;
    }

    /// Set the narrowed state for an optional binding.
    fn setOptionalNarrowed(self: *HandlerVerifier, slot: u16, scope_id: ir.ScopeId, narrowed: bool) void {
        if (self.findOptionalBinding(slot, scope_id)) |ob| {
            ob.narrowed = narrowed;
        }
    }

    /// Check if a node is an un-narrowed optional identifier, and emit diagnostic.
    fn checkOptionalUse(self: *HandlerVerifier, node: NodeIndex) void {
        const t = self.ir_view.getTag(node) orelse return;
        if (t != .identifier) return;
        if (self.getUnnarrowedOptional(node) == null) return;

        self.addDiagnostic(.{
            .severity = .err,
            .kind = .unchecked_optional_use,
            .node = node,
            .message = "optional value used without checking for undefined",
            .help = "check before use: if (val !== undefined) { ... }\n           or provide a default: val ?? \"fallback\"",
        });
    }

    /// Check if a member_access is on an un-narrowed optional_object binding.
    fn checkOptionalObjectAccess(self: *HandlerVerifier, member: Node.MemberExpr, node: NodeIndex) void {
        const obj_tag = self.ir_view.getTag(member.object) orelse return;
        if (obj_tag != .identifier) return;

        const ob = self.getUnnarrowedOptional(member.object) orelse return;
        if (ob.kind != .optional_object) return;

        self.addDiagnostic(.{
            .severity = .err,
            .kind = .unchecked_optional_access,
            .node = node,
            .message = "property access on optional value without checking for undefined",
            .help = "check before access: if (val) { ... val.prop ... }\n           or use optional chaining: val?.prop",
        });
    }

    /// Handle reassignment: if target is a tracked optional and RHS is not optional-producing,
    /// mark as permanently narrowed.
    fn handleOptionalReassignment(self: *HandlerVerifier, assign: Node.AssignExpr) void {
        const target_tag = self.ir_view.getTag(assign.target) orelse return;
        if (target_tag != .identifier) return;

        const binding = self.ir_view.getBinding(assign.target) orelse return;
        const ob = self.findOptionalBinding(binding.slot, binding.scope_id) orelse return;

        // If RHS is another optional-producing call, keep tracking
        if (self.isOptionalProducingCall(assign.value) != null) return;

        // Reassignment to non-optional resolves optionality
        ob.narrowed = true;
    }

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    fn addDiagnostic(self: *HandlerVerifier, diag: Diagnostic) void {
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

/// Find the handler function in a parsed program.
/// Looks for `function handler(...)` or `const handler = ...` at top level.
pub fn findHandlerFunction(ir_view: IrView, root: NodeIndex) ?NodeIndex {
    const tag = ir_view.getTag(root) orelse return null;
    if (tag != .program and tag != .block) return null;
    const block = ir_view.getBlock(root) orelse return null;

    var i: u16 = 0;
    while (i < block.stmts_count) : (i += 1) {
        const stmt_idx = ir_view.getListIndex(block.stmts_start, i);
        const stmt_tag = ir_view.getTag(stmt_idx) orelse continue;

        if (stmt_tag == .function_decl or stmt_tag == .var_decl) {
            const decl = ir_view.getVarDecl(stmt_idx) orelse continue;
            if (decl.binding.kind != .global) continue;
            if (decl.binding.slot != @intFromEnum(object.Atom.handler)) continue;

            const init_tag = ir_view.getTag(decl.init) orelse continue;
            if (init_tag == .function_expr or init_tag == .arrow_function) {
                return decl.init;
            }
        }
    }

    return null;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "ReturnStatus enum" {
    try std.testing.expectEqual(ReturnStatus.always, ReturnStatus.always);
    try std.testing.expectEqual(ReturnStatus.never, ReturnStatus.never);
    try std.testing.expectEqual(ReturnStatus.sometimes, ReturnStatus.sometimes);
}

test "Severity labels" {
    try std.testing.expectEqualStrings("error", Severity.err.label());
    try std.testing.expectEqualStrings("warning", Severity.warning.label());
}

test "DiagnosticKind enum values" {
    // Ensure all variants are distinct
    const kinds = [_]DiagnosticKind{
        .missing_return_else,
        .missing_return_default,
        .missing_return_path,
        .unchecked_result_value,
        .unreachable_after_return,
        .unused_variable,
        .unused_import,
        .non_exhaustive_match,
        .unchecked_optional_use,
        .unchecked_optional_access,
    };
    for (kinds, 0..) |k, i| {
        for (kinds, 0..) |k2, j| {
            if (i != j) {
                try std.testing.expect(k != k2);
            }
        }
    }
}

test "lookupTrackedFunction" {
    // Result-producing
    try std.testing.expectEqual(FunctionProduces.result, lookupTrackedFunction("jwtVerify").?);
    try std.testing.expectEqual(FunctionProduces.result, lookupTrackedFunction("validateJson").?);
    try std.testing.expectEqual(FunctionProduces.result, lookupTrackedFunction("validateObject").?);
    try std.testing.expectEqual(FunctionProduces.result, lookupTrackedFunction("coerceJson").?);
    // Optional-producing
    try std.testing.expectEqual(FunctionProduces.optional_string, lookupTrackedFunction("env").?);
    try std.testing.expectEqual(FunctionProduces.optional_string, lookupTrackedFunction("cacheGet").?);
    try std.testing.expectEqual(FunctionProduces.optional_string, lookupTrackedFunction("parseBearer").?);
    try std.testing.expectEqual(FunctionProduces.optional_object, lookupTrackedFunction("routerMatch").?);
    // Untracked
    try std.testing.expect(lookupTrackedFunction("sha256") == null);
    try std.testing.expect(lookupTrackedFunction("cacheSet") == null);
}

test "getSourceLine" {
    const source = "line one\nline two\nline three";
    try std.testing.expectEqualStrings("line one", getSourceLine(source, 1).?);
    try std.testing.expectEqualStrings("line two", getSourceLine(source, 2).?);
    try std.testing.expectEqualStrings("line three", getSourceLine(source, 3).?);
    try std.testing.expect(getSourceLine(source, 4) == null);
}

test "verifier init and deinit" {
    // Create a minimal IR with just a program node
    var store = ir.IRStore.init(std.testing.allocator);
    defer store.deinit();

    var constants = ir.ConstantPool.init(std.testing.allocator);
    defer constants.deinit();

    const view = IrView.fromIRStore(&store, &constants);

    var verifier = HandlerVerifier.init(std.testing.allocator, view, null);
    defer verifier.deinit();

    try std.testing.expectEqual(@as(usize, 0), verifier.getDiagnostics().len);
    try std.testing.expect(!verifier.hasErrors());
}

test "diagnostic formatting" {
    var store = ir.IRStore.init(std.testing.allocator);
    defer store.deinit();

    var constants = ir.ConstantPool.init(std.testing.allocator);
    defer constants.deinit();

    // Add a node with a source location
    _ = try store.addNode(.return_stmt, .{ .line = 5, .column = 3, .offset = 42 }, .{ .a = null_node, .b = 0 });

    const view = IrView.fromIRStore(&store, &constants);

    var verifier = HandlerVerifier.init(std.testing.allocator, view, null);
    defer verifier.deinit();

    verifier.addDiagnostic(.{
        .severity = .err,
        .kind = .missing_return_path,
        .node = 0,
        .message = "not all code paths return a Response",
        .help = "add return statements",
    });

    try std.testing.expectEqual(@as(usize, 1), verifier.getDiagnostics().len);
    try std.testing.expect(verifier.hasErrors());

    // Test formatting doesn't crash
    const source = "line 1\nline 2\nline 3\nline 4\nline 5 has code\n";
    var output_buf: std.ArrayList(u8) = .empty;
    defer output_buf.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &output_buf);
    try verifier.formatDiagnostics(source, &aw.writer);
    output_buf = aw.toArrayList();
    try std.testing.expect(output_buf.items.len > 0);
    try std.testing.expect(std.mem.indexOf(u8, output_buf.items, "verify error") != null);
}
