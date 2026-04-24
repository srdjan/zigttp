//! Compile-Time Data Flow Provenance Checker
//!
//! Tracks where data comes from (sources with labels) and where it goes
//! (sinks with policies), proving security properties at compile time:
//!   - Secrets (env vars with sensitive names) never reach response bodies
//!   - Credentials (auth tokens, JWTs) never appear in logs
//!   - User input is validated before use in external egress
//!
//! This is possible because zigttp's JS subset has no eval, no exceptions,
//! no dynamic property access beyond known patterns, and virtual modules are
//! the ONLY I/O boundary. The IR tree IS the control flow graph. Data flow
//! analysis is a simple recursive tree walk, not an expensive fixpoint.
//!
//! Labels are auto-derived from ModuleBinding.return_labels and refined by
//! env var naming conventions. No user annotations required for basic proofs.

const std = @import("std");
const ir = @import("parser/ir.zig");
const object = @import("object.zig");
const context = @import("context.zig");
const builtin_modules = @import("builtin_modules.zig");
const mb = @import("module_binding.zig");
const bool_checker_mod = @import("bool_checker.zig");
const counterexample = @import("counterexample.zig");

const Node = ir.Node;
const NodeIndex = ir.NodeIndex;
const NodeTag = ir.NodeTag;
const IrView = ir.IrView;
const null_node = ir.null_node;
const LabelSet = mb.LabelSet;
const DataLabel = mb.DataLabel;

const packBindingKey = bool_checker_mod.packBindingKey;
const getSourceLine = bool_checker_mod.getSourceLine;

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
    secret_in_response, // {secret} data in Response body/headers
    credential_in_response, // {credential} data in Response body
    secret_in_log, // {secret} data in console.log/warn/error
    credential_in_log, // {credential} data in console.log/warn/error
    secret_in_egress_url, // {secret} data in fetchSync URL
    credential_in_egress_url, // {credential} data in fetchSync URL
    secret_in_egress_body, // {secret} data in fetchSync body
    unvalidated_input_in_egress, // {user_input} without {validated} in fetchSync
};

/// Snapshot of the branch constraints and module calls observed on the
/// path that witnessed a flow sink. Present on diagnostics whose kind
/// maps to a `counterexample.PropertyTag`; the solver turns it into an
/// executable request + stub sequence. Owned by the FlowChecker
/// allocator; freed in `FlowChecker.deinit`.
pub const Witness = struct {
    path_constraints: []const counterexample.WitnessConstraint,
    io_calls: []const counterexample.TrackedIoCall,
};

pub const Diagnostic = struct {
    severity: Severity,
    kind: DiagnosticKind,
    node: NodeIndex,
    message: []const u8,
    help: ?[]const u8,
    witness: ?Witness = null,
};

/// Map a diagnostic kind to the property tag it witnesses. `null` when the
/// diagnostic does not correspond to a currently modelled property (the
/// counterexample surface is a subset of flow diagnostics by design).
pub fn propertyTagForKind(kind: DiagnosticKind) ?counterexample.PropertyTag {
    return switch (kind) {
        .secret_in_response,
        .secret_in_log,
        .secret_in_egress_url,
        .secret_in_egress_body,
        => .no_secret_leakage,
        .credential_in_response,
        .credential_in_log,
        .credential_in_egress_url,
        => .no_credential_leakage,
        .unvalidated_input_in_egress => .injection_safe,
    };
}

/// Proven data flow properties for a handler.
pub const FlowProperties = struct {
    /// No {secret} data reaches response bodies, headers, or external egress.
    no_secret_leakage: bool = true,
    /// No {credential} data reaches response bodies or logs.
    no_credential_leakage: bool = true,
    /// All {user_input} data passes through validation before external egress.
    input_validated: bool = true,
    /// {user_input} data does not flow to external egress hosts.
    pii_contained: bool = true,
    /// No unvalidated user input reaches sensitive sinks (egress, HTML responses).
    injection_safe: bool = true,
};

// ---------------------------------------------------------------------------
// External data labels
// ---------------------------------------------------------------------------

/// An externally declared label binding: a field name, its classification,
/// and a human-readable reason shown in diagnostics.
pub const ExternalLabel = struct {
    field: []const u8, // e.g. "User.email" or just "email"
    label: DataLabel,
    reason: []const u8,
};

/// Map a JSON label string to the corresponding DataLabel enum value.
/// Returns null for unrecognized strings.
pub fn parseDataLabel(s: []const u8) ?DataLabel {
    const map = .{
        .{ "secret", DataLabel.secret },
        .{ "credential", DataLabel.credential },
        .{ "user_input", DataLabel.user_input },
        .{ "config", DataLabel.config },
        .{ "internal", DataLabel.internal },
        .{ "external", DataLabel.external },
        .{ "validated", DataLabel.validated },
    };
    inline for (map) |entry| {
        if (std.mem.eql(u8, s, entry[0])) return entry[1];
    }
    return null;
}

/// Parse external label declarations from JSON bytes.
/// Expected format: { "labels": [ { "field": "...", "label": "...", "reason": "..." }, ... ] }
/// Returns owned slice; caller must free with the same allocator.
pub fn parseExternalLabels(allocator: std.mem.Allocator, json_bytes: []const u8) ![]const ExternalLabel {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_bytes, .{});
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidExternalLabels;
    const root = parsed.value.object;
    const labels_val = root.get("labels") orelse return error.InvalidExternalLabels;
    if (labels_val != .array) return error.InvalidExternalLabels;

    const items = labels_val.array.items;
    var result = try allocator.alloc(ExternalLabel, items.len);
    var count: usize = 0;
    errdefer {
        for (result[0..count]) |entry| {
            allocator.free(entry.field);
            allocator.free(entry.reason);
        }
        allocator.free(result);
    }

    for (items) |item| {
        if (item != .object) continue;
        const obj = item.object;

        const field_val = obj.get("field") orelse continue;
        const label_val = obj.get("label") orelse continue;
        const reason_val = obj.get("reason") orelse continue;

        if (field_val != .string or label_val != .string or reason_val != .string) continue;

        const data_label = parseDataLabel(label_val.string) orelse continue;

        // Dupe strings so they outlive the parsed JSON tree
        const field_dupe = try allocator.dupe(u8, field_val.string);
        errdefer allocator.free(field_dupe);
        const reason_dupe = try allocator.dupe(u8, reason_val.string);

        result[count] = .{
            .field = field_dupe,
            .label = data_label,
            .reason = reason_dupe,
        };
        count += 1;
    }

    // Shrink to actual count
    if (count < result.len) {
        result = try allocator.realloc(result, count);
    }
    return result;
}

// ---------------------------------------------------------------------------
// FlowChecker
// ---------------------------------------------------------------------------

pub const FlowChecker = struct {
    allocator: std.mem.Allocator,
    ir_view: IrView,
    atoms: ?*context.AtomTable,
    diagnostics: std.ArrayList(Diagnostic),

    /// Per-binding labels: packed(scope_id, slot) -> LabelSet.
    binding_labels: std.AutoHashMapUnmanaged(u32, LabelSet),
    /// Virtual module import tracking: local slot -> FunctionBinding return_labels.
    module_fn_labels: std.AutoHashMapUnmanaged(u16, LabelSet),
    /// Virtual module function metadata (module name, function name, return kind),
    /// keyed by the same slot as `module_fn_labels`. Populated by `scanImports`
    /// and consumed by the counterexample-witness capture pipeline.
    module_fn_meta: std.AutoHashMapUnmanaged(u16, counterexample.StubInfo),
    /// Per-binding origin: packed(scope_id, slot) -> slot of the module function
    /// whose call initialised this binding. Lets the constraint extractor emit
    /// `stub_truthy` on `if (binding)` patterns.
    binding_origin: std.AutoHashMapUnmanaged(u32, u16),
    /// Result binding tracking: packed(scope_id, slot) -> return_labels from the producing call.
    /// When accessing .value on these bindings, the return_labels are merged in.
    result_binding_labels: std.AutoHashMapUnmanaged(u32, LabelSet),
    /// Handler request parameter binding key (packed scope_id + slot).
    req_binding_key: ?u32,
    /// Slot of the `env` function import (for smart label refinement).
    env_fn_slot: ?u16,
    /// Constraint stack maintained as `walkStmt` descends into conditional
    /// branches. Snapshotted onto every diagnostic at emission time.
    working_constraints: std.ArrayListUnmanaged(counterexample.WitnessConstraint),
    /// Ordered list of virtual-module calls observed by the current walk.
    /// Snapshotted alongside `working_constraints` on diagnostics.
    working_io_calls: std.ArrayListUnmanaged(counterexample.TrackedIoCall),

    /// External label overrides: property name -> LabelSet (additive, merged via OR).
    /// Both qualified ("User.email") and short ("email") forms are registered.
    external_labels: std.StringHashMapUnmanaged(LabelSet),
    /// External label reasons: property name -> human-readable reason for diagnostics.
    external_reasons: std.StringHashMapUnmanaged([]const u8),
    /// Dynamically formatted diagnostic messages that need freeing.
    allocated_messages: std.ArrayListUnmanaged([]const u8),

    properties: FlowProperties,

    pub fn init(allocator: std.mem.Allocator, ir_view: IrView, atoms: ?*context.AtomTable) FlowChecker {
        return .{
            .allocator = allocator,
            .ir_view = ir_view,
            .atoms = atoms,
            .diagnostics = .empty,
            .binding_labels = .empty,
            .module_fn_labels = .empty,
            .module_fn_meta = .empty,
            .binding_origin = .empty,
            .result_binding_labels = .empty,
            .req_binding_key = null,
            .env_fn_slot = null,
            .working_constraints = .empty,
            .working_io_calls = .empty,
            .external_labels = .empty,
            .external_reasons = .empty,
            .allocated_messages = .empty,
            .properties = .{},
        };
    }

    pub fn deinit(self: *FlowChecker) void {
        for (self.diagnostics.items) |d| {
            if (d.witness) |w| {
                if (w.path_constraints.len > 0) self.allocator.free(w.path_constraints);
                if (w.io_calls.len > 0) self.allocator.free(w.io_calls);
            }
        }
        self.diagnostics.deinit(self.allocator);
        self.binding_labels.deinit(self.allocator);
        self.module_fn_labels.deinit(self.allocator);
        self.module_fn_meta.deinit(self.allocator);
        self.binding_origin.deinit(self.allocator);
        self.working_constraints.deinit(self.allocator);
        self.working_io_calls.deinit(self.allocator);
        self.result_binding_labels.deinit(self.allocator);

        // Free dynamically formatted diagnostic messages
        for (self.allocated_messages.items) |msg| {
            self.allocator.free(msg);
        }
        self.allocated_messages.deinit(self.allocator);

        // Free duped key strings and reason strings from external labels
        var label_iter = self.external_labels.iterator();
        while (label_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.external_labels.deinit(self.allocator);

        var reason_iter = self.external_reasons.iterator();
        while (reason_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.external_reasons.deinit(self.allocator);
    }

    /// Run the flow analysis on the given handler function node.
    /// Returns the number of errors found.
    pub fn check(self: *FlowChecker, handler_func: NodeIndex) !u32 {
        self.scanImports();
        self.findHandlerParam(handler_func);
        self.walkStmt(handler_func);

        var error_count: u32 = 0;
        for (self.diagnostics.items) |diag| {
            if (diag.severity == .err) error_count += 1;
        }
        return error_count;
    }

    pub fn getDiagnostics(self: *const FlowChecker) []const Diagnostic {
        return self.diagnostics.items;
    }

    pub fn getProperties(self: *const FlowChecker) FlowProperties {
        return self.properties;
    }

    /// Populate external label maps from parsed ExternalLabel declarations.
    /// For qualified field names like "User.email", both the full name and the
    /// short form ("email") are registered. Labels are additive: if multiple
    /// declarations target the same field, their LabelSets are merged via OR.
    pub fn setExternalLabels(self: *FlowChecker, labels: []const ExternalLabel) void {
        for (labels) |ext| {
            self.registerExternalField(ext.field, ext.label, ext.reason);

            // Also register short form: everything after the last dot
            if (std.mem.lastIndexOfScalar(u8, ext.field, '.')) |dot_pos| {
                const short = ext.field[dot_pos + 1 ..];
                if (short.len > 0) {
                    self.registerExternalField(short, ext.label, ext.reason);
                }
            }
        }
    }

    fn registerExternalField(self: *FlowChecker, name: []const u8, label: DataLabel, reason: []const u8) void {
        const label_set = LabelSet.fromLabel(label);

        if (self.external_labels.getEntry(name)) |entry| {
            // Merge into existing entry - no new key allocation needed
            entry.value_ptr.* = LabelSet.merge(entry.value_ptr.*, label_set);
        } else {
            const duped_key = self.allocator.dupe(u8, name) catch return;
            self.external_labels.put(self.allocator, duped_key, label_set) catch {
                self.allocator.free(duped_key);
                return;
            };
        }

        if (self.external_reasons.get(name) == null) {
            const reason_key = self.allocator.dupe(u8, name) catch return;
            const reason_val = self.allocator.dupe(u8, reason) catch {
                self.allocator.free(reason_key);
                return;
            };
            self.external_reasons.put(self.allocator, reason_key, reason_val) catch {
                self.allocator.free(reason_key);
                self.allocator.free(reason_val);
            };
        }
    }

    pub fn formatDiagnostics(
        self: *const FlowChecker,
        source: []const u8,
        writer: anytype,
    ) !void {
        for (self.diagnostics.items) |diag| {
            const loc = self.ir_view.getLoc(diag.node) orelse continue;
            try writer.print("{s}: {s}\n", .{ diag.severity.label(), diag.message });
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

    fn scanImports(self: *FlowChecker) void {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            if (tag != .import_decl) continue;

            const import_decl = self.ir_view.getImportDecl(idx) orelse continue;
            const module_str = self.ir_view.getString(import_decl.module_idx) orelse continue;
            if (builtin_modules.fromSpecifier(module_str) == null) continue;

            var j: u8 = 0;
            while (j < import_decl.specifiers_count) : (j += 1) {
                const spec_idx = self.ir_view.getListIndex(import_decl.specifiers_start, j);
                const spec = self.ir_view.getImportSpec(spec_idx) orelse continue;
                const imported_name = self.resolveAtomName(spec.imported_atom) orelse continue;

                if (builtin_modules.findExport(module_str, imported_name)) |entry| {
                    if (!entry.func.return_labels.isEmpty()) {
                        self.module_fn_labels.put(
                            self.allocator,
                            spec.local_binding.slot,
                            entry.func.return_labels,
                        ) catch {};
                    }
                    self.module_fn_meta.put(
                        self.allocator,
                        spec.local_binding.slot,
                        .{
                            .module = entry.binding.name,
                            .func = entry.func.name,
                            .returns = entry.func.returns,
                        },
                    ) catch {};
                    if (std.mem.eql(u8, imported_name, "env")) {
                        self.env_fn_slot = spec.local_binding.slot;
                    }
                }
            }
        }
    }

    fn findHandlerParam(self: *FlowChecker, handler_func: NodeIndex) void {
        const func = self.ir_view.getFunction(handler_func) orelse return;
        if (func.params_count == 0) return;

        // First parameter is the request object. The parser wraps every
        // function parameter in a `.pattern_element`, even for the simple
        // `function handler(req)` case; drill through to the binding.
        const param_idx = self.ir_view.getListIndex(func.params_start, 0);
        const binding = self.paramBinding(param_idx) orelse return;
        const key = packBindingKey(binding.scope_id, binding.slot);
        self.req_binding_key = key;
        // Request parameter carries user_input label
        self.binding_labels.put(self.allocator, key, .{ .user_input = true }) catch {};
    }

    /// Unwrap a function parameter node to its binding slot. Handles both
    /// the bare `.identifier` shape and the `.pattern_element` wrapper
    /// the parser emits for every parameter. Destructuring patterns
    /// (object_pattern / array_pattern) return null; those don't carry a
    /// single binding slot and callers treat the request as unnamed.
    fn paramBinding(self: *const FlowChecker, param_idx: NodeIndex) ?ir.BindingRef {
        const tag = self.ir_view.getTag(param_idx) orelse return null;
        if (tag == .identifier) {
            return self.ir_view.getBinding(param_idx);
        }
        if (tag == .pattern_element) {
            const pe = self.ir_view.getPatternElem(param_idx) orelse return null;
            return pe.binding;
        }
        return null;
    }

    fn walkStmt(self: *FlowChecker, node: NodeIndex) void {
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

                // Both `working_constraints` and `working_io_calls` are
                // path-scoped: entering a branch extends them, leaving
                // restores them. Without the io_calls restore, calls made
                // in a sibling branch would appear in the witness for this
                // one, so the synthesised stub sequence would no longer
                // drive the handler down the path that actually witnesses
                // the sink.
                const saved_io = self.working_io_calls.items.len;
                const then_pushed = self.pushConditionConstraints(if_s.condition, false);
                self.walkStmt(if_s.then_branch);
                self.popConstraints(then_pushed);
                self.working_io_calls.shrinkRetainingCapacity(saved_io);

                if (if_s.else_branch != null_node) {
                    const else_pushed = self.pushConditionConstraints(if_s.condition, true);
                    self.walkStmt(if_s.else_branch);
                    self.popConstraints(else_pushed);
                    self.working_io_calls.shrinkRetainingCapacity(saved_io);
                }
            },

            .var_decl => {
                const vd = self.ir_view.getVarDecl(node) orelse return;
                if (vd.init != null_node) {
                    const labels = self.inferLabels(vd.init);
                    if (!labels.isEmpty()) {
                        const key = packBindingKey(vd.binding.scope_id, vd.binding.slot);
                        self.binding_labels.put(self.allocator, key, labels) catch {};
                    }
                    self.trackModuleCallInit(vd);
                    // Track result bindings from validation calls for .value label narrowing
                    self.trackResultBinding(vd);
                }
            },

            .assignment => {
                const asgn = self.ir_view.getAssignment(node) orelse return;
                const labels = self.inferLabels(asgn.value);
                // Track label updates for simple identifier assignments
                const target_tag = self.ir_view.getTag(asgn.target) orelse return;
                if (target_tag == .identifier) {
                    const binding = self.ir_view.getBinding(asgn.target) orelse return;
                    const key = packBindingKey(binding.scope_id, binding.slot);
                    if (asgn.op) |_| {
                        // Compound assignment (+=, etc): merge with existing
                        const existing = self.binding_labels.get(key) orelse LabelSet.empty;
                        self.binding_labels.put(self.allocator, key, LabelSet.merge(existing, labels)) catch {};
                    } else {
                        // Simple assignment: replace
                        self.binding_labels.put(self.allocator, key, labels) catch {};
                    }
                }
            },

            .return_stmt => {
                // Check if returning data via Response helpers
                if (self.ir_view.getOptValue(node)) |ret_val| {
                    self.checkResponseSink(ret_val);
                }
            },

            .expr_stmt => {
                if (self.ir_view.getOptValue(node)) |expr| {
                    self.checkExprSinks(expr);
                }
            },

            .for_of_stmt, .for_in_stmt => {
                const fi = self.ir_view.getForIter(node) orelse return;
                // Iteration variable inherits labels from iterable
                const iterable_labels = self.inferLabels(fi.iterable);
                if (!iterable_labels.isEmpty()) {
                    const key = packBindingKey(fi.binding.scope_id, fi.binding.slot);
                    self.binding_labels.put(self.allocator, key, iterable_labels) catch {};
                }
                self.walkStmt(fi.body);
            },

            .switch_stmt => {
                const sw = self.ir_view.getSwitchStmt(node) orelse return;
                for (0..sw.cases_count) |i| {
                    const case_idx = self.ir_view.getListIndex(sw.cases_start, @intCast(i));
                    const cc = self.ir_view.getCaseClause(case_idx) orelse continue;
                    for (0..cc.body_count) |j| {
                        const body_stmt = self.ir_view.getListIndex(cc.body_start, @intCast(j));
                        self.walkStmt(body_stmt);
                    }
                }
            },

            .function_decl, .function_expr, .arrow_function => {
                const func = self.ir_view.getFunction(node) orelse return;
                self.walkStmt(func.body);
            },

            .export_default => {
                if (self.ir_view.getOptValue(node)) |val| {
                    self.walkStmt(val);
                }
            },

            .call, .method_call => {
                self.checkExprSinks(node);
            },

            .assert_stmt => {
                const assert = self.ir_view.getAssertStmt(node) orelse return;
                if (assert.error_expr != null_node) {
                    self.checkExprSinks(assert.error_expr);
                }
            },

            else => {},
        }
    }

    fn inferLabels(self: *FlowChecker, node: NodeIndex) LabelSet {
        if (node == null_node) return LabelSet.empty;
        const tag = self.ir_view.getTag(node) orelse return LabelSet.empty;

        switch (tag) {
            .lit_int, .lit_float, .lit_string, .lit_bool, .lit_undefined => return LabelSet.empty,

            .identifier => {
                const binding = self.ir_view.getBinding(node) orelse return LabelSet.empty;
                const key = packBindingKey(binding.scope_id, binding.slot);
                return self.binding_labels.get(key) orelse LabelSet.empty;
            },

            .call => {
                const call_data = self.ir_view.getCall(node) orelse return LabelSet.empty;
                return self.inferCallLabels(call_data);
            },

            .binary_op => {
                const bin = self.ir_view.getBinary(node) orelse return LabelSet.empty;
                return LabelSet.merge(self.inferLabels(bin.left), self.inferLabels(bin.right));
            },

            .ternary => {
                const t = self.ir_view.getTernary(node) orelse return LabelSet.empty;
                return LabelSet.merge(self.inferLabels(t.then_branch), self.inferLabels(t.else_branch));
            },

            .template_literal => {
                const tmpl = self.ir_view.getTemplate(node) orelse return LabelSet.empty;
                var labels = LabelSet.empty;
                for (0..tmpl.parts_count) |i| {
                    const part_idx = self.ir_view.getListIndex(tmpl.parts_start, @intCast(i));
                    const part_tag = self.ir_view.getTag(part_idx) orelse continue;
                    if (part_tag == .template_part_expr) {
                        if (self.ir_view.getOptValue(part_idx)) |expr| {
                            labels = LabelSet.merge(labels, self.inferLabels(expr));
                        }
                    }
                }
                return labels;
            },

            .member_access, .optional_chain => {
                const member = self.ir_view.getMember(node) orelse return LabelSet.empty;
                var labels = self.inferLabels(member.object);

                // req.headers.authorization carries credential label
                if (self.isReqProperty(member.object, "headers")) {
                    const prop_name = self.resolveAtomName(member.property) orelse return labels;
                    if (std.mem.eql(u8, prop_name, "authorization")) {
                        return LabelSet.merge(labels, .{ .credential = true });
                    }
                }

                // result.value inherits labels from the producing validation call
                const prop_name = self.resolveAtomName(member.property) orelse return labels;
                if (std.mem.eql(u8, prop_name, "value")) {
                    const obj_tag = self.ir_view.getTag(member.object) orelse return labels;
                    if (obj_tag == .identifier) {
                        const binding = self.ir_view.getBinding(member.object) orelse return labels;
                        const key = packBindingKey(binding.scope_id, binding.slot);
                        if (self.result_binding_labels.get(key)) |result_labels| {
                            labels = LabelSet.merge(labels, result_labels);
                        }
                    }
                }

                // External labels: merge if the property name matches a declared field
                if (self.external_labels.get(prop_name)) |ext_labels| {
                    labels = LabelSet.merge(labels, ext_labels);
                }

                return labels;
            },

            .computed_access => {
                const member = self.ir_view.getMember(node) orelse return LabelSet.empty;
                return self.inferLabels(member.object);
            },

            .object_literal => {
                const obj = self.ir_view.getObject(node) orelse return LabelSet.empty;
                var labels = LabelSet.empty;
                var i: u16 = 0;
                while (i < obj.properties_count) : (i += 1) {
                    const prop_idx = self.ir_view.getListIndex(obj.properties_start, i);
                    const prop_tag = self.ir_view.getTag(prop_idx) orelse continue;
                    if (prop_tag == .object_property) {
                        const prop = self.ir_view.getProperty(prop_idx) orelse continue;
                        labels = LabelSet.merge(labels, self.inferLabels(prop.value));
                    } else if (prop_tag == .object_spread) {
                        if (self.ir_view.getOptValue(prop_idx)) |val| {
                            labels = LabelSet.merge(labels, self.inferLabels(val));
                        }
                    }
                }
                return labels;
            },

            .array_literal => {
                const arr = self.ir_view.getArray(node) orelse return LabelSet.empty;
                var labels = LabelSet.empty;
                var i: u16 = 0;
                while (i < arr.elements_count) : (i += 1) {
                    const elem_idx = self.ir_view.getListIndex(arr.elements_start, i);
                    labels = LabelSet.merge(labels, self.inferLabels(elem_idx));
                }
                return labels;
            },

            .spread => {
                if (self.ir_view.getOptValue(node)) |operand| {
                    return self.inferLabels(operand);
                }
                return LabelSet.empty;
            },

            .match_expr => {
                const match_data = self.ir_view.getMatchExpr(node) orelse return LabelSet.empty;
                var labels = LabelSet.empty;
                var i: u8 = 0;
                while (i < match_data.arms_count) : (i += 1) {
                    const arm_idx = self.ir_view.getListIndex(match_data.arms_start, i);
                    const arm = self.ir_view.getMatchArm(arm_idx) orelse continue;
                    labels = LabelSet.merge(labels, self.inferLabels(arm.body));
                }
                return labels;
            },

            .assignment => {
                const asgn = self.ir_view.getAssignment(node) orelse return LabelSet.empty;
                return self.inferLabels(asgn.value);
            },

            .unary_op => {
                if (self.ir_view.getOptValue(node)) |operand| {
                    return self.inferLabels(operand);
                }
                return LabelSet.empty;
            },

            .method_call => {
                const call_data = self.ir_view.getCall(node) orelse return LabelSet.empty;
                if (call_data.args_count > 0) {
                    const first_arg = self.ir_view.getListIndex(call_data.args_start, 0);
                    return self.inferLabels(first_arg);
                }
                return LabelSet.empty;
            },

            else => return LabelSet.empty,
        }
    }

    /// Infer labels for a function call expression.
    fn inferCallLabels(self: *FlowChecker, call_data: Node.CallExpr) LabelSet {
        const callee_tag = self.ir_view.getTag(call_data.callee) orelse return LabelSet.empty;

        if (callee_tag == .identifier) {
            const binding = self.ir_view.getBinding(call_data.callee) orelse return LabelSet.empty;

            if (self.module_fn_labels.get(binding.slot)) |base_labels| {
                if (self.env_fn_slot != null and binding.slot == self.env_fn_slot.? and call_data.args_count > 0) {
                    const arg = self.ir_view.getListIndex(call_data.args_start, 0);
                    return self.refineEnvLabels(arg, base_labels);
                }
                return base_labels;
            }

            // User-defined function call: unknown labels (conservative)
            return LabelSet.empty;
        }

        return LabelSet.empty;
    }

    fn checkResponseSink(self: *FlowChecker, ret_val: NodeIndex) void {
        const tag = self.ir_view.getTag(ret_val) orelse return;

        // Response.json(data), Response.text(data), Response.html(data), Response.redirect(url)
        if (tag == .method_call or tag == .call) {
            const call_data = self.ir_view.getCall(ret_val) orelse return;

            if (self.isResponseHelper(call_data.callee)) {
                if (call_data.args_count > 0) {
                    const data_arg = self.ir_view.getListIndex(call_data.args_start, 0);
                    const labels = self.inferLabels(data_arg);
                    self.checkSinkLabels(labels, ret_val, .response);

                    // XSS check: Response.html with unvalidated user input
                    if (labels.has(.user_input) and !labels.has(.validated)) {
                        if (self.isGlobalMethodCall(call_data.callee, "Response", &.{"html"})) {
                            self.addDiagnostic(.{
                                .severity = .warning,
                                .kind = .unvalidated_input_in_egress,
                                .node = ret_val,
                                .message = "unvalidated user input in Response.html (potential XSS)",
                                .help = "use JSX with renderToString() for auto-escaping, or pass input through validateObject() first",
                            });
                            self.properties.injection_safe = false;
                        }
                    }
                }
            }
        }
    }

    fn checkExprSinks(self: *FlowChecker, node: NodeIndex) void {
        const tag = self.ir_view.getTag(node) orelse return;
        if (tag != .call and tag != .method_call) return;

        const call_data = self.ir_view.getCall(node) orelse return;

        if (self.isConsoleCall(call_data.callee)) {
            for (0..call_data.args_count) |i| {
                const arg = self.ir_view.getListIndex(call_data.args_start, @intCast(i));
                const labels = self.inferLabels(arg);
                self.checkSinkLabels(labels, node, .console);
            }
            return;
        }

        if (tag == .call and self.isFetchSyncCall(call_data.callee)) {
            if (call_data.args_count > 0) {
                const url_arg = self.ir_view.getListIndex(call_data.args_start, 0);
                self.checkSinkLabels(self.inferLabels(url_arg), node, .egress_url);
            }
            if (call_data.args_count > 1) {
                const opts_arg = self.ir_view.getListIndex(call_data.args_start, 1);
                self.checkSinkLabels(self.inferObjectBodyLabels(opts_arg), node, .egress_body);
            }
        }
    }

    const SinkKind = enum { response, console, egress_url, egress_body };

    fn checkSinkLabels(self: *FlowChecker, labels: LabelSet, node: NodeIndex, sink: SinkKind) void {
        if (labels.isEmpty()) return;

        switch (sink) {
            .response => {
                if (labels.has(.secret)) {
                    self.addDiagnostic(.{
                        .severity = .err,
                        .kind = .secret_in_response,
                        .node = node,
                        .message = self.messageWithReason("secret data flows into response body", .secret),
                        .help = "env vars with sensitive names (SECRET, PASSWORD, KEY, TOKEN) must not appear in responses",
                    });
                    self.properties.no_secret_leakage = false;
                }
                if (labels.has(.credential)) {
                    self.addDiagnostic(.{
                        .severity = .warning,
                        .kind = .credential_in_response,
                        .node = node,
                        .message = self.messageWithReason("credential data flows into response body", .credential),
                        .help = "auth tokens and JWT payloads should not be returned to clients",
                    });
                    self.properties.no_credential_leakage = false;
                }
            },
            .console => {
                if (labels.has(.secret)) {
                    self.addDiagnostic(.{
                        .severity = .err,
                        .kind = .secret_in_log,
                        .node = node,
                        .message = self.messageWithReason("secret data flows into console output", .secret),
                        .help = "env vars with sensitive names must not be logged",
                    });
                    self.properties.no_secret_leakage = false;
                }
                if (labels.has(.credential)) {
                    self.addDiagnostic(.{
                        .severity = .err,
                        .kind = .credential_in_log,
                        .node = node,
                        .message = self.messageWithReason("credential data flows into console output", .credential),
                        .help = "auth tokens and JWTs must not be logged",
                    });
                    self.properties.no_credential_leakage = false;
                }
            },
            .egress_url => {
                if (labels.has(.secret)) {
                    self.addDiagnostic(.{
                        .severity = .err,
                        .kind = .secret_in_egress_url,
                        .node = node,
                        .message = self.messageWithReason("secret data flows into fetchSync URL", .secret),
                        .help = "secrets in URLs are logged by proxies and CDNs; pass secrets in headers or body instead",
                    });
                    self.properties.no_secret_leakage = false;
                }
                if (labels.has(.credential)) {
                    self.addDiagnostic(.{
                        .severity = .warning,
                        .kind = .credential_in_egress_url,
                        .node = node,
                        .message = self.messageWithReason("credential data flows into fetchSync URL", .credential),
                        .help = "pass auth tokens in headers, not URLs",
                    });
                    self.properties.no_credential_leakage = false;
                }
            },
            .egress_body => {
                if (labels.has(.secret)) {
                    self.addDiagnostic(.{
                        .severity = .err,
                        .kind = .secret_in_egress_body,
                        .node = node,
                        .message = self.messageWithReason("secret data flows into fetchSync request body", .secret),
                        .help = "do not send env secrets to external services",
                    });
                    self.properties.no_secret_leakage = false;
                }
                // Check for unvalidated user input in egress
                if (labels.has(.user_input) and !labels.has(.validated)) {
                    self.addDiagnostic(.{
                        .severity = .warning,
                        .kind = .unvalidated_input_in_egress,
                        .node = node,
                        .message = "unvalidated user input flows into fetchSync body",
                        .help = "pass user input through validateJson() or validateObject() before sending to external services",
                    });
                    self.properties.input_validated = false;
                    self.properties.injection_safe = false;
                }
                // PII containment: user input going to external hosts
                if (labels.has(.user_input)) {
                    self.properties.pii_contained = false;
                }
            },
        }
    }

    /// Refine env() labels based on the literal env var name.
    /// Names containing SECRET, PASSWORD, KEY, TOKEN, or PRIVATE keep {secret}.
    /// Others get downgraded to {config}.
    fn refineEnvLabels(self: *const FlowChecker, arg_node: NodeIndex, base_labels: LabelSet) LabelSet {
        const arg_tag = self.ir_view.getTag(arg_node) orelse return base_labels;
        if (arg_tag != .lit_string) return base_labels; // non-literal: keep conservative

        const name = self.ir_view.getString(self.ir_view.getStringIdx(arg_node) orelse return base_labels) orelse return base_labels;

        if (isSensitiveEnvName(name)) {
            return base_labels; // keep {secret}
        }

        // Non-sensitive name: downgrade to {config}
        return .{ .config = true };
    }

    fn isSensitiveEnvName(name: []const u8) bool {
        const patterns = [_][]const u8{
            "SECRET", "PASSWORD", "PASSWD", "KEY", "TOKEN",
            "PRIVATE", "CREDENTIAL", "AUTH",
        };
        for (patterns) |pattern| {
            if (indexOfIgnoreCase(name, pattern)) return true;
        }
        return false;
    }

    fn indexOfIgnoreCase(haystack: []const u8, needle: []const u8) bool {
        if (needle.len > haystack.len) return false;
        const end = haystack.len - needle.len + 1;
        for (0..end) |i| {
            var match = true;
            for (needle, 0..) |nc, j| {
                if (std.ascii.toUpper(haystack[i + j]) != nc) {
                    match = false;
                    break;
                }
            }
            if (match) return true;
        }
        return false;
    }

    /// Check if callee is `Global.method()` where Global and method match the given names.
    fn isGlobalMethodCall(self: *const FlowChecker, callee: NodeIndex, object_name: []const u8, method_names: []const []const u8) bool {
        const tag = self.ir_view.getTag(callee) orelse return false;
        if (tag != .member_access) return false;
        const member = self.ir_view.getMember(callee) orelse return false;

        const obj_tag = self.ir_view.getTag(member.object) orelse return false;
        if (obj_tag != .identifier) return false;
        const binding = self.ir_view.getBinding(member.object) orelse return false;
        if (binding.kind != .undeclared_global) return false;
        const obj_name = self.resolveAtomName(binding.slot) orelse return false;
        if (!std.mem.eql(u8, obj_name, object_name)) return false;

        const method_name = self.resolveAtomName(member.property) orelse return false;
        for (method_names) |name| {
            if (std.mem.eql(u8, method_name, name)) return true;
        }
        return false;
    }

    fn isResponseHelper(self: *const FlowChecker, callee: NodeIndex) bool {
        return self.isGlobalMethodCall(callee, "Response", &.{ "json", "text", "html", "redirect" });
    }

    fn isConsoleCall(self: *const FlowChecker, callee: NodeIndex) bool {
        return self.isGlobalMethodCall(callee, "console", &.{ "log", "warn", "error" });
    }

    fn isFetchSyncCall(self: *const FlowChecker, callee: NodeIndex) bool {
        const tag = self.ir_view.getTag(callee) orelse return false;
        if (tag != .identifier) return false;
        const binding = self.ir_view.getBinding(callee) orelse return false;
        if (binding.kind != .undeclared_global) return false;
        const name = self.resolveAtomName(binding.slot) orelse return false;
        return std.mem.eql(u8, name, "fetchSync");
    }

    fn isReqProperty(self: *const FlowChecker, node: NodeIndex, expected_prop: []const u8) bool {
        const tag = self.ir_view.getTag(node) orelse return false;
        if (tag != .member_access) return false;
        const member = self.ir_view.getMember(node) orelse return false;

        // Check if object is the request binding
        const obj_tag = self.ir_view.getTag(member.object) orelse return false;
        if (obj_tag != .identifier) return false;
        const binding = self.ir_view.getBinding(member.object) orelse return false;
        const key = packBindingKey(binding.scope_id, binding.slot);
        if (self.req_binding_key == null or key != self.req_binding_key.?) return false;

        const prop_name = self.resolveAtomName(member.property) orelse return false;
        return std.mem.eql(u8, prop_name, expected_prop);
    }

    // -------------------------------------------------------------------
    // Helper: extract body labels from options object { body: ... }
    // -------------------------------------------------------------------

    fn inferObjectBodyLabels(self: *FlowChecker, node: NodeIndex) LabelSet {
        const tag = self.ir_view.getTag(node) orelse return LabelSet.empty;
        if (tag == .object_literal) {
            const obj = self.ir_view.getObject(node) orelse return LabelSet.empty;
            var i: u16 = 0;
            while (i < obj.properties_count) : (i += 1) {
                const prop_idx = self.ir_view.getListIndex(obj.properties_start, i);
                const prop_tag = self.ir_view.getTag(prop_idx) orelse continue;
                if (prop_tag == .object_property) {
                    const prop = self.ir_view.getProperty(prop_idx) orelse continue;
                    const key_name = self.getPropertyKeyName(prop.key) orelse continue;
                    if (std.mem.eql(u8, key_name, "body")) {
                        return self.inferLabels(prop.value);
                    }
                }
            }
        }
        // If not an object literal, infer labels of the whole thing
        return self.inferLabels(node);
    }

    /// Get the name of an object property key (identifier or string literal).
    fn getPropertyKeyName(self: *const FlowChecker, key_idx: NodeIndex) ?[]const u8 {
        const tag = self.ir_view.getTag(key_idx) orelse return null;
        if (tag == .identifier) {
            const binding = self.ir_view.getBinding(key_idx) orelse return null;
            return self.resolveAtomName(binding.slot);
        } else if (tag == .lit_string) {
            const str_idx = self.ir_view.getStringIdx(key_idx) orelse return null;
            return self.ir_view.getString(str_idx);
        }
        return null;
    }

    /// Track result bindings from validation function calls.
    /// When `const result = validateJson(...)` is seen, records the function's
    /// return_labels so that `result.value` inherits them during label inference.
    fn trackResultBinding(self: *FlowChecker, vd: ir.Node.VarDecl) void {
        const init_tag = self.ir_view.getTag(vd.init) orelse return;
        if (init_tag != .call) return;

        const call_data = self.ir_view.getCall(vd.init) orelse return;
        const callee_tag = self.ir_view.getTag(call_data.callee) orelse return;
        if (callee_tag != .identifier) return;

        const callee_binding = self.ir_view.getBinding(call_data.callee) orelse return;
        const return_labels = self.module_fn_labels.get(callee_binding.slot) orelse return;

        // Only track if the function returns labels worth propagating (e.g., validated)
        if (return_labels.has(.validated)) {
            const key = packBindingKey(vd.binding.scope_id, vd.binding.slot);
            self.result_binding_labels.put(self.allocator, key, return_labels) catch {};
        }
    }

    /// Find the first external reason matching the given label.
    /// Iterates all external_reasons entries whose corresponding LabelSet
    /// includes the target label. Returns null if no external label contributed.
    fn findExternalReason(self: *const FlowChecker, label: DataLabel) ?[]const u8 {
        var iter = self.external_reasons.iterator();
        while (iter.next()) |entry| {
            if (self.external_labels.get(entry.key_ptr.*)) |ext_ls| {
                if (ext_ls.has(label)) return entry.value_ptr.*;
            }
        }
        return null;
    }

    /// Return a diagnostic message, appending the external reason if one exists.
    /// When no external reason applies, the original literal is returned as-is
    /// (no allocation). When a reason exists, a new string is allocated and
    /// tracked in allocated_messages for cleanup.
    fn messageWithReason(self: *FlowChecker, base: []const u8, label: DataLabel) []const u8 {
        const reason = self.findExternalReason(label) orelse return base;
        const formatted = std.fmt.allocPrint(self.allocator, "{s} ({s})", .{ base, reason }) catch return base;
        self.allocated_messages.append(self.allocator, formatted) catch {
            self.allocator.free(formatted);
            return base;
        };
        return formatted;
    }

    fn addDiagnostic(self: *FlowChecker, diag: Diagnostic) void {
        var owned = diag;
        // Only diagnostics the counterexample surface can actually consume
        // carry a witness snapshot. Skipping the dupe for everything else
        // avoids allocator churn when the handler raises many non-witness
        // diagnostics (unused-variable warnings, XSS warnings, etc.).
        if (propertyTagForKind(diag.kind) != null and
            (self.working_constraints.items.len > 0 or self.working_io_calls.items.len > 0))
        {
            const constraints = self.allocator.dupe(
                counterexample.WitnessConstraint,
                self.working_constraints.items,
            ) catch &[_]counterexample.WitnessConstraint{};
            const io_calls = self.allocator.dupe(
                counterexample.TrackedIoCall,
                self.working_io_calls.items,
            ) catch &[_]counterexample.TrackedIoCall{};
            owned.witness = .{
                .path_constraints = constraints,
                .io_calls = io_calls,
            };
        }
        self.diagnostics.append(self.allocator, owned) catch {
            if (owned.witness) |w| {
                if (w.path_constraints.len > 0) self.allocator.free(w.path_constraints);
                if (w.io_calls.len > 0) self.allocator.free(w.io_calls);
            }
        };
    }

    /// Push zero or more constraints derived from `cond` onto the working
    /// stack. AND chains contribute one constraint per clause; every other
    /// shape contributes at most one. Returns the number pushed so the
    /// caller can pop exactly that many when the branch body is done.
    ///
    /// Under `want_negation` (the else-branch path), we distribute negation
    /// into each AND clause the same way path_generator does. That is
    /// unsound symbolically - `!(a && b)` is `!a || !b`, not `!a && !b` -
    /// but witness synthesis only needs ONE concrete request that reaches
    /// the sink; over-constraining at worst adds irrelevant stubs that the
    /// runtime replay accepts.
    fn pushConditionConstraints(self: *FlowChecker, cond: NodeIndex, want_negation: bool) usize {
        const tag = self.ir_view.getTag(cond) orelse return 0;
        if (tag == .binary_op) {
            const bin = self.ir_view.getBinary(cond) orelse return 0;
            if (bin.op == .and_op) {
                const left = self.pushConditionConstraints(bin.left, want_negation);
                const right = self.pushConditionConstraints(bin.right, want_negation);
                return left + right;
            }
        }

        const raw = self.extractCondConstraint(cond) orelse return 0;
        const final = if (want_negation) counterexample.negate(raw) orelse return 0 else raw;
        self.working_constraints.append(self.allocator, final) catch return 0;
        return 1;
    }

    fn popConstraints(self: *FlowChecker, count: usize) void {
        var remaining = count;
        while (remaining > 0) : (remaining -= 1) _ = self.working_constraints.pop();
    }

    /// Record a virtual-module call on the current path, if `vd.init` is a
    /// direct call to an imported module function. Also remembers the
    /// originating module-function slot on the declared binding so that
    /// later `if (x)` patterns can emit stub_truthy.
    fn trackModuleCallInit(self: *FlowChecker, vd: Node.VarDecl) void {
        const init_tag = self.ir_view.getTag(vd.init) orelse return;
        if (init_tag != .call) return;
        const call = self.ir_view.getCall(vd.init) orelse return;
        const callee_tag = self.ir_view.getTag(call.callee) orelse return;
        if (callee_tag != .identifier) return;
        const binding = self.ir_view.getBinding(call.callee) orelse return;
        const meta = self.module_fn_meta.get(binding.slot) orelse return;
        self.working_io_calls.append(self.allocator, .{
            .module = meta.module,
            .func = meta.func,
            .returns = meta.returns,
        }) catch {};
        const key = packBindingKey(vd.binding.scope_id, vd.binding.slot);
        self.binding_origin.put(self.allocator, key, binding.slot) catch {};
    }

    /// Extract a single WitnessConstraint from a condition node. AND
    /// chains are handled one level up in `pushConditionConstraints`;
    /// supported single-node shapes are documented by the switch arms.
    fn extractCondConstraint(self: *FlowChecker, cond: NodeIndex) ?counterexample.WitnessConstraint {
        const tag = self.ir_view.getTag(cond) orelse return null;
        switch (tag) {
            .identifier => {
                const binding = self.ir_view.getBinding(cond) orelse return null;
                const key = packBindingKey(binding.scope_id, binding.slot);
                const origin_slot = self.binding_origin.get(key) orelse return null;
                const meta = self.module_fn_meta.get(origin_slot) orelse return null;
                return .{ .stub_truthy = .{
                    .module = meta.module,
                    .func = meta.func,
                    .returns = meta.returns,
                } };
            },
            .unary_op => {
                const unary = self.ir_view.getUnary(cond) orelse return null;
                if (unary.op != .not) return null;
                const inner = self.extractCondConstraint(unary.operand) orelse return null;
                return counterexample.negate(inner);
            },
            .binary_op => {
                const bin = self.ir_view.getBinary(cond) orelse return null;
                if (bin.op != .strict_eq and bin.op != .strict_neq) return null;

                const raw = self.extractLiteralReqComparison(bin.left, bin.right) orelse
                    self.extractLiteralReqComparison(bin.right, bin.left) orelse
                    return null;
                return if (bin.op == .strict_eq) raw else counterexample.negate(raw);
            },
            .member_access => {
                return self.extractResultOkConstraint(cond);
            },
            else => return null,
        }
    }

    /// Recognise `req.method === "POST"` / `req.url === "/path"` shapes
    /// (either direction). Handles only direct property access on the
    /// request binding; `const method = req.method` indirection would
    /// need a second binding_origin track and is a follow-up.
    fn extractLiteralReqComparison(
        self: *FlowChecker,
        prop_node: NodeIndex,
        lit_node: NodeIndex,
    ) ?counterexample.WitnessConstraint {
        const lit_tag = self.ir_view.getTag(lit_node) orelse return null;
        if (lit_tag != .lit_string) return null;
        const str_idx = self.ir_view.getStringIdx(lit_node) orelse return null;
        const value = self.ir_view.getString(str_idx) orelse return null;

        if (self.isReqProperty(prop_node, "method")) {
            return .{ .req_method = value };
        }
        if (self.isReqProperty(prop_node, "url") or self.isReqProperty(prop_node, "path")) {
            return .{ .req_url = value };
        }
        return null;
    }

    /// Recognise `result.ok` where `result` is an identifier bound to a
    /// Result-returning module call (validateJson, jwtVerify, etc.).
    fn extractResultOkConstraint(
        self: *FlowChecker,
        member_node: NodeIndex,
    ) ?counterexample.WitnessConstraint {
        const member = self.ir_view.getMember(member_node) orelse return null;
        const prop_name = self.resolveAtomName(member.property) orelse return null;
        if (!std.mem.eql(u8, prop_name, "ok")) return null;

        const obj_tag = self.ir_view.getTag(member.object) orelse return null;
        if (obj_tag != .identifier) return null;
        const binding = self.ir_view.getBinding(member.object) orelse return null;
        const key = packBindingKey(binding.scope_id, binding.slot);
        const origin_slot = self.binding_origin.get(key) orelse return null;
        const meta = self.module_fn_meta.get(origin_slot) orelse return null;
        if (meta.returns != .result) return null;

        return .{ .result_ok = .{
            .module = meta.module,
            .func = meta.func,
            .returns = meta.returns,
        } };
    }

    fn resolveAtomName(self: *const FlowChecker, atom_idx: u16) ?[]const u8 {
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

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "FlowChecker captures witness constraints on secret-in-response" {
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  const secret = env("SECRET_KEY");
        \\  if (secret) {
        \\    return Response.json({ leaked: secret });
        \\  }
        \\  return Response.json({ ok: true });
        \\}
    ;

    var parser = @import("parser/parse.zig").Parser.init(allocator, source);
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);

    const handler_verifier = @import("handler_verifier.zig");
    const handler_fn = handler_verifier.findHandlerFunction(ir_view, root) orelse
        return error.HandlerNotFound;

    var checker = FlowChecker.init(allocator, ir_view, &atoms);
    defer checker.deinit();
    _ = try checker.check(handler_fn);

    // Expect exactly one secret-in-response diagnostic, carrying a
    // stub_truthy constraint on the env call and a tracked env I/O call.
    var found = false;
    for (checker.getDiagnostics()) |d| {
        if (d.kind != .secret_in_response) continue;
        found = true;
        try std.testing.expectEqual(
            counterexample.PropertyTag.no_secret_leakage,
            propertyTagForKind(d.kind).?,
        );
        const w = d.witness orelse return error.MissingWitness;
        try std.testing.expectEqual(@as(usize, 1), w.path_constraints.len);
        try std.testing.expect(w.path_constraints[0] == .stub_truthy);
        try std.testing.expectEqualStrings("env", w.path_constraints[0].stub_truthy.func);
        try std.testing.expect(w.io_calls.len >= 1);
        try std.testing.expectEqualStrings("env", w.io_calls[0].func);
    }
    try std.testing.expect(found);
}

test "FlowChecker does not leak sibling-branch I/O calls into the witness" {
    // If a module call happens inside the branch that does NOT reach the
    // sink, the fall-through sink's witness must not include it - otherwise
    // the synthesised stub sequence would drive the handler down the wrong
    // path. Regression test for the shrinkRetainingCapacity fix around
    // `.if_stmt` in walkStmt.
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\import { cacheGet } from "zigttp:cache";
        \\function handler(req) {
        \\  const secret = env("SECRET_KEY");
        \\  if (!secret) {
        \\    const cached = cacheGet("sibling");
        \\    return Response.json({ ok: cached });
        \\  }
        \\  return Response.json({ leaked: secret });
        \\}
    ;

    var parser = @import("parser/parse.zig").Parser.init(allocator, source);
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);

    const handler_verifier = @import("handler_verifier.zig");
    const handler_fn = handler_verifier.findHandlerFunction(ir_view, root) orelse
        return error.HandlerNotFound;

    var checker = FlowChecker.init(allocator, ir_view, &atoms);
    defer checker.deinit();
    _ = try checker.check(handler_fn);

    var found = false;
    for (checker.getDiagnostics()) |d| {
        if (d.kind != .secret_in_response) continue;
        found = true;
        const w = d.witness orelse return error.MissingWitness;
        for (w.io_calls) |call| {
            // `cacheGet` is only called on the sibling branch that returns
            // without leaking. It must not appear in this witness.
            try std.testing.expect(!std.mem.eql(u8, call.func, "cacheGet"));
        }
    }
    try std.testing.expect(found);
}

test "FlowChecker captures stub_truthy on if-else with negated condition" {
    // `if (!secret) { ok } else { leak }` - the else branch's effective
    // constraint is the double-negation of !secret, i.e. secret truthy.
    // Exercises the `.unary_op` arm of `extractCondConstraint`.
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  const secret = env("SECRET_KEY");
        \\  if (!secret) {
        \\    return Response.json({ ok: true });
        \\  } else {
        \\    return Response.json({ leaked: secret });
        \\  }
        \\}
    ;

    var parser = @import("parser/parse.zig").Parser.init(allocator, source);
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);

    const handler_verifier = @import("handler_verifier.zig");
    const handler_fn = handler_verifier.findHandlerFunction(ir_view, root) orelse
        return error.HandlerNotFound;

    var checker = FlowChecker.init(allocator, ir_view, &atoms);
    defer checker.deinit();
    _ = try checker.check(handler_fn);

    var found = false;
    for (checker.getDiagnostics()) |d| {
        if (d.kind != .secret_in_response) continue;
        found = true;
        const w = d.witness orelse return error.MissingWitness;
        try std.testing.expectEqual(@as(usize, 1), w.path_constraints.len);
        try std.testing.expect(w.path_constraints[0] == .stub_truthy);
        try std.testing.expectEqualStrings("env", w.path_constraints[0].stub_truthy.func);
    }
    try std.testing.expect(found);
}

test "FlowChecker captures req_method constraint from literal comparison" {
    // `if (req.method === "POST") { leak }` - the witness request must be
    // POST, not the default GET, so the handler reaches the sink.
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  const secret = env("SECRET_KEY");
        \\  if (req.method === "POST") {
        \\    return Response.json({ leaked: secret });
        \\  }
        \\  return Response.json({ ok: true });
        \\}
    ;

    var parser = @import("parser/parse.zig").Parser.init(allocator, source);
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    const handler_verifier = @import("handler_verifier.zig");
    const handler_fn = handler_verifier.findHandlerFunction(ir_view, root) orelse
        return error.HandlerNotFound;

    var checker = FlowChecker.init(allocator, ir_view, &atoms);
    defer checker.deinit();
    _ = try checker.check(handler_fn);

    var found = false;
    for (checker.getDiagnostics()) |d| {
        if (d.kind != .secret_in_response) continue;
        found = true;
        const w = d.witness orelse return error.MissingWitness;
        try std.testing.expectEqual(@as(usize, 1), w.path_constraints.len);
        try std.testing.expect(w.path_constraints[0] == .req_method);
        try std.testing.expectEqualStrings("POST", w.path_constraints[0].req_method);
    }
    try std.testing.expect(found);
}

test "FlowChecker captures AND chain as multiple constraints" {
    // `if (req.method === "POST" && secret) { leak }` produces TWO
    // constraints: the method literal and the env truthiness. The solver
    // turns this into a POST request whose env stub returns truthy.
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  const secret = env("SECRET_KEY");
        \\  if (req.method === "POST" && secret) {
        \\    return Response.json({ leaked: secret });
        \\  }
        \\  return Response.json({ ok: true });
        \\}
    ;

    var parser = @import("parser/parse.zig").Parser.init(allocator, source);
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    const handler_verifier = @import("handler_verifier.zig");
    const handler_fn = handler_verifier.findHandlerFunction(ir_view, root) orelse
        return error.HandlerNotFound;

    var checker = FlowChecker.init(allocator, ir_view, &atoms);
    defer checker.deinit();
    _ = try checker.check(handler_fn);

    var found = false;
    for (checker.getDiagnostics()) |d| {
        if (d.kind != .secret_in_response) continue;
        found = true;
        const w = d.witness orelse return error.MissingWitness;
        try std.testing.expectEqual(@as(usize, 2), w.path_constraints.len);

        var saw_method = false;
        var saw_truthy = false;
        for (w.path_constraints) |c| {
            switch (c) {
                .req_method => |m| {
                    try std.testing.expectEqualStrings("POST", m);
                    saw_method = true;
                },
                .stub_truthy => |info| {
                    try std.testing.expectEqualStrings("env", info.func);
                    saw_truthy = true;
                },
                else => return error.UnexpectedConstraintKind,
            }
        }
        try std.testing.expect(saw_method and saw_truthy);
    }
    try std.testing.expect(found);
}

test "FlowChecker captures result_ok constraint on validated path" {
    // `if (r.ok) { leak(env()) }` - the witness must make validateJson
    // return an ok Result so the sink is reachable.
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\import { validateJson } from "zigttp:validate";
        \\function handler(req) {
        \\  const secret = env("SECRET_KEY");
        \\  const r = validateJson(0, "{}");
        \\  if (r.ok) {
        \\    return Response.json({ leaked: secret });
        \\  }
        \\  return Response.json({ ok: true });
        \\}
    ;

    var parser = @import("parser/parse.zig").Parser.init(allocator, source);
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    const handler_verifier = @import("handler_verifier.zig");
    const handler_fn = handler_verifier.findHandlerFunction(ir_view, root) orelse
        return error.HandlerNotFound;

    var checker = FlowChecker.init(allocator, ir_view, &atoms);
    defer checker.deinit();
    _ = try checker.check(handler_fn);

    var found = false;
    for (checker.getDiagnostics()) |d| {
        if (d.kind != .secret_in_response) continue;
        found = true;
        const w = d.witness orelse return error.MissingWitness;
        var saw_result_ok = false;
        for (w.path_constraints) |c| {
            if (c == .result_ok) {
                try std.testing.expectEqualStrings("validateJson", c.result_ok.func);
                saw_result_ok = true;
            }
        }
        try std.testing.expect(saw_result_ok);
    }
    try std.testing.expect(found);
}

test "LabelSet operations in flow context" {
    // Verify secret + credential merge
    const secret = LabelSet{ .secret = true };
    const cred = LabelSet{ .credential = true };
    const merged = LabelSet.merge(secret, cred);
    try std.testing.expect(merged.has(.secret));
    try std.testing.expect(merged.has(.credential));
    try std.testing.expect(!merged.has(.user_input));
}

test "isSensitiveEnvName" {
    try std.testing.expect(FlowChecker.isSensitiveEnvName("DB_PASSWORD"));
    try std.testing.expect(FlowChecker.isSensitiveEnvName("API_KEY"));
    try std.testing.expect(FlowChecker.isSensitiveEnvName("JWT_SECRET"));
    try std.testing.expect(FlowChecker.isSensitiveEnvName("ACCESS_TOKEN"));
    try std.testing.expect(FlowChecker.isSensitiveEnvName("PRIVATE_KEY"));
    try std.testing.expect(FlowChecker.isSensitiveEnvName("db_password"));
    try std.testing.expect(FlowChecker.isSensitiveEnvName("Auth_Header"));

    try std.testing.expect(!FlowChecker.isSensitiveEnvName("APP_NAME"));
    try std.testing.expect(!FlowChecker.isSensitiveEnvName("PORT"));
    try std.testing.expect(!FlowChecker.isSensitiveEnvName("NODE_ENV"));
    try std.testing.expect(!FlowChecker.isSensitiveEnvName("LOG_LEVEL"));
    try std.testing.expect(!FlowChecker.isSensitiveEnvName("DATABASE_URL"));
}

test "FlowProperties defaults to all proven" {
    const props = FlowProperties{};
    try std.testing.expect(props.no_secret_leakage);
    try std.testing.expect(props.no_credential_leakage);
    try std.testing.expect(props.input_validated);
    try std.testing.expect(props.pii_contained);
}

test "parseDataLabel maps all known strings" {
    try std.testing.expectEqual(DataLabel.secret, parseDataLabel("secret").?);
    try std.testing.expectEqual(DataLabel.credential, parseDataLabel("credential").?);
    try std.testing.expectEqual(DataLabel.user_input, parseDataLabel("user_input").?);
    try std.testing.expectEqual(DataLabel.config, parseDataLabel("config").?);
    try std.testing.expectEqual(DataLabel.internal, parseDataLabel("internal").?);
    try std.testing.expectEqual(DataLabel.external, parseDataLabel("external").?);
    try std.testing.expectEqual(DataLabel.validated, parseDataLabel("validated").?);
    try std.testing.expect(parseDataLabel("unknown_label") == null);
    try std.testing.expect(parseDataLabel("") == null);
}

test "parseExternalLabels with valid JSON" {
    const json =
        \\{
        \\  "labels": [
        \\    { "field": "User.email", "label": "secret", "reason": "PII field" },
        \\    { "field": "token", "label": "credential", "reason": "auth token" }
        \\  ]
        \\}
    ;
    const labels = try parseExternalLabels(std.testing.allocator, json);
    defer {
        for (labels) |l| {
            std.testing.allocator.free(l.field);
            std.testing.allocator.free(l.reason);
        }
        std.testing.allocator.free(labels);
    }

    try std.testing.expectEqual(@as(usize, 2), labels.len);
    try std.testing.expectEqualStrings("User.email", labels[0].field);
    try std.testing.expectEqual(DataLabel.secret, labels[0].label);
    try std.testing.expectEqualStrings("PII field", labels[0].reason);
    try std.testing.expectEqualStrings("token", labels[1].field);
    try std.testing.expectEqual(DataLabel.credential, labels[1].label);
    try std.testing.expectEqualStrings("auth token", labels[1].reason);
}

test "parseExternalLabels with empty labels array" {
    const json =
        \\{ "labels": [] }
    ;
    const labels = try parseExternalLabels(std.testing.allocator, json);
    defer std.testing.allocator.free(labels);

    try std.testing.expectEqual(@as(usize, 0), labels.len);
}

test "setExternalLabels registers short form from qualified name" {
    // We cannot construct a full FlowChecker without a valid IrView,
    // so test the external label maps directly via a minimal instance.
    // The IrView is only used by check/walkStmt, not by setExternalLabels.
    var checker = FlowChecker{
        .allocator = std.testing.allocator,
        .ir_view = undefined,
        .atoms = null,
        .diagnostics = .empty,
        .binding_labels = .empty,
        .module_fn_labels = .empty,
        .module_fn_meta = .empty,
        .binding_origin = .empty,
        .result_binding_labels = .empty,
        .req_binding_key = null,
        .env_fn_slot = null,
        .working_constraints = .empty,
        .working_io_calls = .empty,
        .external_labels = .empty,
        .external_reasons = .empty,
        .allocated_messages = .empty,
        .properties = .{},
    };
    defer {
        // Clean up only the maps we populated (skip ir_view-dependent deinit)
        var li = checker.external_labels.iterator();
        while (li.next()) |entry| checker.allocator.free(entry.key_ptr.*);
        checker.external_labels.deinit(checker.allocator);
        var ri = checker.external_reasons.iterator();
        while (ri.next()) |entry| {
            checker.allocator.free(entry.key_ptr.*);
            checker.allocator.free(entry.value_ptr.*);
        }
        checker.external_reasons.deinit(checker.allocator);
    }

    const ext = [_]ExternalLabel{
        .{ .field = "User.email", .label = .secret, .reason = "PII field" },
        .{ .field = "plainField", .label = .credential, .reason = "token data" },
    };
    checker.setExternalLabels(&ext);

    // Qualified name registered
    const email_labels = checker.external_labels.get("User.email").?;
    try std.testing.expect(email_labels.has(.secret));

    // Short form also registered
    const short_labels = checker.external_labels.get("email").?;
    try std.testing.expect(short_labels.has(.secret));

    // Non-qualified name registered directly (no dot, no short form duplication)
    const plain_labels = checker.external_labels.get("plainField").?;
    try std.testing.expect(plain_labels.has(.credential));

    // Reasons registered
    try std.testing.expectEqualStrings("PII field", checker.external_reasons.get("User.email").?);
    try std.testing.expectEqualStrings("PII field", checker.external_reasons.get("email").?);
    try std.testing.expectEqualStrings("token data", checker.external_reasons.get("plainField").?);
}
