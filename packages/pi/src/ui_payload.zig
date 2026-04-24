const std = @import("std");
const json_writer = @import("providers/anthropic/json_writer.zig");

pub const DiagnosticItem = struct {
    code: []u8,
    severity: []u8,
    path: []u8,
    line: u32,
    column: u16,
    message: []u8,
    introduced_by_patch: ?bool = null,

    pub fn init(
        allocator: std.mem.Allocator,
        code: []const u8,
        severity: []const u8,
        path: []const u8,
        line: u32,
        column: u16,
        message: []const u8,
        introduced_by_patch: ?bool,
    ) !DiagnosticItem {
        return .{
            .code = try allocator.dupe(u8, code),
            .severity = try allocator.dupe(u8, severity),
            .path = try allocator.dupe(u8, path),
            .line = line,
            .column = column,
            .message = try allocator.dupe(u8, message),
            .introduced_by_patch = introduced_by_patch,
        };
    }

    pub fn clone(self: DiagnosticItem, allocator: std.mem.Allocator) !DiagnosticItem {
        return init(
            allocator,
            self.code,
            self.severity,
            self.path,
            self.line,
            self.column,
            self.message,
            self.introduced_by_patch,
        );
    }

    pub fn deinit(self: *DiagnosticItem, allocator: std.mem.Allocator) void {
        allocator.free(self.code);
        allocator.free(self.severity);
        allocator.free(self.path);
        allocator.free(self.message);
        self.* = .{
            .code = &.{},
            .severity = &.{},
            .path = &.{},
            .line = 0,
            .column = 0,
            .message = &.{},
            .introduced_by_patch = null,
        };
    }
};

pub const DiagnosticsPayload = struct {
    summary: []u8,
    items: []DiagnosticItem,

    pub fn clone(self: DiagnosticsPayload, allocator: std.mem.Allocator) !DiagnosticsPayload {
        const owned_items = try allocator.alloc(DiagnosticItem, self.items.len);
        errdefer allocator.free(owned_items);
        for (owned_items) |*item| item.* = undefined;
        var i: usize = 0;
        errdefer {
            while (i > 0) {
                i -= 1;
                owned_items[i].deinit(allocator);
            }
            allocator.free(owned_items);
        }
        while (i < self.items.len) : (i += 1) {
            owned_items[i] = try self.items[i].clone(allocator);
        }
        return .{
            .summary = try allocator.dupe(u8, self.summary),
            .items = owned_items,
        };
    }

    pub fn deinit(self: *DiagnosticsPayload, allocator: std.mem.Allocator) void {
        allocator.free(self.summary);
        for (self.items) |*item| item.deinit(allocator);
        allocator.free(self.items);
        self.* = .{ .summary = &.{}, .items = &.{} };
    }
};

pub const ProofStats = struct {
    total: u32,
    new: u32,
    preexisting: ?u32 = null,
};

pub const ProofCardPayload = struct {
    title: []u8,
    summary: []u8,
    stats: ProofStats,
    highlights: [][]u8,

    pub fn clone(self: ProofCardPayload, allocator: std.mem.Allocator) !ProofCardPayload {
        const highlights = try allocator.alloc([]u8, self.highlights.len);
        errdefer allocator.free(highlights);
        var i: usize = 0;
        errdefer {
            while (i > 0) {
                i -= 1;
                allocator.free(highlights[i]);
            }
            allocator.free(highlights);
        }
        while (i < self.highlights.len) : (i += 1) {
            highlights[i] = try allocator.dupe(u8, self.highlights[i]);
        }
        return .{
            .title = try allocator.dupe(u8, self.title),
            .summary = try allocator.dupe(u8, self.summary),
            .stats = self.stats,
            .highlights = highlights,
        };
    }

    pub fn deinit(self: *ProofCardPayload, allocator: std.mem.Allocator) void {
        allocator.free(self.title);
        allocator.free(self.summary);
        for (self.highlights) |highlight| allocator.free(highlight);
        allocator.free(self.highlights);
        self.* = .{
            .title = &.{},
            .summary = &.{},
            .stats = .{ .total = 0, .new = 0, .preexisting = null },
            .highlights = &.{},
        };
    }
};

pub const CommandOutcomePayload = struct {
    title: []u8,
    exit_code: ?u8,
    stdout: []u8,
    stderr: []u8,
    command: []u8,

    pub fn clone(self: CommandOutcomePayload, allocator: std.mem.Allocator) !CommandOutcomePayload {
        return .{
            .title = try allocator.dupe(u8, self.title),
            .exit_code = self.exit_code,
            .stdout = try allocator.dupe(u8, self.stdout),
            .stderr = try allocator.dupe(u8, self.stderr),
            .command = try allocator.dupe(u8, self.command),
        };
    }

    pub fn deinit(self: *CommandOutcomePayload, allocator: std.mem.Allocator) void {
        allocator.free(self.title);
        allocator.free(self.stdout);
        allocator.free(self.stderr);
        allocator.free(self.command);
        self.* = .{
            .title = &.{},
            .exit_code = null,
            .stdout = &.{},
            .stderr = &.{},
            .command = &.{},
        };
    }
};

pub const RepairCandidatePayload = struct {
    path: []u8,
    plan_id: []u8,
    intent_kind: []u8,
    proposed_content: []u8,
    verification_ok: bool,
    verification_summary: []u8,
    stats: ProofStats,

    pub fn init(
        allocator: std.mem.Allocator,
        path: []const u8,
        plan_id: []const u8,
        intent_kind: []const u8,
        proposed_content: []const u8,
        verification_ok: bool,
        verification_summary: []const u8,
        stats: ProofStats,
    ) !RepairCandidatePayload {
        return .{
            .path = try allocator.dupe(u8, path),
            .plan_id = try allocator.dupe(u8, plan_id),
            .intent_kind = try allocator.dupe(u8, intent_kind),
            .proposed_content = try allocator.dupe(u8, proposed_content),
            .verification_ok = verification_ok,
            .verification_summary = try allocator.dupe(u8, verification_summary),
            .stats = stats,
        };
    }

    pub fn clone(self: RepairCandidatePayload, allocator: std.mem.Allocator) !RepairCandidatePayload {
        return init(
            allocator,
            self.path,
            self.plan_id,
            self.intent_kind,
            self.proposed_content,
            self.verification_ok,
            self.verification_summary,
            self.stats,
        );
    }

    pub fn deinit(self: *RepairCandidatePayload, allocator: std.mem.Allocator) void {
        allocator.free(self.path);
        allocator.free(self.plan_id);
        allocator.free(self.intent_kind);
        allocator.free(self.proposed_content);
        allocator.free(self.verification_summary);
        self.* = .{
            .path = &.{},
            .plan_id = &.{},
            .intent_kind = &.{},
            .proposed_content = &.{},
            .verification_ok = false,
            .verification_summary = &.{},
            .stats = .{ .total = 0, .new = 0, .preexisting = null },
        };
    }
};

pub const PropertiesSnapshot = struct {
    pure: bool,
    read_only: bool,
    stateless: bool,
    retry_safe: bool,
    deterministic: bool,
    has_egress: bool,
    no_secret_leakage: bool,
    no_credential_leakage: bool,
    input_validated: bool,
    pii_contained: bool,
    idempotent: bool,
    max_io_depth: ?u32 = null,
    injection_safe: bool,
    state_isolated: bool,
    fault_covered: bool,
    result_safe: bool,
    optional_safe: bool,

    pub const ChangeKind = enum { promoted, demoted };

    pub const Change = struct {
        name: []const u8,
        kind: ChangeKind,
    };

    /// Walk the boolean fields of `after` against `before` and invoke `visitor`
    /// once per field whose value flipped. Missing `before` means no baseline;
    /// every after-field is treated as unchanged. Kept as an inline visitor so
    /// the compile-time field walk inlines at each call site and the caller
    /// decides how to render or collect the changes.
    pub inline fn forEachChange(
        before_opt: ?PropertiesSnapshot,
        after: PropertiesSnapshot,
        comptime Visitor: type,
        visitor: Visitor,
    ) !void {
        const before = before_opt orelse return;
        inline for (@typeInfo(PropertiesSnapshot).@"struct".fields) |field| {
            if (field.type != bool) continue;
            const after_val = @field(after, field.name);
            const before_val = @field(before, field.name);
            if (after_val and !before_val) {
                try visitor.visit(.{ .name = field.name, .kind = .promoted });
            } else if (!after_val and before_val) {
                try visitor.visit(.{ .name = field.name, .kind = .demoted });
            }
        }
    }
};

pub const ViolationDeltaItem = struct {
    stable_key: []u8,
    code: []u8,
    severity: []u8,
    message: []u8,
    line: u32,
    column: u16,
    introduced_by_patch: bool,

    pub fn init(
        allocator: std.mem.Allocator,
        stable_key: []const u8,
        code: []const u8,
        severity: []const u8,
        message: []const u8,
        line: u32,
        column: u16,
        introduced_by_patch: bool,
    ) !ViolationDeltaItem {
        return .{
            .stable_key = try allocator.dupe(u8, stable_key),
            .code = try allocator.dupe(u8, code),
            .severity = try allocator.dupe(u8, severity),
            .message = try allocator.dupe(u8, message),
            .line = line,
            .column = column,
            .introduced_by_patch = introduced_by_patch,
        };
    }

    pub fn clone(self: ViolationDeltaItem, allocator: std.mem.Allocator) !ViolationDeltaItem {
        return init(
            allocator,
            self.stable_key,
            self.code,
            self.severity,
            self.message,
            self.line,
            self.column,
            self.introduced_by_patch,
        );
    }

    pub fn deinit(self: *ViolationDeltaItem, allocator: std.mem.Allocator) void {
        allocator.free(self.stable_key);
        allocator.free(self.code);
        allocator.free(self.severity);
        allocator.free(self.message);
        self.* = .{
            .stable_key = &.{},
            .code = &.{},
            .severity = &.{},
            .message = &.{},
            .line = 0,
            .column = 0,
            .introduced_by_patch = false,
        };
    }
};

pub const DiffHunk = struct {
    old_start: u32,
    old_count: u32,
    new_start: u32,
    new_count: u32,
};

pub const ProveSummary = struct {
    classification: []u8,
    proof_level: []u8,
    recommendation: []u8,
    counterexample: ?[]u8 = null,
    laws_used: [][]u8,

    pub fn clone(self: ProveSummary, allocator: std.mem.Allocator) !ProveSummary {
        const laws_used = try cloneStringSlice(allocator, self.laws_used);
        errdefer freeStringSlice(allocator, laws_used);
        const counterexample = if (self.counterexample) |text|
            try allocator.dupe(u8, text)
        else
            null;
        errdefer if (counterexample) |text| allocator.free(text);
        return .{
            .classification = try allocator.dupe(u8, self.classification),
            .proof_level = try allocator.dupe(u8, self.proof_level),
            .recommendation = try allocator.dupe(u8, self.recommendation),
            .counterexample = counterexample,
            .laws_used = laws_used,
        };
    }

    pub fn deinit(self: *ProveSummary, allocator: std.mem.Allocator) void {
        allocator.free(self.classification);
        allocator.free(self.proof_level);
        allocator.free(self.recommendation);
        if (self.counterexample) |text| allocator.free(text);
        freeStringSlice(allocator, self.laws_used);
        self.* = .{
            .classification = &.{},
            .proof_level = &.{},
            .recommendation = &.{},
            .counterexample = null,
            .laws_used = &.{},
        };
    }
};

pub const SystemProofSummary = struct {
    system_path: []u8,
    proof_level: []u8,
    all_links_resolved: bool,
    all_responses_covered: bool,
    payload_compatible: bool,
    injection_safe: bool,
    no_secret_leakage: bool,
    no_credential_leakage: bool,
    retry_safe: bool,
    fault_covered: bool,
    state_isolated: bool,
    max_system_io_depth: ?u32 = null,
    dynamic_links: u32,
    warnings: [][]u8,

    pub fn clone(self: SystemProofSummary, allocator: std.mem.Allocator) !SystemProofSummary {
        return .{
            .system_path = try allocator.dupe(u8, self.system_path),
            .proof_level = try allocator.dupe(u8, self.proof_level),
            .all_links_resolved = self.all_links_resolved,
            .all_responses_covered = self.all_responses_covered,
            .payload_compatible = self.payload_compatible,
            .injection_safe = self.injection_safe,
            .no_secret_leakage = self.no_secret_leakage,
            .no_credential_leakage = self.no_credential_leakage,
            .retry_safe = self.retry_safe,
            .fault_covered = self.fault_covered,
            .state_isolated = self.state_isolated,
            .max_system_io_depth = self.max_system_io_depth,
            .dynamic_links = self.dynamic_links,
            .warnings = try cloneStringSlice(allocator, self.warnings),
        };
    }

    pub fn deinit(self: *SystemProofSummary, allocator: std.mem.Allocator) void {
        allocator.free(self.system_path);
        allocator.free(self.proof_level);
        freeStringSlice(allocator, self.warnings);
        self.* = .{
            .system_path = &.{},
            .proof_level = &.{},
            .all_links_resolved = false,
            .all_responses_covered = false,
            .payload_compatible = false,
            .injection_safe = false,
            .no_secret_leakage = false,
            .no_credential_leakage = false,
            .retry_safe = false,
            .fault_covered = false,
            .state_isolated = false,
            .max_system_io_depth = null,
            .dynamic_links = 0,
            .warnings = &.{},
        };
    }
};

pub const VerifiedPatchPayload = struct {
    file: []u8,
    policy_hash: []u8,
    applied_at_unix_ms: i64,
    stats: ProofStats,
    before: ?[]u8,
    after: []u8,
    unified_diff: []u8,
    hunks: []DiffHunk,
    violations: []ViolationDeltaItem,
    before_properties: ?PropertiesSnapshot,
    after_properties: ?PropertiesSnapshot,
    prove: ?ProveSummary,
    system: ?SystemProofSummary,
    rule_citations: [][]u8,
    repair_plan_ids: [][]u8 = &.{},
    closed_witness_ids: [][]u8 = &.{},
    patch_hash: ?[32]u8 = null,
    parent_hash: ?[32]u8 = null,
    goal_context: [][]u8 = &.{},
    witnesses_defeated: [][]u8 = &.{},
    witnesses_new: [][]u8 = &.{},
    post_apply_ok: bool,
    post_apply_summary: ?[]u8,

    pub fn clone(self: VerifiedPatchPayload, allocator: std.mem.Allocator) !VerifiedPatchPayload {
        const file_copy = try allocator.dupe(u8, self.file);
        errdefer allocator.free(file_copy);
        const policy_copy = try allocator.dupe(u8, self.policy_hash);
        errdefer allocator.free(policy_copy);
        const before_copy: ?[]u8 = if (self.before) |b| try allocator.dupe(u8, b) else null;
        errdefer if (before_copy) |b| allocator.free(b);
        const after_copy = try allocator.dupe(u8, self.after);
        errdefer allocator.free(after_copy);
        const unified_diff_copy = try allocator.dupe(u8, self.unified_diff);
        errdefer allocator.free(unified_diff_copy);
        const hunks_copy = try allocator.dupe(DiffHunk, self.hunks);
        errdefer allocator.free(hunks_copy);
        const violations_copy = try allocator.alloc(ViolationDeltaItem, self.violations.len);
        errdefer allocator.free(violations_copy);
        for (violations_copy) |*item| item.* = undefined;
        var violation_index: usize = 0;
        errdefer {
            while (violation_index > 0) {
                violation_index -= 1;
                violations_copy[violation_index].deinit(allocator);
            }
            allocator.free(violations_copy);
        }
        while (violation_index < self.violations.len) : (violation_index += 1) {
            violations_copy[violation_index] = try self.violations[violation_index].clone(allocator);
        }
        var prove_copy = if (self.prove) |summary|
            try summary.clone(allocator)
        else
            null;
        errdefer if (prove_copy) |*summary| summary.deinit(allocator);
        var system_copy = if (self.system) |summary|
            try summary.clone(allocator)
        else
            null;
        errdefer if (system_copy) |*summary| summary.deinit(allocator);
        const citations_copy = try cloneStringSlice(allocator, self.rule_citations);
        errdefer freeStringSlice(allocator, citations_copy);
        const repair_plan_ids_copy = try cloneStringSlice(allocator, self.repair_plan_ids);
        errdefer freeStringSlice(allocator, repair_plan_ids_copy);
        const closed_witness_ids_copy = try cloneStringSlice(allocator, self.closed_witness_ids);
        errdefer freeStringSlice(allocator, closed_witness_ids_copy);
        const goal_context_copy = try cloneStringSlice(allocator, self.goal_context);
        errdefer freeStringSlice(allocator, goal_context_copy);
        const witnesses_defeated_copy = try cloneStringSlice(allocator, self.witnesses_defeated);
        errdefer freeStringSlice(allocator, witnesses_defeated_copy);
        const witnesses_new_copy = try cloneStringSlice(allocator, self.witnesses_new);
        errdefer freeStringSlice(allocator, witnesses_new_copy);
        const post_apply_summary_copy: ?[]u8 = if (self.post_apply_summary) |s|
            try allocator.dupe(u8, s)
        else
            null;
        errdefer if (post_apply_summary_copy) |s| allocator.free(s);
        return .{
            .file = file_copy,
            .policy_hash = policy_copy,
            .applied_at_unix_ms = self.applied_at_unix_ms,
            .stats = self.stats,
            .before = before_copy,
            .after = after_copy,
            .unified_diff = unified_diff_copy,
            .hunks = hunks_copy,
            .violations = violations_copy,
            .before_properties = self.before_properties,
            .after_properties = self.after_properties,
            .prove = prove_copy,
            .system = system_copy,
            .rule_citations = citations_copy,
            .repair_plan_ids = repair_plan_ids_copy,
            .closed_witness_ids = closed_witness_ids_copy,
            .patch_hash = self.patch_hash,
            .parent_hash = self.parent_hash,
            .goal_context = goal_context_copy,
            .witnesses_defeated = witnesses_defeated_copy,
            .witnesses_new = witnesses_new_copy,
            .post_apply_ok = self.post_apply_ok,
            .post_apply_summary = post_apply_summary_copy,
        };
    }

    pub fn deinit(self: *VerifiedPatchPayload, allocator: std.mem.Allocator) void {
        allocator.free(self.file);
        allocator.free(self.policy_hash);
        if (self.before) |b| allocator.free(b);
        allocator.free(self.after);
        allocator.free(self.unified_diff);
        allocator.free(self.hunks);
        for (self.violations) |*item| item.deinit(allocator);
        allocator.free(self.violations);
        if (self.prove) |*summary| summary.deinit(allocator);
        if (self.system) |*summary| summary.deinit(allocator);
        freeStringSlice(allocator, self.rule_citations);
        freeStringSlice(allocator, self.repair_plan_ids);
        freeStringSlice(allocator, self.closed_witness_ids);
        freeStringSlice(allocator, self.goal_context);
        freeStringSlice(allocator, self.witnesses_defeated);
        freeStringSlice(allocator, self.witnesses_new);
        if (self.post_apply_summary) |s| allocator.free(s);
        self.* = .{
            .file = &.{},
            .policy_hash = &.{},
            .applied_at_unix_ms = 0,
            .stats = .{ .total = 0, .new = 0, .preexisting = null },
            .before = null,
            .after = &.{},
            .unified_diff = &.{},
            .hunks = &.{},
            .violations = &.{},
            .before_properties = null,
            .after_properties = null,
            .prove = null,
            .system = null,
            .rule_citations = &.{},
            .repair_plan_ids = &.{},
            .closed_witness_ids = &.{},
            .patch_hash = null,
            .parent_hash = null,
            .goal_context = &.{},
            .witnesses_defeated = &.{},
            .witnesses_new = &.{},
            .post_apply_ok = false,
            .post_apply_summary = null,
        };
    }
};

fn cloneStringSlice(allocator: std.mem.Allocator, items: []const []u8) ![][]u8 {
    const copy = try allocator.alloc([]u8, items.len);
    errdefer allocator.free(copy);
    var i: usize = 0;
    errdefer {
        while (i > 0) {
            i -= 1;
            allocator.free(copy[i]);
        }
        allocator.free(copy);
    }
    while (i < items.len) : (i += 1) {
        copy[i] = try allocator.dupe(u8, items[i]);
    }
    return copy;
}

fn freeStringSlice(allocator: std.mem.Allocator, items: []const []u8) void {
    for (items) |item| allocator.free(item);
    allocator.free(items);
}

pub const SessionTreeNode = struct {
    session_id: []u8,
    parent_id: ?[]u8,
    created_at_unix_ms: i64,
    depth: usize,
    is_current: bool,
    is_orphan_root: bool,

    pub fn clone(self: SessionTreeNode, allocator: std.mem.Allocator) !SessionTreeNode {
        return .{
            .session_id = try allocator.dupe(u8, self.session_id),
            .parent_id = if (self.parent_id) |parent_id|
                try allocator.dupe(u8, parent_id)
            else
                null,
            .created_at_unix_ms = self.created_at_unix_ms,
            .depth = self.depth,
            .is_current = self.is_current,
            .is_orphan_root = self.is_orphan_root,
        };
    }

    pub fn deinit(self: *SessionTreeNode, allocator: std.mem.Allocator) void {
        allocator.free(self.session_id);
        if (self.parent_id) |parent_id| allocator.free(parent_id);
        self.* = .{
            .session_id = &.{},
            .parent_id = null,
            .created_at_unix_ms = 0,
            .depth = 0,
            .is_current = false,
            .is_orphan_root = false,
        };
    }
};

pub const SessionTreePayload = struct {
    nodes: []SessionTreeNode,

    pub fn clone(self: SessionTreePayload, allocator: std.mem.Allocator) !SessionTreePayload {
        const owned = try allocator.alloc(SessionTreeNode, self.nodes.len);
        errdefer allocator.free(owned);
        for (owned) |*node| node.* = undefined;
        var i: usize = 0;
        errdefer {
            while (i > 0) {
                i -= 1;
                owned[i].deinit(allocator);
            }
            allocator.free(owned);
        }
        while (i < self.nodes.len) : (i += 1) {
            owned[i] = try self.nodes[i].clone(allocator);
        }
        return .{ .nodes = owned };
    }

    pub fn deinit(self: *SessionTreePayload, allocator: std.mem.Allocator) void {
        for (self.nodes) |*node| node.deinit(allocator);
        allocator.free(self.nodes);
        self.* = .{ .nodes = &.{} };
    }
};

pub const UiPayload = union(enum) {
    session_tree: SessionTreePayload,
    diagnostics: DiagnosticsPayload,
    proof_card: ProofCardPayload,
    command_outcome: CommandOutcomePayload,
    repair_candidate: RepairCandidatePayload,
    verified_patch: VerifiedPatchPayload,
    plain_text: []u8,

    pub fn clone(self: UiPayload, allocator: std.mem.Allocator) !UiPayload {
        return switch (self) {
            .session_tree => |payload| .{ .session_tree = try payload.clone(allocator) },
            .diagnostics => |payload| .{ .diagnostics = try payload.clone(allocator) },
            .proof_card => |payload| .{ .proof_card = try payload.clone(allocator) },
            .command_outcome => |payload| .{ .command_outcome = try payload.clone(allocator) },
            .repair_candidate => |payload| .{ .repair_candidate = try payload.clone(allocator) },
            .verified_patch => |payload| .{ .verified_patch = try payload.clone(allocator) },
            .plain_text => |text| .{ .plain_text = try allocator.dupe(u8, text) },
        };
    }

    pub fn deinit(self: *UiPayload, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .session_tree => |*payload| payload.deinit(allocator),
            .diagnostics => |*payload| payload.deinit(allocator),
            .proof_card => |*payload| payload.deinit(allocator),
            .command_outcome => |*payload| payload.deinit(allocator),
            .repair_candidate => |*payload| payload.deinit(allocator),
            .verified_patch => |*payload| payload.deinit(allocator),
            .plain_text => |text| allocator.free(text),
        }
        self.* = .{ .plain_text = &.{} };
    }
};

pub fn writeJson(writer: *std.Io.Writer, payload: UiPayload) !void {
    try writer.writeByte('{');
    switch (payload) {
        .plain_text => |text| {
            try writer.writeAll("\"kind\":\"plain_text\",\"text\":");
            try json_writer.writeString(writer, text);
        },
        .session_tree => |tree| {
            try writer.writeAll("\"kind\":\"session_tree\",\"nodes\":[");
            for (tree.nodes, 0..) |node, i| {
                if (i > 0) try writer.writeByte(',');
                try writer.writeByte('{');
                try writer.writeAll("\"session_id\":");
                try json_writer.writeString(writer, node.session_id);
                try writer.writeAll(",\"parent_id\":");
                if (node.parent_id) |parent_id| {
                    try json_writer.writeString(writer, parent_id);
                } else {
                    try writer.writeAll("null");
                }
                try writer.writeAll(",\"created_at_unix_ms\":");
                try writer.print("{d}", .{node.created_at_unix_ms});
                try writer.writeAll(",\"depth\":");
                try writer.print("{d}", .{node.depth});
                try writer.writeAll(",\"is_current\":");
                try writer.writeAll(if (node.is_current) "true" else "false");
                try writer.writeAll(",\"is_orphan_root\":");
                try writer.writeAll(if (node.is_orphan_root) "true" else "false");
                try writer.writeByte('}');
            }
            try writer.writeByte(']');
        },
        .diagnostics => |diagnostics| {
            try writer.writeAll("\"kind\":\"diagnostics\",\"summary\":");
            try json_writer.writeString(writer, diagnostics.summary);
            try writer.writeAll(",\"items\":[");
            for (diagnostics.items, 0..) |item, i| {
                if (i > 0) try writer.writeByte(',');
                try writer.writeByte('{');
                try writer.writeAll("\"code\":");
                try json_writer.writeString(writer, item.code);
                try writer.writeAll(",\"severity\":");
                try json_writer.writeString(writer, item.severity);
                try writer.writeAll(",\"path\":");
                try json_writer.writeString(writer, item.path);
                try writer.writeAll(",\"line\":");
                try writer.print("{d}", .{item.line});
                try writer.writeAll(",\"column\":");
                try writer.print("{d}", .{item.column});
                try writer.writeAll(",\"message\":");
                try json_writer.writeString(writer, item.message);
                if (item.introduced_by_patch) |introduced_by_patch| {
                    try writer.writeAll(",\"introduced_by_patch\":");
                    try writer.writeAll(if (introduced_by_patch) "true" else "false");
                }
                try writer.writeByte('}');
            }
            try writer.writeByte(']');
        },
        .proof_card => |proof| {
            try writer.writeAll("\"kind\":\"proof_card\",\"title\":");
            try json_writer.writeString(writer, proof.title);
            try writer.writeAll(",\"summary\":");
            try json_writer.writeString(writer, proof.summary);
            try writer.writeAll(",\"stats\":{\"total\":");
            try writer.print("{d}", .{proof.stats.total});
            try writer.writeAll(",\"new\":");
            try writer.print("{d}", .{proof.stats.new});
            if (proof.stats.preexisting) |preexisting| {
                try writer.writeAll(",\"preexisting\":");
                try writer.print("{d}", .{preexisting});
            }
            try writer.writeAll("},\"highlights\":[");
            for (proof.highlights, 0..) |highlight, i| {
                if (i > 0) try writer.writeByte(',');
                try json_writer.writeString(writer, highlight);
            }
            try writer.writeByte(']');
        },
        .command_outcome => |command| {
            try writer.writeAll("\"kind\":\"command_outcome\",\"title\":");
            try json_writer.writeString(writer, command.title);
            try writer.writeAll(",\"exit_code\":");
            if (command.exit_code) |exit_code| {
                try writer.print("{d}", .{exit_code});
            } else {
                try writer.writeAll("null");
            }
            try writer.writeAll(",\"stdout\":");
            try json_writer.writeString(writer, command.stdout);
            try writer.writeAll(",\"stderr\":");
            try json_writer.writeString(writer, command.stderr);
            try writer.writeAll(",\"command\":");
            try json_writer.writeString(writer, command.command);
        },
        .repair_candidate => |candidate| {
            try writer.writeAll("\"kind\":\"repair_candidate\",\"path\":");
            try json_writer.writeString(writer, candidate.path);
            try writer.writeAll(",\"plan_id\":");
            try json_writer.writeString(writer, candidate.plan_id);
            try writer.writeAll(",\"intent_kind\":");
            try json_writer.writeString(writer, candidate.intent_kind);
            try writer.writeAll(",\"proposed_content\":");
            try json_writer.writeString(writer, candidate.proposed_content);
            try writer.writeAll(",\"verification_ok\":");
            try writer.writeAll(if (candidate.verification_ok) "true" else "false");
            try writer.writeAll(",\"verification_summary\":");
            try json_writer.writeString(writer, candidate.verification_summary);
            try writer.writeAll(",\"stats\":{\"total\":");
            try writer.print("{d}", .{candidate.stats.total});
            try writer.writeAll(",\"new\":");
            try writer.print("{d}", .{candidate.stats.new});
            if (candidate.stats.preexisting) |preexisting| {
                try writer.writeAll(",\"preexisting\":");
                try writer.print("{d}", .{preexisting});
            }
            try writer.writeByte('}');
        },
        .verified_patch => |patch| {
            try writer.writeAll("\"kind\":\"verified_patch\",\"file\":");
            try json_writer.writeString(writer, patch.file);
            try writer.writeAll(",\"policy_hash\":");
            try json_writer.writeString(writer, patch.policy_hash);
            try writer.writeAll(",\"applied_at_unix_ms\":");
            try writer.print("{d}", .{patch.applied_at_unix_ms});
            try writer.writeAll(",\"stats\":{\"total\":");
            try writer.print("{d}", .{patch.stats.total});
            try writer.writeAll(",\"new\":");
            try writer.print("{d}", .{patch.stats.new});
            if (patch.stats.preexisting) |preexisting| {
                try writer.writeAll(",\"preexisting\":");
                try writer.print("{d}", .{preexisting});
            }
            try writer.writeAll("},\"before\":");
            if (patch.before) |b| {
                try json_writer.writeString(writer, b);
            } else {
                try writer.writeAll("null");
            }
            try writer.writeAll(",\"after\":");
            try json_writer.writeString(writer, patch.after);
            try writer.writeAll(",\"unified_diff\":");
            try json_writer.writeString(writer, patch.unified_diff);
            try writer.writeAll(",\"hunks\":[");
            for (patch.hunks, 0..) |hunk, i| {
                if (i > 0) try writer.writeByte(',');
                try writer.writeAll("{\"old_start\":");
                try writer.print("{d}", .{hunk.old_start});
                try writer.writeAll(",\"old_count\":");
                try writer.print("{d}", .{hunk.old_count});
                try writer.writeAll(",\"new_start\":");
                try writer.print("{d}", .{hunk.new_start});
                try writer.writeAll(",\"new_count\":");
                try writer.print("{d}", .{hunk.new_count});
                try writer.writeByte('}');
            }
            try writer.writeAll("],\"violations\":[");
            for (patch.violations, 0..) |violation, i| {
                if (i > 0) try writer.writeByte(',');
                try writer.writeAll("{\"stable_key\":");
                try json_writer.writeString(writer, violation.stable_key);
                try writer.writeAll(",\"code\":");
                try json_writer.writeString(writer, violation.code);
                try writer.writeAll(",\"severity\":");
                try json_writer.writeString(writer, violation.severity);
                try writer.writeAll(",\"message\":");
                try json_writer.writeString(writer, violation.message);
                try writer.writeAll(",\"line\":");
                try writer.print("{d}", .{violation.line});
                try writer.writeAll(",\"column\":");
                try writer.print("{d}", .{violation.column});
                try writer.writeAll(",\"introduced_by_patch\":");
                try writer.writeAll(if (violation.introduced_by_patch) "true" else "false");
                try writer.writeByte('}');
            }
            try writer.writeAll("],\"before_properties\":");
            try writePropertiesSnapshot(writer, patch.before_properties);
            try writer.writeAll(",\"after_properties\":");
            try writePropertiesSnapshot(writer, patch.after_properties);
            try writer.writeAll(",\"prove\":");
            if (patch.prove) |prove| {
                try writer.writeByte('{');
                try writer.writeAll("\"classification\":");
                try json_writer.writeString(writer, prove.classification);
                try writer.writeAll(",\"proof_level\":");
                try json_writer.writeString(writer, prove.proof_level);
                try writer.writeAll(",\"recommendation\":");
                try json_writer.writeString(writer, prove.recommendation);
                try writer.writeAll(",\"counterexample\":");
                if (prove.counterexample) |counterexample| {
                    try json_writer.writeString(writer, counterexample);
                } else {
                    try writer.writeAll("null");
                }
                try writer.writeAll(",\"laws_used\":[");
                for (prove.laws_used, 0..) |law, i| {
                    if (i > 0) try writer.writeByte(',');
                    try json_writer.writeString(writer, law);
                }
                try writer.writeByte(']');
                try writer.writeByte('}');
            } else {
                try writer.writeAll("null");
            }
            try writer.writeAll(",\"system\":");
            if (patch.system) |system| {
                try writer.writeByte('{');
                try writer.writeAll("\"system_path\":");
                try json_writer.writeString(writer, system.system_path);
                try writer.writeAll(",\"proof_level\":");
                try json_writer.writeString(writer, system.proof_level);
                try writer.writeAll(",\"all_links_resolved\":");
                try writer.writeAll(if (system.all_links_resolved) "true" else "false");
                try writer.writeAll(",\"all_responses_covered\":");
                try writer.writeAll(if (system.all_responses_covered) "true" else "false");
                try writer.writeAll(",\"payload_compatible\":");
                try writer.writeAll(if (system.payload_compatible) "true" else "false");
                try writer.writeAll(",\"injection_safe\":");
                try writer.writeAll(if (system.injection_safe) "true" else "false");
                try writer.writeAll(",\"no_secret_leakage\":");
                try writer.writeAll(if (system.no_secret_leakage) "true" else "false");
                try writer.writeAll(",\"no_credential_leakage\":");
                try writer.writeAll(if (system.no_credential_leakage) "true" else "false");
                try writer.writeAll(",\"retry_safe\":");
                try writer.writeAll(if (system.retry_safe) "true" else "false");
                try writer.writeAll(",\"fault_covered\":");
                try writer.writeAll(if (system.fault_covered) "true" else "false");
                try writer.writeAll(",\"state_isolated\":");
                try writer.writeAll(if (system.state_isolated) "true" else "false");
                try writer.writeAll(",\"max_system_io_depth\":");
                if (system.max_system_io_depth) |depth| {
                    try writer.print("{d}", .{depth});
                } else {
                    try writer.writeAll("null");
                }
                try writer.writeAll(",\"dynamic_links\":");
                try writer.print("{d}", .{system.dynamic_links});
                try writer.writeAll(",\"warnings\":[");
                for (system.warnings, 0..) |warning, i| {
                    if (i > 0) try writer.writeByte(',');
                    try json_writer.writeString(writer, warning);
                }
                try writer.writeByte(']');
                try writer.writeByte('}');
            } else {
                try writer.writeAll("null");
            }
            try writer.writeAll(",\"rule_citations\":[");
            for (patch.rule_citations, 0..) |citation, i| {
                if (i > 0) try writer.writeByte(',');
                try json_writer.writeString(writer, citation);
            }
            try writer.writeByte(']');
            try writer.writeAll(",\"repair_plan_ids\":[");
            for (patch.repair_plan_ids, 0..) |repair_id, i| {
                if (i > 0) try writer.writeByte(',');
                try json_writer.writeString(writer, repair_id);
            }
            try writer.writeByte(']');
            try writer.writeAll(",\"closed_witness_ids\":[");
            for (patch.closed_witness_ids, 0..) |closed_id, i| {
                if (i > 0) try writer.writeByte(',');
                try json_writer.writeString(writer, closed_id);
            }
            try writer.writeByte(']');
            if (patch.patch_hash) |hash| {
                try writer.writeAll(",\"patch_hash\":\"");
                const hex = std.fmt.bytesToHex(hash, .lower);
                try writer.writeAll(&hex);
                try writer.writeByte('"');
            }
            if (patch.parent_hash) |hash| {
                try writer.writeAll(",\"parent_hash\":\"");
                const hex = std.fmt.bytesToHex(hash, .lower);
                try writer.writeAll(&hex);
                try writer.writeByte('"');
            }
            try writeOptionalStringArray(writer, "goal_context", patch.goal_context);
            try writeOptionalStringArray(writer, "witnesses_defeated", patch.witnesses_defeated);
            try writeOptionalStringArray(writer, "witnesses_new", patch.witnesses_new);
            try writer.writeAll(",\"post_apply_ok\":");
            try writer.writeAll(if (patch.post_apply_ok) "true" else "false");
            if (patch.post_apply_summary) |s| {
                try writer.writeAll(",\"post_apply_summary\":");
                try json_writer.writeString(writer, s);
            }
        },
    }
    try writer.writeByte('}');
}

pub fn parse(allocator: std.mem.Allocator, value: std.json.Value) !UiPayload {
    if (value != .object) return error.InvalidUiPayload;
    const obj = value.object;
    const kind_val = obj.get("kind") orelse return error.InvalidUiPayload;
    if (kind_val != .string) return error.InvalidUiPayload;

    if (std.mem.eql(u8, kind_val.string, "plain_text")) {
        const text_val = obj.get("text") orelse return error.InvalidUiPayload;
        if (text_val != .string) return error.InvalidUiPayload;
        return .{ .plain_text = try allocator.dupe(u8, text_val.string) };
    }
    if (std.mem.eql(u8, kind_val.string, "session_tree")) {
        const nodes_val = obj.get("nodes") orelse return error.InvalidUiPayload;
        if (nodes_val != .array) return error.InvalidUiPayload;
        const nodes = try allocator.alloc(SessionTreeNode, nodes_val.array.items.len);
        errdefer allocator.free(nodes);
        for (nodes) |*node| node.* = undefined;
        var i: usize = 0;
        errdefer {
            while (i > 0) {
                i -= 1;
                nodes[i].deinit(allocator);
            }
            allocator.free(nodes);
        }
        while (i < nodes_val.array.items.len) : (i += 1) {
            const item = nodes_val.array.items[i];
            if (item != .object) return error.InvalidUiPayload;
            const item_obj = item.object;
            const session_id = getString(item_obj, "session_id") orelse return error.InvalidUiPayload;
            const parent_id = getOptionalString(item_obj, "parent_id") catch return error.InvalidUiPayload;
            const created_at_unix_ms = getInteger(item_obj, "created_at_unix_ms") orelse return error.InvalidUiPayload;
            const depth = getUnsigned(item_obj, "depth") orelse return error.InvalidUiPayload;
            const is_current = getBool(item_obj, "is_current") orelse return error.InvalidUiPayload;
            const is_orphan_root = getBool(item_obj, "is_orphan_root") orelse return error.InvalidUiPayload;
            nodes[i] = .{
                .session_id = try allocator.dupe(u8, session_id),
                .parent_id = if (parent_id) |pid| try allocator.dupe(u8, pid) else null,
                .created_at_unix_ms = created_at_unix_ms,
                .depth = depth,
                .is_current = is_current,
                .is_orphan_root = is_orphan_root,
            };
        }
        return .{ .session_tree = .{ .nodes = nodes } };
    }
    if (std.mem.eql(u8, kind_val.string, "diagnostics")) {
        const summary = getString(obj, "summary") orelse return error.InvalidUiPayload;
        const items_val = obj.get("items") orelse return error.InvalidUiPayload;
        if (items_val != .array) return error.InvalidUiPayload;
        const items = try allocator.alloc(DiagnosticItem, items_val.array.items.len);
        errdefer allocator.free(items);
        for (items) |*item| item.* = undefined;
        var i: usize = 0;
        errdefer {
            while (i > 0) {
                i -= 1;
                items[i].deinit(allocator);
            }
            allocator.free(items);
        }
        while (i < items_val.array.items.len) : (i += 1) {
            const item_val = items_val.array.items[i];
            if (item_val != .object) return error.InvalidUiPayload;
            const item_obj = item_val.object;
            items[i] = try DiagnosticItem.init(
                allocator,
                getString(item_obj, "code") orelse return error.InvalidUiPayload,
                getString(item_obj, "severity") orelse return error.InvalidUiPayload,
                getString(item_obj, "path") orelse return error.InvalidUiPayload,
                @intCast(getUnsigned(item_obj, "line") orelse return error.InvalidUiPayload),
                @intCast(getUnsigned(item_obj, "column") orelse return error.InvalidUiPayload),
                getString(item_obj, "message") orelse return error.InvalidUiPayload,
                getBool(item_obj, "introduced_by_patch"),
            );
        }
        return .{ .diagnostics = .{
            .summary = try allocator.dupe(u8, summary),
            .items = items,
        } };
    }
    if (std.mem.eql(u8, kind_val.string, "proof_card")) {
        const title = getString(obj, "title") orelse return error.InvalidUiPayload;
        const summary = getString(obj, "summary") orelse return error.InvalidUiPayload;
        const stats_val = obj.get("stats") orelse return error.InvalidUiPayload;
        if (stats_val != .object) return error.InvalidUiPayload;
        const stats_obj = stats_val.object;
        const highlights_val = obj.get("highlights") orelse return error.InvalidUiPayload;
        if (highlights_val != .array) return error.InvalidUiPayload;
        const highlights = try allocator.alloc([]u8, highlights_val.array.items.len);
        errdefer allocator.free(highlights);
        var i: usize = 0;
        errdefer {
            while (i > 0) {
                i -= 1;
                allocator.free(highlights[i]);
            }
            allocator.free(highlights);
        }
        while (i < highlights_val.array.items.len) : (i += 1) {
            const highlight = highlights_val.array.items[i];
            if (highlight != .string) return error.InvalidUiPayload;
            highlights[i] = try allocator.dupe(u8, highlight.string);
        }
        return .{ .proof_card = .{
            .title = try allocator.dupe(u8, title),
            .summary = try allocator.dupe(u8, summary),
            .stats = .{
                .total = @intCast(getUnsigned(stats_obj, "total") orelse return error.InvalidUiPayload),
                .new = @intCast(getUnsigned(stats_obj, "new") orelse return error.InvalidUiPayload),
                .preexisting = if (getUnsigned(stats_obj, "preexisting")) |preexisting|
                    @intCast(preexisting)
                else
                    null,
            },
            .highlights = highlights,
        } };
    }
    if (std.mem.eql(u8, kind_val.string, "command_outcome")) {
        const title = getString(obj, "title") orelse return error.InvalidUiPayload;
        const stdout = getString(obj, "stdout") orelse return error.InvalidUiPayload;
        const stderr = getString(obj, "stderr") orelse return error.InvalidUiPayload;
        const command = getString(obj, "command") orelse return error.InvalidUiPayload;
        return .{ .command_outcome = .{
            .title = try allocator.dupe(u8, title),
            .exit_code = if (getUnsigned(obj, "exit_code")) |exit_code|
                @intCast(exit_code)
            else
                null,
            .stdout = try allocator.dupe(u8, stdout),
            .stderr = try allocator.dupe(u8, stderr),
            .command = try allocator.dupe(u8, command),
        } };
    }
    if (std.mem.eql(u8, kind_val.string, "repair_candidate")) {
        const stats_val = obj.get("stats") orelse return error.InvalidUiPayload;
        if (stats_val != .object) return error.InvalidUiPayload;
        const stats_obj = stats_val.object;
        return .{ .repair_candidate = try RepairCandidatePayload.init(
            allocator,
            getString(obj, "path") orelse return error.InvalidUiPayload,
            getString(obj, "plan_id") orelse return error.InvalidUiPayload,
            getString(obj, "intent_kind") orelse return error.InvalidUiPayload,
            getString(obj, "proposed_content") orelse return error.InvalidUiPayload,
            getBool(obj, "verification_ok") orelse return error.InvalidUiPayload,
            getString(obj, "verification_summary") orelse return error.InvalidUiPayload,
            .{
                .total = @intCast(getUnsigned(stats_obj, "total") orelse return error.InvalidUiPayload),
                .new = @intCast(getUnsigned(stats_obj, "new") orelse return error.InvalidUiPayload),
                .preexisting = if (getUnsigned(stats_obj, "preexisting")) |preexisting|
                    @intCast(preexisting)
                else
                    null,
            },
        ) };
    }
    if (std.mem.eql(u8, kind_val.string, "verified_patch")) {
        const file = getString(obj, "file") orelse return error.InvalidUiPayload;
        const policy_hash = getString(obj, "policy_hash") orelse return error.InvalidUiPayload;
        const stats_val = obj.get("stats") orelse return error.InvalidUiPayload;
        if (stats_val != .object) return error.InvalidUiPayload;
        const stats_obj = stats_val.object;
        const after = getString(obj, "after") orelse return error.InvalidUiPayload;
        const before_opt = try getOptionalString(obj, "before");
        const applied_at_unix_ms = getInteger(obj, "applied_at_unix_ms") orelse 0;
        const unified_diff = getString(obj, "unified_diff") orelse "";
        const post_apply_ok = getBool(obj, "post_apply_ok") orelse return error.InvalidUiPayload;
        const hunks = try parseDiffHunks(allocator, obj.get("hunks"));
        errdefer allocator.free(hunks);
        const violations = try parseViolationDeltaItems(allocator, obj.get("violations"));
        errdefer {
            for (violations) |*item| item.deinit(allocator);
            allocator.free(violations);
        }
        const before_properties = try parsePropertiesSnapshot(obj.get("before_properties"));
        const after_properties = try parsePropertiesSnapshot(obj.get("after_properties"));
        var prove = try parseProveSummary(allocator, obj.get("prove"));
        errdefer if (prove) |*summary| summary.deinit(allocator);
        var system = try parseSystemProofSummary(allocator, obj.get("system"));
        errdefer if (system) |*summary| summary.deinit(allocator);
        const rule_citations = try parseStringArrayField(allocator, obj.get("rule_citations"));
        errdefer freeStringSlice(allocator, rule_citations);
        const repair_plan_ids = try parseStringArrayField(allocator, obj.get("repair_plan_ids"));
        errdefer freeStringSlice(allocator, repair_plan_ids);
        const closed_witness_ids = try parseStringArrayField(allocator, obj.get("closed_witness_ids"));
        errdefer freeStringSlice(allocator, closed_witness_ids);
        const goal_context = try parseStringArrayField(allocator, obj.get("goal_context"));
        errdefer freeStringSlice(allocator, goal_context);
        const witnesses_defeated = try parseStringArrayField(allocator, obj.get("witnesses_defeated"));
        errdefer freeStringSlice(allocator, witnesses_defeated);
        const witnesses_new = try parseStringArrayField(allocator, obj.get("witnesses_new"));
        errdefer freeStringSlice(allocator, witnesses_new);
        const patch_hash_opt = try parseHash32(obj.get("patch_hash"));
        const parent_hash_opt = try parseHash32(obj.get("parent_hash"));

        const file_copy = try allocator.dupe(u8, file);
        errdefer allocator.free(file_copy);
        const policy_copy = try allocator.dupe(u8, policy_hash);
        errdefer allocator.free(policy_copy);
        const before_copy: ?[]u8 = if (before_opt) |b| try allocator.dupe(u8, b) else null;
        errdefer if (before_copy) |b| allocator.free(b);
        const after_copy = try allocator.dupe(u8, after);
        errdefer allocator.free(after_copy);
        const unified_diff_copy = try allocator.dupe(u8, unified_diff);
        errdefer allocator.free(unified_diff_copy);
        const post_apply_summary_copy: ?[]u8 = blk: {
            const s = try getOptionalString(obj, "post_apply_summary");
            break :blk if (s) |text| try allocator.dupe(u8, text) else null;
        };
        errdefer if (post_apply_summary_copy) |s| allocator.free(s);

        return .{ .verified_patch = .{
            .file = file_copy,
            .policy_hash = policy_copy,
            .applied_at_unix_ms = applied_at_unix_ms,
            .stats = .{
                .total = @intCast(getUnsigned(stats_obj, "total") orelse return error.InvalidUiPayload),
                .new = @intCast(getUnsigned(stats_obj, "new") orelse return error.InvalidUiPayload),
                .preexisting = if (getUnsigned(stats_obj, "preexisting")) |preexisting|
                    @intCast(preexisting)
                else
                    null,
            },
            .before = before_copy,
            .after = after_copy,
            .unified_diff = unified_diff_copy,
            .hunks = hunks,
            .violations = violations,
            .before_properties = before_properties,
            .after_properties = after_properties,
            .prove = prove,
            .system = system,
            .rule_citations = rule_citations,
            .repair_plan_ids = repair_plan_ids,
            .closed_witness_ids = closed_witness_ids,
            .patch_hash = patch_hash_opt,
            .parent_hash = parent_hash_opt,
            .goal_context = goal_context,
            .witnesses_defeated = witnesses_defeated,
            .witnesses_new = witnesses_new,
            .post_apply_ok = post_apply_ok,
            .post_apply_summary = post_apply_summary_copy,
        } };
    }

    return error.InvalidUiPayload;
}

fn parseHash32(value_opt: ?std.json.Value) !?[32]u8 {
    const value = value_opt orelse return null;
    return switch (value) {
        .null => null,
        .string => |s| blk: {
            if (s.len != 64) return error.InvalidUiPayload;
            var out: [32]u8 = undefined;
            var i: usize = 0;
            while (i < 32) : (i += 1) {
                const hi = hexNibble(s[i * 2]) orelse return error.InvalidUiPayload;
                const lo = hexNibble(s[i * 2 + 1]) orelse return error.InvalidUiPayload;
                out[i] = (hi << 4) | lo;
            }
            break :blk out;
        },
        else => error.InvalidUiPayload,
    };
}

fn hexNibble(c: u8) ?u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'a'...'f' => c - 'a' + 10,
        'A'...'F' => c - 'A' + 10,
        else => null,
    };
}

fn writeOptionalStringArray(
    writer: *std.Io.Writer,
    field_name: []const u8,
    items: []const []u8,
) !void {
    if (items.len == 0) return;
    try writer.writeAll(",\"");
    try writer.writeAll(field_name);
    try writer.writeAll("\":[");
    for (items, 0..) |item, i| {
        if (i > 0) try writer.writeByte(',');
        try json_writer.writeString(writer, item);
    }
    try writer.writeByte(']');
}

fn writePropertiesSnapshot(writer: *std.Io.Writer, snapshot: ?PropertiesSnapshot) !void {
    if (snapshot) |value| {
        try writer.writeByte('{');
        inline for (@typeInfo(PropertiesSnapshot).@"struct".fields, 0..) |field, i| {
            if (i > 0) try writer.writeByte(',');
            try writer.writeByte('"');
            try writer.writeAll(field.name);
            try writer.writeAll("\":");
            switch (@typeInfo(field.type)) {
                .bool => try writer.writeAll(if (@field(value, field.name)) "true" else "false"),
                .optional => {
                    if (@field(value, field.name)) |number| {
                        try writer.print("{d}", .{number});
                    } else {
                        try writer.writeAll("null");
                    }
                },
                else => @compileError("unsupported PropertiesSnapshot field type"),
            }
        }
        try writer.writeByte('}');
        return;
    }
    try writer.writeAll("null");
}

fn parsePropertiesSnapshot(value_opt: ?std.json.Value) !?PropertiesSnapshot {
    const value = value_opt orelse return null;
    return switch (value) {
        .null => null,
        .object => |obj| .{
            .pure = getBoolOrDefault(obj, "pure", false),
            .read_only = getBoolOrDefault(obj, "read_only", false),
            .stateless = getBoolOrDefault(obj, "stateless", false),
            .retry_safe = getBoolOrDefault(obj, "retry_safe", false),
            .deterministic = getBoolOrDefault(obj, "deterministic", false),
            .has_egress = getBoolOrDefault(obj, "has_egress", false),
            .no_secret_leakage = getBoolOrDefault(obj, "no_secret_leakage", false),
            .no_credential_leakage = getBoolOrDefault(obj, "no_credential_leakage", false),
            .input_validated = getBoolOrDefault(obj, "input_validated", false),
            .pii_contained = getBoolOrDefault(obj, "pii_contained", false),
            .idempotent = getBoolOrDefault(obj, "idempotent", false),
            .max_io_depth = try getOptionalUnsignedValue(obj.get("max_io_depth")),
            .injection_safe = getBoolOrDefault(obj, "injection_safe", false),
            .state_isolated = getBoolOrDefault(obj, "state_isolated", false),
            .fault_covered = getBoolOrDefault(obj, "fault_covered", false),
            .result_safe = getBoolOrDefault(obj, "result_safe", false),
            .optional_safe = getBoolOrDefault(obj, "optional_safe", false),
        },
        else => error.InvalidUiPayload,
    };
}

fn parseDiffHunks(allocator: std.mem.Allocator, value_opt: ?std.json.Value) ![]DiffHunk {
    const value = value_opt orelse return allocator.alloc(DiffHunk, 0);
    if (value == .null) return allocator.alloc(DiffHunk, 0);
    if (value != .array) return error.InvalidUiPayload;

    const hunks = try allocator.alloc(DiffHunk, value.array.items.len);
    errdefer allocator.free(hunks);
    for (value.array.items, 0..) |item, i| {
        if (item != .object) return error.InvalidUiPayload;
        const item_obj = item.object;
        hunks[i] = .{
            .old_start = @intCast(getUnsigned(item_obj, "old_start") orelse return error.InvalidUiPayload),
            .old_count = @intCast(getUnsigned(item_obj, "old_count") orelse return error.InvalidUiPayload),
            .new_start = @intCast(getUnsigned(item_obj, "new_start") orelse return error.InvalidUiPayload),
            .new_count = @intCast(getUnsigned(item_obj, "new_count") orelse return error.InvalidUiPayload),
        };
    }
    return hunks;
}

fn parseViolationDeltaItems(allocator: std.mem.Allocator, value_opt: ?std.json.Value) ![]ViolationDeltaItem {
    const value = value_opt orelse return allocator.alloc(ViolationDeltaItem, 0);
    if (value == .null) return allocator.alloc(ViolationDeltaItem, 0);
    if (value != .array) return error.InvalidUiPayload;

    const violations = try allocator.alloc(ViolationDeltaItem, value.array.items.len);
    errdefer allocator.free(violations);
    for (violations) |*item| item.* = undefined;
    var i: usize = 0;
    errdefer {
        while (i > 0) {
            i -= 1;
            violations[i].deinit(allocator);
        }
        allocator.free(violations);
    }
    while (i < value.array.items.len) : (i += 1) {
        const item = value.array.items[i];
        if (item != .object) return error.InvalidUiPayload;
        const item_obj = item.object;
        violations[i] = try ViolationDeltaItem.init(
            allocator,
            getString(item_obj, "stable_key") orelse return error.InvalidUiPayload,
            getString(item_obj, "code") orelse return error.InvalidUiPayload,
            getString(item_obj, "severity") orelse return error.InvalidUiPayload,
            getString(item_obj, "message") orelse return error.InvalidUiPayload,
            @intCast(getUnsigned(item_obj, "line") orelse return error.InvalidUiPayload),
            @intCast(getUnsigned(item_obj, "column") orelse return error.InvalidUiPayload),
            getBool(item_obj, "introduced_by_patch") orelse return error.InvalidUiPayload,
        );
    }
    return violations;
}

fn parseProveSummary(allocator: std.mem.Allocator, value_opt: ?std.json.Value) !?ProveSummary {
    const value = value_opt orelse return null;
    return switch (value) {
        .null => null,
        .object => |obj| blk: {
            const laws_used = try parseStringArrayField(allocator, obj.get("laws_used"));
            errdefer freeStringSlice(allocator, laws_used);
            const counterexample = blk_counterexample: {
                const text = try getOptionalString(obj, "counterexample");
                break :blk_counterexample if (text) |t| try allocator.dupe(u8, t) else null;
            };
            errdefer if (counterexample) |text| allocator.free(text);
            break :blk .{
                .classification = try allocator.dupe(u8, getString(obj, "classification") orelse return error.InvalidUiPayload),
                .proof_level = try allocator.dupe(u8, getString(obj, "proof_level") orelse return error.InvalidUiPayload),
                .recommendation = try allocator.dupe(u8, getString(obj, "recommendation") orelse ""),
                .counterexample = counterexample,
                .laws_used = laws_used,
            };
        },
        else => error.InvalidUiPayload,
    };
}

fn parseSystemProofSummary(allocator: std.mem.Allocator, value_opt: ?std.json.Value) !?SystemProofSummary {
    const value = value_opt orelse return null;
    return switch (value) {
        .null => null,
        .object => |obj| blk: {
            const warnings = try parseStringArrayField(allocator, obj.get("warnings"));
            errdefer freeStringSlice(allocator, warnings);
            break :blk .{
                .system_path = try allocator.dupe(u8, getString(obj, "system_path") orelse return error.InvalidUiPayload),
                .proof_level = try allocator.dupe(u8, getString(obj, "proof_level") orelse return error.InvalidUiPayload),
                .all_links_resolved = getBoolOrDefault(obj, "all_links_resolved", false),
                .all_responses_covered = getBoolOrDefault(obj, "all_responses_covered", false),
                .payload_compatible = getBoolOrDefault(obj, "payload_compatible", false),
                .injection_safe = getBoolOrDefault(obj, "injection_safe", false),
                .no_secret_leakage = getBoolOrDefault(obj, "no_secret_leakage", false),
                .no_credential_leakage = getBoolOrDefault(obj, "no_credential_leakage", false),
                .retry_safe = getBoolOrDefault(obj, "retry_safe", false),
                .fault_covered = getBoolOrDefault(obj, "fault_covered", false),
                .state_isolated = getBoolOrDefault(obj, "state_isolated", false),
                .max_system_io_depth = try getOptionalUnsignedValue(obj.get("max_system_io_depth")),
                .dynamic_links = @intCast(getUnsignedOrDefault(obj, "dynamic_links", 0)),
                .warnings = warnings,
            };
        },
        else => error.InvalidUiPayload,
    };
}

fn parseStringArrayField(allocator: std.mem.Allocator, value_opt: ?std.json.Value) ![][]u8 {
    const value = value_opt orelse return allocator.alloc([]u8, 0);
    if (value == .null) return allocator.alloc([]u8, 0);
    if (value != .array) return error.InvalidUiPayload;

    const items = try allocator.alloc([]u8, value.array.items.len);
    errdefer allocator.free(items);
    var i: usize = 0;
    errdefer {
        while (i > 0) {
            i -= 1;
            allocator.free(items[i]);
        }
        allocator.free(items);
    }
    while (i < value.array.items.len) : (i += 1) {
        const item = value.array.items[i];
        if (item != .string) return error.InvalidUiPayload;
        items[i] = try allocator.dupe(u8, item.string);
    }
    return items;
}

fn getString(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const value = obj.get(key) orelse return null;
    return if (value == .string) value.string else null;
}

fn getOptionalString(obj: std.json.ObjectMap, key: []const u8) !?[]const u8 {
    const value = obj.get(key) orelse return null;
    return switch (value) {
        .null => null,
        .string => value.string,
        else => error.InvalidUiPayload,
    };
}

fn getBool(obj: std.json.ObjectMap, key: []const u8) ?bool {
    const value = obj.get(key) orelse return null;
    return if (value == .bool) value.bool else null;
}

fn getBoolOrDefault(obj: std.json.ObjectMap, key: []const u8, default: bool) bool {
    return getBool(obj, key) orelse default;
}

fn getInteger(obj: std.json.ObjectMap, key: []const u8) ?i64 {
    const value = obj.get(key) orelse return null;
    return if (value == .integer) value.integer else null;
}

fn getUnsigned(obj: std.json.ObjectMap, key: []const u8) ?usize {
    const value = obj.get(key) orelse return null;
    if (value != .integer or value.integer < 0) return null;
    return std.math.cast(usize, value.integer);
}

fn getUnsignedOrDefault(obj: std.json.ObjectMap, key: []const u8, default: usize) usize {
    return getUnsigned(obj, key) orelse default;
}

fn getOptionalUnsignedValue(value_opt: ?std.json.Value) !?u32 {
    const value = value_opt orelse return null;
    return switch (value) {
        .null => null,
        .integer => |integer| blk: {
            if (integer < 0) return error.InvalidUiPayload;
            break :blk std.math.cast(u32, integer) orelse return error.InvalidUiPayload;
        },
        else => error.InvalidUiPayload,
    };
}

const testing = std.testing;

fn roundTrip(allocator: std.mem.Allocator, payload: UiPayload) !UiPayload {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try writeJson(&aw.writer, payload);
    buf = aw.toArrayList();

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, buf.items, .{});
    defer parsed.deinit();
    return try parse(allocator, parsed.value);
}

test "plain_text payload round-trips" {
    var payload: UiPayload = .{ .plain_text = try testing.allocator.dupe(u8, "hello") };
    defer payload.deinit(testing.allocator);

    var roundtripped = try roundTrip(testing.allocator, payload);
    defer roundtripped.deinit(testing.allocator);

    switch (roundtripped) {
        .plain_text => |text| try testing.expectEqualStrings("hello", text),
        else => return error.TestFailed,
    }
}

test "diagnostics payload round-trips" {
    const items = try testing.allocator.alloc(DiagnosticItem, 1);
    items[0] = try DiagnosticItem.init(
        testing.allocator,
        "ZTS001",
        "error",
        "handler.ts",
        3,
        7,
        "unsupported feature",
        true,
    );
    var payload: UiPayload = .{ .diagnostics = .{
        .summary = try testing.allocator.dupe(u8, "1 violation"),
        .items = items,
    } };
    defer payload.deinit(testing.allocator);

    var roundtripped = try roundTrip(testing.allocator, payload);
    defer roundtripped.deinit(testing.allocator);

    switch (roundtripped) {
        .diagnostics => |diagnostics| {
            try testing.expectEqualStrings("1 violation", diagnostics.summary);
            try testing.expectEqual(@as(usize, 1), diagnostics.items.len);
            try testing.expectEqualStrings("ZTS001", diagnostics.items[0].code);
            try testing.expect(diagnostics.items[0].introduced_by_patch.?);
        },
        else => return error.TestFailed,
    }
}

test "proof card payload round-trips" {
    const highlights = try testing.allocator.alloc([]u8, 2);
    highlights[0] = try testing.allocator.dupe(u8, "retry_safe");
    highlights[1] = try testing.allocator.dupe(u8, "idempotent");
    var payload: UiPayload = .{ .proof_card = .{
        .title = try testing.allocator.dupe(u8, "Compiler verification"),
        .summary = try testing.allocator.dupe(u8, "No new violations"),
        .stats = .{ .total = 1, .new = 0, .preexisting = 1 },
        .highlights = highlights,
    } };
    defer payload.deinit(testing.allocator);

    var roundtripped = try roundTrip(testing.allocator, payload);
    defer roundtripped.deinit(testing.allocator);

    switch (roundtripped) {
        .proof_card => |proof| {
            try testing.expectEqualStrings("Compiler verification", proof.title);
            try testing.expectEqual(@as(u32, 1), proof.stats.total);
            try testing.expectEqual(@as(usize, 2), proof.highlights.len);
        },
        else => return error.TestFailed,
    }
}

test "command outcome payload round-trips" {
    var payload: UiPayload = .{ .command_outcome = .{
        .title = try testing.allocator.dupe(u8, "zig test"),
        .exit_code = 0,
        .stdout = try testing.allocator.dupe(u8, "ok"),
        .stderr = try testing.allocator.dupe(u8, ""),
        .command = try testing.allocator.dupe(u8, "zig build test-zigts"),
    } };
    defer payload.deinit(testing.allocator);

    var roundtripped = try roundTrip(testing.allocator, payload);
    defer roundtripped.deinit(testing.allocator);

    switch (roundtripped) {
        .command_outcome => |command| {
            try testing.expectEqualStrings("zig test", command.title);
            try testing.expectEqual(@as(?u8, 0), command.exit_code);
            try testing.expectEqualStrings("zig build test-zigts", command.command);
        },
        else => return error.TestFailed,
    }
}

test "repair candidate payload round-trips" {
    var payload: UiPayload = .{ .repair_candidate = try RepairCandidatePayload.init(
        testing.allocator,
        "handler.ts",
        "rp_001",
        "insert_guard_before_line",
        "function handler(req: Request): Response { return Response.json({ ok: true }); }",
        true,
        "0 total, 0 new, 0 preexisting",
        .{ .total = 0, .new = 0, .preexisting = 0 },
    ) };
    defer payload.deinit(testing.allocator);

    var roundtripped = try roundTrip(testing.allocator, payload);
    defer roundtripped.deinit(testing.allocator);

    switch (roundtripped) {
        .repair_candidate => |candidate| {
            try testing.expectEqualStrings("handler.ts", candidate.path);
            try testing.expectEqualStrings("rp_001", candidate.plan_id);
            try testing.expectEqualStrings("insert_guard_before_line", candidate.intent_kind);
            try testing.expect(candidate.verification_ok);
            try testing.expectEqual(@as(u32, 0), candidate.stats.new);
            try testing.expect(std.mem.indexOf(u8, candidate.proposed_content, "Response.json") != null);
        },
        else => return error.TestFailed,
    }
}

test "verified_patch payload round-trips with rich proof metadata" {
    const hunks = try testing.allocator.alloc(DiffHunk, 2);
    hunks[0] = .{ .old_start = 1, .old_count = 3, .new_start = 1, .new_count = 4 };
    hunks[1] = .{ .old_start = 9, .old_count = 1, .new_start = 10, .new_count = 2 };

    const violations = try testing.allocator.alloc(ViolationDeltaItem, 1);
    violations[0] = try ViolationDeltaItem.init(
        testing.allocator,
        "8d8f",
        "ZTS123",
        "warning",
        "possible egress expansion",
        12,
        4,
        true,
    );

    const laws_used = try testing.allocator.alloc([]u8, 2);
    laws_used[0] = try testing.allocator.dupe(u8, "contract_equivalence");
    laws_used[1] = try testing.allocator.dupe(u8, "response_covariance");

    const warnings = try testing.allocator.alloc([]u8, 1);
    warnings[0] = try testing.allocator.dupe(u8, "dynamic route fan-out not fully resolved");

    const citations = try testing.allocator.alloc([]u8, 2);
    citations[0] = try testing.allocator.dupe(u8, "ZTS204");
    citations[1] = try testing.allocator.dupe(u8, "ZTS311");

    var payload: UiPayload = .{ .verified_patch = .{
        .file = try testing.allocator.dupe(u8, "handler.ts"),
        .policy_hash = try testing.allocator.dupe(u8, "a" ** 64),
        .applied_at_unix_ms = 1700000000123,
        .stats = .{ .total = 1, .new = 0, .preexisting = 1 },
        .before = try testing.allocator.dupe(u8, "old content"),
        .after = try testing.allocator.dupe(u8, "new content"),
        .unified_diff = try testing.allocator.dupe(u8, "@@ -1,1 +1,1 @@\n-old\n+new\n"),
        .hunks = hunks,
        .violations = violations,
        .before_properties = .{
            .pure = true,
            .read_only = true,
            .stateless = true,
            .retry_safe = true,
            .deterministic = true,
            .has_egress = false,
            .no_secret_leakage = true,
            .no_credential_leakage = true,
            .input_validated = true,
            .pii_contained = true,
            .idempotent = true,
            .max_io_depth = 0,
            .injection_safe = true,
            .state_isolated = true,
            .fault_covered = true,
            .result_safe = true,
            .optional_safe = true,
        },
        .after_properties = .{
            .pure = true,
            .read_only = false,
            .stateless = true,
            .retry_safe = true,
            .deterministic = true,
            .has_egress = true,
            .no_secret_leakage = true,
            .no_credential_leakage = true,
            .input_validated = true,
            .pii_contained = true,
            .idempotent = true,
            .max_io_depth = 2,
            .state_isolated = true,
            .injection_safe = true,
            .fault_covered = false,
            .result_safe = true,
            .optional_safe = false,
        },
        .prove = .{
            .classification = try testing.allocator.dupe(u8, "additive"),
            .proof_level = try testing.allocator.dupe(u8, "partial"),
            .recommendation = try testing.allocator.dupe(u8, "review widened response surface"),
            .counterexample = try testing.allocator.dupe(u8, "POST /foo now yields 202"),
            .laws_used = laws_used,
        },
        .system = .{
            .system_path = try testing.allocator.dupe(u8, "system.json"),
            .proof_level = try testing.allocator.dupe(u8, "partial"),
            .all_links_resolved = false,
            .all_responses_covered = true,
            .payload_compatible = true,
            .injection_safe = true,
            .no_secret_leakage = true,
            .no_credential_leakage = true,
            .retry_safe = true,
            .fault_covered = false,
            .state_isolated = true,
            .max_system_io_depth = 3,
            .dynamic_links = 1,
            .warnings = warnings,
        },
        .rule_citations = citations,
        .patch_hash = blk: {
            var bytes: [32]u8 = undefined;
            for (&bytes, 0..) |*b, i| b.* = @intCast(i);
            break :blk bytes;
        },
        .parent_hash = blk: {
            var bytes: [32]u8 = undefined;
            for (&bytes, 0..) |*b, i| b.* = @intCast(31 - i);
            break :blk bytes;
        },
        .goal_context = blk: {
            const goals = try testing.allocator.alloc([]u8, 2);
            goals[0] = try testing.allocator.dupe(u8, "retry_safe");
            goals[1] = try testing.allocator.dupe(u8, "no_secret_leakage");
            break :blk goals;
        },
        .witnesses_defeated = blk: {
            const keys = try testing.allocator.alloc([]u8, 1);
            keys[0] = try testing.allocator.dupe(u8, "d" ** 64);
            break :blk keys;
        },
        .witnesses_new = blk: {
            const keys = try testing.allocator.alloc([]u8, 1);
            keys[0] = try testing.allocator.dupe(u8, "e" ** 64);
            break :blk keys;
        },
        .post_apply_ok = true,
        .post_apply_summary = try testing.allocator.dupe(u8, "post-apply verification passed"),
    } };
    defer payload.deinit(testing.allocator);

    var roundtripped = try roundTrip(testing.allocator, payload);
    defer roundtripped.deinit(testing.allocator);

    switch (roundtripped) {
        .verified_patch => |patch| {
            try testing.expectEqualStrings("handler.ts", patch.file);
            try testing.expectEqualStrings("a" ** 64, patch.policy_hash);
            try testing.expectEqual(@as(i64, 1700000000123), patch.applied_at_unix_ms);
            try testing.expectEqual(@as(u32, 1), patch.stats.total);
            try testing.expectEqual(@as(u32, 0), patch.stats.new);
            try testing.expect(patch.before != null);
            try testing.expectEqualStrings("old content", patch.before.?);
            try testing.expectEqualStrings("new content", patch.after);
            try testing.expectEqualStrings("@@ -1,1 +1,1 @@\n-old\n+new\n", patch.unified_diff);
            try testing.expectEqual(@as(usize, 2), patch.hunks.len);
            try testing.expectEqual(@as(usize, 1), patch.violations.len);
            try testing.expectEqualStrings("ZTS123", patch.violations[0].code);
            try testing.expect(patch.before_properties != null);
            try testing.expect(patch.after_properties != null);
            try testing.expect(patch.before_properties.?.read_only);
            try testing.expect(patch.after_properties.?.pure);
            try testing.expect(!patch.after_properties.?.read_only);
            try testing.expect(patch.after_properties.?.retry_safe);
            try testing.expect(!patch.after_properties.?.fault_covered);
            try testing.expectEqual(@as(?u32, 2), patch.after_properties.?.max_io_depth);
            try testing.expect(patch.prove != null);
            try testing.expectEqualStrings("additive", patch.prove.?.classification);
            try testing.expectEqual(@as(usize, 2), patch.prove.?.laws_used.len);
            try testing.expect(patch.system != null);
            try testing.expectEqualStrings("system.json", patch.system.?.system_path);
            try testing.expectEqual(@as(usize, 2), patch.rule_citations.len);
            try testing.expect(patch.post_apply_ok);
            try testing.expect(patch.post_apply_summary != null);
            try testing.expectEqualStrings("post-apply verification passed", patch.post_apply_summary.?);
            try testing.expect(patch.patch_hash != null);
            try testing.expect(patch.parent_hash != null);
            try testing.expectEqual(@as(u8, 0), patch.patch_hash.?[0]);
            try testing.expectEqual(@as(u8, 31), patch.patch_hash.?[31]);
            try testing.expectEqual(@as(u8, 31), patch.parent_hash.?[0]);
            try testing.expectEqual(@as(u8, 0), patch.parent_hash.?[31]);
            try testing.expectEqual(@as(usize, 2), patch.goal_context.len);
            try testing.expectEqualStrings("retry_safe", patch.goal_context[0]);
            try testing.expectEqualStrings("no_secret_leakage", patch.goal_context[1]);
            try testing.expectEqual(@as(usize, 1), patch.witnesses_defeated.len);
            try testing.expectEqualStrings("d" ** 64, patch.witnesses_defeated[0]);
            try testing.expectEqual(@as(usize, 1), patch.witnesses_new.len);
            try testing.expectEqualStrings("e" ** 64, patch.witnesses_new[0]);
        },
        else => return error.TestFailed,
    }
}

test "verified_patch chain metadata defaults when omitted from JSON" {
    const json =
        \\{
        \\  "kind":"verified_patch",
        \\  "file":"new.ts",
        \\  "policy_hash":"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
        \\  "stats":{"total":0,"new":0},
        \\  "before":null,
        \\  "after":"export default {}",
        \\  "post_apply_ok":true
        \\}
    ;
    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, json, .{});
    defer parsed.deinit();

    var payload = try parse(testing.allocator, parsed.value);
    defer payload.deinit(testing.allocator);

    switch (payload) {
        .verified_patch => |patch| {
            try testing.expect(patch.patch_hash == null);
            try testing.expect(patch.parent_hash == null);
            try testing.expectEqual(@as(usize, 0), patch.goal_context.len);
            try testing.expectEqual(@as(usize, 0), patch.witnesses_defeated.len);
            try testing.expectEqual(@as(usize, 0), patch.witnesses_new.len);
        },
        else => return error.TestFailed,
    }
}

test "verified_patch parser rejects malformed patch_hash hex" {
    const json =
        \\{
        \\  "kind":"verified_patch",
        \\  "file":"new.ts",
        \\  "policy_hash":"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
        \\  "stats":{"total":0,"new":0},
        \\  "before":null,
        \\  "after":"export default {}",
        \\  "post_apply_ok":true,
        \\  "patch_hash":"not-a-hex-string"
        \\}
    ;
    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, json, .{});
    defer parsed.deinit();

    try testing.expectError(error.InvalidUiPayload, parse(testing.allocator, parsed.value));
}

test "verified_patch parser accepts legacy minimal schema" {
    const json =
        \\{
        \\  "kind":"verified_patch",
        \\  "file":"new.ts",
        \\  "policy_hash":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
        \\  "stats":{"total":0,"new":0},
        \\  "before":null,
        \\  "after":"export default {}",
        \\  "after_properties":{"pure":true,"retry_safe":true,"idempotent":true,"state_isolated":true,"injection_safe":true},
        \\  "post_apply_ok":false,
        \\  "post_apply_summary":"verify_paths regressed"
        \\}
    ;
    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, json, .{});
    defer parsed.deinit();

    var payload = try parse(testing.allocator, parsed.value);
    defer payload.deinit(testing.allocator);

    switch (payload) {
        .verified_patch => |patch| {
            try testing.expectEqual(@as(i64, 0), patch.applied_at_unix_ms);
            try testing.expectEqualStrings("", patch.unified_diff);
            try testing.expectEqual(@as(usize, 0), patch.hunks.len);
            try testing.expectEqual(@as(usize, 0), patch.violations.len);
            try testing.expect(patch.before == null);
            try testing.expect(patch.before_properties == null);
            try testing.expect(patch.after_properties != null);
            try testing.expect(patch.after_properties.?.pure);
            try testing.expect(!patch.after_properties.?.read_only);
            try testing.expectEqual(@as(?u32, null), patch.after_properties.?.max_io_depth);
            try testing.expect(patch.prove == null);
            try testing.expect(patch.system == null);
            try testing.expectEqual(@as(usize, 0), patch.rule_citations.len);
            try testing.expect(!patch.post_apply_ok);
            try testing.expectEqualStrings("verify_paths regressed", patch.post_apply_summary.?);
        },
        else => return error.TestFailed,
    }
}

test "session tree payload round-trips" {
    const nodes = try testing.allocator.alloc(SessionTreeNode, 2);
    nodes[0] = .{
        .session_id = try testing.allocator.dupe(u8, "root"),
        .parent_id = null,
        .created_at_unix_ms = 1,
        .depth = 0,
        .is_current = false,
        .is_orphan_root = false,
    };
    nodes[1] = .{
        .session_id = try testing.allocator.dupe(u8, "child"),
        .parent_id = try testing.allocator.dupe(u8, "root"),
        .created_at_unix_ms = 2,
        .depth = 1,
        .is_current = true,
        .is_orphan_root = false,
    };
    var payload: UiPayload = .{ .session_tree = .{ .nodes = nodes } };
    defer payload.deinit(testing.allocator);

    var roundtripped = try roundTrip(testing.allocator, payload);
    defer roundtripped.deinit(testing.allocator);

    switch (roundtripped) {
        .session_tree => |tree| {
            try testing.expectEqual(@as(usize, 2), tree.nodes.len);
            try testing.expectEqualStrings("child", tree.nodes[1].session_id);
            try testing.expect(tree.nodes[1].is_current);
        },
        else => return error.TestFailed,
    }
}
