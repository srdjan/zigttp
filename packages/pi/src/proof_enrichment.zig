const std = @import("std");
const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const transcript_mod = @import("transcript.zig");
const ui_payload = @import("ui_payload.zig");
const tools_common = @import("tools/common.zig");

const edit_simulate = zigts_cli.edit_simulate;
const precompile = zigts_cli.precompile;
const system_analysis = zigts_cli.system_analysis;
const contract_diff = zigts.contract_diff;
const HandlerProperties = zigts.handler_contract.HandlerProperties;

pub const PatchAnalysis = struct {
    stats: ui_payload.ProofStats,
    violations: []ui_payload.ViolationDeltaItem,
    before_properties: ?ui_payload.PropertiesSnapshot,
    after_properties: ?ui_payload.PropertiesSnapshot,
    prove: ?ui_payload.ProveSummary,
    system: ?ui_payload.SystemProofSummary,
    rule_citations: [][]u8,
    unified_diff: []u8,
    hunks: []ui_payload.DiffHunk,

    pub fn deinit(self: *PatchAnalysis, allocator: std.mem.Allocator) void {
        for (self.violations) |*item| item.deinit(allocator);
        allocator.free(self.violations);
        if (self.prove) |*prove| prove.deinit(allocator);
        if (self.system) |*system| system.deinit(allocator);
        for (self.rule_citations) |citation| allocator.free(citation);
        allocator.free(self.rule_citations);
        allocator.free(self.unified_diff);
        allocator.free(self.hunks);
        self.* = .{
            .stats = .{ .total = 0, .new = 0, .preexisting = null },
            .violations = &.{},
            .before_properties = null,
            .after_properties = null,
            .prove = null,
            .system = null,
            .rule_citations = &.{},
            .unified_diff = &.{},
            .hunks = &.{},
        };
    }
};

pub const BuildVerifiedPatchOptions = struct {
    workspace_root: []const u8,
    file: []const u8,
    before: ?[]const u8,
    after: []const u8,
    policy_hash: []const u8,
    applied_at_unix_ms: i64,
    post_apply_ok: bool,
    post_apply_summary: ?[]const u8 = null,
    transcript: ?*const transcript_mod.Transcript = null,
    repair_plan_ids: []const []const u8 = &.{},
    /// If null, buildVerifiedPatchPayload computes a patch_hash from
    /// sha256(normalized_before ++ "\x00" ++ unified_diff). Callers that
    /// already know the hash (the autoloop orchestrator caches it across
    /// turns) can supply it directly to avoid a redundant hash pass.
    patch_hash: ?[32]u8 = null,
    parent_hash: ?[32]u8 = null,
    goal_context: []const []const u8 = &.{},
    witnesses_defeated: []const ui_payload.WitnessBody = &.{},
    witnesses_new: []const ui_payload.WitnessBody = &.{},
};

pub fn buildVerifiedPatchPayload(
    allocator: std.mem.Allocator,
    options: BuildVerifiedPatchOptions,
) !ui_payload.VerifiedPatchPayload {
    var analysis = try analyzePatch(
        allocator,
        options.workspace_root,
        options.file,
        options.before,
        options.after,
        options.transcript,
    );
    errdefer analysis.deinit(allocator);

    const file_copy = try allocator.dupe(u8, options.file);
    errdefer allocator.free(file_copy);
    const policy_copy = try allocator.dupe(u8, options.policy_hash);
    errdefer allocator.free(policy_copy);
    const before_copy: ?[]u8 = if (options.before) |before|
        try allocator.dupe(u8, before)
    else
        null;
    errdefer if (before_copy) |before| allocator.free(before);
    const after_copy = try allocator.dupe(u8, options.after);
    errdefer allocator.free(after_copy);
    const post_apply_summary_copy: ?[]u8 = if (options.post_apply_summary) |summary|
        try allocator.dupe(u8, summary)
    else
        null;
    errdefer if (post_apply_summary_copy) |summary| allocator.free(summary);
    const repair_plan_ids_copy = try cloneStringSlice(allocator, options.repair_plan_ids);
    errdefer freeStringSlice(allocator, repair_plan_ids_copy);
    const goal_context_copy = try cloneStringSlice(allocator, options.goal_context);
    errdefer freeStringSlice(allocator, goal_context_copy);
    const witnesses_defeated_copy = try ui_payload.cloneWitnessBodySlice(allocator, options.witnesses_defeated);
    errdefer ui_payload.freeWitnessBodySlice(allocator, witnesses_defeated_copy);
    const witnesses_new_copy = try ui_payload.cloneWitnessBodySlice(allocator, options.witnesses_new);
    errdefer ui_payload.freeWitnessBodySlice(allocator, witnesses_new_copy);

    const patch_hash_value: [32]u8 = options.patch_hash orelse computePatchHash(options.before, analysis.unified_diff);

    const payload = ui_payload.VerifiedPatchPayload{
        .file = file_copy,
        .policy_hash = policy_copy,
        .applied_at_unix_ms = options.applied_at_unix_ms,
        .stats = analysis.stats,
        .before = before_copy,
        .after = after_copy,
        .unified_diff = analysis.unified_diff,
        .hunks = analysis.hunks,
        .violations = analysis.violations,
        .before_properties = analysis.before_properties,
        .after_properties = analysis.after_properties,
        .prove = analysis.prove,
        .system = analysis.system,
        .rule_citations = analysis.rule_citations,
        .repair_plan_ids = repair_plan_ids_copy,
        .patch_hash = patch_hash_value,
        .parent_hash = options.parent_hash,
        .goal_context = goal_context_copy,
        .witnesses_defeated = witnesses_defeated_copy,
        .witnesses_new = witnesses_new_copy,
        .post_apply_ok = options.post_apply_ok,
        .post_apply_summary = post_apply_summary_copy,
    };
    analysis.unified_diff = &.{};
    analysis.hunks = &.{};
    analysis.violations = &.{};
    analysis.prove = null;
    analysis.system = null;
    analysis.rule_citations = &.{};
    return payload;
}

/// Compute a deterministic content hash for chaining VerifiedPatch events.
///
/// The hash keys the "before" source (empty for root patches on new files)
/// together with the unified diff. Using the diff rather than the "after"
/// source means a cosmetic formatter pass that rewrites the whole file but
/// produces an empty diff yields the same hash as its parent - chain
/// continuity is preserved across reformatting.
pub fn computePatchHash(before: ?[]const u8, unified_diff: []const u8) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    if (before) |bytes| hasher.update(bytes);
    hasher.update("\x00");
    hasher.update(unified_diff);
    var out: [32]u8 = undefined;
    hasher.final(&out);
    return out;
}

fn cloneStringSlice(allocator: std.mem.Allocator, items: []const []const u8) ![][]u8 {
    if (items.len == 0) return &.{};
    const copy = try allocator.alloc([]u8, items.len);
    errdefer allocator.free(copy);
    var i: usize = 0;
    errdefer {
        while (i > 0) {
            i -= 1;
            allocator.free(copy[i]);
        }
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

pub fn analyzePatch(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
    file: []const u8,
    before: ?[]const u8,
    after: []const u8,
    transcript: ?*const transcript_mod.Transcript,
) !PatchAnalysis {
    const absolute_path = try resolveHandlerPath(allocator, workspace_root, file);
    defer allocator.free(absolute_path);

    const system_path = try discoverSystemPath(allocator, workspace_root, absolute_path);
    defer if (system_path) |path| allocator.free(path);

    var simulated = try edit_simulate.simulate(allocator, .{
        .file = absolute_path,
        .content = after,
        .before = before,
    });
    defer simulated.deinit(allocator);

    const violations = try copyViolations(allocator, simulated.violations.items);
    errdefer {
        for (violations) |*item| item.deinit(allocator);
        allocator.free(violations);
    }

    const before_snapshot = try optionalPropertiesSnapshot(allocator, absolute_path, before, system_path);
    const after_snapshot = if (simulated.properties) |properties|
        propertiesSnapshot(properties)
    else
        try optionalPropertiesSnapshot(allocator, absolute_path, after, system_path);

    var prove = try optionalProveSummary(allocator, absolute_path, before, after, system_path);
    errdefer if (prove) |*summary| summary.deinit(allocator);
    var system = try optionalSystemSummary(allocator, system_path);
    errdefer if (system) |*summary| summary.deinit(allocator);
    const citations = if (transcript) |tr|
        try collectRuleCitations(allocator, tr)
    else
        try allocator.alloc([]u8, 0);
    errdefer {
        for (citations) |citation| allocator.free(citation);
        allocator.free(citations);
    }

    const diff = try buildUnifiedDiff(allocator, before, after);
    errdefer {
        allocator.free(diff.text);
        allocator.free(diff.hunks);
    }

    return .{
        .stats = .{
            .total = simulated.total,
            .new = simulated.new_count,
            .preexisting = simulated.preexisting_count,
        },
        .violations = violations,
        .before_properties = before_snapshot,
        .after_properties = after_snapshot,
        .prove = prove,
        .system = system,
        .rule_citations = citations,
        .unified_diff = diff.text,
        .hunks = diff.hunks,
    };
}

pub fn loadProveSummaryForPatch(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
    file: []const u8,
    before: ?[]const u8,
    after: []const u8,
) !?ui_payload.ProveSummary {
    const absolute_path = try resolveHandlerPath(allocator, workspace_root, file);
    defer allocator.free(absolute_path);
    const system_path = try discoverSystemPath(allocator, workspace_root, absolute_path);
    defer if (system_path) |path| allocator.free(path);
    return computeProveSummary(allocator, absolute_path, before, after, system_path);
}

pub fn loadSystemSummary(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
    explicit_system: ?[]const u8,
    discovery_hint: ?[]const u8,
) !?ui_payload.SystemProofSummary {
    const resolved_system = if (explicit_system) |path|
        try tools_common.resolveInsideWorkspace(allocator, workspace_root, path)
    else if (discovery_hint) |hint| blk: {
        const absolute_hint = try tools_common.resolveInsideWorkspace(allocator, workspace_root, hint);
        defer allocator.free(absolute_hint);
        break :blk try discoverSystemPath(allocator, workspace_root, absolute_hint);
    } else try discoverSystemPath(allocator, workspace_root, workspace_root);
    defer if (resolved_system) |path| allocator.free(path);

    return computeSystemSummary(allocator, resolved_system);
}

pub fn formatProveSummary(
    allocator: std.mem.Allocator,
    summary: ?ui_payload.ProveSummary,
) ![]u8 {
    if (summary == null) return allocator.dupe(u8, "prove: unavailable\n");

    const value = summary.?;
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.print(
        "classification: {s}\nproof_level: {s}\nrecommendation: {s}\n",
        .{ value.classification, value.proof_level, value.recommendation },
    );
    if (value.counterexample) |counterexample| {
        try w.print("counterexample: {s}\n", .{counterexample});
    }
    if (value.laws_used.len > 0) {
        try w.writeAll("laws_used:\n");
        for (value.laws_used) |law| {
            try w.print("- {s}\n", .{law});
        }
    }

    buf = aw.toArrayList();
    return buf.toOwnedSlice(allocator);
}

pub fn formatSystemSummary(
    allocator: std.mem.Allocator,
    summary: ?ui_payload.SystemProofSummary,
) ![]u8 {
    if (summary == null) return allocator.dupe(u8, "system proof: unavailable\n");

    const value = summary.?;
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.print(
        "system: {s}\nproof_level: {s}\nall_links_resolved: {s}\nall_responses_covered: {s}\npayload_compatible: {s}\ninjection_safe: {s}\nno_secret_leakage: {s}\nno_credential_leakage: {s}\nretry_safe: {s}\nfault_covered: {s}\nstate_isolated: {s}\n",
        .{
            value.system_path,
            value.proof_level,
            boolString(value.all_links_resolved),
            boolString(value.all_responses_covered),
            boolString(value.payload_compatible),
            boolString(value.injection_safe),
            boolString(value.no_secret_leakage),
            boolString(value.no_credential_leakage),
            boolString(value.retry_safe),
            boolString(value.fault_covered),
            boolString(value.state_isolated),
        },
    );
    if (value.max_system_io_depth) |depth| {
        try w.print("max_system_io_depth: {d}\n", .{depth});
    }
    try w.print("dynamic_links: {d}\n", .{value.dynamic_links});
    if (value.warnings.len > 0) {
        try w.writeAll("warnings:\n");
        for (value.warnings) |warning| {
            try w.print("- {s}\n", .{warning});
        }
    }

    buf = aw.toArrayList();
    return buf.toOwnedSlice(allocator);
}

pub fn propertiesSnapshot(properties: HandlerProperties) ui_payload.PropertiesSnapshot {
    return .{
        .pure = properties.pure,
        .read_only = properties.read_only,
        .stateless = properties.stateless,
        .retry_safe = properties.retry_safe,
        .deterministic = properties.deterministic,
        .has_egress = properties.has_egress,
        .no_secret_leakage = properties.no_secret_leakage,
        .no_credential_leakage = properties.no_credential_leakage,
        .input_validated = properties.input_validated,
        .pii_contained = properties.pii_contained,
        .idempotent = properties.idempotent,
        .max_io_depth = properties.max_io_depth,
        .injection_safe = properties.injection_safe,
        .state_isolated = properties.state_isolated,
        .fault_covered = properties.fault_covered,
        .result_safe = properties.result_safe,
        .optional_safe = properties.optional_safe,
    };
}

fn loadPropertiesSnapshot(
    allocator: std.mem.Allocator,
    absolute_path: []const u8,
    source: ?[]const u8,
    system_path: ?[]const u8,
) !?ui_payload.PropertiesSnapshot {
    const content = source orelse return null;
    var result = try precompile.runCheckOnlyFromSource(
        allocator,
        content,
        absolute_path,
        null,
        true,
        system_path,
        false,
    );
    defer result.deinit(allocator);

    if (result.properties) |properties| {
        return propertiesSnapshot(properties);
    }
    return null;
}

fn optionalPropertiesSnapshot(
    allocator: std.mem.Allocator,
    absolute_path: []const u8,
    source: ?[]const u8,
    system_path: ?[]const u8,
) !?ui_payload.PropertiesSnapshot {
    return loadPropertiesSnapshot(allocator, absolute_path, source, system_path) catch |err| switch (err) {
        error.OutOfMemory => return err,
        else => null,
    };
}

fn computeProveSummary(
    allocator: std.mem.Allocator,
    absolute_path: []const u8,
    before: ?[]const u8,
    after: []const u8,
    system_path: ?[]const u8,
) !?ui_payload.ProveSummary {
    const before_source = before orelse return null;

    var before_result = try precompile.runCheckOnlyFromSource(
        allocator,
        before_source,
        absolute_path,
        null,
        true,
        system_path,
        false,
    );
    defer before_result.deinit(allocator);

    var after_result = try precompile.runCheckOnlyFromSource(
        allocator,
        after,
        absolute_path,
        null,
        true,
        system_path,
        false,
    );
    defer after_result.deinit(allocator);

    if (before_result.contract) |*before_contract| {
        if (after_result.contract) |*after_contract| {
            var diff = try contract_diff.diffContracts(allocator, before_contract, after_contract);
            defer diff.deinit(allocator);
            const classification = diff.classify();
            const recommendation = try contract_diff.generateRecommendation(allocator, classification, &diff, null);
            defer allocator.free(recommendation);
            const laws = try allocator.alloc([]u8, diff.laws_used.items.len);
            errdefer allocator.free(laws);
            var i: usize = 0;
            errdefer {
                while (i > 0) {
                    i -= 1;
                    allocator.free(laws[i]);
                }
                allocator.free(laws);
            }
            while (i < diff.laws_used.items.len) : (i += 1) {
                laws[i] = try allocator.dupe(u8, diff.laws_used.items[i]);
            }

            return .{
                .classification = try allocator.dupe(u8, classification.toString()),
                .proof_level = try allocator.dupe(u8, contract_diff.deriveProofLevel(after_contract).toString()),
                .recommendation = try allocator.dupe(u8, recommendation),
                .counterexample = if (diff.canonical_counterexample) |counterexample|
                    try allocator.dupe(u8, counterexample)
                else
                    null,
                .laws_used = laws,
            };
        }
    }

    return null;
}

fn optionalProveSummary(
    allocator: std.mem.Allocator,
    absolute_path: []const u8,
    before: ?[]const u8,
    after: []const u8,
    system_path: ?[]const u8,
) !?ui_payload.ProveSummary {
    return computeProveSummary(allocator, absolute_path, before, after, system_path) catch |err| switch (err) {
        error.OutOfMemory => return err,
        else => null,
    };
}

fn computeSystemSummary(
    allocator: std.mem.Allocator,
    system_path: ?[]const u8,
) !?ui_payload.SystemProofSummary {
    const path = system_path orelse return null;
    var compiled = system_analysis.loadCompiledSystem(allocator, path) catch |err| switch (err) {
        error.EmptySystemConfig,
        error.SystemHandlerCompilationFailed,
        error.MissingHandlerContract,
        error.FileNotFound,
        => return null,
        else => return err,
    };
    defer compiled.deinit(allocator);

    const warnings = try allocator.alloc([]u8, compiled.analysis.warnings.items.len);
    errdefer allocator.free(warnings);
    var i: usize = 0;
    errdefer {
        while (i > 0) {
            i -= 1;
            allocator.free(warnings[i]);
        }
        allocator.free(warnings);
    }
    while (i < compiled.analysis.warnings.items.len) : (i += 1) {
        warnings[i] = try allocator.dupe(u8, compiled.analysis.warnings.items[i]);
    }

    return .{
        .system_path = try allocator.dupe(u8, path),
        .proof_level = try allocator.dupe(u8, systemProofLevelString(compiled.analysis.proof_level)),
        .all_links_resolved = compiled.analysis.properties.all_links_resolved,
        .all_responses_covered = compiled.analysis.properties.all_responses_covered,
        .payload_compatible = compiled.analysis.properties.payload_compatible,
        .injection_safe = compiled.analysis.properties.injection_safe,
        .no_secret_leakage = compiled.analysis.properties.no_secret_leakage,
        .no_credential_leakage = compiled.analysis.properties.no_credential_leakage,
        .retry_safe = compiled.analysis.properties.retry_safe,
        .fault_covered = compiled.analysis.properties.fault_covered,
        .state_isolated = compiled.analysis.properties.state_isolated,
        .max_system_io_depth = compiled.analysis.properties.max_system_io_depth,
        .dynamic_links = compiled.analysis.dynamic_links,
        .warnings = warnings,
    };
}

fn optionalSystemSummary(
    allocator: std.mem.Allocator,
    system_path: ?[]const u8,
) !?ui_payload.SystemProofSummary {
    return computeSystemSummary(allocator, system_path) catch |err| switch (err) {
        error.OutOfMemory => return err,
        else => null,
    };
}

fn discoverSystemPath(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
    absolute_path: []const u8,
) !?[]u8 {
    const root = try normalizeWorkspaceRoot(allocator, workspace_root);
    defer allocator.free(root);

    var current = if (std.mem.eql(u8, absolute_path, root))
        try allocator.dupe(u8, root)
    else
        try allocator.dupe(u8, std.fs.path.dirname(absolute_path) orelse root);
    defer allocator.free(current);

    while (true) {
        if (!tools_common.isPathInsideRoot(root, current)) break;

        const manifest_path = try std.fs.path.join(allocator, &.{ current, "zigttp.json" });
        defer allocator.free(manifest_path);
        if (try pathExists(allocator, manifest_path)) {
            if (try systemPathFromManifest(allocator, current, manifest_path)) |resolved| return resolved;
        }

        const default_system = try std.fs.path.join(allocator, &.{ current, "system.json" });
        if (try pathExists(allocator, default_system)) {
            return default_system;
        }
        allocator.free(default_system);

        if (std.mem.eql(u8, current, root)) break;
        const parent = std.fs.path.dirname(current) orelse break;
        if (parent.len == current.len) break;
        if (!tools_common.isPathInsideRoot(root, parent)) break;
        const next = try allocator.dupe(u8, parent);
        allocator.free(current);
        current = next;
    }

    return null;
}

fn resolveHandlerPath(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
    file: []const u8,
) ![]u8 {
    const root = try normalizeWorkspaceRoot(allocator, workspace_root);
    defer allocator.free(root);
    return tools_common.resolveInsideWorkspace(allocator, root, file);
}

fn normalizeWorkspaceRoot(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
) ![]u8 {
    if (std.fs.path.isAbsolute(workspace_root)) {
        return std.fs.path.resolve(allocator, &.{workspace_root});
    }

    const cwd_real = tools_common.realCwd(allocator) catch {
        return std.fs.path.resolve(allocator, &.{workspace_root});
    };
    defer allocator.free(cwd_real);

    return std.fs.path.resolve(allocator, &.{ cwd_real, workspace_root });
}

fn pathExists(allocator: std.mem.Allocator, absolute_path: []const u8) !bool {
    if (!std.fs.path.isAbsolute(absolute_path)) return false;
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    std.Io.Dir.accessAbsolute(io_backend.io(), absolute_path, .{}) catch |err| switch (err) {
        error.FileNotFound => return false,
        else => return false,
    };
    return true;
}

fn systemPathFromManifest(
    allocator: std.mem.Allocator,
    dir_path: []const u8,
    manifest_path: []const u8,
) !?[]u8 {
    const bytes = zigts.file_io.readFile(allocator, manifest_path, 256 * 1024) catch return null;
    defer allocator.free(bytes);

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, bytes, .{}) catch return null;
    defer parsed.deinit();
    if (parsed.value != .object) return null;

    if (parsed.value.object.get("system")) |value| {
        if (value == .string) {
            return try std.fs.path.resolve(allocator, &.{ dir_path, value.string });
        }
    }
    return null;
}

fn collectRuleCitations(
    allocator: std.mem.Allocator,
    transcript: *const transcript_mod.Transcript,
) ![][]u8 {
    const start = blk: {
        var i = transcript.len();
        while (i > 0) {
            i -= 1;
            switch (transcript.at(i).*) {
                .user_text => break :blk i,
                else => {},
            }
        }
        break :blk 0;
    };

    var set: std.StringHashMapUnmanaged(void) = .empty;
    defer {
        var it = set.iterator();
        while (it.next()) |entry| allocator.free(entry.key_ptr.*);
        set.deinit(allocator);
    }

    var i = start;
    while (i < transcript.len()) : (i += 1) {
        const entry = transcript.at(i);
        switch (entry.*) {
            .model_text => |body| try scanRuleCodes(allocator, body, &set),
            else => {},
        }
    }

    const out = try allocator.alloc([]u8, set.count());
    errdefer allocator.free(out);
    var it = set.iterator();
    var index: usize = 0;
    while (it.next()) |entry| : (index += 1) {
        out[index] = try allocator.dupe(u8, entry.key_ptr.*);
    }
    std.mem.sort([]u8, out, {}, lessThanString);
    return out;
}

fn scanRuleCodes(
    allocator: std.mem.Allocator,
    text: []const u8,
    set: *std.StringHashMapUnmanaged(void),
) !void {
    var i: usize = 0;
    while (i + 6 <= text.len) : (i += 1) {
        if (!std.mem.eql(u8, text[i .. i + 3], "ZTS")) continue;
        if (!std.ascii.isDigit(text[i + 3]) or !std.ascii.isDigit(text[i + 4]) or !std.ascii.isDigit(text[i + 5])) {
            continue;
        }
        const code = text[i .. i + 6];
        if (set.contains(code)) continue;
        try set.put(allocator, try allocator.dupe(u8, code), {});
    }
}

const UnifiedDiff = struct {
    text: []u8,
    hunks: []ui_payload.DiffHunk,
};

fn buildUnifiedDiff(
    allocator: std.mem.Allocator,
    before: ?[]const u8,
    after: []const u8,
) !UnifiedDiff {
    const before_text = before orelse "";
    if (std.mem.eql(u8, before_text, after)) {
        return .{
            .text = try allocator.alloc(u8, 0),
            .hunks = try allocator.alloc(ui_payload.DiffHunk, 0),
        };
    }

    const before_line_count = lineCount(before_text);
    const after_line_count = lineCount(after);
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.print(
        "@@ -{d},{d} +{d},{d} @@\n",
        .{
            if (before_line_count == 0) @as(u32, 0) else @as(u32, 1),
            before_line_count,
            if (after_line_count == 0) @as(u32, 0) else @as(u32, 1),
            after_line_count,
        },
    );
    try appendPrefixedLines(w, '-', before_text);
    try appendPrefixedLines(w, '+', after);

    buf = aw.toArrayList();
    const hunks = try allocator.alloc(ui_payload.DiffHunk, 1);
    hunks[0] = .{
        .old_start = if (before_line_count == 0) 0 else 1,
        .old_count = before_line_count,
        .new_start = if (after_line_count == 0) 0 else 1,
        .new_count = after_line_count,
    };
    return .{
        .text = try buf.toOwnedSlice(allocator),
        .hunks = hunks,
    };
}

fn appendPrefixedLines(
    w: *std.Io.Writer,
    prefix: u8,
    text: []const u8,
) !void {
    if (text.len == 0) return;
    var start: usize = 0;
    var i: usize = 0;
    while (i < text.len) : (i += 1) {
        if (text[i] != '\n') continue;
        try w.writeByte(prefix);
        try w.writeAll(text[start .. i + 1]);
        start = i + 1;
    }
    if (start < text.len) {
        try w.writeByte(prefix);
        try w.writeAll(text[start..]);
        try w.writeByte('\n');
    }
}

fn lineCount(text: []const u8) u32 {
    if (text.len == 0) return 0;
    var count: u32 = @intCast(std.mem.count(u8, text, "\n"));
    if (text[text.len - 1] != '\n') count += 1;
    return count;
}

fn copyViolations(
    allocator: std.mem.Allocator,
    violations: []const edit_simulate.SimulatedViolation,
) ![]ui_payload.ViolationDeltaItem {
    const out = try allocator.alloc(ui_payload.ViolationDeltaItem, violations.len);
    errdefer allocator.free(out);
    for (out) |*item| item.* = undefined;
    var i: usize = 0;
    errdefer {
        while (i > 0) {
            i -= 1;
            out[i].deinit(allocator);
        }
        allocator.free(out);
    }
    while (i < violations.len) : (i += 1) {
        const stable_key = try stableViolationKey(allocator, violations[i].code, violations[i].message);
        defer allocator.free(stable_key);
        out[i] = try ui_payload.ViolationDeltaItem.init(
            allocator,
            stable_key,
            violations[i].code,
            violations[i].severity,
            violations[i].message,
            violations[i].line,
            violations[i].column,
            violations[i].introduced_by_patch,
        );
    }
    return out;
}

fn stableViolationKey(
    allocator: std.mem.Allocator,
    code: []const u8,
    message: []const u8,
) ![]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(code);
    hasher.update("\x00");
    hasher.update(message);
    var digest: [std.crypto.hash.sha2.Sha256.digest_length]u8 = undefined;
    hasher.final(&digest);

    const out = try allocator.alloc(u8, digest.len * 2);
    @memcpy(out, &std.fmt.bytesToHex(digest, .lower));
    return out;
}

fn systemProofLevelString(level: zigts.system_linker.ProofLevel) []const u8 {
    return switch (level) {
        .complete => "complete",
        .partial => "partial",
        .none => "none",
    };
}

fn boolString(value: bool) []const u8 {
    return if (value) "true" else "false";
}

fn lessThanString(_: void, a: []u8, b: []u8) bool {
    return std.mem.lessThan(u8, a, b);
}

const testing = std.testing;
const IsolatedTmp = @import("test_support/tmp.zig").IsolatedTmp;

fn initTmp(allocator: std.mem.Allocator) !IsolatedTmp {
    return IsolatedTmp.init(allocator, "proof-enrichment");
}

test "propertiesSnapshot maps full handler properties surface" {
    const snapshot = propertiesSnapshot(.{
        .pure = true,
        .read_only = false,
        .stateless = true,
        .retry_safe = true,
        .deterministic = true,
        .has_egress = false,
        .no_secret_leakage = true,
        .no_credential_leakage = true,
        .input_validated = true,
        .pii_contained = true,
        .idempotent = true,
        .max_io_depth = 2,
        .injection_safe = true,
        .state_isolated = true,
        .fault_covered = false,
        .result_safe = true,
        .optional_safe = false,
    });

    try testing.expect(snapshot.stateless);
    try testing.expectEqual(@as(?u32, 2), snapshot.max_io_depth);
    try testing.expect(snapshot.result_safe);
    try testing.expect(!snapshot.optional_safe);
}

test "resolveHandlerPath rejects absolute paths outside workspace" {
    try testing.expectError(
        error.PathOutsideWorkspace,
        resolveHandlerPath(testing.allocator, "/tmp/zigttp-proof-workspace", "/etc/passwd"),
    );
}

test "discoverSystemPath finds system config at workspace root" {
    var tmp = try initTmp(testing.allocator);
    defer tmp.cleanup(testing.allocator);

    try tmp.writeFile(testing.allocator, "system.json", "{}");

    const system_path = try discoverSystemPath(testing.allocator, tmp.abs_path, tmp.abs_path);
    defer if (system_path) |path| testing.allocator.free(path);

    try testing.expect(system_path != null);
    const expected = try tmp.childPath(testing.allocator, "system.json");
    defer testing.allocator.free(expected);
    try testing.expectEqualStrings(expected, system_path.?);
}

test "discoverSystemPath does not climb above workspace root" {
    var tmp = try initTmp(testing.allocator);
    defer tmp.cleanup(testing.allocator);

    try tmp.writeFile(testing.allocator, "outer/system.json", "{}");
    try tmp.mkdir(testing.allocator, "outer/workspace/src");

    const workspace_root = try tmp.childPath(testing.allocator, "outer/workspace");
    defer testing.allocator.free(workspace_root);
    const handler_path = try tmp.childPath(testing.allocator, "outer/workspace/src/handler.ts");
    defer testing.allocator.free(handler_path);

    const system_path = try discoverSystemPath(testing.allocator, workspace_root, handler_path);
    defer if (system_path) |path| testing.allocator.free(path);

    try testing.expect(system_path == null);
}

test "analyzePatch keeps enrichment best-effort for broken system manifests" {
    var tmp = try initTmp(testing.allocator);
    defer tmp.cleanup(testing.allocator);

    try tmp.writeFile(testing.allocator, "system.json", "{not json");

    var analysis = try analyzePatch(
        testing.allocator,
        tmp.abs_path,
        "handler.ts",
        "function handler(req: Request): Response { return Response.json({ ok: true }); }",
        "function handler(req: Request): Response { return Response.json({ ok: true, v: 2 }); }",
        null,
    );
    defer analysis.deinit(testing.allocator);

    try testing.expect(analysis.system == null);
}

test "analyzePatch returns prove classification and rule citations" {
    const workspace_root = try tools_common.workspaceRoot(testing.allocator);
    defer testing.allocator.free(workspace_root);

    var transcript: transcript_mod.Transcript = .{};
    defer transcript.deinit(testing.allocator);
    try transcript.append(testing.allocator, .{ .user_text = "make handler additive" });
    try transcript.append(testing.allocator, .{ .model_text = "Preserve ZTS204 and add a new route without breaking the old one." });

    var analysis = try analyzePatch(
        testing.allocator,
        workspace_root,
        "handler.ts",
        "function handler(req: Request): Response { return Response.json({ ok: true }); }",
        "import { env } from \"zigttp:env\";\nfunction handler(req: Request): Response { return Response.json({ ok: true, region: env(\"REGION\") }); }",
        &transcript,
    );
    defer analysis.deinit(testing.allocator);

    try testing.expectEqual(@as(u32, 0), analysis.stats.new);
    try testing.expect(analysis.after_properties != null);
    try testing.expect(analysis.prove != null);
    try testing.expectEqualStrings("additive", analysis.prove.?.classification);
    try testing.expectEqual(@as(usize, 1), analysis.rule_citations.len);
    try testing.expectEqualStrings("ZTS204", analysis.rule_citations[0]);
    try testing.expect(analysis.hunks.len == 1);
}

test "buildUnifiedDiff emits empty diff for identical content" {
    const diff = try buildUnifiedDiff(testing.allocator, "same\n", "same\n");
    defer testing.allocator.free(diff.text);
    defer testing.allocator.free(diff.hunks);

    try testing.expectEqual(@as(usize, 0), diff.text.len);
    try testing.expectEqual(@as(usize, 0), diff.hunks.len);
}
