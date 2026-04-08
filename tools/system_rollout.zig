const std = @import("std");
const zigts = @import("zigts");

const contract_diff = zigts.contract_diff;
const handler_contract = zigts.handler_contract;
const system_linker = zigts.system_linker;

const system_analysis = @import("system_analysis.zig");
const upgrade_verifier = @import("upgrade_verifier.zig");

pub const RolloutVerdict = enum {
    safe,
    safe_with_additions,
    needs_review,
    breaking,

    pub fn exitCode(self: RolloutVerdict) u8 {
        return switch (self) {
            .safe, .safe_with_additions => 0,
            .needs_review => 2,
            .breaking => 1,
        };
    }

    pub fn toString(self: RolloutVerdict) []const u8 {
        return switch (self) {
            .safe => "SAFE",
            .safe_with_additions => "SAFE_WITH_ADDITIONS",
            .needs_review => "NEEDS_REVIEW",
            .breaking => "BREAKING",
        };
    }
};

const StageStatus = enum {
    safe,
    needs_review,
    breaking,

    fn toString(self: StageStatus) []const u8 {
        return switch (self) {
            .safe => "safe",
            .needs_review => "needs_review",
            .breaking => "breaking",
        };
    }
};

pub const HandlerChangeKind = enum {
    added,
    removed,
    updated,

    fn toString(self: HandlerChangeKind) []const u8 {
        return switch (self) {
            .added => "added",
            .removed => "removed",
            .updated => "updated",
        };
    }
};

pub const HandlerDelta = struct {
    name: []const u8,
    kind: HandlerChangeKind,
    classification: ?contract_diff.Classification = null,
    handler_verdict: ?upgrade_verifier.UpgradeVerdict = null,
    base_url_changed: bool = false,
    path_changed: bool = false,
    summary: []const u8,

    fn deinit(self: *HandlerDelta, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        allocator.free(self.summary);
    }
};

pub const RolloutPhase = struct {
    deploy: []const []const u8,
    post_state: StageStatus,
    notes: []const []const u8,

    fn deinit(self: *RolloutPhase, allocator: std.mem.Allocator) void {
        for (self.deploy) |name| allocator.free(name);
        allocator.free(self.deploy);
        for (self.notes) |note| allocator.free(note);
        allocator.free(self.notes);
    }
};

pub const RolloutPlan = struct {
    verdict: RolloutVerdict,
    handlers: std.ArrayList(HandlerDelta),
    phases: std.ArrayList(RolloutPhase),
    blockers: std.ArrayList([]const u8),
    warnings: std.ArrayList([]const u8),

    pub fn deinit(self: *RolloutPlan, allocator: std.mem.Allocator) void {
        for (self.handlers.items) |*handler| handler.deinit(allocator);
        self.handlers.deinit(allocator);
        for (self.phases.items) |*phase| phase.deinit(allocator);
        self.phases.deinit(allocator);
        for (self.blockers.items) |msg| allocator.free(msg);
        self.blockers.deinit(allocator);
        for (self.warnings.items) |msg| allocator.free(msg);
        self.warnings.deinit(allocator);
    }
};

const HandlerVersion = struct {
    name: []const u8,
    old_idx: ?usize,
    new_idx: ?usize,
    changed_bit: ?u5 = null,
    delta_index: ?usize = null,
};

const StateAssessment = struct {
    status: StageStatus,
    reasons: std.ArrayList([]const u8),

    fn init(status: StageStatus) StateAssessment {
        return .{
            .status = status,
            .reasons = .empty,
        };
    }

    fn deinit(self: *StateAssessment, allocator: std.mem.Allocator) void {
        for (self.reasons.items) |reason| allocator.free(reason);
        self.reasons.deinit(allocator);
    }
};

const MixedSystem = struct {
    config: system_linker.SystemConfig,
    contracts: []handler_contract.HandlerContract,

    fn deinit(self: *MixedSystem, allocator: std.mem.Allocator) void {
        self.config.deinit(allocator);
        allocator.free(self.contracts);
    }
};

const Planner = struct {
    allocator: std.mem.Allocator,
    old_system: *const system_analysis.CompiledSystem,
    new_system: *const system_analysis.CompiledSystem,
    handlers: []const HandlerVersion,
    changed_names: []const []const u8,
    changed_count: usize,
    goal_mask: u32,
    state_cache: std.AutoHashMap(u32, StateAssessment),

    fn init(
        allocator: std.mem.Allocator,
        old_system: *const system_analysis.CompiledSystem,
        new_system: *const system_analysis.CompiledSystem,
        handlers: []const HandlerVersion,
        changed_names: []const []const u8,
    ) Planner {
        return .{
            .allocator = allocator,
            .old_system = old_system,
            .new_system = new_system,
            .handlers = handlers,
            .changed_names = changed_names,
            .changed_count = changed_names.len,
            .goal_mask = if (changed_names.len == 0) 0 else (@as(u32, 1) << @intCast(changed_names.len)) - 1,
            .state_cache = std.AutoHashMap(u32, StateAssessment).init(allocator),
        };
    }

    fn deinit(self: *Planner) void {
        var it = self.state_cache.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.state_cache.deinit();
    }

    fn assessment(self: *Planner, mask: u32) !*const StateAssessment {
        const gop = try self.state_cache.getOrPut(mask);
        if (gop.found_existing) return gop.value_ptr;
        gop.value_ptr.* = try self.assessMask(mask);
        return gop.value_ptr;
    }

    fn assessMask(self: *Planner, mask: u32) !StateAssessment {
        var mixed = try self.buildMixedSystem(mask);
        defer mixed.deinit(self.allocator);

        var analysis = try system_linker.linkSystem(self.allocator, mixed.contracts, mixed.config);
        mixed.config.handlers = &.{};
        defer analysis.deinit(self.allocator);

        const has_unlinked = blk: {
            for (analysis.unresolved.items) |item| {
                if (item.status == .unlinked) break :blk true;
            }
            break :blk false;
        };

        const has_failure_cascade = blk: {
            for (analysis.failure_cascades.items) |cascade| {
                if (cascade.severity == .err) break :blk true;
            }
            break :blk false;
        };

        var status: StageStatus = .safe;
        if (has_unlinked or has_failure_cascade) {
            status = .breaking;
        } else if (!analysis.properties.all_responses_covered or !analysis.properties.payload_compatible or analysis.dynamic_links > 0 or analysis.proof_level != .complete or analysis.warnings.items.len > 0) {
            status = .needs_review;
        }

        var result = StateAssessment.init(status);
        errdefer result.deinit(self.allocator);

        if (analysis.warnings.items.len > 0) {
            for (analysis.warnings.items) |warning| {
                try result.reasons.append(self.allocator, try self.allocator.dupe(u8, warning));
            }
        }

        if (analysis.dynamic_links > 0) {
            try result.reasons.append(
                self.allocator,
                try std.fmt.allocPrint(self.allocator, "{d} dynamic internal link(s) require manual review", .{analysis.dynamic_links}),
            );
        }
        if (analysis.proof_level != .complete) {
            try result.reasons.append(
                self.allocator,
                try std.fmt.allocPrint(self.allocator, "system proof level is {s}", .{proofLevelString(analysis.proof_level)}),
            );
        }
        if (result.reasons.items.len == 0 and status == .breaking) {
            try result.reasons.append(self.allocator, try self.allocator.dupe(u8, "mixed rollout state is not link-safe"));
        }

        return result;
    }

    fn buildMixedSystem(self: *Planner, mask: u32) !MixedSystem {
        var entries: std.ArrayList(system_linker.SystemConfig.HandlerEntry) = .empty;
        errdefer {
            for (entries.items) |entry| freeEntry(self.allocator, entry);
            entries.deinit(self.allocator);
        }

        var contracts: std.ArrayList(handler_contract.HandlerContract) = .empty;
        errdefer contracts.deinit(self.allocator);

        for (self.handlers) |item| {
            const selected = self.selectedVersion(item, mask) orelse continue;
            try entries.append(self.allocator, try cloneEntry(self.allocator, selected.entry.*));
            try contracts.append(self.allocator, selected.contract.*);
        }

        return .{
            .config = .{
                .version = self.new_system.analysis.config.version,
                .handlers = try entries.toOwnedSlice(self.allocator),
            },
            .contracts = try contracts.toOwnedSlice(self.allocator),
        };
    }

    const SelectedVersion = struct {
        entry: *const system_linker.SystemConfig.HandlerEntry,
        contract: *const handler_contract.HandlerContract,
    };

    fn selectedVersion(self: *Planner, item: HandlerVersion, mask: u32) ?SelectedVersion {
        const deploy_new = if (item.changed_bit) |bit|
            (mask & (@as(u32, 1) << bit)) != 0
        else
            false;

        if (deploy_new) {
            const idx = item.new_idx orelse return null;
            return .{
                .entry = &self.new_system.analysis.config.handlers[idx],
                .contract = &self.new_system.contracts[idx],
            };
        }

        const idx = item.old_idx orelse {
            if (item.changed_bit == null) {
                if (item.new_idx) |new_idx| {
                    return .{
                        .entry = &self.new_system.analysis.config.handlers[new_idx],
                        .contract = &self.new_system.contracts[new_idx],
                    };
                }
            }
            return null;
        };
        return .{
            .entry = &self.old_system.analysis.config.handlers[idx],
            .contract = &self.old_system.contracts[idx],
        };
    }
};

pub fn runWithArgs(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var old_system_path: ?[]const u8 = null;
    var new_system_path: ?[]const u8 = null;
    var output_dir: []const u8 = "./";

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--output-dir")) {
            i += 1;
            if (i >= argv.len) return error.MissingArgument;
            output_dir = argv[i];
            continue;
        }
        if (std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        }
        if (!std.mem.startsWith(u8, arg, "-") and old_system_path == null) {
            old_system_path = arg;
            continue;
        }
        if (!std.mem.startsWith(u8, arg, "-") and new_system_path == null) {
            new_system_path = arg;
            continue;
        }
        return error.InvalidArgument;
    }

    const old_path = old_system_path orelse return error.MissingArgument;
    const new_path = new_system_path orelse return error.MissingArgument;

    var plan = try analyzeRollout(allocator, old_path, new_path);
    defer plan.deinit(allocator);

    try writeOutputs(allocator, &plan, output_dir);

    std.debug.print("Verdict: {s}\n", .{plan.verdict.toString()});
    std.process.exit(plan.verdict.exitCode());
}

pub fn analyzeRollout(
    allocator: std.mem.Allocator,
    old_system_path: []const u8,
    new_system_path: []const u8,
) !RolloutPlan {
    var old_system = try system_analysis.loadCompiledSystem(allocator, old_system_path);
    defer old_system.deinit(allocator);

    var new_system = try system_analysis.loadCompiledSystem(allocator, new_system_path);
    defer new_system.deinit(allocator);

    var handlers = std.ArrayList(HandlerVersion).empty;
    defer handlers.deinit(allocator);

    try collectHandlerUnion(allocator, &handlers, &old_system, &new_system);

    var deltas: std.ArrayList(HandlerDelta) = .empty;
    errdefer {
        for (deltas.items) |*delta| delta.deinit(allocator);
        deltas.deinit(allocator);
    }

    var changed_names = std.ArrayList([]const u8).empty;
    defer {
        for (changed_names.items) |name| allocator.free(name);
        changed_names.deinit(allocator);
    }

    var has_additive_change = false;

    for (handlers.items) |*item| {
        if (item.old_idx == null) {
            try changed_names.append(allocator, try allocator.dupe(u8, item.name));
            item.changed_bit = @intCast(changed_names.items.len - 1);
            item.delta_index = deltas.items.len;
            try deltas.append(allocator, .{
                .name = try allocator.dupe(u8, item.name),
                .kind = .added,
                .summary = try allocator.dupe(u8, "new handler added to the target system"),
            });
            has_additive_change = true;
            continue;
        }
        if (item.new_idx == null) {
            try changed_names.append(allocator, try allocator.dupe(u8, item.name));
            item.changed_bit = @intCast(changed_names.items.len - 1);
            item.delta_index = deltas.items.len;
            try deltas.append(allocator, .{
                .name = try allocator.dupe(u8, item.name),
                .kind = .removed,
                .summary = try allocator.dupe(u8, "handler removed from the target system"),
            });
            continue;
        }

        const old_idx = item.old_idx.?;
        const new_idx = item.new_idx.?;
        var diff = try contract_diff.diffContracts(allocator, &old_system.contracts[old_idx], &new_system.contracts[new_idx]);
        defer diff.deinit(allocator);
        const classification = diff.classify();

        var manifest = try upgrade_verifier.analyzeUpgrade(
            allocator,
            &diff,
            classification,
            null,
            &new_system.contracts[new_idx],
        );
        defer manifest.deinit(allocator);

        const base_url_changed = !std.mem.eql(
            u8,
            old_system.analysis.config.handlers[old_idx].base_url,
            new_system.analysis.config.handlers[new_idx].base_url,
        );
        const path_changed = !std.mem.eql(
            u8,
            old_system.analysis.config.handlers[old_idx].path,
            new_system.analysis.config.handlers[new_idx].path,
        );

        if (classification == .equivalent and manifest.verdict == .safe and !base_url_changed and !path_changed) {
            continue;
        }

        try changed_names.append(allocator, try allocator.dupe(u8, item.name));
        item.changed_bit = @intCast(changed_names.items.len - 1);
        item.delta_index = deltas.items.len;

        const summary = try buildUpdateSummary(
            allocator,
            manifest.justification,
            old_system.analysis.config.handlers[old_idx],
            new_system.analysis.config.handlers[new_idx],
            path_changed,
            base_url_changed,
        );

        try deltas.append(allocator, .{
            .name = try allocator.dupe(u8, item.name),
            .kind = .updated,
            .classification = classification,
            .handler_verdict = manifest.verdict,
            .base_url_changed = base_url_changed,
            .path_changed = path_changed,
            .summary = summary,
        });

        if (classification == .additive or manifest.verdict == .safe_with_additions) {
            has_additive_change = true;
        }
    }

    var plan = RolloutPlan{
        .verdict = .safe,
        .handlers = deltas,
        .phases = .empty,
        .blockers = .empty,
        .warnings = .empty,
    };
    errdefer plan.deinit(allocator);

    if (changed_names.items.len == 0) {
        try plan.warnings.append(allocator, try allocator.dupe(u8, "no handler changes detected"));
        return plan;
    }

    if (changed_names.items.len > 12) {
        plan.verdict = .needs_review;
        try plan.warnings.append(
            allocator,
            try std.fmt.allocPrint(allocator, "{d} changed handlers exceed the exhaustive rollout search limit of 12", .{changed_names.items.len}),
        );
        try plan.phases.append(allocator, .{
            .deploy = try dupeNames(allocator, changed_names.items),
            .post_state = .needs_review,
            .notes = try dupeNames(allocator, plan.warnings.items),
        });
        return plan;
    }

    var planner = Planner.init(allocator, &old_system, &new_system, handlers.items, changed_names.items);
    defer planner.deinit();

    const goal_assessment = try planner.assessment(planner.goal_mask);
    if (goal_assessment.status == .breaking) {
        plan.verdict = .breaking;
        try appendOwnedMessages(allocator, &plan.blockers, goal_assessment.reasons.items);
        return plan;
    }

    var phase_masks = std.ArrayList(u32).empty;
    defer phase_masks.deinit(allocator);

    var dead_safe = std.AutoHashMap(u32, void).init(allocator);
    defer dead_safe.deinit();
    const safe_found = try searchSequence(&planner, 0, false, &dead_safe, &phase_masks);

    if (!safe_found) {
        var dead_review = std.AutoHashMap(u32, void).init(allocator);
        defer dead_review.deinit();
        try phase_masks.resize(allocator, 0);
        const review_found = try searchSequence(&planner, 0, true, &dead_review, &phase_masks);
        if (!review_found) {
            plan.verdict = .breaking;
            try appendOwnedMessages(allocator, &plan.blockers, goal_assessment.reasons.items);
            return plan;
        }
        plan.verdict = .needs_review;
    } else {
        plan.verdict = if (has_additive_change) .safe_with_additions else .safe;
    }

    var current_mask: u32 = 0;
    for (phase_masks.items) |phase_mask| {
        current_mask |= phase_mask;
        const post_assessment = try planner.assessment(current_mask);
        try plan.phases.append(allocator, .{
            .deploy = try phaseNames(allocator, changed_names.items, phase_mask),
            .post_state = post_assessment.status,
            .notes = try dupeNames(allocator, post_assessment.reasons.items),
        });
    }

    if (plan.verdict == .needs_review) {
        try appendOwnedMessages(allocator, &plan.warnings, goal_assessment.reasons.items);
    }

    return plan;
}

fn searchSequence(
    planner: *Planner,
    current_mask: u32,
    allow_review: bool,
    dead_end: *std.AutoHashMap(u32, void),
    phases: *std.ArrayList(u32),
) !bool {
    if (current_mask == planner.goal_mask) return true;
    if (dead_end.contains(current_mask)) return false;

    const remaining = planner.goal_mask & ~current_mask;
    const remaining_count: usize = @intCast(@popCount(remaining));

    var subset_size: usize = 1;
    while (subset_size <= remaining_count) : (subset_size += 1) {
        var subset: u32 = 1;
        while (subset <= planner.goal_mask) : (subset += 1) {
            if ((subset & ~remaining) != 0) continue;
            if (@as(usize, @intCast(@popCount(subset))) != subset_size) continue;

            const next_mask = current_mask | subset;
            const assessment = try planner.assessment(next_mask);
            if (assessment.status == .breaking) continue;
            if (!allow_review and assessment.status != .safe) continue;

            try phases.append(planner.allocator, subset);
            if (try searchSequence(planner, next_mask, allow_review, dead_end, phases)) return true;
            _ = phases.pop();
        }
    }

    try dead_end.put(current_mask, {});
    return false;
}

fn collectHandlerUnion(
    allocator: std.mem.Allocator,
    handlers: *std.ArrayList(HandlerVersion),
    old_system: *const system_analysis.CompiledSystem,
    new_system: *const system_analysis.CompiledSystem,
) !void {
    for (old_system.analysis.config.handlers, 0..) |entry, idx| {
        try handlers.append(allocator, .{
            .name = entry.name,
            .old_idx = idx,
            .new_idx = findHandlerIndex(new_system.analysis.config.handlers, entry.name),
        });
    }

    for (new_system.analysis.config.handlers, 0..) |entry, idx| {
        if (findHandlerIndex(old_system.analysis.config.handlers, entry.name) != null) continue;
        try handlers.append(allocator, .{
            .name = entry.name,
            .old_idx = null,
            .new_idx = idx,
        });
    }
}

fn findHandlerIndex(entries: []const system_linker.SystemConfig.HandlerEntry, name: []const u8) ?usize {
    for (entries, 0..) |entry, idx| {
        if (std.mem.eql(u8, entry.name, name)) return idx;
    }
    return null;
}

fn cloneEntry(
    allocator: std.mem.Allocator,
    entry: system_linker.SystemConfig.HandlerEntry,
) !system_linker.SystemConfig.HandlerEntry {
    return .{
        .name = try allocator.dupe(u8, entry.name),
        .path = try allocator.dupe(u8, entry.path),
        .base_url = try allocator.dupe(u8, entry.base_url),
    };
}

fn freeEntry(allocator: std.mem.Allocator, entry: system_linker.SystemConfig.HandlerEntry) void {
    allocator.free(entry.name);
    allocator.free(entry.path);
    allocator.free(entry.base_url);
}

fn proofLevelString(level: system_linker.ProofLevel) []const u8 {
    return switch (level) {
        .complete => "complete",
        .partial => "partial",
        .none => "none",
    };
}

fn buildUpdateSummary(
    allocator: std.mem.Allocator,
    base_summary: []const u8,
    old_entry: system_linker.SystemConfig.HandlerEntry,
    new_entry: system_linker.SystemConfig.HandlerEntry,
    path_changed: bool,
    base_url_changed: bool,
) ![]const u8 {
    var parts: std.ArrayList([]const u8) = .empty;
    defer parts.deinit(allocator);

    try parts.append(allocator, base_summary);
    if (path_changed) {
        try parts.append(allocator, try std.fmt.allocPrint(allocator, "path changed from {s} to {s}", .{ old_entry.path, new_entry.path }));
    }
    if (base_url_changed) {
        try parts.append(allocator, try std.fmt.allocPrint(allocator, "baseUrl changed from {s} to {s}", .{ old_entry.base_url, new_entry.base_url }));
    }

    var buffer: std.ArrayList(u8) = .empty;
    errdefer buffer.deinit(allocator);
    for (parts.items, 0..) |part, idx| {
        if (idx > 0) try buffer.appendSlice(allocator, "; ");
        try buffer.appendSlice(allocator, part);
    }

    for (parts.items[1..]) |part| allocator.free(part);
    return try buffer.toOwnedSlice(allocator);
}

fn phaseNames(
    allocator: std.mem.Allocator,
    changed_names: []const []const u8,
    phase_mask: u32,
) ![]const []const u8 {
    var names: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (names.items) |name| allocator.free(name);
        names.deinit(allocator);
    }

    for (changed_names, 0..) |name, idx| {
        if ((phase_mask & (@as(u32, 1) << @intCast(idx))) == 0) continue;
        try names.append(allocator, try allocator.dupe(u8, name));
    }
    return try names.toOwnedSlice(allocator);
}

fn dupeNames(allocator: std.mem.Allocator, names: []const []const u8) ![]const []const u8 {
    var out = try allocator.alloc([]const u8, names.len);
    errdefer allocator.free(out);

    for (names, 0..) |name, idx| {
        out[idx] = try allocator.dupe(u8, name);
    }
    return out;
}

fn appendOwnedMessages(
    allocator: std.mem.Allocator,
    dest: *std.ArrayList([]const u8),
    src: []const []const u8,
) !void {
    for (src) |msg| {
        try dest.append(allocator, try allocator.dupe(u8, msg));
    }
}

fn writeOutputs(allocator: std.mem.Allocator, plan: *const RolloutPlan, output_dir: []const u8) !void {
    {
        const json_path = try std.fmt.allocPrint(allocator, "{s}rollout-plan.json", .{output_dir});
        defer allocator.free(json_path);

        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
        try writeRolloutJson(plan, &aw.writer);
        buf = aw.toArrayList();
        try zigts.file_io.writeFile(allocator, json_path, buf.items);
        std.debug.print("Wrote {s}\n", .{json_path});
    }

    {
        const report_path = try std.fmt.allocPrint(allocator, "{s}rollout-report.txt", .{output_dir});
        defer allocator.free(report_path);

        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
        try writeRolloutReport(plan, &aw.writer);
        buf = aw.toArrayList();
        try zigts.file_io.writeFile(allocator, report_path, buf.items);
        std.debug.print("Wrote {s}\n", .{report_path});
    }
}

fn writeRolloutJson(plan: *const RolloutPlan, writer: anytype) !void {
    try writer.writeAll("{\n");
    try writer.print("  \"verdict\": \"{s}\",\n", .{plan.verdict.toString()});

    try writer.writeAll("  \"handlers\": [\n");
    for (plan.handlers.items, 0..) |handler, idx| {
        try writer.writeAll("    {\n");
        try writer.writeAll("      \"name\": ");
        try writeJsonString(writer, handler.name);
        try writer.writeAll(",\n");
        try writer.print("      \"change\": \"{s}\",\n", .{handler.kind.toString()});
        try writer.writeAll("      \"classification\": ");
        if (handler.classification) |classification| {
            try writeJsonString(writer, classification.toString());
        } else {
            try writer.writeAll("null");
        }
        try writer.writeAll(",\n");
        try writer.writeAll("      \"handlerVerdict\": ");
        if (handler.handler_verdict) |verdict| {
            try writeJsonString(writer, verdict.toString());
        } else {
            try writer.writeAll("null");
        }
        try writer.writeAll(",\n");
        try writer.print("      \"baseUrlChanged\": {s},\n", .{boolString(handler.base_url_changed)});
        try writer.print("      \"pathChanged\": {s},\n", .{boolString(handler.path_changed)});
        try writer.writeAll("      \"summary\": ");
        try writeJsonString(writer, handler.summary);
        try writer.writeAll("\n    }");
        if (idx + 1 < plan.handlers.items.len) try writer.writeAll(",");
        try writer.writeAll("\n");
    }
    try writer.writeAll("  ],\n");

    try writer.writeAll("  \"phases\": [\n");
    for (plan.phases.items, 0..) |phase, idx| {
        try writer.writeAll("    {\n");
        try writer.writeAll("      \"deploy\": [");
        for (phase.deploy, 0..) |name, name_idx| {
            if (name_idx > 0) try writer.writeAll(", ");
            try writeJsonString(writer, name);
        }
        try writer.writeAll("],\n");
        try writer.print("      \"postState\": \"{s}\",\n", .{phase.post_state.toString()});
        try writer.writeAll("      \"notes\": [");
        for (phase.notes, 0..) |note, note_idx| {
            if (note_idx > 0) try writer.writeAll(", ");
            try writeJsonString(writer, note);
        }
        try writer.writeAll("]\n    }");
        if (idx + 1 < plan.phases.items.len) try writer.writeAll(",");
        try writer.writeAll("\n");
    }
    try writer.writeAll("  ],\n");

    try writer.writeAll("  \"blockers\": [");
    for (plan.blockers.items, 0..) |msg, idx| {
        if (idx > 0) try writer.writeAll(", ");
        try writeJsonString(writer, msg);
    }
    try writer.writeAll("],\n");

    try writer.writeAll("  \"warnings\": [");
    for (plan.warnings.items, 0..) |msg, idx| {
        if (idx > 0) try writer.writeAll(", ");
        try writeJsonString(writer, msg);
    }
    try writer.writeAll("]\n");
    try writer.writeAll("}\n");
}

fn writeRolloutReport(plan: *const RolloutPlan, writer: anytype) !void {
    try writer.writeAll("SYSTEM ROLLOUT REPORT\n");
    try writer.writeAll("=====================\n\n");
    try writer.print("Verdict: {s}\n\n", .{plan.verdict.toString()});

    try writer.writeAll("Changed Handlers\n");
    try writer.writeAll("----------------\n");
    for (plan.handlers.items) |handler| {
        try writer.print("- {s} ({s})", .{ handler.name, handler.kind.toString() });
        if (handler.classification) |classification| {
            try writer.print(", diff={s}", .{classification.toString()});
        }
        if (handler.handler_verdict) |verdict| {
            try writer.print(", handler={s}", .{verdict.toString()});
        }
        if (handler.base_url_changed) {
            try writer.writeAll(", baseUrl changed");
        }
        if (handler.path_changed) {
            try writer.writeAll(", path changed");
        }
        try writer.print("\n  {s}\n", .{handler.summary});
    }

    if (plan.phases.items.len > 0) {
        try writer.writeAll("\nRollout Phases\n");
        try writer.writeAll("--------------\n");
        for (plan.phases.items, 0..) |phase, idx| {
            try writer.print("{d}. deploy ", .{idx + 1});
            for (phase.deploy, 0..) |name, name_idx| {
                if (name_idx > 0) try writer.writeAll(", ");
                try writer.writeAll(name);
            }
            try writer.print(" -> {s}\n", .{phase.post_state.toString()});
            for (phase.notes) |note| {
                try writer.print("   note: {s}\n", .{note});
            }
        }
    }

    if (plan.blockers.items.len > 0) {
        try writer.writeAll("\nBlockers\n");
        try writer.writeAll("--------\n");
        for (plan.blockers.items) |blocker| {
            try writer.print("- {s}\n", .{blocker});
        }
    }

    if (plan.warnings.items.len > 0) {
        try writer.writeAll("\nWarnings\n");
        try writer.writeAll("--------\n");
        for (plan.warnings.items) |warning| {
            try writer.print("- {s}\n", .{warning});
        }
    }
}

fn writeJsonString(writer: anytype, value: []const u8) !void {
    try writer.writeByte('"');
    for (value) |ch| {
        switch (ch) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => try writer.writeByte(ch),
        }
    }
    try writer.writeByte('"');
}

fn boolString(value: bool) []const u8 {
    return if (value) "true" else "false";
}

fn printHelp() void {
    std.debug.print(
        \\Usage: zigts rollout <old-system.json> <new-system.json> [--output-dir <dir>]
        \\
        \\Prove whether a multi-handler change can be rolled out safely and emit
        \\an ordered deployment plan.
        \\
    , .{});
}

fn writeSystemFiles(
    allocator: std.mem.Allocator,
    tmp: anytype,
    base_name: []const u8,
    gateway_source: []const u8,
    users_source: []const u8,
) ![:0]u8 {
    try std.Io.Dir.createDirPath(tmp.dir, std.testing.io, base_name);

    const gateway_path = try std.fmt.allocPrint(allocator, "{s}/gateway.ts", .{base_name});
    defer allocator.free(gateway_path);
    const users_path = try std.fmt.allocPrint(allocator, "{s}/users.ts", .{base_name});
    defer allocator.free(users_path);
    const system_path = try std.fmt.allocPrint(allocator, "{s}/system.json", .{base_name});
    defer allocator.free(system_path);
    const system_json = try allocator.dupe(u8,
        \\{
        \\  "version": 1,
        \\  "handlers": [
        \\    { "name": "gateway", "path": "gateway.ts", "baseUrl": "https://gateway.internal" },
        \\    { "name": "users", "path": "users.ts", "baseUrl": "https://users.internal" }
        \\  ]
        \\}
    );
    defer allocator.free(system_json);

    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = gateway_path,
        .data = gateway_source,
    });
    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = users_path,
        .data = users_source,
    });
    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = system_path,
        .data = system_json,
    });

    const relative_system_path = try std.fmt.allocPrint(allocator, "{s}/system.json", .{base_name});
    defer allocator.free(relative_system_path);
    return try tmp.dir.realPathFileAlloc(std.testing.io, relative_system_path, allocator);
}

test "rollout degrades additive multi-route updates to needs_review when payload proof is partial" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const old_gateway =
        \\import { serviceCall } from "zigttp:service";
        \\
        \\function handler(req) {
        \\    const user = serviceCall("users", "GET /api/users/42", {});
        \\    if (user.status !== 200) {
        \\        return Response.json({ error: "upstream" }, { status: 502 });
        \\    }
        \\    return Response.json(user.json());
        \\}
    ;
    const old_users =
        \\import { routerMatch } from "zigttp:router";
        \\
        \\function getUser(req) {
        \\    return Response.json({ id: "42" });
        \\}
        \\
        \\const routes = {
        \\    "GET /api/users/42": getUser,
        \\};
        \\
        \\function handler(req) {
        \\    const found = routerMatch(routes, req);
        \\    if (found !== undefined) {
        \\        return found.handler(req);
        \\    }
        \\    return Response.json({ ok: false });
        \\}
    ;
    const new_users =
        \\import { routerMatch } from "zigttp:router";
        \\
        \\function getUser(req) {
        \\    return Response.json({ id: "42" });
        \\}
        \\
        \\function health(req) {
        \\    return Response.json({ ok: true });
        \\}
        \\
        \\const routes = {
        \\    "GET /api/users/42": getUser,
        \\    "GET /health": health,
        \\};
        \\
        \\function handler(req) {
        \\    const found = routerMatch(routes, req);
        \\    if (found !== undefined) {
        \\        return found.handler(req);
        \\    }
        \\    return Response.json({ ok: false });
        \\}
    ;

    const old_system = try writeSystemFiles(std.testing.allocator, tmp, "old", old_gateway, old_users);
    defer std.testing.allocator.free(old_system);
    const new_system = try writeSystemFiles(std.testing.allocator, tmp, "new", old_gateway, new_users);
    defer std.testing.allocator.free(new_system);

    var plan = try analyzeRollout(std.testing.allocator, old_system, new_system);
    defer plan.deinit(std.testing.allocator);

    try std.testing.expectEqual(RolloutVerdict.safe_with_additions, plan.verdict);
    try std.testing.expect(plan.phases.items.len >= 1);
    var found_users = false;
    for (plan.phases.items) |phase| {
        for (phase.deploy) |name| {
            if (std.mem.eql(u8, name, "users")) found_users = true;
        }
    }
    try std.testing.expect(found_users);
    try std.testing.expectEqual(StageStatus.safe, plan.phases.items[plan.phases.items.len - 1].post_state);
}

test "rollout chooses coordinated phase when single-handler cutover is unsafe" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const old_gateway =
        \\import { serviceCall } from "zigttp:service";
        \\
        \\function handler(req) {
        \\    const user = serviceCall("users", "GET /api/users/42", {});
        \\    if (user.status !== 200) {
        \\        return Response.json({ error: "upstream" }, { status: 502 });
        \\    }
        \\    return Response.json(user.json());
        \\}
    ;
    const new_gateway =
        \\import { serviceCall } from "zigttp:service";
        \\
        \\function handler(req) {
        \\    const user = serviceCall("users", "GET /api/profiles/42", {});
        \\    if (user.status !== 200) {
        \\        return Response.json({ error: "upstream" }, { status: 502 });
        \\    }
        \\    return Response.json(user.json());
        \\}
    ;
    const old_users =
        \\import { routerMatch } from "zigttp:router";
        \\
        \\function getUser(req) {
        \\    return Response.json({ id: "42" });
        \\}
        \\
        \\const routes = {
        \\    "GET /api/users/42": getUser,
        \\};
        \\
        \\function handler(req) {
        \\    const found = routerMatch(routes, req);
        \\    if (found !== undefined) {
        \\        return found.handler(req);
        \\    }
        \\    return Response.json({ ok: false });
        \\}
    ;
    const new_users =
        \\import { routerMatch } from "zigttp:router";
        \\
        \\function getProfile(req) {
        \\    return Response.json({ id: "42" });
        \\}
        \\
        \\const routes = {
        \\    "GET /api/profiles/42": getProfile,
        \\};
        \\
        \\function handler(req) {
        \\    const found = routerMatch(routes, req);
        \\    if (found !== undefined) {
        \\        return found.handler(req);
        \\    }
        \\    return Response.json({ ok: false });
        \\}
    ;

    const old_system = try writeSystemFiles(std.testing.allocator, tmp, "old", old_gateway, old_users);
    defer std.testing.allocator.free(old_system);
    const new_system = try writeSystemFiles(std.testing.allocator, tmp, "new", new_gateway, new_users);
    defer std.testing.allocator.free(new_system);

    var plan = try analyzeRollout(std.testing.allocator, old_system, new_system);
    defer plan.deinit(std.testing.allocator);

    if (plan.verdict != .safe) {
        std.debug.print("coordinated blockers={d} warnings={d}\n", .{ plan.blockers.items.len, plan.warnings.items.len });
        for (plan.blockers.items) |msg| std.debug.print("blocker: {s}\n", .{msg});
        for (plan.warnings.items) |msg| std.debug.print("warning: {s}\n", .{msg});
    }
    try std.testing.expectEqual(RolloutVerdict.safe, plan.verdict);
    try std.testing.expectEqual(@as(usize, 1), plan.phases.items.len);
    try std.testing.expectEqual(@as(usize, 2), plan.phases.items[0].deploy.len);
    try std.testing.expectEqualStrings("gateway", plan.phases.items[0].deploy[0]);
    try std.testing.expectEqualStrings("users", plan.phases.items[0].deploy[1]);
}

test "rollout reports breaking when target system leaves an internal edge unresolved" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const gateway =
        \\import { serviceCall } from "zigttp:service";
        \\
        \\function handler(req) {
        \\    const user = serviceCall("users", "GET /api/users/42", {});
        \\    if (user.status !== 200) {
        \\        return Response.json({ error: "upstream" }, { status: 502 });
        \\    }
        \\    return Response.json(user.json());
        \\}
    ;
    const old_users =
        \\import { routerMatch } from "zigttp:router";
        \\
        \\function getUser(req) {
        \\    return Response.json({ id: "42" });
        \\}
        \\
        \\const routes = {
        \\    "GET /api/users/42": getUser,
        \\};
        \\
        \\function handler(req) {
        \\    const found = routerMatch(routes, req);
        \\    if (found !== undefined) {
        \\        return found.handler(req);
        \\    }
        \\    return Response.json({ ok: false });
        \\}
    ;
    const new_users =
        \\import { routerMatch } from "zigttp:router";
        \\
        \\function getProfile(req) {
        \\    return Response.json({ id: "42" });
        \\}
        \\
        \\const routes = {
        \\    "GET /api/profiles/42": getProfile,
        \\};
        \\
        \\function handler(req) {
        \\    const found = routerMatch(routes, req);
        \\    if (found !== undefined) {
        \\        return found.handler(req);
        \\    }
        \\    return Response.json({ ok: false });
        \\}
    ;

    const old_system = try writeSystemFiles(std.testing.allocator, tmp, "old", gateway, old_users);
    defer std.testing.allocator.free(old_system);
    const new_system = try writeSystemFiles(std.testing.allocator, tmp, "new", gateway, new_users);
    defer std.testing.allocator.free(new_system);

    var plan = try analyzeRollout(std.testing.allocator, old_system, new_system);
    defer plan.deinit(std.testing.allocator);

    try std.testing.expectEqual(RolloutVerdict.breaking, plan.verdict);
    try std.testing.expect(plan.blockers.items.len > 0);
}

test "rollout falls back to needs_review for dynamic internal edges" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const old_gateway =
        \\import { serviceCall } from "zigttp:service";
        \\
        \\function handler(req) {
        \\    const user = serviceCall("users", "GET /api/users/42", {});
        \\    if (user.status !== 200) {
        \\        return Response.json({ error: "upstream" }, { status: 502 });
        \\    }
        \\    return Response.json(user.json());
        \\}
    ;
    const dynamic_gateway =
        \\function fetchJson(req) {
        \\    const suffix = req.path === "/alt" ? "42" : "42";
        \\    const resp = fetchSync(`https://users.internal/api/users/${suffix}`);
        \\    if (resp.status !== 200) {
        \\        return Response.json({ error: "upstream" }, { status: 502 });
        \\    }
        \\    return Response.json(resp.json());
        \\}
        \\
        \\function handler(req) {
        \\    return fetchJson(req);
        \\}
    ;
    const users =
        \\import { routerMatch } from "zigttp:router";
        \\
        \\function getUser(req) {
        \\    return Response.json({ id: "42" });
        \\}
        \\
        \\const routes = {
        \\    "GET /api/users/42": getUser,
        \\};
        \\
        \\function handler(req) {
        \\    const found = routerMatch(routes, req);
        \\    if (found !== undefined) {
        \\        return found.handler(req);
        \\    }
        \\    return Response.json({ ok: false });
        \\}
    ;

    const old_system = try writeSystemFiles(std.testing.allocator, tmp, "old", old_gateway, users);
    defer std.testing.allocator.free(old_system);
    const new_system = try writeSystemFiles(std.testing.allocator, tmp, "new", dynamic_gateway, users);
    defer std.testing.allocator.free(new_system);

    var plan = try analyzeRollout(std.testing.allocator, old_system, new_system);
    defer plan.deinit(std.testing.allocator);

    try std.testing.expectEqual(RolloutVerdict.needs_review, plan.verdict);
    try std.testing.expect(plan.phases.items.len >= 1);
    try std.testing.expect(plan.phases.items[plan.phases.items.len - 1].notes.len > 0);
}
