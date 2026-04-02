//! Upgrade Verifier
//!
//! Synthesizes surface diff, behavioral diff, property regression analysis,
//! and coverage gap detection into a unified upgrade verdict. This is the
//! top-level analysis layer that answers: "is this handler upgrade safe?"
//!
//! The verdict goes beyond the existing 3-value Classification (equivalent/
//! additive/breaking) by incorporating behavioral path comparison, property
//! regressions with severity, and trace coverage gaps.
//!
//! Called by prove_upgrade.zig when behavioral data is available.

const std = @import("std");
const zts = @import("zts");
const handler_contract = zts.handler_contract;
const contract_diff = zts.contract_diff;
const HandlerContract = handler_contract.HandlerContract;
const ContractDiff = contract_diff.ContractDiff;
const BehaviorDiff = contract_diff.BehaviorDiff;
const Classification = contract_diff.Classification;
const ReplaySummary = contract_diff.ReplaySummary;
const PropertiesChange = contract_diff.PropertiesChange;

// -------------------------------------------------------------------------
// Verdict
// -------------------------------------------------------------------------

pub const UpgradeVerdict = enum {
    /// Contract identical, properties preserved, all traces pass.
    safe,
    /// New capabilities added, existing preserved, no regressions.
    safe_with_additions,
    /// Routes/capabilities removed, traces diverged, or critical property lost.
    breaking,
    /// Structurally OK but property regressions or coverage gaps detected.
    needs_review,

    pub fn exitCode(self: UpgradeVerdict) u8 {
        return switch (self) {
            .safe, .safe_with_additions => 0,
            .needs_review => 2,
            .breaking => 1,
        };
    }

    pub fn toString(self: UpgradeVerdict) []const u8 {
        return switch (self) {
            .safe => "SAFE",
            .safe_with_additions => "SAFE_WITH_ADDITIONS",
            .breaking => "BREAKING",
            .needs_review => "NEEDS_REVIEW",
        };
    }
};

// -------------------------------------------------------------------------
// Property regression
// -------------------------------------------------------------------------

pub const Severity = enum {
    critical,
    warning,
    info,

    pub fn toString(self: Severity) []const u8 {
        return switch (self) {
            .critical => "critical",
            .warning => "warning",
            .info => "info",
        };
    }
};

pub const PropertyRegression = struct {
    field: []const u8,
    old_value: bool,
    new_value: bool,
    severity: Severity,
};

/// Classify a property change by severity.
/// Critical: properties whose loss indicates a safety regression.
/// Warning: properties whose loss changes operational characteristics.
/// Info: informational properties.
fn propertySeverity(field: []const u8, gained: bool) Severity {
    // Gains are always informational
    if (gained) return .info;

    // Losses are classified by impact
    if (std.mem.eql(u8, field, "retry_safe")) return .critical;
    if (std.mem.eql(u8, field, "injection_safe")) return .critical;
    if (std.mem.eql(u8, field, "state_isolated")) return .critical;
    if (std.mem.eql(u8, field, "no_secret_leakage")) return .critical;
    if (std.mem.eql(u8, field, "no_credential_leakage")) return .critical;
    if (std.mem.eql(u8, field, "fault_covered")) return .critical;
    if (std.mem.eql(u8, field, "result_safe")) return .critical;
    if (std.mem.eql(u8, field, "optional_safe")) return .critical;

    if (std.mem.eql(u8, field, "idempotent")) return .warning;
    if (std.mem.eql(u8, field, "deterministic")) return .warning;
    if (std.mem.eql(u8, field, "read_only")) return .warning;
    if (std.mem.eql(u8, field, "input_validated")) return .warning;
    if (std.mem.eql(u8, field, "pii_contained")) return .warning;

    return .info;
}

// -------------------------------------------------------------------------
// Coverage gap
// -------------------------------------------------------------------------

pub const CoverageGap = struct {
    total_new_paths: u32,
    covered_by_traces: u32,

    pub fn uncovered(self: CoverageGap) u32 {
        if (self.covered_by_traces >= self.total_new_paths) return 0;
        return self.total_new_paths - self.covered_by_traces;
    }
};

// -------------------------------------------------------------------------
// Upgrade manifest
// -------------------------------------------------------------------------

pub const SurfaceSummary = struct {
    routes_added: u32 = 0,
    routes_removed: u32 = 0,
    routes_unchanged: u32 = 0,
    capabilities_widened: bool = false,
    capabilities_narrowed: bool = false,
};

pub const UpgradeManifest = struct {
    verdict: UpgradeVerdict,
    justification: []const u8, // owned
    classification: Classification,
    surface: SurfaceSummary,
    replay: ?ReplaySummary,
    behavioral: ?BehavioralSummary,
    property_regressions: std.ArrayList(PropertyRegression),
    property_gains: std.ArrayList(PropertyRegression),
    coverage_gap: ?CoverageGap,

    pub const BehavioralSummary = struct {
        preserved: u32,
        response_changed: u32,
        removed: u32,
        added: u32,
    };

    pub fn deinit(self: *UpgradeManifest, allocator: std.mem.Allocator) void {
        allocator.free(self.justification);
        self.property_regressions.deinit(allocator);
        self.property_gains.deinit(allocator);
    }
};

// -------------------------------------------------------------------------
// Analysis
// -------------------------------------------------------------------------

pub fn analyzeUpgrade(
    allocator: std.mem.Allocator,
    diff: *const ContractDiff,
    classification: Classification,
    replay: ?ReplaySummary,
    new_contract: *const HandlerContract,
) !UpgradeManifest {
    // Surface summary
    var surface = SurfaceSummary{};
    for (diff.routes.items) |r| {
        switch (r.status) {
            .added => surface.routes_added += 1,
            .removed => surface.routes_removed += 1,
            .unchanged => surface.routes_unchanged += 1,
        }
    }
    surface.capabilities_widened = diff.capabilitiesWidened();
    surface.capabilities_narrowed = diff.capabilitiesNarrowed();

    // Property regressions and gains
    var regressions: std.ArrayList(PropertyRegression) = .empty;
    errdefer regressions.deinit(allocator);
    var gains: std.ArrayList(PropertyRegression) = .empty;
    errdefer gains.deinit(allocator);

    for (diff.effect_changes.items) |change| {
        const gained = change.new_value and !change.old_value;
        const entry = PropertyRegression{
            .field = change.field,
            .old_value = change.old_value,
            .new_value = change.new_value,
            .severity = propertySeverity(change.field, gained),
        };
        if (gained) {
            try gains.append(allocator, entry);
        } else {
            try regressions.append(allocator, entry);
        }
    }

    // Behavioral summary
    var behavioral: ?UpgradeManifest.BehavioralSummary = null;
    if (diff.behavior_diff) |bd| {
        behavioral = .{
            .preserved = bd.preserved,
            .response_changed = bd.response_changed,
            .removed = bd.removed,
            .added = bd.added,
        };
    }

    // Coverage gap (new handler paths vs trace coverage)
    var coverage_gap: ?CoverageGap = null;
    if (replay != null and new_contract.behaviors.items.len > 0) {
        coverage_gap = .{
            .total_new_paths = @intCast(new_contract.behaviors.items.len),
            .covered_by_traces = if (replay) |r| r.identical else 0,
        };
    }

    // Synthesize verdict
    const verdict = synthesizeVerdict(classification, &regressions, diff.behavior_diff, coverage_gap);

    // Generate justification
    const justification = try generateJustification(allocator, verdict, &surface, behavioral, &regressions, &gains, coverage_gap);

    return .{
        .verdict = verdict,
        .justification = justification,
        .classification = classification,
        .surface = surface,
        .replay = replay,
        .behavioral = behavioral,
        .property_regressions = regressions,
        .property_gains = gains,
        .coverage_gap = coverage_gap,
    };
}

fn synthesizeVerdict(
    classification: Classification,
    regressions: *const std.ArrayList(PropertyRegression),
    behavior_diff: ?BehaviorDiff,
    coverage_gap: ?CoverageGap,
) UpgradeVerdict {
    // Structural breaking is always breaking
    if (classification == .breaking) return .breaking;

    // Behavioral breaking (removed paths or changed responses)
    if (behavior_diff) |bd| {
        if (bd.hasBreaking()) return .breaking;
    }

    // Critical property regressions are breaking
    for (regressions.items) |r| {
        if (r.severity == .critical) return .breaking;
    }

    // Warning property regressions need review
    for (regressions.items) |r| {
        if (r.severity == .warning) return .needs_review;
    }

    // Significant coverage gaps need review
    if (coverage_gap) |cg| {
        if (cg.total_new_paths > 0 and cg.uncovered() > cg.total_new_paths / 2) {
            return .needs_review;
        }
    }

    // Additive changes
    if (classification == .additive) return .safe_with_additions;
    if (behavior_diff) |bd| {
        if (bd.added > 0) return .safe_with_additions;
    }

    return .safe;
}

fn generateJustification(
    allocator: std.mem.Allocator,
    verdict: UpgradeVerdict,
    surface: *const SurfaceSummary,
    behavioral: ?UpgradeManifest.BehavioralSummary,
    regressions: *const std.ArrayList(PropertyRegression),
    gains: *const std.ArrayList(PropertyRegression),
    coverage_gap: ?CoverageGap,
) ![]const u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const writer = &aw.writer;

    switch (verdict) {
        .safe => try writer.writeAll("Safe to deploy."),
        .safe_with_additions => try writer.writeAll("Safe to deploy with additions."),
        .breaking => try writer.writeAll("Breaking change detected."),
        .needs_review => try writer.writeAll("Requires review."),
    }

    if (surface.routes_added > 0) {
        try writer.print(" {d} route(s) added.", .{surface.routes_added});
    }
    if (surface.routes_removed > 0) {
        try writer.print(" {d} route(s) removed.", .{surface.routes_removed});
    }

    if (behavioral) |b| {
        if (b.preserved > 0) {
            try writer.print(" {d} path(s) preserved.", .{b.preserved});
        }
        if (b.response_changed > 0) {
            try writer.print(" {d} path(s) changed response.", .{b.response_changed});
        }
        if (b.removed > 0) {
            try writer.print(" {d} path(s) removed.", .{b.removed});
        }
        if (b.added > 0) {
            try writer.print(" {d} new path(s).", .{b.added});
        }
    }

    for (regressions.items) |r| {
        try writer.print(" Lost {s} ({s}).", .{ r.field, r.severity.toString() });
    }

    for (gains.items) |g| {
        try writer.print(" Gained {s}.", .{g.field});
    }

    if (coverage_gap) |cg| {
        const unc = cg.uncovered();
        if (unc > 0) {
            try writer.print(" {d}/{d} path(s) uncovered by traces.", .{ unc, cg.total_new_paths });
        }
    }

    buf = aw.toArrayList();
    return try buf.toOwnedSlice(allocator);
}

// -------------------------------------------------------------------------
// JSON emission
// -------------------------------------------------------------------------

pub fn writeUpgradeManifestJson(manifest: *const UpgradeManifest, writer: anytype) !void {
    try writer.writeAll("{\n");
    try writer.writeAll("  \"verdict\": \"");
    try writer.writeAll(manifest.verdict.toString());
    try writer.writeAll("\",\n");

    try writer.writeAll("  \"justification\": ");
    try handler_contract.writeJsonString(writer, manifest.justification);
    try writer.writeAll(",\n");

    try writer.writeAll("  \"classification\": \"");
    try writer.writeAll(manifest.classification.toString());
    try writer.writeAll("\",\n");

    // Surface
    try writer.writeAll("  \"surface\": {\n");
    try writer.print("    \"routesAdded\": {d},\n", .{manifest.surface.routes_added});
    try writer.print("    \"routesRemoved\": {d},\n", .{manifest.surface.routes_removed});
    try writer.print("    \"routesUnchanged\": {d},\n", .{manifest.surface.routes_unchanged});
    try writer.print("    \"capabilitiesWidened\": {s},\n", .{if (manifest.surface.capabilities_widened) "true" else "false"});
    try writer.print("    \"capabilitiesNarrowed\": {s}\n", .{if (manifest.surface.capabilities_narrowed) "true" else "false"});
    try writer.writeAll("  },\n");

    // Behavioral
    if (manifest.behavioral) |b| {
        try writer.writeAll("  \"behavioral\": {\n");
        try writer.print("    \"preserved\": {d},\n", .{b.preserved});
        try writer.print("    \"responseChanged\": {d},\n", .{b.response_changed});
        try writer.print("    \"removed\": {d},\n", .{b.removed});
        try writer.print("    \"added\": {d}\n", .{b.added});
        try writer.writeAll("  },\n");
    } else {
        try writer.writeAll("  \"behavioral\": null,\n");
    }

    // Replay
    if (manifest.replay) |r| {
        try writer.writeAll("  \"replay\": {\n");
        try writer.print("    \"total\": {d},\n", .{r.total});
        try writer.print("    \"identical\": {d},\n", .{r.identical});
        try writer.print("    \"statusChanged\": {d},\n", .{r.status_changed});
        try writer.print("    \"bodyChanged\": {d},\n", .{r.body_changed});
        try writer.print("    \"diverged\": {d}\n", .{r.diverged});
        try writer.writeAll("  },\n");
    } else {
        try writer.writeAll("  \"replay\": null,\n");
    }

    // Property regressions
    try writer.writeAll("  \"propertyRegressions\": [");
    for (manifest.property_regressions.items, 0..) |r, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("\n    {\"field\": ");
        try handler_contract.writeJsonString(writer, r.field);
        try writer.print(", \"old\": {s}, \"new\": {s}, \"severity\": \"{s}\"", .{
            if (r.old_value) "true" else "false",
            if (r.new_value) "true" else "false",
            r.severity.toString(),
        });
        try writer.writeByte('}');
    }
    if (manifest.property_regressions.items.len > 0) try writer.writeAll("\n  ");
    try writer.writeAll("],\n");

    // Property gains
    try writer.writeAll("  \"propertyGains\": [");
    for (manifest.property_gains.items, 0..) |g, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("\n    {\"field\": ");
        try handler_contract.writeJsonString(writer, g.field);
        try writer.print(", \"old\": {s}, \"new\": {s}\"", .{
            if (g.old_value) "true" else "false",
            if (g.new_value) "true" else "false",
        });
        try writer.writeByte('}');
    }
    if (manifest.property_gains.items.len > 0) try writer.writeAll("\n  ");
    try writer.writeAll("],\n");

    // Coverage gap
    if (manifest.coverage_gap) |cg| {
        try writer.writeAll("  \"coverageGap\": {\n");
        try writer.print("    \"totalPaths\": {d},\n", .{cg.total_new_paths});
        try writer.print("    \"coveredByTraces\": {d},\n", .{cg.covered_by_traces});
        try writer.print("    \"uncovered\": {d}\n", .{cg.uncovered()});
        try writer.writeAll("  }\n");
    } else {
        try writer.writeAll("  \"coverageGap\": null\n");
    }

    try writer.writeAll("}\n");
}

// -------------------------------------------------------------------------
// Report emission
// -------------------------------------------------------------------------

pub fn writeUpgradeReport(manifest: *const UpgradeManifest, writer: anytype) !void {
    try writer.writeAll("UPGRADE VERIFICATION REPORT\n");
    try writer.writeAll("===========================\n\n");

    try writer.writeAll("Verdict: ");
    try writer.writeAll(manifest.verdict.toString());
    try writer.writeAll("\n\n");

    try writer.writeAll(manifest.justification);
    try writer.writeAll("\n");

    if (manifest.behavioral) |b| {
        try writer.writeAll("\nBehavioral Paths:\n");
        try writer.print("  Preserved:        {d}\n", .{b.preserved});
        if (b.response_changed > 0) try writer.print("  Response changed: {d}\n", .{b.response_changed});
        if (b.removed > 0) try writer.print("  Removed:          {d}\n", .{b.removed});
        if (b.added > 0) try writer.print("  Added:            {d}\n", .{b.added});
    }

    if (manifest.property_regressions.items.len > 0) {
        try writer.writeAll("\nProperty Regressions:\n");
        for (manifest.property_regressions.items) |r| {
            try writer.print("  [{s}] {s}: true -> false\n", .{ r.severity.toString(), r.field });
        }
    }

    if (manifest.property_gains.items.len > 0) {
        try writer.writeAll("\nProperty Gains:\n");
        for (manifest.property_gains.items) |g| {
            try writer.print("  {s}: false -> true\n", .{g.field});
        }
    }

    if (manifest.coverage_gap) |cg| {
        const unc = cg.uncovered();
        if (unc > 0) {
            try writer.writeAll("\nCoverage Gap:\n");
            try writer.print("  {d} of {d} paths not covered by traces\n", .{ unc, cg.total_new_paths });
        }
    }
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

test "safe verdict for identical contracts" {
    const verdict = synthesizeVerdict(.equivalent, &.{ .items = &.{} }, null, null);
    try std.testing.expectEqual(UpgradeVerdict.safe, verdict);
}

test "safe_with_additions for additive classification" {
    const verdict = synthesizeVerdict(.additive, &.{ .items = &.{} }, null, null);
    try std.testing.expectEqual(UpgradeVerdict.safe_with_additions, verdict);
}

test "breaking for structural breaking" {
    const verdict = synthesizeVerdict(.breaking, &.{ .items = &.{} }, null, null);
    try std.testing.expectEqual(UpgradeVerdict.breaking, verdict);
}

test "breaking for behavioral removal" {
    const bd = BehaviorDiff{ .preserved = 2, .removed = 1 };
    const verdict = synthesizeVerdict(.equivalent, &.{ .items = &.{} }, bd, null);
    try std.testing.expectEqual(UpgradeVerdict.breaking, verdict);
}

test "breaking for behavioral response change" {
    const bd = BehaviorDiff{ .preserved = 2, .response_changed = 1 };
    const verdict = synthesizeVerdict(.equivalent, &.{ .items = &.{} }, bd, null);
    try std.testing.expectEqual(UpgradeVerdict.breaking, verdict);
}

test "breaking for critical property regression" {
    const regressions = [_]PropertyRegression{
        .{ .field = "retry_safe", .old_value = true, .new_value = false, .severity = .critical },
    };
    const list = std.ArrayList(PropertyRegression){ .items = @constCast(&regressions), .capacity = regressions.len };
    const verdict = synthesizeVerdict(.equivalent, &list, null, null);
    try std.testing.expectEqual(UpgradeVerdict.breaking, verdict);
}

test "needs_review for warning property regression" {
    const regressions = [_]PropertyRegression{
        .{ .field = "deterministic", .old_value = true, .new_value = false, .severity = .warning },
    };
    const list = std.ArrayList(PropertyRegression){ .items = @constCast(&regressions), .capacity = regressions.len };
    const verdict = synthesizeVerdict(.equivalent, &list, null, null);
    try std.testing.expectEqual(UpgradeVerdict.needs_review, verdict);
}

test "safe_with_additions for new behavioral paths" {
    const bd = BehaviorDiff{ .preserved = 3, .added = 2 };
    const verdict = synthesizeVerdict(.equivalent, &.{ .items = &.{} }, bd, null);
    try std.testing.expectEqual(UpgradeVerdict.safe_with_additions, verdict);
}

test "property severity classification" {
    try std.testing.expectEqual(Severity.critical, propertySeverity("retry_safe", false));
    try std.testing.expectEqual(Severity.critical, propertySeverity("injection_safe", false));
    try std.testing.expectEqual(Severity.critical, propertySeverity("result_safe", false));
    try std.testing.expectEqual(Severity.critical, propertySeverity("optional_safe", false));
    try std.testing.expectEqual(Severity.warning, propertySeverity("deterministic", false));
    try std.testing.expectEqual(Severity.info, propertySeverity("pure", false));
    // Gains are always info regardless of field
    try std.testing.expectEqual(Severity.info, propertySeverity("retry_safe", true));
}
