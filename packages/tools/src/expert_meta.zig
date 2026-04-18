//! Single source of truth for the `zigts meta` v1 envelope.
//! See docs/zigts-expert-contract.md.

const std = @import("std");
const zigts = @import("zigts");
const rule_registry = zigts.rule_registry;

pub const compiler_version = "0.16.0";
pub const policy_version = "2026.04.2";
pub const mode = "embedded";

pub const Categories = struct {
    verifier: usize,
    policy: usize,
    property: usize,
};

pub const MetaInfo = struct {
    compiler_version: []const u8,
    policy_version: []const u8,
    policy_hash: [64]u8,
    rule_count: usize,
    categories: Categories,
    mode: []const u8,
};

pub const category_counts: Categories = blk: {
    var v: usize = 0;
    var p: usize = 0;
    var pr: usize = 0;
    for (&rule_registry.all_rules) |*rule| {
        switch (rule.category) {
            .verifier => v += 1,
            .policy => p += 1,
            .property => pr += 1,
        }
    }
    break :blk .{ .verifier = v, .policy = p, .property = pr };
};

pub fn compute() MetaInfo {
    return .{
        .compiler_version = compiler_version,
        .policy_version = policy_version,
        .policy_hash = rule_registry.policyHash(),
        .rule_count = rule_registry.all_rules.len,
        .categories = category_counts,
        .mode = mode,
    };
}

pub fn writeJson(writer: anytype, info: *const MetaInfo) !void {
    try writer.print(
        "{{\"compiler_version\":\"{s}\",\"policy_version\":\"{s}\",\"policy_hash\":\"{s}\",\"rule_count\":{d},\"categories\":{{\"verifier\":{d},\"policy\":{d},\"property\":{d}}},\"mode\":\"{s}\"}}\n",
        .{
            info.compiler_version,
            info.policy_version,
            info.policy_hash,
            info.rule_count,
            info.categories.verifier,
            info.categories.policy,
            info.categories.property,
            info.mode,
        },
    );
}

pub fn writeText(writer: anytype, info: *const MetaInfo) !void {
    try writer.print(
        \\zigts policy
        \\  compiler: {s}
        \\  policy:   {s}
        \\  hash:     {s}
        \\  rules:    {d} ({d} verifier, {d} policy, {d} property)
        \\  mode:     {s}
        \\
    , .{
        info.compiler_version,
        info.policy_version,
        info.policy_hash,
        info.rule_count,
        info.categories.verifier,
        info.categories.policy,
        info.categories.property,
        info.mode,
    });
}

test "compute fills all fields" {
    const info = compute();
    try std.testing.expectEqualStrings("0.16.0", info.compiler_version);
    try std.testing.expectEqualStrings("2026.04.2", info.policy_version);
    try std.testing.expectEqualStrings("embedded", info.mode);
    try std.testing.expect(info.rule_count > 0);
    try std.testing.expectEqual(info.rule_count, info.categories.verifier + info.categories.policy + info.categories.property);
}

test "writeJson produces parseable v1 envelope" {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &buf);

    const info = compute();
    try writeJson(&aw.writer, &info);

    buf = aw.toArrayList();
    const s = buf.items;

    try std.testing.expect(std.mem.indexOf(u8, s, "\"compiler_version\":\"0.16.0\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"policy_version\":\"2026.04.2\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"mode\":\"embedded\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"categories\":{") != null);
    try std.testing.expectEqual(@as(u8, '\n'), s[s.len - 1]);
}

test "writeText emits the human-readable report with pinned versions" {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &buf);

    const info = compute();
    try writeText(&aw.writer, &info);

    buf = aw.toArrayList();
    const s = buf.items;

    try std.testing.expect(std.mem.indexOf(u8, s, "zigts policy") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "compiler: 0.16.0") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "policy:   2026.04.2") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "mode:     embedded") != null);
}
