//! Shared property goal driveability classification.
//!
//! The autoloop can only drive properties that the counterexample solver
//! models. Other compiler-computed properties are still useful state, but
//! they are structural facts today rather than goal-check / repair-plan goals.

const std = @import("std");
const zigts = @import("zigts");
const ui_payload = @import("ui_payload.zig");

const counterexample = zigts.counterexample;

pub const supported_goals = [_]counterexample.PropertyTag{
    .no_secret_leakage,
    .no_credential_leakage,
    .injection_safe,
};

pub const Driveability = enum {
    goal_driveable,
    structural,
    unknown,

    pub fn label(self: Driveability) []const u8 {
        return switch (self) {
            .goal_driveable => "goal",
            .structural => "struct",
            .unknown => "unknown",
        };
    }
};

pub fn parseDriveableGoal(name: []const u8) ?counterexample.PropertyTag {
    const tag = std.meta.stringToEnum(counterexample.PropertyTag, name) orelse return null;
    for (supported_goals) |supported| {
        if (tag == supported) return tag;
    }
    return null;
}

pub fn classify(name: []const u8) Driveability {
    if (parseDriveableGoal(name) != null) return .goal_driveable;
    if (boolPropertyIndexOf(name) != null) return .structural;
    return .unknown;
}

pub fn isGoalDriveable(name: []const u8) bool {
    return classify(name) == .goal_driveable;
}

pub fn isStructural(name: []const u8) bool {
    return classify(name) == .structural;
}

/// Comma-separated supported goal names, derived from the canonical goal set
/// so user-facing messages stay in sync with the actual repairable surface.
pub const supported_goal_list = blk: {
    var out: []const u8 = "";
    for (supported_goals, 0..) |goal, i| {
        const name = @tagName(goal);
        out = if (i == 0) name else out ++ ", " ++ name;
    }
    break :blk out;
};

pub const bool_property_count: usize = blk: {
    var count: usize = 0;
    for (@typeInfo(ui_payload.PropertiesSnapshot).@"struct".fields) |field| {
        if (field.type == bool) count += 1;
    }
    break :blk count;
};

pub fn boolPropertyNameAt(index: usize) ?[]const u8 {
    var current: usize = 0;
    inline for (@typeInfo(ui_payload.PropertiesSnapshot).@"struct".fields) |field| {
        if (field.type == bool) {
            if (current == index) return field.name;
            current += 1;
        }
    }
    return null;
}

pub fn boolPropertyIndexOf(name: []const u8) ?usize {
    var current: usize = 0;
    inline for (@typeInfo(ui_payload.PropertiesSnapshot).@"struct".fields) |field| {
        if (field.type == bool) {
            if (std.mem.eql(u8, field.name, name)) return current;
            current += 1;
        }
    }
    return null;
}

const testing = std.testing;

test "solver-backed property tags are goal-driveable" {
    for (supported_goals) |goal| {
        try testing.expectEqual(Driveability.goal_driveable, classify(@tagName(goal)));
        try testing.expect(isGoalDriveable(@tagName(goal)));
        try testing.expectEqual(goal, parseDriveableGoal(@tagName(goal)).?);
    }
}

test "compiler-only boolean properties are structural" {
    try testing.expectEqual(Driveability.structural, classify("pure"));
    try testing.expectEqual(Driveability.structural, classify("retry_safe"));
    try testing.expectEqual(Driveability.structural, classify("deterministic"));
    try testing.expectEqual(Driveability.structural, classify("input_validated"));
    try testing.expectEqual(Driveability.structural, classify("pii_contained"));
    try testing.expect(isStructural("pure"));
    try testing.expect(!isGoalDriveable("input_validated"));
    try testing.expect(!isGoalDriveable("pii_contained"));
}

test "unknown or non-boolean properties are not driveable" {
    try testing.expectEqual(Driveability.unknown, classify("totally_not_a_property"));
    try testing.expectEqual(Driveability.unknown, classify("max_io_depth"));
}

test "supported goal list excludes structural property tags" {
    try testing.expect(std.mem.indexOf(u8, supported_goal_list, "no_secret_leakage") != null);
    try testing.expect(std.mem.indexOf(u8, supported_goal_list, "no_credential_leakage") != null);
    try testing.expect(std.mem.indexOf(u8, supported_goal_list, "injection_safe") != null);
    try testing.expect(std.mem.indexOf(u8, supported_goal_list, "input_validated") == null);
    try testing.expect(std.mem.indexOf(u8, supported_goal_list, "pii_contained") == null);
}
