//! Shared property goal driveability classification.
//!
//! The autoloop can only drive properties that the counterexample solver
//! models. Other compiler-computed properties are still useful state, but
//! they are structural facts today rather than goal-check / repair-plan goals.

const std = @import("std");
const zigts = @import("zigts");
const ui_payload = @import("ui_payload.zig");

const counterexample = zigts.counterexample;

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

pub fn classify(name: []const u8) Driveability {
    if (std.meta.stringToEnum(counterexample.PropertyTag, name) != null) return .goal_driveable;
    if (boolPropertyIndexOf(name) != null) return .structural;
    return .unknown;
}

pub fn isGoalDriveable(name: []const u8) bool {
    return classify(name) == .goal_driveable;
}

pub fn isStructural(name: []const u8) bool {
    return classify(name) == .structural;
}

/// Comma-separated PropertyTag names, derived from the enum at comptime so
/// user-facing messages stay in sync if a tag is added or removed.
pub const supported_goal_list = blk: {
    var out: []const u8 = "";
    for (@typeInfo(counterexample.PropertyTag).@"enum".fields, 0..) |field, i| {
        out = if (i == 0) field.name else out ++ ", " ++ field.name;
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

test "counterexample property tags are goal-driveable" {
    inline for (@typeInfo(counterexample.PropertyTag).@"enum".fields) |field| {
        try testing.expectEqual(Driveability.goal_driveable, classify(field.name));
        try testing.expect(isGoalDriveable(field.name));
    }
}

test "compiler-only boolean properties are structural" {
    try testing.expectEqual(Driveability.structural, classify("pure"));
    try testing.expectEqual(Driveability.structural, classify("retry_safe"));
    try testing.expectEqual(Driveability.structural, classify("deterministic"));
    try testing.expect(isStructural("pure"));
}

test "unknown or non-boolean properties are not driveable" {
    try testing.expectEqual(Driveability.unknown, classify("totally_not_a_property"));
    try testing.expectEqual(Driveability.unknown, classify("max_io_depth"));
}
