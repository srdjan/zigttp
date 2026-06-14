/// Centralized registry of module_state slot indices.
/// Context.module_state is a fixed-size [16]?ModuleStateEntry array.
/// Each slot is reserved for a specific subsystem to avoid collisions.
pub const Slot = enum(u4) {
    durable = 0,
    durable_api = 1,
    sql = 2,
    replay = 3,
    validate = 4,
    cache = 5,
    io = 6,
    trace = 7,
    ratelimit = 8,
    service = 9,
    scope = 10,
    fetch = 11,
    websocket = 12,
};

pub fn ownerSpecifier(slot: usize) ?[]const u8 {
    return switch (slot) {
        @intFromEnum(Slot.durable), @intFromEnum(Slot.durable_api) => "zigttp:durable",
        @intFromEnum(Slot.sql) => "zigttp:sql",
        @intFromEnum(Slot.validate) => "zigttp:validate",
        @intFromEnum(Slot.cache) => "zigttp:cache",
        @intFromEnum(Slot.io) => "zigttp:io",
        @intFromEnum(Slot.ratelimit) => "zigttp:ratelimit",
        @intFromEnum(Slot.service) => "zigttp:service",
        @intFromEnum(Slot.scope) => "zigttp:scope",
        @intFromEnum(Slot.fetch) => "zigttp:fetch",
        @intFromEnum(Slot.websocket) => "zigttp:websocket",
        else => null,
    };
}

pub fn isOwnedBySpecifier(slot: usize, specifier: []const u8) bool {
    const owner = ownerSpecifier(slot) orelse return false;
    return @import("std").mem.eql(u8, owner, specifier);
}

test "module state slots are owned by one active module specifier" {
    const testing = @import("std").testing;

    try testing.expect(isOwnedBySpecifier(@intFromEnum(Slot.sql), "zigttp:sql"));
    try testing.expect(!isOwnedBySpecifier(@intFromEnum(Slot.sql), "zigttp:cache"));
    try testing.expect(isOwnedBySpecifier(@intFromEnum(Slot.validate), "zigttp:validate"));
    try testing.expect(ownerSpecifier(@intFromEnum(Slot.replay)) == null);
    try testing.expect(ownerSpecifier(15) == null);
}
