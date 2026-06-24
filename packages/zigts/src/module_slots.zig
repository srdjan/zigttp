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
    workflow = 13,
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
        @intFromEnum(Slot.workflow) => "zigttp:workflow",
        else => null,
    };
}

pub fn isOwnedBySpecifier(slot: usize, specifier: []const u8) bool {
    const std = @import("std");
    const owner = ownerSpecifier(slot) orelse return false;
    if (std.mem.eql(u8, owner, specifier)) return true;
    // `zigttp:decode` is a thin facade over `zigttp:validate`: decodeJson /
    // decodeForm / decodeQuery / decodeFormMultipart delegate to the shared
    // schema registry stored in the validate slot. Grant decode access to that
    // one slot so a decode call can reach the registry that schemaCompile
    // (zigttp:validate) populated; without this the module-state ownership guard
    // makes setModuleState/getModuleState fail and every schema-based decode
    // returns an "internal error" Result (and schemaCompile silently no-ops).
    if (slot == @intFromEnum(Slot.validate) and std.mem.eql(u8, specifier, "zigttp:decode")) return true;
    return false;
}

test "module state slots are owned by one active module specifier" {
    const testing = @import("std").testing;

    try testing.expect(isOwnedBySpecifier(@intFromEnum(Slot.sql), "zigttp:sql"));
    try testing.expect(!isOwnedBySpecifier(@intFromEnum(Slot.sql), "zigttp:cache"));
    try testing.expect(isOwnedBySpecifier(@intFromEnum(Slot.validate), "zigttp:validate"));
    // zigttp:decode co-owns the validate slot (shared schema registry), but no other.
    try testing.expect(isOwnedBySpecifier(@intFromEnum(Slot.validate), "zigttp:decode"));
    try testing.expect(!isOwnedBySpecifier(@intFromEnum(Slot.sql), "zigttp:decode"));
    try testing.expect(!isOwnedBySpecifier(@intFromEnum(Slot.cache), "zigttp:decode"));
    try testing.expect(ownerSpecifier(@intFromEnum(Slot.replay)) == null);
    try testing.expect(ownerSpecifier(15) == null);
}
