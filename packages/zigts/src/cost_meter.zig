//! Request-local virtual-module call counter.
//!
//! This module is deliberately self-contained so the hot capability-wrapper
//! path can classify module specifiers at comptime without importing the
//! virtual-module registry.

const std = @import("std");

pub const ModuleClass = enum {
    env,
    crypto,
    router,
    auth,
    validate,
    decode,
    cache,
    sql,
    service,
    fetch,
    websocket,
    io,
    durable,
    compose,
    scope,
    url,
    id,
    http,
    log,
    text,
    time,
    ratelimit,
    other,
};

pub const class_count = std.enums.values(ModuleClass).len;

fn classForNameComptime(comptime name: []const u8) ModuleClass {
    @setEvalBranchQuota(4096);
    inline for (std.enums.values(ModuleClass)) |class| {
        if (class == .other) continue;
        if (std.mem.eql(u8, name, @tagName(class))) return class;
    }
    return .other;
}

pub fn classForSpecifier(comptime specifier: []const u8) ModuleClass {
    const prefix = "zigttp:";
    if (std.mem.startsWith(u8, specifier, prefix)) {
        return classForNameComptime(specifier[prefix.len..]);
    }
    return .other;
}

pub fn classForName(name: []const u8) ModuleClass {
    inline for (std.enums.values(ModuleClass)) |class| {
        if (class == .other) continue;
        if (std.mem.eql(u8, name, @tagName(class))) return class;
    }
    return .other;
}

pub const Meter = struct {
    counts: [class_count]u32 = @splat(0),

    pub inline fn bump(self: *Meter, class: ModuleClass) void {
        self.counts[@intFromEnum(class)] +|= 1;
    }

    pub fn reset(self: *Meter) void {
        self.counts = @splat(0);
    }

    pub inline fn count(self: *const Meter, class: ModuleClass) u32 {
        return self.counts[@intFromEnum(class)];
    }

    pub fn total(self: *const Meter) u32 {
        var sum: u32 = 0;
        for (self.counts) |n| {
            sum +|= n;
        }
        return sum;
    }
};

test "classForSpecifier maps builtin specifiers and defaults to other" {
    try std.testing.expectEqual(ModuleClass.env, comptime classForSpecifier("zigttp:env"));
    try std.testing.expectEqual(ModuleClass.crypto, comptime classForSpecifier("zigttp:crypto"));
    try std.testing.expectEqual(ModuleClass.router, comptime classForSpecifier("zigttp:router"));
    try std.testing.expectEqual(ModuleClass.auth, comptime classForSpecifier("zigttp:auth"));
    try std.testing.expectEqual(ModuleClass.validate, comptime classForSpecifier("zigttp:validate"));
    try std.testing.expectEqual(ModuleClass.decode, comptime classForSpecifier("zigttp:decode"));
    try std.testing.expectEqual(ModuleClass.cache, comptime classForSpecifier("zigttp:cache"));
    try std.testing.expectEqual(ModuleClass.sql, comptime classForSpecifier("zigttp:sql"));
    try std.testing.expectEqual(ModuleClass.service, comptime classForSpecifier("zigttp:service"));
    try std.testing.expectEqual(ModuleClass.fetch, comptime classForSpecifier("zigttp:fetch"));
    try std.testing.expectEqual(ModuleClass.websocket, comptime classForSpecifier("zigttp:websocket"));
    try std.testing.expectEqual(ModuleClass.io, comptime classForSpecifier("zigttp:io"));
    try std.testing.expectEqual(ModuleClass.durable, comptime classForSpecifier("zigttp:durable"));
    try std.testing.expectEqual(ModuleClass.compose, comptime classForSpecifier("zigttp:compose"));
    try std.testing.expectEqual(ModuleClass.scope, comptime classForSpecifier("zigttp:scope"));
    try std.testing.expectEqual(ModuleClass.url, comptime classForSpecifier("zigttp:url"));
    try std.testing.expectEqual(ModuleClass.id, comptime classForSpecifier("zigttp:id"));
    try std.testing.expectEqual(ModuleClass.http, comptime classForSpecifier("zigttp:http"));
    try std.testing.expectEqual(ModuleClass.log, comptime classForSpecifier("zigttp:log"));
    try std.testing.expectEqual(ModuleClass.text, comptime classForSpecifier("zigttp:text"));
    try std.testing.expectEqual(ModuleClass.time, comptime classForSpecifier("zigttp:time"));
    try std.testing.expectEqual(ModuleClass.ratelimit, comptime classForSpecifier("zigttp:ratelimit"));
    try std.testing.expectEqual(ModuleClass.other, comptime classForSpecifier("zigttp:workflow"));
    try std.testing.expectEqual(ModuleClass.other, comptime classForSpecifier("partner:foo"));
    try std.testing.expectEqual(ModuleClass.sql, classForName("sql"));
    try std.testing.expectEqual(ModuleClass.other, classForName("workflow"));
}

test "meter bump/reset/total" {
    var meter = Meter{};
    try std.testing.expectEqual(@as(u32, 0), meter.total());

    meter.bump(.sql);
    meter.bump(.sql);
    meter.bump(.cache);

    try std.testing.expectEqual(@as(u32, 2), meter.count(.sql));
    try std.testing.expectEqual(@as(u32, 1), meter.count(.cache));
    try std.testing.expectEqual(@as(u32, 3), meter.total());

    meter.reset();
    try std.testing.expectEqual(@as(u32, 0), meter.count(.sql));
    try std.testing.expectEqual(@as(u32, 0), meter.total());
}
