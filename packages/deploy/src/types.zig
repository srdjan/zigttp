const std = @import("std");

pub const Provider = enum {
    northflank,

    pub fn fromString(raw: []const u8) ?Provider {
        if (std.mem.eql(u8, raw, "northflank")) return .northflank;
        return null;
    }

    pub fn toString(self: Provider) []const u8 {
        return @tagName(self);
    }
};
