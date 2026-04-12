const std = @import("std");

pub fn redactSecret(value: []const u8) []const u8 {
    if (value.len == 0) return "<empty>";
    return "<redacted>";
}

pub fn looksSensitiveKey(key: []const u8) bool {
    return containsAscii(key, "SECRET") or
        containsAscii(key, "TOKEN") or
        containsAscii(key, "KEY") or
        containsAscii(key, "PASSWORD") or
        containsAscii(key, "PRIVATE");
}

fn containsAscii(haystack: []const u8, needle: []const u8) bool {
    var upper = std.ArrayList(u8).empty;
    defer upper.deinit(std.heap.smp_allocator);
    upper.appendSlice(std.heap.smp_allocator, haystack) catch return false;
    std.ascii.upperString(upper.items, haystack);
    return std.mem.indexOf(u8, upper.items, needle) != null;
}

test "redact secret hides non-empty values" {
    try std.testing.expectEqualStrings("<redacted>", redactSecret("abc"));
    try std.testing.expectEqualStrings("<empty>", redactSecret(""));
}

test "looksSensitiveKey detects common secret names" {
    try std.testing.expect(looksSensitiveKey("API_TOKEN"));
    try std.testing.expect(looksSensitiveKey("privateKey"));
    try std.testing.expect(!looksSensitiveKey("PORT"));
}
