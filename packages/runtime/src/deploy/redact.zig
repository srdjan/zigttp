const std = @import("std");

pub fn redactSecret(value: []const u8) []const u8 {
    if (value.len == 0) return "<empty>";
    return "<redacted>";
}

test "redact secret hides non-empty values" {
    try std.testing.expectEqualStrings("<redacted>", redactSecret("abc"));
    try std.testing.expectEqualStrings("<empty>", redactSecret(""));
}
