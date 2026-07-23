//! Shared URL route pattern matching utilities.
//!
//! Provides parameter-aware path comparison where `:id` and `{id}` style
//! segments are treated as equivalent wildcards. Used by fault coverage,
//! manifest alignment, and property expectations.

const std = @import("std");

/// Returns true if the segment is a URL parameter placeholder
/// (`:param` or `{param}` style).
pub fn isParamSegment(seg: []const u8) bool {
    if (seg.len == 0) return false;
    if (seg[0] == ':') return true;
    if (seg.len >= 2 and seg[0] == '{' and seg[seg.len - 1] == '}') return true;
    return false;
}

/// Compare two URL paths segment-by-segment, treating parameter segments
/// as wildcards. `/api/orders/:id` matches `/api/orders/{id}`.
pub fn pathsMatch(a: []const u8, b: []const u8) bool {
    var a_iter = std.mem.splitScalar(u8, a, '/');
    var b_iter = std.mem.splitScalar(u8, b, '/');

    while (true) {
        const a_seg = a_iter.next();
        const b_seg = b_iter.next();

        if (a_seg == null and b_seg == null) return true;
        if (a_seg == null or b_seg == null) return false;

        const sa = a_seg.?;
        const sb = b_seg.?;

        if (isParamSegment(sa) or isParamSegment(sb)) continue;

        if (!std.mem.eql(u8, sa, sb)) return false;
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "pathsMatch: identical static paths" {
    try std.testing.expect(pathsMatch("/api/orders", "/api/orders"));
}

test "pathsMatch: colon and brace params match" {
    try std.testing.expect(pathsMatch("/api/orders/:id", "/api/orders/{id}"));
}

test "pathsMatch: param matches any literal" {
    try std.testing.expect(pathsMatch("/api/orders/:id", "/api/orders/123"));
}

test "pathsMatch: different segment counts" {
    try std.testing.expect(!pathsMatch("/api/orders", "/api/orders/:id"));
}

test "pathsMatch: different literal segments" {
    try std.testing.expect(!pathsMatch("/api/orders", "/api/items"));
}

test "isParamSegment: colon prefix" {
    try std.testing.expect(isParamSegment(":id"));
}

test "isParamSegment: brace style" {
    try std.testing.expect(isParamSegment("{id}"));
}

test "isParamSegment: literal" {
    try std.testing.expect(!isParamSegment("orders"));
}

test "isParamSegment: empty" {
    try std.testing.expect(!isParamSegment(""));
}
