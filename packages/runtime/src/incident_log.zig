//! incident_log.zig — opt-in JSONL sink for runtime soundness incidents.
//!
//! A soundness incident is a runtime fault on a path the compiler proved safe
//! (every guarding chip was discharged, yet the handler still faulted). When
//! `--incident-log <file>` is set, each incident is appended as one JSON line
//! through an O_APPEND fd, so concurrent writes from pooled runtimes stay atomic
//! for the small line sizes involved. Writing is best-effort and never fails the
//! request path.

const std = @import("std");
const zq = @import("zts");

pub fn open(allocator: std.mem.Allocator, path: []const u8) !std.c.fd_t {
    return zq.file_io.openAppend(allocator, path);
}

/// Append one incident record. Best-effort: on any allocation or write error it
/// silently drops the record rather than disturbing the response path.
pub fn write(
    allocator: std.mem.Allocator,
    fd: std.c.fd_t,
    method: []const u8,
    path: []const u8,
    proven_chips: []const []const u8,
    detail: []const u8,
) void {
    var line: std.ArrayList(u8) = .empty;
    defer line.deinit(allocator);
    build(allocator, &line, method, path, proven_chips, detail) catch return;
    // Single small write under O_APPEND is atomic across pooled runtimes.
    _ = std.c.write(fd, line.items.ptr, line.items.len);
}

/// Build one JSON line (no fd needed) so the encoding is unit-testable.
pub fn build(
    allocator: std.mem.Allocator,
    out: *std.ArrayList(u8),
    method: []const u8,
    path: []const u8,
    proven_chips: []const []const u8,
    detail: []const u8,
) !void {
    try out.appendSlice(allocator, "{\"kind\":\"soundness_incident\",\"method\":\"");
    try appendEscaped(allocator, out, method);
    try out.appendSlice(allocator, "\",\"path\":\"");
    try appendEscaped(allocator, out, path);
    try out.appendSlice(allocator, "\",\"proven\":[");
    for (proven_chips, 0..) |chip, i| {
        if (i > 0) try out.append(allocator, ',');
        try out.append(allocator, '"');
        try appendEscaped(allocator, out, chip);
        try out.append(allocator, '"');
    }
    try out.appendSlice(allocator, "],\"detail\":\"");
    try appendEscaped(allocator, out, detail);
    try out.appendSlice(allocator, "\"}\n");
}

fn appendEscaped(allocator: std.mem.Allocator, out: *std.ArrayList(u8), s: []const u8) !void {
    // Index-based so a multi-byte UTF-8 sequence can be consumed as a unit. The
    // dynamic fields (method, path) come from an unvalidated request line, so a
    // lone high byte must never be emitted raw: that would make the JSONL line
    // invalid UTF-8 and rejected by strict RFC 8259 parsers.
    var i: usize = 0;
    while (i < s.len) {
        const c = s[i];
        switch (c) {
            '"' => {
                try out.appendSlice(allocator, "\\\"");
                i += 1;
            },
            '\\' => {
                try out.appendSlice(allocator, "\\\\");
                i += 1;
            },
            '\n' => {
                try out.appendSlice(allocator, "\\n");
                i += 1;
            },
            '\r' => {
                try out.appendSlice(allocator, "\\r");
                i += 1;
            },
            '\t' => {
                try out.appendSlice(allocator, "\\t");
                i += 1;
            },
            else => {
                if (c < 0x20) {
                    var b: [8]u8 = undefined;
                    const esc = std.fmt.bufPrint(&b, "\\u{x:0>4}", .{c}) catch {
                        i += 1;
                        continue;
                    };
                    try out.appendSlice(allocator, esc);
                    i += 1;
                } else if (c < 0x80) {
                    try out.append(allocator, c);
                    i += 1;
                } else {
                    // Copy a complete, valid multi-byte sequence through verbatim;
                    // substitute U+FFFD for an invalid/truncated one and advance a
                    // single byte, keeping the emitted line valid UTF-8.
                    const seq_len = std.unicode.utf8ByteSequenceLength(c) catch {
                        try out.appendSlice(allocator, "\u{FFFD}");
                        i += 1;
                        continue;
                    };
                    if (i + seq_len <= s.len and std.unicode.utf8ValidateSlice(s[i .. i + seq_len])) {
                        try out.appendSlice(allocator, s[i .. i + seq_len]);
                        i += seq_len;
                    } else {
                        try out.appendSlice(allocator, "\u{FFFD}");
                        i += 1;
                    }
                }
            },
        }
    }
}

test "build emits one JSON line with the incident fields" {
    const allocator = std.testing.allocator;
    var line: std.ArrayList(u8) = .empty;
    defer line.deinit(allocator);
    try build(allocator, &line, "GET", "/users", &.{ "optional_safe", "result_safe" }, "NotCallable");

    const s = line.items;
    try std.testing.expect(std.mem.endsWith(u8, s, "\n"));
    try std.testing.expect(std.mem.indexOf(u8, s, "\"kind\":\"soundness_incident\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"method\":\"GET\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"path\":\"/users\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"proven\":[\"optional_safe\",\"result_safe\"]") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"detail\":\"NotCallable\"") != null);
}

test "build escapes quotes and control characters in dynamic fields" {
    const allocator = std.testing.allocator;
    var line: std.ArrayList(u8) = .empty;
    defer line.deinit(allocator);
    try build(allocator, &line, "GET", "/a\"b", &.{}, "line1\nq\"uote");

    const s = line.items;
    try std.testing.expect(std.mem.indexOf(u8, s, "/a\\\"b") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "line1\\nq\\\"uote") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"proven\":[]") != null);
}

test "appendEscaped sanitizes invalid UTF-8 and preserves valid multibyte" {
    const allocator = std.testing.allocator;
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(allocator);

    // A lone high byte (as an unvalidated request path could carry) must be
    // replaced by U+FFFD, never emitted raw, so the JSONL line stays valid UTF-8.
    try appendEscaped(allocator, &out, "a\xE9b");
    try std.testing.expect(std.mem.indexOf(u8, out.items, "\u{FFFD}") != null);
    try std.testing.expect(std.mem.indexOfScalar(u8, out.items, 0xE9) == null);
    try std.testing.expect(std.unicode.utf8ValidateSlice(out.items));

    // A complete, valid multibyte sequence (café; é = 0xC3 0xA9) passes through.
    out.clearRetainingCapacity();
    try appendEscaped(allocator, &out, "caf\xC3\xA9");
    try std.testing.expect(std.mem.indexOf(u8, out.items, "\xC3\xA9") != null);
    try std.testing.expect(std.unicode.utf8ValidateSlice(out.items));
}

test "write appends one line per incident to an O_APPEND fd" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}/inc.jsonl", .{tmp.sub_path});
    defer allocator.free(path);

    const fd = try open(allocator, path);
    write(allocator, fd, "POST", "/x", &.{"optional_safe"}, "NotCallable");
    write(allocator, fd, "GET", "/y", &.{}, "TypeError");
    std.Io.Threaded.closeFd(fd);

    const contents = try zq.file_io.readFile(allocator, path, 64 * 1024);
    defer allocator.free(contents);

    var lines: usize = 0;
    var it = std.mem.tokenizeScalar(u8, contents, '\n');
    while (it.next()) |_| lines += 1;
    try std.testing.expectEqual(@as(usize, 2), lines);
    try std.testing.expect(std.mem.indexOf(u8, contents, "\"path\":\"/x\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, contents, "\"path\":\"/y\"") != null);
}
