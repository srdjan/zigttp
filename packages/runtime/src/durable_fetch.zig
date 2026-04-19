//! Oplog-step helper for `zigttp:fetch` with `durable: { key, retries,
//! backoff, ttl_s }`. Pure functions here: hashing, JSON
//! serialization, filesystem round-trip. The driver that decides when
//! to call these lives in `zruntime.zig` next to the HTTP path.
//!
//! Storage layout: `<durable>/fetch/<hash>.step` — one JSON file per
//! idempotency key. The hash folds the key with the method + URL +
//! body so two callers sharing a key but hitting different endpoints
//! don't collide. `at_ms` inside the file drives TTL; `ttl_s` is the
//! caller-provided cap.

const std = @import("std");
const zq = @import("zigts");

pub const Backoff = enum { none, exponential };

pub const Options = struct {
    key: []const u8,
    retries: u32 = 0,
    backoff: Backoff = .none,
    ttl_s: u32 = 3600,
};

pub const Entry = struct {
    at_ms: i64,
    status: u16,
    status_text: []u8,
    content_type: ?[]u8,
    body: []u8,

    pub fn deinit(self: *Entry, allocator: std.mem.Allocator) void {
        allocator.free(self.status_text);
        if (self.content_type) |ct| allocator.free(ct);
        allocator.free(self.body);
    }
};

/// Stable hash over the logical request. Identical inputs across
/// processes produce identical hex output; changing any component
/// (key, method, URL, body bytes) picks a fresh cache slot.
pub fn computeRequestHash(
    key: []const u8,
    method: []const u8,
    url: []const u8,
    body: []const u8,
) [64]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(key);
    hasher.update(&.{0});
    hasher.update(method);
    hasher.update(&.{0});
    hasher.update(url);
    hasher.update(&.{0});
    hasher.update(body);
    var digest: [32]u8 = undefined;
    hasher.final(&digest);
    return std.fmt.bytesToHex(digest, .lower);
}

/// Serialize `entry` + `at_ms` as JSON and write to `<dir>/<hash>.step`
/// atomically via tmp+rename. The body is base64-encoded so binary
/// responses survive the JSON round-trip unchanged.
pub fn writeEntry(
    allocator: std.mem.Allocator,
    dir: []const u8,
    hash_hex: []const u8,
    entry: Entry,
) !void {
    const tmp_path = try std.fmt.allocPrint(allocator, "{s}/{s}.step.tmp", .{ dir, hash_hex });
    defer allocator.free(tmp_path);
    const final_path = try std.fmt.allocPrint(allocator, "{s}/{s}.step", .{ dir, hash_hex });
    defer allocator.free(final_path);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    try renderEntryJson(allocator, &buf, entry);
    try zq.file_io.writeFile(allocator, tmp_path, buf.items);

    const tmp_z = try allocator.dupeZ(u8, tmp_path);
    defer allocator.free(tmp_z);
    const final_z = try allocator.dupeZ(u8, final_path);
    defer allocator.free(final_z);
    if (std.c.rename(tmp_z, final_z) != 0) {
        _ = std.c.unlink(tmp_z);
        return error.CacheRenameFailed;
    }
}

/// Load and TTL-check the cached entry for `<dir>/<hash>.step`. Returns
/// null when the file is absent or has expired (expired files are
/// unlinked as a side effect so the dir doesn't grow unbounded).
pub fn loadEntry(
    allocator: std.mem.Allocator,
    dir: []const u8,
    hash_hex: []const u8,
    ttl_s: u32,
    now_ms: i64,
) !?Entry {
    const path = try std.fmt.allocPrint(allocator, "{s}/{s}.step", .{ dir, hash_hex });
    defer allocator.free(path);

    const source = zq.file_io.readFile(allocator, path, 4 * 1024 * 1024) catch |err| switch (err) {
        error.FileNotFound => return null,
        else => return err,
    };
    defer allocator.free(source);

    var entry = try parseEntryJson(allocator, source);
    errdefer entry.deinit(allocator);

    const expires_ms = std.math.add(i64, entry.at_ms, @as(i64, ttl_s) * 1000) catch std.math.maxInt(i64);
    if (expires_ms <= now_ms) {
        entry.deinit(allocator);
        unlinkPath(allocator, path);
        return null;
    }

    return entry;
}

// ---------------------------------------------------------------------------
// JSON shape (stable): {"v":1,"at_ms":N,"status":N,"status_text":"...",
//                      "content_type":"..."|null,"body_b64":"..."}
// ---------------------------------------------------------------------------

fn renderEntryJson(
    allocator: std.mem.Allocator,
    buf: *std.ArrayList(u8),
    entry: Entry,
) !void {
    var num_buf: [32]u8 = undefined;
    try buf.appendSlice(allocator, "{\"v\":1,\"at_ms\":");
    try buf.appendSlice(allocator, try std.fmt.bufPrint(&num_buf, "{d}", .{entry.at_ms}));
    try buf.appendSlice(allocator, ",\"status\":");
    try buf.appendSlice(allocator, try std.fmt.bufPrint(&num_buf, "{d}", .{entry.status}));
    try buf.appendSlice(allocator, ",\"status_text\":\"");
    try appendEscaped(buf, allocator, entry.status_text);
    try buf.appendSlice(allocator, "\",\"content_type\":");
    if (entry.content_type) |ct| {
        try buf.append(allocator, '"');
        try appendEscaped(buf, allocator, ct);
        try buf.append(allocator, '"');
    } else {
        try buf.appendSlice(allocator, "null");
    }
    try buf.appendSlice(allocator, ",\"body_b64\":\"");
    const b64_len = std.base64.standard.Encoder.calcSize(entry.body.len);
    const b64_buf = try allocator.alloc(u8, b64_len);
    defer allocator.free(b64_buf);
    _ = std.base64.standard.Encoder.encode(b64_buf, entry.body);
    try buf.appendSlice(allocator, b64_buf);
    try buf.appendSlice(allocator, "\"}");
}

fn parseEntryJson(allocator: std.mem.Allocator, source: []const u8) !Entry {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, source, .{});
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidCacheEntry;
    const obj = parsed.value.object;

    const at_ms_val = obj.get("at_ms") orelse return error.InvalidCacheEntry;
    const status_val = obj.get("status") orelse return error.InvalidCacheEntry;
    const status_text_val = obj.get("status_text") orelse return error.InvalidCacheEntry;
    const content_type_val = obj.get("content_type") orelse return error.InvalidCacheEntry;
    const body_b64_val = obj.get("body_b64") orelse return error.InvalidCacheEntry;

    if (at_ms_val != .integer or status_val != .integer) return error.InvalidCacheEntry;
    if (status_text_val != .string or body_b64_val != .string) return error.InvalidCacheEntry;

    const status_int = status_val.integer;
    if (status_int < 0 or status_int > 999) return error.InvalidCacheEntry;

    const content_type: ?[]u8 = switch (content_type_val) {
        .string => |s| try allocator.dupe(u8, s),
        .null => null,
        else => return error.InvalidCacheEntry,
    };
    errdefer if (content_type) |ct| allocator.free(ct);

    const status_text = try allocator.dupe(u8, status_text_val.string);
    errdefer allocator.free(status_text);

    const body_b64 = body_b64_val.string;
    const body_len = std.base64.standard.Decoder.calcSizeForSlice(body_b64) catch
        return error.InvalidCacheEntry;
    const body = try allocator.alloc(u8, body_len);
    errdefer allocator.free(body);
    std.base64.standard.Decoder.decode(body, body_b64) catch return error.InvalidCacheEntry;

    return .{
        .at_ms = at_ms_val.integer,
        .status = @intCast(status_int),
        .status_text = status_text,
        .content_type = content_type,
        .body = body,
    };
}

fn appendEscaped(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, data: []const u8) !void {
    const hex = "0123456789abcdef";
    for (data) |ch| switch (ch) {
        '"' => try buf.appendSlice(allocator, "\\\""),
        '\\' => try buf.appendSlice(allocator, "\\\\"),
        '\n' => try buf.appendSlice(allocator, "\\n"),
        '\r' => try buf.appendSlice(allocator, "\\r"),
        '\t' => try buf.appendSlice(allocator, "\\t"),
        else => if (ch < 0x20) {
            try buf.appendSlice(allocator, "\\u00");
            try buf.append(allocator, hex[ch >> 4]);
            try buf.append(allocator, hex[ch & 0x0f]);
        } else {
            try buf.append(allocator, ch);
        },
    };
}

fn unlinkPath(allocator: std.mem.Allocator, path: []const u8) void {
    const path_z = allocator.dupeZ(u8, path) catch return;
    defer allocator.free(path_z);
    _ = std.c.unlink(path_z);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "computeRequestHash is stable and distinguishes components" {
    const h1 = computeRequestHash("k", "POST", "http://x/y", "body");
    const h2 = computeRequestHash("k", "POST", "http://x/y", "body");
    try testing.expectEqualSlices(u8, &h1, &h2);

    const h3 = computeRequestHash("k2", "POST", "http://x/y", "body");
    try testing.expect(!std.mem.eql(u8, &h1, &h3));

    const h4 = computeRequestHash("k", "GET", "http://x/y", "body");
    try testing.expect(!std.mem.eql(u8, &h1, &h4));

    const h5 = computeRequestHash("k", "POST", "http://x/y", "different");
    try testing.expect(!std.mem.eql(u8, &h1, &h5));
}

test "writeEntry and loadEntry round-trip through JSON + base64" {
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const dir = try std.fmt.allocPrint(arena, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path[0..]});

    const entry_in = Entry{
        .at_ms = 1_700_000_000_000,
        .status = 201,
        .status_text = try arena.dupe(u8, "Created"),
        .content_type = try arena.dupe(u8, "application/json"),
        .body = try arena.dupe(u8, "{\"ok\":true}\x00raw"),
    };

    try writeEntry(testing.allocator, dir, "abcd", entry_in);

    var loaded = (try loadEntry(testing.allocator, dir, "abcd", 3600, 1_700_000_000_000 + 1)) orelse
        return error.TestFailed;
    defer loaded.deinit(testing.allocator);

    try testing.expectEqual(@as(i64, 1_700_000_000_000), loaded.at_ms);
    try testing.expectEqual(@as(u16, 201), loaded.status);
    try testing.expectEqualStrings("Created", loaded.status_text);
    try testing.expectEqualStrings("application/json", loaded.content_type.?);
    try testing.expectEqualSlices(u8, "{\"ok\":true}\x00raw", loaded.body);
}

test "loadEntry returns null and unlinks when TTL has passed" {
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const dir = try std.fmt.allocPrint(arena, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path[0..]});

    const entry_in = Entry{
        .at_ms = 1_000_000_000_000,
        .status = 200,
        .status_text = try arena.dupe(u8, "OK"),
        .content_type = null,
        .body = try arena.dupe(u8, "stale"),
    };
    try writeEntry(testing.allocator, dir, "ttlhash", entry_in);

    // now_ms is past at_ms + ttl_s*1000 -> expired.
    const expired = try loadEntry(testing.allocator, dir, "ttlhash", 1, 1_000_000_000_000 + 2_000);
    try testing.expect(expired == null);

    // A subsequent load still returns null (file was unlinked).
    const gone = try loadEntry(testing.allocator, dir, "ttlhash", 1, 1_000_000_000_000);
    try testing.expect(gone == null);
}

test "loadEntry returns null for missing file" {
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const dir = try std.fmt.allocPrint(arena, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path[0..]});

    try testing.expect((try loadEntry(testing.allocator, dir, "nope", 3600, 0)) == null);
}
