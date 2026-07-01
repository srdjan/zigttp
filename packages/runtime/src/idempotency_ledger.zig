//! On-disk idempotency ledger backing `DurableStore.writeIdempotencyLedger` /
//! `hasIdempotencyLedger`: records whether an Idempotency-Key has been
//! started/completed so a retried request under the same key does not
//! re-execute a workflow or duplicate its side effects.

const std = @import("std");
const zq = @import("zigts");
const trace = zq.trace;
const durable_store = @import("durable_store.zig");

const Allocator = std.mem.Allocator;
const appendEscaped = durable_store.appendEscaped;
const unixMillis = trace.unixMillis;

pub const IdempotencyLedgerState = enum {
    started,
    completed,

    fn jsonString(self: IdempotencyLedgerState) []const u8 {
        return switch (self) {
            .started => "started",
            .completed => "completed",
        };
    }
};

pub fn dirPath(allocator: Allocator, durable_dir: []const u8) ![]u8 {
    return std.fmt.allocPrint(allocator, "{s}/idempotency", .{durable_dir});
}

fn allocPath(allocator: Allocator, durable_dir: []const u8, idempotency_key: []const u8) ![]u8 {
    const dir = try dirPath(allocator, durable_dir);
    defer allocator.free(dir);
    return std.fmt.allocPrint(allocator, "{s}/idem-{x}.json", .{
        dir,
        std.hash.Fnv1a_64.hash(idempotency_key),
    });
}

fn ensureDir(allocator: Allocator, path: []const u8) !void {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);
    switch (std.posix.errno(std.posix.system.mkdir(path_z, 0o755))) {
        .SUCCESS, .EXIST => {},
        else => return error.MakeDirFailed,
    }
}

pub fn write(
    allocator: Allocator,
    durable_dir: []const u8,
    idempotency_key: []const u8,
    durable_key: []const u8,
    state: IdempotencyLedgerState,
) !void {
    const dir = try dirPath(allocator, durable_dir);
    defer allocator.free(dir);
    try ensureDir(allocator, dir);

    const path = try allocPath(allocator, durable_dir, idempotency_key);
    defer allocator.free(path);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    try buf.appendSlice(allocator, "{\"idempotency_key\":\"");
    try appendEscaped(&buf, allocator, idempotency_key);
    try buf.appendSlice(allocator, "\",\"durable_key\":\"");
    try appendEscaped(&buf, allocator, durable_key);
    try buf.appendSlice(allocator, "\",\"state\":\"");
    try appendEscaped(&buf, allocator, state.jsonString());
    try buf.appendSlice(allocator, "\",\"updated_ms\":");
    var tmp: [32]u8 = undefined;
    const printed = try std.fmt.bufPrint(&tmp, "{d}", .{unixMillis()});
    try buf.appendSlice(allocator, printed);
    try buf.appendSlice(allocator, "}");

    try zq.file_io.writeFile(allocator, path, buf.items);
}

pub fn has(
    allocator: Allocator,
    durable_dir: []const u8,
    idempotency_key: []const u8,
    durable_key: []const u8,
) !bool {
    const path = try allocPath(allocator, durable_dir, idempotency_key);
    defer allocator.free(path);

    const source = zq.file_io.readFile(allocator, path, 1024 * 1024) catch |err| switch (err) {
        error.FileNotFound => return false,
        else => return err,
    };
    defer allocator.free(source);

    return matches(allocator, source, idempotency_key, durable_key);
}

pub fn matches(
    allocator: Allocator,
    source: []const u8,
    idempotency_key: []const u8,
    durable_key: []const u8,
) bool {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, source, .{}) catch return false;
    defer parsed.deinit();
    if (parsed.value != .object) return false;
    const obj = parsed.value.object;

    const stored_idempotency = obj.get("idempotency_key") orelse return false;
    const stored_durable = obj.get("durable_key") orelse return false;
    const stored_state = obj.get("state") orelse return false;
    if (stored_idempotency != .string or stored_durable != .string or stored_state != .string) return false;
    if (!std.mem.eql(u8, stored_idempotency.string, idempotency_key)) return false;
    if (!std.mem.eql(u8, stored_durable.string, durable_key)) return false;
    return std.mem.eql(u8, stored_state.string, "started") or
        std.mem.eql(u8, stored_state.string, "completed");
}

test "idempotency ledger records started and completed states" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    try std.testing.expect(!try has(allocator, durable_dir, "idem:123", "order:123"));

    try write(allocator, durable_dir, "idem:123", "order:123", .started);
    try std.testing.expect(try has(allocator, durable_dir, "idem:123", "order:123"));
    try std.testing.expect(!try has(allocator, durable_dir, "idem:123", "other"));

    try write(allocator, durable_dir, "idem:123", "order:123", .completed);
    try std.testing.expect(try has(allocator, durable_dir, "idem:123", "order:123"));
}

test "idempotency ledger ignores malformed records" {
    try std.testing.expect(!matches(std.testing.allocator, "{\"idempotency_key\":false}", "idem", "key"));
    try std.testing.expect(!matches(
        std.testing.allocator,
        "{\"idempotency_key\":\"idem\",\"durable_key\":\"key\",\"state\":\"unknown\"}",
        "idem",
        "key",
    ));
}
