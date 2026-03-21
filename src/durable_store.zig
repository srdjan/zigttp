//! Durable signal queue and scheduled-signal store.
//!
//! This is the first backend behind the durable expansion paths. It is
//! intentionally small: immediate signals are stored under `signals/`, future
//! signals under `scheduled/`, and both are exposed through the same
//! `DurableStore` surface so a non-filesystem backend can replace it later.

const std = @import("std");
const zq = @import("zts");

const c = @cImport({
    @cInclude("dirent.h");
});

pub const DurableStore = union(enum) {
    fs: FsDurableStore,

    pub fn initFs(allocator: std.mem.Allocator, durable_dir: []const u8) DurableStore {
        return .{ .fs = FsDurableStore.init(allocator, durable_dir) };
    }

    pub fn ensureDirs(self: *DurableStore) !void {
        return switch (self.*) {
            .fs => |*store| store.ensureDirs(),
        };
    }

    pub fn enqueueSignal(
        self: *DurableStore,
        key: []const u8,
        name: []const u8,
        payload_json: []const u8,
    ) !void {
        return switch (self.*) {
            .fs => |*store| store.enqueueSignal(key, name, payload_json),
        };
    }

    pub fn enqueueSignalAt(
        self: *DurableStore,
        key: []const u8,
        name: []const u8,
        at_ms: i64,
        payload_json: []const u8,
    ) !void {
        return switch (self.*) {
            .fs => |*store| store.enqueueSignalAt(key, name, at_ms, payload_json),
        };
    }

    pub fn scanSignals(self: *DurableStore, allocator: std.mem.Allocator, now_ms: i64) ![]SignalCandidate {
        return switch (self.*) {
            .fs => |*store| store.scanSignals(allocator, now_ms),
        };
    }

    pub fn tryClaimSignal(self: *DurableStore, candidate: *const SignalCandidate) !?ClaimedSignal {
        return switch (self.*) {
            .fs => |*store| store.tryClaimSignal(candidate),
        };
    }

    pub fn deleteClaimedSignal(self: *DurableStore, claimed: *const ClaimedSignal) void {
        switch (self.*) {
            .fs => |*store| store.deleteClaimedSignal(claimed),
        }
    }

    pub fn tryConsumeSignal(
        self: *DurableStore,
        key: []const u8,
        name: []const u8,
        now_ms: i64,
    ) !?ConsumedSignal {
        return switch (self.*) {
            .fs => |*store| store.tryConsumeSignal(key, name, now_ms),
        };
    }
};

pub const SignalCandidate = struct {
    key: []const u8,
    name: []const u8,
    payload_json: []const u8,
    path: []const u8,
    at_ms: ?i64,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *SignalCandidate) void {
        self.allocator.free(self.key);
        self.allocator.free(self.name);
        self.allocator.free(self.payload_json);
        self.allocator.free(self.path);
    }
};

pub const ClaimedSignal = struct {
    key: []const u8,
    name: []const u8,
    payload_json: []const u8,
    path: []const u8,
    at_ms: ?i64,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *ClaimedSignal) void {
        self.allocator.free(self.key);
        self.allocator.free(self.name);
        self.allocator.free(self.payload_json);
        self.allocator.free(self.path);
    }
};

pub const ConsumedSignal = struct {
    payload_json: []const u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *ConsumedSignal) void {
        self.allocator.free(self.payload_json);
    }
};

const FsDurableStore = struct {
    allocator: std.mem.Allocator,
    durable_dir: []const u8,

    const Self = @This();
    const counter = struct {
        var value: std.atomic.Value(u64) = std.atomic.Value(u64).init(0);
    };

    fn init(allocator: std.mem.Allocator, durable_dir: []const u8) Self {
        return .{
            .allocator = allocator,
            .durable_dir = durable_dir,
        };
    }

    fn ensureDirs(self: *Self) !void {
        try self.ensureDir(self.durable_dir);
        const signals_dir = try self.allocSignalsDir();
        defer self.allocator.free(signals_dir);
        try self.ensureDir(signals_dir);

        const scheduled_dir = try self.allocScheduledDir();
        defer self.allocator.free(scheduled_dir);
        try self.ensureDir(scheduled_dir);
    }

    fn enqueueSignal(self: *Self, key: []const u8, name: []const u8, payload_json: []const u8) !void {
        try self.ensureDirs();
        const dir = try self.allocSignalsDir();
        defer self.allocator.free(dir);
        const path = try self.allocUniqueSignalPath(dir);
        defer self.allocator.free(path);
        try writeSignalEnvelope(self.allocator, path, key, name, payload_json, null);
    }

    fn enqueueSignalAt(self: *Self, key: []const u8, name: []const u8, at_ms: i64, payload_json: []const u8) !void {
        try self.ensureDirs();
        if (at_ms <= unixMillis()) {
            return self.enqueueSignal(key, name, payload_json);
        }
        const dir = try self.allocScheduledDir();
        defer self.allocator.free(dir);
        const path = try self.allocUniqueSignalPath(dir);
        defer self.allocator.free(path);
        try writeSignalEnvelope(self.allocator, path, key, name, payload_json, at_ms);
    }

    fn scanSignals(self: *Self, allocator: std.mem.Allocator, now_ms: i64) ![]SignalCandidate {
        try self.ensureDirs();
        var out: std.ArrayList(SignalCandidate) = .empty;
        errdefer {
            for (out.items) |*candidate| candidate.deinit();
            out.deinit(allocator);
        }

        const signals_dir = try self.allocSignalsDir();
        defer self.allocator.free(signals_dir);
        try self.scanDirSignals(allocator, &out, signals_dir, false, now_ms);

        const scheduled_dir = try self.allocScheduledDir();
        defer self.allocator.free(scheduled_dir);
        try self.scanDirSignals(allocator, &out, scheduled_dir, true, now_ms);

        return out.toOwnedSlice(allocator);
    }

    fn tryClaimSignal(self: *Self, candidate: *const SignalCandidate) !?ClaimedSignal {
        const claimed_path = try self.allocClaimedPath(candidate.path);
        errdefer self.allocator.free(claimed_path);
        if (!try renamePath(candidate.path, claimed_path)) return null;

        return .{
            .key = try self.allocator.dupe(u8, candidate.key),
            .name = try self.allocator.dupe(u8, candidate.name),
            .payload_json = try self.allocator.dupe(u8, candidate.payload_json),
            .path = claimed_path,
            .at_ms = candidate.at_ms,
            .allocator = self.allocator,
        };
    }

    fn deleteClaimedSignal(_: *Self, claimed: *const ClaimedSignal) void {
        deletePath(claimed.path);
    }

    fn tryConsumeSignal(self: *Self, key: []const u8, name: []const u8, now_ms: i64) !?ConsumedSignal {
        const candidates = try self.scanSignals(self.allocator, now_ms);
        defer {
            for (candidates) |*candidate| candidate.deinit();
            self.allocator.free(candidates);
        }

        for (candidates) |*candidate| {
            if (!std.mem.eql(u8, candidate.key, key)) continue;
            if (!std.mem.eql(u8, candidate.name, name)) continue;

            var claimed = (try self.tryClaimSignal(candidate)) orelse continue;
            defer claimed.deinit();
            defer self.deleteClaimedSignal(&claimed);

            return .{
                .payload_json = try self.allocator.dupe(u8, claimed.payload_json),
                .allocator = self.allocator,
            };
        }

        return null;
    }

    fn scanDirSignals(
        self: *Self,
        allocator: std.mem.Allocator,
        out: *std.ArrayList(SignalCandidate),
        dir_path: []const u8,
        scheduled_only: bool,
        now_ms: i64,
    ) !void {
        const dir_path_z = try self.allocator.dupeZ(u8, dir_path);
        defer self.allocator.free(dir_path_z);

        const dir = c.opendir(dir_path_z) orelse return;
        defer _ = c.closedir(dir);

        while (c.readdir(dir)) |entry| {
            const name_ptr: [*:0]const u8 = @ptrCast(&entry.*.d_name);
            const name = std.mem.sliceTo(name_ptr, 0);
            if (!std.mem.endsWith(u8, name, ".json")) continue;
            if (std.mem.indexOf(u8, name, ".claim-") != null) continue;

            const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, name });
            errdefer allocator.free(full_path);

            const source = zq.file_io.readFile(allocator, full_path, 1024 * 1024) catch {
                allocator.free(full_path);
                continue;
            };
            defer allocator.free(source);

            var parsed = try parseSignalEnvelope(allocator, source);
            errdefer parsed.deinit();

            if (scheduled_only) {
                const at_ms = parsed.at_ms orelse {
                    parsed.deinit();
                    allocator.free(full_path);
                    continue;
                };
                if (at_ms > now_ms) {
                    parsed.deinit();
                    allocator.free(full_path);
                    continue;
                }
            }

            try out.append(allocator, .{
                .key = parsed.key,
                .name = parsed.name,
                .payload_json = parsed.payload_json,
                .path = full_path,
                .at_ms = parsed.at_ms,
                .allocator = allocator,
            });
            parsed.disarm();
        }
    }

    fn allocSignalsDir(self: *Self) ![]u8 {
        return std.fmt.allocPrint(self.allocator, "{s}/signals", .{self.durable_dir});
    }

    fn allocScheduledDir(self: *Self) ![]u8 {
        return std.fmt.allocPrint(self.allocator, "{s}/scheduled", .{self.durable_dir});
    }

    fn allocUniqueSignalPath(self: *Self, dir_path: []const u8) ![]u8 {
        const id = counter.value.fetchAdd(1, .acq_rel) + 1;
        const now_ms = unixMillis();
        return std.fmt.allocPrint(self.allocator, "{s}/signal-{d}-{x}.json", .{
            dir_path,
            now_ms,
            id,
        });
    }

    fn allocClaimedPath(self: *Self, path: []const u8) ![]u8 {
        const id = counter.value.fetchAdd(1, .acq_rel) + 1;
        return std.fmt.allocPrint(self.allocator, "{s}.claim-{x}", .{ path, id });
    }

    fn ensureDir(self: *Self, path: []const u8) !void {
        const path_z = try self.allocator.dupeZ(u8, path);
        defer self.allocator.free(path_z);
        switch (std.posix.errno(std.posix.system.mkdir(path_z, 0o755))) {
            .SUCCESS, .EXIST => {},
            else => return error.MakeDirFailed,
        }
    }
};

fn unixMillis() i64 {
    var ts: std.posix.timespec = undefined;
    switch (std.posix.errno(std.posix.system.clock_gettime(.REALTIME, &ts))) {
        .SUCCESS => {
            const secs: i64 = @intCast(ts.sec);
            const nanos: i64 = @intCast(ts.nsec);
            return (secs * 1000) + @divTrunc(nanos, 1_000_000);
        },
        else => return 0,
    }
}

const ParsedSignalEnvelope = struct {
    key: []const u8,
    name: []const u8,
    payload_json: []const u8,
    at_ms: ?i64,
    allocator: std.mem.Allocator,

    fn deinit(self: *ParsedSignalEnvelope) void {
        self.allocator.free(self.key);
        self.allocator.free(self.name);
        self.allocator.free(self.payload_json);
    }

    fn disarm(self: *ParsedSignalEnvelope) void {
        self.key = &.{};
        self.name = &.{};
        self.payload_json = &.{};
    }
};

fn parseSignalEnvelope(allocator: std.mem.Allocator, source: []const u8) !ParsedSignalEnvelope {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, source, .{});
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidSignalEnvelope;
    const obj = parsed.value.object;
    const key = obj.get("key") orelse return error.InvalidSignalEnvelope;
    const name = obj.get("name") orelse return error.InvalidSignalEnvelope;
    const payload = obj.get("payload_json") orelse return error.InvalidSignalEnvelope;

    if (key != .string or name != .string or payload != .string) {
        return error.InvalidSignalEnvelope;
    }

    const at_ms = if (obj.get("at_ms")) |value| switch (value) {
        .integer => value.integer,
        else => null,
    } else null;

    return .{
        .key = try allocator.dupe(u8, key.string),
        .name = try allocator.dupe(u8, name.string),
        .payload_json = try allocator.dupe(u8, payload.string),
        .at_ms = at_ms,
        .allocator = allocator,
    };
}

fn writeSignalEnvelope(
    allocator: std.mem.Allocator,
    path: []const u8,
    key: []const u8,
    name: []const u8,
    payload_json: []const u8,
    at_ms: ?i64,
) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);

    try buf.appendSlice(allocator, "{\"key\":\"");
    try appendEscaped(&buf, allocator, key);
    try buf.appendSlice(allocator, "\",\"name\":\"");
    try appendEscaped(&buf, allocator, name);
    try buf.appendSlice(allocator, "\",\"payload_json\":\"");
    try appendEscaped(&buf, allocator, payload_json);
    if (at_ms) |value| {
        try buf.appendSlice(allocator, "\",\"at_ms\":");
        var tmp: [32]u8 = undefined;
        const printed = try std.fmt.bufPrint(&tmp, "{d}", .{value});
        try buf.appendSlice(allocator, printed);
        try buf.appendSlice(allocator, "}");
    } else {
        try buf.appendSlice(allocator, "\"}");
    }

    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);
    const fd = try std.posix.openatZ(
        std.posix.AT.FDCWD,
        path_z,
        .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true },
        0o644,
    );
    defer std.Io.Threaded.closeFd(fd);
    writeAll(fd, buf.items);
}

fn appendEscaped(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, data: []const u8) !void {
    const hex = "0123456789abcdef";
    for (data) |char| {
        switch (char) {
            '"' => try buf.appendSlice(allocator, "\\\""),
            '\\' => try buf.appendSlice(allocator, "\\\\"),
            '\n' => try buf.appendSlice(allocator, "\\n"),
            '\r' => try buf.appendSlice(allocator, "\\r"),
            '\t' => try buf.appendSlice(allocator, "\\t"),
            else => {
                if (char < 0x20) {
                    try buf.appendSlice(allocator, "\\u00");
                    try buf.append(allocator, hex[char >> 4]);
                    try buf.append(allocator, hex[char & 0x0f]);
                } else {
                    try buf.append(allocator, char);
                }
            },
        }
    }
}

fn renamePath(old_path: []const u8, new_path: []const u8) !bool {
    const old_z = try std.heap.c_allocator.dupeZ(u8, old_path);
    defer std.heap.c_allocator.free(old_z);
    const new_z = try std.heap.c_allocator.dupeZ(u8, new_path);
    defer std.heap.c_allocator.free(new_z);
    const rc = std.c.rename(old_z, new_z);
    if (rc == 0) return true;
    return false;
}

fn deletePath(path: []const u8) void {
    const path_z = std.heap.c_allocator.dupeZ(u8, path) catch return;
    defer std.heap.c_allocator.free(path_z);
    _ = std.c.unlink(path_z);
}

fn writeAll(fd: std.c.fd_t, data: []const u8) void {
    var remaining = data;
    while (remaining.len > 0) {
        const result = std.c.write(fd, remaining.ptr, remaining.len);
        if (result < 0) break;
        remaining = remaining[@intCast(result)..];
    }
}

test "durable store enqueue and consume immediate signal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    var store = DurableStore.initFs(allocator, durable_dir);
    try store.enqueueSignal("order:123", "approved", "{\"ok\":true}");

    var consumed = (try store.tryConsumeSignal("order:123", "approved", unixMillis())).?;
    defer consumed.deinit();
    try std.testing.expectEqualStrings("{\"ok\":true}", consumed.payload_json);

    try std.testing.expect((try store.tryConsumeSignal("order:123", "approved", unixMillis())) == null);
}

test "durable store hides future scheduled signals until due" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    var store = DurableStore.initFs(allocator, durable_dir);
    const now_ms = unixMillis();
    try store.enqueueSignalAt("order:123", "approved", now_ms + 60_000, "{\"ok\":true}");

    try std.testing.expect((try store.tryConsumeSignal("order:123", "approved", now_ms)) == null);

    var consumed = (try store.tryConsumeSignal("order:123", "approved", now_ms + 60_000)).?;
    defer consumed.deinit();
    try std.testing.expectEqualStrings("{\"ok\":true}", consumed.payload_json);
}
