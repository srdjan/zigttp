//! Durable signal queue and scheduled-signal store.
//!
//! This is the first backend behind the durable expansion paths. It is
//! intentionally small: immediate signals are stored under `signals/`, future
//! signals under `scheduled/`, and both are exposed through the same
//! `DurableStore` surface so a non-filesystem backend can replace it later.

const std = @import("std");
const builtin = @import("builtin");
const zq = @import("zigts");
const trace = zq.trace;

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

    /// Returns an allocated absolute path `<durable_dir>/<name>` and
    /// ensures the directory exists. Caller owns the returned slice.
    /// Used by non-signal subsystems (`zigttp:websocket`, `zigttp:fetch`)
    /// to carve out namespaced storage under the shared durable root
    /// without colliding with `signals/`, `scheduled/`, or top-level
    /// `durable-*.jsonl` oplog files.
    pub fn subtreeDir(
        self: *DurableStore,
        allocator: std.mem.Allocator,
        name: []const u8,
    ) ![]u8 {
        return switch (self.*) {
            .fs => |*store| store.subtreeDir(allocator, name),
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

    pub fn scanSignals(self: *DurableStore, allocator: std.mem.Allocator, now_ms: i64) ![]Signal {
        return switch (self.*) {
            .fs => |*store| store.scanSignals(allocator, now_ms),
        };
    }

    pub fn tryClaimSignal(self: *DurableStore, candidate: *const Signal) !?Signal {
        return switch (self.*) {
            .fs => |*store| store.tryClaimSignal(candidate),
        };
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

    /// Remove the claimed file behind a consumed signal. Call only after the
    /// consumption has been persisted to the run oplog.
    pub fn finalizeConsumedSignal(self: *DurableStore, consumed: *const ConsumedSignal) void {
        switch (self.*) {
            .fs => deletePath(consumed.path),
        }
    }
};

pub const Signal = struct {
    key: []const u8,
    name: []const u8,
    payload_json: []const u8,
    path: []const u8,
    at_ms: ?i64,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Signal) void {
        self.allocator.free(self.key);
        self.allocator.free(self.name);
        self.allocator.free(self.payload_json);
        self.allocator.free(self.path);
    }
};

pub const ConsumedSignal = struct {
    payload_json: []const u8,
    /// Claimed on-disk file backing this signal. The caller deletes it via
    /// `finalizeConsumedSignal` only AFTER the consumption is durably
    /// persisted to the run oplog; unlinking first would lose the signal on
    /// a crash between unlink and persist. A claim left unfinalized is invisible
    /// to scans (they skip `.claim-` files) and is deliberately never
    /// re-delivered, which keeps consume from double-delivering a signal.
    ///
    /// KNOWN LIMITATION: a crash in the narrow window between the claim-rename
    /// and the oplog resume-persist strands that one signal - the run replays,
    /// re-waits, and the payload (locked in the invisible `.claim-` file) is
    /// unreachable, so it re-suspends until a fresh signal arrives. The sound
    /// fix is to persist the resume BEFORE claiming (oplog-first), so a crash
    /// either leaves the signal consumable or the oplog already records it; that
    /// reorders the consume protocol across the durable runtime and needs
    /// crash-injection coverage, so it is deferred rather than patched here with
    /// a `.claim-` recovery that could double-deliver under shared run keys.
    path: []const u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *ConsumedSignal) void {
        self.allocator.free(self.payload_json);
        self.allocator.free(self.path);
    }
};

const FsDurableStore = struct {
    allocator: std.mem.Allocator,
    durable_dir: []const u8,
    dirs_ready: bool = false,

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
        if (self.dirs_ready) return;
        try self.ensureDir(self.durable_dir);
        const signals_dir = try self.allocSignalsDir();
        defer self.allocator.free(signals_dir);
        try self.ensureDir(signals_dir);

        const scheduled_dir = try self.allocScheduledDir();
        defer self.allocator.free(scheduled_dir);
        try self.ensureDir(scheduled_dir);
        self.dirs_ready = true;
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

    fn scanSignals(self: *Self, allocator: std.mem.Allocator, now_ms: i64) ![]Signal {
        try self.ensureDirs();
        var out: std.ArrayList(Signal) = .empty;
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

    fn tryClaimSignal(self: *Self, candidate: *const Signal) !?Signal {
        const claimed_path = try self.allocClaimedPath(candidate.path);
        errdefer self.allocator.free(claimed_path);
        if (!try renamePath(candidate.path, claimed_path)) {
            // errdefer does NOT run on a (successful) `return null`, only on an
            // error return. A lost claim race (another claimant already renamed
            // the candidate away -> rename ENOENT -> false) hits this path on
            // every losing pass, so free claimed_path explicitly or it leaks.
            self.allocator.free(claimed_path);
            return null;
        }

        return .{
            .key = try self.allocator.dupe(u8, candidate.key),
            .name = try self.allocator.dupe(u8, candidate.name),
            .payload_json = try self.allocator.dupe(u8, candidate.payload_json),
            .path = claimed_path,
            .at_ms = candidate.at_ms,
            .allocator = self.allocator,
        };
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

            // The claimed file is NOT deleted here: the caller persists the
            // consumption to the run oplog first, then finalizes. See
            // ConsumedSignal.path.
            const payload_json = try self.allocator.dupe(u8, claimed.payload_json);
            errdefer self.allocator.free(payload_json);
            return .{
                .payload_json = payload_json,
                .path = try self.allocator.dupe(u8, claimed.path),
                .allocator = self.allocator,
            };
        }

        return null;
    }

    fn scanDirSignals(
        self: *Self,
        allocator: std.mem.Allocator,
        out: *std.ArrayList(Signal),
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

            // One torn or malformed signal file must not poison every scan
            // (and with it all signal consumption) forever: move it aside
            // and keep going.
            var parsed = parseSignalEnvelope(allocator, source) catch |err| switch (err) {
                error.OutOfMemory => return err,
                else => {
                    self.quarantineSignalFile(full_path);
                    allocator.free(full_path);
                    continue;
                },
            };
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

    /// Rename an unparseable signal file out of the scanned namespace
    /// (the scan only considers `.json` suffixes) so it stops poisoning
    /// consumption but stays on disk for inspection.
    fn quarantineSignalFile(self: *Self, path: []const u8) void {
        const quarantined = std.fmt.allocPrint(self.allocator, "{s}.quarantined", .{path}) catch return;
        defer self.allocator.free(quarantined);
        _ = renamePath(path, quarantined) catch return;
        if (!builtin.is_test) {
            std.log.warn("durable: quarantined unreadable signal file {s}", .{path});
        }
    }

    fn allocUniqueSignalPath(self: *Self, dir_path: []const u8) ![]u8 {
        const id = counter.value.fetchAdd(1, .acq_rel) + 1;
        const now_ms = unixMillis();
        // The counter is process-local: a second process (server vs
        // scheduler) can mint the same (timestamp, counter) pair within one
        // millisecond and silently overwrite the other's signal file. The
        // pid disambiguates across processes.
        const pid: u32 = @intCast(std.c.getpid());
        return std.fmt.allocPrint(self.allocator, "{s}/signal-{d}-{x}-{x}.json", .{
            dir_path,
            now_ms,
            pid,
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

    fn subtreeDir(self: *Self, allocator: std.mem.Allocator, name: []const u8) ![]u8 {
        if (name.len == 0) return error.InvalidSubtreeName;
        for (name) |ch| {
            const ok = (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or
                (ch >= '0' and ch <= '9') or ch == '_' or ch == '-';
            if (!ok) return error.InvalidSubtreeName;
        }
        try self.ensureDir(self.durable_dir);
        const path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ self.durable_dir, name });
        errdefer allocator.free(path);
        try self.ensureDir(path);
        return path;
    }
};

const unixMillis = trace.unixMillis;

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

pub fn appendEscaped(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, data: []const u8) !void {
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

const writeAll = trace.writeAll;

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

test "subtreeDir creates a namespaced directory under the durable root" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    var store = DurableStore.initFs(allocator, durable_dir);

    const ws_dir = try store.subtreeDir(allocator, "ws");
    try std.testing.expect(std.mem.endsWith(u8, ws_dir, "/ws"));

    // Idempotent: calling twice with the same name succeeds.
    const ws_dir_again = try store.subtreeDir(allocator, "ws");
    try std.testing.expectEqualStrings(ws_dir, ws_dir_again);

    // Different names yield different paths, both coexist with signals/scheduled.
    const fetch_dir = try store.subtreeDir(allocator, "fetch");
    try std.testing.expect(std.mem.endsWith(u8, fetch_dir, "/fetch"));
    try std.testing.expect(!std.mem.eql(u8, ws_dir, fetch_dir));
}

test "subtreeDir rejects names that would escape the root" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    var store = DurableStore.initFs(allocator, durable_dir);

    try std.testing.expectError(error.InvalidSubtreeName, store.subtreeDir(allocator, ""));
    try std.testing.expectError(error.InvalidSubtreeName, store.subtreeDir(allocator, "../etc"));
    try std.testing.expectError(error.InvalidSubtreeName, store.subtreeDir(allocator, "ws/../escape"));
    try std.testing.expectError(error.InvalidSubtreeName, store.subtreeDir(allocator, "has space"));
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

test "durable store consume is crash-safe: unlink only at finalize" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    var store = DurableStore.initFs(allocator, durable_dir);
    try store.enqueueSignal("order:9", "approved", "{\"ok\":true}");

    var consumed = (try store.tryConsumeSignal("order:9", "approved", unixMillis())).?;
    defer consumed.deinit();

    // The claimed file still exists until finalize (a crash here must not
    // lose the signal before the oplog records it)...
    const claim_bytes = try zq.file_io.readFile(allocator, consumed.path, 64 * 1024);
    allocator.free(claim_bytes);
    // ...but a rescan never re-delivers a claimed signal.
    try std.testing.expect((try store.tryConsumeSignal("order:9", "approved", unixMillis())) == null);

    store.finalizeConsumedSignal(&consumed);
    try std.testing.expectError(error.FileNotFound, zq.file_io.readFile(allocator, consumed.path, 64 * 1024));
}

test "durable store quarantines a torn signal file instead of poisoning scans" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    var store = DurableStore.initFs(allocator, durable_dir);
    try store.enqueueSignal("order:1", "approved", "{\"ok\":true}");

    // A crash mid-write leaves a truncated envelope behind.
    const torn_path = try std.fmt.allocPrint(allocator, "{s}/signals/signal-torn.json", .{durable_dir});
    try zq.file_io.writeFile(allocator, torn_path, "{\"key\":\"order");

    // The good signal is still consumable...
    var consumed = (try store.tryConsumeSignal("order:1", "approved", unixMillis())).?;
    defer consumed.deinit();
    try std.testing.expectEqualStrings("{\"ok\":true}", consumed.payload_json);
    store.finalizeConsumedSignal(&consumed);

    // ...and the torn file was moved aside, not deleted.
    try std.testing.expectError(error.FileNotFound, zq.file_io.readFile(allocator, torn_path, 64 * 1024));
    const quarantined = try std.fmt.allocPrint(allocator, "{s}.quarantined", .{torn_path});
    const quarantined_bytes = try zq.file_io.readFile(allocator, quarantined, 64 * 1024);
    allocator.free(quarantined_bytes);
}
