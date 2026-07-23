//! Durable signal queue and scheduled-signal store.
//!
//! This is the first backend behind the durable expansion paths. It is
//! intentionally small: immediate signals are stored under `signals/`, future
//! signals under `scheduled/`, and both are exposed through the same
//! `DurableStore` surface so a non-filesystem backend can replace it later.

const std = @import("std");
const builtin = @import("builtin");
const zq = @import("zts");
const trace = zq.trace;
const idempotency_ledger = @import("idempotency_ledger.zig");

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
    /// Used by non-signal subsystems (`zttp:websocket`, `zttp:fetch`)
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

    pub fn scanRecoverableSignals(self: *DurableStore, allocator: std.mem.Allocator, now_ms: i64) ![]Signal {
        return switch (self.*) {
            .fs => |*store| store.scanRecoverableSignals(allocator, now_ms),
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

    pub fn tryRecoverSignal(
        self: *DurableStore,
        key: []const u8,
        name: []const u8,
        now_ms: i64,
    ) !?ConsumedSignal {
        return switch (self.*) {
            .fs => |*store| store.tryRecoverSignal(key, name, now_ms),
        };
    }

    /// Remove the claimed file behind a consumed signal. Call only after the
    /// consumption has been persisted to the run oplog.
    pub fn finalizeConsumedSignal(self: *DurableStore, consumed: *const ConsumedSignal) void {
        switch (self.*) {
            .fs => deletePath(consumed.path),
        }
    }

    pub fn finalizeResumedSignalClaims(
        self: *DurableStore,
        key: []const u8,
        name: []const u8,
        payload_json: []const u8,
    ) !u32 {
        return switch (self.*) {
            .fs => |*store| store.finalizeResumedSignalClaims(key, name, payload_json),
        };
    }

    pub fn listSignalArtifacts(self: *DurableStore, allocator: std.mem.Allocator) ![]SignalArtifact {
        return switch (self.*) {
            .fs => |*store| store.listSignalArtifacts(allocator),
        };
    }

    pub fn cleanupSignalArtifacts(self: *DurableStore, kind: SignalArtifactKind) !u32 {
        return switch (self.*) {
            .fs => |*store| store.cleanupSignalArtifacts(kind),
        };
    }

    pub fn writeIdempotencyLedger(
        self: *DurableStore,
        idempotency_key: []const u8,
        durable_key: []const u8,
        state: IdempotencyLedgerState,
    ) !void {
        return switch (self.*) {
            .fs => |*store| store.writeIdempotencyLedger(idempotency_key, durable_key, state),
        };
    }

    pub fn hasIdempotencyLedger(
        self: *DurableStore,
        idempotency_key: []const u8,
        durable_key: []const u8,
    ) !bool {
        return switch (self.*) {
            .fs => |*store| store.hasIdempotencyLedger(idempotency_key, durable_key),
        };
    }
};

pub const IdempotencyLedgerState = idempotency_ledger.IdempotencyLedgerState;

pub const SignalArtifactKind = enum {
    claimed,
    quarantined,
    /// A plain, undelivered signal envelope (not yet claimed by any live
    /// waitSignal()). Includes signals whose waiter already timed out and
    /// moved on - durableWaitSignal returns before ever consuming the
    /// signal once its deadline expires, so a late delivery is otherwise
    /// invisible and never cleaned up. Unlike `claimed`/`quarantined`,
    /// this kind may also match a signal that is still legitimately
    /// awaiting an in-progress waitSignal() call - callers must not sweep
    /// it away unconditionally.
    pending,
};

pub const SignalArtifact = struct {
    path: []u8,
    kind: SignalArtifactKind,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *SignalArtifact) void {
        self.allocator.free(self.path);
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
    /// a crash between unlink and persist. Normal scans skip `.claim-` files so
    /// a live waiter cannot double-deliver one. Recovery scans can include them
    /// when the oplog has a pending wait without a resume record, making the
    /// claim-before-resume crash window recoverable.
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

        const idempotency_dir = try idempotency_ledger.dirPath(self.allocator, self.durable_dir);
        defer self.allocator.free(idempotency_dir);
        try self.ensureDir(idempotency_dir);
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
        return self.scanSignalsInternal(allocator, now_ms, false);
    }

    fn scanRecoverableSignals(self: *Self, allocator: std.mem.Allocator, now_ms: i64) ![]Signal {
        return self.scanSignalsInternal(allocator, now_ms, true);
    }

    fn scanSignalsInternal(self: *Self, allocator: std.mem.Allocator, now_ms: i64, include_claimed: bool) ![]Signal {
        try self.ensureDirs();
        var out: std.ArrayList(Signal) = .empty;
        errdefer {
            for (out.items) |*candidate| candidate.deinit();
            out.deinit(allocator);
        }

        const signals_dir = try self.allocSignalsDir();
        defer self.allocator.free(signals_dir);
        try self.scanDirSignals(allocator, &out, signals_dir, false, now_ms, include_claimed);

        const scheduled_dir = try self.allocScheduledDir();
        defer self.allocator.free(scheduled_dir);
        try self.scanDirSignals(allocator, &out, scheduled_dir, true, now_ms, include_claimed);

        return out.toOwnedSlice(allocator);
    }

    fn tryClaimSignal(self: *Self, candidate: *const Signal) !?Signal {
        if (isClaimedSignalPath(candidate.path)) {
            const key = try self.allocator.dupe(u8, candidate.key);
            errdefer self.allocator.free(key);
            const name = try self.allocator.dupe(u8, candidate.name);
            errdefer self.allocator.free(name);
            const payload_json = try self.allocator.dupe(u8, candidate.payload_json);
            errdefer self.allocator.free(payload_json);
            const path = try self.allocator.dupe(u8, candidate.path);
            return .{
                .key = key,
                .name = name,
                .payload_json = payload_json,
                .path = path,
                .at_ms = candidate.at_ms,
                .allocator = self.allocator,
            };
        }

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
        return self.tryConsumeSignalInternal(key, name, now_ms, false);
    }

    fn tryRecoverSignal(self: *Self, key: []const u8, name: []const u8, now_ms: i64) !?ConsumedSignal {
        return self.tryConsumeSignalInternal(key, name, now_ms, true);
    }

    fn tryConsumeSignalInternal(self: *Self, key: []const u8, name: []const u8, now_ms: i64, include_claimed: bool) !?ConsumedSignal {
        const candidates = try self.scanSignalsInternal(self.allocator, now_ms, include_claimed);
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
        include_claimed: bool,
    ) !void {
        const dir_path_z = try self.allocator.dupeZ(u8, dir_path);
        defer self.allocator.free(dir_path_z);

        const dir = c.opendir(dir_path_z) orelse return;
        defer _ = c.closedir(dir);

        while (c.readdir(dir)) |entry| {
            const name_ptr: [*:0]const u8 = @ptrCast(&entry.*.d_name);
            const name = std.mem.sliceTo(name_ptr, 0);
            if (!isSignalEnvelopeFileName(name, include_claimed)) continue;

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

    fn finalizeResumedSignalClaims(self: *Self, key: []const u8, name: []const u8, payload_json: []const u8) !u32 {
        try self.ensureDirs();

        // A replay resumes exactly one logical delivery per `.delivered`
        // oplog entry, so at most one on-disk claim may correspond to this
        // call. Matching is by (key, name, payload_json) content since the
        // exact claimed path isn't persisted to the oplog - if two distinct
        // deliveries share identical content, deleting every match here
        // would also destroy a still-unresumed claim for a later
        // occurrence of the same waitSignal name+payload. Stop at the
        // first match instead.
        const signals_dir = try self.allocSignalsDir();
        defer self.allocator.free(signals_dir);
        if (try self.finalizeMatchingClaimsInDir(signals_dir, key, name, payload_json)) return 1;

        const scheduled_dir = try self.allocScheduledDir();
        defer self.allocator.free(scheduled_dir);
        if (try self.finalizeMatchingClaimsInDir(scheduled_dir, key, name, payload_json)) return 1;

        return 0;
    }

    /// Removes at most one matching claimed-signal file, returning whether
    /// one was found. See finalizeResumedSignalClaims for why this must not
    /// delete every content-matching claim.
    fn finalizeMatchingClaimsInDir(
        self: *Self,
        dir_path: []const u8,
        key: []const u8,
        name: []const u8,
        payload_json: []const u8,
    ) !bool {
        const dir_path_z = try self.allocator.dupeZ(u8, dir_path);
        defer self.allocator.free(dir_path_z);

        const dir = c.opendir(dir_path_z) orelse return false;
        defer _ = c.closedir(dir);

        while (c.readdir(dir)) |entry| {
            const name_ptr: [*:0]const u8 = @ptrCast(&entry.*.d_name);
            const file_name = std.mem.sliceTo(name_ptr, 0);
            if (!isClaimedSignalFileName(file_name)) continue;

            const full_path = try std.fmt.allocPrint(self.allocator, "{s}/{s}", .{ dir_path, file_name });
            defer self.allocator.free(full_path);

            const source = zq.file_io.readFile(self.allocator, full_path, 1024 * 1024) catch continue;
            defer self.allocator.free(source);

            var parsed = parseSignalEnvelope(self.allocator, source) catch continue;
            defer parsed.deinit();

            if (!std.mem.eql(u8, parsed.key, key)) continue;
            if (!std.mem.eql(u8, parsed.name, name)) continue;
            if (!std.mem.eql(u8, parsed.payload_json, payload_json)) continue;

            deletePath(full_path);
            return true;
        }
        return false;
    }

    fn listSignalArtifacts(self: *Self, allocator: std.mem.Allocator) ![]SignalArtifact {
        try self.ensureDirs();
        var artifacts: std.ArrayList(SignalArtifact) = .empty;
        errdefer {
            for (artifacts.items) |*artifact| artifact.deinit();
            artifacts.deinit(allocator);
        }

        const signals_dir = try self.allocSignalsDir();
        defer self.allocator.free(signals_dir);
        try self.listSignalArtifactsInDir(allocator, &artifacts, signals_dir);

        const scheduled_dir = try self.allocScheduledDir();
        defer self.allocator.free(scheduled_dir);
        try self.listSignalArtifactsInDir(allocator, &artifacts, scheduled_dir);

        std.mem.sort(SignalArtifact, artifacts.items, {}, lessThanSignalArtifactPath);
        return artifacts.toOwnedSlice(allocator);
    }

    fn listSignalArtifactsInDir(
        self: *Self,
        allocator: std.mem.Allocator,
        artifacts: *std.ArrayList(SignalArtifact),
        dir_path: []const u8,
    ) !void {
        const dir_path_z = try self.allocator.dupeZ(u8, dir_path);
        defer self.allocator.free(dir_path_z);

        const dir = c.opendir(dir_path_z) orelse return;
        defer _ = c.closedir(dir);

        while (c.readdir(dir)) |entry| {
            const name_ptr: [*:0]const u8 = @ptrCast(&entry.*.d_name);
            const name = std.mem.sliceTo(name_ptr, 0);
            const kind: SignalArtifactKind = if (isClaimedSignalFileName(name))
                .claimed
            else if (std.mem.endsWith(u8, name, ".quarantined"))
                .quarantined
            else if (std.mem.endsWith(u8, name, ".json"))
                .pending
            else
                continue;
            const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, name });
            errdefer allocator.free(full_path);
            try artifacts.append(allocator, .{
                .path = full_path,
                .kind = kind,
                .allocator = allocator,
            });
        }
    }

    fn cleanupSignalArtifacts(self: *Self, kind: SignalArtifactKind) !u32 {
        const artifacts = try self.listSignalArtifacts(self.allocator);
        defer {
            for (artifacts) |*artifact| artifact.deinit();
            self.allocator.free(artifacts);
        }

        var removed: u32 = 0;
        for (artifacts) |artifact| {
            if (artifact.kind != kind) continue;
            deletePath(artifact.path);
            removed += 1;
        }
        return removed;
    }

    fn writeIdempotencyLedger(
        self: *Self,
        idempotency_key: []const u8,
        durable_key: []const u8,
        state: IdempotencyLedgerState,
    ) !void {
        try self.ensureDirs();
        return idempotency_ledger.write(self.allocator, self.durable_dir, idempotency_key, durable_key, state);
    }

    fn hasIdempotencyLedger(
        self: *Self,
        idempotency_key: []const u8,
        durable_key: []const u8,
    ) !bool {
        try self.ensureDirs();
        return idempotency_ledger.has(self.allocator, self.durable_dir, idempotency_key, durable_key);
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
        switch (std.posix.errno(std.posix.system.mkdir(path_z, 0o700))) {
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

    const tmp_path = try std.fmt.allocPrint(
        allocator,
        "{s}.tmp-{d}-{x}",
        .{ path, std.c.getpid(), @intFromPtr(buf.items.ptr) },
    );
    defer allocator.free(tmp_path);
    errdefer deletePath(tmp_path);

    const tmp_z = try allocator.dupeZ(u8, tmp_path);
    defer allocator.free(tmp_z);
    const fd = try std.posix.openatZ(
        std.posix.AT.FDCWD,
        tmp_z,
        .{ .ACCMODE = .WRONLY, .CREAT = true, .EXCL = true },
        0o600,
    );
    defer std.Io.Threaded.closeFd(fd);
    try trace.writeAllChecked(fd, buf.items);
    if (std.c.fsync(fd) != 0) return error.FileWriteFailed;
    if (!(try renamePath(tmp_path, path))) return error.RenameFailed;
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

fn isSignalEnvelopeFileName(name: []const u8, include_claimed: bool) bool {
    if (std.mem.endsWith(u8, name, ".json")) return true;
    return include_claimed and isClaimedSignalFileName(name);
}

fn isClaimedSignalFileName(name: []const u8) bool {
    if (std.mem.endsWith(u8, name, ".quarantined")) return false;
    return std.mem.indexOf(u8, name, ".json.claim-") != null;
}

fn isClaimedSignalPath(path: []const u8) bool {
    return isClaimedSignalFileName(std.fs.path.basename(path));
}

fn lessThanSignalArtifactPath(_: void, lhs: SignalArtifact, rhs: SignalArtifact) bool {
    return std.mem.lessThan(u8, lhs.path, rhs.path);
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

test "durable signal files are private when created atomically" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    var store = DurableStore.initFs(allocator, durable_dir);
    try store.enqueueSignal("order:private", "approved", "{\"ok\":true}");

    var consumed = (try store.tryConsumeSignal("order:private", "approved", unixMillis())).?;
    defer consumed.deinit();

    const path_z = try allocator.dupeZ(u8, consumed.path);
    const fd = try std.posix.openatZ(std.posix.AT.FDCWD, path_z, .{ .ACCMODE = .RDONLY }, 0);
    defer std.Io.Threaded.closeFd(fd);
    const stat = try zq.file_io.fstatFd(fd);
    try std.testing.expectEqual(@as(u32, 0o600), stat.mode & 0o777);
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

test "durable store wires idempotency ledger calls through to the fs store" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    var store = DurableStore.initFs(allocator, durable_dir);
    try std.testing.expect(!try store.hasIdempotencyLedger("idem:123", "order:123"));

    try store.writeIdempotencyLedger("idem:123", "order:123", .started);
    try std.testing.expect(try store.hasIdempotencyLedger("idem:123", "order:123"));
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

test "durable store recovers claimed signal when resume was not persisted" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    var store = DurableStore.initFs(allocator, durable_dir);
    try store.enqueueSignal("order:recover", "approved", "{\"ok\":true}");

    var claimed = (try store.tryConsumeSignal("order:recover", "approved", unixMillis())).?;
    defer claimed.deinit();
    try std.testing.expect((try store.tryConsumeSignal("order:recover", "approved", unixMillis())) == null);

    var recovered = (try store.tryRecoverSignal("order:recover", "approved", unixMillis())).?;
    defer recovered.deinit();
    try std.testing.expectEqualStrings("{\"ok\":true}", recovered.payload_json);

    store.finalizeConsumedSignal(&recovered);
    try std.testing.expect((try store.tryRecoverSignal("order:recover", "approved", unixMillis())) == null);
}

test "durable store finalizes resumed claims without touching available signals" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    var store = DurableStore.initFs(allocator, durable_dir);
    try store.enqueueSignal("order:resume", "approved", "{\"first\":true}");
    var claimed = (try store.tryConsumeSignal("order:resume", "approved", unixMillis())).?;
    defer claimed.deinit();

    try store.enqueueSignal("order:resume", "approved", "{\"second\":true}");
    try std.testing.expectEqual(@as(u32, 1), try store.finalizeResumedSignalClaims("order:resume", "approved", "{\"first\":true}"));

    var second = (try store.tryConsumeSignal("order:resume", "approved", unixMillis())).?;
    defer second.deinit();
    try std.testing.expectEqualStrings("{\"second\":true}", second.payload_json);
}

test "durable store finalizeResumedSignalClaims removes only one claim among identical-content duplicates" {
    // Two independent waitSignal("approved") deliveries with identical
    // payload content, both claimed but never cleaned up (simulating a
    // crash between the claim-rename and the oplog resume-persist for
    // each). A single finalizeResumedSignalClaims call - matching by
    // (key, name, payload_json) content since the exact claimed path isn't
    // persisted to the oplog - must resolve exactly one logical delivery,
    // not sweep away every content-matching claim at once and destroy the
    // other, still-unresumed one.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    var store = DurableStore.initFs(allocator, durable_dir);
    try store.enqueueSignal("order:dup", "approved", "{\"approved\":true}");
    try store.enqueueSignal("order:dup", "approved", "{\"approved\":true}");

    var first_claim = (try store.tryConsumeSignal("order:dup", "approved", unixMillis())).?;
    defer first_claim.deinit();
    var second_claim = (try store.tryConsumeSignal("order:dup", "approved", unixMillis())).?;
    defer second_claim.deinit();

    // Neither claim is finalized via its exact path (simulating the crash
    // window), leaving two claim files with identical content on disk.
    try std.testing.expectEqual(
        @as(u32, 1),
        try store.finalizeResumedSignalClaims("order:dup", "approved", "{\"approved\":true}"),
    );

    // Exactly one claim was removed; the other must still be recoverable
    // for the second waitSignal() call's own resolution.
    var recovered = (try store.tryRecoverSignal("order:dup", "approved", unixMillis())).?;
    defer recovered.deinit();
    try std.testing.expectEqualStrings("{\"approved\":true}", recovered.payload_json);
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

test "durable store cleanup removes only selected signal artifact kind" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    var store = DurableStore.initFs(allocator, durable_dir);
    try store.enqueueSignal("order:cleanup", "approved", "{\"ok\":true}");
    var claimed = (try store.tryConsumeSignal("order:cleanup", "approved", unixMillis())).?;
    defer claimed.deinit();

    const torn_path = try std.fmt.allocPrint(allocator, "{s}/signals/signal-torn.json", .{durable_dir});
    try zq.file_io.writeFile(allocator, torn_path, "{\"key\":\"order");
    const scanned = try store.scanSignals(allocator, unixMillis());
    defer {
        for (scanned) |*signal| signal.deinit();
        allocator.free(scanned);
    }

    const artifacts = try store.listSignalArtifacts(allocator);
    defer {
        for (artifacts) |*artifact| artifact.deinit();
        allocator.free(artifacts);
    }
    try std.testing.expectEqual(@as(usize, 2), artifacts.len);

    try std.testing.expectEqual(@as(u32, 1), try store.cleanupSignalArtifacts(.quarantined));
    var recovered = (try store.tryRecoverSignal("order:cleanup", "approved", unixMillis())) orelse return error.ExpectedRecoverableSignal;
    defer recovered.deinit();
    try std.testing.expectEqual(@as(u32, 1), try store.cleanupSignalArtifacts(.claimed));
    try std.testing.expect((try store.tryRecoverSignal("order:cleanup", "approved", unixMillis())) == null);
}

test "durable store surfaces and purges an orphaned undelivered signal" {
    // A signal delivered after its waitSignal()'s deadline has already
    // expired is never claimed (durableWaitSignal returns before touching
    // the store once timeoutExpired), so the plain envelope file is
    // otherwise invisible to listSignalArtifacts/cleanupSignalArtifacts -
    // unlike claimed/quarantined artifacts, nothing ever discovers or
    // purges it.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    var store = DurableStore.initFs(allocator, durable_dir);
    try store.enqueueSignal("order:orphan", "approved", "{\"late\":true}");

    const artifacts = try store.listSignalArtifacts(allocator);
    defer {
        for (artifacts) |*artifact| artifact.deinit();
        allocator.free(artifacts);
    }
    try std.testing.expectEqual(@as(usize, 1), artifacts.len);
    try std.testing.expectEqual(SignalArtifactKind.pending, artifacts[0].kind);

    try std.testing.expectEqual(@as(u32, 1), try store.cleanupSignalArtifacts(.pending));
    try std.testing.expect((try store.tryConsumeSignal("order:orphan", "approved", unixMillis())) == null);
}
