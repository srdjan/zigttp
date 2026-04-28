//! Append-only ledger of proof events. One JSONL line per `zigttp deploy`,
//! per `dev --watch --prove` swap, and per `zigts check` success.
//!
//! Storage: `.zigttp/proofs.jsonl`. Lines are appended via POSIX `O_APPEND`;
//! concurrent writers within the PIPE_BUF window (4 KiB on Linux/macOS)
//! cannot tear lines. Lines exceeding that window are safe under a single
//! writer.
//!
//! `ReviewFacts` (from `deploy/review.zig`) is already PII/secret-free, so
//! the ledger inherits that contract.

const std = @import("std");
const zigts = @import("zigts");
const review = @import("deploy/review.zig");
const state = @import("deploy/state.zig");
const json_util = @import("deploy/json_util.zig");

pub const EventKind = enum {
    deploy,
    swap,
    check,

    pub fn toString(self: EventKind) []const u8 {
        return switch (self) {
            .deploy => "deploy",
            .swap => "swap",
            .check => "check",
        };
    }

    pub fn fromString(s: []const u8) ?EventKind {
        if (std.mem.eql(u8, s, "deploy")) return .deploy;
        if (std.mem.eql(u8, s, "swap")) return .swap;
        if (std.mem.eql(u8, s, "check")) return .check;
        return null;
    }
};

/// Strings are owned so a parsed event survives independently of the buffer
/// it came from.
pub const Event = struct {
    ts_unix_ms: i64,
    kind: EventKind,
    handler_path: []const u8,
    service_name: ?[]const u8,
    facts: review.ReviewFacts,

    pub fn deinit(self: *Event, allocator: std.mem.Allocator) void {
        allocator.free(self.handler_path);
        if (self.service_name) |s| allocator.free(s);
        self.facts.deinit(allocator);
    }
};

pub fn ledgerPath() []const u8 {
    return ".zigttp/proofs.jsonl";
}

pub const AppendParams = struct {
    kind: EventKind,
    facts: *const review.ReviewFacts,
    handler_path: []const u8,
    service_name: ?[]const u8 = null,
    /// Override clock for tests; null means read the wall clock.
    now_unix_ms: ?i64 = null,
};

pub fn appendEvent(allocator: std.mem.Allocator, params: AppendParams) !void {
    try state.ensureStateDir();

    const ts = params.now_unix_ms orelse defaultNowMs();

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("ts");
    try json.write(ts);
    try json.objectField("kind");
    try json.write(params.kind.toString());
    try json.objectField("handlerPath");
    try json.write(params.handler_path);
    if (params.service_name) |name| {
        try json.objectField("serviceName");
        try json.write(name);
    }
    try json.objectField("facts");
    try params.facts.writeJson(&json);
    try json.endObject();
    try aw.writer.writeByte('\n');

    const bytes = aw.writer.buffered();

    const fd = try zigts.file_io.openAppend(allocator, ledgerPath());
    defer std.Io.Threaded.closeFd(fd);

    var written: usize = 0;
    while (written < bytes.len) {
        const result = std.c.write(fd, bytes[written..].ptr, bytes.len - written);
        if (result < 0) {
            // SIGCHLD from cleanup of child processes is the common case during deploys.
            if (std.posix.errno(result) == .INTR) continue;
            return error.LedgerWriteFailed;
        }
        if (result == 0) return error.LedgerWriteFailed;
        written += @intCast(result);
    }
}

/// Reads in chronological order (oldest first). A missing ledger file
/// returns an empty slice. Caller frees with `freeEvents`.
pub fn readEvents(allocator: std.mem.Allocator) ![]Event {
    const bytes = zigts.file_io.readFile(allocator, ledgerPath(), 16 * 1024 * 1024) catch |err| switch (err) {
        error.FileNotFound => return try allocator.alloc(Event, 0),
        else => return err,
    };
    defer allocator.free(bytes);

    var out: std.ArrayList(Event) = .empty;
    errdefer {
        for (out.items) |*ev| ev.deinit(allocator);
        out.deinit(allocator);
    }

    var line_iter = std.mem.splitScalar(u8, bytes, '\n');
    while (line_iter.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len == 0) continue;
        const event = try parseLine(allocator, trimmed);
        try out.append(allocator, event);
    }

    return try out.toOwnedSlice(allocator);
}

pub fn freeEvents(allocator: std.mem.Allocator, events: []Event) void {
    for (events) |*ev| ev.deinit(allocator);
    allocator.free(events);
}

// ---------------------------------------------------------------------------
// Ref resolution
// ---------------------------------------------------------------------------

/// `head` is the most recent event; `head_back(N)` is N entries before
/// HEAD; `sha_prefix` matches against `facts.contract_sha` and rejects
/// ambiguity.
pub const Ref = union(enum) {
    head: void,
    head_back: usize,
    sha_prefix: []const u8,
};

pub const RefError = error{
    EmptyRef,
    InvalidHeadOffset,
};

/// Borrows the input slice for `sha_prefix`; caller must keep it alive.
pub fn parseRef(text: []const u8) RefError!Ref {
    if (text.len == 0) return error.EmptyRef;
    if (std.mem.eql(u8, text, "HEAD")) return .{ .head = {} };
    if (std.mem.startsWith(u8, text, "HEAD~")) {
        const tail = text["HEAD~".len..];
        if (tail.len == 0) return error.InvalidHeadOffset;
        const n = std.fmt.parseInt(usize, tail, 10) catch return error.InvalidHeadOffset;
        return .{ .head_back = n };
    }
    return .{ .sha_prefix = text };
}

pub const ResolveError = error{
    NoEvents,
    OutOfRange,
    NotFound,
    Ambiguous,
};

/// `events` are ordered oldest-first as returned by `readEvents`.
pub fn resolve(events: []const Event, ref: Ref) ResolveError!usize {
    if (events.len == 0) return error.NoEvents;
    return switch (ref) {
        .head => events.len - 1,
        .head_back => |n| if (n >= events.len) error.OutOfRange else events.len - 1 - n,
        .sha_prefix => |prefix| blk: {
            var match: ?usize = null;
            for (events, 0..) |ev, i| {
                if (std.mem.startsWith(u8, ev.facts.contract_sha, prefix)) {
                    if (match != null) break :blk error.Ambiguous;
                    match = i;
                }
            }
            break :blk match orelse error.NotFound;
        },
    };
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

// Wall clock in unix milliseconds. Mirrors `control_plane.defaultNowSec` but
// includes ms precision so two events appended within the same second still
// sort deterministically.
fn defaultNowMs() i64 {
    var ts: std.posix.timespec = undefined;
    switch (std.posix.errno(std.posix.system.clock_gettime(.REALTIME, &ts))) {
        .SUCCESS => {
            const sec_ms: i64 = @as(i64, @intCast(ts.sec)) * 1000;
            const nsec_ms: i64 = @divTrunc(@as(i64, @intCast(ts.nsec)), std.time.ns_per_ms);
            return sec_ms + nsec_ms;
        },
        else => return 0,
    }
}

fn parseLine(allocator: std.mem.Allocator, line: []const u8) !Event {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, line, .{});
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidLedgerLine;
    const obj = parsed.value.object;

    const ts = json_util.getI64(obj, "ts") orelse return error.InvalidLedgerLine;

    const kind_str = json_util.getString(obj, "kind") orelse return error.InvalidLedgerLine;
    const kind = EventKind.fromString(kind_str) orelse return error.InvalidLedgerLine;

    const handler_path_value = json_util.getString(obj, "handlerPath") orelse return error.InvalidLedgerLine;
    const handler_path = try allocator.dupe(u8, handler_path_value);
    errdefer allocator.free(handler_path);

    var service_name: ?[]const u8 = null;
    errdefer if (service_name) |s| allocator.free(s);
    if (json_util.getString(obj, "serviceName")) |name| {
        service_name = try allocator.dupe(u8, name);
    }

    const facts_value = obj.get("facts") orelse return error.InvalidLedgerLine;
    if (facts_value != .object) return error.InvalidLedgerLine;
    var facts = try review.ReviewFacts.parseJson(allocator, facts_value.object);
    errdefer facts.deinit(allocator);

    return .{
        .ts_unix_ms = ts,
        .kind = kind,
        .handler_path = handler_path,
        .service_name = service_name,
        .facts = facts,
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

/// Chdir to a tmp dir for tests that need an isolated `.zigttp/` root.
/// Returned slice is the previous cwd; caller frees with the testing
/// allocator and restores via `std.Io.Threaded.chdir`.
pub fn chdirTmpForTest(tmp: *std.testing.TmpDir) ![:0]u8 {
    const old_cwd = try std.process.currentPathAlloc(testing.io, testing.allocator);
    errdefer testing.allocator.free(old_cwd);
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    try std.Io.Threaded.chdir(buf[0..len]);
    return old_cwd;
}

/// Test fixture: a ReviewFacts with one env key and a few proven properties.
/// `with_route` adds a `/healthz` route so callers can build deltas that
/// flip between safe and safe_with_additions.
pub fn buildFactsForTest(
    allocator: std.mem.Allocator,
    sha: []const u8,
    with_route: bool,
) !review.ReviewFacts {
    var routes: []review.Route = undefined;
    if (with_route) {
        routes = try allocator.alloc(review.Route, 1);
        routes[0] = .{ .pattern = try allocator.dupe(u8, "/healthz"), .is_prefix = false };
    } else {
        routes = try allocator.alloc(review.Route, 0);
    }
    return review.ReviewFacts{
        .contract_sha = try allocator.dupe(u8, sha),
        .proof_level = .complete,
        .env_keys = blk: {
            const arr = try allocator.alloc([]const u8, 1);
            arr[0] = try allocator.dupe(u8, "PORT");
            break :blk arr;
        },
        .egress_hosts = try allocator.alloc([]const u8, 0),
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = routes,
        .capabilities = try allocator.alloc([]const u8, 0),
        .properties = .{ .results_safe = true, .read_only = true },
    };
}

test "appendEvent then readEvents round trips" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var facts = try buildFactsForTest(testing.allocator, "sha-abc123", false);
    defer facts.deinit(testing.allocator);

    try appendEvent(testing.allocator, .{
        .kind = .deploy,
        .facts = &facts,
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .now_unix_ms = 1_700_000_000_000,
    });

    const events = try readEvents(testing.allocator);
    defer freeEvents(testing.allocator, events);

    try testing.expectEqual(@as(usize, 1), events.len);
    try testing.expectEqual(EventKind.deploy, events[0].kind);
    try testing.expectEqual(@as(i64, 1_700_000_000_000), events[0].ts_unix_ms);
    try testing.expectEqualStrings("src/handler.ts", events[0].handler_path);
    try testing.expect(events[0].service_name != null);
    try testing.expectEqualStrings("demo", events[0].service_name.?);
    try testing.expectEqualStrings("sha-abc123", events[0].facts.contract_sha);
    try testing.expectEqual(@as(usize, 1), events[0].facts.env_keys.len);
    try testing.expectEqualStrings("PORT", events[0].facts.env_keys[0]);
    try testing.expect(events[0].facts.properties.results_safe);
    try testing.expect(events[0].facts.properties.read_only);
}

test "readEvents returns empty when ledger is missing" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const events = try readEvents(testing.allocator);
    defer freeEvents(testing.allocator, events);
    try testing.expectEqual(@as(usize, 0), events.len);
}

test "appendEvent appends in order across calls" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-1", false);
    defer f1.deinit(testing.allocator);
    var f2 = try buildFactsForTest(testing.allocator, "sha-2", false);
    defer f2.deinit(testing.allocator);
    var f3 = try buildFactsForTest(testing.allocator, "sha-3", false);
    defer f3.deinit(testing.allocator);

    try appendEvent(testing.allocator, .{
        .kind = .check,
        .facts = &f1,
        .handler_path = "src/handler.ts",
        .now_unix_ms = 1,
    });
    try appendEvent(testing.allocator, .{
        .kind = .swap,
        .facts = &f2,
        .handler_path = "src/handler.ts",
        .now_unix_ms = 2,
    });
    try appendEvent(testing.allocator, .{
        .kind = .deploy,
        .facts = &f3,
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .now_unix_ms = 3,
    });

    const events = try readEvents(testing.allocator);
    defer freeEvents(testing.allocator, events);

    try testing.expectEqual(@as(usize, 3), events.len);
    try testing.expectEqual(EventKind.check, events[0].kind);
    try testing.expectEqual(EventKind.swap, events[1].kind);
    try testing.expectEqual(EventKind.deploy, events[2].kind);
    try testing.expectEqualStrings("sha-1", events[0].facts.contract_sha);
    try testing.expectEqualStrings("sha-3", events[2].facts.contract_sha);
}

test "parseRef accepts HEAD, HEAD~N, sha prefix" {
    try testing.expectEqual(Ref{ .head = {} }, try parseRef("HEAD"));

    const back = try parseRef("HEAD~3");
    try testing.expectEqual(@as(usize, 3), back.head_back);

    const sha = try parseRef("abc123");
    try testing.expectEqualStrings("abc123", sha.sha_prefix);
}

test "parseRef rejects empty and malformed offsets" {
    try testing.expectError(RefError.EmptyRef, parseRef(""));
    try testing.expectError(RefError.InvalidHeadOffset, parseRef("HEAD~"));
    try testing.expectError(RefError.InvalidHeadOffset, parseRef("HEAD~xyz"));
}

test "resolve handles HEAD, HEAD~N, sha prefix" {
    const allocator = testing.allocator;
    var f1 = try buildFactsForTest(allocator, "sha-aaa-1", false);
    defer f1.deinit(allocator);
    var f2 = try buildFactsForTest(allocator, "sha-bbb-2", false);
    defer f2.deinit(allocator);
    var f3 = try buildFactsForTest(allocator, "sha-ccc-3", false);
    defer f3.deinit(allocator);

    const events = [_]Event{
        .{ .ts_unix_ms = 1, .kind = .check, .handler_path = "h", .service_name = null, .facts = f1 },
        .{ .ts_unix_ms = 2, .kind = .swap, .handler_path = "h", .service_name = null, .facts = f2 },
        .{ .ts_unix_ms = 3, .kind = .deploy, .handler_path = "h", .service_name = null, .facts = f3 },
    };

    try testing.expectEqual(@as(usize, 2), try resolve(&events, .{ .head = {} }));
    try testing.expectEqual(@as(usize, 1), try resolve(&events, .{ .head_back = 1 }));
    try testing.expectEqual(@as(usize, 0), try resolve(&events, .{ .head_back = 2 }));
    try testing.expectError(ResolveError.OutOfRange, resolve(&events, .{ .head_back = 3 }));
    try testing.expectEqual(@as(usize, 1), try resolve(&events, .{ .sha_prefix = "sha-bbb" }));
    try testing.expectError(ResolveError.NotFound, resolve(&events, .{ .sha_prefix = "sha-zzz" }));
    try testing.expectError(ResolveError.Ambiguous, resolve(&events, .{ .sha_prefix = "sha-" }));
}

test "resolve on empty ledger fails with NoEvents" {
    const events = &[_]Event{};
    try testing.expectError(ResolveError.NoEvents, resolve(events, .{ .head = {} }));
}

test "readEvents rejects malformed lines" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try state.ensureStateDir();

    const cases = [_][]const u8{
        "[1,2,3]\n", // root is array, not object
        "{\"kind\":\"deploy\",\"handlerPath\":\"h.ts\",\"facts\":{}}\n", // missing ts
        "{\"ts\":1,\"handlerPath\":\"h.ts\",\"facts\":{}}\n", // missing kind
        "{\"ts\":1,\"kind\":\"unknown-kind\",\"handlerPath\":\"h.ts\",\"facts\":{}}\n", // bad kind
        "{\"ts\":1,\"kind\":\"deploy\",\"facts\":{}}\n", // missing handlerPath
        "{\"ts\":1,\"kind\":\"deploy\",\"handlerPath\":\"h.ts\"}\n", // missing facts
        "{\"ts\":1,\"kind\":\"deploy\",\"handlerPath\":\"h.ts\",\"facts\":\"oops\"}\n", // facts not object
    };

    for (cases) |line| {
        // writeFile uses O_TRUNC, so each iteration starts fresh.
        try zigts.file_io.writeFile(testing.allocator, ledgerPath(), line);
        try testing.expectError(error.InvalidLedgerLine, readEvents(testing.allocator));
    }
}
