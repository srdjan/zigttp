const std = @import("std");
const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const review = @import("deploy/review.zig");
const proof_ledger = @import("proof_ledger.zig");
const proof_card_tui = @import("proof_card_tui.zig");
const compat = zigts.compat;
const demo = @import("demo.zig");

/// Mutated and read only on the live-reload thread, so no extra sync beyond
/// the existing `replaceJson` swap is required.
pub const RecentEntry = struct {
    timestamp_ms: i64,
    verdict: review.Verdict,
    recompile_ms: ?u64,
    /// First 8 hex chars of `facts.contract_sha`, padded if shorter.
    sha8: [8]u8,
    sha8_len: u8,
};

pub const recent_capacity: usize = 10;

/// One prove cycle's worth of inputs into the studio state. Grouping these
/// keeps the call site readable as fields grow and makes the optional
/// counterexample obvious at the call site.
pub const FactsUpdate = struct {
    facts: *const review.ReviewFacts,
    baseline: ?*const review.ReviewFacts,
    delta: *const review.ReviewDelta,
    recompile_ms: ?u64,
    counterexample: ?review.CounterexamplePreview = null,
    /// Causes for demoted properties, used to render the "Why:" row in the
    /// proof card frame. Empty means no demotion attribution to show; the
    /// frame still renders but skips the Why row, matching terminal behavior.
    property_causes: []const review.PropertyCauseEntry = &.{},
};

/// A single ZTS diagnostic carried into studio for browser-side display.
/// All slices are owned by the producer so the diagnostic stays valid past
/// the transient `CheckResult` lifetime upstream.
pub const Diagnostic = struct {
    code: []u8,
    severity: []u8,
    file: []u8,
    line: u32,
    column: u16,
    message: []u8,
    suggestion: ?[]u8,

    pub fn deinit(self: *Diagnostic, allocator: std.mem.Allocator) void {
        allocator.free(self.code);
        allocator.free(self.severity);
        allocator.free(self.file);
        allocator.free(self.message);
        if (self.suggestion) |s| allocator.free(s);
        self.* = undefined;
    }
};

pub const State = struct {
    allocator: std.mem.Allocator,
    handler_path: []u8,
    demo_config: ?DemoConfig = null,
    mutex: compat.Mutex = .{},
    json: []u8,
    recent: [recent_capacity]RecentEntry = undefined,
    recent_count: u8 = 0,
    /// Server-Sent Events subscribers. Each fd belongs to a long-lived HTTP
    /// connection that the threaded request loop has handed off via
    /// `RequestOutcome.transferred`. broadcast() iterates this list whenever
    /// the json buffer is refreshed.
    subscribers: std.ArrayList(std.posix.fd_t) = .empty,
    subscribers_mutex: compat.Mutex = .{},

    pub fn init(allocator: std.mem.Allocator, handler_path: []const u8) !State {
        return .{
            .allocator = allocator,
            .handler_path = try allocator.dupe(u8, handler_path),
            .json = try initialJson(allocator, handler_path),
        };
    }

    pub fn initDemo(allocator: std.mem.Allocator, handler_path: []const u8, config: DemoConfig) !State {
        return .{
            .allocator = allocator,
            .handler_path = try allocator.dupe(u8, handler_path),
            .demo_config = .{
                .workspace_root = try allocator.dupe(u8, config.workspace_root),
            },
            .json = try initialJson(allocator, handler_path),
        };
    }

    pub fn deinit(self: *State) void {
        self.subscribers_mutex.lock();
        for (self.subscribers.items) |fd| _ = std.c.close(fd);
        self.subscribers.deinit(self.allocator);
        self.subscribers_mutex.unlock();
        if (self.demo_config) |*cfg| cfg.deinit(self.allocator);
        self.allocator.free(self.handler_path);
        self.allocator.free(self.json);
        self.* = undefined;
    }

    pub fn subscribe(self: *State, fd: std.posix.fd_t) !void {
        self.subscribers_mutex.lock();
        defer self.subscribers_mutex.unlock();
        try self.subscribers.append(self.allocator, fd);
    }

    /// Push the current json payload to every SSE subscriber as a single
    /// `data: ...\n\n` event. Subscribers whose write fails are closed and
    /// dropped from the list. Safe to call from any thread.
    pub fn broadcast(self: *State) void {
        var payload: []u8 = undefined;
        {
            self.mutex.lock();
            defer self.mutex.unlock();
            payload = self.allocator.dupe(u8, self.json) catch return;
        }
        defer self.allocator.free(payload);

        var frame: std.ArrayList(u8) = .empty;
        defer frame.deinit(self.allocator);
        frame.appendSlice(self.allocator, "data: ") catch return;
        frame.appendSlice(self.allocator, payload) catch return;
        frame.appendSlice(self.allocator, "\n\n") catch return;

        self.subscribers_mutex.lock();
        defer self.subscribers_mutex.unlock();
        var i: usize = 0;
        while (i < self.subscribers.items.len) {
            const fd = self.subscribers.items[i];
            if (writeAllFd(fd, frame.items)) {
                i += 1;
            } else |_| {
                _ = std.c.close(fd);
                _ = self.subscribers.swapRemove(i);
            }
        }
    }

    pub fn updateChecking(self: *State) void {
        const next = initialJson(self.allocator, self.handler_path) catch return;
        self.replaceJson(next);
    }

    pub fn updateError(self: *State, message: []const u8) void {
        const next = errorJson(self.allocator, self.handler_path, message, null) catch return;
        self.replaceJson(next);
    }

    /// Replace state with `status: error` plus a structured diagnostics array.
    /// Callers own the diagnostic slice; this method renders synchronously and
    /// does not retain it.
    pub fn updateDiagnostics(self: *State, message: []const u8, diagnostics: []const Diagnostic) void {
        const next = errorJson(self.allocator, self.handler_path, message, diagnostics) catch return;
        self.replaceJson(next);
    }

    pub fn updateFacts(self: *State, update: FactsUpdate) void {
        self.pushRecent(update.facts, update.delta, update.recompile_ms);
        const next = factsJson(
            self.allocator,
            self.handler_path,
            update,
            self.recent[0..self.recent_count],
        ) catch return;
        self.replaceJson(next);
    }

    fn pushRecent(
        self: *State,
        facts: *const review.ReviewFacts,
        delta: *const review.ReviewDelta,
        recompile_ms: ?u64,
    ) void {
        const verdict = review.classify(delta);
        var entry = RecentEntry{
            .timestamp_ms = proof_ledger.defaultNowMs(),
            .verdict = verdict,
            .recompile_ms = recompile_ms,
            .sha8 = [_]u8{0} ** 8,
            .sha8_len = 0,
        };
        const sha = facts.contract_sha;
        const len: u8 = @intCast(@min(sha.len, entry.sha8.len));
        @memcpy(entry.sha8[0..len], sha[0..len]);
        entry.sha8_len = len;

        const keep: usize = @min(self.recent_count, recent_capacity - 1);
        var i: usize = keep;
        while (i > 0) : (i -= 1) {
            self.recent[i] = self.recent[i - 1];
        }
        self.recent[0] = entry;
        self.recent_count = @intCast(keep + 1);
    }

    pub fn stateJsonCopy(self: *State, allocator: std.mem.Allocator) ![]u8 {
        self.mutex.lock();
        defer self.mutex.unlock();
        return try allocator.dupe(u8, self.json);
    }

    pub fn demoStateJsonCopy(self: *State, allocator: std.mem.Allocator) ![]u8 {
        const cfg = self.demo_config orelse return error.DemoDisabled;
        const proof_json = try self.stateJsonCopy(allocator);
        defer allocator.free(proof_json);
        return try demo.writeStateJson(allocator, .{
            .workspace_root = cfg.workspace_root,
            .handler_path = self.handler_path,
        }, proof_json);
    }

    pub fn applyDemoAction(self: *State, allocator: std.mem.Allocator, action: demo.Action) ![]u8 {
        const cfg = self.demo_config orelse return error.DemoDisabled;
        const step = try demo.applyAction(allocator, .{
            .workspace_root = cfg.workspace_root,
            .handler_path = self.handler_path,
        }, action);
        self.broadcast();

        var aw: std.Io.Writer.Allocating = .init(allocator);
        defer aw.deinit();
        var json: std.json.Stringify = .{ .writer = &aw.writer };
        try json.beginObject();
        try json.objectField("ok");
        try json.write(true);
        try json.objectField("action");
        try json.write(action.toString());
        try json.objectField("step");
        try json.write(step.toString());
        try json.endObject();
        return try allocator.dupe(u8, aw.writer.buffered());
    }

    pub fn generatedTests(self: *State, allocator: std.mem.Allocator) ![]u8 {
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
        _ = try zigts_cli.precompile.runGenTests(allocator, self.handler_path, &aw.writer);
        buf = aw.toArrayList();
        return try buf.toOwnedSlice(allocator);
    }

    /// `error.InvalidWitnessKey` if the key is not 1..64 hex chars.
    /// `error.WitnessNotFound` if the witness file does not exist.
    pub fn witnessDetailJson(
        self: *State,
        allocator: std.mem.Allocator,
        key: []const u8,
    ) ![]u8 {
        if (!isHexKey(key)) return error.InvalidWitnessKey;

        const corpus_dir = zigts.witness_corpus.corpusDir(allocator, self.handler_path) catch return error.WitnessNotFound;
        defer allocator.free(corpus_dir);

        const file_path = try std.fmt.allocPrint(
            allocator,
            "{s}/{s}.witness.jsonl",
            .{ corpus_dir, key },
        );
        defer allocator.free(file_path);

        const bytes = zigts.file_io.readFile(allocator, file_path, 1 * 1024 * 1024) catch |err| switch (err) {
            error.FileNotFound => return error.WitnessNotFound,
            else => return err,
        };
        defer allocator.free(bytes);

        const marker_path = try std.fmt.allocPrint(
            allocator,
            "{s}/{s}.pinned",
            .{ corpus_dir, key },
        );
        defer allocator.free(marker_path);
        const pinned = zigts.file_io.fileExists(allocator, marker_path);

        var aw: std.Io.Writer.Allocating = .init(allocator);
        defer aw.deinit();
        var json: std.json.Stringify = .{ .writer = &aw.writer };
        try json.beginObject();
        try json.objectField("key");
        try json.write(key);
        try json.objectField("pinned");
        try json.write(pinned);
        try json.objectField("events");
        try json.beginArray();

        var lines = std.mem.splitScalar(u8, bytes, '\n');
        while (lines.next()) |line| {
            if (line.len == 0) continue;
            const parsed = std.json.parseFromSlice(std.json.Value, allocator, line, .{}) catch continue;
            defer parsed.deinit();
            try json.write(parsed.value);
        }
        try json.endArray();
        try json.endObject();
        return try allocator.dupe(u8, aw.writer.buffered());
    }

    fn replaceJson(self: *State, next: []u8) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        self.allocator.free(self.json);
        self.json = next;
    }
};

pub const DemoConfig = struct {
    workspace_root: []const u8,

    fn deinit(self: *DemoConfig, allocator: std.mem.Allocator) void {
        allocator.free(self.workspace_root);
        self.* = undefined;
    }
};

fn writeAllFd(fd: std.posix.fd_t, data: []const u8) !void {
    var remaining = data;
    while (remaining.len > 0) {
        const result = std.c.write(fd, remaining.ptr, remaining.len);
        if (result <= 0) return error.WriteFailed;
        const n: usize = @intCast(result);
        remaining = remaining[n..];
    }
}

pub fn isStudioPath(path: []const u8) bool {
    if (std.mem.eql(u8, path, "/_zigttp/studio")) return true;
    if (std.mem.eql(u8, path, "/_zigttp/studio/")) return true;
    if (std.mem.eql(u8, path, "/_zigttp/studio/state.json")) return true;
    if (std.mem.eql(u8, path, "/_zigttp/studio/tests.jsonl")) return true;
    if (std.mem.eql(u8, path, demo.state_path)) return true;
    if (std.mem.eql(u8, path, demo.action_path)) return true;
    if (std.mem.eql(u8, path, sse_path)) return true;
    if (witnessDetailKey(path) != null) return true;
    return false;
}

pub const sse_path = "/_zigttp/studio/events";
pub const demo_state_path = demo.state_path;
pub const demo_action_path = demo.action_path;

pub fn parseDemoAction(allocator: std.mem.Allocator, body: ?[]const u8) !demo.Action {
    return demo.parseActionBody(allocator, body);
}

pub const sse_response_headers =
    "HTTP/1.1 200 OK\r\n" ++
    "Content-Type: text/event-stream\r\n" ++
    "Cache-Control: no-cache\r\n" ++
    "Connection: keep-alive\r\n" ++
    "X-Accel-Buffering: no\r\n" ++
    "\r\n";

/// Write the SSE handshake plus the current state snapshot to `fd` and
/// register the fd as a broadcast subscriber. The fd is owned by the studio
/// state from here on - callers must hand off ownership (the threaded
/// dispatch returns `RequestOutcome.transferred`).
pub fn upgradeToSse(state: *State, fd: std.posix.fd_t, allocator: std.mem.Allocator) !void {
    try writeAllFd(fd, sse_response_headers);

    const snap = try state.stateJsonCopy(allocator);
    defer allocator.free(snap);

    var frame: std.ArrayList(u8) = .empty;
    defer frame.deinit(allocator);
    try frame.appendSlice(allocator, "data: ");
    try frame.appendSlice(allocator, snap);
    try frame.appendSlice(allocator, "\n\n");
    try writeAllFd(fd, frame.items);

    try state.subscribe(fd);
}

/// If `path` is `/_zigttp/studio/witness/<key>.json` and `<key>` is a valid
/// hex string, return the key slice (borrowed from `path`). Otherwise null.
pub fn witnessDetailKey(path: []const u8) ?[]const u8 {
    const prefix = "/_zigttp/studio/witness/";
    const suffix = ".json";
    if (!std.mem.startsWith(u8, path, prefix)) return null;
    if (!std.mem.endsWith(u8, path, suffix)) return null;
    const key = path[prefix.len .. path.len - suffix.len];
    if (!isHexKey(key)) return null;
    return key;
}

fn isHexKey(key: []const u8) bool {
    if (key.len == 0 or key.len > 64) return false;
    for (key) |c| {
        if (!std.ascii.isHex(c)) return false;
    }
    return true;
}

fn initialJson(allocator: std.mem.Allocator, handler_path: []const u8) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("status");
    try json.write("checking");
    try json.objectField("handlerPath");
    try json.write(handler_path);
    try json.objectField("message");
    try json.write("running proof analysis");
    try json.endObject();
    return try allocator.dupe(u8, aw.writer.buffered());
}

fn errorJson(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    message: []const u8,
    diagnostics: ?[]const Diagnostic,
) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("status");
    try json.write("error");
    try json.objectField("handlerPath");
    try json.write(handler_path);
    try json.objectField("message");
    try json.write(message);
    if (diagnostics) |list| if (list.len > 0) {
        try json.objectField("diagnostics");
        try writeDiagnosticsJson(&json, list);
    };
    try json.endObject();
    return try allocator.dupe(u8, aw.writer.buffered());
}

fn writeDiagnosticsJson(json: *std.json.Stringify, diagnostics: []const Diagnostic) !void {
    try json.beginArray();
    for (diagnostics) |d| {
        try json.beginObject();
        try json.objectField("code");
        try json.write(d.code);
        try json.objectField("severity");
        try json.write(d.severity);
        try json.objectField("file");
        try json.write(d.file);
        try json.objectField("line");
        try json.write(d.line);
        try json.objectField("column");
        try json.write(d.column);
        try json.objectField("message");
        try json.write(d.message);
        if (d.suggestion) |s| {
            try json.objectField("suggestion");
            try json.write(s);
        }
        try json.endObject();
    }
    try json.endArray();
}

/// Render the same ProofCard frame the terminal HUD prints, returning the
/// rendered ASCII text. Used to populate the studio's "frame" JSON field so
/// the browser overlay shows the literal terminal output, byte-for-byte.
/// The renderer is the single source of truth for both surfaces.
fn renderProofCardFrame(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    update: FactsUpdate,
) ![]u8 {
    const card = review.ProofCard{
        .handler_path = handler_path,
        .service_name = "dev",
        .region = "local",
        .current = update.facts,
        .baseline = update.baseline,
        .delta = update.delta,
        .property_causes = update.property_causes,
        .counterexample = update.counterexample,
    };
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    const recompile_ms_u32: ?u32 = if (update.recompile_ms) |ms|
        @intCast(@min(ms, std.math.maxInt(u32)))
    else
        null;
    try proof_card_tui.writeProofCardFrame(allocator, &card, &aw.writer, .{
        .recompile_ms = recompile_ms_u32,
    });
    return try allocator.dupe(u8, aw.writer.buffered());
}

fn factsJson(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    update: FactsUpdate,
    recent: []const RecentEntry,
) ![]u8 {
    const witnesses = try loadWitnessEntries(allocator, handler_path);
    defer if (witnesses) |entries| zigts.witness_corpus.freeEntries(allocator, entries);
    const witness_total: usize = if (witnesses) |entries| entries.len else 0;
    const verdict = review.classify(update.delta);

    // Render the proof card frame once and embed it as a string. Failure here
    // degrades to an empty frame string rather than aborting the JSON snapshot:
    // the browser can fall back to its native panes if the mirror is unavailable.
    const frame_bytes = renderProofCardFrame(allocator, handler_path, update) catch null;
    defer if (frame_bytes) |b| allocator.free(b);

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("status");
    try json.write("ready");
    try json.objectField("handlerPath");
    try json.write(handler_path);
    try json.objectField("verdict");
    try json.write(verdict.toString());
    if (update.recompile_ms) |ms| {
        try json.objectField("recompileMs");
        try json.write(ms);
    }
    try json.objectField("frame");
    try json.write(if (frame_bytes) |b| b else "");

    // Pre-rendered AI proof certificate. Same artifact the TUI shows in
    // its Handover lens and the Studio Copy button hands to the
    // clipboard. Failure to render degrades to an empty string.
    const cert_card = review.ProofCard{
        .handler_path = handler_path,
        .service_name = "dev",
        .region = "local",
        .current = update.facts,
        .baseline = update.baseline,
        .delta = update.delta,
        .property_causes = update.property_causes,
        .counterexample = update.counterexample,
    };
    const cert_bytes = proof_card_tui.buildProofCertificate(allocator, &cert_card) catch null;
    defer if (cert_bytes) |b| allocator.free(b);
    try json.objectField("proofCertificate");
    try json.write(if (cert_bytes) |b| b else "");

    try json.objectField("facts");
    try update.facts.writeJson(&json);
    if (update.baseline) |b| {
        try json.objectField("baseline");
        try b.writeJson(&json);
    }
    try json.objectField("delta");
    try writeDeltaJson(&json, update.delta);
    try json.objectField("witnesses");
    try writeWitnessesJson(allocator, &json, witnesses);
    if (update.counterexample) |cx| {
        try json.objectField("counterexample");
        try writeCounterexampleJson(&json, &cx);
    }
    try json.objectField("releaseReadiness");
    try writeReleaseReadinessJson(&json, update.facts, verdict);
    try json.objectField("nextActions");
    try writeNextActionsJson(allocator, &json, handler_path, update.facts, verdict, witness_total);
    try json.objectField("recent");
    try writeRecentJson(&json, recent);
    try json.endObject();
    return try allocator.dupe(u8, aw.writer.buffered());
}

/// Mirrors `review.CounterexamplePreview` field-for-field so the browser
/// renderer reads it without a translation layer.
fn writeCounterexampleJson(
    json: *std.json.Stringify,
    cx: *const review.CounterexamplePreview,
) !void {
    try json.beginObject();
    try json.objectField("field");
    try json.write(cx.field);
    try json.objectField("label");
    try json.write(cx.label);
    try json.objectField("line");
    try json.write(cx.line);
    try json.objectField("column");
    try json.write(cx.column);
    try json.objectField("snippet");
    try json.write(cx.snippet);
    try json.objectField("handlerPath");
    try json.write(cx.handler_path);
    if (cx.suggestion) |hint| {
        try json.objectField("suggestion");
        try json.write(hint);
    }
    if (cx.failing_request) |req| {
        try json.objectField("failingRequest");
        try json.beginObject();
        try json.objectField("method");
        try json.write(req.method);
        try json.objectField("url");
        try json.write(req.url);
        try json.objectField("hasAuthHeader");
        try json.write(req.has_auth_header);
        if (req.body) |body| {
            try json.objectField("body");
            try json.write(body);
        }
        try json.endObject();
    }
    if (cx.previous_response) |prev| {
        try json.objectField("previousResponse");
        try writeReplayResponseJson(json, prev);
    }
    if (cx.current_response) |curr| {
        try json.objectField("currentResponse");
        try writeReplayResponseJson(json, curr);
    }
    try json.endObject();
}

fn writeReplayResponseJson(
    json: *std.json.Stringify,
    r: review.CounterexamplePreview.ReplayResponse,
) !void {
    try json.beginObject();
    try json.objectField("status");
    try json.write(r.status);
    try json.objectField("body");
    try json.write(r.body);
    if (r.error_text) |err| {
        try json.objectField("error");
        try json.write(err);
    }
    try json.endObject();
}

fn writeRecentJson(json: *std.json.Stringify, recent: []const RecentEntry) !void {
    try json.beginArray();
    for (recent) |entry| {
        try json.beginObject();
        try json.objectField("timestampMs");
        try json.write(entry.timestamp_ms);
        try json.objectField("verdict");
        try json.write(entry.verdict.toString());
        if (entry.recompile_ms) |ms| {
            try json.objectField("recompileMs");
            try json.write(ms);
        }
        try json.objectField("sha8");
        try json.write(entry.sha8[0..entry.sha8_len]);
        try json.endObject();
    }
    try json.endArray();
}

fn loadWitnessEntries(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
) !?[]zigts.witness_corpus.Entry {
    const corpus_dir = zigts.witness_corpus.corpusDir(allocator, handler_path) catch return null;
    defer allocator.free(corpus_dir);
    return zigts.witness_corpus.loadEntries(allocator, corpus_dir) catch |err| switch (err) {
        error.WitnessCorpusMissing => null,
        else => err,
    };
}

fn writeReleaseReadinessJson(
    json: *std.json.Stringify,
    facts: *const review.ReviewFacts,
    verdict: review.Verdict,
) !void {
    const specs_ok = specsPass(facts);
    const deploy_ready = specs_ok and verdict != .breaking;

    try json.beginObject();
    try json.objectField("declaredSpecsPass");
    try json.write(specs_ok);
    try json.objectField("deployVerdict");
    try json.write(verdict.toString());
    try json.objectField("deployReady");
    try json.write(deploy_ready);
    try json.endObject();
}

fn writeNextActionsJson(
    allocator: std.mem.Allocator,
    json: *std.json.Stringify,
    handler_path: []const u8,
    facts: *const review.ReviewFacts,
    verdict: review.Verdict,
    witness_total: usize,
) !void {
    try json.beginArray();
    if (facts.declared_specs.len == 0) {
        try writeAction(json, .{
            .kind = "add_specs",
            .severity = "info",
            .title = "Declare source-level proof guardrails",
            .command = "import type { Spec } from \"zigttp:types\"; type Guardrails = Spec<\"idempotent\" | \"deterministic\" | \"injection_safe\">;",
            .detail = "Add a Spec<...> alias and intersect it with the handler return type so the compiler enforces the author's intent.",
        });
    }

    if (!specsPass(facts)) {
        const command = try std.fmt.allocPrint(allocator, "zigts check {s} --json", .{handler_path});
        defer allocator.free(command);
        try writeAction(json, .{
            .kind = "repair_specs",
            .severity = "error",
            .title = "Repair failed declared specs",
            .command = command,
            .detail = "Surface failing Spec<...> obligations as compiler diagnostics, then drive the repair loop from zigts expert via /specs <handler>.",
        });
    }

    if (witness_total > 0) {
        const command = try std.fmt.allocPrint(allocator, "zigttp witnesses list {s}", .{handler_path});
        defer allocator.free(command);
        try writeAction(json, .{
            .kind = "inspect_witnesses",
            .severity = "warning",
            .title = "Inspect persisted counterexamples",
            .command = command,
            .detail = "Replay, pin, or mint witness regressions before release so known falsifying inputs stay defended.",
        });
    }

    const declared_specs_pass = specsPass(facts);

    if (verdict == .breaking) {
        try writeAction(json, .{
            .kind = "review_breaking_delta",
            .severity = "error",
            .title = "Review breaking proof delta",
            .command = "zigttp proofs diff HEAD~1 HEAD",
            .detail = "The current proof delta removes surface or demotes a property. Review before deploy.",
        });
    } else if (declared_specs_pass) {
        try writeAction(json, .{
            .kind = "deploy_ready",
            .severity = "success",
            .title = "Proof state is deploy-ready",
            .command = "zigttp deploy",
            .detail = "The handler verifies, declared specs pass, and the proof delta is not breaking.",
        });
        try writeAction(json, .{
            .kind = "share_badge",
            .severity = "success",
            .title = "Write a proof badge",
            .command = "zigttp deploy && zigttp proofs badge",
            .detail = "After the local deploy writes a ledger row, export ./zigttp-proof.svg and the README markdown snippet.",
        });
    }
    try json.endArray();
}

const Action = struct {
    kind: []const u8,
    severity: []const u8,
    title: []const u8,
    command: []const u8,
    detail: []const u8,
};

fn writeAction(json: *std.json.Stringify, action: Action) !void {
    try json.beginObject();
    try json.objectField("kind");
    try json.write(action.kind);
    try json.objectField("severity");
    try json.write(action.severity);
    try json.objectField("title");
    try json.write(action.title);
    try json.objectField("command");
    try json.write(action.command);
    try json.objectField("detail");
    try json.write(action.detail);
    try json.endObject();
}

fn specsPass(facts: *const review.ReviewFacts) bool {
    for (facts.declared_specs) |spec| {
        if (!spec.discharged) return false;
    }
    return true;
}

fn writeWitnessesJson(
    allocator: std.mem.Allocator,
    json: *std.json.Stringify,
    entries: ?[]const zigts.witness_corpus.Entry,
) !void {
    const visible = entries orelse &[_]zigts.witness_corpus.Entry{};

    try json.beginObject();
    try json.objectField("total");
    try json.write(visible.len);

    try json.objectField("byProperty");
    try json.beginObject();
    if (visible.len > 0) {
        const counts = try zigts.witness_corpus.countByPropertySlice(allocator, visible);
        defer zigts.witness_corpus.freeCounts(allocator, counts);
        for (counts) |c| {
            try json.objectField(c.property);
            try json.write(c.count);
        }
    }
    try json.endObject();

    // The HUD shows at most 20 entries; deeper inspection lives in the CLI
    // and the pi_witnesses agent tool.
    const max_entries: usize = 20;
    const entry_count = @min(visible.len, max_entries);
    try json.objectField("entries");
    try json.beginArray();
    for (visible[0..entry_count]) |e| {
        try json.beginObject();
        try json.objectField("key");
        try json.write(e.key);
        try json.objectField("property");
        try json.write(e.property);
        try json.objectField("summary");
        try json.write(e.summary);
        try json.objectField("pinned");
        try json.write(e.pinned);
        try json.endObject();
    }
    try json.endArray();
    try json.endObject();
}

fn writeDeltaJson(json: *std.json.Stringify, delta: *const review.ReviewDelta) !void {
    try json.beginObject();
    try json.objectField("isEmpty");
    try json.write(delta.isEmpty());
    try json.objectField("addedRoutes");
    try writeRoutes(json, delta.added_routes);
    try json.objectField("removedRoutes");
    try writeRoutes(json, delta.removed_routes);
    try json.objectField("addedEnv");
    try writeStrings(json, delta.added_env);
    try json.objectField("removedEnv");
    try writeStrings(json, delta.removed_env);
    try json.objectField("addedEgress");
    try writeStrings(json, delta.added_egress);
    try json.objectField("removedEgress");
    try writeStrings(json, delta.removed_egress);
    try json.objectField("addedCache");
    try writeStrings(json, delta.added_cache);
    try json.objectField("removedCache");
    try writeStrings(json, delta.removed_cache);
    try json.objectField("addedCapabilities");
    try writeStrings(json, delta.added_capabilities);
    try json.objectField("removedCapabilities");
    try writeStrings(json, delta.removed_capabilities);
    try json.objectField("promotedProperties");
    try writePropertyChanges(json, delta.promoted_properties);
    try json.objectField("demotedProperties");
    try writePropertyChanges(json, delta.demoted_properties);
    if (delta.proof_level_change) |change| {
        try json.objectField("proofLevelChange");
        try json.beginObject();
        try json.objectField("old");
        try json.write(change.old.toString());
        try json.objectField("new");
        try json.write(change.new.toString());
        try json.endObject();
    }
    try json.endObject();
}

fn writeStrings(json: *std.json.Stringify, items: []const []const u8) !void {
    try json.beginArray();
    for (items) |item| try json.write(item);
    try json.endArray();
}

fn writeRoutes(json: *std.json.Stringify, routes: []const review.Route) !void {
    try json.beginArray();
    for (routes) |route| {
        try json.beginObject();
        try json.objectField("pattern");
        try json.write(route.pattern);
        try json.objectField("isPrefix");
        try json.write(route.is_prefix);
        try json.endObject();
    }
    try json.endArray();
}

fn writePropertyChanges(json: *std.json.Stringify, props: []const review.PropertyChange) !void {
    try json.beginArray();
    for (props) |prop| {
        try json.beginObject();
        try json.objectField("name");
        try json.write(prop.name);
        try json.objectField("label");
        try json.write(prop.label);
        try json.endObject();
    }
    try json.endArray();
}

pub const index_html =
    \\<!doctype html>
    \\<html lang="en">
    \\<head>
    \\<meta charset="utf-8">
    \\<meta name="viewport" content="width=device-width, initial-scale=1">
    \\<title>zigttp studio</title>
    \\<style>
    \\:root{color-scheme:dark;--bg:#090b0f;--panel:#11151d;--line:#273140;--text:#eef3f8;--muted:#8d9bab;--ok:#49d17d;--warn:#f1b84b;--bad:#ff6b6b;--accent:#64b5ff}
    \\*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--text);font:14px/1.4 ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif}
    \\main{min-height:100svh;display:grid;grid-template-rows:auto 1fr auto}
    \\header{padding:22px 28px;border-bottom:1px solid var(--line);display:flex;justify-content:space-between;gap:20px;align-items:end}
    \\h1{font-size:34px;line-height:1;margin:0;letter-spacing:0}.sub{color:var(--muted);margin-top:8px}.status{font:12px ui-monospace,SFMono-Regular,Menlo,monospace;color:var(--muted);text-align:right}
    \\.grid{display:grid;grid-template-columns:1.1fr 1.2fr 1fr;gap:1px;background:var(--line);min-height:0}.pane{background:var(--panel);padding:20px;min-width:0;overflow:auto}
    \\h2{font-size:12px;text-transform:uppercase;color:var(--muted);letter-spacing:.08em;margin:0 0 14px}.big{font-size:42px;font-weight:650;margin:2px 0 10px}.pill{display:inline-flex;border:1px solid var(--line);border-radius:999px;padding:5px 9px;margin:0 6px 6px 0;font:12px ui-monospace,SFMono-Regular,Menlo,monospace;color:var(--muted)}
    \\.pill.on{color:var(--ok);border-color:#2e7d4c;background:#102318}.pill.off{color:var(--bad);border-color:#73333a;background:#251217}.pill.add{color:var(--accent);border-color:#315a7e}.pill.remove{color:var(--bad);border-color:#73333a}
    \\li.witness{cursor:pointer}li.witness:hover{background:rgba(100,181,255,.06)}li.witness>.detail{display:none;margin-top:8px;padding:10px 12px;background:#07090d;border:1px solid var(--line);border-radius:6px;font:12px ui-monospace,SFMono-Regular,Menlo,monospace}li.witness.open>.detail{display:block}li.witness .detail .row{display:grid;grid-template-columns:90px 1fr;gap:6px 12px;margin-bottom:6px}li.witness .detail .row dt{color:var(--muted)}li.witness .detail pre{margin:0;max-height:none;color:#cbd7e3;white-space:pre-wrap;overflow-wrap:anywhere}
    \\.spec{display:inline-block;vertical-align:top;margin:0 6px 6px 0}.spec>.pill{cursor:default}.spec.has-diag>.pill{cursor:pointer}.spec .diag{display:none;margin-top:4px;padding:6px 9px;background:#07090d;border:1px solid var(--line);border-radius:6px;font:12px ui-monospace,SFMono-Regular,Menlo,monospace;color:#cbd7e3;max-width:360px}.spec.open .diag{display:block}.spec .diag .code{color:var(--bad)}.spec .diag .loc{color:var(--muted)}.spec .diag .msg{margin-top:4px;color:#dceeff}
    \\#timeline{display:flex;flex-wrap:wrap;gap:4px;margin:0 0 14px;font:11px ui-monospace,SFMono-Regular,Menlo,monospace}#timeline .tick{padding:2px 6px;border:1px solid var(--line);border-radius:4px;color:var(--muted);background:#07090d}#timeline .tick.safe{border-color:#2e7d4c;color:var(--ok)}#timeline .tick.safe_with_additions{border-color:#315a7e;color:var(--accent)}#timeline .tick.breaking{border-color:#73333a;color:var(--bad)}#timeline .tick.first{box-shadow:inset 0 0 0 1px currentColor}
    \\dl{display:grid;grid-template-columns:90px 1fr;gap:8px 14px;margin:0}dt{color:var(--muted)}dd{margin:0;min-width:0;overflow-wrap:anywhere}code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;color:#cfe6ff}
    \\ul{list-style:none;margin:0;padding:0}li{padding:7px 0;border-bottom:1px solid rgba(255,255,255,.06);overflow-wrap:anywhere}.empty{color:var(--muted)}
    \\footer{padding:14px 20px;border-top:1px solid var(--line);display:flex;gap:10px;background:#0d1016}input,select,textarea,button{font:inherit}select,input,textarea{background:#07090d;color:var(--text);border:1px solid var(--line);border-radius:6px;padding:9px}input{flex:1}button{border:1px solid #38658f;background:#10243a;color:#dceeff;border-radius:6px;padding:9px 13px;cursor:pointer}pre{white-space:pre-wrap;margin:12px 0 0;color:#cbd7e3;max-height:180px;overflow:auto}
    \\#diagnosticsList li{padding:8px 0;border-bottom:1px solid rgba(255,255,255,.06)}#diagnosticsList .code{color:var(--bad);font-family:ui-monospace,SFMono-Regular,Menlo,monospace}#diagnosticsList .loc{color:var(--muted);font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:12px;margin-left:8px}#diagnosticsList .msg{margin-top:4px;color:#dceeff}#diagnosticsList .hint{margin-top:4px;color:var(--muted);font-style:italic}
    \\#counterexampleBlock{margin-bottom:22px;padding:12px;border:1px solid #73333a;background:#251217;border-radius:6px}#counterexampleBlock[hidden]{display:none}#counterexampleBody{font:12px ui-monospace,SFMono-Regular,Menlo,monospace;color:#dceeff}#counterexampleBody .row{display:grid;grid-template-columns:92px 1fr;gap:6px 12px;margin-bottom:6px}#counterexampleBody dt{color:var(--muted)}#counterexampleBody dd{margin:0}#counterexampleBody .bad{color:var(--bad)}#counterexampleBody .hint{color:#ffd0d0}
    \\#demoPanel{display:grid;grid-template-columns:1fr auto;gap:14px;align-items:center;padding:14px 28px;border-bottom:1px solid var(--line);background:#0c1118}#demoPanel[hidden]{display:none}#demoPanel h2{margin:0 0 6px}#demoPanel p{margin:0;color:#cbd7e3}#demoActions{display:flex;gap:8px;flex-wrap:wrap;justify-content:flex-end}#demoWitness{grid-column:1/-1;margin-top:2px;padding:10px 12px;border:1px solid var(--line);border-radius:6px;background:#07090d;font:12px ui-monospace,SFMono-Regular,Menlo,monospace;color:#cbd7e3}
    \\#terminalMirror{padding:18px 28px 22px;border-bottom:1px solid var(--line);background:#070a0e}#terminalMirror[hidden]{display:none}#terminalMirror header{padding:0 0 10px;border:0;display:flex;justify-content:space-between;align-items:baseline;gap:14px}#terminalMirror h2{font-size:12px;color:var(--muted);letter-spacing:.08em;text-transform:uppercase;margin:0}#terminalMirror .echo{font:11px ui-monospace,SFMono-Regular,Menlo,monospace;color:var(--muted)}#frameMirror{margin:0;padding:14px 18px;background:#040608;border:1px solid var(--line);border-radius:6px;color:#cfe6ff;font:13px/1.45 ui-monospace,SFMono-Regular,Menlo,monospace;white-space:pre;overflow-x:auto;tab-size:4}
    \\@media(max-width:900px){.grid{grid-template-columns:1fr}.status{text-align:left}header{align-items:start;flex-direction:column}.big{font-size:32px}}
    \\#onboarding{border:1px solid var(--line);background:var(--panel);color:var(--text);padding:24px 28px;max-width:560px;border-radius:10px;box-shadow:0 20px 60px rgba(0,0,0,.55)}#onboarding::backdrop{background:rgba(9,11,15,.78)}#onboarding h2{font-size:12px;margin:0 0 14px;text-transform:uppercase;letter-spacing:.08em;color:var(--muted)}#onboarding ol{margin:0 0 18px;padding-left:22px;line-height:1.55}#onboarding ol strong{color:var(--text)}#onboarding ol code,#onboarding p code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;color:var(--accent);background:#07090d;border:1px solid var(--line);border-radius:4px;padding:1px 5px}#onboarding .hint{color:var(--muted);font-size:13px;margin:0 0 16px}#onboarding .actions{display:flex;justify-content:flex-end;gap:8px}
    \\#lensBar{display:inline-flex;gap:4px;margin:0 0 14px;padding:4px;border:1px solid var(--line);border-radius:8px;background:#07090d}#lensBar button{appearance:none;border:0;background:transparent;color:var(--muted);font:12px ui-monospace,SFMono-Regular,Menlo,monospace;letter-spacing:.04em;padding:5px 10px;border-radius:5px;cursor:pointer}#lensBar button:hover{color:var(--text)}#lensBar button.active{background:#10243a;color:#dceeff}
    \\.lensPane{display:none}.lensPane.active{display:block}
    \\#tradeRows{margin:0;padding:0;list-style:none}#tradeRows li{padding:10px 0;border-bottom:1px solid rgba(255,255,255,.06)}#tradeRows .head{display:flex;align-items:baseline;gap:8px;margin-bottom:6px}#tradeRows .head .pill{margin:0}#tradeRows .gave,#tradeRows .earned{font:12px ui-monospace,SFMono-Regular,Menlo,monospace;color:#cbd7e3;display:block;margin-left:4px}#tradeRows .gave .lbl,#tradeRows .earned .lbl{color:var(--muted);display:inline-block;width:78px}#tradeRows li.off{opacity:.5}
    \\#handoverPre{margin:0;padding:14px 18px;background:#040608;border:1px solid var(--line);border-radius:6px;color:#cfe6ff;font:13px/1.45 ui-monospace,SFMono-Regular,Menlo,monospace;white-space:pre;overflow-x:auto;tab-size:4}#handoverActions{display:flex;gap:8px;margin-top:10px}#handoverActions button{border:1px solid #38658f;background:#10243a;color:#dceeff;border-radius:6px;padding:7px 12px;cursor:pointer}#handoverStatus{color:var(--muted);font:12px ui-monospace,SFMono-Regular,Menlo,monospace;margin-left:6px;align-self:center}
    \\</style>
    \\</head>
    \\<body><main>
    \\<dialog id="onboarding"><h2>welcome to studio</h2><ol><li><strong>Proven Surface</strong> lists the routes, env vars, and egress hosts the compiler extracted from your handler.</li><li><strong>Property pills</strong> are the proofs the compiler discharged. Watch them flip when you save.</li><li><strong>Witnesses</strong> capture the request that would break a proof. Click one to see the failing path.</li><li>Press <code>.</code> to reopen this overlay any time.</li></ol><p class="hint">Edit your handler. Watch the proof flip live.</p><div class="actions"><button id="onboardingDismiss">Got it</button></div></dialog>
    \\<header><div><h1>zigttp studio</h1><div class="sub">Your terminal HUD, hyperlinked. A proof substrate for humans and AI agents.</div></div><div class="status" id="status">connecting</div></header>
    \\<section id="terminalMirror" hidden><header><h2>Live HUD - terminal mirror</h2><span class="echo">same frame as <code>zigttp dev</code></span></header><pre id="frameMirror" aria-label="Proof card frame mirrored from the terminal HUD"></pre></section>
    \\<section id="demoPanel" hidden><div><h2>Proof Theater</h2><p id="demoTitle"></p><p class="empty" id="demoWorkspace"></p></div><div id="demoActions"></div><div id="demoWitness" hidden></div></section>
    \\<section class="grid"><div class="pane"><div id="lensBar" role="tablist" aria-label="Proof lens"><button data-lens="properties" class="active">Properties</button><button data-lens="trade">Trade</button><button data-lens="handover">Handover</button></div><div class="lensPane active" data-lens="properties"><h2>Verdict</h2><div class="big" id="verdict">...</div><div id="timeline"></div><div id="diagnosticsBlock" hidden><h2 style="color:var(--bad)">Diagnostics</h2><ul id="diagnosticsList"></ul></div><dl id="summary"></dl><h2 style="margin-top:24px">Properties</h2><div id="properties"></div><h2 style="margin-top:24px" id="specsHeading" hidden>Specs (declared)</h2><div id="specs"></div></div><div class="lensPane" data-lens="trade"><h2>Trade</h2><p class="empty" style="margin:0 0 12px">Each proven property paired with the substrate restrictions that bought it.</p><ul id="tradeRows"></ul></div><div class="lensPane" data-lens="handover"><h2>AI Handover</h2><p class="empty" style="margin:0 0 12px">A proof certificate you can paste into Cursor, Claude, or any coding agent.</p><pre id="handoverPre"></pre><div id="handoverActions"><button id="handoverCopy">Copy to clipboard</button><span id="handoverStatus"></span></div></div></div><div class="pane"><h2>Proven Surface</h2><div id="surface"></div><h2 style="margin-top:24px">Next Actions</h2><ul id="actions"></ul></div><div class="pane"><section id="counterexampleBlock" hidden><h2 style="color:var(--bad)">Counterexample</h2><div id="counterexampleBody"></div></section><h2>Proof Delta</h2><div id="delta"></div><h2 style="margin-top:24px" id="witnessesHeading" hidden>Witnesses</h2><div id="witnessesCounts"></div><ul id="witnessesList"></ul><h2 style="margin-top:24px">Generated Tests</h2><p><a id="testsLink" href="/_zigttp/studio/tests.jsonl" download="handler.tests.jsonl">Download tests.jsonl</a> <span class="empty">regenerated on every recompile</span></p></div></section>
    \\<footer><select id="method"><option>GET</option><option>POST</option><option>PUT</option><option>DELETE</option></select><input id="url" value="/" aria-label="URL"><button id="send">Send</button></footer>
    \\<pre id="response"></pre>
    \\</main><script>
    \\const $=id=>document.getElementById(id);
    \\function esc(s){return String(s).replace(/[&<>"']/g,c=>({"&":"&amp;","<":"&lt;",">":"&gt;","\"":"&quot;","'":"&#39;"}[c]))}
    \\function list(title,items){return `<h2>${esc(title)} (${items.length})</h2>`+(items.length?`<ul>${items.map(x=>`<li><code>${esc(x.pattern||x)}</code>${x.isPrefix?" prefix":""}</li>`).join("")}</ul>`:`<p class=empty>none</p>`)}
    \\function pills(obj){return Object.entries(obj||{}).map(([k,v])=>`<span class="pill ${v?"on":"off"}">${v?"+":"-"}${esc(k)}</span>`).join("")}
    \\function specDiag(s){if(!s.diagnosticCode&&!s.diagnosticMessage)return"";const loc=s.sourceLine?` <span class="loc">line ${s.sourceLine}${s.sourceColumn?":"+s.sourceColumn:""}${s.sourceSnippet?" — <code>"+esc(s.sourceSnippet)+"</code>":""}</span>`:"";const code=s.diagnosticCode?`<span class="code">${esc(s.diagnosticCode)}</span>`:"";const msg=s.diagnosticMessage?`<div class="msg">${esc(s.diagnosticMessage)}</div>`:"";return `<div class="diag">${code}${loc}${msg}</div>`}
    \\function specPills(items){return (items||[]).map(s=>{const diag=specDiag(s);const cls=diag?"spec has-diag":"spec";return `<span class="${cls}" data-spec="${esc(s.name)}"><span class="pill ${s.discharged?"on":"off"}">${s.discharged?"✓":"✗"} spec ${esc(s.name)}</span>${diag}</span>`}).join("")}
    \\function changes(label,items,cls){return items&&items.length?items.map(x=>`<span class="pill ${cls}">${label} ${esc(x.label||x.pattern||x)}</span>`).join(""):""}
    \\function witnessCounts(byProp){return Object.entries(byProp||{}).map(([k,v])=>`<span class="pill on">${esc(k)}: ${v}</span>`).join("")}
    \\function witnessRows(entries){return (entries||[]).map(e=>`<li class="witness" data-key="${esc(e.key)}"><code>${esc(e.key.slice(0,12))}</code>${e.pinned?' <span class="pill add">pinned</span>':""} <span class="pill off">${esc(e.property)}</span> ${esc(e.summary)}<div class="detail"></div></li>`).join("")}
    \\function fmtUnix(s){if(!s)return"";const d=new Date(s*1000);return d.toISOString().replace("T"," ").replace(/\.\d+Z$/,"Z")}
    \\function renderWitness(d){const meta=(d.events||[]).find(e=>e.type==="witness")||{};const req=(d.events||[]).find(e=>e.type==="request")||{};const ios=(d.events||[]).filter(e=>e.type==="io");const headers=req.headers&&Object.keys(req.headers).length?JSON.stringify(req.headers):"none";const body=req.body==null?"none":(typeof req.body==="string"?req.body:JSON.stringify(req.body));const origin=meta.origin?`line ${meta.origin.line}, col ${meta.origin.column}`:"";const sink=meta.sink?`line ${meta.sink.line}, col ${meta.sink.column}`:"";const stubs=ios.length?`<pre>${esc(ios.map(e=>`#${e.seq} ${e.module}.${e.fn} -> ${typeof e.result==="string"?e.result:JSON.stringify(e.result)}`).join("\n"))}</pre>`:'<span class="empty">none</span>';return `<div class="row"><dt>property</dt><dd>${esc(meta.property||"")}</dd><dt>origin</dt><dd>${esc(origin)}</dd><dt>sink</dt><dd>${esc(sink)}</dd><dt>summary</dt><dd>${esc(meta.summary||"")}</dd><dt>request</dt><dd><code>${esc(req.method||"")} ${esc(req.url||"")}</code></dd><dt>headers</dt><dd>${esc(headers)}</dd><dt>body</dt><dd>${esc(body)}</dd><dt>io stubs</dt><dd>${stubs}</dd><dt>pinned</dt><dd>${d.pinned?"yes":"no"}</dd></div>`}
    \\async function toggleWitness(li){const detail=li.querySelector(".detail");if(li.classList.contains("open")){li.classList.remove("open");return}li.classList.add("open");if(detail.dataset.loaded==="1")return;detail.innerHTML='<span class="empty">loading...</span>';try{const r=await fetch(`/_zigttp/studio/witness/${encodeURIComponent(li.dataset.key)}.json`,{cache:"no-store"});if(!r.ok){detail.innerHTML=`<span class="empty">HTTP ${r.status}</span>`;return}const d=await r.json();detail.innerHTML=renderWitness(d);detail.dataset.loaded="1"}catch(e){detail.innerHTML=`<span class="empty">${esc(String(e))}</span>`}}
    \\document.addEventListener("click",ev=>{const li=ev.target.closest("li.witness");if(li){toggleWitness(li);return}const sp=ev.target.closest(".spec.has-diag");if(sp)sp.classList.toggle("open")})
    \\function readiness(r){if(!r)return"";return `<h2>Release Checklist</h2><ul><li>${r.declaredSpecsPass?"✓":"✗"} declared specs pass</li><li>${r.deployReady?"✓":"✗"} deploy verdict: <code>${esc(r.deployVerdict)}</code></li></ul>`}
    \\function actions(items){return (items||[]).map(a=>`<li><span class="pill ${a.severity==="success"?"on":a.severity==="error"?"off":"add"}">${esc(a.kind)}</span><strong>${esc(a.title)}</strong><p>${esc(a.detail)}</p><code>${esc(a.command)}</code></li>`).join("")||"<p class=empty>none</p>"}
    \\let lastWitnessFingerprint="";let lastSpecsFingerprint="";
    \\function witnessFingerprint(entries){return (entries||[]).map(e=>`${e.key}:${e.pinned?1:0}:${e.property}`).join("|")}
    \\function specsFingerprint(items){return (items||[]).map(s=>`${s.name}:${s.discharged?1:0}:${s.diagnosticCode||""}:${s.diagnosticMessage||""}:${s.sourceLine||""}:${s.sourceColumn||""}:${s.sourceSnippet||""}`).join("|")}
    \\function fmtClock(ms){const d=new Date(ms);return `${String(d.getHours()).padStart(2,"0")}:${String(d.getMinutes()).padStart(2,"0")}:${String(d.getSeconds()).padStart(2,"0")}`}
    \\function timeline(entries){if(!entries||!entries.length)return"";return entries.map((e,i)=>`<span class="tick ${esc(e.verdict)}${i===0?" first":""}" title="${esc(e.verdict)} · sha ${esc(e.sha8||"")} · ${e.recompileMs??0}ms · ${fmtClock(e.timestampMs)}">${fmtClock(e.timestampMs)} ${esc((e.sha8||"").slice(0,4))}${e.recompileMs!=null?" · "+e.recompileMs+"ms":""}</span>`).join("")}
    \\function diagnostics(items){return (items||[]).map(d=>`<li><code class="code">${esc(d.code)}</code><span class="loc">${esc(d.file)}:${d.line}:${d.column}</span><div class="msg">${esc(d.message)}</div>${d.suggestion?`<div class="hint">hint: ${esc(d.suggestion)}</div>`:""}</li>`).join("")}
    \\function renderDiagnostics(s){const diags=s.diagnostics||[];$("diagnosticsBlock").hidden=diags.length===0;$("diagnosticsList").innerHTML=diagnostics(diags)}
    \\function replayRow(label,r){if(!r)return"";const body=r.error?`crashed - ${r.error}`:`${r.status} ${r.body||""}`;return `<dt>${esc(label)}</dt><dd>${esc(body)}</dd>`}
    \\function renderCounterexample(cx){const block=$("counterexampleBlock");const body=$("counterexampleBody");if(!cx){block.hidden=true;body.innerHTML="";return}block.hidden=false;const req=cx.failingRequest?`${cx.failingRequest.method} ${cx.failingRequest.url}${cx.failingRequest.body?" body="+cx.failingRequest.body:""}`:"structural proof cause";body.innerHTML=`<dl class="row"><dt>property</dt><dd><span class="bad">-${esc(cx.label||cx.field||"property")}</span></dd><dt>source</dt><dd><code>${esc(cx.handlerPath||"")}:${cx.line||0}:${cx.column||0}</code></dd><dt>snippet</dt><dd><code>${esc(cx.snippet||"")}</code></dd><dt>why</dt><dd class="hint">${esc(cx.suggestion||"inspect the demoted property and repair the source")}</dd><dt>request</dt><dd>${esc(req)}</dd>${replayRow("previous",cx.previousResponse)}${replayRow("current",cx.currentResponse)}<dt>next</dt><dd>[r] replay live · [s] pin regression · [a] ask expert</dd></dl>`}
    \\function demoButton(a){const label={introduce_bug:"Introduce unsafe edit",repair_bug:"Repair",deploy:"Deploy local",reset:"Reset"}[a]||a;return `<button data-demo-action="${esc(a)}">${esc(label)}</button>`}
    \\function renderDemo(d){$("demoPanel").hidden=false;$("demoTitle").textContent=d.title||d.step||"";$("demoWorkspace").textContent=d.workspace?`workspace: ${d.workspace}`:"";$("demoActions").innerHTML=(d.availableActions||[]).map(demoButton).join("");const w=d.witness;if(w){$("demoWitness").hidden=false;$("demoWitness").innerHTML=`property: ${esc(w.property)}<br>request: ${esc(w.request)}<br>span: ${esc(w.span)}<br>path: ${esc(w.failingPath)}`}else if(d.receipt){$("demoWitness").hidden=false;$("demoWitness").innerHTML=`receipt: ${esc(d.receipt.ledger)} · service ${esc(d.receipt.service)}`}else{$("demoWitness").hidden=true;$("demoWitness").innerHTML=""}}
    \\async function pullDemo(){try{const r=await fetch("/_zigttp/studio/demo/state.json",{cache:"no-store"});if(r.status===404){$("demoPanel").hidden=true;return}if(r.ok)renderDemo(await r.json())}catch(e){}}
    \\async function runDemoAction(action){const r=await fetch("/_zigttp/studio/demo/action",{method:"POST",headers:{"content-type":"application/json"},body:JSON.stringify({action})});if(!r.ok){$("demoWitness").hidden=false;$("demoWitness").textContent=`action failed: HTTP ${r.status} ${await r.text()}`;return}await pull();await pullDemo()}
    \\document.addEventListener("click",ev=>{const b=ev.target.closest("[data-demo-action]");if(b)runDemoAction(b.dataset.demoAction)})
    \\function renderFrameMirror(s){const sec=$("terminalMirror");const pre=$("frameMirror");if(!sec||!pre)return;const txt=s&&typeof s.frame==="string"?s.frame:"";if(!txt){sec.hidden=true;pre.textContent="";return}sec.hidden=false;pre.textContent=txt}
    \\function render(s){$("status").textContent=`${s.status} · ${s.handlerPath||""}`;renderDiagnostics(s);renderFrameMirror(s);if(s.status!=="ready"){renderCounterexample(null);$("verdict").textContent=s.status;$("summary").innerHTML=`<dt>message</dt><dd>${esc(s.message||"")}</dd>`;return}renderCounterexample(s.counterexample);const f=s.facts;$("verdict").textContent=s.verdict;$("timeline").innerHTML=timeline(s.recent);$("summary").innerHTML=`<dt>proof</dt><dd>${esc(f.proofLevel)}</dd><dt>contract</dt><dd><code>${esc(f.contractSha).slice(0,16)}</code></dd><dt>recompile</dt><dd>${s.recompileMs??0}ms</dd>`+readiness(s.releaseReadiness);$("properties").innerHTML=pills(f.properties);const ds=f.declaredSpecs||[];$("specsHeading").hidden=ds.length===0;{const fp=specsFingerprint(ds);if(fp!==lastSpecsFingerprint){$("specs").innerHTML=ds.length?specPills(ds):"";lastSpecsFingerprint=fp}}$("surface").innerHTML=list("routes",f.routes)+list("env",f.envKeys)+list("egress",f.egressHosts)+list("cache",f.cacheNamespaces)+list("capabilities",f.capabilities);const d=s.delta;$("delta").innerHTML=(changes("+ route",d.addedRoutes,"add")+changes("- route",d.removedRoutes,"remove")+changes("+ prop",d.promotedProperties,"add")+changes("- prop",d.demotedProperties,"remove")+changes("+ env",d.addedEnv,"add")+changes("+ egress",d.addedEgress,"add")+changes("+ cap",d.addedCapabilities,"add"))||"<p class=empty>no changes against baseline</p>";const w=s.witnesses||{total:0,byProperty:{},entries:[]};$("witnessesHeading").hidden=w.total===0;$("witnessesCounts").innerHTML=w.total?witnessCounts(w.byProperty):"";const fp=witnessFingerprint(w.entries);if(fp!==lastWitnessFingerprint){$("witnessesList").innerHTML=w.total?witnessRows(w.entries):"";lastWitnessFingerprint=fp}{const a=$("testsLink");if(a)a.setAttribute("download",((s.handlerPath||"handler").split("/").pop())+".tests.jsonl")}$("actions").innerHTML=actions(s.nextActions)}
    \\// Mirror of `proof_to_restrictions` in packages/runtime/src/deploy/review.zig. Keep in sync.
    \\const TRADE_TABLE=[{prop:"deterministic",label:"deterministic",gave:["async/await","while","do...while","for(;;)"],earned:"deterministic, replayable, AI-refactorable"},{prop:"retrySafe",label:"retry-safe",gave:["try/catch","throw"],earned:"Result-narrowed, exhaustive paths, no hidden control flow"},{prop:"stateIsolated",label:"state-isolated",gave:["class","this","++","--"],earned:"explicit data flow, no shared mutable receivers"},{prop:"readOnly",label:"read-only",gave:["delete","++","--"],earned:"shape-stable property access, no hidden writes"},{prop:"inputValidated",label:"input-validated",gave:["regex"],earned:"schema-checkable validation, no opaque accept sets"},{prop:"injectionSafe",label:"injection-safe",gave:[],earned:"flow analysis tracks user-input into sinks"},{prop:"idempotent",label:"idempotent",gave:[],earned:"earned by analysis; retries are safe"},{prop:"noSecretLeakage",label:"no-secret-leakage",gave:[],earned:"flow analysis tracks secret labels to sinks"},{prop:"noCredentialLeakage",label:"no-credential-leakage",gave:[],earned:"flow analysis tracks credential labels to sinks"},{prop:"piiContained",label:"pii-contained",gave:[],earned:"PII never reaches egress without an explicit boundary"},{prop:"resultsSafe",label:"results-safe",gave:[],earned:"all paths return a Response or Result.err"},{prop:"faultCovered",label:"fault-covered",gave:[],earned:"every failure path has a witness or test"}];
    \\function tradeRowEl(row,on){const li=document.createElement("li");if(!on)li.className="off";const head=document.createElement("div");head.className="head";const pill=document.createElement("span");pill.className="pill "+(on?"on":"off");pill.textContent=(on?"+ ":"- ")+row.label;head.appendChild(pill);li.appendChild(head);if(row.gave.length){const g=document.createElement("span");g.className="gave";const gl=document.createElement("span");gl.className="lbl";gl.textContent="gave up:";g.appendChild(gl);g.appendChild(document.createTextNode(row.gave.join(", ")));li.appendChild(g)}const e=document.createElement("span");e.className="earned";const el=document.createElement("span");el.className="lbl";el.textContent="earned:";e.appendChild(el);e.appendChild(document.createTextNode(row.earned));li.appendChild(e);return li}
    \\function renderTrade(facts){const props=(facts&&facts.properties)||{};const ul=$("tradeRows");while(ul.firstChild)ul.removeChild(ul.firstChild);TRADE_TABLE.forEach(r=>ul.appendChild(tradeRowEl(r,!!props[r.prop])))}
    \\function renderHandover(s){const cert=(s&&s.proofCertificate)||"";$("handoverPre").textContent=cert||"(no certificate yet - waiting for first proof)"}
    \\let activeLens="properties";
    \\function applyLens(name){activeLens=name;document.querySelectorAll("#lensBar button").forEach(b=>b.classList.toggle("active",b.dataset.lens===name));document.querySelectorAll(".lensPane").forEach(p=>p.classList.toggle("active",p.dataset.lens===name));try{localStorage.setItem("zigttp.studio.lens",name)}catch(e){}}
    \\document.addEventListener("click",ev=>{const b=ev.target.closest("#lensBar button");if(b)applyLens(b.dataset.lens)});
    \\(function(){let saved="properties";try{saved=localStorage.getItem("zigttp.studio.lens")||"properties"}catch(e){}if(saved==="trade"||saved==="handover")applyLens(saved)})();
    \\async function copyHandover(){const cert=$("handoverPre").textContent||"";const status=$("handoverStatus");try{await navigator.clipboard.writeText(cert);status.textContent="copied"}catch(e){status.textContent="copy failed - select and copy manually"}setTimeout(()=>{status.textContent=""},2400)}
    \\{const b=$("handoverCopy");if(b)b.onclick=copyHandover}
    \\async function pull(){try{const r=await fetch("/_zigttp/studio/state.json",{cache:"no-store"});const s=await r.json();render(s);if(s&&s.facts)renderTrade(s.facts);renderHandover(s)}catch(e){$("status").textContent=String(e)}}
    \\let pollTimer=null;function startPolling(){if(!pollTimer)pollTimer=setInterval(pull,750)}function stopPolling(){if(pollTimer){clearInterval(pollTimer);pollTimer=null}}
    \\function startEvents(){let es;try{es=new EventSource("/_zigttp/studio/events")}catch(e){startPolling();return}es.onmessage=ev=>{stopPolling();try{const s=JSON.parse(ev.data);render(s);if(s&&s.facts)renderTrade(s.facts);renderHandover(s)}catch(e){}};es.onerror=()=>{es.close();startPolling()}}
    \\startEvents();pull();pullDemo();setInterval(pullDemo,1200);
    \\$("send").onclick=async()=>{const r=await fetch($("url").value,{method:$("method").value});$("response").textContent=`HTTP ${r.status}\n`+await r.text()}
    \\function openOnboarding(){const d=$("onboarding");if(d&&!d.open&&typeof d.showModal==="function")d.showModal()}
    \\function closeOnboarding(){const d=$("onboarding");if(d&&d.open)d.close();try{localStorage.setItem("zigttp.onboarding.seen","1")}catch(e){}}
    \\{const b=$("onboardingDismiss");if(b)b.onclick=closeOnboarding}
    \\document.addEventListener("keydown",ev=>{if(ev.key!==".")return;const t=ev.target;if(t&&/^(INPUT|TEXTAREA|SELECT)$/.test(t.tagName))return;ev.preventDefault();const d=$("onboarding");if(d&&d.open)closeOnboarding();else openOnboarding()})
    \\{let seen="0";try{seen=localStorage.getItem("zigttp.onboarding.seen")||"0"}catch(e){}if(seen!=="1")openOnboarding()}
    \\</script></body></html>
;

test "studio path matcher is scoped to studio endpoints" {
    try std.testing.expect(isStudioPath("/_zigttp/studio"));
    try std.testing.expect(isStudioPath("/_zigttp/studio/state.json"));
    try std.testing.expect(isStudioPath(sse_path));
    try std.testing.expect(!isStudioPath("/_zigttp/durable/contract"));
}

test "broadcast on a State with no subscribers does not allocate or leak" {
    var s = try State.init(std.testing.allocator, "h.ts");
    defer s.deinit();
    s.broadcast();
    s.broadcast();
}

test "studio HTML client wires EventSource with polling fallback" {
    try std.testing.expect(std.mem.indexOf(u8, index_html, "new EventSource(\"/_zigttp/studio/events\")") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "startPolling") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "setInterval(pull,750)") != null);
}

test "studio HTML wires the diagnostics card" {
    try std.testing.expect(std.mem.indexOf(u8, index_html, "id=\"diagnosticsBlock\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "id=\"diagnosticsList\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "function renderDiagnostics") != null);
}

test "studio HTML wires the on-boarding overlay" {
    try std.testing.expect(std.mem.indexOf(u8, index_html, "id=\"onboarding\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "welcome to studio") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "zigttp.onboarding.seen") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "openOnboarding") != null);
}

test "updateDiagnostics produces JSON with a diagnostics array" {
    var s = try State.init(std.testing.allocator, "h.ts");
    defer s.deinit();

    const d_code = try std.testing.allocator.dupe(u8, "ZTS001");
    const d_sev = try std.testing.allocator.dupe(u8, "error");
    const d_file = try std.testing.allocator.dupe(u8, "src/handler.ts");
    const d_msg = try std.testing.allocator.dupe(u8, "unsupported feature: var");
    const d_hint = try std.testing.allocator.dupe(u8, "use let or const");

    var diags = [_]Diagnostic{.{
        .code = d_code,
        .severity = d_sev,
        .file = d_file,
        .line = 5,
        .column = 3,
        .message = d_msg,
        .suggestion = d_hint,
    }};
    defer for (&diags) |*d| d.deinit(std.testing.allocator);

    s.updateDiagnostics("compilation failed", &diags);

    const snap = try s.stateJsonCopy(std.testing.allocator);
    defer std.testing.allocator.free(snap);

    try std.testing.expect(std.mem.indexOf(u8, snap, "\"status\":\"error\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, snap, "\"diagnostics\":[") != null);
    try std.testing.expect(std.mem.indexOf(u8, snap, "\"code\":\"ZTS001\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, snap, "\"line\":5") != null);
    try std.testing.expect(std.mem.indexOf(u8, snap, "\"suggestion\":\"use let or const\"") != null);
}

test "errorJson omits diagnostics field when slice is empty or null" {
    const allocator = std.testing.allocator;
    const plain = try errorJson(allocator, "h.ts", "boom", null);
    defer allocator.free(plain);
    try std.testing.expect(std.mem.indexOf(u8, plain, "\"diagnostics\"") == null);

    const empty_diags: []const Diagnostic = &.{};
    const also_plain = try errorJson(allocator, "h.ts", "boom", empty_diags);
    defer allocator.free(also_plain);
    try std.testing.expect(std.mem.indexOf(u8, also_plain, "\"diagnostics\"") == null);
}

test "witnessDetailKey accepts hex keys and rejects bad shapes" {
    try std.testing.expectEqualStrings(
        "abc123",
        witnessDetailKey("/_zigttp/studio/witness/abc123.json").?,
    );
    try std.testing.expectEqualStrings(
        "5adb97e49d3781ba300ffac0e47dcd1ea6efa9079992502041544b84944eb892",
        witnessDetailKey("/_zigttp/studio/witness/5adb97e49d3781ba300ffac0e47dcd1ea6efa9079992502041544b84944eb892.json").?,
    );
    try std.testing.expect(witnessDetailKey("/_zigttp/studio/witness/.json") == null);
    try std.testing.expect(witnessDetailKey("/_zigttp/studio/witness/abc.txt") == null);
    try std.testing.expect(witnessDetailKey("/_zigttp/studio/witness/../etc/passwd.json") == null);
    try std.testing.expect(witnessDetailKey("/_zigttp/studio/witness/has-dash.json") == null);
    try std.testing.expect(witnessDetailKey("/_zigttp/studio/witness/ABC123.json") != null);
    try std.testing.expect(isStudioPath("/_zigttp/studio/witness/abc123.json"));
}

test "factsJson includes release readiness and next actions" {
    const allocator = std.testing.allocator;
    const specs = [_]review.SpecState{.{
        .name = "idempotent",
        .discharged = false,
        .diagnostic_code = "ZTS500",
        .diagnostic_message = "remove Date.now()",
        .source_line = 11,
        .source_column = 25,
        .source_snippet = "Date.now()",
    }};
    const facts = review.ReviewFacts{
        .contract_sha = "abc123",
        .proof_level = .complete,
        .env_keys = &.{},
        .egress_hosts = &.{},
        .cache_namespaces = &.{},
        .routes = &.{},
        .capabilities = &.{},
        .properties = .{},
        .declared_specs = &specs,
    };
    const delta = review.ReviewDelta{};

    const recent = [_]RecentEntry{
        .{ .timestamp_ms = 1714579200000, .verdict = .safe, .recompile_ms = 4, .sha8 = "abc12300".*, .sha8_len = 6 },
    };
    const body = try factsJson(allocator, "handler.ts", .{
        .facts = &facts,
        .baseline = null,
        .delta = &delta,
        .recompile_ms = 4,
    }, recent[0..]);
    defer allocator.free(body);

    try std.testing.expect(std.mem.indexOf(u8, body, "\"releaseReadiness\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"declaredSpecsPass\":false") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"nextActions\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"kind\":\"repair_specs\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"kind\":\"share_badge\"") == null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"kind\":\"export_tests\"") == null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"diagnosticCode\":\"ZTS500\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"diagnosticMessage\":\"remove Date.now()\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"sourceLine\":11") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"sourceSnippet\":\"Date.now()\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"recent\":[") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"sha8\":\"abc123\"") != null);
}

test "factsJson carries counterexample preview for studio rendering" {
    const allocator = std.testing.allocator;
    const facts = review.ReviewFacts{
        .contract_sha = "abc123",
        .proof_level = .complete,
        .env_keys = &.{},
        .egress_hosts = &.{},
        .cache_namespaces = &.{},
        .routes = &.{},
        .capabilities = &.{},
        .properties = .{},
        .declared_specs = &.{},
    };
    const delta = review.ReviewDelta{};
    const cx = review.CounterexamplePreview{
        .field = "deterministic",
        .label = "deterministic",
        .line = 14,
        .column = 9,
        .snippet = "Date.now()",
        .handler_path = "src/handler.ts",
        .suggestion = "remove Date.now() or move it into a durable step",
    };
    const body = try factsJson(allocator, "src/handler.ts", .{
        .facts = &facts,
        .baseline = null,
        .delta = &delta,
        .recompile_ms = 4,
        .counterexample = cx,
    }, &.{});
    defer allocator.free(body);

    try std.testing.expect(std.mem.indexOf(u8, body, "\"counterexample\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"snippet\":\"Date.now()\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"suggestion\":\"remove Date.now()") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"kind\":\"share_badge\"") != null);
}

test "studio spec fingerprint tracks rendered diagnostic details" {
    try std.testing.expect(std.mem.indexOf(u8, index_html, "diagnosticMessage||") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "sourceColumn||") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "sourceSnippet||") != null);
}

test "studio HTML renders counterexample payload" {
    try std.testing.expect(std.mem.indexOf(u8, index_html, "counterexampleBlock") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "renderCounterexample") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "s.counterexample") != null);
}

test "studio HTML wires the terminal-mirror frame" {
    // The browser surface renders the same proof card the terminal HUD prints.
    // These markers anchor the unification: a hidden-by-default section, a
    // <pre> sink keyed off the JSON `frame` field, and a JS renderer that only
    // unhides the section when a frame string is present.
    try std.testing.expect(std.mem.indexOf(u8, index_html, "id=\"terminalMirror\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "id=\"frameMirror\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "function renderFrameMirror") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "renderFrameMirror(s)") != null);
    // The header copy makes the relationship explicit so the user reads the
    // two surfaces as one product, not two parallel UIs.
    try std.testing.expect(std.mem.indexOf(u8, index_html, "terminal mirror") != null);
}

test "studio HTML exposes the proof-lens tab bar and three lens panes" {
    // The tab bar is the user-facing affordance for the lens; its absence
    // would silently break the parity with the TUI Tab keystroke.
    try std.testing.expect(std.mem.indexOf(u8, index_html, "id=\"lensBar\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "data-lens=\"properties\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "data-lens=\"trade\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "data-lens=\"handover\"") != null);

    // Each lens has its own pane wrapper so the toggle can show/hide them.
    try std.testing.expect(std.mem.indexOf(u8, index_html, "class=\"lensPane active\" data-lens=\"properties\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "class=\"lensPane\" data-lens=\"trade\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "class=\"lensPane\" data-lens=\"handover\"") != null);

    // Trade rendering reads property states from the JSON facts.
    try std.testing.expect(std.mem.indexOf(u8, index_html, "function renderTrade") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "TRADE_TABLE") != null);

    // Handover rendering targets the certificate string from the JSON state.
    try std.testing.expect(std.mem.indexOf(u8, index_html, "function renderHandover") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "id=\"handoverPre\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "id=\"handoverCopy\"") != null);

    // Active-lens persistence so a returning developer keeps their view.
    try std.testing.expect(std.mem.indexOf(u8, index_html, "zigttp.studio.lens") != null);

    // The AI-substrate framing lives in the header subtitle as a quiet
    // undertone; the explicit pitch text lives in the Handover certificate.
    try std.testing.expect(std.mem.indexOf(u8, index_html, "proof substrate for humans and AI agents") != null);
}

test "factsJson emits proofCertificate field with substrate paragraph" {
    const allocator = std.testing.allocator;
    const facts = review.ReviewFacts{
        .contract_sha = "deadbeef00000000",
        .proof_level = .complete,
        .env_keys = &.{},
        .egress_hosts = &.{},
        .cache_namespaces = &.{},
        .routes = &.{},
        .capabilities = &.{},
        .properties = .{ .read_only = true, .deterministic = true },
        .declared_specs = &.{},
    };
    const delta = review.ReviewDelta{};
    const body = try factsJson(allocator, "src/handler.ts", .{
        .facts = &facts,
        .baseline = null,
        .delta = &delta,
    }, &.{});
    defer allocator.free(body);

    try std.testing.expect(std.mem.indexOf(u8, body, "\"proofCertificate\":") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "AI handover") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "Substrate") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "Refactor contract") != null);
}

test "factsJson embeds the rendered proof card frame" {
    const allocator = std.testing.allocator;
    const facts = review.ReviewFacts{
        .contract_sha = "deadbeef00000000",
        .proof_level = .complete,
        .env_keys = &.{},
        .egress_hosts = &.{},
        .cache_namespaces = &.{},
        .routes = &.{},
        .capabilities = &.{},
        .properties = .{ .read_only = true, .deterministic = true },
        .declared_specs = &.{},
    };
    const delta = review.ReviewDelta{};
    const body = try factsJson(allocator, "src/handler.ts", .{
        .facts = &facts,
        .baseline = null,
        .delta = &delta,
        .recompile_ms = 7,
    }, &.{});
    defer allocator.free(body);

    // The frame field is present and non-empty.
    try std.testing.expect(std.mem.indexOf(u8, body, "\"frame\":\"") != null);
    // The terminal frame's column headers serialize through (JSON escapes
    // newlines and pipes as plain characters).
    try std.testing.expect(std.mem.indexOf(u8, body, "Properties") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "Surface") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "Requests") != null);
    // Verdict line from writeStatusBar lands in the embedded frame.
    try std.testing.expect(std.mem.indexOf(u8, body, "Verdict: safe") != null);
}

test "pushRecent keeps newest first and caps at recent_capacity" {
    var s = State{
        .allocator = std.testing.allocator,
        .handler_path = try std.testing.allocator.dupe(u8, "h.ts"),
        .json = try std.testing.allocator.dupe(u8, "{}"),
    };
    defer s.deinit();

    const facts_a = review.ReviewFacts{
        .contract_sha = "aaaa1111",
        .proof_level = .complete,
        .env_keys = &.{},
        .egress_hosts = &.{},
        .cache_namespaces = &.{},
        .routes = &.{},
        .capabilities = &.{},
        .properties = .{},
        .declared_specs = &.{},
    };
    const delta = review.ReviewDelta{};

    var i: usize = 0;
    while (i < recent_capacity + 3) : (i += 1) {
        s.pushRecent(&facts_a, &delta, @as(u64, i));
    }
    try std.testing.expectEqual(@as(u8, recent_capacity), s.recent_count);
    // newest first: last push had recompile_ms = recent_capacity + 2
    try std.testing.expectEqual(@as(u64, recent_capacity + 2), s.recent[0].recompile_ms.?);
    try std.testing.expectEqualStrings("aaaa1111", s.recent[0].sha8[0..s.recent[0].sha8_len]);
}
