//! Counterexample witness replay (PI side).
//!
//! The PI agent host owns the witness pane, the autoloop, and the
//! verified-patch ledger - none of which can take a direct build
//! dependency on the runtime stack (PI is consumed by the runtime
//! binaries, so the dependency would invert). Instead, the binaries
//! that ship PI together with the runtime register a replay function
//! pointer at startup via `setReplayFn`. PI calls `replay()` against
//! that pointer; if it has not been registered the call returns
//! `error.WitnessReplayNotConfigured`, which callers surface as
//! "replay unavailable" without aborting.
//!
//! The wire protocol is trace-compatible JSONL, the same shape the
//! engine consumes via `--test`. Callers serialise a `WitnessBody`
//! through `writeWitnessJsonl` here and hand the bytes to `replay`.

const std = @import("std");
const ui_payload = @import("ui_payload.zig");
const json_writer = @import("providers/anthropic/json_writer.zig");

pub const Verdict = struct {
    /// True when the engine ran the witness through to a response.
    /// False when the run errored (parse, interpreter, replay-state
    /// divergence, etc.); see `error_text`.
    ran: bool,
    actual_status: u16,
    actual_body: []u8,
    error_text: ?[]u8,

    pub fn deinit(self: *Verdict, allocator: std.mem.Allocator) void {
        if (self.actual_body.len > 0) allocator.free(self.actual_body);
        if (self.error_text) |t| allocator.free(t);
        self.* = .{ .ran = false, .actual_status = 0, .actual_body = &.{}, .error_text = null };
    }

    /// True when the run reproduced the violation the witness was built
    /// to demonstrate. The classifier is property-specific: for the
    /// flow-leak family (`no_secret_leakage`, `no_credential_leakage`)
    /// it looks for the stub sentinel string the solver planted in the
    /// IO script (e.g. `"secret-sentinel"` for an optional-string env
    /// return). Other property tags fall back to "ran without error" -
    /// good enough until per-property classifiers ship.
    pub fn reproducedViolation(self: Verdict, witness: ui_payload.WitnessBody) bool {
        if (!self.ran) return false;
        if (witness.io_stubs.len == 0) return self.actual_status >= 200 and self.actual_status < 300;

        // For the leak family the sentinel lives in the first stub's
        // result_json. Strip the surrounding JSON quotes when it is a
        // string literal so a literal sentinel value can be substring-
        // matched against the response body.
        const stub_result = witness.io_stubs[0].result_json;
        const sentinel = unwrapJsonString(stub_result);
        if (sentinel.len == 0) return self.actual_status >= 200 and self.actual_status < 300;
        return std.mem.indexOf(u8, self.actual_body, sentinel) != null;
    }
};

pub const ReplayFn = *const fn (
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    witness_jsonl: []const u8,
) anyerror!Verdict;

var replay_fn: ?ReplayFn = null;

/// Register the runtime-backed implementation. Called once at startup
/// from a binary that ships both PI and the runtime stack. Subsequent
/// calls overwrite the pointer (last-write-wins; the codebase only
/// registers from one boot path so the second call would be a bug).
pub fn setReplayFn(f: ReplayFn) void {
    replay_fn = f;
}

pub fn isConfigured() bool {
    return replay_fn != null;
}

/// Replay a witness body against a handler at `handler_path`. Returns
/// `error.WitnessReplayNotConfigured` when no implementation is wired
/// (e.g. unit tests that stub the agent layer without the runtime).
pub fn replay(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    witness: ui_payload.WitnessBody,
) !Verdict {
    const f = replay_fn orelse return error.WitnessReplayNotConfigured;

    var jsonl: std.ArrayList(u8) = .empty;
    defer jsonl.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &jsonl);
    try writeWitnessJsonl(&aw.writer, witness);
    jsonl = aw.toArrayList();

    return f(allocator, handler_path, jsonl.items);
}

/// Emit a witness body as trace-compatible JSONL: one `request` record
/// followed by one `io` record per stub. The format matches what
/// `zigts.counterexample.writeJsonl` produces (minus the optional
/// `witness` header line, which the trace parser drops anyway).
pub fn writeWitnessJsonl(w: *std.Io.Writer, body: ui_payload.WitnessBody) !void {
    try w.print("{{\"type\":\"request\",\"method\":\"{s}\",\"url\":", .{body.request_method});
    try json_writer.writeString(w, body.request_url);
    if (body.request_has_auth) {
        try w.writeAll(",\"headers\":{\"authorization\":\"Bearer counterexample-token\"}");
    } else {
        try w.writeAll(",\"headers\":{}");
    }
    if (body.request_body) |b| {
        try w.writeAll(",\"body\":");
        try json_writer.writeString(w, b);
    } else {
        try w.writeAll(",\"body\":null");
    }
    try w.writeAll("}\n");

    for (body.io_stubs) |stub| {
        try w.print(
            "{{\"type\":\"io\",\"seq\":{d},\"module\":\"{s}\",\"fn\":\"{s}\",\"result\":{s}}}\n",
            .{ stub.seq, stub.module, stub.func, stub.result_json },
        );
    }
}

fn unwrapJsonString(text: []const u8) []const u8 {
    if (text.len < 2) return &.{};
    if (text[0] != '"' or text[text.len - 1] != '"') return &.{};
    return text[1 .. text.len - 1];
}

const testing = std.testing;

test "writeWitnessJsonl emits a parseable trace pair" {
    const allocator = testing.allocator;
    const stubs = [_]ui_payload.WitnessStub{
        .{
            .seq = 0,
            .module = @constCast("env"),
            .func = @constCast("env"),
            .result_json = @constCast("\"sentinel\""),
        },
    };
    const body = ui_payload.WitnessBody{
        .key = @constCast("a" ** 64),
        .property = @constCast("no_secret_leakage"),
        .summary = @constCast("test"),
        .origin_line = 1,
        .origin_column = 1,
        .sink_line = 2,
        .sink_column = 1,
        .request_method = @constCast("GET"),
        .request_url = @constCast("/"),
        .request_has_auth = false,
        .request_body = null,
        .io_stubs = @constCast(&stubs),
    };

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try writeWitnessJsonl(&aw.writer, body);
    buf = aw.toArrayList();

    try testing.expect(std.mem.indexOf(u8, buf.items, "\"type\":\"request\"") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"method\":\"GET\"") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"type\":\"io\"") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"fn\":\"env\"") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"result\":\"sentinel\"") != null);
}

test "Verdict.reproducedViolation matches sentinel substring" {
    const stubs = [_]ui_payload.WitnessStub{
        .{
            .seq = 0,
            .module = @constCast("env"),
            .func = @constCast("env"),
            .result_json = @constCast("\"secret-sentinel\""),
        },
    };
    const body = ui_payload.WitnessBody{
        .key = @constCast(""),
        .property = @constCast("no_secret_leakage"),
        .summary = @constCast(""),
        .origin_line = 1,
        .origin_column = 1,
        .sink_line = 1,
        .sink_column = 1,
        .request_method = @constCast("GET"),
        .request_url = @constCast("/"),
        .request_has_auth = false,
        .request_body = null,
        .io_stubs = @constCast(&stubs),
    };

    const reproducing = Verdict{
        .ran = true,
        .actual_status = 200,
        .actual_body = @constCast("{\"leaked\":\"secret-sentinel\"}"),
        .error_text = null,
    };
    try testing.expect(reproducing.reproducedViolation(body));

    const fixed = Verdict{
        .ran = true,
        .actual_status = 401,
        .actual_body = @constCast("Unauthorized"),
        .error_text = null,
    };
    try testing.expect(!fixed.reproducedViolation(body));

    const not_run = Verdict{ .ran = false, .actual_status = 0, .actual_body = &.{}, .error_text = null };
    try testing.expect(!not_run.reproducedViolation(body));
}

test "replay returns error.WitnessReplayNotConfigured when no fn is registered" {
    const allocator = testing.allocator;
    const body = ui_payload.WitnessBody{
        .key = @constCast(""),
        .property = @constCast(""),
        .summary = @constCast(""),
        .origin_line = 1,
        .origin_column = 1,
        .sink_line = 1,
        .sink_column = 1,
        .request_method = @constCast("GET"),
        .request_url = @constCast("/"),
        .request_has_auth = false,
        .request_body = null,
        .io_stubs = &.{},
    };

    // Save and restore so the test does not interfere with another run.
    const saved = replay_fn;
    defer replay_fn = saved;
    replay_fn = null;

    try testing.expectError(error.WitnessReplayNotConfigured, replay(allocator, "h.ts", body));
}
