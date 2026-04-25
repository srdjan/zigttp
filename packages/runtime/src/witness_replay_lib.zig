//! Witness replay library: thin wrapper that lets the PI agent host
//! reproduce a counterexample witness against a handler in-process,
//! without taking a direct dependency on the runtime package.
//!
//! The agent host (`pi_app`) registers the replay implementation via a
//! function pointer at startup; the binary that ships pi together with
//! the runtime stack (`zigts` / `zigttp`) provides this wrapper. The
//! protocol on the wire is JSONL - the same shape the engine already
//! consumes via `--test` - so neither side has to share a Zig type
//! beyond a tiny `Verdict` struct.

const std = @import("std");
const zq = @import("zigts");
const pi_app = @import("pi_app");
const replay_runner = @import("replay_runner.zig");
const RuntimeConfig = @import("zruntime.zig").RuntimeConfig;

const trace = zq.trace;
const Verdict = pi_app.witness_replay.Verdict;

/// Replay a witness emitted in trace-compatible JSONL against a handler
/// loaded from disk. The JSONL must contain at least one `{type:"request"}`
/// line and zero or more `{type:"io"}` lines; the parser ignores the
/// optional `{type:"witness"}` header line so callers can pass output
/// from `zigts.counterexample.writeJsonl` straight through.
pub fn replayWitnessJsonl(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    witness_jsonl: []const u8,
) anyerror!Verdict {
    const groups = try trace.parseTraceFile(allocator, witness_jsonl);
    defer {
        for (groups) |g| allocator.free(g.io_calls);
        allocator.free(groups);
    }
    if (groups.len == 0) return makeErrorVerdict(allocator, error.NoRequestInWitness);
    const group = groups[0];

    const handler_code = zq.file_io.readFile(allocator, handler_path, 16 * 1024 * 1024) catch |err| {
        return makeErrorVerdict(allocator, err);
    };
    defer allocator.free(handler_code);

    const config = RuntimeConfig{
        .replay_file_path = "witness-replay",
        .enforce_arena_escape = false,
    };

    var result = replay_runner.replayOne(
        allocator,
        config,
        handler_code,
        handler_path,
        &group,
    ) catch |err| {
        return makeErrorVerdict(allocator, err);
    };
    defer result.deinit(allocator);

    if (result.err) |err| return makeErrorVerdict(allocator, err);

    const body_copy: []u8 = if (result.actual_body_owned.len == 0)
        &[_]u8{}
    else
        try allocator.dupe(u8, result.actual_body_owned);
    return .{
        .ran = true,
        .actual_status = result.actual_status,
        .actual_body = body_copy,
        .error_text = null,
    };
}

fn makeErrorVerdict(allocator: std.mem.Allocator, err: anyerror) !Verdict {
    const text = try allocator.dupe(u8, @errorName(err));
    return .{
        .ran = false,
        .actual_status = 0,
        .actual_body = &.{},
        .error_text = text,
    };
}
