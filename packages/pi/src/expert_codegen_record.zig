//! expert_codegen_record - live baseline recorder for the codegen eval.
//!
//! Recording spends real tokens, so everything here is gated behind
//! `ZIGTTP_CODEGEN_RECORD=1` and a live ANTHROPIC_API_KEY; without both, every
//! test skips and the default `zig build test-expert-app` never touches the
//! network. The recorder drives real expert turns (full persona + tool
//! registry) through the live Anthropic client, which tees each roundtrip's SSE
//! body to a per-case cassette. Those cassettes are then committed and replayed
//! deterministically (free) by the offline eval. Recorded once, replayed
//! forever.

const std = @import("std");
const anthropic = @import("providers/anthropic/client.zig");
const cassette_client = @import("providers/cassette_client.zig");
const transcript_mod = @import("transcript.zig");

const testing = std.testing;

/// Where committed cassettes live, relative to the repo root (the cwd when the
/// recorder runs via `zig build`).
pub const cassette_root = "packages/pi/src/providers/testdata/codegen";

/// Borrowed env-var read (no allocation), mirroring agent.zig's `envVar`:
/// std.process env helpers are not the 0.16 path; std.c.getenv is.
fn envValue(name_z: [:0]const u8) ?[]const u8 {
    const raw = std.c.getenv(name_z) orelse return null;
    const v = std.mem.sliceTo(raw, 0);
    return if (v.len == 0) null else v;
}

fn recordingRequested() bool {
    const flag = envValue("ZIGTTP_CODEGEN_RECORD") orelse return false;
    return std.mem.eql(u8, flag, "1");
}

// Smoke test: prove the live record-tee writes a cassette that replays to the
// same reply, with a single cheap call, before spending tokens on the full
// corpus. Skipped unless ZIGTTP_CODEGEN_RECORD=1 and a key is present.
test "record-tee captures a faithful anthropic cassette (live, gated)" {
    const allocator = testing.allocator;
    if (!recordingRequested()) return error.SkipZigTest;
    const key = envValue("ANTHROPIC_API_KEY") orelse return error.SkipZigTest;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    var client = anthropic.Client.init(.{
        .api_key = key,
        .system_prompt = "You are a terse assistant. Reply with exactly one word.",
    });
    client.enableRecording(cassette_root, "_smoke");

    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(allocator);
    try tr.append(allocator, .{ .user_text = "Say OK." });

    const live = client.sendTurn(a, &tr, null) catch |err| {
        std.debug.print("[codegen-smoke] sendTurn failed: {s}\n", .{@errorName(err)});
        return err;
    };

    // Replay the just-written cassette and confirm it round-trips to a reply.
    const path = cassette_root ++ "/_smoke/step_0.jsonl";
    const cassette = try cassette_client.loadCassetteFromPath(a, path);
    const replayed = try cassette_client.replay(a, cassette);

    const live_kind = std.meta.activeTag(live.reply.response);
    const replay_kind = std.meta.activeTag(replayed.reply.response);
    try testing.expectEqual(live_kind, replay_kind);
    std.debug.print("[codegen-smoke] live and replay agree: {s}\n", .{@tagName(replay_kind)});
}
