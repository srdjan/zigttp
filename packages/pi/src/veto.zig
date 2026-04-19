//! Compiler veto hook. Bridges the phase-2 turn state machine's `run_veto`
//! action to the phase-1 `edit_simulate.simulate` primitive. Given a typed
//! `turn.ModelReply.Edit`, runs the analysis pipeline, serializes the v1
//! edit-simulate envelope, and returns a `turn.EditOutcome` the state machine
//! can feed back through `edit_verified`.
//!
//! `EditOutcome.ok = result.new_count == 0` — pre-existing violations don't
//! block the edit, which matches the contract pinned in turn.zig's tests and
//! in docs/zigts-expert-contract.md.

const std = @import("std");
const edit_simulate = @import("zigts_cli").edit_simulate;
const turn = @import("turn.zig");

/// Run edit-simulate against a proposed edit and produce an outcome the turn
/// state machine can consume. The returned `body` is an allocator-owned slice
/// (v1 edit-simulate JSON envelope) that the caller must free.
pub fn runVeto(
    allocator: std.mem.Allocator,
    edit: turn.Edit,
) !turn.EditOutcome {
    const input: edit_simulate.EditSimulateInput = .{
        .file = edit.file,
        .content = edit.content,
        .before = edit.before,
    };

    var result = try edit_simulate.simulate(allocator, input);
    defer result.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try edit_simulate.writeResultJson(&aw.writer, &result);

    buf = aw.toArrayList();
    return .{
        .ok = result.new_count == 0,
        .body = try buf.toOwnedSlice(allocator),
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "clean handler passes the veto with an empty violations body" {
    var outcome = try runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "function handler(req: Request): Response { return Response.json({ok: true}); }",
        .before = null,
    });
    defer outcome.deinit(testing.allocator);

    try testing.expect(outcome.ok);
    try testing.expect(std.mem.indexOf(u8, outcome.body, "\"total\":0") != null);
    try testing.expect(std.mem.indexOf(u8, outcome.body, "\"new\":0") != null);
}

test "broken handler (var) fails the veto and body surfaces ZTS001" {
    var outcome = try runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "function handler(req: Request): Response { var x = 1; return Response.json({x}); }",
        .before = null,
    });
    defer outcome.deinit(testing.allocator);

    try testing.expect(!outcome.ok);
    try testing.expect(std.mem.indexOf(u8, outcome.body, "\"ZTS001\"") != null);
    try testing.expect(std.mem.indexOf(u8, outcome.body, "\"introduced_by_patch\":true") != null);
}

test "pre-existing violation with matching before passes the veto" {
    // The edit adds a field to the response but leaves the `var x = 1` alone.
    // With `before` supplied, edit_simulate's violation-key heuristic matches
    // the var on both sides and marks it preexisting - new_count drops to 0
    // and the veto passes even though the body still contains the ZTS001.
    var outcome = try runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "function handler(req: Request): Response { var x = 1; return Response.json({x, y: 2}); }",
        .before = "function handler(req: Request): Response { var x = 1; return Response.json({x}); }",
    });
    defer outcome.deinit(testing.allocator);

    try testing.expect(outcome.ok);
    try testing.expect(std.mem.indexOf(u8, outcome.body, "\"introduced_by_patch\":false") != null);
    try testing.expect(std.mem.indexOf(u8, outcome.body, "\"new\":0") != null);
}

test "runVeto output fits turn.TurnMachine edit_verified event path" {
    var outcome = try runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "function handler(req: Request): Response { return Response.json({ok: true}); }",
        .before = null,
    });
    defer outcome.deinit(testing.allocator);

    // The state machine's verifying_edit state consumes exactly this shape
    // on the edit_verified event. Prove the wiring: machine enters
    // verifying_edit, then transitions to done with render(proof_card) when
    // fed the veto's outcome.
    var machine: turn.TurnMachine = .{ .state = .verifying_edit };
    const action = machine.transition(.{ .edit_verified = outcome });
    try testing.expectEqual(turn.TurnState.done, machine.state);
    switch (action) {
        .render => |msg| switch (msg) {
            .proof_card => |body| try testing.expect(body.len > 0),
            else => return error.TestFailed,
        },
        else => return error.TestFailed,
    }
}
