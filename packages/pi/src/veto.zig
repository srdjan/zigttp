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
const ui_payload = @import("ui_payload.zig");

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
    const llm_text = try buf.toOwnedSlice(allocator);
    errdefer allocator.free(llm_text);

    var payload = if (result.new_count == 0)
        try buildProofCardPayload(allocator, &result)
    else
        try buildDiagnosticsPayload(allocator, edit.file, &result);
    errdefer payload.deinit(allocator);

    return .{
        .ok = result.new_count == 0,
        .llm_text = llm_text,
        .ui_payload = payload,
    };
}

fn buildDiagnosticsPayload(
    allocator: std.mem.Allocator,
    path: []const u8,
    result: *const edit_simulate.SimulateResult,
) !ui_payload.UiPayload {
    const items = try allocator.alloc(ui_payload.DiagnosticItem, result.violations.items.len);
    errdefer allocator.free(items);
    for (items) |*item| item.* = undefined;
    var i: usize = 0;
    errdefer {
        while (i > 0) {
            i -= 1;
            items[i].deinit(allocator);
        }
        allocator.free(items);
    }
    while (i < result.violations.items.len) : (i += 1) {
        const violation = result.violations.items[i];
        items[i] = try ui_payload.DiagnosticItem.init(
            allocator,
            violation.code,
            violation.severity,
            path,
            violation.line,
            violation.column,
            violation.message,
            violation.introduced_by_patch,
        );
    }

    return .{ .diagnostics = .{
        .summary = try std.fmt.allocPrint(
            allocator,
            "{d} violation(s), {d} new, {d} preexisting",
            .{ result.total, result.new_count, result.preexisting_count },
        ),
        .items = items,
    } };
}

fn buildProofCardPayload(
    allocator: std.mem.Allocator,
    result: *const edit_simulate.SimulateResult,
) !ui_payload.UiPayload {
    var highlights_list: std.ArrayList([]u8) = .empty;
    defer {
        for (highlights_list.items) |highlight| allocator.free(highlight);
        highlights_list.deinit(allocator);
    }

    if (result.properties) |properties| {
        inline for ([_]struct {
            label: []const u8,
            value: bool,
        }{
            .{ .label = "retry_safe", .value = properties.retry_safe },
            .{ .label = "idempotent", .value = properties.idempotent },
            .{ .label = "deterministic", .value = properties.deterministic },
            .{ .label = "read_only", .value = properties.read_only },
            .{ .label = "state_isolated", .value = properties.state_isolated },
            .{ .label = "injection_safe", .value = properties.injection_safe },
            .{ .label = "fault_covered", .value = properties.fault_covered },
        }) |entry| {
            if (entry.value) try highlights_list.append(allocator, try allocator.dupe(u8, entry.label));
        }
    }

    const highlights = try highlights_list.toOwnedSlice(allocator);
    const summary = if (result.new_count == 0 and result.total == 0)
        try allocator.dupe(u8, "No violations found.")
    else
        try allocator.dupe(u8, "No new violations introduced.");
    return .{ .proof_card = .{
        .title = try allocator.dupe(u8, "Compiler verification"),
        .summary = summary,
        .stats = .{
            .total = result.total,
            .new = result.new_count,
            .preexisting = result.preexisting_count,
        },
        .highlights = highlights,
    } };
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
    try testing.expect(std.mem.indexOf(u8, outcome.llm_text, "\"total\":0") != null);
    try testing.expect(std.mem.indexOf(u8, outcome.llm_text, "\"new\":0") != null);
    try testing.expect(outcome.ui_payload != null);
}

test "broken handler (var) fails the veto and body surfaces ZTS001" {
    var outcome = try runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "function handler(req: Request): Response { var x = 1; return Response.json({x}); }",
        .before = null,
    });
    defer outcome.deinit(testing.allocator);

    try testing.expect(!outcome.ok);
    try testing.expect(std.mem.indexOf(u8, outcome.llm_text, "\"ZTS001\"") != null);
    try testing.expect(std.mem.indexOf(u8, outcome.llm_text, "\"introduced_by_patch\":true") != null);
    try testing.expect(outcome.ui_payload != null);
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
    try testing.expect(std.mem.indexOf(u8, outcome.llm_text, "\"introduced_by_patch\":false") != null);
    try testing.expect(std.mem.indexOf(u8, outcome.llm_text, "\"new\":0") != null);
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
            .proof_card => |body| try testing.expect(body.llm_text.len > 0),
            else => return error.TestFailed,
        },
        else => return error.TestFailed,
    }
}
