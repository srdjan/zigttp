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
const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const edit_simulate = zigts_cli.edit_simulate;
const canonicalize = zigts_cli.canonicalize;
const turn = @import("turn.zig");
const ui_payload = @import("ui_payload.zig");
const proof_enrichment = @import("proof_enrichment.zig");

/// Structured veto summary extracted from `edit_simulate.simulate`.
///
/// The `outcome` is the turn-machine-facing payload (ok bit, llm_text for the
/// provider, optional UiPayload for rendering). The `report` carries the
/// compiler-proof-flavored fields that a `verified_patch` event needs: the
/// `policy_hash` in force at apply time, the violation counts keyed by the
/// edit-simulate delta heuristic, and the post-edit `HandlerProperties` when
/// analysis reached the contract phase.
///
/// Both halves share the same allocator; deinit frees them together.
pub const VetoReport = struct {
    policy_hash: []u8,
    total: u32,
    new: u32,
    preexisting: u32,
    after_properties: ?ui_payload.PropertiesSnapshot,
    /// The edit content reduced to Canonical Normal Form. Non-null only when
    /// the edit passed the veto (`new == 0`) and normalization changed at least
    /// one byte. When null, the caller writes/attests the model's original
    /// bytes unchanged. Owned.
    normalized_content: ?[]u8 = null,
    /// Typed names of the canonical rewrites the normalize pass applied
    /// (`RepairIntent.asString`), in application order. Empty when nothing was
    /// rewritten. Owned (each string and the slice).
    rewrite_trace: [][]u8 = &.{},
    /// True iff the (normalized) edit content carries zero residual
    /// canonical-band diagnostics. The edit already passed the veto so it is a
    /// compiling handler; canonical violations are hard `check` errors, so this
    /// is normally true.
    is_canonical: bool = false,

    pub fn deinit(self: *VetoReport, allocator: std.mem.Allocator) void {
        allocator.free(self.policy_hash);
        if (self.normalized_content) |bytes| allocator.free(bytes);
        for (self.rewrite_trace) |intent| allocator.free(intent);
        allocator.free(self.rewrite_trace);
        self.* = .{
            .policy_hash = &.{},
            .total = 0,
            .new = 0,
            .preexisting = 0,
            .after_properties = null,
            .normalized_content = null,
            .rewrite_trace = &.{},
            .is_canonical = false,
        };
    }
};

pub const VetoResult = struct {
    outcome: turn.EditOutcome,
    report: VetoReport,

    pub fn deinit(self: *VetoResult, allocator: std.mem.Allocator) void {
        self.outcome.deinit(allocator);
        self.report.deinit(allocator);
    }
};

/// Run edit-simulate against a proposed edit and produce an outcome plus a
/// structured report. The outcome's `llm_text` is an allocator-owned slice
/// carrying the v1 edit-simulate JSON envelope. The report owns a copy of the
/// policy hash string. Call `VetoResult.deinit` to free both.
pub fn runVeto(
    allocator: std.mem.Allocator,
    edit: turn.Edit,
) !VetoResult {
    const input: edit_simulate.EditSimulateInput = .{
        .file = edit.file,
        .content = edit.content,
        .before = edit.before,
    };

    var result = try edit_simulate.simulate(allocator, input);
    defer result.deinit(allocator);

    // Salvage-on-reject. When an edit would be rejected, try normalizing the
    // draft and re-checking: if normalization clears the (canonical-band)
    // violations and the edit now passes, apply the canonicalized bytes instead
    // of bouncing the model — turning a veto rejection into an in-place auto-fix
    // for the model's canonical slips (ternary, `+=`, redundant bool compare,
    // ...). Normalize runs ONLY on a would-be-reject, so an already-passing edit
    // is applied verbatim (surgical; no wasted pass). The `ok` bit stays
    // `new_count == 0` on the final (possibly salvaged) result.
    var salvaged_content: ?[]u8 = null;
    errdefer if (salvaged_content) |b| allocator.free(b);
    var rewrite_trace: [][]u8 = &.{};
    errdefer {
        for (rewrite_trace) |intent| allocator.free(intent);
        allocator.free(rewrite_trace);
    }
    if (result.new_count > 0) {
        if (canonicalize.normalizeSource(allocator, edit.content, edit.file)) |nr_value| {
            var nr = nr_value;
            defer nr.deinit(allocator);
            if (!std.mem.eql(u8, nr.canonical_source, edit.content)) {
                if (edit_simulate.simulate(allocator, .{
                    .file = edit.file,
                    .content = nr.canonical_source,
                    .before = edit.before,
                })) |r2_value| {
                    var r2 = r2_value;
                    if (r2.new_count == 0) {
                        // Salvaged: adopt the normalized analysis and bytes. The
                        // old result is freed here; the function-level
                        // `defer result.deinit` frees the moved-in r2 at exit.
                        result.deinit(allocator);
                        result = r2;
                        salvaged_content = try allocator.dupe(u8, nr.canonical_source);
                        rewrite_trace = try dupeRewriteTrace(allocator, &nr);
                    } else {
                        r2.deinit(allocator);
                    }
                } else |_| {}
            }
        } else |_| {}
    }

    // `is_canonical` reflects the applied content. A contract is only built for
    // a fully-canonical handler, so `properties.canonical` is the authoritative
    // chip; it is null (false) when the content still carries a canonical-band
    // hard error the normalizer could not clear.
    const is_canonical = if (result.properties) |p| p.canonical else false;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try edit_simulate.writeResultJson(&aw.writer, &result);

    buf = aw.toArrayList();
    const llm_text = try buf.toOwnedSlice(allocator);
    errdefer allocator.free(llm_text);

    var payload = if (result.new_count == 0)
        try buildProofCardPayload(allocator, &result, is_canonical)
    else
        try buildDiagnosticsPayload(allocator, edit.file, &result);
    errdefer payload.deinit(allocator);

    const hash_bytes = zigts.rule_registry.policyHash();
    const hash_copy = try allocator.dupe(u8, &hash_bytes);
    errdefer allocator.free(hash_copy);

    const snapshot: ?ui_payload.PropertiesSnapshot = if (result.properties) |p|
        proof_enrichment.propertiesSnapshot(p)
    else
        null;

    return .{
        .outcome = .{
            .ok = result.new_count == 0,
            .llm_text = llm_text,
            .ui_payload = payload,
        },
        .report = .{
            .policy_hash = hash_copy,
            .total = result.total,
            .new = result.new_count,
            .preexisting = result.preexisting_count,
            .after_properties = snapshot,
            .normalized_content = salvaged_content,
            .rewrite_trace = rewrite_trace,
            .is_canonical = is_canonical,
        },
    };
}

/// Dupe a normalize pass's rewrite trace (typed `RepairIntent` names) into an
/// owned `[][]u8` for the `VetoReport`. Self-contained cleanup on a mid-dupe
/// OOM so the caller never sees a partially-owned slice.
fn dupeRewriteTrace(
    allocator: std.mem.Allocator,
    nr: *const canonicalize.NormalizeResult,
) ![][]u8 {
    const trace = try allocator.alloc([]u8, nr.rewrite_trace.items.len);
    errdefer allocator.free(trace);
    var i: usize = 0;
    errdefer {
        while (i > 0) {
            i -= 1;
            allocator.free(trace[i]);
        }
    }
    while (i < nr.rewrite_trace.items.len) : (i += 1) {
        trace[i] = try allocator.dupe(u8, nr.rewrite_trace.items[i].asString());
    }
    return trace;
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
    is_canonical: bool,
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

    // The applied bytes are reduced to Canonical Normal Form on the way to
    // disk, so surface a `canonical` chip alongside the property highlights.
    if (is_canonical) try highlights_list.append(allocator, try allocator.dupe(u8, "canonical"));

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
    var result = try runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "function handler(req: Request): Response & Spec<\"deterministic\"> { return Response.json({ok: true}); }",
        .before = null,
    });
    defer result.deinit(testing.allocator);

    try testing.expect(result.outcome.ok);
    try testing.expect(std.mem.indexOf(u8, result.outcome.llm_text, "\"total\":0") != null);
    try testing.expect(std.mem.indexOf(u8, result.outcome.llm_text, "\"new\":0") != null);
    try testing.expect(result.outcome.ui_payload != null);
    try testing.expectEqual(@as(usize, 64), result.report.policy_hash.len);
    try testing.expectEqual(@as(u32, 0), result.report.total);
    try testing.expectEqual(@as(u32, 0), result.report.new);
}

test "a veto-passing edit is normalized: is_canonical set and the proof card carries a canonical chip" {
    // A handler that passes the veto (new_count == 0). Because every
    // canonical-band diagnostic is a hard `check` error that would itself fail
    // the veto, any edit that reaches the green arm is already canonical, so
    // normalizeSource is a behavior-preserving no-op here: the report carries
    // is_canonical = true, no rewrite trace, and no separate normalized buffer
    // (the caller writes the model's original bytes).
    var result = try runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "const handler = (req: Request): Response & Spec<\"deterministic\"> => Response.json({ok: true});",
        .before = null,
    });
    defer result.deinit(testing.allocator);

    try testing.expect(result.outcome.ok);
    try testing.expect(result.report.is_canonical);
    try testing.expect(result.report.normalized_content == null);
    try testing.expectEqual(@as(usize, 0), result.report.rewrite_trace.len);

    // The proof card surfaces a `canonical` chip among its highlights.
    try testing.expect(result.outcome.ui_payload != null);
    switch (result.outcome.ui_payload.?) {
        .proof_card => |card| {
            var saw_canonical = false;
            for (card.highlights) |highlight| {
                if (std.mem.eql(u8, highlight, "canonical")) saw_canonical = true;
            }
            try testing.expect(saw_canonical);
        },
        else => return error.TestFailed,
    }
}

test "a canonical-band failing edit is salvaged by normalize-on-reject" {
    // `let x = 1;` is ZTS604, a hard canonical error: the raw draft would be
    // rejected. Salvage-on-reject normalizes it (`let` -> `const`), re-checks,
    // and the edit then passes — the canonicalized bytes are applied and the
    // rewrite trace is surfaced. This is the model's canonical slip auto-fixed.
    var result = try runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "function handler(req: Request): Response & Spec<\"deterministic\"> {\n  let x = 1;\n  return Response.json({ x });\n}\n",
        .before = null,
    });
    defer result.deinit(testing.allocator);

    try testing.expect(result.outcome.ok);
    try testing.expect(result.report.is_canonical);
    try testing.expect(result.report.normalized_content != null);
    try testing.expect(std.mem.indexOf(u8, result.report.normalized_content.?, "const x = 1") != null);
    try testing.expect(std.mem.indexOf(u8, result.report.normalized_content.?, "let x") == null);
    try testing.expectEqual(@as(usize, 1), result.report.rewrite_trace.len);
    try testing.expectEqualStrings("replace_let_with_const", result.report.rewrite_trace[0]);
}

test "a non-canonical failing edit is not salvaged (var stays rejected)" {
    // `var` (ZTS001) has no canonical rewriter, so normalize is a no-op and the
    // edit stays rejected: no salvage, no canonical chip, no rewrite trace.
    var result = try runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "function handler(req: Request): Response { var x = 1; return Response.json({x}); }",
        .before = null,
    });
    defer result.deinit(testing.allocator);

    try testing.expect(!result.outcome.ok);
    try testing.expect(!result.report.is_canonical);
    try testing.expect(result.report.normalized_content == null);
    try testing.expectEqual(@as(usize, 0), result.report.rewrite_trace.len);
}

test "broken handler (var) fails the veto and body surfaces ZTS001" {
    var result = try runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "function handler(req: Request): Response & Spec<\"deterministic\"> { var x = 1; return Response.json({x}); }",
        .before = null,
    });
    defer result.deinit(testing.allocator);

    try testing.expect(!result.outcome.ok);
    try testing.expect(std.mem.indexOf(u8, result.outcome.llm_text, "\"ZTS001\"") != null);
    try testing.expect(std.mem.indexOf(u8, result.outcome.llm_text, "\"introduced_by_patch\":true") != null);
    try testing.expect(result.outcome.ui_payload != null);
    try testing.expect(result.report.new >= 1);
}

test "pre-existing violation with matching before passes the veto" {
    var result = try runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "function handler(req: Request): Response & Spec<\"deterministic\"> { var x = 1; return Response.json({x, y: 2}); }",
        .before = "function handler(req: Request): Response & Spec<\"deterministic\"> { var x = 1; return Response.json({x}); }",
    });
    defer result.deinit(testing.allocator);

    try testing.expect(result.outcome.ok);
    try testing.expect(std.mem.indexOf(u8, result.outcome.llm_text, "\"introduced_by_patch\":false") != null);
    try testing.expect(std.mem.indexOf(u8, result.outcome.llm_text, "\"new\":0") != null);
    try testing.expectEqual(@as(u32, 0), result.report.new);
    try testing.expect(result.report.preexisting >= 1);
}

test "runVeto output fits turn.TurnMachine edit_verified event path" {
    var result = try runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "function handler(req: Request): Response & Spec<\"deterministic\"> { return Response.json({ok: true}); }",
        .before = null,
    });
    defer result.deinit(testing.allocator);

    var machine: turn.TurnMachine = .{ .state = .verifying_edit };
    const action = machine.transition(.{ .edit_verified = result.outcome });
    try testing.expectEqual(turn.TurnState.done, machine.state);
    switch (action) {
        .render => |msg| switch (msg) {
            .proof_card => |body| try testing.expect(body.llm_text.len > 0),
            else => return error.TestFailed,
        },
        else => return error.TestFailed,
    }
}
