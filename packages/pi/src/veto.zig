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
///
/// zigttp:sql edits validate against the project's SQL schema, discovered from
/// the `sqlite` entry in the nearest zigttp.json (the same source `zigttp dev`
/// and `zigttp test` feed the analyzer). Callers issuing several veto attempts
/// (the turn loop's retries) should discover once with
/// `discoverSqlSchemaPath` and call `runVetoWithSchema` per attempt.
pub fn runVeto(
    allocator: std.mem.Allocator,
    edit: turn.Edit,
) !VetoResult {
    const discovered_schema = discoverSqlSchemaPath(allocator);
    defer if (discovered_schema) |path| allocator.free(path);
    return runVetoWithSchema(allocator, edit, discovered_schema);
}

/// Resolve the project's SQL schema from the nearest zigttp.json (cwd-anchored).
/// Returns null when no project or no `sqlite` entry. Caller frees.
pub fn discoverSqlSchemaPath(allocator: std.mem.Allocator) ?[]u8 {
    return edit_simulate.discoverProjectSqlSchemaPath(allocator, null);
}

/// `runVeto` with an explicit SQL schema path instead of project discovery.
/// Public for tests; `runVeto` is the production entry point.
pub fn runVetoWithSchema(
    allocator: std.mem.Allocator,
    edit: turn.Edit,
    sql_schema_path: ?[]const u8,
) !VetoResult {
    // A zigttp:sql edit without a configured schema cannot be verified.
    // Detecting the import up front (rather than relying on the analyzer's
    // schema-less failure, which varies by build configuration) gives the model
    // deterministic, actionable guidance instead of an opaque MissingSqlSchema
    // that crashes the turn or loops on an unsatisfiable diagnostic. The
    // `catch error.MissingSqlSchema` backstop below is the correct-altitude
    // guard for analyzer paths that still surface the failure as an error.
    if (sql_schema_path == null and importsSqlModule(edit.content)) {
        return try failedVetoWithGuidance(allocator, sql_unsupported_guidance);
    }

    const input: edit_simulate.EditSimulateInput = .{
        .file = edit.file,
        .content = edit.content,
        .before = edit.before,
        .sql_schema_path = sql_schema_path,
    };

    var result = edit_simulate.simulate(allocator, input) catch |err| switch (err) {
        // Backstop for any analyzer path that still surfaces the schema-less
        // failure as an error rather than a clean diagnostic.
        error.MissingSqlSchema => return try failedVetoWithGuidance(allocator, sql_unsupported_guidance),
        // The analyzer reports query-vs-schema failures as errors, not
        // diagnostics. A bad query in a draft is the model's mistake to fix,
        // not a fatal agent error: end the turn with a failed veto carrying
        // guidance instead of crashing it.
        error.InvalidSqlQuery,
        error.UnsupportedSqlStatement,
        error.PositionalSqlParameter,
        error.SqlitePrepareFailed,
        => return try failedVetoWithGuidance(allocator, sql_validation_guidance),
        // A configured schema that cannot be loaded (stale `sqlite` entry in
        // zigttp.json, missing or unreadable file, invalid schema SQL) must
        // not crash the turn either. Attribute file errors to the schema only
        // when one was actually configured; otherwise they are real analyzer
        // failures that should propagate.
        error.SqliteOpenFailed,
        error.SqliteExecFailed,
        error.FileNotFound,
        error.AccessDenied,
        => {
            if (sql_schema_path != null) {
                return try failedVetoWithGuidance(allocator, sql_schema_load_guidance);
            }
            return err;
        },
        else => return err,
    };
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
            if (nr.converged and nr.fully_canonical and !std.mem.eql(u8, nr.canonical_source, edit.content)) {
                if (edit_simulate.simulate(allocator, .{
                    .file = edit.file,
                    .content = nr.canonical_source,
                    .before = edit.before,
                    .sql_schema_path = sql_schema_path,
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

/// True when the edit statically imports the zigttp:sql virtual module.
fn importsSqlModule(content: []const u8) bool {
    var tokenizer = zigts.parser.Tokenizer.init(content);
    var at_statement_start = true;

    while (true) {
        const tok = tokenizer.next();
        switch (tok.type) {
            .eof => return false,
            .semicolon, .rbrace => at_statement_start = true,
            .kw_import => {
                if (at_statement_start) {
                    switch (scanImportForSqlModule(&tokenizer, content)) {
                        .matched => return true,
                        .finished_statement => at_statement_start = true,
                        .not_import_decl => at_statement_start = false,
                    }
                } else {
                    at_statement_start = false;
                }
            },
            else => at_statement_start = false,
        }
    }
}

const ImportScanResult = enum {
    matched,
    finished_statement,
    not_import_decl,
};

fn scanImportForSqlModule(tokenizer: *zigts.parser.Tokenizer, content: []const u8) ImportScanResult {
    const first = tokenizer.next();
    return switch (first.type) {
        .string_literal => if (stringLiteralEquals(first, content, "zigttp:sql")) .matched else .not_import_decl,
        .identifier => if (tokenTextEquals(first, content, "type"))
            skipImportStatement(tokenizer)
        else
            scanImportFromClauseForSqlModule(tokenizer, content),
        .lparen, .dot => .not_import_decl,
        else => scanImportFromClauseForSqlModule(tokenizer, content),
    };
}

fn scanImportFromClauseForSqlModule(tokenizer: *zigts.parser.Tokenizer, content: []const u8) ImportScanResult {
    var saw_from = false;
    while (true) {
        const tok = tokenizer.next();
        switch (tok.type) {
            .eof, .semicolon => return .finished_statement,
            .kw_from => saw_from = true,
            .string_literal => if (saw_from and stringLiteralEquals(tok, content, "zigttp:sql")) return .matched,
            else => {},
        }
    }
}

fn skipImportStatement(tokenizer: *zigts.parser.Tokenizer) ImportScanResult {
    while (true) {
        switch (tokenizer.next().type) {
            .eof, .semicolon => return .finished_statement,
            else => {},
        }
    }
}

fn tokenTextEquals(tok: zigts.parser.Token, content: []const u8, expected: []const u8) bool {
    return std.mem.eql(u8, tok.text(content), expected);
}

fn stringLiteralEquals(tok: zigts.parser.Token, content: []const u8, expected: []const u8) bool {
    const text = tok.text(content);
    if (text.len < 2) return false;
    const quote = text[0];
    if (quote != '"' and quote != '\'') return false;
    if (text[text.len - 1] != quote) return false;
    return std.mem.eql(u8, text[1 .. text.len - 1], expected);
}

/// No schema configured: tell the model (and, through it, the user) how to
/// configure one, or to avoid zigttp:sql. Used in place of the opaque
/// MissingSqlSchema error so the turn ends with guidance rather than crashing
/// or looping.
const sql_unsupported_guidance =
    "This edit imports zigttp:sql but the project has no SQL schema configured, " ++
    "so the analyzer cannot verify the queries. Do not retry this edit unchanged. " ++
    "Either ask the user to add a \"sqlite\" entry pointing at their schema to " ++
    "zigttp.json (for example \"sqlite\": \"schema.sql\") and then retry, or avoid " ++
    "zigttp:sql (for example use zigttp:cache instead). The handler can also be " ++
    "verified directly with `zigttp check --sql-schema <schema.sql> <handler.ts>`.";

/// A schema IS configured; the draft's SQL is what needs fixing.
const sql_validation_guidance =
    "This edit's zigttp:sql queries failed validation against the project's " ++
    "configured SQL schema: a declared query did not prepare (unknown table or " ++
    "column, unsupported statement shape, or positional parameters - only named " ++
    "parameters are supported). Fix the query to match the schema and emit a " ++
    "new, complete edit.";

/// A schema is configured but cannot be loaded: the config, not the draft,
/// needs fixing.
const sql_schema_load_guidance =
    "This edit needs the project's SQL schema, but the configured schema could " ++
    "not be loaded: the \"sqlite\" entry in zigttp.json points at a file that is " ++
    "missing, unreadable, or not a valid schema. Do not retry this edit " ++
    "unchanged. Ask the user to fix the \"sqlite\" path or restore the schema " ++
    "file, then retry.";

/// A failed veto carrying actionable guidance instead of an analyzer error
/// that would crash the turn. The guidance string must be a static literal
/// (duped into the result so deinit stays uniform).
fn failedVetoWithGuidance(allocator: std.mem.Allocator, guidance: []const u8) !VetoResult {
    const llm_text = try allocator.dupe(u8, guidance);
    errdefer allocator.free(llm_text);

    const hash_bytes = zigts.rule_registry.policyHash();
    const hash_copy = try allocator.dupe(u8, &hash_bytes);
    errdefer allocator.free(hash_copy);

    return .{
        .outcome = .{ .ok = false, .llm_text = llm_text, .ui_payload = null },
        .report = .{
            .policy_hash = hash_copy,
            .total = 0,
            .new = 0,
            .preexisting = 0,
            .after_properties = null,
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

test "failedVetoWithGuidance yields a failed veto with actionable guidance" {
    var result = try failedVetoWithGuidance(testing.allocator, sql_unsupported_guidance);
    defer result.deinit(testing.allocator);

    try testing.expect(!result.outcome.ok);
    try testing.expect(result.outcome.ui_payload == null);
    try testing.expect(std.mem.indexOf(u8, result.outcome.llm_text, "zigttp:sql") != null);
    try testing.expect(std.mem.indexOf(u8, result.outcome.llm_text, "sql-schema") != null);
    try testing.expectEqual(@as(u32, 0), result.report.new);
}

test "importsSqlModule matches only static zigttp sql imports" {
    try testing.expect(importsSqlModule("import { sql } from \"zigttp:sql\";"));
    try testing.expect(importsSqlModule("import { sqlOne } from 'zigttp:sql';"));
    try testing.expect(importsSqlModule("import \"zigttp:sql\";"));

    try testing.expect(!importsSqlModule("const marker = \"zigttp:sql\";"));
    try testing.expect(!importsSqlModule("// import { sql } from \"zigttp:sql\";\nconst ok = true;"));
    try testing.expect(!importsSqlModule("import type { SqlRow } from \"zigttp:sql\";"));
    try testing.expect(!importsSqlModule("const mod = import(\"zigttp:sql\");"));
    try testing.expect(!importsSqlModule("const shape = { import: true, from: \"zigttp:sql\" };"));
}

test "a zigttp:sql handler fails the veto with guidance instead of crashing" {
    // No project schema is discoverable from the test cwd, so the edit is
    // short-circuited to configuration guidance.
    var result = try runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "import { sqlOne } from \"zigttp:sql\";\n" ++
            "function handler(req: Request): Response { const row = sqlOne(\"SELECT * FROM users\"); return Response.json({row}); }",
        .before = null,
    });
    defer result.deinit(testing.allocator);

    try testing.expect(!result.outcome.ok);
    try testing.expect(std.mem.indexOf(u8, result.outcome.llm_text, "zigttp:sql") != null);
    try testing.expect(std.mem.indexOf(u8, result.outcome.llm_text, "sql-schema") != null);
}

test "a zigttp:sql edit passes the veto when a schema is configured" {
    // Mirrors the doctor fixture: the schema validates the declared query, so
    // the edit verifies end-to-end through the same pipeline `zigttp check
    // --sql-schema` uses. Before schema threading landed, every zigttp:sql
    // edit was short-circuited to "unsupported".
    const schema_sql =
        \\CREATE TABLE users (
        \\    id INTEGER PRIMARY KEY,
        \\    name TEXT NOT NULL
        \\);
    ;
    var uniquifier: u8 = undefined;
    const addr = @intFromPtr(&uniquifier);
    const schema_path = try std.fmt.allocPrintSentinel(
        testing.allocator,
        "/tmp/zigts-veto-schema-{x}.sql",
        .{addr},
        0,
    );
    defer testing.allocator.free(schema_path);
    try zigts.file_io.writeFile(testing.allocator, schema_path, schema_sql);
    defer _ = std.c.unlink(schema_path);

    var result = try runVetoWithSchema(testing.allocator, .{
        .file = "handler.ts",
        .content = "import { sql, sqlMany } from \"zigttp:sql\";\n" ++
            "\n" ++
            "sql(\"listUsers\", \"SELECT id, name FROM users ORDER BY id ASC\");\n" ++
            "\n" ++
            "function handler(req: Request): Response & Spec<\"state_isolated\"> {\n" ++
            "    return Response.json({ users: sqlMany(\"listUsers\", {}) });\n" ++
            "}\n",
        .before = null,
    }, schema_path);
    defer result.deinit(testing.allocator);

    try testing.expect(result.outcome.ok);
    try testing.expect(std.mem.indexOf(u8, result.outcome.llm_text, "sql-schema") == null);
    try testing.expectEqual(@as(u32, 0), result.report.new);
}

test "a zigttp:sql edit with an unloadable configured schema fails the veto with guidance" {
    // A stale `sqlite` entry (file moved or deleted) must end the turn with
    // configuration guidance, not propagate error.FileNotFound out of the
    // veto and crash the agent.
    var result = try runVetoWithSchema(testing.allocator, .{
        .file = "handler.ts",
        .content = "import { sql, sqlMany } from \"zigttp:sql\";\n" ++
            "\n" ++
            "sql(\"listUsers\", \"SELECT id, name FROM users ORDER BY id ASC\");\n" ++
            "\n" ++
            "function handler(req: Request): Response & Spec<\"state_isolated\"> {\n" ++
            "    return Response.json({ users: sqlMany(\"listUsers\", {}) });\n" ++
            "}\n",
        .before = null,
    }, "/nonexistent/zigttp-veto-missing-schema.sql");
    defer result.deinit(testing.allocator);

    try testing.expect(!result.outcome.ok);
    try testing.expect(std.mem.indexOf(u8, result.outcome.llm_text, "could not be loaded") != null);
}

test "a zigttp:sql edit against a mismatched schema fails the veto with diagnostics" {
    // The query references a column the schema does not define: the analyzer
    // must reject the edit through normal diagnostics, not crash with
    // MissingSqlSchema and not pass unverified.
    const schema_sql =
        \\CREATE TABLE users (
        \\    id INTEGER PRIMARY KEY
        \\);
    ;
    var uniquifier: u8 = undefined;
    const addr = @intFromPtr(&uniquifier);
    const schema_path = try std.fmt.allocPrintSentinel(
        testing.allocator,
        "/tmp/zigts-veto-schema-bad-{x}.sql",
        .{addr},
        0,
    );
    defer testing.allocator.free(schema_path);
    try zigts.file_io.writeFile(testing.allocator, schema_path, schema_sql);
    defer _ = std.c.unlink(schema_path);

    var result = try runVetoWithSchema(testing.allocator, .{
        .file = "handler.ts",
        .content = "import { sql, sqlMany } from \"zigttp:sql\";\n" ++
            "\n" ++
            "sql(\"listUsers\", \"SELECT id, name FROM users ORDER BY id ASC\");\n" ++
            "\n" ++
            "function handler(req: Request): Response & Spec<\"state_isolated\"> {\n" ++
            "    return Response.json({ users: sqlMany(\"listUsers\", {}) });\n" ++
            "}\n",
        .before = null,
    }, schema_path);
    defer result.deinit(testing.allocator);

    try testing.expect(!result.outcome.ok);
    try testing.expect(std.mem.indexOf(u8, result.outcome.llm_text, "failed validation") != null);
}

test "a non-sql handler mentioning zigttp sql as text still runs normal veto" {
    var result = try runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "// import { sqlOne } from \"zigttp:sql\";\n" ++
            "function handler(req: Request): Response & Spec<\"deterministic\"> {\n" ++
            "  const marker = \"zigttp:sql\";\n" ++
            "  return Response.json({ marker });\n" ++
            "}\n",
        .before = null,
    });
    defer result.deinit(testing.allocator);

    try testing.expect(result.outcome.ok);
    try testing.expect(std.mem.indexOf(u8, result.outcome.llm_text, "sql-schema") == null);
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
