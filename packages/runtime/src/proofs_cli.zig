//! `zigttp proofs` subcommand. `list` and `show <ref>` render the proof
//! ledger using the same card renderer as `zigttp deploy`. `show`
//! reconstructs a `DeployReview` over a borrowed historical snapshot.

const std = @import("std");
const proof_ledger = @import("proof_ledger.zig");
const review = @import("deploy/review.zig");
const printer_mod = @import("deploy/printer.zig");

const Subcommand = enum {
    list,
    show,
    diff,
    watch,
    @"export",
    help,
};

/// Errors that proofs_cli explained on stderr. Callers like dev_cli swallow
/// these so the shell exit is clean.
pub fn isExpectedUserError(err: anyerror) bool {
    return switch (err) {
        error.UnknownSubcommand,
        error.MissingRefArgument,
        error.TooManyArguments,
        error.InvalidRef,
        error.NoEvents,
        error.OutOfRange,
        error.NotFound,
        error.Ambiguous,
        error.MissingArgValue,
        error.InvalidFormat,
        error.UnknownArgument,
        => true,
        else => false,
    };
}

pub fn run(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var stdout_buf: [4096]u8 = undefined;
    var stderr_buf: [1024]u8 = undefined;
    var stdout_writer = printer_mod.FdWriter.init(std.c.STDOUT_FILENO, stdout_buf[0..]);
    var stderr_writer = printer_mod.FdWriter.init(std.c.STDERR_FILENO, stderr_buf[0..]);
    defer stdout_writer.interface.flush() catch {};
    defer stderr_writer.interface.flush() catch {};

    try runWith(allocator, argv, &stdout_writer.interface, &stderr_writer.interface);
}

/// Tests pass `std.Io.Writer.Allocating` buffers as `stdout`/`stderr` and
/// assert on the captured output.
pub fn runWith(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !void {
    const sub = parseSubcommand(argv) catch |err| {
        try stderr.writeAll("zigttp proofs: unknown subcommand\n\n");
        try writeHelp(stderr);
        return err;
    };

    switch (sub) {
        .help => {
            try writeHelp(stdout);
        },
        .list => try listCommand(allocator, stdout),
        .show => try showCommand(allocator, argv[1..], stdout, stderr),
        .diff => try diffCommand(allocator, argv[1..], stdout, stderr),
        .watch => try watchCommand(allocator, stdout),
        .@"export" => try exportCommand(allocator, argv[1..], stdout, stderr),
    }
}

fn parseSubcommand(argv: []const []const u8) !Subcommand {
    if (argv.len == 0) return .list;
    const first = argv[0];
    if (std.mem.eql(u8, first, "--help") or std.mem.eql(u8, first, "-h") or std.mem.eql(u8, first, "help")) return .help;
    if (std.mem.eql(u8, first, "list")) return .list;
    if (std.mem.eql(u8, first, "show")) return .show;
    if (std.mem.eql(u8, first, "diff")) return .diff;
    if (std.mem.eql(u8, first, "watch")) return .watch;
    if (std.mem.eql(u8, first, "export")) return .@"export";
    return error.UnknownSubcommand;
}

fn writeHelp(w: *std.Io.Writer) !void {
    try w.writeAll(
        \\Usage: zigttp proofs <subcommand>
        \\
        \\Subcommands:
        \\  list             Print the last 10 ledger entries (newest last).
        \\  show <ref>       Re-render the proof review card from a past entry.
        \\  diff <a> <b>     Render the card for <b> using <a> as the baseline.
        \\                   <a> is typically the older ref, <b> the newer.
        \\  watch            Print existing entries, then tail new ones as they
        \\                   land (Ctrl+C to exit).
        \\  export           Render a single entry in a shareable format.
        \\                   Flags: [--format md|html|svg] [--ref REF].
        \\                   Defaults: --format md, --ref HEAD.
        \\
        \\Refs may be HEAD, HEAD~N, or a contract sha prefix.
        \\Ledger file: .zigttp/proofs.jsonl
        \\
    );
}

// ---------------------------------------------------------------------------
// list
// ---------------------------------------------------------------------------

fn listCommand(allocator: std.mem.Allocator, stdout: *std.Io.Writer) !void {
    const events = try proof_ledger.readEvents(allocator);
    defer proof_ledger.freeEvents(allocator, events);

    if (events.len == 0) {
        try stdout.writeAll("No proofs recorded yet. Run `zigttp deploy`, `zigttp dev --watch --prove`, or `zigts check` to populate .zigttp/proofs.jsonl.\n");
        return;
    }

    try stdout.writeAll(list_header);
    const max_rows: usize = 10;
    const start: usize = if (events.len > max_rows) events.len - max_rows else 0;
    try printRows(allocator, stdout, events, start);
}

const list_header = "Time              Kind    Sha       Verdict     Surface\n";

/// Print one row per event in `events[start_idx..]`, using each row's
/// preceding event (`events[idx - 1]`) as the baseline for delta classification.
fn printRows(
    allocator: std.mem.Allocator,
    stdout: *std.Io.Writer,
    events: []const proof_ledger.Event,
    start_idx: usize,
) !void {
    for (events[start_idx..], start_idx..) |ev, idx| {
        try writeListRow(allocator, stdout, &ev, if (idx == 0) null else &events[idx - 1].facts);
    }
}

fn writeListRow(
    allocator: std.mem.Allocator,
    stdout: *std.Io.Writer,
    ev: *const proof_ledger.Event,
    baseline: ?*const review.ReviewFacts,
) !void {
    var ts_buf: [20]u8 = undefined;
    const ts_str = formatTimestampShort(ev.ts_unix_ms, ts_buf[0..]);

    const sha = ev.facts.contract_sha;
    const sha_short = sha[0..@min(sha.len, 8)];

    var delta = try review.deriveDelta(allocator, &ev.facts, baseline);
    defer delta.deinit(allocator);

    try stdout.print("{s}  {s:<6}  {s:<8}  {s:<11} {d}r/{d}e/{d}h/{d}c/{d}cap\n", .{
        ts_str,
        ev.kind.toString(),
        sha_short,
        review.classify(&delta).toString(),
        ev.facts.routes.len,
        ev.facts.env_keys.len,
        ev.facts.egress_hosts.len,
        ev.facts.cache_namespaces.len,
        ev.facts.capabilities.len,
    });
}

// ---------------------------------------------------------------------------
// show
// ---------------------------------------------------------------------------

fn showCommand(
    allocator: std.mem.Allocator,
    rest: []const []const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !void {
    if (rest.len == 0) {
        try stderr.writeAll("zigttp proofs show requires a ref (HEAD, HEAD~N, or a contract sha prefix).\n");
        return error.MissingRefArgument;
    }
    if (rest.len > 1) {
        try stderr.writeAll("zigttp proofs show accepts exactly one ref.\n");
        return error.TooManyArguments;
    }

    const events = try proof_ledger.readEvents(allocator);
    defer proof_ledger.freeEvents(allocator, events);

    const idx = try resolveRefOrExplain(events, rest[0], stderr);
    try renderHistorical(allocator, &events[idx], if (idx == 0) null else &events[idx - 1].facts, stdout);
}

fn diffCommand(
    allocator: std.mem.Allocator,
    rest: []const []const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !void {
    if (rest.len < 2) {
        try stderr.writeAll("zigttp proofs diff requires two refs: <a> <b> (a = baseline, b = current).\n");
        return error.MissingRefArgument;
    }
    if (rest.len > 2) {
        try stderr.writeAll("zigttp proofs diff accepts exactly two refs.\n");
        return error.TooManyArguments;
    }

    const events = try proof_ledger.readEvents(allocator);
    defer proof_ledger.freeEvents(allocator, events);

    const a_idx = try resolveRefOrExplain(events, rest[0], stderr);
    const b_idx = try resolveRefOrExplain(events, rest[1], stderr);

    try renderHistorical(allocator, &events[b_idx], &events[a_idx].facts, stdout);
}

/// Tail the ledger forever: print existing entries, then poll every 500 ms
/// and emit any newly appended rows. Exits only on signal (Ctrl+C) or an
/// unrecoverable read error.
fn watchCommand(allocator: std.mem.Allocator, stdout: *std.Io.Writer) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var seen: usize = 0;
    var printed_header = false;

    while (true) {
        const events = try proof_ledger.readEvents(allocator);
        defer proof_ledger.freeEvents(allocator, events);

        if (!printed_header and events.len > 0) {
            try stdout.writeAll(list_header);
            printed_header = true;
        }
        if (events.len > seen) {
            try printRows(allocator, stdout, events, seen);
            try stdout.flush();
            seen = events.len;
        }

        try std.Io.sleep(io, .fromMilliseconds(500), .awake);
    }
}

// ---------------------------------------------------------------------------
// export
// ---------------------------------------------------------------------------

const ExportFormat = enum {
    md,
    html,
    svg,

    fn fromString(s: []const u8) ?ExportFormat {
        if (std.mem.eql(u8, s, "md") or std.mem.eql(u8, s, "markdown")) return .md;
        if (std.mem.eql(u8, s, "html")) return .html;
        if (std.mem.eql(u8, s, "svg")) return .svg;
        return null;
    }
};

fn exportCommand(
    allocator: std.mem.Allocator,
    rest: []const []const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !void {
    var format: ExportFormat = .md;
    var ref_text: []const u8 = "HEAD";

    var i: usize = 0;
    while (i < rest.len) : (i += 1) {
        const arg = rest[i];
        if (std.mem.eql(u8, arg, "--format")) {
            i += 1;
            if (i >= rest.len) {
                try stderr.writeAll("--format requires a value (md, html, or svg).\n");
                return error.MissingArgValue;
            }
            format = ExportFormat.fromString(rest[i]) orelse {
                try stderr.writeAll("Unknown --format. Expected md, html, or svg.\n");
                return error.InvalidFormat;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--ref")) {
            i += 1;
            if (i >= rest.len) {
                try stderr.writeAll("--ref requires a value (HEAD, HEAD~N, or a sha prefix).\n");
                return error.MissingArgValue;
            }
            ref_text = rest[i];
            continue;
        }
        try stderr.writeAll("zigttp proofs export accepts --format and --ref only.\n");
        return error.UnknownArgument;
    }

    const events = try proof_ledger.readEvents(allocator);
    defer proof_ledger.freeEvents(allocator, events);

    const idx = try resolveRefOrExplain(events, ref_text, stderr);
    const ev = &events[idx];
    const baseline: ?*const review.ReviewFacts = if (idx == 0) null else &events[idx - 1].facts;

    var delta = try review.deriveDelta(allocator, &ev.facts, baseline);
    defer delta.deinit(allocator);
    const verdict = review.classify(&delta);

    switch (format) {
        .md => try renderMarkdown(stdout, ev, baseline, &delta, verdict),
        .html => try renderHtml(stdout, ev, baseline, &delta, verdict),
        .svg => try renderSvg(stdout, &ev.facts, verdict),
    }
}

fn renderMarkdown(
    stdout: *std.Io.Writer,
    ev: *const proof_ledger.Event,
    baseline: ?*const review.ReviewFacts,
    delta: *const review.ReviewDelta,
    verdict: review.Verdict,
) !void {
    try stdout.writeAll("## Proof review\n\n");
    try stdout.print("- **Verdict**: `{s}`\n", .{verdict.toString()});
    try stdout.print("- **Handler**: `{s}`\n", .{ev.handler_path});
    if (ev.service_name) |name| try stdout.print("- **Service**: {s}\n", .{name});
    try stdout.print("- **Kind**: {s}\n", .{ev.kind.toString()});
    try stdout.print("- **Contract**: `{s}`\n", .{ev.facts.contract_sha});
    try stdout.print("- **Proof level**: {s}\n", .{ev.facts.proof_level.toString()});
    if (baseline) |b| {
        try stdout.print("- **Baseline**: `{s}`\n", .{b.contract_sha});
    } else {
        try stdout.writeAll("- **Baseline**: _(none — first entry)_\n");
    }

    try stdout.writeAll("\n### Proven properties\n\n");
    var any = false;
    inline for (review.property_metas) |meta| {
        if (@field(ev.facts.properties, meta.field)) {
            try stdout.print("- {s}\n", .{meta.label});
            any = true;
        }
    }
    if (!any) try stdout.writeAll("_(none)_\n");

    const has_surface = ev.facts.routes.len > 0 or ev.facts.env_keys.len > 0 or
        ev.facts.egress_hosts.len > 0 or ev.facts.cache_namespaces.len > 0 or
        ev.facts.capabilities.len > 0;
    if (has_surface) {
        try stdout.writeAll("\n### Surface\n\n");
        if (ev.facts.routes.len > 0) {
            try stdout.writeAll("- **Routes**:\n");
            for (ev.facts.routes) |r| {
                try stdout.print("  - `{s}`{s}\n", .{ r.pattern, if (r.is_prefix) " (prefix)" else "" });
            }
        }
        try writeMdStringList(stdout, "Env keys", ev.facts.env_keys);
        try writeMdStringList(stdout, "Egress hosts", ev.facts.egress_hosts);
        try writeMdStringList(stdout, "Cache namespaces", ev.facts.cache_namespaces);
        try writeMdStringList(stdout, "Capabilities", ev.facts.capabilities);
    }

    if (baseline != null and !delta.isEmpty()) {
        try stdout.writeAll("\n### Changes\n\n");
        for (delta.added_routes) |r| try writeMdChange(stdout, "+ route", r.pattern);
        for (delta.removed_routes) |r| try writeMdChange(stdout, "- route", r.pattern);
        for (delta.added_env) |k| try writeMdChange(stdout, "+ env", k);
        for (delta.removed_env) |k| try writeMdChange(stdout, "- env", k);
        for (delta.added_egress) |h| try writeMdChange(stdout, "+ egress", h);
        for (delta.removed_egress) |h| try writeMdChange(stdout, "- egress", h);
        for (delta.added_cache) |n| try writeMdChange(stdout, "+ cache", n);
        for (delta.removed_cache) |n| try writeMdChange(stdout, "- cache", n);
        for (delta.added_capabilities) |c| try writeMdChange(stdout, "+ cap", c);
        for (delta.removed_capabilities) |c| try writeMdChange(stdout, "- cap", c);
        for (delta.promoted_properties) |p| try writeMdChange(stdout, "+ property", p.label);
        for (delta.demoted_properties) |p| try writeMdChange(stdout, "- property", p.label);
        if (delta.proof_level_change) |change| {
            try stdout.print("- `proof level: {s} -> {s}`\n", .{ change.old.toString(), change.new.toString() });
        }
    }
}

fn writeMdChange(stdout: *std.Io.Writer, prefix: []const u8, value: []const u8) !void {
    try stdout.print("- `{s} {s}`\n", .{ prefix, value });
}

fn writeMdStringList(stdout: *std.Io.Writer, label: []const u8, items: []const []const u8) !void {
    if (items.len == 0) return;
    try stdout.print("- **{s}**: ", .{label});
    for (items, 0..) |item, idx| {
        if (idx > 0) try stdout.writeAll(", ");
        try stdout.print("`{s}`", .{item});
    }
    try stdout.writeAll("\n");
}

fn renderHtml(
    stdout: *std.Io.Writer,
    ev: *const proof_ledger.Event,
    baseline: ?*const review.ReviewFacts,
    delta: *const review.ReviewDelta,
    verdict: review.Verdict,
) !void {
    try stdout.writeAll(
        \\<!DOCTYPE html>
        \\<html lang="en"><head><meta charset="utf-8"><title>Proof review</title>
        \\<style>
        \\body{font:14px/1.5 -apple-system,BlinkMacSystemFont,system-ui,sans-serif;max-width:720px;margin:2rem auto;padding:0 1rem;color:#111}
        \\h1{font-size:1.4rem;margin-bottom:0.4rem}
        \\h2{font-size:1.05rem;margin:1.4rem 0 0.4rem}
        \\.verdict{display:inline-block;padding:0.15rem 0.6rem;border-radius:999px;color:#fff;font-weight:600;font-size:0.85rem}
        \\.safe{background:#22c55e}
        \\.safe_with_additions{background:#3b82f6}
        \\.breaking{background:#ef4444}
        \\code{background:#f3f4f6;padding:0.05rem 0.3rem;border-radius:4px;font-size:0.9em}
        \\dl{display:grid;grid-template-columns:auto 1fr;gap:0.2rem 1rem;margin:0}
        \\dt{font-weight:600;color:#555}
        \\dd{margin:0}
        \\ul{margin:0.2rem 0 0;padding-left:1.4rem}
        \\</style></head><body>
        \\<h1>Proof review</h1>
        \\
    );
    try stdout.print("<p><span class=\"verdict {s}\">{s}</span></p>\n", .{ verdict.toString(), verdict.toString() });
    try stdout.writeAll("<dl>\n<dt>Handler</dt><dd><code>");
    try writeHtmlEscaped(stdout, ev.handler_path);
    try stdout.writeAll("</code></dd>\n");
    if (ev.service_name) |name| {
        try stdout.writeAll("<dt>Service</dt><dd>");
        try writeHtmlEscaped(stdout, name);
        try stdout.writeAll("</dd>\n");
    }
    try stdout.print("<dt>Kind</dt><dd>{s}</dd>\n", .{ev.kind.toString()});
    try stdout.writeAll("<dt>Contract</dt><dd><code>");
    try writeHtmlEscaped(stdout, ev.facts.contract_sha);
    try stdout.writeAll("</code></dd>\n");
    try stdout.print("<dt>Proof level</dt><dd>{s}</dd>\n", .{ev.facts.proof_level.toString()});
    if (baseline) |b| {
        try stdout.writeAll("<dt>Baseline</dt><dd><code>");
        try writeHtmlEscaped(stdout, b.contract_sha);
        try stdout.writeAll("</code></dd>\n");
    } else {
        try stdout.writeAll("<dt>Baseline</dt><dd><em>none (first entry)</em></dd>\n");
    }
    try stdout.writeAll("</dl>\n");

    try stdout.writeAll("<h2>Proven properties</h2><ul>\n");
    var any = false;
    inline for (review.property_metas) |meta| {
        if (@field(ev.facts.properties, meta.field)) {
            try stdout.print("<li>{s}</li>\n", .{meta.label});
            any = true;
        }
    }
    if (!any) try stdout.writeAll("<li><em>none</em></li>\n");
    try stdout.writeAll("</ul>\n");

    try writeHtmlSurfaceList(stdout, "Routes", ev.facts.routes);
    try writeHtmlStringList(stdout, "Env keys", ev.facts.env_keys);
    try writeHtmlStringList(stdout, "Egress hosts", ev.facts.egress_hosts);
    try writeHtmlStringList(stdout, "Cache namespaces", ev.facts.cache_namespaces);
    try writeHtmlStringList(stdout, "Capabilities", ev.facts.capabilities);

    if (baseline != null and !delta.isEmpty()) {
        try stdout.writeAll("<h2>Changes</h2><ul>\n");
        for (delta.added_routes) |r| try writeHtmlChange(stdout, "+ route", r.pattern);
        for (delta.removed_routes) |r| try writeHtmlChange(stdout, "- route", r.pattern);
        for (delta.added_env) |k| try writeHtmlChange(stdout, "+ env", k);
        for (delta.removed_env) |k| try writeHtmlChange(stdout, "- env", k);
        for (delta.added_egress) |h| try writeHtmlChange(stdout, "+ egress", h);
        for (delta.removed_egress) |h| try writeHtmlChange(stdout, "- egress", h);
        for (delta.added_cache) |n| try writeHtmlChange(stdout, "+ cache", n);
        for (delta.removed_cache) |n| try writeHtmlChange(stdout, "- cache", n);
        for (delta.added_capabilities) |c| try writeHtmlChange(stdout, "+ cap", c);
        for (delta.removed_capabilities) |c| try writeHtmlChange(stdout, "- cap", c);
        for (delta.promoted_properties) |p| try stdout.print("<li>+ property {s}</li>\n", .{p.label});
        for (delta.demoted_properties) |p| try stdout.print("<li>- property {s}</li>\n", .{p.label});
        if (delta.proof_level_change) |change| {
            try stdout.print("<li>proof level: {s} -> {s}</li>\n", .{ change.old.toString(), change.new.toString() });
        }
        try stdout.writeAll("</ul>\n");
    }
    try stdout.writeAll("</body></html>\n");
}

fn writeHtmlStringList(stdout: *std.Io.Writer, label: []const u8, items: []const []const u8) !void {
    if (items.len == 0) return;
    try stdout.print("<h2>{s}</h2><ul>\n", .{label});
    for (items) |item| {
        try stdout.writeAll("<li><code>");
        try writeHtmlEscaped(stdout, item);
        try stdout.writeAll("</code></li>\n");
    }
    try stdout.writeAll("</ul>\n");
}

fn writeHtmlSurfaceList(stdout: *std.Io.Writer, label: []const u8, routes: []const review.Route) !void {
    if (routes.len == 0) return;
    try stdout.print("<h2>{s}</h2><ul>\n", .{label});
    for (routes) |r| {
        try stdout.writeAll("<li><code>");
        try writeHtmlEscaped(stdout, r.pattern);
        try stdout.writeAll("</code>");
        if (r.is_prefix) try stdout.writeAll(" (prefix)");
        try stdout.writeAll("</li>\n");
    }
    try stdout.writeAll("</ul>\n");
}

fn writeHtmlChange(stdout: *std.Io.Writer, prefix: []const u8, value: []const u8) !void {
    try stdout.print("<li>{s} <code>", .{prefix});
    try writeHtmlEscaped(stdout, value);
    try stdout.writeAll("</code></li>\n");
}

/// Escape `<`, `>`, `&` for safe interpolation into HTML body / attribute
/// contexts. Contract-derived inputs are mostly hex / paths today, but cache
/// namespaces and route patterns have no charset guarantee.
fn writeHtmlEscaped(stdout: *std.Io.Writer, s: []const u8) !void {
    var start: usize = 0;
    for (s, 0..) |c, i| {
        const replacement: ?[]const u8 = switch (c) {
            '<' => "&lt;",
            '>' => "&gt;",
            '&' => "&amp;",
            else => null,
        };
        if (replacement) |rep| {
            if (i > start) try stdout.writeAll(s[start..i]);
            try stdout.writeAll(rep);
            start = i + 1;
        }
    }
    if (start < s.len) try stdout.writeAll(s[start..]);
}

fn renderSvg(
    stdout: *std.Io.Writer,
    facts: *const review.ReviewFacts,
    verdict: review.Verdict,
) !void {
    const fill = switch (verdict) {
        .safe => "#22c55e",
        .safe_with_additions => "#3b82f6",
        .breaking => "#ef4444",
    };

    // Up to three property labels for context. Order matches `property_metas`.
    var top_props: [3][]const u8 = .{ "", "", "" };
    var n: usize = 0;
    inline for (review.property_metas) |meta| {
        if (n < top_props.len and @field(facts.properties, meta.field)) {
            top_props[n] = meta.label;
            n += 1;
        }
    }

    try stdout.print(
        \\<svg xmlns="http://www.w3.org/2000/svg" width="320" height="30" role="img" aria-label="proven {s}">
        \\<rect width="320" height="30" rx="14" fill="{s}"/>
        \\<text x="160" y="20" font-family="-apple-system,Segoe UI,Roboto,sans-serif" font-size="13" fill="white" text-anchor="middle" font-weight="600">proven · {s}
    , .{ verdict.toString(), fill, verdict.toString() });
    if (n > 0) {
        try stdout.writeAll(" · ");
        for (top_props[0..n], 0..) |label, idx| {
            if (idx > 0) try stdout.writeAll(", ");
            try stdout.writeAll(label);
        }
    }
    try stdout.writeAll("</text>\n</svg>\n");
}

fn resolveRefOrExplain(
    events: []const proof_ledger.Event,
    text: []const u8,
    stderr: *std.Io.Writer,
) !usize {
    const ref = proof_ledger.parseRef(text) catch {
        try stderr.writeAll("Invalid ref. Expected HEAD, HEAD~N, or a contract sha prefix.\n");
        return error.InvalidRef;
    };
    return proof_ledger.resolve(events, ref) catch |err| {
        switch (err) {
            error.NoEvents => try stderr.writeAll("No proofs recorded yet.\n"),
            error.OutOfRange => try stderr.writeAll("Ref points before the start of the ledger.\n"),
            error.NotFound => try stderr.writeAll("No event matches that contract sha prefix.\n"),
            error.Ambiguous => try stderr.writeAll("Contract sha prefix is ambiguous; provide more characters.\n"),
        }
        return err;
    };
}

/// `card.current` borrows `ev.facts`; calling `DeployReview.deinit` would
/// double-free the event. The owned `delta` is freed below.
fn renderHistorical(
    allocator: std.mem.Allocator,
    ev: *const proof_ledger.Event,
    baseline: ?*const review.ReviewFacts,
    stdout: *std.Io.Writer,
) !void {
    var delta = try review.deriveDelta(allocator, &ev.facts, baseline);
    defer delta.deinit(allocator);

    var card = review.DeployReview{
        .handler_path = ev.handler_path,
        .service_name = ev.service_name orelse "(unrecorded)",
        .region = "(historical)",
        .current = ev.facts,
        .baseline = baseline,
        .delta = delta,
    };
    try review.renderReviewCard(allocator, &card, stdout, .{});
}

// ---------------------------------------------------------------------------
// Formatting helpers
// ---------------------------------------------------------------------------

/// Writes "YYYY-MM-DD HH:MM" UTC into `buf` (must be ≥16 bytes).
fn formatTimestampShort(ts_unix_ms: i64, buf: []u8) []const u8 {
    std.debug.assert(buf.len >= 16);
    const seconds_since_epoch: u64 = @intCast(@divFloor(ts_unix_ms, std.time.ms_per_s));
    const epoch_seconds = std.time.epoch.EpochSeconds{ .secs = seconds_since_epoch };
    const epoch_day = epoch_seconds.getEpochDay();
    const day_seconds = epoch_seconds.getDaySeconds();
    const year_day = epoch_day.calculateYearDay();
    const month_day = year_day.calculateMonthDay();

    const written = std.fmt.bufPrint(buf, "{d:0>4}-{d:0>2}-{d:0>2} {d:0>2}:{d:0>2}", .{
        year_day.year,
        @intFromEnum(month_day.month),
        @as(u16, month_day.day_index) + 1,
        day_seconds.getHoursIntoDay(),
        day_seconds.getMinutesIntoHour(),
    }) catch return buf[0..0];
    return written;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;
const chdirTmpForTest = proof_ledger.chdirTmpForTest;
const buildFactsForTest = proof_ledger.buildFactsForTest;

test "list: empty ledger prints help-style hint" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();

    try runWith(testing.allocator, &.{}, &out.writer, &err.writer);
    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "No proofs recorded yet") != null);
    try testing.expect(std.mem.indexOf(u8, text, ".zigttp/proofs.jsonl") != null);
}

test "list: tabulates two events with verdicts" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-aaaaaa", false);
    defer f1.deinit(testing.allocator);
    var f2 = try buildFactsForTest(testing.allocator, "sha-bbbbbb", true);
    defer f2.deinit(testing.allocator);

    try proof_ledger.appendEvent(testing.allocator, .{
        .kind = .deploy,
        .facts = &f1,
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .now_unix_ms = 1_700_000_000_000,
    });
    try proof_ledger.appendEvent(testing.allocator, .{
        .kind = .deploy,
        .facts = &f2,
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .now_unix_ms = 1_700_000_060_000,
    });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();
    try runWith(testing.allocator, &.{"list"}, &out.writer, &err.writer);

    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "Time") != null);
    try testing.expect(std.mem.indexOf(u8, text, "Kind") != null);
    try testing.expect(std.mem.indexOf(u8, text, "deploy") != null);
    try testing.expect(std.mem.indexOf(u8, text, "sha-aaaa") != null);
    try testing.expect(std.mem.indexOf(u8, text, "sha-bbbb") != null);
    // First row has no baseline -> safe; second adds a route -> safe_with_additions.
    try testing.expect(std.mem.indexOf(u8, text, "safe") != null);
    try testing.expect(std.mem.indexOf(u8, text, "safe_with_additions") != null);
    // Surface column for second row: 1 route, 0 env, 0 egress, 0 cache, 0 caps.
    try testing.expect(std.mem.indexOf(u8, text, "1r/1e/0h/0c/0cap") != null);
}

test "show HEAD: re-renders the latest card" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-old11", false);
    defer f1.deinit(testing.allocator);
    var f2 = try buildFactsForTest(testing.allocator, "sha-new22", true);
    defer f2.deinit(testing.allocator);

    try proof_ledger.appendEvent(testing.allocator, .{
        .kind = .deploy,
        .facts = &f1,
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .now_unix_ms = 1,
    });
    try proof_ledger.appendEvent(testing.allocator, .{
        .kind = .deploy,
        .facts = &f2,
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .now_unix_ms = 2,
    });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();
    try runWith(testing.allocator, &.{ "show", "HEAD" }, &out.writer, &err.writer);

    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "Proof review:") != null);
    try testing.expect(std.mem.indexOf(u8, text, "Handler:    src/handler.ts") != null);
    try testing.expect(std.mem.indexOf(u8, text, "Contract:   sha-new22") != null);
    try testing.expect(std.mem.indexOf(u8, text, "Baseline:   sha-old11") != null);
    try testing.expect(std.mem.indexOf(u8, text, "+ route /healthz") != null);
    try testing.expect(std.mem.indexOf(u8, text, "Verdict:    safe_with_additions") != null);
}

test "show with sha prefix: resolves and renders" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-aaa", false);
    defer f1.deinit(testing.allocator);
    var f2 = try buildFactsForTest(testing.allocator, "sha-bbb", false);
    defer f2.deinit(testing.allocator);

    try proof_ledger.appendEvent(testing.allocator, .{
        .kind = .deploy,
        .facts = &f1,
        .handler_path = "h.ts",
        .now_unix_ms = 1,
    });
    try proof_ledger.appendEvent(testing.allocator, .{
        .kind = .deploy,
        .facts = &f2,
        .handler_path = "h.ts",
        .now_unix_ms = 2,
    });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();
    try runWith(testing.allocator, &.{ "show", "sha-bbb" }, &out.writer, &err.writer);

    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "Contract:   sha-bbb") != null);
}

test "show without ref returns MissingRefArgument" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();

    try testing.expectError(
        error.MissingRefArgument,
        runWith(testing.allocator, &.{"show"}, &out.writer, &err.writer),
    );
    try testing.expect(std.mem.indexOf(u8, err.writer.buffered(), "requires a ref") != null);
}

test "help subcommand prints usage" {
    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();

    try runWith(testing.allocator, &.{"--help"}, &out.writer, &err.writer);
    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "Usage:") != null);
    try testing.expect(std.mem.indexOf(u8, text, "list") != null);
    try testing.expect(std.mem.indexOf(u8, text, "show <ref>") != null);
}

test "diff: renders b's card with a as baseline" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-baseline", false);
    defer f1.deinit(testing.allocator);
    var f2 = try buildFactsForTest(testing.allocator, "sha-current", true);
    defer f2.deinit(testing.allocator);

    try proof_ledger.appendEvent(testing.allocator, .{
        .kind = .deploy,
        .facts = &f1,
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .now_unix_ms = 1,
    });
    try proof_ledger.appendEvent(testing.allocator, .{
        .kind = .deploy,
        .facts = &f2,
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .now_unix_ms = 2,
    });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();

    try runWith(testing.allocator, &.{ "diff", "HEAD~1", "HEAD" }, &out.writer, &err.writer);
    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "Contract:   sha-current") != null);
    try testing.expect(std.mem.indexOf(u8, text, "Baseline:   sha-baseline") != null);
    try testing.expect(std.mem.indexOf(u8, text, "+ route /healthz") != null);
    try testing.expect(std.mem.indexOf(u8, text, "Verdict:    safe_with_additions") != null);
}

test "diff: reverse order surfaces removals" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-old", false);
    defer f1.deinit(testing.allocator);
    var f2 = try buildFactsForTest(testing.allocator, "sha-new", true);
    defer f2.deinit(testing.allocator);

    try proof_ledger.appendEvent(testing.allocator, .{
        .kind = .deploy,
        .facts = &f1,
        .handler_path = "src/handler.ts",
        .now_unix_ms = 1,
    });
    try proof_ledger.appendEvent(testing.allocator, .{
        .kind = .deploy,
        .facts = &f2,
        .handler_path = "src/handler.ts",
        .now_unix_ms = 2,
    });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();

    // Reverse order: a=newer, b=older. The route present in `a` is a removal in the diff.
    try runWith(testing.allocator, &.{ "diff", "HEAD", "HEAD~1" }, &out.writer, &err.writer);
    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "- route /healthz") != null);
    try testing.expect(std.mem.indexOf(u8, text, "Verdict:    breaking") != null);
}

test "diff: missing or extra args fail with explanation" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();

    try testing.expectError(
        error.MissingRefArgument,
        runWith(testing.allocator, &.{ "diff", "HEAD" }, &out.writer, &err.writer),
    );
    try testing.expect(std.mem.indexOf(u8, err.writer.buffered(), "requires two refs") != null);

    err.writer.end = 0;
    try testing.expectError(
        error.TooManyArguments,
        runWith(testing.allocator, &.{ "diff", "HEAD", "HEAD~1", "HEAD~2" }, &out.writer, &err.writer),
    );
    try testing.expect(std.mem.indexOf(u8, err.writer.buffered(), "exactly two refs") != null);
}

test "diff: identity diff against same ref is empty + safe" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-only", true);
    defer f1.deinit(testing.allocator);
    try proof_ledger.appendEvent(testing.allocator, .{
        .kind = .deploy,
        .facts = &f1,
        .handler_path = "src/handler.ts",
        .now_unix_ms = 1,
    });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();

    try runWith(testing.allocator, &.{ "diff", "HEAD", "HEAD" }, &out.writer, &err.writer);
    const text = out.writer.buffered();
    // Same event on both sides: contract == baseline, no add/remove bullets, verdict safe.
    try testing.expect(std.mem.indexOf(u8, text, "Contract:   sha-only") != null);
    try testing.expect(std.mem.indexOf(u8, text, "Baseline:   sha-only") != null);
    try testing.expect(std.mem.indexOf(u8, text, "Verdict:    safe") != null);
    try testing.expect(std.mem.indexOf(u8, text, "+ route") == null);
    try testing.expect(std.mem.indexOf(u8, text, "- route") == null);
}

test "show: too many refs returns TooManyArguments" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();

    try testing.expectError(
        error.TooManyArguments,
        runWith(testing.allocator, &.{ "show", "HEAD", "HEAD~1" }, &out.writer, &err.writer),
    );
    try testing.expect(std.mem.indexOf(u8, err.writer.buffered(), "exactly one ref") != null);
}

test "diff: bad ref reports through stderr" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-only", false);
    defer f1.deinit(testing.allocator);
    try proof_ledger.appendEvent(testing.allocator, .{
        .kind = .deploy,
        .facts = &f1,
        .handler_path = "h.ts",
        .now_unix_ms = 1,
    });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();

    try testing.expectError(
        error.NotFound,
        runWith(testing.allocator, &.{ "diff", "sha-only", "missing" }, &out.writer, &err.writer),
    );
    try testing.expect(std.mem.indexOf(u8, err.writer.buffered(), "No event matches") != null);
}

test "printRows: prints only events from start_idx onward" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-aaa", false);
    defer f1.deinit(testing.allocator);
    var f2 = try buildFactsForTest(testing.allocator, "sha-bbb", true);
    defer f2.deinit(testing.allocator);
    var f3 = try buildFactsForTest(testing.allocator, "sha-ccc", true);
    defer f3.deinit(testing.allocator);

    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f1, .handler_path = "h.ts", .now_unix_ms = 1 });
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .swap, .facts = &f2, .handler_path = "h.ts", .now_unix_ms = 2 });
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .check, .facts = &f3, .handler_path = "h.ts", .now_unix_ms = 3 });

    const events = try proof_ledger.readEvents(testing.allocator);
    defer proof_ledger.freeEvents(testing.allocator, events);

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();

    // start_idx=2 should print only the third event.
    try printRows(testing.allocator, &out.writer, events, 2);
    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "sha-ccc") != null);
    try testing.expect(std.mem.indexOf(u8, text, "sha-aaa") == null);
    try testing.expect(std.mem.indexOf(u8, text, "sha-bbb") == null);
    // Verdict for ccc vs bbb baseline: identical facts -> safe.
    try testing.expect(std.mem.indexOf(u8, text, "safe") != null);
    // Newline-terminated single row.
    var newline_count: usize = 0;
    for (text) |c| if (c == '\n') {
        newline_count += 1;
    };
    try testing.expectEqual(@as(usize, 1), newline_count);
}

test "printRows: start_idx=0 includes baseline=null for the first row" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-only", true);
    defer f1.deinit(testing.allocator);

    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f1, .handler_path = "h.ts", .now_unix_ms = 1 });

    const events = try proof_ledger.readEvents(testing.allocator);
    defer proof_ledger.freeEvents(testing.allocator, events);

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();

    try printRows(testing.allocator, &out.writer, events, 0);
    const text = out.writer.buffered();
    // First-row classification has no baseline -> verdict safe regardless of facts.
    try testing.expect(std.mem.indexOf(u8, text, "safe") != null);
    try testing.expect(std.mem.indexOf(u8, text, "sha-only") != null);
}

test "export: default md format renders verdict, properties, and changes" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-base", false);
    defer f1.deinit(testing.allocator);
    var f2 = try buildFactsForTest(testing.allocator, "sha-feat", true);
    defer f2.deinit(testing.allocator);

    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f1, .handler_path = "src/handler.ts", .service_name = "demo", .now_unix_ms = 1 });
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f2, .handler_path = "src/handler.ts", .service_name = "demo", .now_unix_ms = 2 });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();

    try runWith(testing.allocator, &.{"export"}, &out.writer, &err.writer);
    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "## Proof review") != null);
    try testing.expect(std.mem.indexOf(u8, text, "**Verdict**: `safe_with_additions`") != null);
    try testing.expect(std.mem.indexOf(u8, text, "**Handler**: `src/handler.ts`") != null);
    try testing.expect(std.mem.indexOf(u8, text, "**Contract**: `sha-feat`") != null);
    try testing.expect(std.mem.indexOf(u8, text, "**Baseline**: `sha-base`") != null);
    try testing.expect(std.mem.indexOf(u8, text, "### Proven properties") != null);
    try testing.expect(std.mem.indexOf(u8, text, "- read-only") != null);
    try testing.expect(std.mem.indexOf(u8, text, "### Changes") != null);
    try testing.expect(std.mem.indexOf(u8, text, "`+ route /healthz`") != null);
}

test "export: html format produces a self-contained document" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-only", false);
    defer f1.deinit(testing.allocator);
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f1, .handler_path = "h.ts", .now_unix_ms = 1 });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();
    try runWith(testing.allocator, &.{ "export", "--format", "html" }, &out.writer, &err.writer);

    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "<!DOCTYPE html>") != null);
    try testing.expect(std.mem.indexOf(u8, text, "class=\"verdict safe\"") != null);
    try testing.expect(std.mem.indexOf(u8, text, "<code>sha-only</code>") != null);
    try testing.expect(std.mem.indexOf(u8, text, "</body></html>") != null);
}

test "export: svg renders verdict pill with color" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-old", false);
    defer f1.deinit(testing.allocator);
    var f2 = try buildFactsForTest(testing.allocator, "sha-new", true);
    defer f2.deinit(testing.allocator);
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f1, .handler_path = "h.ts", .now_unix_ms = 1 });
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f2, .handler_path = "h.ts", .now_unix_ms = 2 });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();
    try runWith(testing.allocator, &.{ "export", "--format", "svg" }, &out.writer, &err.writer);

    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "<svg") != null);
    try testing.expect(std.mem.indexOf(u8, text, "#3b82f6") != null); // safe_with_additions blue
    try testing.expect(std.mem.indexOf(u8, text, "proven · safe_with_additions") != null);
    try testing.expect(std.mem.indexOf(u8, text, "</svg>") != null);
}

test "export: --ref selects a past entry" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-old", false);
    defer f1.deinit(testing.allocator);
    var f2 = try buildFactsForTest(testing.allocator, "sha-new", true);
    defer f2.deinit(testing.allocator);
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f1, .handler_path = "h.ts", .now_unix_ms = 1 });
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f2, .handler_path = "h.ts", .now_unix_ms = 2 });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();
    try runWith(testing.allocator, &.{ "export", "--ref", "HEAD~1", "--format", "md" }, &out.writer, &err.writer);

    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "**Contract**: `sha-old`") != null);
    try testing.expect(std.mem.indexOf(u8, text, "**Baseline**: _(none — first entry)_") != null);
}

test "export svg: safe verdict renders green" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-only", false);
    defer f1.deinit(testing.allocator);
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f1, .handler_path = "h.ts", .now_unix_ms = 1 });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();
    try runWith(testing.allocator, &.{ "export", "--format", "svg" }, &out.writer, &err.writer);
    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "#22c55e") != null);
    try testing.expect(std.mem.indexOf(u8, text, "proven · safe") != null);
}

test "export svg: breaking verdict renders red" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-with-route", true);
    defer f1.deinit(testing.allocator);
    var f2 = try buildFactsForTest(testing.allocator, "sha-no-route", false);
    defer f2.deinit(testing.allocator);
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f1, .handler_path = "h.ts", .now_unix_ms = 1 });
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f2, .handler_path = "h.ts", .now_unix_ms = 2 });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();
    try runWith(testing.allocator, &.{ "export", "--format", "svg" }, &out.writer, &err.writer);
    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "#ef4444") != null);
    try testing.expect(std.mem.indexOf(u8, text, "proven · breaking") != null);
}

test "export svg: zero proven properties omits trailer" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var bare = review.ReviewFacts{
        .contract_sha = try testing.allocator.dupe(u8, "sha-bare"),
        .proof_level = .none,
        .env_keys = try testing.allocator.alloc([]const u8, 0),
        .egress_hosts = try testing.allocator.alloc([]const u8, 0),
        .cache_namespaces = try testing.allocator.alloc([]const u8, 0),
        .routes = try testing.allocator.alloc(review.Route, 0),
        .capabilities = try testing.allocator.alloc([]const u8, 0),
        .properties = .{
            .injection_safe = false,
            .state_isolated = false,
            .no_secret_leakage = false,
            .no_credential_leakage = false,
            .input_validated = false,
            .pii_contained = false,
        },
    };
    defer bare.deinit(testing.allocator);
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &bare, .handler_path = "h.ts", .now_unix_ms = 1 });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();
    try runWith(testing.allocator, &.{ "export", "--format", "svg" }, &out.writer, &err.writer);

    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "proven · safe") != null);
    // No trailing " · " after the verdict word means no property list got concatenated.
    try testing.expect(std.mem.indexOf(u8, text, "proven · safe · ") == null);
}

test "export md: proof level change renders" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-old", false);
    defer f1.deinit(testing.allocator);
    var f2 = try buildFactsForTest(testing.allocator, "sha-new", false);
    f2.proof_level = .partial;
    defer f2.deinit(testing.allocator);
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f1, .handler_path = "h.ts", .now_unix_ms = 1 });
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f2, .handler_path = "h.ts", .now_unix_ms = 2 });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();
    try runWith(testing.allocator, &.{ "export", "--format", "md" }, &out.writer, &err.writer);
    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "proof level: complete -> partial") != null);
}

test "export html: proof level change renders" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var f1 = try buildFactsForTest(testing.allocator, "sha-old", false);
    defer f1.deinit(testing.allocator);
    var f2 = try buildFactsForTest(testing.allocator, "sha-new", false);
    f2.proof_level = .partial;
    defer f2.deinit(testing.allocator);
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f1, .handler_path = "h.ts", .now_unix_ms = 1 });
    try proof_ledger.appendEvent(testing.allocator, .{ .kind = .deploy, .facts = &f2, .handler_path = "h.ts", .now_unix_ms = 2 });

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();
    try runWith(testing.allocator, &.{ "export", "--format", "html" }, &out.writer, &err.writer);
    const text = out.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, text, "<li>proof level: complete -> partial</li>") != null);
}

test "writeHtmlEscaped: escapes <, >, &" {
    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    try writeHtmlEscaped(&out.writer, "a<b>c&d");
    try testing.expectEqualStrings("a&lt;b&gt;c&amp;d", out.writer.buffered());
}

test "writeHtmlEscaped: passes through strings without specials" {
    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    try writeHtmlEscaped(&out.writer, "src/handler.ts");
    try testing.expectEqualStrings("src/handler.ts", out.writer.buffered());
}

test "export: unknown format errors via stderr" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var out = std.Io.Writer.Allocating.init(testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(testing.allocator);
    defer err.deinit();

    try testing.expectError(
        error.InvalidFormat,
        runWith(testing.allocator, &.{ "export", "--format", "pdf" }, &out.writer, &err.writer),
    );
    try testing.expect(std.mem.indexOf(u8, err.writer.buffered(), "Unknown --format") != null);
}

test "formatTimestampShort: known epoch" {
    var buf: [20]u8 = undefined;
    // 2024-01-01T00:00:00Z = 1_704_067_200 s = 1_704_067_200_000 ms
    const out = formatTimestampShort(1_704_067_200_000, buf[0..]);
    try testing.expectEqualStrings("2024-01-01 00:00", out);
}
