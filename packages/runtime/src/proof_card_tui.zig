//! TUI frame writer for ProofCard.
//!
//! Renders a ProofCard as a static three-pane ASCII frame: properties grid,
//! surface tree, request audit. The frame does not move the cursor and does
//! not require raw mode, so callers can drive a live HUD by simply rendering
//! a fresh frame after each event (e.g. each recompile).
//!
//! Layout (default 100 cols):
//!
//!     +-------- Properties --------+----- Surface -----+--- Requests ---+
//!     | [+] retry-safe             | routes (2)        | (no requests)  |
//!     | [+] read-only              |   /healthz        |                |
//!     | ...                        |   /api (prefix)   |                |
//!     +----------------------------+-------------------+----------------+
//!     | Verdict: safe              | Contract: sha-... | Baseline: ...  |
//!     +----------------------------+-------------------+----------------+

const std = @import("std");
const review = @import("zigttp_deploy").review;
const audit_ring = @import("proof_audit_ring.zig");

/// Which view the left pane of the proof card renders. Only the left
/// pane and its header swap between lenses; the Surface and Requests
/// panes plus the status bar stay identical so frame geometry (and the
/// Studio SSE frame mirror) is stable across all four.
pub const Lens = enum {
    properties,
    trade,
    handover,
    caller_view,

    pub fn label(self: Lens) []const u8 {
        return switch (self) {
            .properties => "Properties",
            .trade => "Trade",
            .handover => "Handover",
            .caller_view => "Caller view",
        };
    }

    pub fn next(self: Lens) Lens {
        return switch (self) {
            .properties => .trade,
            .trade => .handover,
            .handover => .caller_view,
            .caller_view => .properties,
        };
    }
};

/// Per-build attestation snapshot the `.caller_view` lens renders.
/// Caller-owned strings; the render does not free them. Slice 2 item D.
pub const CallerView = struct {
    /// Value of the `Zigttp-Proofs` header, or empty/null when no chip is
    /// proven (the runtime omits the header line in that case).
    proofs_header_value: []const u8,
    /// Value of the `Zigttp-Attest` header (compact JWS).
    attest_header_value: []const u8,
    /// SHA-256 fingerprint of the public key, lowercase hex, 64 chars.
    key_fingerprint_hex: []const u8,
    /// Host the running server is bound to (typically `127.0.0.1` or `0.0.0.0`).
    host: []const u8,
    /// Port the running server is bound to.
    port: u16,
};

pub const FrameOptions = struct {
    /// Total visible width in columns. Min 60, max 200.
    width: u16 = 100,
    /// Recompile elapsed time in ms, shown as a sub-line in the properties
    /// pane header. `null` means dev hasn't reported a recompile yet.
    recompile_ms: ?u32 = null,
    /// Which lens to render in the left pane. Defaults to `.properties`
    /// so existing callers and tests are unaffected.
    lens: Lens = .properties,
    /// Caller-side view of what an external HTTP consumer sees. Required
    /// only when `lens == .caller_view`; null on other lenses renders an
    /// "(not attested; pass --no-attest off or rebuild)" message.
    caller_view: ?CallerView = null,
    /// Pre-rendered `proofTrace` JSON object from the analyzer. When present,
    /// a "Proof trace" block beneath the panes surfaces the reasoning behind
    /// every red proof. Null (the default) skips the block entirely.
    proof_trace_json: ?[]const u8 = null,
};

const ColumnWidths = struct {
    properties: u16,
    surface: u16,
    requests: u16,

    /// Three panes plus four border columns consume the total width. The
    /// content split is roughly 30/40/30 with the surface pane absorbing
    /// rounding so the totals always match.
    fn from(total_width: u16) ColumnWidths {
        const clamped: u16 = @max(60, @min(total_width, 200));
        const inside: u16 = clamped - 4;
        const props: u16 = @max(20, inside * 30 / 100);
        const reqs: u16 = @max(20, inside * 30 / 100);
        const surf: u16 = if (inside > props + reqs) inside - props - reqs else 30;
        return .{ .properties = props, .surface = surf, .requests = reqs };
    }
};

pub const StudioFooterOptions = struct {
    host: []const u8,
    port: u16,
    /// When true, render the footer with an extra emphasis attribute so the
    /// first render after `zigttp dev` boot stands out, then settles into
    /// neutral on subsequent renders.
    first_time: bool = false,
    /// When true, emit OSC 8 hyperlink escape sequences. Callers pass the
    /// stderr-is-a-tty result so non-TTY consumers (CI logs, piped output)
    /// see a plain URL.
    tty: bool = false,
};

/// Print the one-line "Studio mirror" footer beneath a proof card frame.
/// The footer is the polish-release affordance that wires the terminal HUD
/// and the browser Studio together: same proof card, two skins, one URL.
/// Renders as plain text (URL only) when `tty=false` so CI logs stay grep-friendly.
pub fn writeStudioFooter(
    writer: *std.Io.Writer,
    opts: StudioFooterOptions,
) !void {
    var url_buf: [256]u8 = undefined;
    const url = try std.fmt.bufPrint(
        &url_buf,
        "http://{s}:{d}/_zigttp/studio",
        .{ opts.host, opts.port },
    );

    if (!opts.tty) {
        try writer.print("  Studio mirror: {s}\n", .{url});
        return;
    }

    // ANSI control sequences. We compose them explicitly rather than via a
    // palette helper because the footer mixes hyperlink (OSC 8) and inline
    // emphasis (SGR) escapes; the existing palette only models SGR.
    const dim = "\x1b[2m";
    const bold = "\x1b[1m";
    const reset = "\x1b[0m";
    const link_open_prefix = "\x1b]8;;";
    const st = "\x1b\\";
    const link_close = "\x1b]8;;\x1b\\";

    const label_open: []const u8 = if (opts.first_time) bold else dim;
    try writer.print(
        "  {s}Studio mirror:{s} {s}{s}{s}{s}{s}\n",
        .{
            label_open,
            reset,
            link_open_prefix,
            url,
            st,
            url,
            link_close,
        },
    );
}

/// Render a ProofCard as a three-pane TUI frame. Caller flushes the writer.
pub fn writeProofCardFrame(
    allocator: std.mem.Allocator,
    card: *const review.ProofCard,
    writer: *std.Io.Writer,
    opts: FrameOptions,
) !void {
    const widths = ColumnWidths.from(opts.width);

    var left = try buildLeftPane(allocator, card, opts.lens, opts.caller_view);
    defer freeLines(allocator, &left);
    var surface = try buildSurfacePane(allocator, card);
    defer freeLines(allocator, &surface);
    var requests = try buildRequestsPane(allocator);
    defer freeLines(allocator, &requests);

    try writeBorder(writer, widths);
    try writeRow(writer, widths, opts.lens.label(), "Surface", "Requests");
    if (opts.recompile_ms) |ms| {
        var buf: [32]u8 = undefined;
        const stamp = try std.fmt.bufPrint(&buf, "  {d}ms recompile", .{ms});
        try writeRow(writer, widths, stamp, "", "");
    }
    try writeBorder(writer, widths);

    const tall: usize = @max(@max(left.items.len, surface.items.len), requests.items.len);
    var i: usize = 0;
    while (i < tall) : (i += 1) {
        const p = if (i < left.items.len) left.items[i] else "";
        const s = if (i < surface.items.len) surface.items[i] else "";
        const r = if (i < requests.items.len) requests.items[i] else "";
        try writeRow(writer, widths, p, s, r);
    }

    try writeBorder(writer, widths);
    if (try buildWhyRow(allocator, card)) |why_line| {
        defer allocator.free(why_line);
        try writeFullRow(writer, widths, why_line);
        try writeBorder(writer, widths);
    }
    if (card.counterexample) |cx| {
        try writeCounterexampleBlock(allocator, writer, widths, &cx);
        try writeBorder(writer, widths);
    }
    // The proof trace pairs with the Properties chips; render it only on
    // that lens. This also keeps the JSON parse off the other three lens
    // renders that `refreshLensCache` performs every recompile.
    if (opts.lens == .properties) {
        if (opts.proof_trace_json) |ptj| {
            if (try writeProofTraceBlock(allocator, writer, widths, ptj)) {
                try writeBorder(writer, widths);
            }
        }
    }
    try writeStatusBar(writer, widths, card);
    try writeBorder(writer, widths);
}

fn writeCounterexampleBlock(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    widths: ColumnWidths,
    cx: *const review.CounterexamplePreview,
) !void {
    try writeCounterexampleHeadline(allocator, writer, widths, cx);
    if (cx.suggestion) |hint| try writeWhyRow(allocator, writer, widths, hint);
    if (cx.failing_request) |req| try writeReplaySection(allocator, writer, widths, req, cx);
    try writeFullRow(
        writer,
        widths,
        "  [r] replay live   [s] pin as regression test   [a] ask expert to fix",
    );
}

fn writeCounterexampleHeadline(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    widths: ColumnWidths,
    cx: *const review.CounterexamplePreview,
) !void {
    const line = try std.fmt.allocPrint(
        allocator,
        "Counterexample: -{s} at {s}:{d}: {s}",
        .{ cx.label, cx.handler_path, cx.line, cx.snippet },
    );
    defer allocator.free(line);
    try writeFullRow(writer, widths, line);
}

fn writeWhyRow(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    widths: ColumnWidths,
    hint: []const u8,
) !void {
    const line = try std.fmt.allocPrint(allocator, "  why: {s}", .{hint});
    defer allocator.free(line);
    try writeFullRow(writer, widths, line);
}

fn writeReplaySection(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    widths: ColumnWidths,
    req: review.CounterexamplePreview.FailingRequest,
    cx: *const review.CounterexamplePreview,
) !void {
    const req_line = try std.fmt.allocPrint(
        allocator,
        "  request: {s} {s}",
        .{ req.method, req.url },
    );
    defer allocator.free(req_line);
    try writeFullRow(writer, widths, req_line);

    if (cx.previous_response) |prev| {
        const line = try formatReplayLine(allocator, "  previous build", prev);
        defer allocator.free(line);
        try writeFullRow(writer, widths, line);
    }
    if (cx.current_response) |curr| {
        const line = try formatReplayLine(allocator, "  this build    ", curr);
        defer allocator.free(line);
        try writeFullRow(writer, widths, line);
    }
}

fn formatReplayLine(
    allocator: std.mem.Allocator,
    prefix: []const u8,
    r: review.CounterexamplePreview.ReplayResponse,
) ![]u8 {
    if (r.error_text) |err| {
        return std.fmt.allocPrint(allocator, "{s}: crashed - {s}", .{ prefix, err });
    }
    return std.fmt.allocPrint(allocator, "{s}: {d}  {s}", .{ prefix, r.status, r.body });
}

/// Build the Why-on-regression row content for the live HUD: when a property
/// demoted between this prove cycle and the baseline AND the live-reload path
/// supplied a cause for that property, surface "Why: -<prop> at handler.ts:N: <snippet>".
/// Returns null when there is nothing to say (no demotions, no causes, or no
/// match between the two). Caller frees the returned slice.
fn buildWhyRow(
    allocator: std.mem.Allocator,
    card: *const review.ProofCard,
) !?[]u8 {
    if (card.property_causes.len == 0) return null;
    if (card.delta.demoted_properties.len == 0) return null;

    for (card.delta.demoted_properties) |demoted| {
        for (card.property_causes) |cause| {
            if (std.mem.eql(u8, cause.field, demoted.name)) {
                return try std.fmt.allocPrint(
                    allocator,
                    "Why: -{s} at {s}:{d}: {s}",
                    .{ demoted.label, card.handler_path, cause.line, cause.snippet },
                );
            }
        }
    }
    return null;
}

/// Render a single full-width row that spans every pane (no internal pipes).
/// Used for the Why-on-regression row, which is one logical message and reads
/// better unbroken than packed into the three-pane grid.
fn writeFullRow(
    writer: *std.Io.Writer,
    widths: ColumnWidths,
    content: []const u8,
) !void {
    const inside_width: usize =
        @as(usize, widths.properties) + @as(usize, widths.surface) + @as(usize, widths.requests) + 2;
    try writer.writeAll("|");
    const visible: usize = @min(content.len, inside_width);
    try writer.writeAll(content[0..visible]);
    try writer.splatByteAll(' ', inside_width - visible);
    try writer.writeAll("|\n");
}

/// Render the "Proof trace" block: for every headline property that did not
/// hold, one full-width row carrying the analyzer's reasoning. Surfaces the
/// counterexample behind each red chip without a keystroke. Returns true when
/// at least one row was written. `json` is the analyzer's `proofTrace` object;
/// a parse failure is non-fatal and simply skips the block.
fn writeProofTraceBlock(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    widths: ColumnWidths,
    json: []const u8,
) !bool {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, json, .{}) catch return false;
    defer parsed.deinit();
    const obj = switch (parsed.value) {
        .object => |o| o,
        else => return false,
    };

    var wrote_title = false;
    inline for (review.property_metas) |meta| {
        const e: ?std.json.ObjectMap = if (obj.get(meta.field)) |entry| switch (entry) {
            .object => |o| o,
            else => null,
        } else null;
        if (e) |fields| {
            const holds = if (fields.get("holds")) |h| switch (h) {
                .bool => |b| b,
                else => true,
            } else true;
            if (!holds) {
                if (!wrote_title) {
                    try writeFullRow(writer, widths, "Proof trace: how each red proof broke");
                    wrote_title = true;
                }
                const summary: []const u8 = if (fields.get("summary")) |s| switch (s) {
                    .string => |str| str,
                    else => "",
                } else "";
                const line = try std.fmt.allocPrint(allocator, "  -{s}: {s}", .{ meta.label, summary });
                defer allocator.free(line);
                try writeFullRow(writer, widths, line);
            }
        }
    }
    return wrote_title;
}

/// Render the verdict / contract sha / baseline sha row at the bottom of the
/// frame. Shas are clipped to 24 chars so the row fits the requests-pane
/// column width.
fn writeStatusBar(
    writer: *std.Io.Writer,
    widths: ColumnWidths,
    card: *const review.ProofCard,
) !void {
    const sha_max: usize = 24;
    const current_sha = card.current.contract_sha;
    var verdict_buf: [64]u8 = undefined;
    var contract_buf: [64]u8 = undefined;
    var baseline_buf: [64]u8 = undefined;

    const verdict_text = try std.fmt.bufPrint(
        &verdict_buf,
        "Verdict: {s}",
        .{card.verdict().toString()},
    );
    const contract_text = try std.fmt.bufPrint(
        &contract_buf,
        "Contract: {s}",
        .{current_sha[0..@min(current_sha.len, sha_max)]},
    );
    const baseline_text = if (card.baseline) |b| blk: {
        const sha = b.contract_sha;
        break :blk try std.fmt.bufPrint(
            &baseline_buf,
            "Baseline: {s}",
            .{sha[0..@min(sha.len, sha_max)]},
        );
    } else "Baseline: (first deploy)";

    try writeRow(writer, widths, verdict_text, contract_text, baseline_text);
}

// ---------------------------------------------------------------------------
// Pane builders
// ---------------------------------------------------------------------------

const LineList = std.ArrayList([]u8);

fn buildLeftPane(
    allocator: std.mem.Allocator,
    card: *const review.ProofCard,
    lens: Lens,
    caller_view: ?CallerView,
) !LineList {
    return switch (lens) {
        .properties => buildPropertiesPane(allocator, card),
        .trade => buildTradePane(allocator, card),
        .handover => buildHandoverPane(allocator, card),
        .caller_view => buildCallerViewPane(allocator, caller_view),
    };
}

fn buildPropertiesPane(
    allocator: std.mem.Allocator,
    card: *const review.ProofCard,
) !LineList {
    var lines: LineList = .empty;
    errdefer freeLines(allocator, &lines);

    inline for (review.property_metas) |meta| {
        const proven = @field(card.current.properties, meta.field);
        const glyph: []const u8 = if (proven) "[+]" else "[-]";
        const line = try std.fmt.allocPrint(allocator, "{s} {s}", .{ glyph, meta.label });
        try lines.append(allocator, line);
    }

    const proof_line = try std.fmt.allocPrint(
        allocator,
        "proof: {s}",
        .{card.current.proof_level.toString()},
    );
    try lines.append(allocator, proof_line);

    // Active specs render directly under the inferred properties. Failing
    // specs use [-]; passing specs use [*] to distinguish them from the
    // inferred property dots.
    if (card.current.declared_specs.len > 0) {
        try lines.append(allocator, try allocator.dupe(u8, ""));
        try lines.append(allocator, try allocator.dupe(u8, "Specs (active)"));
        for (card.current.declared_specs) |s| {
            const glyph: []const u8 = if (s.discharged) "[*]" else "[-]";
            const line = try std.fmt.allocPrint(allocator, "{s} spec {s}", .{ glyph, s.name });
            try lines.append(allocator, line);
        }
    }
    return lines;
}

/// Trade view: each proof property grouped with the restrictions that earned
/// it. Unproven properties render with `(not yet)` since the renderer cannot
/// dim text without assuming a TTY.
fn buildTradePane(
    allocator: std.mem.Allocator,
    card: *const review.ProofCard,
) !LineList {
    var lines: LineList = .empty;
    errdefer freeLines(allocator, &lines);

    inline for (review.proof_to_restrictions) |row| {
        // Find the label and current proven state from property_metas /
        // ReviewFacts. The inline-for keeps this branchless at codegen.
        const proven: bool = @field(card.current.properties, row.property_field);
        const label: []const u8 = blk: {
            inline for (review.property_metas) |meta| {
                if (std.mem.eql(u8, meta.field, row.property_field)) break :blk meta.label;
            }
            break :blk row.property_field;
        };

        const glyph: []const u8 = if (proven) "[+]" else "[-]";
        const suffix: []const u8 = if (proven) "" else " (not yet)";
        const header = try std.fmt.allocPrint(
            allocator,
            "{s} {s}{s}",
            .{ glyph, label, suffix },
        );
        try lines.append(allocator, header);

        if (row.restrictions.len == 0) {
            const earned = try std.fmt.allocPrint(allocator, "  earned: {s}", .{row.earned});
            try lines.append(allocator, earned);
        } else {
            const gave = try std.mem.join(allocator, ", ", row.restrictions);
            defer allocator.free(gave);
            const gave_line = try std.fmt.allocPrint(allocator, "  gave up: {s}", .{gave});
            try lines.append(allocator, gave_line);
            const earned_line = try std.fmt.allocPrint(allocator, "  earned : {s}", .{row.earned});
            try lines.append(allocator, earned_line);
        }
        try lines.append(allocator, try allocator.dupe(u8, ""));
    }
    return lines;
}

/// Handover view: a compact summary that fits the left pane. The
/// full-width certificate (Studio Copy button, `proofCertificate` JSON
/// field) lives in `buildProofCertificate`.
fn buildHandoverPane(
    allocator: std.mem.Allocator,
    card: *const review.ProofCard,
) !LineList {
    var lines: LineList = .empty;
    errdefer freeLines(allocator, &lines);

    const intro = [_][]const u8{
        "AI handover",
        "",
        "Substrate",
        "  zigttp - deterministic",
        "  TypeScript subset.",
        "  no exceptions, no shared",
        "  mutable state, no",
        "  nondeterminism, no eval.",
        "",
        "Proven for this handler",
    };
    for (intro) |line| try lines.append(allocator, try allocator.dupe(u8, line));

    var proven_count: usize = 0;
    inline for (review.property_metas) |meta| {
        if (@field(card.current.properties, meta.field)) {
            try lines.append(allocator, try std.fmt.allocPrint(allocator, "  [+] {s}", .{meta.label}));
            proven_count += 1;
        }
    }
    if (proven_count == 0) {
        try lines.append(allocator, try allocator.dupe(u8, "  (none yet)"));
    }

    try lines.append(allocator, try allocator.dupe(u8, ""));
    try lines.append(allocator, try std.fmt.allocPrint(
        allocator,
        "surface: {d} routes, {d} paths,",
        .{ card.current.routes.len, card.current.behavior_path_count },
    ));
    try lines.append(allocator, try std.fmt.allocPrint(
        allocator,
        "         {d} intents verified",
        .{card.current.intent_assertion_count},
    ));

    const outro = [_][]const u8{
        "",
        "Refactor contract",
        "  keep these proven.",
        "  compiler blocks any edit",
        "  that breaks them.",
        "",
        "Studio has the full text",
        "and a one-click copy.",
    };
    for (outro) |line| try lines.append(allocator, try allocator.dupe(u8, line));
    return lines;
}

/// Render the AI proof-certificate. Returned slice is owned by the caller.
///
/// Exposed publicly so `studio.zig` can serialise the exact same artifact
/// as the `proofCertificate` JSON field; the browser Copy button then
/// places this byte sequence on the clipboard, matching what `c` does in
/// the TUI.
pub fn buildProofCertificate(
    allocator: std.mem.Allocator,
    card: *const review.ProofCard,
) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    const w = &aw.writer;

    try w.writeAll("AI handover\n\n");
    try w.writeAll("Substrate\n");
    try w.writeAll("  zigttp - deterministic TypeScript subset.\n");
    try w.writeAll("  no exceptions, no shared mutable state, no nondeterminism,\n");
    try w.writeAll("  no eval, no implicit coercion. compiler proves on every save.\n\n");

    try w.writeAll("Proven for this handler\n");
    var proven_count: usize = 0;
    inline for (review.property_metas) |meta| {
        const proven: bool = @field(card.current.properties, meta.field);
        if (proven) {
            try w.print("  [+] {s}\n", .{meta.label});
            proven_count += 1;
        }
    }
    if (proven_count == 0) {
        try w.writeAll("  (no properties proven yet)\n");
    }

    try w.print(
        "  surface: {d} routes, {d} execution paths, {d} intents verified\n\n",
        .{
            card.current.routes.len,
            card.current.behavior_path_count,
            card.current.intent_assertion_count,
        },
    );

    if (card.current.declared_specs.len > 0) {
        try w.writeAll("Active specs\n");
        for (card.current.declared_specs) |s| {
            const glyph: []const u8 = if (s.discharged) "[*]" else "[-]";
            try w.print("  {s} {s}\n", .{ glyph, s.name });
        }
        try w.writeAll("\n");
    }

    try w.writeAll("Refactor contract\n");
    try w.writeAll("  keep these proven. compiler blocks any edit that breaks them.\n");

    return try allocator.dupe(u8, aw.writer.buffered());
}

/// Caller view: what an external HTTP consumer of this attested build sees.
/// Slice 2 item D. Renders four blocks in the left pane: live response-header
/// preview, the `/.well-known/zigttp-attest` URL, a copy-pasteable
/// `zigttp verify` command, and the public-key fingerprint plus pinning hint.
/// Long strings (the JWS, the proofs chip list, the fingerprint) are
/// truncated for the narrow left pane; the full values live in the well-known
/// doc and the response headers the caller actually consumes.
fn buildCallerViewPane(
    allocator: std.mem.Allocator,
    caller_view: ?CallerView,
) !LineList {
    var lines: LineList = .empty;
    errdefer freeLines(allocator, &lines);

    const view = caller_view orelse {
        try lines.append(allocator, try allocator.dupe(u8, "(not attested)"));
        try lines.append(allocator, try allocator.dupe(u8, ""));
        try lines.append(allocator, try allocator.dupe(u8, "This build emits no Zigttp-Proofs"));
        try lines.append(allocator, try allocator.dupe(u8, "or Zigttp-Attest headers. Rebuild"));
        try lines.append(allocator, try allocator.dupe(u8, "without --no-attest to opt back in."));
        return lines;
    };

    try lines.append(allocator, try allocator.dupe(u8, "Response headers"));
    if (view.proofs_header_value.len > 0) {
        try lines.append(allocator, try truncatedHeaderLine(allocator, "  Zigttp-Proofs: ", view.proofs_header_value));
    }
    try lines.append(allocator, try truncatedHeaderLine(allocator, "  Zigttp-Attest: ", view.attest_header_value));
    try lines.append(allocator, try allocator.dupe(u8, ""));

    try lines.append(allocator, try allocator.dupe(u8, "Well-known doc"));
    try lines.append(allocator, try std.fmt.allocPrint(
        allocator,
        "  http://{s}:{d}/.well-known/zigttp-attest",
        .{ view.host, view.port },
    ));
    try lines.append(allocator, try allocator.dupe(u8, ""));

    try lines.append(allocator, try allocator.dupe(u8, "Verify from anywhere"));
    try lines.append(allocator, try std.fmt.allocPrint(
        allocator,
        "  zigttp verify http://{s}:{d}/",
        .{ view.host, view.port },
    ));
    try lines.append(allocator, try allocator.dupe(u8, ""));

    try lines.append(allocator, try allocator.dupe(u8, "Key fingerprint"));
    const fp_short_len = @min(view.key_fingerprint_hex.len, 16);
    try lines.append(allocator, try std.fmt.allocPrint(
        allocator,
        "  {s}...",
        .{view.key_fingerprint_hex[0..fp_short_len]},
    ));
    try lines.append(allocator, try allocator.dupe(u8, "  pin: zigttp verify <url>"));
    try lines.append(allocator, try allocator.dupe(u8, "       --trust-key <full-hex>"));

    return lines;
}

/// Format `<prefix><value>` truncated so the formatted line fits the widest
/// left pane the frame produces. The frame caps `total_width` at 200, which
/// gives a 58-char left pane; with the 17-char prefix `"  Zigttp-Attest: "`
/// and the 3-char `...` suffix, 30 chars of value keeps the trailing dots
/// visible at the right edge of the pane. The well-known doc and the actual
/// response headers carry the full strings; this is the in-HUD preview.
fn truncatedHeaderLine(
    allocator: std.mem.Allocator,
    prefix: []const u8,
    value: []const u8,
) ![]u8 {
    const max_value: usize = 30;
    if (value.len <= max_value) {
        return std.fmt.allocPrint(allocator, "{s}{s}", .{ prefix, value });
    }
    return std.fmt.allocPrint(allocator, "{s}{s}...", .{ prefix, value[0..max_value] });
}

fn buildSurfacePane(
    allocator: std.mem.Allocator,
    card: *const review.ProofCard,
) !LineList {
    var lines: LineList = .empty;
    errdefer freeLines(allocator, &lines);

    // Routes carry the prefix/exact distinction, so they get their own block.
    try appendCountedHeading(allocator, &lines, "routes", card.current.routes.len);
    for (card.current.routes) |r| {
        const suffix: []const u8 = if (r.is_prefix) " (prefix)" else "";
        const line = try std.fmt.allocPrint(allocator, "  {s}{s}", .{ r.pattern, suffix });
        try lines.append(allocator, line);
    }

    try appendStringSection(allocator, &lines, "env", card.current.env_keys);
    try appendStringSection(allocator, &lines, "egress", card.current.egress_hosts);
    try appendStringSection(allocator, &lines, "caches", card.current.cache_namespaces);
    try appendStringSection(allocator, &lines, "capabilities", card.current.capabilities);

    return lines;
}

fn appendStringSection(
    allocator: std.mem.Allocator,
    lines: *LineList,
    name: []const u8,
    items: []const []const u8,
) !void {
    try appendCountedHeading(allocator, lines, name, items.len);
    for (items) |item| {
        const line = try std.fmt.allocPrint(allocator, "  {s}", .{item});
        try lines.append(allocator, line);
    }
}

fn buildRequestsPane(allocator: std.mem.Allocator) !LineList {
    var lines: LineList = .empty;
    errdefer freeLines(allocator, &lines);

    var snapshot_buf: [audit_ring.RING_CAPACITY]audit_ring.AuditEvent = undefined;
    const n = audit_ring.snapshot(&snapshot_buf);
    if (n == 0) {
        try lines.append(allocator, try allocator.dupe(u8, "(no requests yet)"));
        return lines;
    }

    // Render newest-first so the most recent activity sits at the top of the
    // pane the same way a tail view does. The ring's `snapshot` writes
    // chronologically (oldest -> newest), so iterate in reverse.
    var i: usize = n;
    while (i > 0) {
        i -= 1;
        const ev = snapshot_buf[i];
        const glyph: []const u8 = switch (ev.kind) {
            .cache_hit => "*",
            .route_blocked => "x",
        };
        const line = try std.fmt.allocPrint(
            allocator,
            "{s} {s} {s} {s}",
            .{ glyph, ev.kind.label(), ev.methodSlice(), ev.pathSlice() },
        );
        try lines.append(allocator, line);
    }
    return lines;
}

fn appendCountedHeading(
    allocator: std.mem.Allocator,
    lines: *LineList,
    name: []const u8,
    count: usize,
) !void {
    const line = try std.fmt.allocPrint(allocator, "{s} ({d})", .{ name, count });
    try lines.append(allocator, line);
}

fn freeLines(allocator: std.mem.Allocator, lines: *LineList) void {
    for (lines.items) |line| allocator.free(line);
    lines.deinit(allocator);
}

// ---------------------------------------------------------------------------
// Frame primitives
// ---------------------------------------------------------------------------

fn writeBorder(writer: *std.Io.Writer, widths: ColumnWidths) !void {
    try writer.writeAll("+");
    try writer.splatByteAll('-', widths.properties);
    try writer.writeAll("+");
    try writer.splatByteAll('-', widths.surface);
    try writer.writeAll("+");
    try writer.splatByteAll('-', widths.requests);
    try writer.writeAll("+\n");
}

fn writeRow(
    writer: *std.Io.Writer,
    widths: ColumnWidths,
    a: []const u8,
    b: []const u8,
    c: []const u8,
) !void {
    try writer.writeAll("|");
    try writePadded(writer, a, widths.properties);
    try writer.writeAll("|");
    try writePadded(writer, b, widths.surface);
    try writer.writeAll("|");
    try writePadded(writer, c, widths.requests);
    try writer.writeAll("|\n");
}

fn writePadded(writer: *std.Io.Writer, s: []const u8, width: u16) !void {
    const visible: usize = @min(s.len, width);
    try writer.writeAll(s[0..visible]);
    try writer.splatByteAll(' ', @as(usize, width) - visible);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const Route = review.Route;
const ReviewFacts = review.ReviewFacts;
const ProofCard = review.ProofCard;

fn buildTestFacts(allocator: std.mem.Allocator) !ReviewFacts {
    return ReviewFacts{
        .contract_sha = try allocator.dupe(u8, "sha-frame-test-12345678"),
        .proof_level = .complete,
        .env_keys = blk: {
            var arr = try allocator.alloc([]const u8, 2);
            arr[0] = try allocator.dupe(u8, "DATABASE_URL");
            arr[1] = try allocator.dupe(u8, "PORT");
            break :blk arr;
        },
        .egress_hosts = blk: {
            var arr = try allocator.alloc([]const u8, 1);
            arr[0] = try allocator.dupe(u8, "api.example.com");
            break :blk arr;
        },
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = blk: {
            var arr = try allocator.alloc(Route, 2);
            arr[0] = .{ .pattern = try allocator.dupe(u8, "/api"), .is_prefix = true };
            arr[1] = .{ .pattern = try allocator.dupe(u8, "/healthz"), .is_prefix = false };
            break :blk arr;
        },
        .capabilities = blk: {
            var arr = try allocator.alloc([]const u8, 1);
            arr[0] = try allocator.dupe(u8, "clock");
            break :blk arr;
        },
        .properties = .{ .read_only = true, .input_validated = true },
    };
}

test "writeProofCardFrame: structure and content" {
    const allocator = std.testing.allocator;
    var current = try buildTestFacts(allocator);
    defer current.deinit(allocator);

    var delta = try review.deriveDelta(allocator, &current, null);
    defer delta.deinit(allocator);

    const card = ProofCard{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "us-east",
        .current = &current,
        .baseline = null,
        .delta = &delta,
    };

    audit_ring.clear();
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try writeProofCardFrame(allocator, &card, &aw.writer, .{});
    const out = aw.writer.buffered();

    // Frame headers and panes are present.
    try std.testing.expect(std.mem.indexOf(u8, out, "Properties") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Surface") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Requests") != null);

    // Property glyphs reflect proven/not-proven flags.
    try std.testing.expect(std.mem.indexOf(u8, out, "[+] read-only") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "[-] retry-safe") != null);

    // Surface tree heading + entries.
    try std.testing.expect(std.mem.indexOf(u8, out, "routes (2)") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "/healthz") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "/api (prefix)") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "env (2)") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "DATABASE_URL") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "egress (1)") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "api.example.com") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "capabilities (1)") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "clock") != null);

    // Empty audit ring -> placeholder text.
    try std.testing.expect(std.mem.indexOf(u8, out, "(no requests yet)") != null);

    // Status bar shows verdict, truncated contract sha, baseline note.
    try std.testing.expect(std.mem.indexOf(u8, out, "Verdict: safe") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Contract: sha-frame-test-12345678") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Baseline: (first deploy)") != null);

    // Border characters are present.
    try std.testing.expect(std.mem.indexOf(u8, out, "+--") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "|") != null);
}

test "writeProofCardFrame: every row has identical visible width" {
    const allocator = std.testing.allocator;
    var current = try buildTestFacts(allocator);
    defer current.deinit(allocator);

    var delta = try review.deriveDelta(allocator, &current, null);
    defer delta.deinit(allocator);

    const card = ProofCard{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "us-east",
        .current = &current,
        .baseline = null,
        .delta = &delta,
    };

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try writeProofCardFrame(allocator, &card, &aw.writer, .{ .width = 100 });
    const out = aw.writer.buffered();

    var expected_width: ?usize = null;
    var iter = std.mem.splitScalar(u8, out, '\n');
    while (iter.next()) |line| {
        if (line.len == 0) continue;
        if (expected_width == null) expected_width = line.len;
        try std.testing.expectEqual(expected_width.?, line.len);
    }
    try std.testing.expectEqual(@as(usize, 100), expected_width.?);
}

test "writeProofCardFrame: proof trace block surfaces failing properties and keeps width" {
    const allocator = std.testing.allocator;
    var current = try buildTestFacts(allocator);
    defer current.deinit(allocator);

    var delta = try review.deriveDelta(allocator, &current, null);
    defer delta.deinit(allocator);

    const card = ProofCard{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "us-east",
        .current = &current,
        .baseline = null,
        .delta = &delta,
    };

    const trace_json =
        \\{"deterministic":{"holds":false,"kind":"structural","summary":"Date.now() reads a clock."},
        \\"read_only":{"holds":true,"kind":"structural","summary":"no writes"}}
    ;

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try writeProofCardFrame(allocator, &card, &aw.writer, .{ .width = 100, .proof_trace_json = trace_json });
    const out = aw.writer.buffered();

    // The failing property appears with its reasoning; the passing one does not.
    try std.testing.expect(std.mem.indexOf(u8, out, "Proof trace: how each red proof broke") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Date.now() reads a clock.") != null);

    // Every rendered row still has identical visible width.
    var iter = std.mem.splitScalar(u8, out, '\n');
    while (iter.next()) |line| {
        if (line.len == 0) continue;
        try std.testing.expectEqual(@as(usize, 100), line.len);
    }
}

test "writeProofCardFrame: requests pane renders audit events newest-first" {
    const allocator = std.testing.allocator;
    var current = try buildTestFacts(allocator);
    defer current.deinit(allocator);

    var delta = try review.deriveDelta(allocator, &current, null);
    defer delta.deinit(allocator);

    const card = ProofCard{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "us-east",
        .current = &current,
        .baseline = null,
        .delta = &delta,
    };

    audit_ring.clear();
    audit_ring.pushCacheHit("GET", "/healthz");
    audit_ring.pushRouteBlocked("POST", "/admin");

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try writeProofCardFrame(allocator, &card, &aw.writer, .{});
    const out = aw.writer.buffered();

    // Both events render in the pane.
    try std.testing.expect(std.mem.indexOf(u8, out, "cache hit GET /healthz") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "route blocked POST /admin") != null);

    // Newest event renders first within the pane: route_blocked must precede
    // cache_hit in the output bytes.
    const route_idx = std.mem.indexOf(u8, out, "route blocked").?;
    const cache_idx = std.mem.indexOf(u8, out, "cache hit").?;
    try std.testing.expect(route_idx < cache_idx);

    // Placeholder is gone once the ring has events.
    try std.testing.expect(std.mem.indexOf(u8, out, "(no requests yet)") == null);

    audit_ring.clear();
}

test "writeProofCardFrame: recompile_ms surfaces in properties header" {
    const allocator = std.testing.allocator;
    var current = try buildTestFacts(allocator);
    defer current.deinit(allocator);

    var delta = try review.deriveDelta(allocator, &current, null);
    defer delta.deinit(allocator);

    const card = ProofCard{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "us-east",
        .current = &current,
        .baseline = null,
        .delta = &delta,
    };

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try writeProofCardFrame(allocator, &card, &aw.writer, .{ .recompile_ms = 145 });
    const out = aw.writer.buffered();

    try std.testing.expect(std.mem.indexOf(u8, out, "145ms recompile") != null);
}

test "writeProofCardFrame: renders Why row when a demoted property has a cause" {
    const allocator = std.testing.allocator;

    // Baseline: deterministic = true (proven). Current: deterministic = false
    // (regressed). Everything else stays equal so the only delta entry is the
    // deterministic demotion.
    var current = try buildTestFacts(allocator);
    defer current.deinit(allocator);
    var baseline = try buildTestFacts(allocator);
    defer baseline.deinit(allocator);
    current.properties.deterministic = false;
    baseline.properties.deterministic = true;

    var delta = try review.deriveDelta(allocator, &current, &baseline);
    defer delta.deinit(allocator);

    const causes = [_]review.PropertyCauseEntry{.{
        .field = "deterministic",
        .line = 14,
        .column = 9,
        .snippet = "Date.now()",
    }};

    const card = ProofCard{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "local",
        .current = &current,
        .baseline = &baseline,
        .delta = &delta,
        .property_causes = causes[0..],
    };

    audit_ring.clear();
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try writeProofCardFrame(allocator, &card, &aw.writer, .{});
    const out = aw.writer.buffered();

    try std.testing.expect(std.mem.indexOf(u8, out, "Why: -deterministic at src/handler.ts:14: Date.now()") != null);
}

test "writeProofCardFrame: no Why row when there are no demotions" {
    const allocator = std.testing.allocator;
    var current = try buildTestFacts(allocator);
    defer current.deinit(allocator);

    // No baseline -> empty delta -> no demotions.
    var delta = try review.deriveDelta(allocator, &current, null);
    defer delta.deinit(allocator);

    const causes = [_]review.PropertyCauseEntry{.{
        .field = "deterministic",
        .line = 14,
        .column = 9,
        .snippet = "Date.now()",
    }};

    const card = ProofCard{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "local",
        .current = &current,
        .baseline = null,
        .delta = &delta,
        .property_causes = causes[0..],
    };

    audit_ring.clear();
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try writeProofCardFrame(allocator, &card, &aw.writer, .{});
    const out = aw.writer.buffered();

    try std.testing.expect(std.mem.indexOf(u8, out, "Why:") == null);
}

test "writeProofCardFrame: demotion without a matching cause renders no Why row" {
    const allocator = std.testing.allocator;

    var current = try buildTestFacts(allocator);
    defer current.deinit(allocator);
    var baseline = try buildTestFacts(allocator);
    defer baseline.deinit(allocator);
    current.properties.deterministic = false;
    baseline.properties.deterministic = true;

    var delta = try review.deriveDelta(allocator, &current, &baseline);
    defer delta.deinit(allocator);

    const card = ProofCard{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "local",
        .current = &current,
        .baseline = &baseline,
        .delta = &delta,
        // property_causes intentionally empty.
    };

    audit_ring.clear();
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try writeProofCardFrame(allocator, &card, &aw.writer, .{});
    const out = aw.writer.buffered();

    try std.testing.expect(std.mem.indexOf(u8, out, "Why:") == null);
}

const TestDemote = enum { none, deterministic, input_validated };

/// Build a ProofCard around a freshly-derived demotion delta and render it,
/// returning the buffered TUI output for substring assertions. Passing
/// `.none` produces a clean first-deploy frame with no baseline.
fn renderForTest(
    allocator: std.mem.Allocator,
    demote: TestDemote,
    cx: ?review.CounterexamplePreview,
    out_aw: *std.Io.Writer.Allocating,
) !void {
    var current = try buildTestFacts(allocator);
    defer current.deinit(allocator);
    var baseline = try buildTestFacts(allocator);
    defer baseline.deinit(allocator);

    switch (demote) {
        .none => {},
        .deterministic => {
            current.properties.deterministic = false;
            baseline.properties.deterministic = true;
        },
        .input_validated => {
            current.properties.input_validated = false;
            baseline.properties.input_validated = true;
        },
    }

    const baseline_ptr: ?*const review.ReviewFacts =
        if (demote == .none) null else &baseline;

    var delta = try review.deriveDelta(allocator, &current, baseline_ptr);
    defer delta.deinit(allocator);

    const card = ProofCard{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "local",
        .current = &current,
        .baseline = baseline_ptr,
        .delta = &delta,
        .counterexample = cx,
    };

    audit_ring.clear();
    try writeProofCardFrame(allocator, &card, &out_aw.writer, .{});
}

test "writeProofCardFrame: renders Counterexample block with suggestion and key hints" {
    const allocator = std.testing.allocator;
    const cx = review.CounterexamplePreview{
        .field = "deterministic",
        .label = "deterministic",
        .line = 14,
        .column = 9,
        .snippet = "Date.now()",
        .handler_path = "src/handler.ts",
        .suggestion = "remove Date.now() / Math.random() or move the call inside a `durable.step`.",
    };

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try renderForTest(allocator, .deterministic, cx, &aw);
    const out = aw.writer.buffered();

    try std.testing.expect(std.mem.indexOf(u8, out, "Counterexample: -deterministic at src/handler.ts:14: Date.now()") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "why: remove Date.now()") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "[r] replay live") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "[s] pin as regression test") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "[a] ask expert to fix") != null);
}

test "writeProofCardFrame: omits Counterexample block when card has none" {
    const allocator = std.testing.allocator;
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try renderForTest(allocator, .none, null, &aw);
    try std.testing.expect(std.mem.indexOf(u8, aw.writer.buffered(), "Counterexample:") == null);
}

test "writeStudioFooter: plain URL on non-TTY" {
    const allocator = std.testing.allocator;
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try writeStudioFooter(&aw.writer, .{ .host = "127.0.0.1", .port = 3000, .tty = false });
    const out = aw.writer.buffered();
    try std.testing.expectEqualStrings("  Studio mirror: http://127.0.0.1:3000/_zigttp/studio\n", out);
}

test "writeStudioFooter: TTY emits OSC 8 hyperlink with URL as both target and visible text" {
    const allocator = std.testing.allocator;
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try writeStudioFooter(&aw.writer, .{ .host = "localhost", .port = 8787, .tty = true });
    const out = aw.writer.buffered();
    // Hyperlink open escape with target URL.
    try std.testing.expect(std.mem.indexOf(u8, out, "\x1b]8;;http://localhost:8787/_zigttp/studio\x1b\\") != null);
    // Hyperlink close.
    try std.testing.expect(std.mem.indexOf(u8, out, "\x1b]8;;\x1b\\") != null);
    // Same URL appears as the visible link text.
    try std.testing.expect(std.mem.indexOf(u8, out, "http://localhost:8787/_zigttp/studio") != null);
}

test "writeStudioFooter: first_time uses bold emphasis instead of dim" {
    const allocator = std.testing.allocator;
    var aw_first = std.Io.Writer.Allocating.init(allocator);
    defer aw_first.deinit();
    try writeStudioFooter(&aw_first.writer, .{ .host = "127.0.0.1", .port = 3000, .tty = true, .first_time = true });
    try std.testing.expect(std.mem.indexOf(u8, aw_first.writer.buffered(), "\x1b[1m") != null);

    var aw_neutral = std.Io.Writer.Allocating.init(allocator);
    defer aw_neutral.deinit();
    try writeStudioFooter(&aw_neutral.writer, .{ .host = "127.0.0.1", .port = 3000, .tty = true, .first_time = false });
    try std.testing.expect(std.mem.indexOf(u8, aw_neutral.writer.buffered(), "\x1b[2m") != null);
}

test "writeProofCardFrame: Counterexample block includes failing request and replay diff when present" {
    const allocator = std.testing.allocator;
    const cx = review.CounterexamplePreview{
        .field = "input_validated",
        .label = "input-validated",
        .line = 18,
        .column = 12,
        .snippet = ".length",
        .handler_path = "src/handler.ts",
        .suggestion = "guard with `?.` or validate the input shape with zigttp:validate.",
        .failing_request = .{ .method = "POST", .url = "/api/users", .body = "{\"name\":null}" },
        .previous_response = .{ .status = 201, .body = "{\"id\":\"u_abc\"}" },
        .current_response = .{ .status = 500, .body = "", .error_text = "cannot read length of undefined" },
    };

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try renderForTest(allocator, .input_validated, cx, &aw);
    const out = aw.writer.buffered();

    try std.testing.expect(std.mem.indexOf(u8, out, "request: POST /api/users") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "previous build: 201  {\"id\":\"u_abc\"}") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "this build    : crashed - cannot read length of undefined") != null);
}

// ---------------------------------------------------------------------------
// Proof Lens tests
// ---------------------------------------------------------------------------

/// Render a frame with a specific lens around the standard test facts and
/// return the buffered output. Tests assert on substrings.
fn renderLensForTest(
    allocator: std.mem.Allocator,
    lens: Lens,
    out_aw: *std.Io.Writer.Allocating,
) !void {
    var current = try buildTestFacts(allocator);
    defer current.deinit(allocator);

    var delta = try review.deriveDelta(allocator, &current, null);
    defer delta.deinit(allocator);

    const card = ProofCard{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "local",
        .current = &current,
        .baseline = null,
        .delta = &delta,
    };

    audit_ring.clear();
    try writeProofCardFrame(allocator, &card, &out_aw.writer, .{ .lens = lens });
}

test "writeProofCardFrame: trade lens groups restrictions by property" {
    const allocator = std.testing.allocator;
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try renderLensForTest(allocator, .trade, &aw);
    const out = aw.writer.buffered();

    // Header label switches to "Trade".
    try std.testing.expect(std.mem.indexOf(u8, out, "|Trade") != null);
    // A property earned by restrictions appears with its trade.
    try std.testing.expect(std.mem.indexOf(u8, out, "deterministic") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "gave up:") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "try/catch") != null);
    // A property earned by analysis (no restrictions) uses the "earned:" line.
    try std.testing.expect(std.mem.indexOf(u8, out, "earned:") != null);
}

test "writeProofCardFrame: handover lens emits narrow certificate summary" {
    const allocator = std.testing.allocator;
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try renderLensForTest(allocator, .handover, &aw);
    const out = aw.writer.buffered();

    // Header label switches to Handover; the narrow summary lives in the
    // left pane and points at Studio for the full text.
    try std.testing.expect(std.mem.indexOf(u8, out, "|Handover") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "AI handover") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Substrate") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Proven for this handler") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Refactor contract") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Studio has the full text") != null);
    // buildTestFacts proves read_only and input_validated; both must appear.
    try std.testing.expect(std.mem.indexOf(u8, out, "[+] read-only") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "[+] input-validated") != null);
}

test "writeProofCardFrame: all three lenses produce identical frame width" {
    const allocator = std.testing.allocator;

    const lenses = [_]Lens{ .properties, .trade, .handover };
    var widths: [3]usize = undefined;
    for (lenses, 0..) |lens, idx| {
        var aw = std.Io.Writer.Allocating.init(allocator);
        defer aw.deinit();
        try renderLensForTest(allocator, lens, &aw);
        const out = aw.writer.buffered();

        var iter = std.mem.splitScalar(u8, out, '\n');
        var max_w: usize = 0;
        while (iter.next()) |line| {
            if (line.len == 0) continue;
            if (max_w == 0) max_w = line.len;
            try std.testing.expectEqual(max_w, line.len);
        }
        widths[idx] = max_w;
    }
    try std.testing.expectEqual(widths[0], widths[1]);
    try std.testing.expectEqual(widths[1], widths[2]);
}

test "buildProofCertificate emits the substrate paragraph and surface counts" {
    const allocator = std.testing.allocator;
    var current = try buildTestFacts(allocator);
    defer current.deinit(allocator);
    current.behavior_path_count = 7;
    current.intent_assertion_count = 2;

    var delta = try review.deriveDelta(allocator, &current, null);
    defer delta.deinit(allocator);

    const card = ProofCard{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "local",
        .current = &current,
        .baseline = null,
        .delta = &delta,
    };

    const cert = try buildProofCertificate(allocator, &card);
    defer allocator.free(cert);

    try std.testing.expect(std.mem.indexOf(u8, cert, "AI handover") != null);
    try std.testing.expect(std.mem.indexOf(u8, cert, "Substrate") != null);
    try std.testing.expect(std.mem.indexOf(u8, cert, "Proven for this handler") != null);
    try std.testing.expect(std.mem.indexOf(u8, cert, "2 routes, 7 execution paths, 2 intents verified") != null);
    try std.testing.expect(std.mem.indexOf(u8, cert, "Refactor contract") != null);
}

test "Lens.next cycles through all four variants" {
    try std.testing.expectEqual(Lens.trade, Lens.properties.next());
    try std.testing.expectEqual(Lens.handover, Lens.trade.next());
    try std.testing.expectEqual(Lens.caller_view, Lens.handover.next());
    try std.testing.expectEqual(Lens.properties, Lens.caller_view.next());
}

test "Lens.label reports human-readable text per variant" {
    try std.testing.expectEqualStrings("Caller view", Lens.caller_view.label());
}

test "caller_view lens with null caller_view renders the opt-out notice" {
    const allocator = std.testing.allocator;
    var current = try buildTestFacts(allocator);
    defer current.deinit(allocator);
    var delta = try review.deriveDelta(allocator, &current, null);
    defer delta.deinit(allocator);

    const card = ProofCard{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "local",
        .current = &current,
        .baseline = null,
        .delta = &delta,
    };

    audit_ring.clear();
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try writeProofCardFrame(allocator, &card, &aw.writer, .{ .lens = .caller_view });
    const out = aw.writer.buffered();

    try std.testing.expect(std.mem.indexOf(u8, out, "Caller view") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "(not attested)") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "--no-attest") != null);
}

test "caller_view lens with attestation context renders the four blocks" {
    const allocator = std.testing.allocator;
    var current = try buildTestFacts(allocator);
    defer current.deinit(allocator);
    var delta = try review.deriveDelta(allocator, &current, null);
    defer delta.deinit(allocator);

    const card = ProofCard{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "local",
        .current = &current,
        .baseline = null,
        .delta = &delta,
    };

    const view = CallerView{
        .proofs_header_value = "pure, read_only, injection_safe",
        .attest_header_value = "eyJhbGciOiJFZERTQSJ9.eyJ2IjoiMSJ9.SIG",
        .key_fingerprint_hex = "abcdef0123456789" ++ ("0" ** 48),
        .host = "127.0.0.1",
        .port = 3000,
    };

    audit_ring.clear();
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    // Wider frame so the left pane fits the long Zigttp-Attest line and the
    // well-known URL without `writePadded` clipping the substring assertions.
    try writeProofCardFrame(allocator, &card, &aw.writer, .{ .width = 200, .lens = .caller_view, .caller_view = view });
    const out = aw.writer.buffered();

    try std.testing.expect(std.mem.indexOf(u8, out, "Caller view") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Response headers") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Zigttp-Proofs: pure, read_only") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Zigttp-Attest: eyJh") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Well-known doc") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "http://127.0.0.1:3000/.well-known/zigttp-attest") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Verify from anywhere") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "zigttp verify http://127.0.0.1:3000/") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Key fingerprint") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "abcdef0123456789...") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "--trust-key") != null);
}

test "caller_view truncates long header values for the narrow left pane" {
    const allocator = std.testing.allocator;
    var current = try buildTestFacts(allocator);
    defer current.deinit(allocator);
    var delta = try review.deriveDelta(allocator, &current, null);
    defer delta.deinit(allocator);

    const card = ProofCard{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "local",
        .current = &current,
        .baseline = null,
        .delta = &delta,
    };

    // 100-char JWS forces truncation.
    const long_jws = "X" ** 100;
    const view = CallerView{
        .proofs_header_value = "pure",
        .attest_header_value = long_jws,
        .key_fingerprint_hex = "f" ** 64,
        .host = "127.0.0.1",
        .port = 3000,
    };

    audit_ring.clear();
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try writeProofCardFrame(allocator, &card, &aw.writer, .{ .width = 200, .lens = .caller_view, .caller_view = view });
    const out = aw.writer.buffered();

    // Truncated form ends in "..."; original full string must not appear.
    try std.testing.expect(std.mem.indexOf(u8, out, "Zigttp-Attest: " ++ ("X" ** 30) ++ "...") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "X" ** 40) == null);
}
