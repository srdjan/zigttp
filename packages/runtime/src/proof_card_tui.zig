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
const review = @import("deploy/review.zig");
const audit_ring = @import("proof_audit_ring.zig");

pub const FrameOptions = struct {
    /// Total visible width in columns. Min 60, max 200.
    width: u16 = 100,
    /// Recompile elapsed time in ms, shown as a sub-line in the properties
    /// pane header. `null` means dev hasn't reported a recompile yet.
    recompile_ms: ?u32 = null,
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

/// Render a ProofCard as a three-pane TUI frame. Caller flushes the writer.
pub fn writeProofCardFrame(
    allocator: std.mem.Allocator,
    card: *const review.ProofCard,
    writer: *std.Io.Writer,
    opts: FrameOptions,
) !void {
    const widths = ColumnWidths.from(opts.width);

    var props = try buildPropertiesPane(allocator, card);
    defer freeLines(allocator, &props);
    var surface = try buildSurfacePane(allocator, card);
    defer freeLines(allocator, &surface);
    var requests = try buildRequestsPane(allocator);
    defer freeLines(allocator, &requests);

    try writeBorder(writer, widths);
    try writeRow(writer, widths, "Properties", "Surface", "Requests");
    if (opts.recompile_ms) |ms| {
        var buf: [32]u8 = undefined;
        const stamp = try std.fmt.bufPrint(&buf, "  {d}ms recompile", .{ms});
        try writeRow(writer, widths, stamp, "", "");
    }
    try writeBorder(writer, widths);

    const tall: usize = @max(@max(props.items.len, surface.items.len), requests.items.len);
    var i: usize = 0;
    while (i < tall) : (i += 1) {
        const p = if (i < props.items.len) props.items[i] else "";
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
    try writeStatusBar(writer, widths, card);
    try writeBorder(writer, widths);
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

    // Author-declared specs render directly under the inferred properties.
    // Empty when the handler has no `Spec<...>` annotation - the back-compat
    // path is invisible to keep the HUD quiet for handlers that haven't
    // opted in. Failing specs use [-]; passing specs use [*] to distinguish
    // them from the inferred property dots.
    if (card.current.declared_specs.len > 0) {
        try lines.append(allocator, try allocator.dupe(u8, ""));
        try lines.append(allocator, try allocator.dupe(u8, "Specs (declared)"));
        for (card.current.declared_specs) |s| {
            const glyph: []const u8 = if (s.discharged) "[*]" else "[-]";
            const line = try std.fmt.allocPrint(allocator, "{s} spec {s}", .{ glyph, s.name });
            try lines.append(allocator, line);
        }
    }
    return lines;
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
