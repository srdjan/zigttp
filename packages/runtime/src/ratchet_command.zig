//! `zigttp ratchet` — cross-build monotonicity gate for proven property sets.
//!
//! Slice 4 of the attest program. The compiler already infers and signs
//! every handler's proven-property set (`provenSpecs` in contract.json,
//! covered by the JWS payload). What was missing was a temporal axis:
//! comparing two builds and failing CI when the set weakens.
//!
//! Subcommands:
//!   show  <handler.ts>                              print provenSpecs
//!   check --baseline <old.json> <handler.ts>        diff; exit 1 on regression
//!
//! Waiver signing (a signed file under `.zigttp/waivers/` that lets the
//! operator accept a regression with a recorded reason) lands in a
//! follow-up. The current `check` exits 1 on any weakening; that is the
//! mechanically-correct default for a ratchet.

const std = @import("std");
const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const precompile = zigts_cli.precompile;
const HandlerProperties = zigts.handler_contract.HandlerProperties;

/// Source-file size cap for `ratchet show`/`check`. Handlers are
/// authored, not generated, so 10 MB is generous; this keeps the limit
/// explicit at the call site instead of borrowing the misleading
/// `default_output_limit` constant from the pi tools.
const handler_source_limit: usize = 10 * 1024 * 1024;

pub const RatchetError = error{
    UnknownSubcommand,
    MissingArgument,
    MissingBaseline,
    HandlerCompileFailed,
    Regression,
} || std.mem.Allocator.Error;

const Subcommand = enum { show, check, help };

fn parseSub(name: []const u8) ?Subcommand {
    if (std.mem.eql(u8, name, "show")) return .show;
    if (std.mem.eql(u8, name, "check")) return .check;
    if (std.mem.eql(u8, name, "help") or std.mem.eql(u8, name, "--help") or std.mem.eql(u8, name, "-h")) return .help;
    return null;
}

pub fn run(allocator: std.mem.Allocator, argv: []const []const u8) RatchetError!void {
    if (argv.len == 0) {
        printHelp();
        return error.MissingArgument;
    }
    const sub = parseSub(argv[0]) orelse {
        std.debug.print("zigttp ratchet: unknown subcommand `{s}`\n\n", .{argv[0]});
        printHelp();
        return error.UnknownSubcommand;
    };
    switch (sub) {
        .show => try runShow(allocator, argv[1..]),
        .check => try runCheck(allocator, argv[1..]),
        .help => printHelp(),
    }
}

fn runShow(allocator: std.mem.Allocator, argv: []const []const u8) RatchetError!void {
    if (argv.len == 0) {
        std.debug.print("zigttp ratchet show: handler path required\n", .{});
        return error.MissingArgument;
    }
    const handler_path = argv[0];

    var proven = try collectProvenSet(allocator, handler_path);
    defer proven.deinit(allocator);

    std.debug.print("proven specs for {s}:\n", .{handler_path});
    if (proven.names.items.len == 0) {
        std.debug.print("  (none — compile failed or no properties proven)\n", .{});
        return;
    }
    for (proven.names.items) |name| {
        std.debug.print("  - {s}\n", .{name});
    }
}

fn runCheck(allocator: std.mem.Allocator, argv: []const []const u8) RatchetError!void {
    var baseline_path: ?[]const u8 = null;
    var handler_path: ?[]const u8 = null;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--baseline")) {
            i += 1;
            if (i >= argv.len) {
                std.debug.print("--baseline requires a path\n", .{});
                return error.MissingArgument;
            }
            baseline_path = argv[i];
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            handler_path = arg;
        }
    }

    const handler = handler_path orelse {
        std.debug.print("zigttp ratchet check: handler path required\n", .{});
        return error.MissingArgument;
    };
    const baseline = baseline_path orelse {
        std.debug.print("zigttp ratchet check: --baseline <contract.json> required\n", .{});
        return error.MissingBaseline;
    };

    var current = try collectProvenSet(allocator, handler);
    defer current.deinit(allocator);

    var prior = try loadBaseline(allocator, baseline);
    defer prior.deinit(allocator);

    var dropped = std.ArrayListUnmanaged([]const u8).empty;
    defer dropped.deinit(allocator);
    var gained = std.ArrayListUnmanaged([]const u8).empty;
    defer gained.deinit(allocator);

    for (prior.names.items) |old_name| {
        if (!containsName(current.names.items, old_name)) {
            try dropped.append(allocator, old_name);
        }
    }
    for (current.names.items) |new_name| {
        if (!containsName(prior.names.items, new_name)) {
            try gained.append(allocator, new_name);
        }
    }

    std.debug.print("ratchet check for {s} vs baseline {s}:\n", .{ handler, baseline });
    if (gained.items.len > 0) {
        std.debug.print("  + gained: ", .{});
        for (gained.items, 0..) |n, idx| {
            if (idx > 0) std.debug.print(", ", .{});
            std.debug.print("{s}", .{n});
        }
        std.debug.print("\n", .{});
    }
    if (dropped.items.len > 0) {
        std.debug.print("  - lost:   ", .{});
        for (dropped.items, 0..) |n, idx| {
            if (idx > 0) std.debug.print(", ", .{});
            std.debug.print("{s}", .{n});
        }
        std.debug.print("\n", .{});
        std.debug.print("\nratchet weakened — failing.\n", .{});
        return error.Regression;
    }
    if (gained.items.len == 0) {
        std.debug.print("  (no change — ratchet held)\n", .{});
    } else {
        std.debug.print("ratchet strengthened — passing.\n", .{});
    }
}

/// Heap-owned set of property names for cross-build comparison. Both
/// branches (collected from a freshly-built contract and parsed from a
/// baseline JSON file) dupe their strings into this set so deinit is
/// uniform and the set survives any tear-down of its source.
const ProvenSet = struct {
    names: std.ArrayListUnmanaged([]const u8) = .empty,

    fn deinit(self: *ProvenSet, allocator: std.mem.Allocator) void {
        for (self.names.items) |n| allocator.free(n);
        self.names.deinit(allocator);
    }
};

fn collectProvenSet(allocator: std.mem.Allocator, handler_path: []const u8) RatchetError!ProvenSet {
    const source = zigts.file_io.readFile(allocator, handler_path, handler_source_limit) catch |err| {
        std.debug.print("ratchet: failed to read {s}: {s}\n", .{ handler_path, @errorName(err) });
        return error.HandlerCompileFailed;
    };
    defer allocator.free(source);

    // emit_verify is load-bearing: result_safe and optional_safe in
    // HandlerProperties are populated only when HandlerVerifier runs, so
    // turning verification off would silently weaken every ratchet check.
    var compiled = precompile.compileHandler(allocator, source, handler_path, .{
        .emit_contract = true,
        .emit_verify = true,
    }) catch {
        std.debug.print("ratchet: handler compile failed for {s}\n", .{handler_path});
        return error.HandlerCompileFailed;
    };
    defer compiled.deinit(allocator);

    var set = ProvenSet{};
    errdefer set.deinit(allocator);

    const contract = compiled.contract orelse return set;
    const props = contract.properties orelse return set;

    var buf: [HandlerProperties.max_proven_specs]?[]const u8 = undefined;
    const count = props.provenSpecNames(&buf);
    try set.names.ensureTotalCapacity(allocator, count);
    for (buf[0..count]) |name_opt| {
        if (name_opt) |nm| {
            const owned = try allocator.dupe(u8, nm);
            errdefer allocator.free(owned);
            set.names.appendAssumeCapacity(owned);
        }
    }
    return set;
}

fn loadBaseline(allocator: std.mem.Allocator, baseline_path: []const u8) RatchetError!ProvenSet {
    const bytes = zigts.file_io.readFile(allocator, baseline_path, 4 * 1024 * 1024) catch |err| {
        std.debug.print("ratchet: failed to read baseline {s}: {s}\n", .{ baseline_path, @errorName(err) });
        return error.MissingBaseline;
    };
    defer allocator.free(bytes);

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, bytes, .{}) catch {
        std.debug.print("ratchet: baseline {s} is not valid JSON\n", .{baseline_path});
        return error.MissingBaseline;
    };
    defer parsed.deinit();

    var set = ProvenSet{};
    errdefer set.deinit(allocator);

    if (parsed.value != .object) return set;
    const root = parsed.value.object;

    // Prefer the explicit `provenSpecs` array; fall back to deriving from the
    // `properties` object so old contracts (without provenSpecs) still work.
    if (root.get("provenSpecs")) |ps| {
        if (ps == .array) {
            for (ps.array.items) |item| {
                if (item == .string) {
                    if (!HandlerProperties.isMonotonicProvenSpecName(item.string)) continue;
                    const owned = try allocator.dupe(u8, item.string);
                    errdefer allocator.free(owned);
                    try set.names.append(allocator, owned);
                }
            }
            return set;
        }
    }
    if (root.get("properties")) |props_val| {
        if (props_val == .object) {
            var it = props_val.object.iterator();
            while (it.next()) |entry| {
                if (entry.value_ptr.* == .bool and entry.value_ptr.*.bool) {
                    if (HandlerProperties.snakeKeyFor(entry.key_ptr.*)) |snake| {
                        if (!HandlerProperties.isMonotonicProvenSpecName(snake)) continue;
                        const owned = try allocator.dupe(u8, snake);
                        errdefer allocator.free(owned);
                        try set.names.append(allocator, owned);
                    }
                }
            }
        }
    }
    return set;
}

fn containsName(haystack: []const []const u8, needle: []const u8) bool {
    for (haystack) |n| {
        if (std.mem.eql(u8, n, needle)) return true;
    }
    return false;
}

test "loadBaseline filters non-monotonic proven specs" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = "explicit.json",
        .data =
        \\{
        \\  "provenSpecs": ["has_egress", "read_only"]
        \\}
        ,
    });
    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = "legacy.json",
        .data =
        \\{
        \\  "properties": {
        \\    "hasEgress": true,
        \\    "readOnly": true
        \\  }
        \\}
        ,
    });

    const tmp_path = try tmp.dir.realpathAlloc(std.testing.io, allocator, ".");
    defer allocator.free(tmp_path);

    const explicit_path = try std.fs.path.join(allocator, &.{ tmp_path, "explicit.json" });
    defer allocator.free(explicit_path);
    var explicit = try loadBaseline(allocator, explicit_path);
    defer explicit.deinit(allocator);
    try std.testing.expect(!containsName(explicit.names.items, "has_egress"));
    try std.testing.expect(containsName(explicit.names.items, "read_only"));

    const legacy_path = try std.fs.path.join(allocator, &.{ tmp_path, "legacy.json" });
    defer allocator.free(legacy_path);
    var legacy = try loadBaseline(allocator, legacy_path);
    defer legacy.deinit(allocator);
    try std.testing.expect(!containsName(legacy.names.items, "has_egress"));
    try std.testing.expect(containsName(legacy.names.items, "read_only"));
}

fn printHelp() void {
    std.debug.print(
        \\zigttp ratchet — proven-property monotonicity gate (slice 4 of attest)
        \\
        \\Usage:
        \\  zigttp ratchet show <handler.ts>
        \\      Compile the handler and print the property set it currently proves.
        \\
        \\  zigttp ratchet check --baseline <old-contract.json> <handler.ts>
        \\      Compile the handler, parse the baseline contract, and compare.
        \\      Exits 1 if any previously-proven property is no longer provable.
        \\
        \\The proven set is also written to contract.json under `provenSpecs`
        \\and rides inside the signed Zigttp-Attest JWS, so cross-build diffs
        \\are mechanical and attestable.
        \\
    , .{});
}
