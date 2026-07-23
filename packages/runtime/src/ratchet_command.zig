//! `zttp ratchet` — Spec-derived monotonicity gate for proven properties.
//!
//! Slice 4 of the attest program. The compiler already infers and signs
//! every handler's proven-property set (`provenSpecs` in contract.json,
//! covered by the JWS payload) and records the active obligations in
//! `contract.declared_specs`. An explicit handler `Spec<...>` narrows
//! that active set; otherwise every supported spec is active by default.
//! `ratchet check` ties the two together: it compiles the handler once,
//! takes `declared_specs` as the obligation set, and fails when the
//! proven set does not cover it.
//!
//! Subcommands:
//!   show  <handler.ts>     print provenSpecs
//!   check <handler.ts>     compile, diff active vs proven, exit 1
//!                          if any active spec is not proven
//!
//! There is no `--baseline` file. The earlier hand-maintained baseline
//! JSON has been retired: the obligation set lives in the compiled
//! contract, with source `Spec<...>` acting as a narrowing override.
//!
//! Waiver signing (a signed file under `.zttp/waivers/` that lets the
//! operator accept a regression with a recorded reason) lands in a
//! follow-up. The current `check` exits 1 on any unmet declaration; that
//! is the mechanically-correct default for a ratchet.

const std = @import("std");
const zts = @import("zts");
const zts_cli = @import("zts_cli");
const cli_args = @import("cli_args.zig");
const precompile = zts_cli.precompile;
const HandlerProperties = zts.handler_contract.HandlerProperties;

/// Source-file size cap for `ratchet show`/`check`. Handlers are
/// authored, not generated, so 10 MB is generous; this keeps the limit
/// explicit at the call site instead of borrowing the misleading
/// `default_output_limit` constant from the pi tools.
const handler_source_limit: usize = 10 * 1024 * 1024;

pub const RatchetError = error{
    UnknownSubcommand,
    MissingArgument,
    UnknownFlag,
    TooManyArguments,
    BaselineFlagRemoved,
    NonRatchetableSpec,
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
        std.debug.print("zttp ratchet: unknown subcommand `{s}`\n\n", .{argv[0]});
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
    if (cli_args.hasHelpFlag(argv)) {
        printHelp();
        return;
    }

    var handler_path: ?[]const u8 = null;

    // Flag-aware parsing keeps `runShow` symmetric with `runCheck`:
    // a stray `--anything` becomes an UnknownFlag error instead of a
    // cryptic "failed to read --anything" file-not-found message.
    for (argv) |arg| {
        if (std.mem.startsWith(u8, arg, "-")) {
            std.debug.print(
                "zttp ratchet show: unknown flag `{s}` (this subcommand takes no flags)\n",
                .{arg},
            );
            return error.UnknownFlag;
        }
        if (handler_path != null) {
            std.debug.print(
                "zttp ratchet show: multiple handler paths given (`{s}` then `{s}`)\n",
                .{ handler_path.?, arg },
            );
            return error.TooManyArguments;
        }
        handler_path = arg;
    }

    const handler = handler_path orelse {
        std.debug.print("zttp ratchet show: handler path required\n", .{});
        return error.MissingArgument;
    };

    var sets = try collectSpecSets(allocator, handler);
    defer sets.deinit(allocator);

    // `show` is a read-out of the proven set. It intentionally does NOT
    // surface `sets.dropped` (non-monotonic declared names) — that
    // warning belongs to `check`, where it actually changes the verdict.

    std.debug.print("proven specs for {s}:\n", .{handler});
    if (sets.proven.names.items.len == 0) {
        std.debug.print("  (none — compile failed or no properties proven)\n", .{});
        return;
    }
    for (sets.proven.names.items) |name| {
        std.debug.print("  - {s}\n", .{name});
    }
}

fn runCheck(allocator: std.mem.Allocator, argv: []const []const u8) RatchetError!void {
    if (cli_args.hasHelpFlag(argv)) {
        printHelp();
        return;
    }

    var handler_path: ?[]const u8 = null;

    // Argv parser is strict: every token is either the retired
    // `--baseline` (any form), or a positional (and only one). Anything
    // else — a typo like `--basline`, a leftover `--json`, a second
    // positional — fails loudly. Silent acceptance is the failure mode
    // this command has to avoid: a CI script that silently checks the
    // wrong file is worse than one that exits 2 with "I don't know what
    // you meant".
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--baseline") or
            std.mem.startsWith(u8, arg, "--baseline="))
        {
            // Retired flag. Print an actionable migration message rather
            // than silently ignoring — anyone still scripting `--baseline`
            // (in either `--baseline x` or `--baseline=x` form) wants a
            // loud failure with a pointer to the new shape.
            std.debug.print(
                \\zttp ratchet check: `--baseline` has been removed.
                \\The baseline is now the active spec set in the compiled
                \\contract. Drop the flag; use `Spec` from "zttp:types"
                \\on the handler return type only when you need to narrow
                \\the default supported set, e.g.
                \\
                \\    import type {{ Spec }} from "zttp:types";
                \\    type Guardrails = Spec<"pure" | "deterministic">;
                \\    function handler(req: Request): Response & Guardrails {{ ... }}
                \\
            , .{});
            return error.BaselineFlagRemoved;
        }
        if (std.mem.startsWith(u8, arg, "-")) {
            std.debug.print(
                "zttp ratchet check: unknown flag `{s}`. " ++
                    "This subcommand takes no flags — see `zttp ratchet --help`.\n",
                .{arg},
            );
            return error.UnknownFlag;
        }
        if (handler_path != null) {
            std.debug.print(
                "zttp ratchet check: multiple handler paths given (`{s}` then `{s}`). " ++
                    "Ratchet check takes exactly one handler.\n",
                .{ handler_path.?, arg },
            );
            return error.TooManyArguments;
        }
        handler_path = arg;
    }

    const handler = handler_path orelse {
        std.debug.print("zttp ratchet check: handler path required\n", .{});
        return error.MissingArgument;
    };

    var sets = try collectSpecSets(allocator, handler);
    defer sets.deinit(allocator);

    // Non-monotonic declared names (e.g. `Spec<"has_egress">`) cannot be
    // ratcheted — a true value would be a weakening, not a strengthening.
    // collectSpecSets stores them in `sets.dropped` rather than printing
    // inline so this error path stays out of `ratchet show`. We fail
    // loudly here: the operator has typed an obligation the gate cannot
    // discharge, and silent acceptance would re-open exactly the gap a
    // ratchet is supposed to close.
    if (sets.dropped.names.items.len > 0) {
        std.debug.print(
            "ratchet check for {s}: non-ratchetable active spec(s) — these are not monotonic obligations:\n",
            .{handler},
        );
        for (sets.dropped.names.items) |name| {
            std.debug.print("  - {s}\n", .{name});
        }
        std.debug.print(
            "\nRemove the non-monotonic name(s) from `Spec<...>` (or correct the typo). " ++
                "spec_discharge has already emitted a compile-time diagnostic for any unknown name.\n",
            .{},
        );
        return error.NonRatchetableSpec;
    }

    // Empty active obligations are not expected for newly compiled
    // contracts, but keep the branch for older/corrupt inputs.
    if (sets.declared.names.items.len == 0) {
        std.debug.print(
            "ratchet check for {s}: no active specs — nothing to ratchet.\n",
            .{handler},
        );
        return;
    }

    var unmet = std.ArrayListUnmanaged([]const u8).empty;
    defer unmet.deinit(allocator);
    var extra = std.ArrayListUnmanaged([]const u8).empty;
    defer extra.deinit(allocator);

    for (sets.declared.names.items) |declared| {
        if (!containsName(sets.proven.names.items, declared)) {
            try unmet.append(allocator, declared);
        }
    }
    for (sets.proven.names.items) |proven_name| {
        if (!containsName(sets.declared.names.items, proven_name)) {
            try extra.append(allocator, proven_name);
        }
    }

    std.debug.print("ratchet check for {s}:\n", .{handler});
    std.debug.print("  declared: ", .{});
    printList(sets.declared.names.items);
    std.debug.print("  proven:   ", .{});
    printList(sets.proven.names.items);

    // Failure narrative: unmet -> verdict. Extras are suppressed on a
    // weakening so the cause-effect line stays unambiguous — without
    // this, a regression with extras prints "+ extra: ..." before
    // "- unmet: ...", visually wedging the extras as the failure cause.
    if (unmet.items.len > 0) {
        std.debug.print("  - unmet (declared but not proven): ", .{});
        printList(unmet.items);
        std.debug.print("\nratchet weakened — failing.\n", .{});
        return error.Regression;
    }

    // Success narrative: extras (if any) -> verdict.
    if (extra.items.len > 0) {
        std.debug.print("  + extra (proven beyond declared): ", .{});
        printList(extra.items);
        std.debug.print("\nratchet held — every active spec is proven (extras above are informational).\n", .{});
    } else {
        std.debug.print("\nratchet held — every active spec is proven.\n", .{});
    }
}

fn printList(items: []const []const u8) void {
    if (items.len == 0) {
        std.debug.print("(none)\n", .{});
        return;
    }
    for (items, 0..) |n, idx| {
        if (idx > 0) std.debug.print(", ", .{});
        std.debug.print("{s}", .{n});
    }
    std.debug.print("\n", .{});
}

/// Heap-owned set of property names. Both the proven set (from
/// `HandlerProperties.provenSpecNames`) and the active set (from
/// `contract.declared_specs`) live in this shape so the diff stays
/// symmetric and deinit is uniform.
const ProvenSet = struct {
    names: std.ArrayListUnmanaged([]const u8) = .empty,

    fn deinit(self: *ProvenSet, allocator: std.mem.Allocator) void {
        for (self.names.items) |n| allocator.free(n);
        self.names.deinit(allocator);
    }
};

/// Triple of sets pulled out of a single compile pass:
///   - `proven`   — what the compiler classified as true on this build.
///   - `declared` — active spec names that are monotonic (ratchetable).
///                  Filtered through
///                  `HandlerProperties.isMonotonicProvenSpecName`.
///   - `dropped`  — active names that the filter rejected
///                  (non-monotonic facts like `has_egress`, where "true"
///                  is a weakening, not a strengthening). Held as data
///                  so `runCheck` can surface them loudly as a hard
///                  error while `runShow` stays a pure read-out.
const SpecSets = struct {
    proven: ProvenSet = .{},
    declared: ProvenSet = .{},
    dropped: ProvenSet = .{},

    fn deinit(self: *SpecSets, allocator: std.mem.Allocator) void {
        self.proven.deinit(allocator);
        self.declared.deinit(allocator);
        self.dropped.deinit(allocator);
    }
};

fn collectSpecSets(allocator: std.mem.Allocator, handler_path: []const u8) RatchetError!SpecSets {
    const source = zts.file_io.readFile(allocator, handler_path, handler_source_limit) catch |err| {
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

    var sets = SpecSets{};
    errdefer sets.deinit(allocator);

    const contract = compiled.contract orelse return sets;

    // Proven set: only present when the compiler populated
    // HandlerProperties (i.e. emit_contract succeeded above).
    if (contract.properties) |props| {
        var buf: [HandlerProperties.max_proven_specs]?[]const u8 = undefined;
        const count = props.provenSpecNames(&buf);
        try sets.proven.names.ensureTotalCapacity(allocator, count);
        for (buf[0..count]) |name_opt| {
            if (name_opt) |nm| {
                const owned = try allocator.dupe(u8, nm);
                errdefer allocator.free(owned);
                sets.proven.names.appendAssumeCapacity(owned);
            }
        }
    }

    // Declared set: filter to monotonic names so a stray `has_egress`
    // in a `Spec<...>` does not end up demanding "I do egress" as an
    // obligation. Non-monotonic names go into `sets.dropped` (not
    // printed here) so that `runShow` stays a pure read-out and only
    // `runCheck` surfaces the warning — and surfaces it loudly, as a
    // hard error rather than a stderr line a CI grep might miss.
    for (contract.declared_specs.items) |declared| {
        if (!HandlerProperties.isMonotonicProvenSpecName(declared)) {
            const owned = try allocator.dupe(u8, declared);
            errdefer allocator.free(owned);
            try sets.dropped.names.append(allocator, owned);
            continue;
        }
        const owned = try allocator.dupe(u8, declared);
        errdefer allocator.free(owned);
        try sets.declared.names.append(allocator, owned);
    }

    return sets;
}

fn containsName(haystack: []const []const u8, needle: []const u8) bool {
    for (haystack) |n| {
        if (std.mem.eql(u8, n, needle)) return true;
    }
    return false;
}

test "runCheck holds when every declared spec is proven" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = "held.ts",
        .data =
        \\import type { Spec } from "zttp:types";
        \\type Guardrails = Spec<"pure" | "deterministic">;
        \\function handler(req: Request): Response & Guardrails {
        \\    return Response.json({ ok: true });
        \\}
        ,
    });

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const tmp_len = try tmp.dir.realPath(std.testing.io, &path_buf);
    const tmp_path = path_buf[0..tmp_len];
    const handler_path = try std.fs.path.join(allocator, &.{ tmp_path, "held.ts" });
    defer allocator.free(handler_path);

    // No baseline arg. The declared set comes from the Spec<...> on the
    // return type, the proven set from compile-time inference. A pure
    // handler with no I/O proves both `pure` and `deterministic`, so
    // the ratchet must hold.
    try runCheck(allocator, &.{handler_path});
}

test "runCheck fails when a declared spec is not proven" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = "regress.ts",
        .data =
        \\import type { Spec } from "zttp:types";
        \\type Guardrails = Spec<"fault_covered">;
        \\function handler(req: Request): Response & Guardrails {
        \\    return Response.json({ ok: true });
        \\}
        ,
    });

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const tmp_len = try tmp.dir.realPath(std.testing.io, &path_buf);
    const tmp_path = path_buf[0..tmp_len];
    const handler_path = try std.fs.path.join(allocator, &.{ tmp_path, "regress.ts" });
    defer allocator.free(handler_path);

    // Declares `fault_covered` but the handler has zero failable I/O,
    // so the classifier sets fault_covered=false. spec_discharge emits
    // ZTS500 not_discharged during compile (does not abort), so the
    // contract still populates declared_specs with `fault_covered` and
    // properties with `fault_covered=false`. Ratchet must surface the
    // gap as Regression.
    const result = runCheck(allocator, &.{handler_path});
    try std.testing.expectError(error.Regression, result);
}

test "runCheck enforces default specs when the handler declares no Spec<...>" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = "plain.ts",
        .data =
        \\function handler(req: Request): Response {
        \\    return Response.json({ ok: true });
        \\}
        ,
    });

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const tmp_len = try tmp.dir.realPath(std.testing.io, &path_buf);
    const tmp_path = path_buf[0..tmp_len];
    const handler_path = try std.fs.path.join(allocator, &.{ tmp_path, "plain.ts" });
    defer allocator.free(handler_path);

    // No `Spec<...>` activates every supported spec by default. This
    // plain handler does not prove the full set, so ratchet must surface
    // the gap as a regression.
    const result = runCheck(allocator, &.{handler_path});
    try std.testing.expectError(error.Regression, result);
}

test "runCheck rejects the retired --baseline flag with a migration message" {
    const allocator = std.testing.allocator;
    const result = runCheck(allocator, &.{ "--baseline", "anything.json", "handler.ts" });
    try std.testing.expectError(error.BaselineFlagRemoved, result);
}

test "runCheck rejects the retired --baseline=value form too" {
    const allocator = std.testing.allocator;
    const result = runCheck(allocator, &.{ "--baseline=anything.json", "handler.ts" });
    try std.testing.expectError(error.BaselineFlagRemoved, result);
}

test "runCheck rejects unknown -prefixed flags instead of silent-ignore" {
    const allocator = std.testing.allocator;
    // Typo (`--basline`) used to fall through both branches and be
    // silently dropped; now it fails loudly.
    try std.testing.expectError(
        error.UnknownFlag,
        runCheck(allocator, &.{ "--basline", "anything.json", "handler.ts" }),
    );
    try std.testing.expectError(
        error.UnknownFlag,
        runCheck(allocator, &.{ "--json", "handler.ts" }),
    );
}

test "runCheck rejects multiple positional handler paths" {
    const allocator = std.testing.allocator;
    try std.testing.expectError(
        error.TooManyArguments,
        runCheck(allocator, &.{ "a.ts", "b.ts" }),
    );
}

test "runShow rejects unknown -prefixed flags but not --help" {
    const allocator = std.testing.allocator;
    // `--help` is the project-wide help convention (hasHelpFlag in
    // dev_cli.zig:408); ratchet honours it anywhere in argv.
    try runShow(allocator, &.{"--help"});
    try runShow(allocator, &.{ "-h", "anything" });
    // Other -prefixed tokens still fail loudly.
    try std.testing.expectError(
        error.UnknownFlag,
        runShow(allocator, &.{ "--json", "handler.ts" }),
    );
}

test "runCheck prints help when --help is anywhere in argv" {
    const allocator = std.testing.allocator;
    // The strict-flag parser used to trap `--help` as UnknownFlag,
    // breaking the project-wide help-anywhere convention.
    try runCheck(allocator, &.{"--help"});
    try runCheck(allocator, &.{ "handler.ts", "--help" });
    try runCheck(allocator, &.{"-h"});
    try runCheck(allocator, &.{"help"});
}

test "runCheck fails NonRatchetableSpec when every declared name is non-monotonic" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = "egress.ts",
        .data =
        \\import type { Spec } from "zttp:types";
        \\type Egress = Spec<"has_egress">;
        \\function handler(req: Request): Response & Egress {
        \\    return Response.json({ ok: true });
        \\}
        ,
    });

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const tmp_len = try tmp.dir.realPath(std.testing.io, &path_buf);
    const tmp_path = path_buf[0..tmp_len];
    const handler_path = try std.fs.path.join(allocator, &.{ tmp_path, "egress.ts" });
    defer allocator.free(handler_path);

    // `has_egress` is a known HandlerProperties field but non-monotonic
    // (true = weakening). collectSpecSets stores it in sets.dropped;
    // runCheck must surface the gap as NonRatchetableSpec rather than
    // silently exiting 0 via the "no declared specs" branch.
    try std.testing.expectError(
        error.NonRatchetableSpec,
        runCheck(allocator, &.{handler_path}),
    );
}

test "runCheck holds when Spec<\"pure\"> is declared on a pure handler" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = "pure.ts",
        .data =
        \\import type { Spec } from "zttp:types";
        \\type G = Spec<"pure">;
        \\function handler(req: Request): Response & G {
        \\    return Response.json({ ok: true });
        \\}
        ,
    });

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const tmp_len = try tmp.dir.realPath(std.testing.io, &path_buf);
    const tmp_path = path_buf[0..tmp_len];
    const handler_path = try std.fs.path.join(allocator, &.{ tmp_path, "pure.ts" });
    defer allocator.free(handler_path);

    // Regression guard for the v1_specs/HandlerProperties drift: `pure`
    // is now in v1_specs so spec_discharge stops emitting ZTS502 for it,
    // and ratchet's filter still accepts it as monotonic.
    try runCheck(allocator, &.{handler_path});
}

fn printHelp() void {
    std.debug.print(
        \\zttp ratchet — Spec-derived monotonicity gate (slice 4 of attest)
        \\
        \\Usage:
        \\  zttp ratchet show <handler.ts>
        \\      Compile the handler and print the property set it currently proves.
        \\
        \\  zttp ratchet check <handler.ts>
        \\      Compile the handler and diff the active spec obligations against
        \\      the proven property set. Exits 1 if any active spec is not proven.
        \\      A handler with no `Spec<...>` activates every supported spec.
        \\
        \\Declaring obligations:
        \\
        \\    import type {{ Spec }} from "zttp:types";
        \\    type Guardrails = Spec<"pure" | "deterministic">;
        \\    function handler(req: Request): Response & Guardrails {{ ... }}
        \\
        \\The proven set is also written to contract.json under `provenSpecs`
        \\and rides inside the signed Zttp-Attest JWS, so cross-build diffs
        \\are mechanical and attestable. The active set appears alongside it
        \\under `declaredSpecs`.
        \\
    , .{});
}
