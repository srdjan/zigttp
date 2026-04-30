//! Proven Live Reload
//!
//! Watches handler source files and hot-swaps the running handler when changes
//! are detected. With --prove, diffs the behavioral contract against the
//! running version and only applies changes proven safe by the upgrade verifier.
//!
//! Data flow on file change:
//!   1. Read handler source, run full analysis pipeline (runCheckOnly)
//!   2. If analysis fails: print errors, keep running old handler
//!   3. Diff new contract against current contract (if --prove)
//!   4. If safe or safe_with_additions: hot-swap pool, update server contract
//!   5. If breaking: print diff, keep old handler, suggest --force-swap

const std = @import("std");
const Server = @import("server.zig").Server;
const contract_runtime = @import("contract_runtime.zig");
const zigts = @import("zigts");
const compat = zigts.compat;
const zigts_cli = @import("zigts_cli");
const precompile = zigts_cli.precompile;
const contract_diff = zigts.contract_diff;
const upgrade_verifier = zigts_cli.upgrade_verifier;
const HandlerContract = zigts.HandlerContract;
const deploy_manifest = zigts_cli.deploy_manifest;
const review_facts_mod = @import("deploy/review.zig");
const proof_ledger = @import("proof_ledger.zig");
const proof_card_tui = @import("proof_card_tui.zig");
const printer_mod = @import("deploy/printer.zig");

pub const LiveReloadConfig = struct {
    /// Enable contract diffing (--prove)
    prove: bool = false,
    /// Allow breaking changes to be applied (--force-swap)
    force_swap: bool = false,
    /// Polling interval in milliseconds
    poll_interval_ms: i64 = 250,
    /// SQL schema path for contract extraction
    sql_schema_path: ?[]const u8 = null,
    /// system.json path for service linking
    system_path: ?[]const u8 = null,
};

pub const LiveReloadState = struct {
    allocator: std.mem.Allocator,
    server: *Server,
    handler_path: []const u8,
    config: LiveReloadConfig,
    current_contract: ?HandlerContract,
    /// Previous handler code kept alive for in-flight requests
    previous_code: ?[]const u8,
    previous_stamp: u64,
    watch_paths: []const []const u8,
    /// Last source-bytes sha logged to .zigttp/proofs.jsonl. Suppresses
    /// duplicate appends when the watcher fires for a no-op save (or when
    /// our own ledger write trips the recursive directory walk).
    last_ledger_sha: ?[std.crypto.hash.sha2.Sha256.digest_length * 2]u8,

    pub fn init(
        allocator: std.mem.Allocator,
        server: *Server,
        handler_path: []const u8,
        watch_paths: []const []const u8,
        config: LiveReloadConfig,
    ) LiveReloadState {
        return .{
            .allocator = allocator,
            .server = server,
            .handler_path = handler_path,
            .config = config,
            .current_contract = null,
            .previous_code = null,
            .previous_stamp = 0,
            .watch_paths = watch_paths,
            .last_ledger_sha = null,
        };
    }

    pub fn deinit(self: *LiveReloadState) void {
        if (self.current_contract) |*c| c.deinit(self.allocator);
        if (self.previous_code) |code| self.allocator.free(code);
    }

    /// Run the watch-reload loop. Blocks until the server stops.
    pub fn run(self: *LiveReloadState, io: std.Io) !void {
        self.previous_stamp = try computeStamp(io, self.watch_paths);
        if (self.config.prove) {
            self.seedInitialProof();
        }

        while (self.server.running) {
            try std.Io.sleep(io, .fromMilliseconds(self.config.poll_interval_ms), .awake);

            const next_stamp = try computeStamp(io, self.watch_paths);
            if (next_stamp == self.previous_stamp) continue;
            self.previous_stamp = next_stamp;

            self.recompileAndSwap();
        }
    }

    /// Run analysis on pre-read source, cleaning up CheckResult.
    fn runAnalysisFromSource(self: *LiveReloadState, source: []const u8, skip_contract: bool) ?AnalysisResult {
        var result = precompile.runCheckOnlyFromSource(
            self.allocator,
            source,
            self.handler_path,
            self.config.sql_schema_path,
            false,
            self.config.system_path,
            skip_contract,
        ) catch return null;

        // Steal contract ownership before deinit frees it
        const contract = result.contract;
        result.contract = null;
        const errors = result.totalErrors();
        const pinned_regressions = result.pinned_witness_regressions;
        result.deinit(self.allocator);

        return .{ .contract = contract, .errors = errors, .pinned_regressions = pinned_regressions };
    }

    const AnalysisResult = struct {
        contract: ?HandlerContract,
        errors: u32,
        pinned_regressions: usize,

        fn deinitContract(self: *AnalysisResult, allocator: std.mem.Allocator) void {
            if (self.contract) |*c| c.deinit(allocator);
            self.contract = null;
        }
    };

    fn recompileAndSwap(self: *LiveReloadState) void {
        if (self.server.studio) |*studio| studio.updateChecking();

        var new_code: ?[]const u8 = zigts.file_io.readFile(self.allocator, self.handler_path, 10 * 1024 * 1024) catch |err| {
            if (self.server.studio) |*studio| studio.updateError("failed to read handler");
            printReload("Failed to read handler: {}. Keeping previous.\n", .{err});
            return;
        };
        defer if (new_code) |c| self.allocator.free(c);

        var timer = compat.Timer.start() catch null;

        var analysis = self.runAnalysisFromSource(new_code.?, !self.config.prove) orelse {
            if (self.server.studio) |*studio| studio.updateError("recompilation failed");
            printReload("Recompilation failed. Keeping previous handler.\n", .{});
            return;
        };
        defer if (analysis.contract != null) analysis.deinitContract(self.allocator);

        const elapsed_ms = if (timer) |*t| t.read() / std.time.ns_per_ms else 0;

        if (analysis.errors > 0) {
            if (self.server.studio) |*studio| studio.updateError("compilation failed");
            printReload("Compilation failed ({d} error(s), {d}ms). Keeping previous handler.\n", .{
                analysis.errors,
                elapsed_ms,
            });
            if (analysis.pinned_regressions > 0) {
                printReload("[witnesses] {d} pinned witness(es) re-fired - regression of a previously defended pattern.\n", .{analysis.pinned_regressions});
            }
            return;
        }

        printReload("Change detected in {s} ({d}ms recompile)\n", .{
            std.fs.path.basename(self.handler_path),
            elapsed_ms,
        });

        if (analysis.pinned_regressions > 0) {
            printReload("[witnesses] {d} pinned witness(es) re-fired during analysis.\n", .{analysis.pinned_regressions});
        }

        if (self.config.prove) {
            var new_contract = analysis.contract orelse {
                printReload("No contract extracted. Reloading without proof.\n", .{});
                self.doSwap(&new_code, false);
                return;
            };
            analysis.contract = null;

            // Hash the source bytes once: the HUD uses the prefix as the
            // contract identifier in its status bar, and the ledger uses the
            // full hex for change-detection on swap.
            var sha_digest: [std.crypto.hash.sha2.Sha256.digest_length]u8 = undefined;
            std.crypto.hash.sha2.Sha256.hash(new_code.?, &sha_digest, .{});
            const sha_hex = std.fmt.bytesToHex(sha_digest, .lower);

            if (self.current_contract) |*old_contract| {
                if (new_contract.durable.used) {
                    printProve("Handler uses durable execution. Live swap refused.\n", .{});
                    printProve("Restart the server to apply changes.\n", .{});
                    new_contract.deinit(self.allocator);
                    return;
                }

                var diff = contract_diff.diffContracts(
                    self.allocator,
                    old_contract,
                    &new_contract,
                ) catch |err| {
                    printProve("Contract diff failed: {}. Reloading without proof.\n", .{err});
                    self.updateCurrentContract(new_contract);
                    self.doSwap(&new_code, false);
                    return;
                };
                defer diff.deinit(self.allocator);

                const classification = diff.classify();

                var manifest = upgrade_verifier.analyzeUpgrade(
                    self.allocator,
                    &diff,
                    classification,
                    null,
                    &new_contract,
                ) catch |err| {
                    printProve("Upgrade analysis failed: {}. Reloading without proof.\n", .{err});
                    self.updateCurrentContract(new_contract);
                    self.doSwap(&new_code, false);
                    return;
                };
                defer manifest.deinit(self.allocator);

                self.renderHud(&new_contract, old_contract, elapsed_ms, &sha_hex);

                const verdict = manifest.verdict;
                const should_swap = verdict == .safe or verdict == .safe_with_additions or self.config.force_swap;
                if (should_swap) {
                    if (verdict != .safe and verdict != .safe_with_additions) {
                        printProve("Verdict: {s}. Applying anyway (--force-swap).\n", .{verdict.toString()});
                    }
                    self.tryLogSwap(&new_contract, &sha_hex);
                    self.updateCurrentContract(new_contract);
                    self.doSwap(&new_code, true);
                } else {
                    printProve("Swap blocked. Fix the breaking change or use --force-swap.\n", .{});
                    new_contract.deinit(self.allocator);
                }
            } else {
                self.renderHud(&new_contract, null, elapsed_ms, &sha_hex);
                self.tryLogSwap(&new_contract, &sha_hex);
                self.updateCurrentContract(new_contract);
                self.doSwap(&new_code, true);
            }
        } else {
            self.doSwap(&new_code, false);
        }
    }

    fn seedInitialProof(self: *LiveReloadState) void {
        if (self.server.studio) |*studio| studio.updateChecking();

        const source = zigts.file_io.readFile(self.allocator, self.handler_path, 10 * 1024 * 1024) catch |err| {
            if (self.server.studio) |*studio| studio.updateError("failed to read handler");
            printProve("Initial proof skipped: failed to read handler: {}\n", .{err});
            return;
        };
        defer self.allocator.free(source);

        var timer = compat.Timer.start() catch null;
        var analysis = self.runAnalysisFromSource(source, false) orelse {
            if (self.server.studio) |*studio| studio.updateError("initial proof analysis failed");
            printProve("Initial proof analysis failed.\n", .{});
            return;
        };
        defer if (analysis.contract != null) analysis.deinitContract(self.allocator);

        const elapsed_ms = if (timer) |*t| t.read() / std.time.ns_per_ms else 0;
        if (analysis.errors > 0) {
            if (self.server.studio) |*studio| studio.updateError("initial compilation failed");
            printProve("Initial proof failed ({d} error(s), {d}ms).\n", .{ analysis.errors, elapsed_ms });
            return;
        }

        var contract = analysis.contract orelse {
            if (self.server.studio) |*studio| studio.updateError("no contract extracted");
            printProve("Initial proof skipped: no contract extracted.\n", .{});
            return;
        };
        analysis.contract = null;

        var sha_digest: [std.crypto.hash.sha2.Sha256.digest_length]u8 = undefined;
        std.crypto.hash.sha2.Sha256.hash(source, &sha_digest, .{});
        const sha_hex = std.fmt.bytesToHex(sha_digest, .lower);

        self.renderHud(&contract, null, elapsed_ms, &sha_hex);
        self.tryLogSwap(&contract, &sha_hex);
        self.updateCurrentContract(contract);
        self.installRuntimeContract();
        printProve("Initial proof ready ({d}ms).\n", .{elapsed_ms});
    }

    fn updateCurrentContract(self: *LiveReloadState, new_contract: HandlerContract) void {
        if (self.current_contract) |*c| c.deinit(self.allocator);
        self.current_contract = new_contract;
    }

    fn installRuntimeContract(self: *LiveReloadState) void {
        if (self.current_contract) |*hc| {
            const raw = contract_runtime.fromHandlerContract(self.allocator, hc) catch |err| {
                printReload("Failed to build runtime contract: {}.\n", .{err});
                return;
            };
            const validated = contract_runtime.validate(raw, .{}) catch |err| {
                printReload("Contract failed validation: {}.\n", .{err});
                return;
            };
            self.server.updateContract(validated);
        }
    }

    /// Dedupe against the last logged sha and append a `kind=swap` row when
    /// the proven facts have actually changed. Failures warn but never block
    /// the swap. `sha_hex` is the hex digest of the source bytes, computed by
    /// the caller so the HUD render path can share it.
    fn tryLogSwap(
        self: *LiveReloadState,
        contract: *const HandlerContract,
        sha_hex: *const [std.crypto.hash.sha2.Sha256.digest_length * 2]u8,
    ) void {
        if (self.last_ledger_sha) |prev| {
            if (std.mem.eql(u8, sha_hex, &prev)) return;
        }

        appendSwapToLedger(self.allocator, contract, self.handler_path, sha_hex) catch |err| {
            printProve("Proof ledger append failed: {}. Swap continues.\n", .{err});
            return;
        };
        self.last_ledger_sha = sha_hex.*;
    }

    /// Render the live HUD frame for the new contract relative to the running
    /// baseline. Failures degrade silently to a single `[prove]` warning so
    /// a renderer bug can never block a swap. `sha_hex` is the source-bytes
    /// hex digest, shared with the ledger-append path to avoid double-hashing.
    fn renderHud(
        self: *LiveReloadState,
        new_contract: *const HandlerContract,
        baseline_contract: ?*const HandlerContract,
        elapsed_ms: u64,
        sha_hex: *const [std.crypto.hash.sha2.Sha256.digest_length * 2]u8,
    ) void {
        self.renderHudInner(new_contract, baseline_contract, elapsed_ms, sha_hex) catch |err| {
            printProve("HUD render skipped: {}\n", .{err});
        };
    }

    fn renderHudInner(
        self: *LiveReloadState,
        new_contract: *const HandlerContract,
        baseline_contract: ?*const HandlerContract,
        elapsed_ms: u64,
        sha_hex: *const [std.crypto.hash.sha2.Sha256.digest_length * 2]u8,
    ) !void {
        var new_facts = try factsFromContract(self.allocator, new_contract, sha_hex[0..16]);
        defer new_facts.deinit(self.allocator);

        var maybe_baseline_facts: ?review_facts_mod.ReviewFacts = null;
        defer if (maybe_baseline_facts) |*b| b.deinit(self.allocator);
        if (baseline_contract) |bc| {
            const baseline_label: []const u8 = if (self.last_ledger_sha) |prev|
                prev[0..16]
            else
                "(unknown)";
            maybe_baseline_facts = try factsFromContract(self.allocator, bc, baseline_label);
        }

        const baseline_ptr: ?*const review_facts_mod.ReviewFacts =
            if (maybe_baseline_facts) |*b| b else null;

        var delta = try review_facts_mod.deriveDelta(self.allocator, &new_facts, baseline_ptr);
        defer delta.deinit(self.allocator);

        if (self.server.studio) |*studio| {
            studio.updateFacts(&new_facts, baseline_ptr, &delta, elapsed_ms);
        }

        // Translate the contract's per-property provenance into the renderer's
        // PropertyCauseEntry slice. The slice borrows from the static
        // `property_metas` field names and the contract's static snippets, so
        // it lives only as long as new_contract on this stack frame.
        var causes_buf: [4]review_facts_mod.PropertyCauseEntry = undefined;
        var n_causes: usize = 0;
        if (new_contract.property_provenance.deterministic) |c| {
            causes_buf[n_causes] = .{
                .field = "deterministic",
                .line = c.line,
                .column = c.column,
                .snippet = c.snippet,
            };
            n_causes += 1;
        }

        const card = review_facts_mod.ProofCard{
            .handler_path = self.handler_path,
            .service_name = "dev",
            .region = "local",
            .current = &new_facts,
            .baseline = baseline_ptr,
            .delta = &delta,
            .property_causes = causes_buf[0..n_causes],
        };

        var stderr_buf: [16 * 1024]u8 = undefined;
        var stderr_writer = printer_mod.FdWriter.init(std.c.STDERR_FILENO, stderr_buf[0..]);
        const recompile_ms: u32 = @intCast(@min(elapsed_ms, std.math.maxInt(u32)));
        try proof_card_tui.writeProofCardFrame(self.allocator, &card, &stderr_writer.interface, .{
            .recompile_ms = recompile_ms,
        });
        stderr_writer.interface.flush() catch {};
    }

    /// Take ownership of new_code and swap the handler pool.
    /// Nulls the caller's pointer so the defer cleanup is a no-op.
    fn doSwap(self: *LiveReloadState, new_code_ptr: *?[]const u8, update_contract: bool) void {
        const new_code = new_code_ptr.* orelse return;
        new_code_ptr.* = null;

        if (self.previous_code) |old| self.allocator.free(old);
        self.previous_code = new_code;

        if (self.server.pool) |*pool| {
            const invalidated = pool.reloadHandler(new_code, self.handler_path);

            if (update_contract) {
                if (self.current_contract) |*hc| {
                    const raw = contract_runtime.fromHandlerContract(self.allocator, hc) catch |err| {
                        printReload("Failed to build runtime contract: {}.\n", .{err});
                        printReload("Handler swapped. {d} runtime(s) invalidated.\n", .{invalidated});
                        return;
                    };
                    // Live reload validates without an embedded artifact: the
                    // HandlerContract carries hashes only when the original build
                    // emitted them, and verifyArtifactHash skips when absent.
                    const validated = contract_runtime.validate(raw, .{}) catch |err| {
                        printReload("Contract failed validation: {}.\n", .{err});
                        printReload("Handler swapped. {d} runtime(s) invalidated.\n", .{invalidated});
                        return;
                    };
                    self.server.updateContract(validated);

                    if (self.server.proof_cache != null) {
                        printProve("Proof cache: enabled (deterministic + read_only)\n", .{});
                    }
                }
            }

            printReload("Handler swapped. {d} runtime(s) invalidated.\n", .{invalidated});
        } else {
            printReload("No handler pool available. Swap skipped.\n", .{});
        }
    }
};

// ---------------------------------------------------------------------------
// Output formatting
// ---------------------------------------------------------------------------

/// Project a HandlerContract into the persisted-and-rendered ReviewFacts
/// shape. Used by both the HUD render path and the proof-ledger append path
/// so dev mode and the ledger never disagree on the proven surface.
pub fn factsFromContract(
    allocator: std.mem.Allocator,
    contract: *const HandlerContract,
    contract_sha: []const u8,
) !review_facts_mod.ReviewFacts {
    var extract = try deploy_manifest.extractProvenFacts(allocator, contract);
    defer {
        allocator.free(extract.checks_buf);
        allocator.free(extract.routes_buf);
    }

    const caps_slice = contract.capabilities.slice();
    const cap_names = try allocator.alloc([]const u8, caps_slice.len);
    defer allocator.free(cap_names);
    for (caps_slice, 0..) |cap, i| cap_names[i] = @tagName(cap);

    const spec_states = try buildSpecStates(allocator, contract);
    defer allocator.free(spec_states);

    return review_facts_mod.ReviewFacts.fromProvenFacts(
        allocator,
        &extract.facts,
        cap_names,
        contract_sha,
        spec_states,
    );
}

/// Build a transient SpecState slice from the contract's author-declared
/// specs and the spec_discharge diagnostics. The slice is borrowed - names
/// reference the contract's owned strings - and must be freed by the caller
/// before the contract is freed. ReviewFacts.fromProvenFacts dupes the
/// names into its own ownership.
fn buildSpecStates(
    allocator: std.mem.Allocator,
    contract: *const HandlerContract,
) ![]review_facts_mod.SpecState {
    const decls = contract.declared_specs.items;
    const out = try allocator.alloc(review_facts_mod.SpecState, decls.len);
    errdefer allocator.free(out);
    for (decls, 0..) |name, i| {
        out[i] = .{
            .name = name,
            .discharged = !specHasNotDischarged(contract, name),
        };
    }
    return out;
}

fn specHasNotDischarged(contract: *const HandlerContract, name: []const u8) bool {
    for (contract.spec_diagnostics.items) |d| {
        switch (d.kind) {
            .not_discharged, .incompatible_with_import, .unknown_name => {
                if (std.mem.eql(u8, d.spec_name, name)) return true;
            },
        }
    }
    return false;
}

/// Compute a hash over all watched paths (files and directory contents).
/// Replicates the recursive directory walking from zigttp_cli.foldPathIntoHash
/// to detect changes in subdirectories.
fn computeStamp(io: std.Io, paths: []const []const u8) !u64 {
    var hash = std.hash.Wyhash.init(0);
    for (paths) |path| {
        try foldPathIntoHash(io, &hash, path);
    }
    return hash.final();
}

fn foldPathIntoHash(io: std.Io, hash: *std.hash.Wyhash, path: []const u8) !void {
    const stat = std.Io.Dir.statFile(std.Io.Dir.cwd(), io, path, .{}) catch |err| switch (err) {
        error.FileNotFound, error.NotDir => return,
        else => return err,
    };

    hash.update(path);
    hash.update(std.mem.asBytes(&stat.mtime.nanoseconds));
    hash.update(std.mem.asBytes(&stat.size));

    if (stat.kind != .directory) return;

    var dir = std.Io.Dir.openDir(std.Io.Dir.cwd(), io, path, .{ .iterate = true }) catch return;
    defer dir.close(io);

    var walker = dir.walk(std.heap.smp_allocator) catch return;
    defer walker.deinit();

    while (walker.next(io) catch null) |entry| {
        const entry_stat = std.Io.Dir.statFile(entry.dir, io, entry.basename, .{}) catch continue;
        hash.update(entry.path);
        hash.update(std.mem.asBytes(&entry_stat.mtime.nanoseconds));
        hash.update(std.mem.asBytes(&entry_stat.size));
    }
}

fn printReload(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("[reload] " ++ fmt, args);
}

fn printProve(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("[prove]  " ++ fmt, args);
}

/// Project the new contract into ReviewFacts and append a `kind=swap` row to
/// `.zigttp/proofs.jsonl`. `sha_hex` is the source-bytes sha256 hex (already
/// computed by the caller for dedupe).
fn appendSwapToLedger(
    allocator: std.mem.Allocator,
    contract: *const HandlerContract,
    handler_path: []const u8,
    sha_hex: []const u8,
) !void {
    var facts = try factsFromContract(allocator, contract, sha_hex);
    defer facts.deinit(allocator);

    try proof_ledger.appendEvent(allocator, .{
        .kind = .swap,
        .facts = &facts,
        .handler_path = handler_path,
        .service_name = null,
    });
}

// ============================================================================
// Tests
// ============================================================================

test "LiveReloadConfig defaults" {
    const config = LiveReloadConfig{};
    try std.testing.expect(!config.prove);
    try std.testing.expect(!config.force_swap);
    try std.testing.expectEqual(@as(i64, 250), config.poll_interval_ms);
}
