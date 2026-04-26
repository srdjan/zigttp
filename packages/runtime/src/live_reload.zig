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
        };
    }

    pub fn deinit(self: *LiveReloadState) void {
        if (self.current_contract) |*c| c.deinit(self.allocator);
        if (self.previous_code) |code| self.allocator.free(code);
    }

    /// Run the watch-reload loop. Blocks until the server stops.
    pub fn run(self: *LiveReloadState, io: std.Io) !void {
        self.previous_stamp = try computeStamp(io, self.watch_paths);

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
        result.deinit(self.allocator);

        return .{ .contract = contract, .errors = errors };
    }

    const AnalysisResult = struct {
        contract: ?HandlerContract,
        errors: u32,

        fn deinitContract(self: *AnalysisResult, allocator: std.mem.Allocator) void {
            if (self.contract) |*c| c.deinit(allocator);
            self.contract = null;
        }
    };

    fn recompileAndSwap(self: *LiveReloadState) void {
        var new_code: ?[]const u8 = zigts.file_io.readFile(self.allocator, self.handler_path, 10 * 1024 * 1024) catch |err| {
            printReload("Failed to read handler: {}. Keeping previous.\n", .{err});
            return;
        };
        defer if (new_code) |c| self.allocator.free(c);

        var timer = compat.Timer.start() catch null;

        var analysis = self.runAnalysisFromSource(new_code.?, !self.config.prove) orelse {
            printReload("Recompilation failed. Keeping previous handler.\n", .{});
            return;
        };
        defer if (analysis.contract != null) analysis.deinitContract(self.allocator);

        const elapsed_ms = if (timer) |*t| t.read() / std.time.ns_per_ms else 0;

        if (analysis.errors > 0) {
            printReload("Compilation failed ({d} error(s), {d}ms). Keeping previous handler.\n", .{
                analysis.errors,
                elapsed_ms,
            });
            return;
        }

        printReload("Change detected in {s} ({d}ms recompile)\n", .{
            std.fs.path.basename(self.handler_path),
            elapsed_ms,
        });

        if (self.config.prove) {
            var new_contract = analysis.contract orelse {
                printReload("No contract extracted. Reloading without proof.\n", .{});
                self.doSwap(&new_code, false);
                return;
            };
            analysis.contract = null;

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

                formatUpgradeDiff(&manifest);

                const verdict = manifest.verdict;
                const should_swap = verdict == .safe or verdict == .safe_with_additions or self.config.force_swap;
                if (should_swap) {
                    if (verdict != .safe and verdict != .safe_with_additions) {
                        printProve("Verdict: {s}. Applying anyway (--force-swap).\n", .{verdict.toString()});
                    }
                    self.updateCurrentContract(new_contract);
                    self.doSwap(&new_code, true);
                } else {
                    printProve("Swap blocked. Fix the breaking change or use --force-swap.\n", .{});
                    new_contract.deinit(self.allocator);
                }
            } else {
                self.updateCurrentContract(new_contract);
                self.doSwap(&new_code, true);
            }
        } else {
            self.doSwap(&new_code, false);
        }
    }

    fn updateCurrentContract(self: *LiveReloadState, new_contract: HandlerContract) void {
        if (self.current_contract) |*c| c.deinit(self.allocator);
        self.current_contract = new_contract;
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

fn formatUpgradeDiff(manifest: *const upgrade_verifier.UpgradeManifest) void {
    const s = manifest.surface;

    // Routes
    if (s.routes_added > 0 or s.routes_removed > 0 or s.routes_unchanged > 0) {
        printProve("Routes: ", .{});
        if (s.routes_added > 0) printRaw("+{d} added", .{s.routes_added});
        if (s.routes_removed > 0) {
            if (s.routes_added > 0) printRaw(", ", .{});
            printRaw("-{d} removed", .{s.routes_removed});
        }
        if (s.routes_unchanged > 0) {
            if (s.routes_added > 0 or s.routes_removed > 0) printRaw(", ", .{});
            printRaw("{d} unchanged", .{s.routes_unchanged});
        }
        printRaw("\n", .{});
    }

    // Property changes
    if (manifest.property_regressions.items.len > 0) {
        for (manifest.property_regressions.items) |reg| {
            printProve("Property lost: {s} (severity: {s})\n", .{
                reg.field,
                @tagName(reg.severity),
            });
        }
    }
    if (manifest.property_gains.items.len > 0) {
        for (manifest.property_gains.items) |gain| {
            printProve("Property gained: {s}\n", .{gain.field});
        }
    }

    // Behavioral summary
    if (manifest.behavioral) |b| {
        printProve("Behaviors: {d} preserved, {d} added, {d} changed, {d} removed\n", .{
            b.preserved,
            b.added,
            b.response_changed,
            b.removed,
        });
    }

    // Verdict
    printProve("Verdict: {s}\n", .{manifest.verdict.toString()});
    if (manifest.justification.len > 0) {
        printProve("Reason: {s}\n", .{manifest.justification});
    }
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

fn printRaw(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
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
