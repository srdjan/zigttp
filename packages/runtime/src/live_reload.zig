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
const shared = @import("cli_shared.zig");
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
const spec_diagnostic = @import("deploy/spec_diagnostic.zig");
const proof_ledger = @import("proof_ledger.zig");
const proof_card_tui = @import("proof_card_tui.zig");
const keystroke_input = @import("keystroke_input.zig");
const printer_mod = @import("deploy/printer.zig");
const studio_mod = @import("runtime_features.zig").studio;
const counterexample_pipeline = @import("counterexample_pipeline.zig");
const json_diag = precompile.json_diag;

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
    /// True until the first proof card frame has been rendered. Drives the
    /// one-shot bold emphasis on the Studio mirror footer so the URL stands
    /// out the first time the HUD lights up, then settles into neutral.
    studio_footer_first_render: bool,
    /// Active proof-card lens (stored as enum tag; atomic for cross-thread reads).
    lens: std.atomic.Value(u8),
    /// Pre-rendered frames per lens so the keystroke thread can repaint
    /// from cache without re-running analysis. Indexed by `@intFromEnum`.
    lens_cache_mu: compat.Mutex,
    lens_cache: [4]?[]u8,
    keystroke_handle: ?*keystroke_input.Handle,

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
            .studio_footer_first_render = true,
            .lens = std.atomic.Value(u8).init(@intFromEnum(proof_card_tui.Lens.properties)),
            .lens_cache_mu = .{},
            .lens_cache = .{ null, null, null, null },
            .keystroke_handle = null,
        };
    }

    pub fn deinit(self: *LiveReloadState) void {
        if (self.keystroke_handle) |h| keystroke_input.uninstall(h);
        self.lens_cache_mu.lock();
        defer self.lens_cache_mu.unlock();
        for (&self.lens_cache) |*slot| {
            if (slot.*) |bytes| self.allocator.free(bytes);
            slot.* = null;
        }
        if (self.current_contract) |*c| c.deinit(self.allocator);
        if (self.previous_code) |code| self.allocator.free(code);
    }

    /// Read the active lens. Clamped against the enum tag range so adding
    /// a fourth variant later cannot panic on out-of-range stores.
    pub fn currentLens(self: *const LiveReloadState) proof_card_tui.Lens {
        const raw = self.lens.load(.acquire);
        const max = @typeInfo(proof_card_tui.Lens).@"enum".fields.len;
        const clamped: u8 = if (raw >= max) 0 else raw;
        return @enumFromInt(clamped);
    }

    /// Run the watch-reload loop. Blocks until the server stops.
    pub fn run(self: *LiveReloadState, io: std.Io) !void {
        self.previous_stamp = try computeStamp(io, self.watch_paths);
        // Install the keystroke listener before the first render so the very
        // first proof card already advertises Tab. No-op on non-TTY.
        self.keystroke_handle = keystroke_input.install(self.allocator, self, onKeystroke);
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

    /// Run analysis on pre-read source, cleaning up CheckResult. Runs with
    /// `json_mode = true` so structured diagnostics flow into studio; the
    /// terminal HUD reformats them itself via `printDiagnosticLines`.
    fn runAnalysisFromSource(self: *LiveReloadState, source: []const u8, skip_contract: bool) ?AnalysisResult {
        var result = precompile.runCheckOnlyFromSource(
            self.allocator,
            source,
            self.handler_path,
            self.config.sql_schema_path,
            true,
            self.config.system_path,
            skip_contract,
        ) catch return null;

        // Deep-copy diagnostics first so that on failure here, `result.deinit`
        // still frees the contract (we have not stolen it yet).
        const diagnostics = ownDiagnostics(self.allocator, result.json_diagnostics.items) catch {
            result.deinit(self.allocator);
            return null;
        };

        const contract = result.contract;
        result.contract = null;
        const errors = result.totalErrors();
        const pinned_regressions = result.pinned_witness_regressions;
        result.deinit(self.allocator);

        return .{
            .contract = contract,
            .errors = errors,
            .pinned_regressions = pinned_regressions,
            .diagnostics = diagnostics,
        };
    }

    const AnalysisResult = struct {
        contract: ?HandlerContract,
        errors: u32,
        pinned_regressions: usize,
        diagnostics: []studio_mod.Diagnostic,

        fn deinitContract(self: *AnalysisResult, allocator: std.mem.Allocator) void {
            if (self.contract) |*c| c.deinit(allocator);
            self.contract = null;
        }

        fn deinitDiagnostics(self: *AnalysisResult, allocator: std.mem.Allocator) void {
            for (self.diagnostics) |*d| d.deinit(allocator);
            allocator.free(self.diagnostics);
            self.diagnostics = &.{};
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
        defer analysis.deinitDiagnostics(self.allocator);

        const elapsed_ms = if (timer) |*t| t.read() / std.time.ns_per_ms else 0;

        if (analysis.errors > 0) {
            if (self.server.studio) |*studio| studio.updateDiagnostics("compilation failed", analysis.diagnostics);
            printReload("Compilation failed ({d} error(s), {d}ms). Keeping previous handler.\n", .{
                analysis.errors,
                elapsed_ms,
            });
            printDiagnosticLines(analysis.diagnostics, new_code);
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

                const verdict = manifest.verdict;
                const should_swap = verdict == .safe or verdict == .safe_with_additions or self.config.force_swap;
                if (should_swap) {
                    if (verdict != .safe and verdict != .safe_with_additions) {
                        printProve("Verdict: {s}. Applying anyway (--force-swap).\n", .{verdict.toString()});
                    }
                    self.tryLogSwap(&new_contract, &sha_hex);
                    self.updateCurrentContract(new_contract);
                    self.doSwap(&new_code, true);
                    self.refreshDevAttestation(self.previous_code orelse "");
                    if (self.current_contract) |*current| self.renderHud(current, null, elapsed_ms, &sha_hex);
                } else {
                    self.renderHud(&new_contract, old_contract, elapsed_ms, &sha_hex);
                    printProve("Swap blocked. Fix the breaking change or use --force-swap.\n", .{});
                    new_contract.deinit(self.allocator);
                }
            } else {
                self.tryLogSwap(&new_contract, &sha_hex);
                self.updateCurrentContract(new_contract);
                self.doSwap(&new_code, true);
                self.refreshDevAttestation(self.previous_code orelse "");
                if (self.current_contract) |*current| self.renderHud(current, null, elapsed_ms, &sha_hex);
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
        defer analysis.deinitDiagnostics(self.allocator);

        const elapsed_ms = if (timer) |*t| t.read() / std.time.ns_per_ms else 0;
        if (analysis.errors > 0) {
            if (self.server.studio) |*studio| studio.updateDiagnostics("initial compilation failed", analysis.diagnostics);
            printProve("Initial proof failed ({d} error(s), {d}ms).\n", .{ analysis.errors, elapsed_ms });
            printDiagnosticLines(analysis.diagnostics, source);
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

        self.tryLogSwap(&contract, &sha_hex);
        self.updateCurrentContract(contract);
        self.installRuntimeContract();
        self.refreshDevAttestation(source);
        if (self.current_contract) |*current| self.renderHud(current, null, elapsed_ms, &sha_hex);
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

    fn refreshDevAttestation(self: *LiveReloadState, source: []const u8) void {
        if (!self.server.config.studio or !self.config.prove or source.len == 0) return;

        var compiled = precompile.compileHandler(self.allocator, source, self.handler_path, .{
            .emit_verify = true,
            .emit_contract = true,
            .sql_schema_path = self.config.sql_schema_path,
            .system_path = self.config.system_path,
        }) catch |err| {
            printProve("Caller receipt skipped: compile failed: {}.\n", .{err});
            return;
        };
        defer compiled.deinit(self.allocator);

        if (compiled.verify_failed or compiled.bytecode.len == 0) {
            printProve("Caller receipt skipped: no verified bytecode.\n", .{});
            return;
        }
        const contract = if (compiled.contract) |*c| c else {
            printProve("Caller receipt skipped: no contract emitted.\n", .{});
            return;
        };
        std.crypto.hash.sha2.Sha256.hash(compiled.bytecode, &contract.artifact_sha256, .{});

        var json_output: std.ArrayList(u8) = .empty;
        defer json_output.deinit(self.allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(self.allocator, &json_output);
        zigts.writeContractJson(contract, &aw.writer) catch |err| {
            printProve("Caller receipt skipped: contract JSON failed: {}.\n", .{err});
            return;
        };
        json_output = aw.toArrayList();
        if (json_output.items.len == 0) return;

        self.server.installDevAttestation(json_output.items, compiled.bytecode, contract) catch |err| {
            printProve("Caller receipt skipped: attestation failed: {}.\n", .{err});
            return;
        };
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

        appendLedgerEntry(self.allocator, .swap, contract, self.handler_path, sha_hex, null) catch |err| {
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

        // PropertyCauseEntry borrows from the static `property_metas` field
        // names and the contract's static snippets, so the slice lives only
        // as long as new_contract on this stack frame.
        var causes_buf: [4]review_facts_mod.PropertyCauseEntry = undefined;
        var n_causes: usize = 0;
        if (new_contract.property_provenance.causeFor("deterministic")) |c| {
            causes_buf[n_causes] = .{
                .field = "deterministic",
                .line = c.line,
                .column = c.column,
                .snippet = c.snippet,
            };
            n_causes += 1;
        }

        const cx_preview = counterexample_pipeline.buildFromDelta(
            self.handler_path,
            &new_contract.property_provenance,
            &delta,
        );

        if (self.server.studio) |*studio| {
            studio.updateFacts(.{
                .facts = &new_facts,
                .baseline = baseline_ptr,
                .delta = &delta,
                .recompile_ms = elapsed_ms,
                .counterexample = cx_preview,
                .property_causes = causes_buf[0..n_causes],
            });
            studio.broadcast();
        }

        const card = review_facts_mod.ProofCard{
            .handler_path = self.handler_path,
            .service_name = "dev",
            .region = "local",
            .current = &new_facts,
            .baseline = baseline_ptr,
            .delta = &delta,
            .property_causes = causes_buf[0..n_causes],
            .counterexample = cx_preview,
        };

        const recompile_ms: u32 = @intCast(@min(elapsed_ms, std.math.maxInt(u32)));
        try self.refreshLensCache(&card, recompile_ms);

        const active = self.currentLens();
        var stderr_buf: [16 * 1024]u8 = undefined;
        var stderr_writer = printer_mod.FdWriter.init(std.c.STDERR_FILENO, stderr_buf[0..]);
        try self.writeCachedFrame(active, &stderr_writer.interface);
        try writeLensHint(&stderr_writer.interface, active, shared.stderrIsTty());
        if (self.server.config.studio) {
            const first = self.studio_footer_first_render;
            proof_card_tui.writeStudioFooter(&stderr_writer.interface, .{
                .host = self.server.config.host,
                .port = self.server.config.port,
                .first_time = first,
                .tty = shared.stderrIsTty(),
            }) catch {};
            self.studio_footer_first_render = false;
        }
        stderr_writer.interface.flush() catch {};
    }

    /// Render every lens against the same card and swap the cache under
    /// the mutex so the keystroke thread sees a consistent set.
    fn refreshLensCache(
        self: *LiveReloadState,
        card: *const review_facts_mod.ProofCard,
        recompile_ms: u32,
    ) !void {
        var rendered: [4]?[]u8 = .{ null, null, null, null };
        errdefer for (&rendered) |*slot| if (slot.*) |b| self.allocator.free(b);

        const caller_view = self.buildCallerView();

        const lenses = [_]proof_card_tui.Lens{ .properties, .trade, .handover, .caller_view };
        for (lenses, 0..) |lens, idx| {
            var aw: std.Io.Writer.Allocating = .init(self.allocator);
            defer aw.deinit();
            try proof_card_tui.writeProofCardFrame(self.allocator, card, &aw.writer, .{
                .recompile_ms = recompile_ms,
                .lens = lens,
                .caller_view = caller_view,
            });
            rendered[idx] = try self.allocator.dupe(u8, aw.writer.buffered());
        }

        self.lens_cache_mu.lock();
        defer self.lens_cache_mu.unlock();
        for (&self.lens_cache, 0..) |*slot, idx| {
            if (slot.*) |old| self.allocator.free(old);
            slot.* = rendered[idx];
            rendered[idx] = null;
        }
    }

    /// Snapshot the running server's attestation state into a CallerView
    /// for the `.caller_view` lens. Returns null on unattested builds; the
    /// pane renders the opt-out notice in that case. Borrowed strings; the
    /// CallerView is valid only for the duration of this render cycle, which
    /// is fine because the cache stores the rendered bytes, not the struct.
    fn buildCallerView(self: *const LiveReloadState) ?proof_card_tui.CallerView {
        const hs = self.server.attestation_headers orelse return null;
        const fp = self.server.signer_fingerprint_hex orelse return null;
        return .{
            .proofs_header_value = hs.proofs_value,
            .attest_header_value = hs.attest_value,
            .key_fingerprint_hex = &fp,
            .host = self.server.config.host,
            .port = self.server.config.port,
        };
    }

    /// Silent no-op when the cache is empty (no prove cycle has landed yet).
    fn writeCachedFrame(
        self: *LiveReloadState,
        lens: proof_card_tui.Lens,
        writer: *std.Io.Writer,
    ) !void {
        self.lens_cache_mu.lock();
        defer self.lens_cache_mu.unlock();
        const slot = self.lens_cache[@intFromEnum(lens)] orelse return;
        try writer.writeAll(slot);
    }

    /// Tab (0x09) and `l`/`L` (fallback when a terminal intercepts Tab)
    /// rotate the lens; other bytes are ignored.
    fn onKeystroke(ctx: *anyopaque, byte: u8) void {
        const self: *LiveReloadState = @ptrCast(@alignCast(ctx));
        switch (byte) {
            0x09, 'l', 'L' => self.rotateLens(),
            else => {},
        }
    }

    pub fn rotateLens(self: *LiveReloadState) void {
        const current = self.currentLens();
        const next = current.next();
        self.lens.store(@intFromEnum(next), .release);

        var stderr_buf: [16 * 1024]u8 = undefined;
        var stderr_writer = printer_mod.FdWriter.init(std.c.STDERR_FILENO, stderr_buf[0..]);
        self.writeCachedFrame(next, &stderr_writer.interface) catch return;
        writeLensHint(&stderr_writer.interface, next, shared.stderrIsTty()) catch {};
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

fn buildSpecStates(
    allocator: std.mem.Allocator,
    contract: *const HandlerContract,
) ![]review_facts_mod.SpecState {
    const decls = contract.declared_specs.items;
    const out = try allocator.alloc(review_facts_mod.SpecState, decls.len);
    errdefer allocator.free(out);
    for (decls, 0..) |name, i| {
        out[i] = spec_diagnostic.specStateFor(contract, name);
    }
    return out;
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

fn mentionsAiRelevantProof(proof_unlocked: []const u8) bool {
    return std.mem.indexOf(u8, proof_unlocked, "deterministic") != null or
        std.mem.indexOf(u8, proof_unlocked, "Result") != null or
        std.mem.indexOf(u8, proof_unlocked, "replayable") != null;
}

fn writeLensHint(
    writer: *std.Io.Writer,
    lens: proof_card_tui.Lens,
    tty: bool,
) !void {
    const c = shared.palette(tty);
    try writer.print("{s}  Lens: {s}   (tab to rotate){s}\n", .{ c.dim, lens.label(), c.reset });
}

/// Render compile errors to stderr. Restriction-class diagnostics (ZTS001) get
/// a framed why/buys/try block so first-time authors read the cut as deliberate
/// design rather than a linter rule. `source`, when non-null, gates the
/// in-frame code excerpt; pass the same buffer that was just analyzed.
fn printDiagnosticLines(diagnostics: []const studio_mod.Diagnostic, source: ?[]const u8) void {
    const tty = shared.stderrIsTty();
    var stderr_buf: [8 * 1024]u8 = undefined;
    var stderr_writer = printer_mod.FdWriter.init(std.c.STDERR_FILENO, stderr_buf[0..]);
    writeDiagnosticLines(&stderr_writer.interface, diagnostics, source, tty) catch {};
    stderr_writer.interface.flush() catch {};
}

fn writeDiagnosticLines(
    writer: *std.Io.Writer,
    diagnostics: []const studio_mod.Diagnostic,
    source: ?[]const u8,
    tty: bool,
) !void {
    for (diagnostics) |d| {
        if (std.mem.eql(u8, d.code, "ZTS001")) {
            try writeRestrictionFrame(writer, d, source, tty);
            continue;
        }
        try writer.print("  {s}:{d}:{d}: {s}: {s}\n", .{ d.file, d.line, d.column, d.code, d.message });
        if (d.suggestion) |s| try writer.print("      hint: {s}\n", .{s});
    }
}

fn writeRestrictionFrame(
    writer: *std.Io.Writer,
    d: studio_mod.Diagnostic,
    source: ?[]const u8,
    tty: bool,
) !void {
    const c = shared.palette(tty);

    const feature = json_diag.extractFeatureFromMessage(d.message);
    const info: ?json_diag.RestrictionInfo = if (feature) |name| json_diag.lookupRestriction(name) else null;
    const display_name = if (info) |i| i.name else feature orelse "";

    try writer.writeByte('\n');
    try writer.print("  {s}restriction{s} {s}{s}{s}\n", .{ c.red, c.reset, c.bold, display_name, c.reset });
    try writer.print("  {s}----------------------------------------------------------------------{s}\n", .{ c.dim, c.reset });
    try writer.print("  {s}{s}:{d}:{d}{s}\n", .{ c.dim, d.file, d.line, d.column, c.reset });

    if (source) |src| {
        if (zigts.bool_checker.getSourceLine(src, d.line)) |line_text| {
            try writer.print("    {d} | {s}\n", .{ d.line, line_text });
        }
    }

    if (info) |i| {
        try writer.print("  {s}why{s}    {s}\n", .{ c.bold, c.reset, i.blocked_reason });
        // Restrictions that buy determinism, Result narrowing, or replayable
        // behavior get a quiet AI-substrate tail. These are exactly the
        // properties an AI coding agent needs in order to safely refactor
        // around the handler without re-verifying every effect.
        const ai_tail: []const u8 = if (mentionsAiRelevantProof(i.proof_unlocked))
            " (also: an AI assistant can refactor against this)"
        else
            "";
        try writer.print("  {s}buys{s}   removing this unlocks: {s}{s}\n", .{ c.green, c.reset, i.proof_unlocked, ai_tail });
        try writer.print("  {s}try{s}    {s}\n", .{ c.yellow, c.reset, i.alternative });
    } else if (d.suggestion) |s| {
        try writer.print("  {s}try{s}    {s}\n", .{ c.yellow, c.reset, s });
    }
    try writer.writeByte('\n');
}

/// Dupe each parser/checker JsonDiagnostic into an owned studio.Diagnostic
/// so the slice survives `CheckResult.deinit`. On allocation failure mid-
/// loop, frees what has already been duped and propagates `error.OutOfMemory`.
fn ownDiagnostics(allocator: std.mem.Allocator, source: []const json_diag.JsonDiagnostic) ![]studio_mod.Diagnostic {
    var owned = try allocator.alloc(studio_mod.Diagnostic, source.len);
    var filled: usize = 0;
    errdefer {
        var i: usize = 0;
        while (i < filled) : (i += 1) owned[i].deinit(allocator);
        allocator.free(owned);
    }
    while (filled < source.len) : (filled += 1) {
        const src = source[filled];
        owned[filled] = .{
            .code = try allocator.dupe(u8, src.code),
            .severity = try allocator.dupe(u8, src.severity),
            .file = try allocator.dupe(u8, src.file),
            .line = src.line,
            .column = src.column,
            .message = try allocator.dupe(u8, src.message),
            .suggestion = if (src.suggestion) |s| try allocator.dupe(u8, s) else null,
        };
    }
    return owned;
}

/// Project the contract into ReviewFacts and append a row to
/// `.zigttp/proofs.jsonl`. `sha_hex` is the source-bytes sha256 hex
/// (callers that have already computed it for the HUD pass it in).
pub fn appendLedgerEntry(
    allocator: std.mem.Allocator,
    kind: proof_ledger.EventKind,
    contract: *const HandlerContract,
    handler_path: []const u8,
    sha_hex: []const u8,
    service_name: ?[]const u8,
) !void {
    var facts = try factsFromContract(allocator, contract, sha_hex);
    defer facts.deinit(allocator);

    try proof_ledger.appendEvent(allocator, .{
        .kind = kind,
        .facts = &facts,
        .handler_path = handler_path,
        .service_name = service_name,
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

test "writeDiagnosticLines: ZTS001 renders framed restriction rationale" {
    const allocator = std.testing.allocator;
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();

    const diagnostics = [_]studio_mod.Diagnostic{.{
        .code = @constCast("ZTS001"),
        .severity = @constCast("error"),
        .file = @constCast("handler.ts"),
        .line = 2,
        .column = 5,
        .message = @constCast("'try/catch' is not supported"),
        .suggestion = @constCast("use Result types for error handling"),
    }};
    const source =
        \\function handler(req) {
        \\    try {
        \\        return Response.text("ok");
        \\    } catch (err) {
        \\        return Response.text("bad", { status: 500 });
        \\    }
        \\}
    ;

    try writeDiagnosticLines(&aw.writer, &diagnostics, source, false);
    const out = aw.writer.buffered();
    try std.testing.expect(std.mem.indexOf(u8, out, "restriction try/catch") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "handler.ts:2:5") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "2 |     try {") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "why    exceptions are an invisible second return channel") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "buys   removing this unlocks: Result narrowing") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "try    use Result types and check .ok") != null);
}

test "writeDiagnosticLines: unknown ZTS001 falls back to suggestion" {
    const allocator = std.testing.allocator;
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();

    const diagnostics = [_]studio_mod.Diagnostic{.{
        .code = @constCast("ZTS001"),
        .severity = @constCast("error"),
        .file = @constCast("handler.ts"),
        .line = 1,
        .column = 1,
        .message = @constCast("'future syntax' is not supported"),
        .suggestion = @constCast("rewrite it explicitly"),
    }};

    try writeDiagnosticLines(&aw.writer, &diagnostics, null, false);
    const out = aw.writer.buffered();
    try std.testing.expect(std.mem.indexOf(u8, out, "restriction future syntax") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "try    rewrite it explicitly") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "why    ") == null);
    try std.testing.expect(std.mem.indexOf(u8, out, "buys   ") == null);
}

test "writeDiagnosticLines: non-ZTS001 keeps compact diagnostic form" {
    const allocator = std.testing.allocator;
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();

    const diagnostics = [_]studio_mod.Diagnostic{.{
        .code = @constCast("ZTS308"),
        .severity = @constCast("error"),
        .file = @constCast("handler.ts"),
        .line = 7,
        .column = 11,
        .message = @constCast("unchecked optional use"),
        .suggestion = @constCast("guard with !== undefined"),
    }};

    try writeDiagnosticLines(&aw.writer, &diagnostics, null, false);
    try std.testing.expectEqualStrings(
        "  handler.ts:7:11: ZTS308: unchecked optional use\n      hint: guard with !== undefined\n",
        aw.writer.buffered(),
    );
}
