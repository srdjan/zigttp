const std = @import("std");
const smt_solver = @import("smt_solver.zig");
const zts = @import("zts");

const zts_file_io = zts.file_io;

pub const ReceiptProbe = *const fn (std.mem.Allocator, zts.semantics_check.Receipt) void;

pub const RunContext = struct {
    receipt_probe: ?ReceiptProbe = null,
};

fn isHelpToken(arg: []const u8) bool {
    return std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "help");
}

fn appendFmt(allocator: std.mem.Allocator, buf: *std.ArrayList(u8), comptime fmt: []const u8, args: anytype) !void {
    const s = try std.fmt.allocPrint(allocator, fmt, args);
    defer allocator.free(s);
    try buf.appendSlice(allocator, s);
}

pub fn runSpecHashCommand(_: std.mem.Allocator, argv: []const []const u8) !void {
    var json_mode = false;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
        } else if (isHelpToken(arg)) {} else {
            return error.InvalidArgument;
        }
    }
    const hash = zts.semantics.semanticsHash();
    if (json_mode) {
        const allocator = std.heap.smp_allocator;
        const line = try std.fmt.allocPrint(allocator, "{{\"semanticsHash\":\"{s}\"}}\n", .{hash});
        defer allocator.free(line);
        _ = std.c.write(std.c.STDOUT_FILENO, line.ptr, line.len);
    } else {
        _ = std.c.write(std.c.STDOUT_FILENO, &hash, hash.len);
        _ = std.c.write(std.c.STDOUT_FILENO, "\n", 1);
    }
}

pub fn runSpecRenderCommand(_: std.mem.Allocator, argv: []const []const u8) !void {
    var out_path: ?[]const u8 = null;
    var check_path: ?[]const u8 = null;
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--out")) {
            i += 1;
            if (i >= argv.len) return error.MissingArgument;
            out_path = argv[i];
        } else if (std.mem.eql(u8, arg, "--check")) {
            i += 1;
            if (i >= argv.len) return error.MissingArgument;
            check_path = argv[i];
        } else if (isHelpToken(arg)) {} else {
            return error.InvalidArgument;
        }
    }

    const allocator = std.heap.smp_allocator;
    const rendered = try zts.semantics_render.renderSpecTs(allocator);
    defer allocator.free(rendered);

    if (check_path) |path| {
        // Drift gate: the committed artifact must match the current registry.
        const existing = zts_file_io.readFile(allocator, path, 1 << 20) catch {
            const msg = "spec-render --check: cannot read the spec artifact\n";
            _ = std.c.write(std.c.STDERR_FILENO, msg, msg.len);
            std.process.exit(2);
        };
        defer allocator.free(existing);
        if (!std.mem.eql(u8, existing, rendered)) {
            const msg = "spec-render --check: the committed spec is stale; run `zts spec-render --out <path>`\n";
            _ = std.c.write(std.c.STDERR_FILENO, msg, msg.len);
            std.process.exit(1);
        }
        const ok = "spec-render --check: spec is in sync\n";
        _ = std.c.write(std.c.STDOUT_FILENO, ok, ok.len);
        return;
    }

    if (out_path) |path| {
        try zts_file_io.writeFile(allocator, path, rendered);
        return;
    }

    if (rendered.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, rendered.ptr, rendered.len);
    }
}

fn appendJsonString(allocator: std.mem.Allocator, buf: *std.ArrayList(u8), s: []const u8) !void {
    try buf.append(allocator, '"');
    for (s) |c| {
        switch (c) {
            '"' => try buf.appendSlice(allocator, "\\\""),
            '\\' => try buf.appendSlice(allocator, "\\\\"),
            '\n' => try buf.appendSlice(allocator, "\\n"),
            else => try buf.append(allocator, c),
        }
    }
    try buf.append(allocator, '"');
}

const SpecCheckReport = struct {
    check: *const zts.semantics_check.CheckResult,
    corpus: *const zts.semantics_corpus.CorpusResult,
    smt: *const zts.semantics_check.SmtResult,
    audit: *const zts.semantics_check.AuditResult,
    /// Whether --audit was passed (so "not run" is distinguished from "z3 absent").
    audit_requested: bool = false,

    fn ok(self: SpecCheckReport) bool {
        return self.check.ok() and self.corpus.ok() and self.smt.ok() and self.audit.ok();
    }

    fn failureCount(self: SpecCheckReport) usize {
        return self.check.failures.items.len +
            self.corpus.failures.items.len +
            self.smt.failures.items.len +
            self.audit.failures.items.len;
    }
};

pub fn runSpecCheckCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    try runSpecCheckCommandWithContext(allocator, argv, .{});
}

pub fn runSpecCheckCommandWithContext(_: std.mem.Allocator, argv: []const []const u8, context: RunContext) !void {
    var json_mode = false;
    var want_audit = false;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
        } else if (std.mem.eql(u8, arg, "--audit")) {
            // The exclusion audit's f64-associativity refutation is slow (~5s),
            // so it is opt-in: off keeps interactive spec-check fast; CI
            // (scripts/verify.sh) passes --audit to gate the soundness boundary.
            want_audit = true;
        } else if (isHelpToken(arg)) {
            // no separate help screen
        } else {
            return error.InvalidArgument;
        }
    }

    const allocator = std.heap.smp_allocator;
    var result = zts.semantics_check.runCheck(allocator) catch {
        const msg = "spec-check: internal error\n";
        _ = std.c.write(std.c.STDERR_FILENO, msg, msg.len);
        std.process.exit(2);
    };
    defer result.deinit();

    // Mechanism 4: the differential corpus runs the registry's denotations
    // against the real compiler's output.
    var corpus = zts.semantics_corpus.runCorpus(allocator) catch {
        const msg = "spec-check: corpus internal error\n";
        _ = std.c.write(std.c.STDERR_FILENO, msg, msg.len);
        std.process.exit(2);
    };
    defer corpus.deinit();

    // Mechanism 5: SMT equivalence (Alive2-style refinement). z3 is injected only
    // when present, so spec-check still passes on its structural mechanisms where
    // no solver is installed (CI without z3, the wasm build).
    const z3_present = smt_solver.available(allocator);
    var smt = zts.semantics_check.runSmt(allocator, if (z3_present) smt_solver.solve else null) catch {
        const msg = "spec-check: smt internal error\n";
        _ = std.c.write(std.c.STDERR_FILENO, msg, msg.len);
        std.process.exit(2);
    };
    defer smt.deinit();

    // Exclusion audit: faithful-model refutation of the declared excluded laws
    // (the dual of mechanism 5). Opt-in (--audit) because the f64-associativity
    // refutation is slow; reuses the same injected solver when requested.
    var audit = zts.semantics_check.runAudit(allocator, if (want_audit and z3_present) smt_solver.solve else null) catch {
        const msg = "spec-check: audit internal error\n";
        _ = std.c.write(std.c.STDERR_FILENO, msg, msg.len);
        std.process.exit(2);
    };
    defer audit.deinit();

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    const report: SpecCheckReport = .{ .check = &result, .corpus = &corpus, .smt = &smt, .audit = &audit, .audit_requested = want_audit };
    if (json_mode) {
        try writeSpecCheckJson(allocator, &buf, report);
    } else {
        try writeSpecCheckText(allocator, &buf, report);
    }
    if (buf.items.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
    }

    const all_ok = report.ok();

    // Emit a signed kind=semantics receipt when the runtime supplied the
    // persistent signer. Only on a clean pass - a failing registry should not be
    // attested.
    if (all_ok) {
        if (context.receipt_probe) |probe| {
            const receipt = zts.semantics_check.buildReceiptFromInput(&result, .{
                .differential = .{ .passed = @intCast(corpus.cases_passed), .total = @intCast(corpus.cases_total) },
                .smt = .{ .available = smt.available, .proved = @intCast(smt.proved), .total = @intCast(smt.total) },
                .audit = .{ .available = audit.available, .refuted = @intCast(audit.refuted), .total = @intCast(audit.total) },
            });
            probe(allocator, receipt);
        }
    }

    // exit 0 conform, 1 divergence (2 for internal error handled above).
    if (!all_ok) std.process.exit(1);
}

fn writeSpecCheckText(allocator: std.mem.Allocator, buf: *std.ArrayList(u8), report: SpecCheckReport) !void {
    const cov = zts.semantics.coverage();
    const sh = zts.semantics.semanticsHash();
    const r = report.check;
    const corpus = report.corpus;
    const smt = report.smt;
    try appendFmt(allocator, buf, "zts semantics spec-check\n", .{});
    try appendFmt(allocator, buf, "  semantics hash:    {s}\n", .{sh});
    try appendFmt(allocator, buf, "  nodes specified:   {d}/{d}\n", .{ cov.nodes_specified, cov.nodes_total });
    try appendFmt(allocator, buf, "  opcodes specified: {d}/{d}\n", .{ cov.opcodes_specified, cov.opcodes_total });
    try appendFmt(allocator, buf, "  value proofs:      {d} (binop x{d}, unop x{d})\n", .{ r.nodes_proven, r.binop_instances, r.unop_instances });
    try appendFmt(allocator, buf, "  refinements:       {d}\n", .{r.refinements_proven});
    try appendFmt(allocator, buf, "  structural-only:   {d}\n", .{r.nodes_structural});
    try appendFmt(allocator, buf, "  stack-effect:      {d} checks\n", .{r.stack_effect_checked});
    try appendFmt(allocator, buf, "  differential:      {d}/{d} cases vs real codegen\n", .{ corpus.cases_passed, corpus.cases_total });
    if (smt.available) {
        if (smt.unproven > 0) {
            try appendFmt(allocator, buf, "  smt equivalence:   {d}/{d} proved, {d} unproven (solver could not decide/run) (z3)\n", .{ smt.proved, smt.total, smt.unproven });
        } else {
            try appendFmt(allocator, buf, "  smt equivalence:   {d}/{d} proved (z3)\n", .{ smt.proved, smt.total });
        }
    } else {
        try appendFmt(allocator, buf, "  smt equivalence:   skipped ({d} obligations; z3 not found)\n", .{smt.total});
    }
    const audit = report.audit;
    if (!report.audit_requested) {
        try appendFmt(allocator, buf, "  exclusion audit:   not run ({d} excluded laws; pass --audit to enable)\n", .{audit.total});
    } else if (audit.available) {
        if (audit.inconclusive > 0) {
            try appendFmt(allocator, buf, "  exclusion audit:   {d}/{d} refuted, {d} inconclusive (faithful model, z3)\n", .{ audit.refuted, audit.total, audit.inconclusive });
        } else {
            try appendFmt(allocator, buf, "  exclusion audit:   {d}/{d} excluded laws refuted (faithful model, z3)\n", .{ audit.refuted, audit.total });
        }
    } else {
        try appendFmt(allocator, buf, "  exclusion audit:   skipped ({d} excluded laws; z3 not found)\n", .{audit.total});
    }
    if (report.ok()) {
        try appendFmt(allocator, buf, "  result:            PASS\n", .{});
    } else {
        try appendFmt(allocator, buf, "  result:            FAIL ({d} divergence)\n", .{report.failureCount()});
        try appendSpecCheckFailuresText(allocator, buf, report);
    }
}

fn appendSpecCheckFailuresText(allocator: std.mem.Allocator, buf: *std.ArrayList(u8), report: SpecCheckReport) !void {
    for (report.check.failures.items) |f| {
        try appendFmt(allocator, buf, "    {s} {s}: {s}\n", .{ f.code.code(), f.where, f.message });
    }
    for (report.corpus.failures.items) |f| {
        try appendFmt(allocator, buf, "    {s} {s}: {s}\n", .{ zts.semantics.SpecCode.lowering_divergence.code(), f.where, f.message });
    }
    for (report.smt.failures.items) |f| {
        try appendFmt(allocator, buf, "    {s} {s}: {s}\n", .{ f.code.code(), f.where, f.message });
    }
    for (report.audit.failures.items) |f| {
        try appendFmt(allocator, buf, "    {s} {s}: {s}\n", .{ f.code.code(), f.where, f.message });
    }
}

fn writeSpecCheckJson(allocator: std.mem.Allocator, buf: *std.ArrayList(u8), report: SpecCheckReport) !void {
    const cov = zts.semantics.coverage();
    const sh = zts.semantics.semanticsHash();
    const ir_h = zts.semantics.irTableHash();
    const op_h = zts.semantics.opcodeTableHash();
    const r = report.check;
    const corpus = report.corpus;
    const smt = report.smt;
    try appendFmt(allocator, buf, "{{\"semanticsHash\":\"{s}\",\"irTableHash\":\"{s}\",\"opcodeTableHash\":\"{s}\",", .{ sh, ir_h, op_h });
    try appendFmt(allocator, buf, "\"nodes\":{{\"specified\":{d},\"total\":{d}}},\"opcodes\":{{\"specified\":{d},\"total\":{d}}},", .{ cov.nodes_specified, cov.nodes_total, cov.opcodes_specified, cov.opcodes_total });
    try appendFmt(allocator, buf, "\"valueProofs\":{d},\"binopInstances\":{d},\"unopInstances\":{d},\"refinements\":{d},\"structural\":{d},\"stackEffectChecks\":{d},", .{ r.nodes_proven, r.binop_instances, r.unop_instances, r.refinements_proven, r.nodes_structural, r.stack_effect_checked });
    try appendFmt(allocator, buf, "\"differential\":{{\"passed\":{d},\"total\":{d}}},", .{ corpus.cases_passed, corpus.cases_total });
    try appendFmt(allocator, buf, "\"smt\":{{\"available\":{s},\"proved\":{d},\"unproven\":{d},\"total\":{d}}},", .{ if (smt.available) "true" else "false", smt.proved, smt.unproven, smt.total });
    const audit = report.audit;
    try appendFmt(allocator, buf, "\"audit\":{{\"requested\":{s},\"available\":{s},\"refuted\":{d},\"inconclusive\":{d},\"total\":{d}}},", .{ if (report.audit_requested) "true" else "false", if (audit.available) "true" else "false", audit.refuted, audit.inconclusive, audit.total });
    try appendFmt(allocator, buf, "\"ok\":{s},\"failures\":[", .{if (report.ok()) "true" else "false"});
    try appendSpecCheckFailuresJson(allocator, buf, report);
    try appendFmt(allocator, buf, "]}}\n", .{});
}

fn appendJsonFailure(
    allocator: std.mem.Allocator,
    buf: *std.ArrayList(u8),
    first: *bool,
    code: []const u8,
    where: []const u8,
    message: []const u8,
) !void {
    if (!first.*) try buf.append(allocator, ',');
    first.* = false;
    try appendFmt(allocator, buf, "{{\"code\":\"{s}\",\"where\":\"{s}\",\"message\":", .{ code, where });
    try appendJsonString(allocator, buf, message);
    try buf.append(allocator, '}');
}

fn appendSpecCheckFailuresJson(allocator: std.mem.Allocator, buf: *std.ArrayList(u8), report: SpecCheckReport) !void {
    var first = true;
    for (report.check.failures.items) |f| {
        try appendJsonFailure(allocator, buf, &first, f.code.code(), f.where, f.message);
    }
    for (report.corpus.failures.items) |f| {
        try appendJsonFailure(allocator, buf, &first, zts.semantics.SpecCode.lowering_divergence.code(), f.where, f.message);
    }
    for (report.smt.failures.items) |f| {
        try appendJsonFailure(allocator, buf, &first, f.code.code(), f.where, f.message);
    }
    for (report.audit.failures.items) |f| {
        try appendJsonFailure(allocator, buf, &first, f.code.code(), f.where, f.message);
    }
}

test "spec-check renderers aggregate structural corpus and smt failures" {
    const allocator = std.testing.allocator;

    var check = zts.semantics_check.CheckResult{ .arena = std.heap.ArenaAllocator.init(allocator) };
    defer check.deinit();
    try check.failures.append(check.arena.allocator(), .{
        .code = .lowering_divergence,
        .where = "binary_op",
        .message = "lowering mismatch",
    });

    var corpus = zts.semantics_corpus.CorpusResult{ .arena = std.heap.ArenaAllocator.init(allocator) };
    defer corpus.deinit();
    corpus.cases_total = 2;
    corpus.cases_passed = 1;
    try corpus.failures.append(corpus.arena.allocator(), .{
        .where = "unary_op",
        .message = "real codegen mismatch",
    });

    var smt = zts.semantics_check.SmtResult{ .arena = std.heap.ArenaAllocator.init(allocator) };
    defer smt.deinit();
    smt.available = true;
    smt.proved = 1;
    smt.unproven = 1;
    smt.total = 3;
    try smt.failures.append(smt.arena.allocator(), .{
        .code = .smt_counterexample,
        .where = "smt_law",
        .message = "counterexample",
    });

    var audit = zts.semantics_check.AuditResult{ .arena = std.heap.ArenaAllocator.init(allocator) };
    defer audit.deinit();
    audit.available = true;
    audit.refuted = 2;
    audit.total = 3;
    try audit.failures.append(audit.arena.allocator(), .{
        .code = .excluded_law_holds,
        .where = "bad_exclusion",
        .message = "an excluded law actually holds",
    });

    const report: SpecCheckReport = .{ .check = &check, .corpus = &corpus, .smt = &smt, .audit = &audit, .audit_requested = true };
    try std.testing.expect(!report.ok());
    try std.testing.expectEqual(@as(usize, 4), report.failureCount());

    var json: std.ArrayList(u8) = .empty;
    defer json.deinit(allocator);
    try writeSpecCheckJson(allocator, &json, report);
    try std.testing.expect(std.mem.indexOf(u8, json.items, "\"ok\":false") != null);
    try std.testing.expect(std.mem.indexOf(u8, json.items, "\"where\":\"binary_op\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json.items, "\"where\":\"unary_op\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json.items, "\"where\":\"smt_law\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json.items, "\"where\":\"bad_exclusion\"") != null);

    var text: std.ArrayList(u8) = .empty;
    defer text.deinit(allocator);
    try writeSpecCheckText(allocator, &text, report);
    try std.testing.expect(std.mem.indexOf(u8, text.items, "FAIL (4 divergence)") != null);
    try std.testing.expect(std.mem.indexOf(u8, text.items, "binary_op: lowering mismatch") != null);
    try std.testing.expect(std.mem.indexOf(u8, text.items, "unary_op: real codegen mismatch") != null);
    try std.testing.expect(std.mem.indexOf(u8, text.items, "smt_law: counterexample") != null);
    try std.testing.expect(std.mem.indexOf(u8, text.items, "bad_exclusion: an excluded law actually holds") != null);
}
