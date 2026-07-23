//! CheckResult data type plus presentation/codegen helpers
//! (formatProofCard, generateTypeDefs). The orchestrators that produce
//! CheckResult values live in precompile.zig.

const std = @import("std");
const builtin = @import("builtin");
const zts = @import("zts");

const handler_contract = zts.handler_contract;
const HandlerContract = handler_contract.HandlerContract;
const json_diag = @import("json_diagnostics.zig");

pub const CheckResult = struct {
    line_count: u32 = 0,
    parse_errors: u32 = 0,
    bool_specializations: u32 = 0,
    bool_errors: u32 = 0,
    bool_warnings: u32 = 0,
    type_errors: u32 = 0,
    strict_errors: u32 = 0,
    strict_warnings: u32 = 0,
    is_typescript: bool = false,
    verify_ran: bool = false,
    verify_errors: u32 = 0,
    verify_warnings: u32 = 0,
    flow_errors: u32 = 0,
    flow_warnings: u32 = 0,
    canonical_errors: u32 = 0,
    exhaustive_returns: bool = false,
    results_safe: bool = false,
    optionals_safe: bool = false,
    state_isolated: bool = true,
    no_unreachable: bool = true,
    paths_enumerated: u32 = 0,
    paths_exhaustive: bool = false,
    max_io_depth: ?u32 = null,
    fault_total: u32 = 0,
    fault_covered: u32 = 0,
    properties: ?handler_contract.HandlerProperties = null,
    contract: ?HandlerContract = null,
    /// Structured diagnostics for JSON output mode.
    json_diagnostics: std.ArrayList(json_diag.JsonDiagnostic) = .empty,
    /// Count of pinned-witness regressions re-fired by this build. Surfaced
    /// by `--watch --prove` as a HUD-adjacent toast so authors notice when
    /// a defended-against pattern reappears.
    pinned_witness_regressions: usize = 0,
    /// Pre-rendered `proofTrace` JSON object: per-property reasoning for the
    /// proof card (how each proof was discharged, or the counterexample that
    /// broke it). Null when no contract was produced. Owned by CheckResult.
    proof_trace_json: ?[]u8 = null,

    pub fn totalErrors(self: *const CheckResult) u32 {
        return self.parse_errors + self.bool_errors + self.type_errors + self.strict_errors + self.verify_errors + self.flow_errors + self.canonical_errors + self.specErrors();
    }

    pub fn totalWarnings(self: *const CheckResult) u32 {
        return self.bool_warnings + self.strict_warnings + self.verify_warnings + self.flow_warnings + self.specWarnings();
    }

    pub fn deinit(self: *CheckResult, allocator: std.mem.Allocator) void {
        if (self.contract) |*c| c.deinit(allocator);
        // Checker diagnostics own a heap copy of their message (duped at
        // capture so it outlives the checker allocator); free those here.
        // Static-string messages keep `message_owned` false and are skipped.
        for (self.json_diagnostics.items) |*d| d.deinit(allocator);
        self.json_diagnostics.deinit(allocator);
        if (self.proof_trace_json) |ptj| allocator.free(ptj);
    }

    /// Error-severity spec diagnostics: handler Spec ZTS500/501/502, helper
    /// capsule ZTS500/502, ZTS606, and the error-level Effects diagnostics
    /// (ZTS503/504/506/607). Warning-level entries are excluded so an
    /// over-declared `Effects<...>` ceiling never fails a build.
    fn specErrors(self: *const CheckResult) u32 {
        const contract = if (self.contract) |*c| c else return 0;
        var n: u32 = 0;
        for (contract.spec_diagnostics.items) |d| {
            if (d.kind.severity() == .err) n += 1;
        }
        return n;
    }

    /// Warning-severity spec diagnostics: ZTS505 over-declaration and the
    /// docs-mode ZTS507/508 missing-capsule prompts.
    fn specWarnings(self: *const CheckResult) u32 {
        const contract = if (self.contract) |*c| c else return 0;
        var n: u32 = 0;
        for (contract.spec_diagnostics.items) |d| {
            if (d.kind.severity() == .warn) n += 1;
        }
        return n;
    }
};

/// Walk the flow_checker diagnostics, materialise a counterexample witness
/// for each one that maps to a tracked PropertyTag, and persist it to
/// `.zttp/witnesses/<short_hash>/`. Failures here never block the
/// analysis result; the corpus is best-effort persistence. Returns the
/// number of pinned witnesses re-fired by this build, which the live
/// reload HUD surfaces as a regression toast.
///
/// The corpus is filesystem state, so freestanding/wasm analyzer builds skip
/// it: `persistFlowWitnessesNative` is referenced only inside the comptime
/// branch below, keeping the `witness_corpus` subtree out of the module graph.
pub fn persistFlowWitnesses(
    allocator: std.mem.Allocator,
    flow_diags: []const zts.flow_checker.Diagnostic,
    ir_view: zts.parser.IrView,
    handler_path: []const u8,
) usize {
    if (comptime builtin.target.os.tag != .freestanding) {
        return persistFlowWitnessesNative(allocator, flow_diags, ir_view, handler_path);
    }
    return 0;
}

fn persistFlowWitnessesNative(
    allocator: std.mem.Allocator,
    flow_diags: []const zts.flow_checker.Diagnostic,
    ir_view: zts.parser.IrView,
    handler_path: []const u8,
) usize {
    if (flow_diags.len == 0) return 0;

    const corpus_dir = zts.witness_corpus.corpusDir(allocator, handler_path) catch return 0;
    defer allocator.free(corpus_dir);
    zts.witness_corpus.ensureCorpusDir(allocator, corpus_dir, handler_path) catch return 0;

    var pinned_regressions: usize = 0;
    for (flow_diags) |diag| {
        const tag = zts.flow_checker.propertyTagForKind(diag.kind) orelse continue;
        const loc = ir_view.getLoc(diag.node) orelse continue;
        const constraints: []const zts.counterexample.WitnessConstraint =
            if (diag.witness) |wit| wit.path_constraints else &.{};
        const io_calls: []const zts.counterexample.TrackedIoCall =
            if (diag.witness) |wit| wit.io_calls else &.{};

        var witness = zts.counterexample.solve(allocator, .{
            .property = tag,
            .origin = .{ .line = loc.line, .column = loc.column },
            .sink = .{ .line = loc.line, .column = loc.column },
            .summary = diag.message,
            .constraints = constraints,
            .io_calls = io_calls,
        }) catch continue;
        defer witness.deinit(allocator);

        if (zts.witness_corpus.persist(allocator, corpus_dir, witness)) |pres| {
            var owned = pres;
            defer owned.deinit(allocator);
            // .refreshed means the witness was already persisted on a prior
            // build. If the author had pinned it, this build re-fires a
            // regression they explicitly chose to defend against.
            if (owned.outcome == .refreshed and
                zts.witness_corpus.isPinned(allocator, corpus_dir, owned.key))
            {
                pinned_regressions += 1;
            }
        } else |_| {}
    }
    return pinned_regressions;
}

pub fn syncCheckResultProperties(result: *CheckResult) void {
    if (result.contract) |*contract| {
        result.properties = contract.properties;
    } else {
        result.properties = null;
    }
}

pub fn appendSpecDiagnosticsJson(
    allocator: std.mem.Allocator,
    result: *CheckResult,
    handler_path: []const u8,
) void {
    const contract = if (result.contract) |*c| c else return;
    for (contract.spec_diagnostics.items) |diag| {
        const code: []const u8 = diag.kind.code();
        // Single source of truth shared with the human card (formatProofCard)
        // so the two surfaces cannot drift.
        const message: []const u8 = specDiagnosticMessage(diag);
        result.json_diagnostics.append(allocator, .{
            .code = code,
            .severity = if (diag.kind.severity() == .warn) "warning" else "error",
            .message = message,
            .file = handler_path,
            .line = contract.handler.line,
            .column = @intCast(@min(contract.handler.column, std.math.maxInt(u16))),
            .suggestion = diag.suggestion,
        }) catch {};
    }
}

/// Opt-in docs mode: ask every exported helper to carry an explicit
/// `Effects<...>` capsule (ZTS507) and `Proof<...>` capsule (ZTS508). Off by
/// default and warning-only - it never fails a build. Diagnostics are
/// appended to `result.json_diagnostics`; non-exported helpers are untouched.
pub fn appendExportCapsuleDiagnostics(
    allocator: std.mem.Allocator,
    result: *CheckResult,
    handler_path: []const u8,
) void {
    const contract = if (result.contract) |*c| c else return;
    for (contract.function_effect_capsules.items) |cap| {
        if (!cap.exported or cap.declared.items.len > 0) continue;
        result.json_diagnostics.append(allocator, .{
            .code = "ZTS507",
            .severity = "warning",
            .message = "exported helper carries no Effects<...> capsule",
            .file = handler_path,
            .line = cap.line,
            .column = 0,
            .suggestion = "annotate the exported helper's return type with `Effects<T, \"...\">` to document its capability ceiling.",
        }) catch {};
    }
    for (contract.function_capsules.items) |cap| {
        if (!cap.exported or cap.declared.items.len > 0) continue;
        result.json_diagnostics.append(allocator, .{
            .code = "ZTS508",
            .severity = "warning",
            .message = "exported helper carries no Proof<...> capsule",
            .file = handler_path,
            .line = cap.line,
            .column = 0,
            .suggestion = "annotate the exported helper's return type with `Proof<T, \"...\">` to document its proven properties.",
        }) catch {};
    }
}

/// Default canonical profile: public helpers that participate in capability
/// or declared-proof paths carry explicit capsules.
pub fn appendCanonicalPublicHelperDiagnostics(
    allocator: std.mem.Allocator,
    result: *CheckResult,
    handler_path: []const u8,
) void {
    const contract = if (result.contract) |*c| c else return;

    for (contract.function_effect_capsules.items) |cap| {
        if (!cap.exported or !cap.handler_reachable or cap.declared.items.len > 0 or cap.inferred.items.len == 0) continue;
        result.json_diagnostics.append(allocator, .{
            .code = "ZTS610",
            .severity = "error",
            .message = "public helper reaches capabilities and should declare Effects<...>",
            .file = handler_path,
            .line = cap.line,
            .column = 0,
            .suggestion = "annotate the exported helper's return type with `Effects<T, \"...\">` for the reached capabilities.",
        }) catch {};
        result.canonical_errors += 1;
    }

    if (!declaresProofSupportedSpec(contract.declared_specs.items)) return;
    for (contract.function_capsules.items) |cap| {
        if (!cap.exported or !cap.handler_reachable or cap.declared.items.len > 0) continue;
        result.json_diagnostics.append(allocator, .{
            .code = "ZTS611",
            .severity = "error",
            .message = "public helper participates in declared specs and should declare Proof<...>",
            .file = handler_path,
            .line = cap.line,
            .column = 0,
            .suggestion = "annotate the exported helper's return type with `Proof<T, \"...\">` for the declared proof properties it preserves.",
        }) catch {};
        result.canonical_errors += 1;
    }
}

fn declaresProofSupportedSpec(specs: []const []const u8) bool {
    for (specs) |name| {
        if (std.mem.eql(u8, name, "total") or
            std.mem.eql(u8, name, "pure") or
            std.mem.eql(u8, name, "read_only") or
            std.mem.eql(u8, name, "deterministic"))
        {
            return true;
        }
    }
    return false;
}

pub fn refreshSpecDiagnostics(allocator: std.mem.Allocator, result: *CheckResult) !void {
    const contract = if (result.contract) |*c| c else return;

    // Re-discharge the handler's Spec<...> obligations against the freshly
    // classified properties. Capsule diagnostics - those carrying a
    // `function` - come from proof-carrying-function discharge, which this
    // refresh does not re-run; handler-level Effects diagnostics also come
    // from the capability-budget pass. Clone them across so helper
    // ZTS500/ZTS502/ZTS606 and handler ZTS504/ZTS506 entries survive. The
    // clone (rather than a move) keeps `contract.spec_diagnostics` intact
    // until the swap, so an OOM mid-loop leaves both lists individually
    // consistent.
    var refreshed = try zts.spec_discharge.dischargeSpecs(
        allocator,
        contract.declared_specs.items,
        contract.properties,
        contract.modules.items,
        contract.declared_specs_implicit,
    );
    errdefer {
        for (refreshed.items) |*d| d.deinit(allocator);
        refreshed.deinit(allocator);
    }
    for (contract.spec_diagnostics.items) |*diag| {
        if (preserveDuringSpecRefresh(diag.*)) {
            try refreshed.append(allocator, try diag.clone(allocator));
        }
    }
    for (contract.spec_diagnostics.items) |*diag| diag.deinit(allocator);
    contract.spec_diagnostics.deinit(allocator);
    contract.spec_diagnostics = refreshed;
}

fn preserveDuringSpecRefresh(diag: handler_contract.SpecDiagnostic) bool {
    if (diag.function != null) return true;
    return switch (diag.kind) {
        .not_discharged,
        .incompatible_with_import,
        .unknown_name,
        => false,
        else => true,
    };
}

/// Format a structured proof card showing what the compiler proved.
pub fn formatProofCard(writer: anytype, r: *const CheckResult, filename: []const u8) void {
    writer.print("\ncheck: {s}\n\n", .{filename}) catch return;

    // Parse
    writeDotted(writer, "Parse", 24);
    if (r.parse_errors > 0) {
        writer.print("FAIL ({d} errors)\n", .{r.parse_errors}) catch return;
    } else {
        writer.print("OK ({d} lines)\n", .{r.line_count}) catch return;
    }

    // Types
    if (r.is_typescript) {
        writeDotted(writer, "Types", 24);
        if (r.type_errors > 0) {
            writer.print("FAIL ({d} errors)\n", .{r.type_errors}) catch return;
        } else {
            writer.print("OK\n", .{}) catch return;
        }
    }

    // Sound mode
    writeDotted(writer, "Sound mode", 24);
    if (r.bool_errors > 0) {
        writer.print("FAIL ({d} errors)\n", .{r.bool_errors}) catch return;
    } else if (r.bool_specializations > 0) {
        writer.print("OK ({d} specializations)\n", .{r.bool_specializations}) catch return;
    } else {
        writer.print("OK\n", .{}) catch return;
    }

    // Strict profile
    writeDotted(writer, "Strict ZigTS", 24);
    if (r.strict_errors > 0) {
        writer.print("FAIL ({d} errors)\n", .{r.strict_errors}) catch return;
    } else {
        writer.print("OK\n", .{}) catch return;
    }

    // Verification
    if (r.verify_ran) {
        writer.print("\n  Verification:\n", .{}) catch return;
        writeProven(writer, "exhaustive_returns", r.verify_errors == 0 and r.exhaustive_returns);
        writeProven(writer, "results_safe", r.results_safe);
        writeProven(writer, "optionals_safe", r.optionals_safe);
        writeProven(writer, "state_isolated", r.state_isolated);
        writeProven(writer, "no_unreachable", r.no_unreachable);
    }

    // Properties
    if (r.properties) |props| {
        writer.print("\n  Properties:\n", .{}) catch return;
        writeProven(writer, "retry_safe", props.retry_safe);
        writeProven(writer, "idempotent", props.idempotent);
        writeProven(writer, "injection_safe", props.injection_safe);
        writeProven(writer, "deterministic", props.deterministic);
        writeProven(writer, "read_only", props.read_only);
        writeProven(writer, "cost_bounded", props.cost_bounded);

        writer.print("\n  Security:\n", .{}) catch return;
        writeProven(writer, "no_secret_leakage", props.no_secret_leakage);
        writeProven(writer, "no_credential_leak", props.no_credential_leakage);
        writeProven(writer, "input_validated", props.input_validated);
    }

    // Summary stats
    writer.print("\n", .{}) catch return;
    if (r.fault_total > 0) {
        writer.print("  Fault coverage: {d}/{d} paths covered\n", .{ r.fault_covered, r.fault_total }) catch return;
    }
    if (r.paths_enumerated > 0) {
        writer.print("  Execution paths: {d}", .{r.paths_enumerated}) catch return;
        if (r.paths_exhaustive) {
            writer.print(" (exhaustive)\n", .{}) catch return;
        } else {
            writer.print(" (limit reached)\n", .{}) catch return;
        }
    }
    if (r.max_io_depth) |depth| {
        writer.print("  Max I/O depth: {d}\n", .{depth}) catch return;
    }
    writeCostBound(writer, r);

    if (hasCanonicalDiagnostic(r.json_diagnostics.items)) {
        writer.print("\n  Canonical diagnostics:\n", .{}) catch return;
        for (r.json_diagnostics.items) |d| {
            if (!isCanonicalDiagnostic(d.code)) continue;
            writer.print(
                "    {s} ({s}) {s}:{d}:{d}  {s}\n",
                .{ d.code, d.severity, d.file, d.line, d.column, d.message },
            ) catch return;
            if (d.suggestion) |suggestion| {
                writer.print("      help: {s}\n", .{suggestion}) catch return;
            }
        }
    }

    // Spec/Effects diagnostics live in contract.spec_diagnostics, not in
    // json_diagnostics, so the human card must render them here or the printed
    // error lines fall short of the footer count (e.g. a Spec-less handler
    // trips ZTS500 in `specErrors()` but otherwise prints nothing).
    if (r.contract) |*contract| {
        if (r.specErrors() > 0) {
            writer.print("\n  Spec diagnostics:\n", .{}) catch return;
            for (contract.spec_diagnostics.items) |d| {
                if (d.kind.severity() != .err) continue;
                writer.print(
                    "    {s} (error) {s}:{d}:{d}  {s}\n",
                    .{ d.kind.code(), filename, contract.handler.line, contract.handler.column, specDiagnosticMessage(d) },
                ) catch return;
                if (d.suggestion) |suggestion| {
                    writer.print("      help: {s}\n", .{suggestion}) catch return;
                }
            }
        }
    }

    writer.print("\n  {d} errors, {d} warnings\n", .{ r.totalErrors(), r.totalWarnings() }) catch return;
}

/// Human-readable message for a spec/Effects diagnostic, mirroring the JSON
/// text in `appendSpecDiagnosticsJson` so the card and `--json` agree.
fn specDiagnosticMessage(diag: handler_contract.SpecDiagnostic) []const u8 {
    if (diag.implicit_default) {
        switch (diag.kind) {
            .not_discharged => return "handler declares no Spec<...>; the default proof profile demands a property this handler does not hold",
            .incompatible_with_import => return "handler declares no Spec<...>; the default profile's read_only conflicts with a stateful module import",
            else => {},
        }
    }
    return switch (diag.kind) {
        .not_discharged => "declared Spec was not discharged by handler proof",
        .incompatible_with_import => "declared Spec is incompatible with imported module",
        .unknown_name => "declared Spec name is not recognized",
        .missing_capsule => "helper breaks a handler-demanded property and carries no Proof<...> capsule",
        .effect_undeclared => "function reaches a capability outside its declared Effects<...> ceiling",
        .effect_unknown_capability => "Effects<...> names an unknown capability",
        .effect_over_declared => "declared capability is never reached by the function",
        .budget_exceeded => "handler reaches a capability outside its declared Effects<...> budget",
        .helper_budget_exceeded => "helper reaches a capability outside the handler's Effects<...> budget",
        .missing_effects_capsule => "exported helper carries no Effects<...> capsule",
        .missing_proof_capsule_export => "exported helper carries no Proof<...> capsule",
        .workflow_call_in_step => "workflow.call/saga/fanout/follow used inside a step() callback silently loses durability",
        .saga_step_missing_compensate => "a non-last saga step has no compensate, leaving a partial-rollback hole",
    };
}

fn isCanonicalDiagnostic(code: []const u8) bool {
    return zts.rule_registry.isCanonicalProfileCode(code);
}

fn hasCanonicalDiagnostic(diagnostics: []const json_diag.JsonDiagnostic) bool {
    for (diagnostics) |d| {
        if (isCanonicalDiagnostic(d.code)) return true;
    }
    return false;
}

const dots = "." ** 32;

fn writeDotted(writer: anytype, label: []const u8, width: usize) void {
    writer.print("  {s} ", .{label}) catch return;
    const pad = @min(width -| (label.len + 1), dots.len);
    writer.writeAll(dots[0..pad]) catch return;
    writer.writeAll(" ") catch return;
}

fn writeProven(writer: anytype, label: []const u8, proven: bool) void {
    writer.print("    {s} ", .{label}) catch return;
    const pad = @min(20 -| label.len, dots.len);
    writer.writeAll(dots[0..pad]) catch return;
    writer.writeAll(if (proven) " PROVEN\n" else " ---\n") catch return;
}

fn writeCostBound(writer: anytype, r: *const CheckResult) void {
    const contract = if (r.contract) |*c| c else return;
    const envelope = contract.cost_envelope orelse return;
    switch (envelope.total) {
        .constant => {},
        .linear => |linear| {
            writer.print(
                "  Cost bound: {d}+{d}*|source| (line {d})\n",
                .{ linear.base, linear.coefficient, linear.source.line },
            ) catch return;
        },
        .unbounded => |source| {
            writer.print(
                "  Cost bound: unbounded (line {d}) - see cost_bounded\n",
                .{source.line},
            ) catch return;
        },
    }
}

/// Generate TypeScript type definitions for all virtual modules.
pub fn generateTypeDefs(writer: anytype) void {
    writer.print("// Generated by: zts check --types\n// Do not edit manually.\n\n", .{}) catch return;

    // Request and Response globals
    writer.print(
        \\interface RequestInit {{
        \\  method?: string;
        \\  headers?: Record<string, string>;
        \\  body?: string;
        \\}}
        \\
        \\interface ResponseInit {{
        \\  status?: number;
        \\  statusText?: string;
        \\  headers?: Record<string, string>;
        \\}}
        \\
        \\declare class Request {{
        \\  readonly method: string;
        \\  readonly url: string;
        \\  readonly path: string;
        \\  readonly query: string;
        \\  readonly body: string | undefined;
        \\  readonly headers: Record<string, string>;
        \\}}
        \\
        \\declare class Response {{
        \\  readonly body: string;
        \\  readonly status: number;
        \\  readonly statusText: string;
        \\  readonly ok: boolean;
        \\  readonly headers: Record<string, string>;
        \\  static json(data: unknown, init?: ResponseInit): Response;
        \\  static text(text: string, init?: ResponseInit): Response;
        \\  static html(html: string, init?: ResponseInit): Response;
        \\  static redirect(url: string, status?: number): Response;
        \\}}
        \\
        \\
    , .{}) catch return;

    const modules = @import("zts").builtin_modules;
    for (modules.all) |binding| {
        writer.print("declare module \"{s}\" {{\n", .{binding.specifier}) catch return;
        for (binding.exports) |func| {
            writer.print("  export function {s}(", .{func.name}) catch return;
            const required_arg_count = func.required_arg_count orelse @as(u8, @intCast(@min(func.param_types.len, 255)));
            for (func.param_types, 0..) |pt, i| {
                if (i > 0) writer.print(", ", .{}) catch return;
                const optional_marker: []const u8 = if (i >= required_arg_count) "?" else "";
                writer.print("arg{d}{s}: {s}", .{ i, optional_marker, returnKindToTs(pt) }) catch return;
            }
            writer.print("): {s};\n", .{returnKindToTs(func.returns)}) catch return;
        }
        writer.print("}}\n\n", .{}) catch return;
    }
}

fn returnKindToTs(kind: @import("zts").module_binding.ReturnKind) []const u8 {
    return switch (kind) {
        .boolean => "boolean",
        .number => "number",
        .string => "string",
        .object => "Record<string, unknown>",
        .undefined => "void",
        .unknown => "unknown",
        .optional_string => "string | undefined",
        .optional_object => "Record<string, unknown> | undefined",
        .result => "{ ok: boolean; value?: unknown; error?: string; errors?: unknown }",
    };
}
