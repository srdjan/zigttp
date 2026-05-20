//! CheckResult data type plus presentation/codegen helpers
//! (formatProofCard, generateTypeDefs). The orchestrators that produce
//! CheckResult values live in precompile.zig.

const std = @import("std");
const zigts = @import("zigts");

const handler_contract = zigts.handler_contract;
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

    pub fn totalErrors(self: *const CheckResult) u32 {
        return self.parse_errors + self.bool_errors + self.type_errors + self.strict_errors + self.verify_errors + self.flow_errors + self.specErrors();
    }

    pub fn totalWarnings(self: *const CheckResult) u32 {
        return self.bool_warnings + self.strict_warnings + self.verify_warnings + self.flow_warnings;
    }

    pub fn deinit(self: *CheckResult, allocator: std.mem.Allocator) void {
        if (self.contract) |*c| c.deinit(allocator);
        self.json_diagnostics.deinit(allocator);
    }

    fn specErrors(self: *const CheckResult) u32 {
        if (self.contract) |*contract| return @intCast(contract.spec_diagnostics.items.len);
        return 0;
    }
};

/// Walk the flow_checker diagnostics, materialise a counterexample witness
/// for each one that maps to a tracked PropertyTag, and persist it to
/// `.zigttp/witnesses/<short_hash>/`. Failures here never block the
/// analysis result; the corpus is best-effort persistence. Returns the
/// number of pinned witnesses re-fired by this build, which the live
/// reload HUD surfaces as a regression toast.
pub fn persistFlowWitnesses(
    allocator: std.mem.Allocator,
    flow_diags: []const zigts.flow_checker.Diagnostic,
    ir_view: zigts.parser.IrView,
    handler_path: []const u8,
) usize {
    if (flow_diags.len == 0) return 0;

    const corpus_dir = zigts.witness_corpus.corpusDir(allocator, handler_path) catch return 0;
    defer allocator.free(corpus_dir);
    zigts.witness_corpus.ensureCorpusDir(allocator, corpus_dir, handler_path) catch return 0;

    var pinned_regressions: usize = 0;
    for (flow_diags) |diag| {
        const tag = zigts.flow_checker.propertyTagForKind(diag.kind) orelse continue;
        const loc = ir_view.getLoc(diag.node) orelse continue;
        const constraints: []const zigts.counterexample.WitnessConstraint =
            if (diag.witness) |wit| wit.path_constraints else &.{};
        const io_calls: []const zigts.counterexample.TrackedIoCall =
            if (diag.witness) |wit| wit.io_calls else &.{};

        var witness = zigts.counterexample.solve(allocator, .{
            .property = tag,
            .origin = .{ .line = loc.line, .column = loc.column },
            .sink = .{ .line = loc.line, .column = loc.column },
            .summary = diag.message,
            .constraints = constraints,
            .io_calls = io_calls,
        }) catch continue;
        defer witness.deinit(allocator);

        if (zigts.witness_corpus.persist(allocator, corpus_dir, witness)) |pres| {
            var owned = pres;
            defer owned.deinit(allocator);
            // .refreshed means the witness was already persisted on a prior
            // build. If the author had pinned it, this build re-fires a
            // regression they explicitly chose to defend against.
            if (owned.outcome == .refreshed and
                zigts.witness_corpus.isPinned(allocator, corpus_dir, owned.key))
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
        const message: []const u8 = switch (diag.kind) {
            .not_discharged => "declared Spec was not discharged by handler proof",
            .incompatible_with_import => "declared Spec is incompatible with imported module",
            .unknown_name => "declared Spec name is not recognized",
            .missing_capsule => "helper breaks a handler-demanded property and carries no Proof<...> capsule",
        };
        result.json_diagnostics.append(allocator, .{
            .code = code,
            .severity = "error",
            .message = message,
            .file = handler_path,
            .line = contract.handler.line,
            .column = @intCast(@min(contract.handler.column, std.math.maxInt(u16))),
            .suggestion = diag.suggestion,
        }) catch {};
    }
}

pub fn refreshSpecDiagnostics(allocator: std.mem.Allocator, result: *CheckResult) !void {
    const contract = if (result.contract) |*c| c else return;
    if (contract.declared_specs.items.len == 0) return;

    // Re-discharge the handler's Spec<...> obligations against the freshly
    // classified properties. Capsule diagnostics - those carrying a
    // `function` - come from proof-carrying-function discharge, which this
    // refresh does not re-run; they are cloned across so the helper
    // ZTS500/ZTS502 and ZTS606 entries survive. The clone (rather than a
    // move) keeps `contract.spec_diagnostics` intact until the swap, so an
    // OOM mid-loop leaves both lists individually consistent.
    var refreshed = try zigts.spec_discharge.dischargeSpecs(
        allocator,
        contract.declared_specs.items,
        contract.properties,
        contract.modules.items,
    );
    errdefer {
        for (refreshed.items) |*d| d.deinit(allocator);
        refreshed.deinit(allocator);
    }
    for (contract.spec_diagnostics.items) |*diag| {
        if (diag.function != null) {
            try refreshed.append(allocator, try diag.clone(allocator));
        }
    }
    for (contract.spec_diagnostics.items) |*diag| diag.deinit(allocator);
    contract.spec_diagnostics.deinit(allocator);
    contract.spec_diagnostics = refreshed;
}

/// Format a structured proof card showing what the compiler proved.
pub fn formatProofCard(writer: anytype, r: *const CheckResult, filename: []const u8) void {
    writer.print("\nzts check: {s}\n\n", .{filename}) catch return;

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

    writer.print("\n  {d} errors, {d} warnings\n", .{ r.totalErrors(), r.totalWarnings() }) catch return;
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

/// Generate TypeScript type definitions for all virtual modules.
pub fn generateTypeDefs(writer: anytype) void {
    writer.print("// Generated by: zigts check --types\n// Do not edit manually.\n\n", .{}) catch return;

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

    const modules = @import("zigts").builtin_modules;
    for (modules.all) |binding| {
        writer.print("declare module \"{s}\" {{\n", .{binding.specifier}) catch return;
        for (binding.exports) |func| {
            writer.print("  export function {s}(", .{func.name}) catch return;
            for (func.param_types, 0..) |pt, i| {
                if (i > 0) writer.print(", ", .{}) catch return;
                writer.print("arg{d}: {s}", .{ i, returnKindToTs(pt) }) catch return;
            }
            writer.print("): {s};\n", .{returnKindToTs(func.returns)}) catch return;
        }
        writer.print("}}\n\n", .{}) catch return;
    }
}

fn returnKindToTs(kind: @import("zigts").module_binding.ReturnKind) []const u8 {
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
