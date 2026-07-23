//! Structured JSON Diagnostic Output
//!
//! Converts internal diagnostics from the parser, BoolChecker, TypeChecker,
//! and HandlerVerifier into machine-readable JSON for the compiler-in-the-loop
//! workflow. `zts expert` calls `zts check --json` and parses these
//! diagnostics to fix handler code using the `suggestion` field.
//!
//! Architecture: each checker's Diagnostic -> JsonDiagnostic -> JSON on stdout

const std = @import("std");
const zts = @import("zts");
const parser = zts.parser;
const bool_checker = zts.bool_checker;
const type_checker = zts.type_checker;
const strict_checker = zts.strict_checker;
const handler_verifier = zts.handler_verifier;
const flow_checker = zts.flow_checker;
const handler_contract = zts.handler_contract;
const writeJsonString = handler_contract.writeJsonString;

pub const IrView = parser.IrView;
pub const SourceLocation = parser.SourceLocation;
pub const ParseError = parser.ParseError;
pub const ErrorKind = parser.ErrorKind;

// -------------------------------------------------------------------------
// Structured diagnostic
// -------------------------------------------------------------------------

pub const JsonDiagnostic = struct {
    code: []const u8,
    severity: []const u8,
    message: []const u8,
    file: []const u8,
    line: u32,
    column: u32,
    suggestion: ?[]const u8,
    /// True when `message` is a heap copy owned by this diagnostic (duped at
    /// capture time so it outlives the checker allocator that produced it).
    /// `deinit` frees it; static-string messages keep this false and are
    /// left untouched. See `fromCheckerDiagnostic`.
    message_owned: bool = false,

    pub fn deinit(self: *JsonDiagnostic, allocator: std.mem.Allocator) void {
        if (self.message_owned) allocator.free(self.message);
    }
};

// -------------------------------------------------------------------------
// Diagnostic code mapping
// -------------------------------------------------------------------------

/// Parser error codes: ZTS0xx
fn parserErrorCode(kind: ErrorKind) []const u8 {
    return switch (kind) {
        .unsupported_feature => "ZTS001",
        .unexpected_token => "ZTS002",
        .expected_token => "ZTS003",
        .expected_expression => "ZTS004",
        .expected_statement => "ZTS005",
        .expected_identifier => "ZTS006",
        .unexpected_eof => "ZTS007",
        .unterminated_string => "ZTS008",
        .unterminated_template => "ZTS009",
        .unterminated_regex => "ZTS010",
        .unterminated_comment => "ZTS011",
        .invalid_number => "ZTS012",
        .invalid_escape_sequence => "ZTS013",
        .invalid_unicode_escape => "ZTS014",
        .expected_property_name => "ZTS015",
        .invalid_assignment_target => "ZTS016",
        .invalid_destructuring => "ZTS017",
        .duplicate_parameter => "ZTS018",
        .duplicate_binding => "ZTS019",
        .undeclared_variable => "ZTS020",
        .const_without_initializer => "ZTS021",
        .invalid_break => "ZTS022",
        .invalid_continue => "ZTS023",
        .invalid_return => "ZTS024",
        .invalid_yield => "ZTS025",
        .invalid_await => "ZTS026",
        .invalid_super => "ZTS027",
        .too_many_parameters => "ZTS028",
        .too_many_locals => "ZTS029",
        .too_many_upvalues => "ZTS030",
        .too_many_constants => "ZTS031",
        .jump_too_large => "ZTS032",
        .mismatched_jsx_tag => "ZTS033",
        .invalid_jsx_attribute => "ZTS034",
        .unclosed_jsx_element => "ZTS035",
        .jsx_expression_expected => "ZTS036",
        .invalid_import => "ZTS037",
        .invalid_export => "ZTS038",
        .duplicate_export => "ZTS039",
        .unexpected_character => "ZTS040",
        .nesting_too_deep => "ZTS041",
    };
}

/// BoolChecker error codes: ZTS1xx
fn boolCheckerCode(kind: bool_checker.DiagnosticKind) []const u8 {
    return switch (kind) {
        .condition_not_boolean => "ZTS100",
        .logical_operand_not_boolean => "ZTS101",
        .not_operand_not_boolean => "ZTS102",
        .nullish_on_non_nullable => "ZTS103",
        .arithmetic_on_non_numeric => "ZTS104",
        .mixed_type_add => "ZTS105",
        .add_on_non_addable => "ZTS106",
        .tautological_comparison => "ZTS107",
    };
}

/// TypeChecker error codes: ZTS2xx
fn typeCheckerCode(kind: type_checker.DiagnosticKind) []const u8 {
    return switch (kind) {
        .type_mismatch => "ZTS200",
        .missing_field => "ZTS201",
        .arg_count_mismatch => "ZTS202",
        .arg_type_mismatch => "ZTS203",
        .return_type_mismatch => "ZTS204",
        .non_exhaustive_match => "ZTS205",
    };
}

/// HandlerVerifier error codes: ZTS3xx (structural checks), ZTS5xx (specs).
fn verifierCode(kind: handler_verifier.DiagnosticKind) []const u8 {
    return switch (kind) {
        .missing_return_else => "ZTS300",
        .missing_return_default => "ZTS301",
        .missing_return_path => "ZTS302",
        .unchecked_result_value => "ZTS303",
        .unreachable_after_return => "ZTS304",
        .unused_variable => "ZTS305",
        .unused_import => "ZTS306",
        .non_exhaustive_match => "ZTS307",
        .unchecked_optional_use => "ZTS308",
        .unchecked_optional_access => "ZTS309",
        .module_scope_mutation => "ZTS310",
        .websocket_import_without_events => "ZTS320",
        .websocket_events_without_import => "ZTS321",
        .spec_not_discharged => "ZTS500",
        .spec_incompatible_with_import => "ZTS501",
        .spec_unknown_name => "ZTS502",
    };
}

/// StrictChecker error codes: ZTS6xx
fn strictCheckerCode(kind: strict_checker.DiagnosticKind) []const u8 {
    return switch (kind) {
        .implicit_unknown => "ZTS600",
        .missing_public_annotation => "ZTS601",
        .dynamic_capability_access => "ZTS602",
        .non_exhaustive_profile_match => "ZTS603",
        .avoidable_let => "ZTS604",
        .computed_property_access => "ZTS605",
        .canonical_arrow_helper => "ZTS608",
        .canonical_export_function_const => "ZTS609",
        .canonical_public_helper_effects => "ZTS610",
        .canonical_public_helper_proof => "ZTS611",
        .canonical_ternary => "ZTS612",
        .canonical_compound_assignment => "ZTS613",
        .canonical_non_leading_spread => "ZTS614",
        .canonical_template_complex_interp => "ZTS615",
        .canonical_call_spread => "ZTS616",
        .canonical_default_parameter => "ZTS617",
        .canonical_destructure_depth => "ZTS618",
        .canonical_unused_index_alias => "ZTS619",
        .canonical_redundant_bool_compare => "ZTS620",
    };
}

/// FlowChecker error codes: ZTS4xx
fn flowCheckerCode(kind: flow_checker.DiagnosticKind) []const u8 {
    return switch (kind) {
        .secret_in_response => "ZTS400",
        .credential_in_response => "ZTS401",
        .secret_in_log => "ZTS402",
        .credential_in_log => "ZTS403",
        .secret_in_egress_url => "ZTS404",
        .credential_in_egress_url => "ZTS405",
        .secret_in_egress_body => "ZTS406",
        .unvalidated_input_in_egress => "ZTS407",
    };
}

// -------------------------------------------------------------------------
// Conversion helpers
// -------------------------------------------------------------------------

/// Extract suggestion from unsupported_feature parser errors.
/// These follow the pattern: "'X' is not supported; use Y instead"
fn splitSuggestion(message: []const u8) struct { msg: []const u8, suggestion: ?[]const u8 } {
    if (std.mem.indexOf(u8, message, "; ")) |sep| {
        return .{
            .msg = message[0..sep],
            .suggestion = message[sep + 2 ..],
        };
    }
    return .{ .msg = message, .suggestion = null };
}

pub fn fromParseError(err: ParseError, file: []const u8) JsonDiagnostic {
    const code = parserErrorCode(err.kind);

    if (err.kind == .unsupported_feature) {
        const parts = splitSuggestion(err.message);
        return .{
            .code = code,
            .severity = "error",
            .message = parts.msg,
            .file = file,
            .line = err.location.line,
            .column = err.location.column,
            .suggestion = parts.suggestion,
        };
    }

    return .{
        .code = code,
        .severity = "error",
        .message = err.message,
        .file = file,
        .line = err.location.line,
        .column = err.location.column,
        .suggestion = err.expected,
    };
}

/// Type stripper error codes: ZTS04x (continues the parser ZTS0xx range).
fn stripErrorCode(kind: zts.StripDiagnosticKind) []const u8 {
    return switch (kind) {
        .any_type => "ZTS041",
        .as_assertion => "ZTS042",
        .satisfies_assertion => "ZTS043",
    };
}

/// Map a TypeScript stripper rejection to a structured diagnostic so `--json`
/// consumers and the `zts expert` agent see a ZTS code, location, and
/// suggestion instead of a swallowed `error.StripFailed`.
pub fn fromStripError(diag: zts.StripDiagnostic, file: []const u8) JsonDiagnostic {
    const parts = splitSuggestion(diag.kind.message());
    return .{
        .code = stripErrorCode(diag.kind),
        .severity = "error",
        .message = parts.msg,
        .file = file,
        .line = diag.line,
        // Clamp instead of @intCast: a strip error past column 65535 (a minified
        // single-line bundle is one long line) would otherwise panic
        // `zttp check --json`.
        .column = @intCast(@min(diag.column, std.math.maxInt(u16))),
        .suggestion = parts.suggestion,
    };
}

/// Convert a checker `Diagnostic` into a `JsonDiagnostic`, duping the message
/// into `allocator` at capture time.
///
/// Checker diagnostics build their `.message` with `allocPrint` on the
/// checker's own allocator (e.g. ZTS202/ZTS203 arg-count/arg-type detail),
/// which is torn down with the pipeline before the JSON is serialized. Without
/// this dupe the slice dangles and the `--json` / edit-simulate surface reads
/// freed bytes (0xaa in debug). `allocator.dupe` is safe for both heap and
/// static-string messages; the resulting copy is owned (`message_owned`) and
/// freed by `JsonDiagnostic.deinit`. On OOM the borrowed slice is returned
/// unowned, preserving prior behavior.
fn fromCheckerDiagnostic(
    allocator: std.mem.Allocator,
    comptime codeFn: anytype,
    diag: anytype,
    ir_view: IrView,
    file: []const u8,
) ?JsonDiagnostic {
    const loc = ir_view.getLoc(diag.node) orelse return null;
    const owned_msg = allocator.dupe(u8, diag.message) catch null;
    return .{
        .code = codeFn(diag.kind),
        .severity = diag.severity.label(),
        .message = owned_msg orelse diag.message,
        .file = file,
        .line = loc.line,
        .column = loc.column,
        .suggestion = diag.help,
        .message_owned = owned_msg != null,
    };
}

pub fn fromBoolDiagnostic(allocator: std.mem.Allocator, diag: bool_checker.Diagnostic, ir_view: IrView, file: []const u8) ?JsonDiagnostic {
    return fromCheckerDiagnostic(allocator, boolCheckerCode, diag, ir_view, file);
}

pub fn fromTypeDiagnostic(allocator: std.mem.Allocator, diag: type_checker.Diagnostic, ir_view: IrView, file: []const u8) ?JsonDiagnostic {
    return fromCheckerDiagnostic(allocator, typeCheckerCode, diag, ir_view, file);
}

pub fn fromStrictDiagnostic(allocator: std.mem.Allocator, diag: strict_checker.Diagnostic, ir_view: IrView, file: []const u8) ?JsonDiagnostic {
    return fromCheckerDiagnostic(allocator, strictCheckerCode, diag, ir_view, file);
}

pub fn fromVerifierDiagnostic(allocator: std.mem.Allocator, diag: handler_verifier.Diagnostic, ir_view: IrView, file: []const u8) ?JsonDiagnostic {
    return fromCheckerDiagnostic(allocator, verifierCode, diag, ir_view, file);
}

pub fn fromFlowDiagnostic(allocator: std.mem.Allocator, diag: flow_checker.Diagnostic, ir_view: IrView, file: []const u8) ?JsonDiagnostic {
    return fromCheckerDiagnostic(allocator, flowCheckerCode, diag, ir_view, file);
}

// -------------------------------------------------------------------------
// JSON serialization
// -------------------------------------------------------------------------

pub fn writeDiagnosticJson(writer: anytype, diag: *const JsonDiagnostic) !void {
    try writer.writeAll("{\"code\":");
    try writeJsonString(writer, diag.code);
    try writer.writeAll(",\"severity\":");
    try writeJsonString(writer, diag.severity);
    try writer.writeAll(",\"message\":");
    try writeJsonString(writer, diag.message);
    try writer.writeAll(",\"file\":");
    try writeJsonString(writer, diag.file);
    try writer.print(",\"line\":{d},\"column\":{d}", .{ diag.line, diag.column });
    if (diag.suggestion) |s| {
        try writer.writeAll(",\"suggestion\":");
        try writeJsonString(writer, s);
    } else {
        try writer.writeAll(",\"suggestion\":null");
    }
    try writer.writeByte('}');
}

fn writeDiagnosticsArray(writer: anytype, diagnostics: []const JsonDiagnostic) !void {
    try writer.writeByte('[');
    for (diagnostics, 0..) |*diag, i| {
        if (i > 0) try writer.writeByte(',');
        try writeDiagnosticJson(writer, diag);
    }
    try writer.writeByte(']');
}

/// Emit success JSON with proof summary and optional warnings.
///
/// `witnesses_block_json`, when non-null, is a pre-formatted JSON object
/// (e.g. `{"total":3,"by_property":{"injection_safe":2,"no_secret_leakage":1}}`)
/// inserted under `proof.witnesses`. Pass null when the corpus is unavailable
/// or the caller chooses not to surface it. The caller is responsible for
/// ensuring the block is well-formed JSON.
pub fn writeSuccessJson(
    writer: anytype,
    contract: ?*const handler_contract.HandlerContract,
    diagnostics: []const JsonDiagnostic,
    witnesses_block_json: ?[]const u8,
    proof_trace_json: ?[]const u8,
) !void {
    try writer.writeAll("{\"success\":true");

    // Proof section from contract
    if (contract) |c| {
        try writer.writeAll(",\"proof\":{");

        // Env vars
        try writer.writeAll("\"env_vars\":[");
        for (c.env.literal.items, 0..) |v, i| {
            if (i > 0) try writer.writeByte(',');
            try writeJsonString(writer, v);
        }
        try writer.writeByte(']');

        // Outbound hosts
        try writer.writeAll(",\"outbound_hosts\":[");
        for (c.egress.hosts.items, 0..) |h, i| {
            if (i > 0) try writer.writeByte(',');
            try writeJsonString(writer, h);
        }
        try writer.writeByte(']');

        // Virtual modules
        try writer.writeAll(",\"virtual_modules\":[");
        for (c.modules.items, 0..) |m, i| {
            if (i > 0) try writer.writeByte(',');
            try writeJsonString(writer, m);
        }
        try writer.writeByte(']');

        // Properties
        if (c.properties) |props| {
            try writer.writeAll(",\"properties\":{");
            try writer.print("\"retry_safe\":{},\"idempotent\":{},\"injection_safe\":{},\"deterministic\":{},\"read_only\":{},\"state_isolated\":{},\"fault_covered\":{}", .{
                props.retry_safe,
                props.idempotent,
                props.injection_safe,
                props.deterministic,
                props.read_only,
                props.state_isolated,
                props.fault_covered,
            });
            try writer.writeByte('}');
        }

        // proofTrace: per-property reasoning for the proof card. Additive
        // sibling of `properties`; pre-rendered JSON object or omitted.
        if (proof_trace_json) |ptj| {
            try writer.writeAll(",\"proofTrace\":");
            try writer.writeAll(ptj);
        }

        // declared_specs: surface the effective active set so tools
        // (pi_specs_status, the agent autoloop) can read obligations
        // straight from the proof JSON.
        try writer.writeAll(",\"declared_specs\":[");
        for (c.declared_specs.items, 0..) |s, i| {
            if (i > 0) try writer.writeByte(',');
            try writeJsonString(writer, s);
        }
        try writer.writeByte(']');

        try writer.writeByte(',');
        try writeSpecAndCapsulesJson(writer, c);

        if (witnesses_block_json) |wb| {
            try writer.writeAll(",\"witnesses\":");
            try writer.writeAll(wb);
        }

        try writer.writeByte('}');
    }

    // Diagnostics (warnings only for success)
    try writer.writeAll(",\"diagnostics\":");
    try writeDiagnosticsArray(writer, diagnostics);

    try writer.writeAll("}\n");
}

/// Emit error JSON with diagnostics array. When a contract exists, include the
/// same proof object as the success envelope so clients can inspect declared
/// specs that failed discharge while still treating the check as failed.
///
/// `witnesses_block_json` is the same as on `writeSuccessJson`: pre-formatted
/// JSON inserted under `proof.witnesses`, or null to omit.
pub fn writeErrorJson(
    writer: anytype,
    contract: ?*const handler_contract.HandlerContract,
    diagnostics: []const JsonDiagnostic,
    witnesses_block_json: ?[]const u8,
    proof_trace_json: ?[]const u8,
) !void {
    try writer.writeAll("{\"success\":false");
    if (contract) |c| {
        try writer.writeAll(",\"proof\":{");

        try writer.writeAll("\"env_vars\":[");
        for (c.env.literal.items, 0..) |v, i| {
            if (i > 0) try writer.writeByte(',');
            try writeJsonString(writer, v);
        }
        try writer.writeAll("],\"outbound_hosts\":[");
        for (c.egress.hosts.items, 0..) |h, i| {
            if (i > 0) try writer.writeByte(',');
            try writeJsonString(writer, h);
        }
        try writer.writeAll("],\"virtual_modules\":[");
        for (c.modules.items, 0..) |m, i| {
            if (i > 0) try writer.writeByte(',');
            try writeJsonString(writer, m);
        }
        try writer.writeByte(']');

        if (c.properties) |props| {
            try writer.writeAll(",\"properties\":{");
            try writer.print("\"retry_safe\":{},\"idempotent\":{},\"injection_safe\":{},\"deterministic\":{},\"read_only\":{},\"state_isolated\":{},\"fault_covered\":{}", .{
                props.retry_safe,
                props.idempotent,
                props.injection_safe,
                props.deterministic,
                props.read_only,
                props.state_isolated,
                props.fault_covered,
            });
            try writer.writeByte('}');
        }

        if (proof_trace_json) |ptj| {
            try writer.writeAll(",\"proofTrace\":");
            try writer.writeAll(ptj);
        }

        try writer.writeAll(",\"declared_specs\":[");
        for (c.declared_specs.items, 0..) |s, i| {
            if (i > 0) try writer.writeByte(',');
            try writeJsonString(writer, s);
        }
        try writer.writeAll("],");
        try writeSpecAndCapsulesJson(writer, c);
        if (witnesses_block_json) |wb| {
            try writer.writeAll(",\"witnesses\":");
            try writer.writeAll(wb);
        }
        try writer.writeByte('}');
    }
    try writer.writeAll(",\"diagnostics\":");
    try writeDiagnosticsArray(writer, diagnostics);
    try writer.writeAll("}\n");
}

/// Emit the `spec_diagnostics` array: handler `Spec<...>` discharge results
/// (ZTS500/501/502) plus helper capsule discharge and ZTS606. The `function`
/// key is present only on capsule diagnostics, attributing them to a helper.
fn writeSpecDiagnosticsJson(writer: anytype, items: anytype) !void {
    try writer.writeByte('[');
    for (items, 0..) |d, i| {
        if (i > 0) try writer.writeByte(',');
        try writer.writeAll("{\"code\":");
        try writeJsonString(writer, d.kind.code());
        try writer.writeAll(",\"kind\":");
        try writeJsonString(writer, @tagName(d.kind));
        try writer.writeAll(",\"severity\":");
        try writeJsonString(writer, if (d.kind.severity() == .warn) "warning" else "error");
        try writer.writeAll(",\"spec_name\":");
        try writeJsonString(writer, d.spec_name);
        if (d.function) |func| {
            try writer.writeAll(",\"function\":");
            try writeJsonString(writer, func);
        }
        if (d.incompatible_module) |module_name| {
            try writer.writeAll(",\"incompatible_module\":");
            try writeJsonString(writer, module_name);
        }
        if (d.suggestion) |suggestion| {
            try writer.writeAll(",\"suggestion\":");
            try writeJsonString(writer, suggestion);
        }
        try writer.writeByte('}');
    }
    try writer.writeByte(']');
}

/// Emit `"spec_diagnostics":[...],"proofCapsules":[...]` with no leading
/// separator. Shared verbatim by the success and error envelopes.
fn writeSpecAndCapsulesJson(
    writer: anytype,
    c: *const handler_contract.HandlerContract,
) !void {
    try writer.writeAll("\"spec_diagnostics\":");
    try writeSpecDiagnosticsJson(writer, c.spec_diagnostics.items);
    try writer.writeAll(",\"proofCapsules\":");
    try writeProofCapsulesJson(writer, c.function_capsules.items);
    try writer.writeAll(",\"effectCapsules\":");
    try writeEffectCapsulesJson(writer, c.function_effect_capsules.items);
}

/// Emit the `proofCapsules` array: per-helper `Proof<...>` capsule discharge
/// summary, so an agent can read which helpers proved which properties
/// without re-running the compiler.
fn writeProofCapsulesJson(writer: anytype, items: anytype) !void {
    try writer.writeByte('[');
    for (items, 0..) |cap, i| {
        if (i > 0) try writer.writeByte(',');
        try writer.writeAll("{\"function\":");
        try writeJsonString(writer, cap.function);
        try writer.print(",\"line\":{d},\"declared\":[", .{cap.line});
        for (cap.declared.items, 0..) |s, j| {
            if (j > 0) try writer.writeByte(',');
            try writeJsonString(writer, s);
        }
        try writer.print(
            "],\"proven\":{{\"total\":{},\"pure\":{},\"read_only\":{},\"deterministic\":{}}},\"discharged\":{}}}",
            .{ cap.proven_total, cap.proven_pure, cap.proven_read_only, cap.proven_deterministic, cap.discharged },
        );
    }
    try writer.writeByte(']');
}

/// Emit the `effectCapsules` array: per-helper `Effects<...>` capability
/// capsule discharge summary, so an agent can read each helper's declared
/// ceiling and inferred capability row without re-running the compiler.
fn writeEffectCapsulesJson(writer: anytype, items: anytype) !void {
    try writer.writeByte('[');
    for (items, 0..) |cap, i| {
        if (i > 0) try writer.writeByte(',');
        try writer.writeAll("{\"function\":");
        try writeJsonString(writer, cap.function);
        try writer.print(",\"line\":{d},\"declared\":[", .{cap.line});
        for (cap.declared.items, 0..) |s, j| {
            if (j > 0) try writer.writeByte(',');
            try writeJsonString(writer, s);
        }
        try writer.writeAll("],\"inferred\":[");
        for (cap.inferred.items, 0..) |s, j| {
            if (j > 0) try writer.writeByte(',');
            try writeJsonString(writer, s);
        }
        try writer.print("],\"discharged\":{}}}", .{cap.discharged});
    }
    try writer.writeByte(']');
}

// -------------------------------------------------------------------------
// Module listing
// -------------------------------------------------------------------------

pub fn writeModulesJson(writer: anytype) !void {
    const builtin_modules = zts.builtin_modules;
    try writer.writeByte('[');
    for (builtin_modules.all, 0..) |binding, i| {
        if (i > 0) try writer.writeByte(',');
        try writer.writeAll("{\"specifier\":");
        try writeJsonString(writer, binding.specifier);
        try writer.writeAll(",\"exports\":[");
        for (binding.exports, 0..) |func, j| {
            if (j > 0) try writer.writeByte(',');
            try writeJsonString(writer, func.name);
        }
        // Parallel signatures array so the agent learns arity, parameter
        // types, and return type without trial-and-error round-trips.
        try writer.writeAll("],\"signatures\":[");
        for (binding.exports, 0..) |func, j| {
            if (j > 0) try writer.writeByte(',');
            try writer.writeAll("{\"name\":");
            try writeJsonString(writer, func.name);
            const required_arg_count = func.required_arg_count orelse @as(u8, @intCast(@min(func.param_types.len, 255)));
            try writer.print(",\"arg_count\":{d},\"required_arg_count\":{d},\"params\":[", .{ func.arg_count, required_arg_count });
            for (func.param_types, 0..) |pt, k| {
                if (k > 0) try writer.writeByte(',');
                try writeJsonString(writer, pt.jsTypeName());
            }
            try writer.writeAll("],\"returns\":");
            try writeJsonString(writer, func.returns.jsTypeName());
            try writer.writeByte('}');
        }
        try writer.writeAll("]}");
    }
    try writer.writeAll("]\n");
}

pub fn writeModulesText(writer: anytype) !void {
    const builtin_modules = zts.builtin_modules;
    for (builtin_modules.all) |binding| {
        try writer.print("{s}\n", .{binding.specifier});
        for (binding.exports) |func| {
            try writer.print("  {s}\n", .{func.name});
        }
    }
}

// -------------------------------------------------------------------------
// Features listing
// -------------------------------------------------------------------------

const FeatureStatus = enum {
    allowed,
    blocked,

    fn label(self: FeatureStatus) []const u8 {
        return switch (self) {
            .allowed => "allowed",
            .blocked => "blocked",
        };
    }
};

const Feature = struct {
    name: []const u8,
    status: FeatureStatus,
    alternative: ?[]const u8,
    /// Why the construct is forbidden. Required for `.blocked`, null for `.allowed`.
    blocked_reason: ?[]const u8 = null,
    /// Class of failure the cut prevents (e.g. "hidden exceptional control flow").
    /// Required for `.blocked`, null for `.allowed`.
    failure_class: ?[]const u8 = null,
    /// Proof the cut unlocks (e.g. "Result narrowing and exhaustive path enumeration").
    /// Required for `.blocked`, null for `.allowed`.
    proof_unlocked: ?[]const u8 = null,
};

const features = [_]Feature{
    // Allowed
    .{ .name = "const", .status = .allowed, .alternative = null },
    .{ .name = "let", .status = .allowed, .alternative = null },
    .{ .name = "function", .status = .allowed, .alternative = null },
    .{ .name = "arrow functions", .status = .allowed, .alternative = null },
    .{ .name = "destructuring", .status = .allowed, .alternative = null },
    .{ .name = "spread/rest", .status = .allowed, .alternative = null },
    .{ .name = "template literals", .status = .allowed, .alternative = null },
    .{ .name = "if/else", .status = .allowed, .alternative = null },
    .{ .name = "for...of", .status = .allowed, .alternative = null },
    .{ .name = "ternary", .status = .allowed, .alternative = null },
    .{ .name = "optional chaining", .status = .allowed, .alternative = null },
    .{ .name = "nullish coalescing", .status = .allowed, .alternative = null },
    .{ .name = "match expression", .status = .allowed, .alternative = null },
    .{ .name = "assert statement", .status = .allowed, .alternative = null },
    .{ .name = "pipe operator", .status = .allowed, .alternative = null },
    .{ .name = "import/export", .status = .allowed, .alternative = null },
    .{ .name = "type annotations", .status = .allowed, .alternative = null },
    .{ .name = "distinct type", .status = .allowed, .alternative = null },
    .{ .name = "readonly fields", .status = .allowed, .alternative = null },
    .{ .name = "type guards (x is T)", .status = .allowed, .alternative = null },
    .{ .name = "template literal types", .status = .allowed, .alternative = null },
    .{ .name = "comptime()", .status = .allowed, .alternative = null },
    // Blocked
    .{
        .name = "switch/case",
        .status = .blocked,
        .alternative = "use 'match' expression",
        .blocked_reason = "fallthrough makes coverage ambiguous and lets cases share state through implicit fallthrough.",
        .failure_class = "non-exhaustive control flow and implicit fallthrough",
        .proof_unlocked = "match coverage and exhaustive return analysis",
    },
    .{
        .name = "var",
        .status = .blocked,
        .alternative = "use 'let' or 'const'",
        .blocked_reason = "hoisting and function-scoping create temporal dead zones the verifier cannot reason about.",
        .failure_class = "scope hoisting and temporal dead zones",
        .proof_unlocked = "block-scoped data flow and reachability analysis",
    },
    .{
        .name = "class",
        .status = .blocked,
        .alternative = "use plain objects and functions",
        .blocked_reason = "implicit mutable receivers hide data flow from the contract extractor.",
        .failure_class = "implicit mutable receivers and hidden state",
        .proof_unlocked = "explicit data flow and effect analysis",
    },
    .{
        .name = "while",
        .status = .blocked,
        .alternative = "use 'for...of' with a finite collection",
        .blocked_reason = "unbounded back-edges defeat finite path enumeration.",
        .failure_class = "unbounded back-edges and non-termination",
        .proof_unlocked = "finite path enumeration and termination",
    },
    .{
        .name = "do...while",
        .status = .blocked,
        .alternative = "use 'for...of' with a finite collection",
        .blocked_reason = "unbounded back-edges defeat finite path enumeration.",
        .failure_class = "unbounded back-edges and non-termination",
        .proof_unlocked = "finite path enumeration and termination",
    },
    .{
        .name = "for(;;)",
        .status = .blocked,
        .alternative = "use 'for (const i of range(n))'",
        .blocked_reason = "C-style loops carry no bound; gen-tests cannot enumerate every iteration.",
        .failure_class = "unbounded back-edges and non-termination",
        .proof_unlocked = "finite path enumeration and termination",
    },
    .{
        .name = "for...in",
        .status = .blocked,
        .alternative = "use 'for (const k of Object.keys(obj))'",
        .blocked_reason = "for...in walks the prototype chain; iteration order is implementation-defined.",
        .failure_class = "prototype-chain iteration and non-deterministic order",
        .proof_unlocked = "deterministic iteration and shape-stable access",
    },
    .{
        .name = "try/catch",
        .status = .blocked,
        .alternative = "use Result types and check .ok",
        .blocked_reason = "exceptions are an invisible second return channel that bypasses the type system.",
        .failure_class = "hidden exceptional control flow",
        .proof_unlocked = "Result narrowing and exhaustive path enumeration",
    },
    .{
        .name = "throw",
        .status = .blocked,
        .alternative = "return an error Response",
        .blocked_reason = "throw is the producer side of the hidden exception channel.",
        .failure_class = "hidden exceptional control flow",
        .proof_unlocked = "Result narrowing and exhaustive return analysis",
    },
    .{
        .name = "async/await",
        .status = .blocked,
        .alternative = "use fetchSync(), parallel(), race()",
        .blocked_reason = "ambient scheduling produces interleavings the replay log cannot reproduce.",
        .failure_class = "ambient scheduling and non-deterministic interleavings",
        .proof_unlocked = "deterministic effect boundary and replayable I/O",
    },
    .{
        .name = "new",
        .status = .blocked,
        .alternative = "use factory functions or object literals",
        .blocked_reason = "constructor dispatch combined with prototypes hides effects from the IR.",
        .failure_class = "constructor dispatch and hidden initialization effects",
        .proof_unlocked = "explicit factory call sites and visible effects",
    },
    .{
        .name = "this",
        .status = .blocked,
        .alternative = "use explicit parameter passing",
        .blocked_reason = "the binding of `this` is dynamic and unreadable from the IR.",
        .failure_class = "dynamic receiver binding",
        .proof_unlocked = "static call-graph and visible data flow",
    },
    .{
        .name = "null",
        .status = .blocked,
        .alternative = "use undefined",
        .blocked_reason = "two absent-value sentinels split optional narrowing into two incompatible lattices.",
        .failure_class = "dual absent-value sentinels",
        .proof_unlocked = "optional-narrowing proof totality",
    },
    .{
        .name = "== / !=",
        .status = .blocked,
        .alternative = "use === / !==",
        .blocked_reason = "loose equality coerces operands, creating control-flow paths the type checker cannot see.",
        .failure_class = "implicit coercion paths",
        .proof_unlocked = "sound type-directed comparison",
    },
    .{
        .name = "++ / --",
        .status = .blocked,
        .alternative = "use x = x + 1",
        .blocked_reason = "in-place mutation hides write effects in expression positions.",
        .failure_class = "hidden in-place mutation in expressions",
        .proof_unlocked = "explicit assignment effects and state isolation",
    },
    .{
        .name = "regex",
        .status = .blocked,
        .alternative = "use string methods (includes, startsWith, etc.)",
        .blocked_reason = "regex literals describe an opaque accept set the validator cannot reason about.",
        .failure_class = "opaque accept set and catastrophic backtracking",
        .proof_unlocked = "shape-checkable validation via zttp:validate schemas",
    },
    .{
        .name = "delete",
        .status = .blocked,
        .alternative = "build a new object literal with only the keys you keep",
        .blocked_reason = "delete mutates hidden-class shape, defeating shape-stable property access.",
        .failure_class = "hidden-class shape mutation",
        .proof_unlocked = "shape-stable property access",
    },
    .{
        .name = "enum",
        .status = .blocked,
        .alternative = "use object literals or discriminated unions",
        .blocked_reason = "TS enums emit dual numeric/string lookups that bypass exhaustive match checking.",
        .failure_class = "dual numeric/string lookup and non-exhaustive cases",
        .proof_unlocked = "exhaustive match coverage on discriminated unions",
    },
    .{
        .name = "decorator (@)",
        .status = .blocked,
        .alternative = "use function composition",
        .blocked_reason = "decorators rewrite their target at runtime in ways the contract extractor cannot trace.",
        .failure_class = "implicit metaprogramming and target rewriting",
        .proof_unlocked = "static call-graph and visible effect composition",
    },
    .{
        .name = "namespace",
        .status = .blocked,
        .alternative = "use ES6 modules",
        .blocked_reason = "TS namespaces compile to closures with mutable internals invisible to the module graph.",
        .failure_class = "module-graph blind spots",
        .proof_unlocked = "AST-driven contract extraction",
    },
};

// Comptime assertion: every blocked feature must populate `alternative`,
// `blocked_reason`, `failure_class`, and `proof_unlocked`. Allowed features
// must leave all four null. Drift causes a compile-time error.
comptime {
    for (features) |f| {
        switch (f.status) {
            .blocked => {
                if (f.alternative == null) @compileError("blocked feature missing alternative: " ++ f.name);
                if (f.blocked_reason == null) @compileError("blocked feature missing blocked_reason: " ++ f.name);
                if (f.failure_class == null) @compileError("blocked feature missing failure_class: " ++ f.name);
                if (f.proof_unlocked == null) @compileError("blocked feature missing proof_unlocked: " ++ f.name);
            },
            .allowed => {
                if (f.alternative != null) @compileError("allowed feature has alternative: " ++ f.name);
                if (f.blocked_reason != null) @compileError("allowed feature has blocked_reason: " ++ f.name);
                if (f.failure_class != null) @compileError("allowed feature has failure_class: " ++ f.name);
                if (f.proof_unlocked != null) @compileError("allowed feature has proof_unlocked: " ++ f.name);
            },
        }
    }
}

pub fn writeFeaturesJson(writer: anytype) !void {
    try writer.writeByte('[');
    for (features, 0..) |f, i| {
        if (i > 0) try writer.writeByte(',');
        try writer.writeAll("{\"name\":");
        try writeJsonString(writer, f.name);
        try writer.writeAll(",\"status\":");
        try writeJsonString(writer, f.status.label());
        try writer.writeAll(",\"alternative\":");
        try writeOptionalString(writer, f.alternative);
        try writer.writeAll(",\"blocked_reason\":");
        try writeOptionalString(writer, f.blocked_reason);
        try writer.writeAll(",\"failure_class\":");
        try writeOptionalString(writer, f.failure_class);
        try writer.writeAll(",\"proof_unlocked\":");
        try writeOptionalString(writer, f.proof_unlocked);
        try writer.writeByte('}');
    }
    try writer.writeAll("]\n");
}

pub fn writeFeaturesText(writer: anytype) !void {
    try writer.writeAll("Allowed:\n");
    for (features) |f| {
        if (f.status == .allowed) {
            try writer.print("  {s}\n", .{f.name});
        }
    }
    try writer.writeAll("\nBlocked:\n");
    for (features) |f| {
        if (f.status == .blocked) {
            if (f.alternative) |alt| {
                try writer.print("  {s} - {s}\n", .{ f.name, alt });
            } else {
                try writer.print("  {s}\n", .{f.name});
            }
        }
    }
}

fn writeOptionalString(writer: anytype, value: ?[]const u8) !void {
    if (value) |s| {
        try writeJsonString(writer, s);
    } else {
        try writer.writeAll("null");
    }
}

// -------------------------------------------------------------------------
// Restriction-to-proof table
//
// Same source-of-truth as the features list. The restrictions surface
// projects each blocked feature into a (failure_class, proof_unlocked,
// alternative) row, grouped either by proof or by failure class.
// -------------------------------------------------------------------------

/// View of a blocked feature for human-facing renderers (terminal diagnostic
/// frame, Studio overlay). All fields are guaranteed non-null because they are
/// asserted at compile time on `.blocked` entries.
pub const RestrictionInfo = struct {
    name: []const u8,
    blocked_reason: []const u8,
    failure_class: []const u8,
    proof_unlocked: []const u8,
    alternative: []const u8,
};

/// Look up a blocked feature by the keyword the parser reports. Exact match
/// wins; on miss, falls back to substring match so `"switch"` finds
/// `"switch/case"` and `"do"` finds `"do...while"`. Returns null for allowed
/// features or unknown tokens.
pub fn lookupRestriction(name: []const u8) ?RestrictionInfo {
    for (features) |f| {
        if (f.status != .blocked) continue;
        if (std.mem.eql(u8, f.name, name)) return restrictionInfoFrom(f);
    }
    for (features) |f| {
        if (f.status != .blocked) continue;
        if (std.mem.indexOf(u8, f.name, name) != null) return restrictionInfoFrom(f);
    }
    return null;
}

fn restrictionInfoFrom(f: Feature) RestrictionInfo {
    return .{
        .name = f.name,
        .blocked_reason = f.blocked_reason.?,
        .failure_class = f.failure_class.?,
        .proof_unlocked = f.proof_unlocked.?,
        .alternative = f.alternative.?,
    };
}

/// Extract the quoted feature keyword from a parser diagnostic message like
/// `"'var' is not supported; use 'let' or 'const' instead"`. Returns null when
/// the message does not start with a single-quoted token.
pub fn extractFeatureFromMessage(message: []const u8) ?[]const u8 {
    if (message.len < 2 or message[0] != '\'') return null;
    const end = std.mem.indexOfScalarPos(u8, message, 1, '\'') orelse return null;
    return message[1..end];
}

pub const RestrictionGrouping = enum { proof, failure_class, none };

pub fn writeRestrictionsJson(writer: anytype) !void {
    try writer.writeByte('[');
    var first = true;
    for (features) |f| {
        if (f.status != .blocked) continue;
        if (!first) try writer.writeByte(',');
        first = false;
        try writer.writeAll("{\"name\":");
        try writeJsonString(writer, f.name);
        try writer.writeAll(",\"alternative\":");
        try writeOptionalString(writer, f.alternative);
        try writer.writeAll(",\"blocked_reason\":");
        try writeOptionalString(writer, f.blocked_reason);
        try writer.writeAll(",\"failure_class\":");
        try writeOptionalString(writer, f.failure_class);
        try writer.writeAll(",\"proof_unlocked\":");
        try writeOptionalString(writer, f.proof_unlocked);
        try writer.writeByte('}');
    }
    try writer.writeAll("]\n");
}

pub fn writeRestrictionsText(writer: anytype, group_by: RestrictionGrouping) !void {
    switch (group_by) {
        .none => {
            try writer.writeAll("Restriction -> Failure class -> Proof unlocked\n\n");
            for (features) |f| {
                if (f.status != .blocked) continue;
                try writer.print("{s}\n", .{f.name});
                try writer.print("  reason:    {s}\n", .{f.blocked_reason.?});
                try writer.print("  failure:   {s}\n", .{f.failure_class.?});
                try writer.print("  proof:     {s}\n", .{f.proof_unlocked.?});
                try writer.print("  alternative: {s}\n\n", .{f.alternative.?});
            }
        },
        .proof => try writeGroupedBy(writer, .proof),
        .failure_class => try writeGroupedBy(writer, .failure_class),
    }
}

fn writeGroupedBy(writer: anytype, group_by: RestrictionGrouping) !void {
    const header = switch (group_by) {
        .proof => "Grouped by proof unlocked",
        .failure_class => "Grouped by failure class",
        .none => unreachable,
    };
    try writer.print("{s}\n\n", .{header});

    var seen_buf: [features.len][]const u8 = undefined;
    var seen_len: usize = 0;

    for (features) |f| {
        if (f.status != .blocked) continue;
        const key = switch (group_by) {
            .proof => f.proof_unlocked.?,
            .failure_class => f.failure_class.?,
            .none => unreachable,
        };
        if (containsKey(seen_buf[0..seen_len], key)) continue;
        seen_buf[seen_len] = key;
        seen_len += 1;

        try writer.print("{s}\n", .{key});
        for (features) |g| {
            if (g.status != .blocked) continue;
            const g_key = switch (group_by) {
                .proof => g.proof_unlocked.?,
                .failure_class => g.failure_class.?,
                .none => unreachable,
            };
            if (!std.mem.eql(u8, g_key, key)) continue;
            try writer.print("  - {s} (alternative: {s})\n", .{ g.name, g.alternative.? });
        }
        try writer.writeByte('\n');
    }
}

fn containsKey(keys: []const []const u8, key: []const u8) bool {
    for (keys) |k| {
        if (std.mem.eql(u8, k, key)) return true;
    }
    return false;
}

/// Emit the canonical `docs/restrictions-to-proofs.md` derived from the
/// `features` table. The on-disk doc is asserted to match this output by a
/// test so the doc cannot drift from the registry.
pub fn writeRestrictionsMarkdown(writer: anytype) !void {
    try writer.writeAll(
        \\# Restrictions to Proofs
        \\
        \\This document is generated from `packages/tools/src/json_diagnostics.zig`.
        \\It maps each zts language restriction to the failure class it eliminates
        \\and the proof it unlocks. Run `zts restrictions` for the live table.
        \\
        \\Every entry is a deliberate cut from JavaScript or TypeScript that buys
        \\a specific soundness guarantee. The author-declared intent assertions
        \\(extracted with `-Dcontract`) and the contract diff (`zttp proofs show`)
        \\live above these cuts; the cuts themselves are what make those
        \\higher-level claims possible.
        \\
        \\| Restriction | Failure class prevented | Proof unlocked | Alternative |
        \\|-------------|-------------------------|----------------|-------------|
        \\
    );
    for (features) |f| {
        if (f.status != .blocked) continue;
        try writer.print(
            "| `{s}` | {s} | {s} | {s} |\n",
            .{ f.name, f.failure_class.?, f.proof_unlocked.?, f.alternative.? },
        );
    }
    try writer.writeAll(
        \\
        \\## Why
        \\
        \\Per-restriction rationale. Each line answers "why is this cut worth
        \\making?" in one sentence.
        \\
        \\
    );
    for (features) |f| {
        if (f.status != .blocked) continue;
        try writer.print("- **`{s}`** - {s}\n", .{ f.name, f.blocked_reason.? });
    }
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

test "splitSuggestion: unsupported feature message" {
    const result = splitSuggestion("'while' is not supported; use 'for-of' with a finite collection instead");
    try std.testing.expectEqualStrings("'while' is not supported", result.msg);
    try std.testing.expectEqualStrings("use 'for-of' with a finite collection instead", result.suggestion.?);
}

test "splitSuggestion: no semicolon" {
    const result = splitSuggestion("unexpected token");
    try std.testing.expectEqualStrings("unexpected token", result.msg);
    try std.testing.expect(result.suggestion == null);
}

test "parserErrorCode maps all kinds" {
    // Spot check a few
    try std.testing.expectEqualStrings("ZTS001", parserErrorCode(.unsupported_feature));
    try std.testing.expectEqualStrings("ZTS002", parserErrorCode(.unexpected_token));
    try std.testing.expectEqualStrings("ZTS037", parserErrorCode(.invalid_import));
}

test "boolCheckerCode maps all kinds" {
    try std.testing.expectEqualStrings("ZTS100", boolCheckerCode(.condition_not_boolean));
    try std.testing.expectEqualStrings("ZTS105", boolCheckerCode(.mixed_type_add));
}

test "typeCheckerCode maps all kinds" {
    try std.testing.expectEqualStrings("ZTS200", typeCheckerCode(.type_mismatch));
    try std.testing.expectEqualStrings("ZTS202", typeCheckerCode(.arg_count_mismatch));
}

test "verifierCode maps all kinds" {
    try std.testing.expectEqualStrings("ZTS300", verifierCode(.missing_return_else));
    try std.testing.expectEqualStrings("ZTS303", verifierCode(.unchecked_result_value));
    try std.testing.expectEqualStrings("ZTS305", verifierCode(.unused_variable));
    try std.testing.expectEqualStrings("ZTS310", verifierCode(.module_scope_mutation));
}

test "fromParseError: unsupported feature extracts suggestion" {
    const err = ParseError{
        .kind = .unsupported_feature,
        .location = .{ .line = 23, .column = 3, .offset = 100 },
        .message = "'try/catch' is not supported; use Result types for error handling",
        .token_text = null,
        .expected = null,
    };
    const diag = fromParseError(err, "handler.ts");
    try std.testing.expectEqualStrings("ZTS001", diag.code);
    try std.testing.expectEqualStrings("error", diag.severity);
    try std.testing.expectEqualStrings("'try/catch' is not supported", diag.message);
    try std.testing.expectEqualStrings("handler.ts", diag.file);
    try std.testing.expectEqual(@as(u32, 23), diag.line);
    try std.testing.expectEqual(@as(u32, 3), diag.column);
    try std.testing.expectEqualStrings("use Result types for error handling", diag.suggestion.?);
}

test "fromParseError: expected token uses expected field" {
    const err = ParseError{
        .kind = .expected_token,
        .location = .{ .line = 5, .column = 10, .offset = 40 },
        .message = "unexpected token",
        .token_text = null,
        .expected = "';'",
    };
    const diag = fromParseError(err, "handler.ts");
    try std.testing.expectEqualStrings("ZTS003", diag.code);
    try std.testing.expectEqualStrings("';'", diag.suggestion.?);
}

test "writeRestrictionsJson includes canonical entries" {
    const allocator = std.testing.allocator;
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try writeRestrictionsJson(&aw.writer);
    buf = aw.toArrayList();

    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"try/catch\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "Result narrowing") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "async/await") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "deterministic effect boundary") != null);
}

test "writeRestrictionsText grouped by proof clusters entries" {
    const allocator = std.testing.allocator;
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try writeRestrictionsText(&aw.writer, .proof);
    buf = aw.toArrayList();

    try std.testing.expect(std.mem.indexOf(u8, buf.items, "Grouped by proof unlocked") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "finite path enumeration and termination") != null);
}

test "writeRestrictionsMarkdown mentions every blocked feature" {
    const allocator = std.testing.allocator;
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try writeRestrictionsMarkdown(&aw.writer);
    buf = aw.toArrayList();

    for (features) |f| {
        if (f.status != .blocked) continue;
        const found = std.mem.indexOf(u8, buf.items, f.name) != null;
        try std.testing.expect(found);
    }
}

test "fromStripError maps any-type to ZTS041 with a suggestion" {
    const diag = zts.StripDiagnostic{ .line = 3, .column = 12, .kind = .any_type };
    const jd = fromStripError(diag, "handler.ts");
    try std.testing.expectEqualStrings("ZTS041", jd.code);
    try std.testing.expectEqualStrings("error", jd.severity);
    try std.testing.expectEqualStrings("handler.ts", jd.file);
    try std.testing.expectEqual(@as(u32, 3), jd.line);
    try std.testing.expectEqual(@as(u32, 12), jd.column);
    try std.testing.expect(jd.suggestion != null);
}

test "fromStripError maps as and satisfies to distinct codes" {
    const as_jd = fromStripError(.{ .line = 1, .column = 1, .kind = .as_assertion }, "h.ts");
    try std.testing.expectEqualStrings("ZTS042", as_jd.code);
    const sat_jd = fromStripError(.{ .line = 1, .column = 1, .kind = .satisfies_assertion }, "h.ts");
    try std.testing.expectEqualStrings("ZTS043", sat_jd.code);
}
