//! Proof Trace - per-property reasoning for the proof card.
//!
//! Every handler property the analyzer proves (deterministic, read_only,
//! no_secret_leakage, ...) carries a verdict today but no *reasoning*. This
//! module turns data the analyzer already computed - enumerated paths,
//! fault-coverage counts, flow-checker diagnostics with their counterexample
//! witnesses, and the deterministic-demotion provenance - into a small,
//! legible per-property explanation: how a proof was discharged, or the
//! concrete counterexample that broke it.
//!
//! Pure and freestanding-safe: no filesystem, no clock, no libc. The wasm
//! analyzer build emits the exact same `proofTrace` JSON as `zigts check`.

const std = @import("std");
const handler_contract = @import("handler_contract.zig");
const flow_checker = @import("flow_checker.zig");
const counterexample = @import("counterexample.zig");
const ir = @import("parser/ir.zig");
const json_utils = @import("json_utils.zig");

const HandlerProperties = handler_contract.HandlerProperties;
const HandlerContract = handler_contract.HandlerContract;

/// The family of reasoning that discharged (or failed to discharge) a property.
pub const ProofKind = enum {
    /// Backed by exhaustive symbolic path enumeration.
    path_enumeration,
    /// Backed by data-flow provenance: a source-to-sink taint trace.
    flow_trace,
    /// Backed by a single-pass structural scan of the IR.
    structural,

    pub fn asString(self: ProofKind) []const u8 {
        return switch (self) {
            .path_enumeration => "path-enumeration",
            .flow_trace => "flow-trace",
            .structural => "structural",
        };
    }
};

pub const CounterexampleKind = enum { offending_node, flow_chain };

/// A concrete demonstration of a failed proof. `offending_node` locates a
/// single source construct; `flow_chain` walks a tainted value from its
/// source call to the sink, plus the request that drives that path.
pub const ProofCounterexample = struct {
    kind: CounterexampleKind,
    /// offending_node: the source location of the breaking construct.
    line: u32 = 0,
    column: u32 = 0,
    snippet: []const u8 = "",
    /// flow_chain: ordered human-readable steps the tainted value travels.
    flow: []const []const u8 = &.{},
    request_method: []const u8 = "",
    request_url: []const u8 = "",
    request_has_auth: bool = false,
    /// The remediation, in one line.
    fix: []const u8 = "",
};

/// One property's full reasoning. `summary` is always present and legible;
/// `counterexample` is present only when `holds` is false and a concrete one
/// is derivable. Facts are only meaningful for `path_enumeration` traces.
pub const ProofTrace = struct {
    /// snake_case `HandlerProperties` field name.
    property: []const u8,
    holds: bool,
    kind: ProofKind,
    summary: []const u8,
    paths_enumerated: u32 = 0,
    paths_exhaustive: bool = false,
    failable_sites: u32 = 0,
    covered_sites: u32 = 0,
    counterexample: ?ProofCounterexample = null,
};

fn wirePropertyName(name: []const u8) []const u8 {
    if (eql(name, "result_safe")) return "results_safe";
    return name;
}

fn eql(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

const Family = enum { structural, flow, path };

fn proofKind(family: Family) ProofKind {
    return switch (family) {
        .structural => .structural,
        .flow => .flow_trace,
        .path => .path_enumeration,
    };
}

/// Static metadata for one handler property: how it is proved, the
/// counterexample tag it witnesses (flow properties only), and the
/// passing / failing one-line explanations. `failing` text for properties
/// that carry a concrete counterexample (deterministic, the flow set,
/// fault_covered) is a fallback only - `buildTrace` overrides it at runtime.
const PropertyInfo = struct {
    name: []const u8,
    family: Family,
    flow_tag: ?counterexample.PropertyTag = null,
    passing: []const u8,
    failing: []const u8,
};

const generic_failing = "The compiler could not discharge this property.";

const property_info = [_]PropertyInfo{
    .{
        .name = "pure",
        .family = .structural,
        .passing = "No virtual-module calls: the handler is a pure function of the request.",
        .failing = "The handler calls virtual modules, so it is not a pure function of the request.",
    },
    .{
        .name = "read_only",
        .family = .structural,
        .passing = "Every virtual-module call is read-classified: no state is mutated.",
        .failing = "A write-classified virtual-module call mutates state.",
    },
    .{
        .name = "stateless",
        .family = .structural,
        .passing = "Read-only and no cache reads: the handler never depends on mutable state.",
        .failing = "The handler reads mutable state (a cache or store), so its result is not stateless.",
    },
    .{
        .name = "deterministic",
        .family = .structural,
        .passing = "No Date.now() or Math.random() on any enumerated path: every run of a request is identical.",
        .failing = generic_failing,
    },
    .{
        .name = "has_egress",
        .family = .structural,
        .passing = "The handler makes outbound fetch calls.",
        .failing = "The handler makes no outbound fetch calls.",
    },
    .{
        .name = "state_isolated",
        .family = .structural,
        .passing = "No module-scope mutation in the handler body: requests cannot leak state into one another.",
        .failing = "A module-scope variable is mutated inside the handler body.",
    },
    .{
        .name = "retry_safe",
        .family = .path,
        .passing = "Read-only, or every write is inside a durable step: safe to retry.",
        .failing = "A write happens outside a durable step, so a retry could double it.",
    },
    .{
        .name = "idempotent",
        .family = .path,
        .passing = "Deterministic and retry-safe: safe under at-least-once delivery.",
        .failing = "Non-deterministic or not retry-safe: unsafe for at-least-once delivery.",
    },
    .{
        .name = "fault_covered",
        .family = .path,
        .passing = "Every failable I/O site has an explicit failure path.",
        .failing = generic_failing,
    },
    .{
        .name = "result_safe",
        .family = .path,
        .passing = "Every Result.ok access is guarded before the value is used.",
        .failing = "A Result.ok value is used without being guarded first.",
    },
    .{
        .name = "optional_safe",
        .family = .path,
        .passing = "Every optional value is narrowed before use.",
        .failing = "An optional value is used without being narrowed first.",
    },
    .{
        .name = "no_secret_leakage",
        .family = .flow,
        .flow_tag = .no_secret_leakage,
        .passing = "No secret-labelled value reaches a response body, header, or egress call.",
        .failing = generic_failing,
    },
    .{
        .name = "no_credential_leakage",
        .family = .flow,
        .flow_tag = .no_credential_leakage,
        .passing = "No credential-labelled value reaches a response body or log.",
        .failing = generic_failing,
    },
    .{
        .name = "input_validated",
        .family = .flow,
        .flow_tag = .input_validated,
        .passing = "All user input passes a validation step before any egress call.",
        .failing = "User input reaches an egress call without passing a validation step.",
    },
    .{
        .name = "pii_contained",
        .family = .flow,
        .flow_tag = .pii_contained,
        .passing = "User input never flows to an external egress host.",
        .failing = "User input flows to an external egress host.",
    },
    .{
        .name = "injection_safe",
        .family = .flow,
        .flow_tag = .injection_safe,
        .passing = "No unvalidated user input reaches a SQL or HTML sink.",
        .failing = generic_failing,
    },
};

/// Fallback for a property with no `property_info` row. The comptime check
/// below makes a missing row a compile error, so this is only reached if
/// `collect` is ever called with a name outside `HandlerProperties`.
const default_info = PropertyInfo{
    .name = "",
    .family = .structural,
    .passing = "Proven by the compiler.",
    .failing = generic_failing,
};

// Every `HandlerProperties` bool field must have a `property_info` row, so
// adding a property cannot silently fall through to generic text.
comptime {
    for (@typeInfo(HandlerProperties).@"struct".fields) |field| {
        if (field.type != bool) continue;
        var found = false;
        for (property_info) |p| {
            if (std.mem.eql(u8, p.name, field.name)) found = true;
        }
        if (!found) @compileError("proof_trace: HandlerProperties field has no property_info row: " ++ field.name);
    }
}

fn infoFor(name: []const u8) PropertyInfo {
    for (property_info) |p| {
        if (eql(p.name, name)) return p;
    }
    return default_info;
}

/// Where a flow diagnostic's tainted value lands, as a chain endpoint label.
fn sinkLabel(kind: flow_checker.DiagnosticKind) []const u8 {
    return switch (kind) {
        .secret_in_response, .credential_in_response => "the response body",
        .secret_in_log, .credential_in_log => "a log line",
        .secret_in_egress_url, .credential_in_egress_url => "an egress URL",
        .secret_in_egress_body => "an egress request body",
        .unvalidated_input_in_egress => "an egress call",
    };
}

/// Build the per-property trace set. All strings are allocated from `arena`;
/// the caller owns the arena and frees the whole set at once.
pub fn collect(
    arena: std.mem.Allocator,
    contract: *const HandlerContract,
    flow_diags: []const flow_checker.Diagnostic,
    ir_view: ir.IrView,
    paths_enumerated: u32,
    paths_exhaustive: bool,
) ![]ProofTrace {
    const props = contract.properties orelse return &.{};

    var list: std.ArrayListUnmanaged(ProofTrace) = .empty;
    inline for (@typeInfo(HandlerProperties).@"struct".fields) |field| {
        if (field.type == bool) {
            const name = field.name;
            const holds = @field(props, name);
            try list.append(arena, try buildTrace(
                arena,
                name,
                holds,
                contract,
                flow_diags,
                ir_view,
                paths_enumerated,
                paths_exhaustive,
            ));
        }
    }
    return list.toOwnedSlice(arena);
}

fn buildTrace(
    arena: std.mem.Allocator,
    name: []const u8,
    holds: bool,
    contract: *const HandlerContract,
    flow_diags: []const flow_checker.Diagnostic,
    ir_view: ir.IrView,
    paths_enumerated: u32,
    paths_exhaustive: bool,
) !ProofTrace {
    const info = infoFor(name);
    var trace: ProofTrace = .{
        .property = name,
        .holds = holds,
        .kind = proofKind(info.family),
        .summary = if (holds) info.passing else info.failing,
    };

    switch (info.family) {
        .path => {
            trace.paths_enumerated = paths_enumerated;
            trace.paths_exhaustive = paths_exhaustive;
            if (eql(name, "fault_covered")) {
                if (contract.fault_coverage) |fc| {
                    trace.failable_sites = fc.total_failable;
                    trace.covered_sites = fc.covered;
                    if (holds) {
                        trace.summary = try std.fmt.allocPrint(
                            arena,
                            "All {d} failable I/O site(s) have an explicit failure path, across {d} enumerated path(s).",
                            .{ fc.total_failable, paths_enumerated },
                        );
                    } else if (fc.total_failable == 0) {
                        trace.summary = "No failable I/O sites in this handler yet.";
                    } else {
                        trace.summary = try std.fmt.allocPrint(
                            arena,
                            "{d} of {d} failable I/O site(s) have no explicit failure path.",
                            .{ fc.total_failable -| fc.covered, fc.total_failable },
                        );
                    }
                }
            } else if (holds and paths_enumerated > 0) {
                trace.summary = try std.fmt.allocPrint(
                    arena,
                    "{s} Checked across {d} enumerated path(s){s}.",
                    .{
                        info.passing,
                        paths_enumerated,
                        if (paths_exhaustive) " (exhaustive)" else "",
                    },
                );
            }
        },
        .structural => {
            if (!holds and eql(name, "deterministic")) {
                if (contract.property_provenance.causeFor("deterministic")) |cause| {
                    trace.summary = try std.fmt.allocPrint(
                        arena,
                        "{s} reads a clock or RNG: two runs of the same request can differ.",
                        .{cause.snippet},
                    );
                    trace.counterexample = .{
                        .kind = .offending_node,
                        .line = cause.line,
                        .column = cause.column,
                        .snippet = cause.snippet,
                        .fix = "move the call inside a durable step() from zigttp:durable, or take the value from the request.",
                    };
                }
            }
        },
        .flow => {
            if (!holds) {
                if (info.flow_tag) |tag| {
                    if (findFlowDiag(flow_diags, tag)) |diag| {
                        trace.summary = diag.message;
                        trace.counterexample = try flowCounterexample(arena, diag, tag, ir_view);
                    }
                }
            }
        },
    }
    return trace;
}

fn findFlowDiag(
    flow_diags: []const flow_checker.Diagnostic,
    tag: counterexample.PropertyTag,
) ?flow_checker.Diagnostic {
    for (flow_diags) |diag| {
        const diag_tag = flow_checker.propertyTagForKind(diag.kind) orelse continue;
        if (diag_tag == tag) return diag;
    }
    return null;
}

fn flowCounterexample(
    arena: std.mem.Allocator,
    diag: flow_checker.Diagnostic,
    tag: counterexample.PropertyTag,
    ir_view: ir.IrView,
) !ProofCounterexample {
    const loc = ir_view.getLoc(diag.node);

    const constraints: []const counterexample.WitnessConstraint =
        if (diag.witness) |w| w.path_constraints else &.{};
    const io_calls: []const counterexample.TrackedIoCall =
        if (diag.witness) |w| w.io_calls else &.{};

    const witness = counterexample.solve(arena, .{
        .property = tag,
        .origin = .{ .line = if (loc) |l| l.line else 0, .column = if (loc) |l| l.column else 0 },
        .sink = .{ .line = if (loc) |l| l.line else 0, .column = if (loc) |l| l.column else 0 },
        .summary = diag.message,
        .constraints = constraints,
        .io_calls = io_calls,
    }) catch null;

    // Build the source-to-sink chain from the I/O calls observed on the
    // path, ending at the sink the diagnostic flagged.
    var flow: std.ArrayListUnmanaged([]const u8) = .empty;
    for (io_calls) |call| {
        try flow.append(arena, try std.fmt.allocPrint(arena, "{s}() call", .{call.func}));
    }
    try flow.append(arena, sinkLabel(diag.kind));

    return .{
        .kind = .flow_chain,
        .line = if (loc) |l| l.line else 0,
        .column = if (loc) |l| l.column else 0,
        .flow = try flow.toOwnedSlice(arena),
        .request_method = if (witness) |w| w.request.method else "GET",
        .request_url = if (witness) |w| w.request.url else "/",
        .request_has_auth = if (witness) |w| w.request.has_auth_header else false,
        .fix = diag.help orelse "validate or drop the untrusted value before it reaches the sink.",
    };
}

// ---------------------------------------------------------------------------
// JSON wire format
// ---------------------------------------------------------------------------

/// Emit the `proofTrace` object: one entry per UI-facing snake_case property
/// key. Additive sibling of `proof.properties`; existing parsers that read the
/// booleans are unaffected.
pub fn writeJson(writer: anytype, traces: []const ProofTrace) !void {
    try writer.writeByte('{');
    for (traces, 0..) |trace, i| {
        if (i > 0) try writer.writeByte(',');
        try json_utils.writeJsonString(writer, wirePropertyName(trace.property));
        try writer.writeAll(":{\"holds\":");
        try writer.writeAll(if (trace.holds) "true" else "false");
        try writer.writeAll(",\"kind\":");
        try json_utils.writeJsonString(writer, trace.kind.asString());
        try writer.writeAll(",\"summary\":");
        try json_utils.writeJsonString(writer, trace.summary);

        if (trace.kind == .path_enumeration) {
            try writer.print(
                ",\"facts\":{{\"pathsEnumerated\":{d},\"pathsExhaustive\":{},\"failableSites\":{d},\"coveredSites\":{d}}}",
                .{ trace.paths_enumerated, trace.paths_exhaustive, trace.failable_sites, trace.covered_sites },
            );
        }

        if (trace.counterexample) |cx| {
            try writer.writeAll(",\"counterexample\":");
            try writeCounterexample(writer, cx);
        }
        try writer.writeByte('}');
    }
    try writer.writeByte('}');
}

fn writeCounterexample(writer: anytype, cx: ProofCounterexample) !void {
    switch (cx.kind) {
        .offending_node => {
            try writer.print(
                "{{\"kind\":\"offending-node\",\"location\":{{\"line\":{d},\"column\":{d}}},\"snippet\":",
                .{ cx.line, cx.column },
            );
            try json_utils.writeJsonString(writer, cx.snippet);
            try writer.writeAll(",\"fix\":");
            try json_utils.writeJsonString(writer, cx.fix);
            try writer.writeByte('}');
        },
        .flow_chain => {
            try writer.print(
                "{{\"kind\":\"flow-chain\",\"sink\":{{\"line\":{d},\"column\":{d}}},\"flow\":[",
                .{ cx.line, cx.column },
            );
            for (cx.flow, 0..) |step, i| {
                if (i > 0) try writer.writeByte(',');
                try json_utils.writeJsonString(writer, step);
            }
            try writer.writeAll("],\"request\":{\"method\":");
            try json_utils.writeJsonString(writer, cx.request_method);
            try writer.writeAll(",\"url\":");
            try json_utils.writeJsonString(writer, cx.request_url);
            try writer.print(",\"hasAuthHeader\":{}}}", .{cx.request_has_auth});
            try writer.writeAll(",\"fix\":");
            try json_utils.writeJsonString(writer, cx.fix);
            try writer.writeByte('}');
        },
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "infoFor classifies every property family" {
    try testing.expectEqual(Family.flow, infoFor("no_secret_leakage").family);
    try testing.expectEqual(Family.flow, infoFor("injection_safe").family);
    try testing.expectEqual(Family.path, infoFor("fault_covered").family);
    try testing.expectEqual(Family.path, infoFor("retry_safe").family);
    try testing.expectEqual(Family.structural, infoFor("deterministic").family);
    try testing.expectEqual(Family.structural, infoFor("read_only").family);
}

test "writeJson emits a path-enumeration trace with facts" {
    const traces = [_]ProofTrace{.{
        .property = "fault_covered",
        .holds = true,
        .kind = .path_enumeration,
        .summary = "ok",
        .paths_enumerated = 5,
        .paths_exhaustive = true,
        .failable_sites = 3,
        .covered_sites = 3,
    }};
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);
    try writeJson(&aw.writer, &traces);
    buf = aw.toArrayList();

    try testing.expect(std.mem.indexOf(u8, buf.items, "\"fault_covered\":{") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"kind\":\"path-enumeration\"") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"pathsEnumerated\":5") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"coveredSites\":3") != null);
}

test "writeJson maps result_safe to results_safe UI key" {
    const traces = [_]ProofTrace{.{
        .property = "result_safe",
        .holds = false,
        .kind = .path_enumeration,
        .summary = "A Result.ok value is used without being guarded first.",
    }};
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);
    try writeJson(&aw.writer, &traces);
    buf = aw.toArrayList();

    try testing.expect(std.mem.indexOf(u8, buf.items, "\"results_safe\":{") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"result_safe\":{") == null);
}

test "writeJson emits an offending-node counterexample" {
    const traces = [_]ProofTrace{.{
        .property = "deterministic",
        .holds = false,
        .kind = .structural,
        .summary = "Date.now() reads a clock",
        .counterexample = .{
            .kind = .offending_node,
            .line = 14,
            .column = 9,
            .snippet = "Date.now()",
            .fix = "use durable step()",
        },
    }};
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);
    try writeJson(&aw.writer, &traces);
    buf = aw.toArrayList();

    try testing.expect(std.mem.indexOf(u8, buf.items, "\"kind\":\"offending-node\"") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"line\":14") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"snippet\":\"Date.now()\"") != null);
}

test "writeJson emits a flow-chain counterexample" {
    const steps = [_][]const u8{ "env() call", "the response body" };
    const traces = [_]ProofTrace{.{
        .property = "no_secret_leakage",
        .holds = false,
        .kind = .flow_trace,
        .summary = "SECRET flows into the response",
        .counterexample = .{
            .kind = .flow_chain,
            .line = 7,
            .column = 12,
            .flow = &steps,
            .request_method = "POST",
            .request_url = "/leak",
            .request_has_auth = false,
            .fix = "do not return env values",
        },
    }};
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);
    try writeJson(&aw.writer, &traces);
    buf = aw.toArrayList();

    try testing.expect(std.mem.indexOf(u8, buf.items, "\"kind\":\"flow-chain\"") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"flow\":[\"env() call\",\"the response body\"]") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"method\":\"POST\"") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"hasAuthHeader\":false") != null);
}
