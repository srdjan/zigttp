//! zts_expert_narrow - per-path label flow report for a handler.
//!
//! Runs FlowChecker on the handler and reports each diagnostic together with
//! the path constraints that witness it (method comparisons, stub truthiness,
//! result-ok narrowing). The expert uses this to suggest a discriminating
//! guard that would close a leak on one branch without restructuring the
//! handler; the autoloop uses it as the input to /guard-for synthesis.

const std = @import("std");
const zts = @import("zts");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");

const ir = zts.parser;
const flow_checker = zts.flow_checker;
const counterexample = zts.counterexample;
const handler_verifier = zts.handler_verifier;
const writeJsonString = zts.handler_contract.writeJsonString;

const name = "zts_expert_narrow";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "narrow",
    .effect = .analyze,
    .description =
    \\Report per-path label flow for a handler. Each diagnostic carries the
    \\path constraints (request method comparisons, stub truthiness checks,
    \\result-ok narrowing) that witness it. The expert reads this to
    \\propose a discriminating guard that would prove a sink safe on the
    \\offending branch instead of restructuring the handler. Compose with
    \\pi_repair_plan to land the guard, then re-run zts_expert_narrow to
    \\confirm the path is closed.
    ,
    .input_schema = "{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"}},\"required\":[\"path\"]}",
    .decode_json = decodeJson,
    .execute = execute,
};

fn decodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    return registry_mod.helpers.decodeSingleStringField(allocator, args_json, "path");
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) {
        return registry_mod.ToolResult.err(allocator, name ++ ": requires a path\n");
    }

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const absolute = try common.resolveInsideWorkspace(allocator, root, args[0]);
    defer allocator.free(absolute);

    const source = zts.file_io.readFile(
        allocator,
        absolute,
        common.default_output_limit,
    ) catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": cannot read {s}: {s}\n",
            .{ args[0], @errorName(e) },
        );
    };
    defer allocator.free(source);

    var strip_result = zts.strip(allocator, source, .{
        .comptime_env = .{},
    }) catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": TypeScript strip failed: {s}\n",
            .{@errorName(e)},
        );
    };
    defer strip_result.deinit();

    var atoms = zts.context.AtomTable.init(allocator);
    defer atoms.deinit();
    var js_parser = zts.parser.JsParser.init(allocator, strip_result.code);
    defer js_parser.deinit();
    js_parser.setAtomTable(&atoms);

    const program_root = js_parser.parse() catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": parse failed: {s}\n",
            .{@errorName(e)},
        );
    };
    const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);

    const handler_fn = handler_verifier.findHandlerFunction(ir_view, program_root) orelse {
        return registry_mod.ToolResult.err(
            allocator,
            name ++ ": no handler function found in file\n",
        );
    };

    var checker = zts.FlowChecker.init(allocator, ir_view, &atoms);
    defer checker.deinit();
    _ = checker.check(handler_fn) catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": flow check failed: {s}\n",
            .{@errorName(e)},
        );
    };

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try writeEnvelope(&aw.writer, args[0], checker.getDiagnostics());
    buf = aw.toArrayList();

    return .{
        .ok = true,
        .llm_text = try buf.toOwnedSlice(allocator),
    };
}

fn writeEnvelope(
    writer: *std.Io.Writer,
    path: []const u8,
    diagnostics: []const flow_checker.Diagnostic,
) !void {
    try writer.writeAll("{\"path\":\"");
    try writer.writeAll(path);
    try writer.writeAll("\",\"witnesses\":[");
    var emitted: usize = 0;
    for (diagnostics) |diag| {
        const witness = diag.witness orelse continue;
        const tag = flow_checker.propertyTagForKind(diag.kind) orelse continue;
        if (emitted > 0) try writer.writeByte(',');
        emitted += 1;
        try writeWitness(writer, tag, diag, witness);
    }
    try writer.writeAll("]}");
}

fn writeWitness(
    writer: *std.Io.Writer,
    tag: counterexample.PropertyTag,
    diag: flow_checker.Diagnostic,
    witness: flow_checker.Witness,
) !void {
    try writer.writeAll("{\"property\":\"");
    try writer.writeAll(tag.asString());
    try writer.writeAll("\",\"message\":");
    try writeJsonString(writer, diag.message);
    try writer.writeAll(",\"conditions\":[");
    for (witness.path_constraints, 0..) |c, i| {
        if (i > 0) try writer.writeByte(',');
        try writeConstraint(writer, c);
    }
    try writer.writeAll("]}");
}

fn writeConstraint(writer: *std.Io.Writer, c: counterexample.WitnessConstraint) !void {
    switch (c) {
        .req_method => |value| {
            try writer.writeAll("{\"kind\":\"req_method\",\"value\":");
            try writeJsonString(writer, value);
            try writer.writeAll("}");
        },
        .req_method_not => |value| {
            try writer.writeAll("{\"kind\":\"req_method_not\",\"value\":");
            try writeJsonString(writer, value);
            try writer.writeAll("}");
        },
        .req_url => |value| {
            try writer.writeAll("{\"kind\":\"req_url\",\"value\":");
            try writeJsonString(writer, value);
            try writer.writeAll("}");
        },
        .req_url_not => |value| {
            try writer.writeAll("{\"kind\":\"req_url_not\",\"value\":");
            try writeJsonString(writer, value);
            try writer.writeAll("}");
        },
        .stub_truthy => |info| try writeStubConstraint(writer, "stub_truthy", info),
        .stub_falsy => |info| try writeStubConstraint(writer, "stub_falsy", info),
        .result_ok => |info| try writeStubConstraint(writer, "result_ok", info),
        .result_not_ok => |info| try writeStubConstraint(writer, "result_not_ok", info),
    }
}

fn writeStubConstraint(
    writer: *std.Io.Writer,
    kind: []const u8,
    info: counterexample.StubInfo,
) !void {
    try writer.writeAll("{\"kind\":\"");
    try writer.writeAll(kind);
    try writer.writeAll("\",\"module\":");
    try writeJsonString(writer, info.module);
    try writer.writeAll(",\"func\":");
    try writeJsonString(writer, info.func);
    try writer.writeAll("}");
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "missing args returns not-ok" {
    var result = try execute(testing.allocator, &.{});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "requires a path") != null);
}

test "tool description mentions guard synthesis" {
    try testing.expect(std.mem.indexOf(u8, tool.description, "discriminating guard") != null);
    try testing.expect(std.mem.indexOf(u8, tool.description, "path constraints") != null);
}
