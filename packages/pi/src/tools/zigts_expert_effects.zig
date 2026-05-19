//! zigts_expert_effects - report the inferred effect row for every named
//! function in a source file.
//!
//! Bottom-up effect inference extends the proof boundary across helpers.
//! Each named function gets capabilities, determinism, purity, recursion,
//! and egress flags. The expert calls this before proposing edits so it
//! can warn the user when an edit widens a helper's effect row.

const std = @import("std");
const zigts = @import("zigts");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");

const ir = zigts.parser;
const effect_inference = zigts.effect_inference;

const name = "zigts_expert_effects";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "effect rows",
    .description =
    \\Report the inferred effect row for every named function in a source
    \\file. Each row records the union of capabilities required by direct
    \\or transitive calls, plus determinism, purity, recursion, and
    \\egress flags. Use this before proposing edits to surface the effect
    \\delta of a refactor; widening an effect row should be a conscious
    \\decision the user reviews on the proof card.
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

    const source = zigts.file_io.readFile(
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

    var strip_result = zigts.strip(allocator, source, .{
        .comptime_env = .{},
    }) catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": TypeScript strip failed: {s}\n",
            .{@errorName(e)},
        );
    };
    defer strip_result.deinit();

    var atoms = zigts.context.AtomTable.init(allocator);
    defer atoms.deinit();
    var js_parser = zigts.parser.JsParser.init(allocator, strip_result.code);
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

    var analyzer = effect_inference.Analyzer.init(allocator, ir_view, &atoms);
    defer analyzer.deinit();
    analyzer.analyze(program_root) catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": analyze failed: {s}\n",
            .{@errorName(e)},
        );
    };

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try writeEnvelope(&aw.writer, args[0], analyzer.all());
    buf = aw.toArrayList();

    return .{
        .ok = true,
        .llm_text = try buf.toOwnedSlice(allocator),
    };
}

fn writeEnvelope(
    writer: *std.Io.Writer,
    path: []const u8,
    functions: []const effect_inference.FunctionEffect,
) !void {
    try writer.writeAll("{\"path\":\"");
    try writer.writeAll(path);
    try writer.writeAll("\",\"functions\":[");
    for (functions, 0..) |fe, i| {
        if (i > 0) try writer.writeByte(',');
        try writeFunction(writer, fe);
    }
    try writer.writeAll("]}");
}

fn writeFunction(
    writer: *std.Io.Writer,
    fe: effect_inference.FunctionEffect,
) !void {
    try writer.writeAll("{\"name\":\"");
    try writer.writeAll(fe.name);
    try writer.writeAll("\",\"pure\":");
    try writer.writeAll(if (fe.row.pure) "true" else "false");
    try writer.writeAll(",\"deterministic\":");
    try writer.writeAll(if (fe.row.deterministic) "true" else "false");
    try writer.writeAll(",\"recursive\":");
    try writer.writeAll(if (fe.row.recursive) "true" else "false");
    try writer.writeAll(",\"has_egress\":");
    try writer.writeAll(if (fe.row.has_egress) "true" else "false");
    try writer.writeAll(",\"capabilities\":[");
    var first = true;
    var it = fe.row.capabilities.iterator();
    while (it.next()) |cap| {
        if (!first) try writer.writeByte(',');
        first = false;
        try writer.writeByte('"');
        try writer.writeAll(@tagName(cap));
        try writer.writeByte('"');
    }
    try writer.writeAll("]}");
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

test "tool description names the proof-card behaviour" {
    try testing.expect(std.mem.indexOf(u8, tool.description, "effect row") != null);
    try testing.expect(std.mem.indexOf(u8, tool.description, "proof card") != null);
}
