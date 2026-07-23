//! zts_expert_ratchet - report the proven-property set for a handler.
//!
//! Mirrors `zttp ratchet show` from the expert side. The compiler already
//! infers every handler's property set and writes it to contract.json under
//! `provenSpecs`. The expert uses this tool to surface the current proven
//! set so a `/ratchet` or `/tighten` prompt can anchor in compiler facts
//! instead of LLM speculation. Cross-build deltas are derived from the
//! handler's own `Spec<...>` declarations — there is no baseline file to
//! pass — so this tool intentionally exposes only the `path` field; the
//! delta belongs to the corresponding CLI surface (`zttp ratchet check`).

const std = @import("std");
const zts = @import("zts");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");

const HandlerProperties = zts.handler_contract.HandlerProperties;

const name = "zts_expert_ratchet";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "ratchet",
    .effect = .analyze,
    .description =
    \\Report the property set the compiler currently proves for a handler.
    \\The set comes from contract.json under provenSpecs (also signed inside
    \\the Zttp-Attest JWS), so it is mechanical and attestable. Use this
    \\to ground a /ratchet or /tighten suggestion: read the current set,
    \\identify the next reachable property, propose the minimal edit, and
    \\verify with edit_simulate before drafting.
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

    const source = zts.file_io.readFile(allocator, absolute, common.default_output_limit) catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": cannot read {s}: {s}\n",
            .{ args[0], @errorName(e) },
        );
    };
    defer allocator.free(source);

    const precompile = @import("zts_cli").precompile;
    var compiled = precompile.compileHandler(allocator, source, args[0], .{
        .emit_contract = true,
        .emit_verify = true,
    }) catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": compile failed: {s}\n",
            .{@errorName(e)},
        );
    };
    defer compiled.deinit(allocator);

    const contract = compiled.contract orelse {
        return registry_mod.ToolResult.err(allocator, name ++ ": no contract emitted\n");
    };
    const props = contract.properties orelse {
        return registry_mod.ToolResult.err(allocator, name ++ ": no properties emitted\n");
    };

    var buf: [HandlerProperties.max_proven_specs]?[]const u8 = undefined;
    const count = props.provenSpecNames(&buf);

    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &out);
    const w = &aw.writer;

    try w.writeAll("{\"path\":\"");
    try w.writeAll(args[0]);
    try w.writeAll("\",\"provenSpecs\":[");
    var emitted: usize = 0;
    for (buf[0..count]) |name_opt| {
        if (name_opt) |nm| {
            if (emitted > 0) try w.writeByte(',');
            emitted += 1;
            try w.writeByte('"');
            try w.writeAll(nm);
            try w.writeByte('"');
        }
    }
    try w.writeAll("]}");

    out = aw.toArrayList();
    return .{ .ok = true, .llm_text = try out.toOwnedSlice(allocator) };
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

test "tool description names provenSpecs and the JWS link" {
    try testing.expect(std.mem.indexOf(u8, tool.description, "provenSpecs") != null);
    try testing.expect(std.mem.indexOf(u8, tool.description, "Zttp-Attest JWS") != null);
}
