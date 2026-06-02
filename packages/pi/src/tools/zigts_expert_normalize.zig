//! Reduce a handler file to its unique Canonical Normal Form.
//!
//! Read-only: returns the canonical source, the `fullyCanonical` fixed-point
//! flag, any residual diagnostics, and the rewrite trace. The agent applies the
//! canonical source through its normal `apply_edit` path; this tool never writes
//! the file. Distinct from `zigts_expert_canonicalize`, which previews single
//! per-node refactor intents - this returns the fixed point.

const std = @import("std");
const canonicalize = @import("zigts_cli").canonicalize;
const registry_mod = @import("../registry/registry.zig");

const name = "zigts_expert_normalize";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "normalize",
    .description = "Reduce a handler file to its unique Canonical Normal Form and return the canonical source, fullyCanonical, residualDiagnostics, and the rewrite trace. Read-only: never writes the file. Use it to canonicalize a draft before applying, so the compiler veto never rejects it on a ZTS6xx canonical-form violation.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"file\":{\"type\":\"string\",\"description\":\"Handler file to normalize.\"}},\"required\":[\"file\"]}",
    .decode_json = decodeJson,
    .execute = execute,
};

fn decodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, args_json, .{});
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidToolArgsJson;
    const file_value = parsed.value.object.get("file") orelse return error.InvalidToolArgsJson;
    if (file_value != .string) return error.InvalidToolArgsJson;

    var args: std.ArrayList([]const u8) = .empty;
    errdefer args.deinit(allocator);
    try args.append(allocator, try allocator.dupe(u8, file_value.string));
    return try args.toOwnedSlice(allocator);
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len != 1) {
        return registry_mod.ToolResult.err(allocator, "zigts_expert_normalize requires a single file argument\n");
    }

    var result = try canonicalize.normalize(allocator, args[0]);
    defer result.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try canonicalize.writeNormalizeJson(&aw.writer, args[0], &result, false);
    buf = aw.toArrayList();

    return .{
        .ok = true,
        .llm_text = try buf.toOwnedSlice(allocator),
    };
}

const testing = std.testing;

test "tool decodes file arg" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const args = try decodeJson(arena.allocator(), "{\"file\":\"handler.ts\"}");
    try testing.expectEqual(@as(usize, 1), args.len);
    try testing.expectEqualStrings("handler.ts", args[0]);
}

test "tool execute returns a canonical-source envelope" {
    const source =
        \\function handler(req: Request): Response {
        \\  let msg = "hi";
        \\  return Response.json({ msg });
        \\}
    ;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try std.fmt.allocPrint(testing.allocator, ".zig-cache/tmp/{s}", .{tmp.sub_path});
    defer testing.allocator.free(root);
    const file = try std.fs.path.join(testing.allocator, &.{ root, "handler.ts" });
    defer testing.allocator.free(file);
    const zigts = @import("zigts");
    try zigts.file_io.writeFile(testing.allocator, file, source);

    var result = try execute(testing.allocator, &.{file});
    defer result.deinit(testing.allocator);
    try testing.expect(result.ok);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, result.llm_text, .{});
    defer parsed.deinit();
    const obj = parsed.value.object;
    try testing.expect((obj.get("ok") orelse return error.MissingOk).bool);
    try testing.expect((obj.get("fullyCanonical") orelse return error.MissingFlag).bool);
    const trace = (obj.get("rewriteTrace") orelse return error.MissingTrace).array;
    try testing.expectEqual(@as(usize, 1), trace.items.len);
    try testing.expectEqualStrings("replace_let_with_const", trace.items[0].string);
    const canonical_source = (obj.get("canonicalSource") orelse return error.MissingSource).string;
    try testing.expect(std.mem.indexOf(u8, canonical_source, "const msg") != null);
}
