//! Preview compiler-authored canonical refactors for a handler file.

const std = @import("std");
const canonicalize = @import("zigts_cli").canonicalize;
const registry_mod = @import("../registry/registry.zig");
const zigts = @import("zigts");

const name = "zigts_expert_canonicalize";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "canonicalize preview",
    .description = "Preview local canonical ZigTS refactor intents. Set simulate=true to apply previews in memory through edit_simulate.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"file\":{\"type\":\"string\",\"description\":\"Handler file to analyze.\"},\"simulate\":{\"type\":\"boolean\",\"description\":\"Apply previews in memory and run edit_simulate.\"}},\"required\":[\"file\"]}",
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
    const obj = parsed.value.object;
    const file_value = obj.get("file") orelse return error.InvalidToolArgsJson;
    if (file_value != .string) return error.InvalidToolArgsJson;
    const simulate = if (obj.get("simulate")) |value| blk: {
        if (value != .bool) return error.InvalidToolArgsJson;
        break :blk value.bool;
    } else false;

    var args: std.ArrayList([]const u8) = .empty;
    errdefer args.deinit(allocator);
    try args.append(allocator, try allocator.dupe(u8, file_value.string));
    if (simulate) try args.append(allocator, "--simulate");
    return try args.toOwnedSlice(allocator);
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0 or args.len > 2 or (args.len == 2 and !std.mem.eql(u8, args[1], "--simulate"))) {
        return registry_mod.ToolResult.err(allocator, "zigts_expert_canonicalize requires a file and optional --simulate\n");
    }

    var result = try canonicalize.collect(allocator, args[0]);
    defer result.deinit(allocator);
    const simulation = if (args.len == 2)
        try canonicalize.simulateRefactors(allocator, args[0], &result)
    else
        null;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try canonicalize.writeJsonWithSimulation(&aw.writer, &result, simulation);
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

test "tool decodes simulate arg" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const args = try decodeJson(arena.allocator(), "{\"file\":\"handler.ts\",\"simulate\":true}");
    try testing.expectEqual(@as(usize, 2), args.len);
    try testing.expectEqualStrings("handler.ts", args[0]);
    try testing.expectEqualStrings("--simulate", args[1]);
}

test "tool execute returns canonicalize JSON envelope" {
    const source =
        \\const parse = (x: number): number => x;
        \\function handler(req: Request): Response {
        \\  const a = parse(1);
        \\  const b = parse(2);
        \\  return Response.json({ a, b });
        \\}
    ;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try std.fmt.allocPrint(testing.allocator, ".zig-cache/tmp/{s}", .{tmp.sub_path});
    defer testing.allocator.free(root);
    const file = try std.fs.path.join(testing.allocator, &.{ root, "handler.ts" });
    defer testing.allocator.free(file);
    try zigts.file_io.writeFile(testing.allocator, file, source);

    var result = try execute(testing.allocator, &.{ file, "--simulate" });
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, result.llm_text, .{});
    defer parsed.deinit();
    try testing.expect(parsed.value == .object);
    const obj = parsed.value.object;
    try testing.expect((obj.get("ok") orelse return error.MissingOk) == .bool);
    try testing.expect((obj.get("ok") orelse return error.MissingOk).bool);
    try testing.expectEqualStrings(file, (obj.get("file") orelse return error.MissingFile).string);
    try testing.expectEqual(@as(usize, 64), (obj.get("policy_hash") orelse return error.MissingPolicyHash).string.len);
    const refactors = obj.get("refactors") orelse return error.MissingRefactors;
    try testing.expect(refactors == .array);
    try testing.expectEqual(@as(usize, 1), refactors.array.items.len);
    const refactor = refactors.array.items[0].object;
    try testing.expectEqualStrings("canonicalize_arrow_helper", (refactor.get("kind") orelse return error.MissingKind).string);
    try testing.expectEqualStrings("function parse(x: number): number { return x; }", (refactor.get("replacement") orelse return error.MissingReplacement).string);
    const simulation = obj.get("simulation") orelse return error.MissingSimulation;
    try testing.expect(simulation == .object);
    try testing.expect((simulation.object.get("ok") orelse return error.MissingOk) == .bool);
    try testing.expect((simulation.object.get("ok") orelse return error.MissingOk).bool);
}
