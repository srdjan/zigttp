//! pi_extension_catalog - given a set of partner module manifests, report
//! which `zigttp-ext:*` specifiers (and optionally exports) are registered
//! in this session. The agent calls this before suggesting a partner import
//! so it can flag unregistered specifiers up front instead of letting the
//! compiler veto catch them after the fact.
//!
//! Input:
//!   {
//!     "module_manifest_paths": ["path/to/zigttp-module.json", ...],
//!     "specifier": "zigttp-ext:foo",
//!     "export": "bar"   // optional; checked against the manifest's exports
//!   }
//!
//! Output (llm_text):
//!   {
//!     "ok": bool,
//!     "specifier": "zigttp-ext:foo",
//!     "specifier_known": bool,
//!     "export": "bar"|null,
//!     "export_known": bool,
//!     "registered_specifiers": ["zigttp-ext:foo", ...],
//!     "parse_errors": [{ "path": "...", "error": "InvalidSpecifier" }, ...]
//!   }
//!
//! Manifest-parse failures populate `parse_errors` and set `ok=false`; the
//! agent should fail loud rather than silently treating the catalog as
//! authoritative.

const std = @import("std");
const zigts = @import("zigts");
const registry_mod = @import("../registry/registry.zig");
const writeJsonString = zigts.handler_contract.writeJsonString;

const name = "pi_extension_catalog";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "extension-catalog",
    .effect = .read_workspace,
    .description =
    \\Confirm whether a partner specifier (`zigttp-ext:*`) is registered for
    \\this session via one or more `zigttp-module.json` manifests. Pass the
    \\manifest paths the user has on disk plus the specifier (and optionally
    \\an export name) the agent is about to suggest. The tool returns
    \\specifier_known + export_known booleans alongside the registered
    \\specifier list so the agent can warn the user before drafting an
    \\import that the compiler veto would later reject.
    ,
    .input_schema = "{\"type\":\"object\",\"properties\":{\"module_manifest_paths\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}},\"specifier\":{\"type\":\"string\"},\"export\":{\"type\":\"string\"}},\"required\":[\"specifier\"]}",
    .decode_json = registry_mod.helpers.decodeJsonPassthrough,
    .execute = execute,
};

const ParseError = struct {
    path: []const u8,
    err: []const u8,
};

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len != 1) {
        return registry_mod.ToolResult.err(allocator, name ++ ": expected a single JSON object argument\n");
    }

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, args[0], .{}) catch {
        return registry_mod.ToolResult.err(allocator, name ++ ": invalid JSON input\n");
    };
    defer parsed.deinit();
    if (parsed.value != .object) {
        return registry_mod.ToolResult.err(allocator, name ++ ": expected JSON object\n");
    }
    const obj = parsed.value.object;

    const specifier_value = obj.get("specifier") orelse {
        return registry_mod.ToolResult.err(allocator, name ++ ": missing required field 'specifier'\n");
    };
    if (specifier_value != .string) {
        return registry_mod.ToolResult.err(allocator, name ++ ": 'specifier' must be a string\n");
    }
    const specifier = specifier_value.string;

    const export_name_opt: ?[]const u8 = if (obj.get("export")) |value| blk: {
        if (value != .string) return registry_mod.ToolResult.err(allocator, name ++ ": 'export' must be a string\n");
        break :blk value.string;
    } else null;

    var registry = zigts.manifest_registry.Registry.init(allocator);
    defer registry.deinit();

    var parse_errors: std.ArrayListUnmanaged(ParseError) = .empty;
    defer parse_errors.deinit(allocator);

    if (obj.get("module_manifest_paths")) |paths_value| {
        if (paths_value != .array) {
            return registry_mod.ToolResult.err(allocator, name ++ ": 'module_manifest_paths' must be an array of strings\n");
        }
        for (paths_value.array.items) |item| {
            if (item != .string) {
                return registry_mod.ToolResult.err(allocator, name ++ ": 'module_manifest_paths' must be strings\n");
            }
            try loadManifestInto(allocator, item.string, &registry, &parse_errors);
        }
    }

    const specifier_known = registry.fromSpecifier(specifier) != null;
    const export_known = blk: {
        const exp_name = export_name_opt orelse break :blk false;
        break :blk registry.findExport(specifier, exp_name) != null;
    };

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    const ok = parse_errors.items.len == 0;
    try w.print("{{\"ok\":{s}", .{if (ok) "true" else "false"});
    try w.writeAll(",\"specifier\":");
    try writeJsonString(w, specifier);
    try w.print(",\"specifier_known\":{s}", .{if (specifier_known) "true" else "false"});

    if (export_name_opt) |exp_name| {
        try w.writeAll(",\"export\":");
        try writeJsonString(w, exp_name);
        try w.print(",\"export_known\":{s}", .{if (export_known) "true" else "false"});
    } else {
        try w.writeAll(",\"export\":null,\"export_known\":false");
    }

    try w.writeAll(",\"registered_specifiers\":[");
    for (registry.manifests.items, 0..) |manifest, i| {
        if (i > 0) try w.writeByte(',');
        try writeJsonString(w, manifest.specifier);
    }
    try w.writeAll("],\"parse_errors\":[");
    for (parse_errors.items, 0..) |entry, i| {
        if (i > 0) try w.writeByte(',');
        try w.writeAll("{\"path\":");
        try writeJsonString(w, entry.path);
        try w.writeAll(",\"error\":");
        try writeJsonString(w, entry.err);
        try w.writeByte('}');
    }
    try w.writeAll("]}");

    buf = aw.toArrayList();
    const llm_text = try buf.toOwnedSlice(allocator);
    return .{ .ok = ok and specifier_known, .llm_text = llm_text };
}

fn loadManifestInto(
    allocator: std.mem.Allocator,
    path: []const u8,
    registry: *zigts.manifest_registry.Registry,
    parse_errors: *std.ArrayListUnmanaged(ParseError),
) !void {
    const bytes = zigts.file_io.readFile(allocator, path, 256 * 1024) catch |err| {
        try parse_errors.append(allocator, .{ .path = path, .err = @errorName(err) });
        return;
    };
    defer allocator.free(bytes);

    var manifest = zigts.module_manifest.parse(allocator, bytes) catch |err| {
        try parse_errors.append(allocator, .{ .path = path, .err = @errorName(err) });
        return;
    };
    errdefer manifest.deinit(allocator);

    registry.register(manifest) catch |err| {
        var owned = manifest;
        owned.deinit(allocator);
        try parse_errors.append(allocator, .{ .path = path, .err = @errorName(err) });
        return;
    };
}

const testing = std.testing;

test "missing specifier returns an error body" {
    var result = try execute(testing.allocator, &.{"{}"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "missing required field 'specifier'") != null);
}

test "unregistered specifier reports specifier_known=false" {
    var result = try execute(testing.allocator, &.{"{\"specifier\":\"zigttp-ext:nope\"}"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"specifier_known\":false") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"registered_specifiers\":[]") != null);
}

test "registered specifier with export round-trips" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const manifest_path = try std.fmt.allocPrint(testing.allocator, ".zig-cache/tmp/{s}/m.json", .{tmp.sub_path});
    defer testing.allocator.free(manifest_path);
    const manifest_json =
        \\{
        \\  "schemaVersion": 1,
        \\  "specifier": "zigttp-ext:foo",
        \\  "exports": [{ "name": "bar" }]
        \\}
    ;
    try zigts.file_io.writeFile(testing.allocator, manifest_path, manifest_json);

    const args_json = try std.fmt.allocPrint(
        testing.allocator,
        "{{\"module_manifest_paths\":[\"{s}\"],\"specifier\":\"zigttp-ext:foo\",\"export\":\"bar\"}}",
        .{manifest_path},
    );
    defer testing.allocator.free(args_json);

    var result = try execute(testing.allocator, &.{args_json});
    defer result.deinit(testing.allocator);
    try testing.expect(result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"specifier_known\":true") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"export_known\":true") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"registered_specifiers\":[\"zigttp-ext:foo\"]") != null);
}
