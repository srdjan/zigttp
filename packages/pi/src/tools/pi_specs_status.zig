//! pi_specs_status - return the active spec set for a handler plus the
//! current discharge state of each active spec. The agent reads this
//! before drafting an edit so its repair plan targets the obligations the
//! compiler is enforcing, not whatever the autoloop's CLI flag happened
//! to be set to.
//!
//! The tool shells out to `zigts check --json` to keep the implementation
//! a single source of truth with the in-process verifier; the JSON output
//! already includes `declared_specs` and `spec_diagnostics`.
//!
//! Input:
//!   { "path": "handler.ts" }
//!
//! Output:
//!   { "ok": bool,
//!     "declared_specs": ["idempotent", ...],
//!     "spec_diagnostics": [
//!       { "code": "ZTS500",
//!         "kind": "not_discharged",
//!         "spec_name": "idempotent",
//!         "suggestion": "remove Date.now() ..." } ] }

const std = @import("std");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");

const name = "pi_specs_status";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "specs-status",
    .effect = .execute_process,
    .description =
    \\Read the handler's active spec set and return each active spec's
    \\current discharge state. A source `Response & Spec<...>` narrows
    \\the set; without one, all supported specs are active. Use this
    \\before drafting a repair plan: the discharge state plus the
    \\per-spec suggestion is the agent's authoritative target list, not
    \\the `--goal` CLI flag.
    \\
    \\Output kinds:
    \\  - "not_discharged"          (ZTS500 - corresponding HandlerProperties
    \\                               field is false; use the suggestion)
    \\  - "incompatible_with_import" (ZTS501 - spec contradicts an imported
    \\                               module; resolve the import or drop the
    \\                               spec, do not enter repair)
    \\  - "unknown_name"            (ZTS502 - spec name not in the v1 set;
    \\                               correct the typo)
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
    const relative = common.relativeToRoot(root, absolute);

    const argv = [_][]const u8{ "zig", "build", "cli", "--", "check", relative, "--json" };
    var outcome = try common.runCommand(allocator, root, &argv);
    defer outcome.deinit(allocator);

    // Parse the JSON and project just the spec section so the agent does
    // not have to parse the whole proof envelope on every invocation.
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, outcome.stdout, .{}) catch |err| {
        if (outcome.exit_code != 0) {
            return try common.commandOutcomeToToolResult(allocator, &argv, &outcome);
        }
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": failed to parse zigts check output: {s}\n",
            .{@errorName(err)},
        );
    };
    defer parsed.deinit();

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &out);
    var w = &aw.writer;

    try w.writeAll("{\"ok\":");
    try w.writeAll(if (outcome.exit_code == 0) "true" else "false");

    const proof = blk: {
        if (parsed.value != .object) break :blk null;
        const proof_value = parsed.value.object.get("proof") orelse break :blk null;
        if (proof_value != .object) break :blk null;
        break :blk proof_value.object;
    };

    try w.writeAll(",\"declared_specs\":");
    if (proof) |p| {
        if (p.get("declared_specs")) |declared_value| {
            try writeJsonValue(w, declared_value);
        } else {
            try w.writeAll("[]");
        }
    } else {
        try w.writeAll("[]");
    }

    try w.writeAll(",\"spec_diagnostics\":");
    if (proof) |p| {
        if (p.get("spec_diagnostics")) |diag_value| {
            try writeJsonValue(w, diag_value);
        } else {
            try w.writeAll("[]");
        }
    } else {
        try w.writeAll("[]");
    }

    try w.writeAll("}\n");
    out = aw.toArrayList();

    const text = try out.toOwnedSlice(allocator);
    defer allocator.free(text);
    return try registry_mod.ToolResult.withPlainText(allocator, outcome.exit_code == 0, text);
}

fn writeJsonValue(w: *std.Io.Writer, value: std.json.Value) !void {
    switch (value) {
        .null => try w.writeAll("null"),
        .bool => |b| try w.writeAll(if (b) "true" else "false"),
        .integer => |i| try w.print("{d}", .{i}),
        .float => |f| try w.print("{d}", .{f}),
        .number_string => |s| try w.writeAll(s),
        .string => |s| try writeJsonString(w, s),
        .array => |arr| {
            try w.writeByte('[');
            for (arr.items, 0..) |item, i| {
                if (i > 0) try w.writeByte(',');
                try writeJsonValue(w, item);
            }
            try w.writeByte(']');
        },
        .object => |obj| {
            try w.writeByte('{');
            var it = obj.iterator();
            var first = true;
            while (it.next()) |entry| {
                if (!first) try w.writeByte(',');
                first = false;
                try writeJsonString(w, entry.key_ptr.*);
                try w.writeByte(':');
                try writeJsonValue(w, entry.value_ptr.*);
            }
            try w.writeByte('}');
        },
    }
}

fn writeJsonString(w: *std.Io.Writer, s: []const u8) !void {
    try w.writeByte('"');
    for (s) |c| {
        switch (c) {
            '\\' => try w.writeAll("\\\\"),
            '"' => try w.writeAll("\\\""),
            '\n' => try w.writeAll("\\n"),
            '\r' => try w.writeAll("\\r"),
            '\t' => try w.writeAll("\\t"),
            else => {
                if (c < 0x20) {
                    try w.print("\\u{x:0>4}", .{c});
                } else {
                    try w.writeByte(c);
                }
            },
        }
    }
    try w.writeByte('"');
}

const testing = std.testing;

test "tool registers expected name and label" {
    try testing.expectEqualStrings("pi_specs_status", tool.name);
    try testing.expectEqualStrings("specs-status", tool.label);
    try testing.expect(std.mem.indexOf(u8, tool.description, "Spec<...>") != null);
    try testing.expect(std.mem.indexOf(u8, tool.description, "ZTS500") != null);
}
