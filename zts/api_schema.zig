//! JSON Schema emission for proven API types.
//!
//! Converts TypePool types into canonical JSON Schema fragments when the type
//! is representable without guessing. Unrepresentable types return null so
//! callers can mark the contract as dynamic.

const std = @import("std");
const type_pool_mod = @import("type_pool.zig");
const type_env_mod = @import("type_env.zig");
const handler_contract = @import("handler_contract.zig");

const TypePool = type_pool_mod.TypePool;
const TypeIndex = type_pool_mod.TypeIndex;
const TypeTag = type_pool_mod.TypeTag;
const TypeEnv = type_env_mod.TypeEnv;

const SeenSet = std.AutoHashMapUnmanaged(TypeIndex, void);

pub fn schemaFromType(
    allocator: std.mem.Allocator,
    env: *const TypeEnv,
    type_idx: TypeIndex,
) !?[]u8 {
    var output: std.ArrayList(u8) = .empty;
    errdefer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    var seen: SeenSet = .empty;
    defer seen.deinit(allocator);

    const ok = try writeSchema(&aw.writer, allocator, env, type_idx, &seen);
    if (!ok) {
        output.deinit(allocator);
        return null;
    }

    output = aw.toArrayList();
    return try output.toOwnedSlice(allocator);
}

fn writeSchema(
    writer: anytype,
    allocator: std.mem.Allocator,
    env: *const TypeEnv,
    type_idx: TypeIndex,
    seen: *SeenSet,
) anyerror!bool {
    const pool = env.pool;
    const resolved = resolveAlias(env, type_idx) orelse type_idx;
    const tag = pool.getTag(resolved) orelse return false;

    switch (tag) {
        .t_boolean => {
            try writer.writeAll("{\"type\":\"boolean\"}");
            return true;
        },
        .t_number => {
            try writer.writeAll("{\"type\":\"number\"}");
            return true;
        },
        .t_string => {
            try writer.writeAll("{\"type\":\"string\"}");
            return true;
        },
        .t_literal_string => {
            const literal = pool.getRefName(resolved);
            try writer.writeAll("{\"type\":\"string\",\"const\":");
            try writeJsonString(writer, literal);
            try writer.writeByte('}');
            return true;
        },
        .t_literal_number => {
            const data = pool.getData(resolved) orelse return false;
            try writer.writeAll("{\"const\":");
            try writer.print("{d}", .{data.a});
            try writer.writeByte('}');
            return true;
        },
        .t_literal_bool => {
            const data = pool.getData(resolved) orelse return false;
            try writer.writeAll("{\"const\":");
            try writer.writeAll(if (data.a != 0) "true" else "false");
            try writer.writeByte('}');
            return true;
        },
        .t_record => return writeRecordSchema(writer, allocator, env, resolved, seen),
        .t_array => return writeArraySchema(writer, allocator, env, resolved, seen),
        .t_tuple => return writeTupleSchema(writer, allocator, env, resolved, seen),
        .t_union => return writeLiteralUnionSchema(writer, allocator, env, resolved, seen),
        .t_ref => {
            if (seen.get(resolved) != null) return false;
            try seen.put(allocator, resolved, {});
            defer _ = seen.remove(resolved);

            const alias = resolveAlias(env, resolved) orelse return false;
            return writeSchema(writer, allocator, env, alias, seen);
        },
        else => return false,
    }
}

fn writeRecordSchema(
    writer: anytype,
    allocator: std.mem.Allocator,
    env: *const TypeEnv,
    type_idx: TypeIndex,
    seen: *SeenSet,
) anyerror!bool {
    const pool = env.pool;
    const fields = pool.getRecordFields(type_idx);

    try writer.writeAll("{\"type\":\"object\"");
    if (fields.len > 0) {
        try writer.writeAll(",\"properties\":{");
        for (fields, 0..) |field, i| {
            if (i > 0) try writer.writeByte(',');
            try writeJsonString(writer, pool.getName(field.name_start, field.name_len));
            try writer.writeByte(':');
            if (!try writeSchema(writer, allocator, env, field.type_idx, seen)) return false;
        }
        try writer.writeByte('}');

        var required_count: usize = 0;
        for (fields) |field| {
            if (!field.optional) required_count += 1;
        }

        if (required_count > 0) {
            try writer.writeAll(",\"required\":[");
            var wrote_required = false;
            for (fields) |field| {
                if (field.optional) continue;
                if (wrote_required) try writer.writeByte(',');
                wrote_required = true;
                try writeJsonString(writer, pool.getName(field.name_start, field.name_len));
            }
            try writer.writeByte(']');
        }
    }
    try writer.writeByte('}');
    return true;
}

fn writeArraySchema(
    writer: anytype,
    allocator: std.mem.Allocator,
    env: *const TypeEnv,
    type_idx: TypeIndex,
    seen: *SeenSet,
) anyerror!bool {
    const pool = env.pool;
    const element = pool.getArrayElement(type_idx);
    if (element == type_pool_mod.null_type_idx) return false;

    try writer.writeAll("{\"type\":\"array\",\"items\":");
    if (!try writeSchema(writer, allocator, env, element, seen)) return false;
    try writer.writeByte('}');
    return true;
}

fn writeTupleSchema(
    writer: anytype,
    allocator: std.mem.Allocator,
    env: *const TypeEnv,
    type_idx: TypeIndex,
    seen: *SeenSet,
) anyerror!bool {
    const items = getTupleMembers(env.pool, type_idx);
    if (items.len == 0) return false;

    try writer.writeAll("{\"type\":\"array\",\"prefixItems\":[");
    for (items, 0..) |item, i| {
        if (i > 0) try writer.writeByte(',');
        if (!try writeSchema(writer, allocator, env, item, seen)) return false;
    }
    try writer.writeAll("],\"items\":false,\"minItems\":");
    try writer.print("{d}", .{items.len});
    try writer.writeAll(",\"maxItems\":");
    try writer.print("{d}", .{items.len});
    try writer.writeByte('}');
    return true;
}

fn writeLiteralUnionSchema(
    writer: anytype,
    allocator: std.mem.Allocator,
    env: *const TypeEnv,
    type_idx: TypeIndex,
    seen: *SeenSet,
) anyerror!bool {
    _ = allocator;
    _ = seen;
    const pool = env.pool;
    const members = pool.getUnionMembers(type_idx);
    if (members.len == 0) return false;

    const first_tag = pool.getTag(resolveAlias(env, members[0]) orelse members[0]) orelse return false;
    const kind = switch (first_tag) {
        .t_literal_string => "string",
        .t_literal_number => "number",
        .t_literal_bool => "boolean",
        else => return false,
    };

    for (members[1..]) |member| {
        const tag = pool.getTag(resolveAlias(env, member) orelse member) orelse return false;
        switch (first_tag) {
            .t_literal_string => if (tag != .t_literal_string) return false,
            .t_literal_number => if (tag != .t_literal_number) return false,
            .t_literal_bool => if (tag != .t_literal_bool) return false,
            else => return false,
        }
    }

    try writer.writeAll("{\"type\":\"");
    try writer.writeAll(kind);
    try writer.writeAll("\",\"enum\":[");
    for (members, 0..) |member, i| {
        if (i > 0) try writer.writeByte(',');
        const resolved = resolveAlias(env, member) orelse member;
        switch (pool.getTag(resolved).?) {
            .t_literal_string => try writeJsonString(writer, pool.getRefName(resolved)),
            .t_literal_number => {
                const data = pool.getData(resolved) orelse return false;
                try writer.print("{d}", .{data.a});
            },
            .t_literal_bool => {
                const data = pool.getData(resolved) orelse return false;
                try writer.writeAll(if (data.a != 0) "true" else "false");
            },
            else => return false,
        }
    }
    try writer.writeAll("]}");
    return true;
}

fn resolveAlias(env: *const TypeEnv, type_idx: TypeIndex) ?TypeIndex {
    const pool = env.pool;
    if (pool.getTag(type_idx) != .t_ref) return null;
    const name = pool.getRefName(type_idx);
    return env.getTypeAlias(name) orelse env.getInterface(name);
}

fn getTupleMembers(pool: *const TypePool, type_idx: TypeIndex) []const TypeIndex {
    const data = pool.getData(type_idx) orelse return &.{};
    if (pool.getTag(type_idx) != .t_tuple) return &.{};
    const start = data.a;
    const count = data.b;
    if (start + count > pool.members.items.len) return &.{};
    return pool.members.items[start .. start + count];
}

const writeJsonString = handler_contract.writeJsonString;

test "schemaFromType renders record with required field" {
    const allocator = std.testing.allocator;

    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const id_name = pool.addName(allocator, "id");
    const ok_name = pool.addName(allocator, "ok");
    const rec = pool.addRecord(allocator, &.{
        .{ .name_start = id_name.start, .name_len = id_name.len, .type_idx = pool.idx_string, .optional = false },
        .{ .name_start = ok_name.start, .name_len = ok_name.len, .type_idx = pool.idx_boolean, .optional = true },
    });

    const schema = (try schemaFromType(allocator, &env, rec)).?;
    defer allocator.free(schema);

    try std.testing.expect(std.mem.indexOf(u8, schema, "\"type\":\"object\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, schema, "\"id\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, schema, "\"required\":[\"id\"]") != null);
}

test "schemaFromType resolves alias" {
    const allocator = std.testing.allocator;

    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const type_idx = pool.addRecord(allocator, &.{});
    const alias_name = env.internName("User");
    try env.type_aliases.put(allocator, alias_name, type_idx);

    const ref_idx = pool.addRef(allocator, "User");
    const schema = (try schemaFromType(allocator, &env, ref_idx)).?;
    defer allocator.free(schema);

    try std.testing.expectEqualStrings("{\"type\":\"object\"}", schema);
}

test "schemaFromType rejects nullable" {
    const allocator = std.testing.allocator;

    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const nullable = pool.addNullable(allocator, pool.idx_string);
    try std.testing.expect((try schemaFromType(allocator, &env, nullable)) == null);
}
