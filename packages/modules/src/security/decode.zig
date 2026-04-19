//! zigttp:decode - typed request ingress helpers

const std = @import("std");
const sdk = @import("zigttp-sdk");
const validate = @import("validate.zig");

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:decode",
    .name = "decode",
    .stateful = false,
    .exports = &.{
        .{
            .name = "decodeJson",
            .module_func = decodeJsonImpl,
            .arg_count = 2,
            .returns = .result,
            .param_types = &.{ .string, .string },
            .failure_severity = .critical,
            .contract_extractions = &.{.{ .category = .request_schema }},
            .return_labels = .{ .validated = true },
        },
        .{
            .name = "decodeForm",
            .module_func = decodeFormImpl,
            .arg_count = 2,
            .returns = .result,
            .param_types = &.{ .string, .string },
            .failure_severity = .critical,
            .contract_extractions = &.{.{ .category = .request_schema }},
            .return_labels = .{ .validated = true },
        },
        .{
            .name = "decodeQuery",
            .module_func = decodeQueryImpl,
            .arg_count = 2,
            .returns = .result,
            .param_types = &.{ .string, .object },
            .failure_severity = .critical,
            .contract_extractions = &.{.{ .category = .request_schema }},
            .return_labels = .{ .validated = true },
        },
    },
};

fn decodeJsonImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.resultErr(handle, "missing arguments");
    const name = sdk.extractString(args[0]) orelse return sdk.resultErr(handle, "name must be a string");
    const body = sdk.extractString(args[1]) orelse return sdk.resultErr(handle, "body must be a string");
    return validate.decodeJson(handle, name, body, false);
}

fn decodeFormImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.resultErr(handle, "missing arguments");
    const name = sdk.extractString(args[0]) orelse return sdk.resultErr(handle, "name must be a string");
    const body = sdk.extractString(args[1]) orelse return sdk.resultErr(handle, "body must be a string");
    const obj = try parseFormObject(handle, body);
    return validate.decodeObject(handle, name, obj, true);
}

fn decodeQueryImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.resultErr(handle, "missing arguments");
    const name = sdk.extractString(args[0]) orelse return sdk.resultErr(handle, "name must be a string");
    if (!sdk.isObject(args[1])) return sdk.resultErr(handle, "query must be an object");
    return validate.decodeObject(handle, name, args[1], true);
}

fn parseFormObject(handle: *sdk.ModuleHandle, raw: []const u8) !sdk.JSValue {
    const allocator = sdk.getAllocator(handle);
    const obj = try sdk.createObject(handle);
    var parts = std.mem.splitScalar(u8, raw, '&');
    while (parts.next()) |part| {
        if (part.len == 0) continue;

        const eq_idx = std.mem.indexOfScalar(u8, part, '=') orelse part.len;
        const key_raw = part[0..eq_idx];
        if (key_raw.len == 0) continue;
        const val_raw = if (eq_idx < part.len) part[eq_idx + 1 ..] else "";

        const key = try decodeFormComponent(allocator, key_raw);
        defer allocator.free(key);
        const val = try decodeFormComponent(allocator, val_raw);
        defer allocator.free(val);

        const js_val = try sdk.createString(handle, val);
        try sdk.objectSet(handle, obj, key, js_val);
    }
    return obj;
}

fn decodeFormComponent(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    try out.ensureTotalCapacity(allocator, input.len);

    var i: usize = 0;
    while (i < input.len) : (i += 1) {
        const c = input[i];
        if (c == '+') {
            out.appendAssumeCapacity(' ');
            continue;
        }
        if (c == '%' and i + 2 < input.len) {
            const hi = fromHex(input[i + 1]);
            const lo = fromHex(input[i + 2]);
            if (hi != null and lo != null) {
                out.appendAssumeCapacity((hi.? << 4) | lo.?);
                i += 2;
                continue;
            }
        }
        out.appendAssumeCapacity(c);
    }

    return out.toOwnedSlice(allocator);
}

fn fromHex(c: u8) ?u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'a'...'f' => c - 'a' + 10,
        'A'...'F' => c - 'A' + 10,
        else => null,
    };
}

test "decodeFormComponent decodes plus and percent escapes" {
    const allocator = std.testing.allocator;
    const decoded = try decodeFormComponent(allocator, "display+name=zig%20ttp");
    defer allocator.free(decoded);
    try std.testing.expectEqualStrings("display name=zig ttp", decoded);
}
