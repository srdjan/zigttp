//! zigttp:decode - typed request ingress helpers
//!
//! Exports:
//!   decodeJson(name: string, body: string) -> { ok: true, value } | { ok: false, errors: [...] }
//!   decodeForm(name: string, body: string) -> { ok: true, value } | { ok: false, errors: [...] }
//!   decodeQuery(name: string, query: object) -> { ok: true, value } | { ok: false, errors: [...] }

const std = @import("std");
const context = @import("../context.zig");
const object = @import("../object.zig");
const value = @import("../value.zig");
const mb = @import("../module_binding.zig");
const util = @import("util.zig");
const validate = @import("validate.zig");

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:decode",
    .name = "decode",
    .stateful = false,
    .exports = &.{
        .{
            .name = "decodeJson",
            .func = decodeJsonNative,
            .arg_count = 2,
            .returns = .result,
            .param_types = &.{ .string, .string },
            .failure_severity = .critical,
            .contract_extractions = &.{.{ .category = .request_schema }},
            .return_labels = .{ .validated = true },
        },
        .{
            .name = "decodeForm",
            .func = decodeFormNative,
            .arg_count = 2,
            .returns = .result,
            .param_types = &.{ .string, .string },
            .failure_severity = .critical,
            .contract_extractions = &.{.{ .category = .request_schema }},
            .return_labels = .{ .validated = true },
        },
        .{
            .name = "decodeQuery",
            .func = decodeQueryNative,
            .arg_count = 2,
            .returns = .result,
            .param_types = &.{ .string, .object },
            .failure_severity = .critical,
            .contract_extractions = &.{.{ .category = .request_schema }},
            .return_labels = .{ .validated = true },
        },
    },
};

pub const exports = binding.toModuleExports();

fn decodeJsonNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    if (args.len < 2) return util.createPlainResultErr(ctx, "missing arguments");
    const name = util.extractString(args[0]) orelse return util.createPlainResultErr(ctx, "name must be a string");
    const body = util.extractString(args[1]) orelse return util.createPlainResultErr(ctx, "body must be a string");
    return validate.decodeJson(ctx, name, body, false);
}

fn decodeFormNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    if (args.len < 2) return util.createPlainResultErr(ctx, "missing arguments");
    const name = util.extractString(args[0]) orelse return util.createPlainResultErr(ctx, "name must be a string");
    const body = util.extractString(args[1]) orelse return util.createPlainResultErr(ctx, "body must be a string");
    const obj = try parseFormObject(ctx, body);
    return validate.decodeObject(ctx, name, obj.toValue(), true);
}

fn decodeQueryNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    if (args.len < 2) return util.createPlainResultErr(ctx, "missing arguments");
    const name = util.extractString(args[0]) orelse return util.createPlainResultErr(ctx, "name must be a string");
    if (!args[1].isObject()) return util.createPlainResultErr(ctx, "query must be an object");
    return validate.decodeObject(ctx, name, args[1], true);
}

fn parseFormObject(ctx: *context.Context, raw: []const u8) !*object.JSObject {
    const obj = try ctx.createObject(null);
    var parts = std.mem.splitScalar(u8, raw, '&');
    while (parts.next()) |part| {
        if (part.len == 0) continue;

        const eq_idx = std.mem.indexOfScalar(u8, part, '=') orelse part.len;
        const key_raw = part[0..eq_idx];
        if (key_raw.len == 0) continue;
        const val_raw = if (eq_idx < part.len) part[eq_idx + 1 ..] else "";

        const key = try decodeFormComponent(ctx.allocator, key_raw);
        defer ctx.allocator.free(key);
        const val = try decodeFormComponent(ctx.allocator, val_raw);
        defer ctx.allocator.free(val);

        const atom = try ctx.atoms.intern(key);
        const js_val = try ctx.createString(val);
        try ctx.setPropertyChecked(obj, atom, js_val);
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
