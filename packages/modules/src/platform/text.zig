//! zigttp:text - String escaping and transformation

const std = @import("std");
const sdk = @import("zigttp-sdk");

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:text",
    .name = "text",
    .exports = &.{
        .{ .name = "escapeHtml", .module_func = escapeHtmlImpl, .arg_count = 1, .returns = .string, .param_types = &.{.string}, .return_labels = .{ .validated = true }, .laws = &.{.pure} },
        .{ .name = "unescapeHtml", .module_func = unescapeHtmlImpl, .arg_count = 1, .returns = .string, .param_types = &.{.string}, .laws = &.{.pure} },
        .{ .name = "slugify", .module_func = slugifyImpl, .arg_count = 1, .returns = .string, .param_types = &.{.string}, .laws = &.{.pure} },
        .{ .name = "truncate", .module_func = truncateImpl, .arg_count = 3, .returns = .string, .param_types = &.{ .string, .number, .string }, .laws = &.{.pure} },
        .{ .name = "mask", .module_func = maskImpl, .arg_count = 2, .returns = .string, .param_types = &.{ .string, .number }, .return_labels = .{ .internal = true }, .laws = &.{.pure} },
    },
};

fn escapeHtmlImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const input = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    if (std.mem.indexOfAny(u8, input, "&<>\"'") == null) {
        return sdk.createString(handle, input) catch sdk.JSValue.undefined_val;
    }

    const allocator = sdk.getAllocator(handle);
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(allocator);

    for (input) |c| switch (c) {
        '&' => buf.appendSlice(allocator, "&amp;") catch return sdk.JSValue.undefined_val,
        '<' => buf.appendSlice(allocator, "&lt;") catch return sdk.JSValue.undefined_val,
        '>' => buf.appendSlice(allocator, "&gt;") catch return sdk.JSValue.undefined_val,
        '"' => buf.appendSlice(allocator, "&quot;") catch return sdk.JSValue.undefined_val,
        '\'' => buf.appendSlice(allocator, "&#39;") catch return sdk.JSValue.undefined_val,
        else => buf.append(allocator, c) catch return sdk.JSValue.undefined_val,
    };

    return sdk.createString(handle, buf.items) catch sdk.JSValue.undefined_val;
}

fn unescapeHtmlImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const input = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    if (std.mem.indexOfScalar(u8, input, '&') == null) {
        return sdk.createString(handle, input) catch sdk.JSValue.undefined_val;
    }

    const allocator = sdk.getAllocator(handle);
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(allocator);

    var i: usize = 0;
    while (i < input.len) {
        if (input[i] == '&') {
            if (matchEntity(input[i..])) |entity| {
                buf.append(allocator, entity.char) catch return sdk.JSValue.undefined_val;
                i += entity.len;
                continue;
            }
        }
        buf.append(allocator, input[i]) catch return sdk.JSValue.undefined_val;
        i += 1;
    }

    return sdk.createString(handle, buf.items) catch sdk.JSValue.undefined_val;
}

const Entity = struct { char: u8, len: usize };

fn matchEntity(s: []const u8) ?Entity {
    if (s.len >= 5 and std.mem.startsWith(u8, s, "&amp;")) return .{ .char = '&', .len = 5 };
    if (s.len >= 4 and std.mem.startsWith(u8, s, "&lt;")) return .{ .char = '<', .len = 4 };
    if (s.len >= 4 and std.mem.startsWith(u8, s, "&gt;")) return .{ .char = '>', .len = 4 };
    if (s.len >= 6 and std.mem.startsWith(u8, s, "&quot;")) return .{ .char = '"', .len = 6 };
    if (s.len >= 5 and std.mem.startsWith(u8, s, "&#39;")) return .{ .char = '\'', .len = 5 };
    return null;
}

fn slugifyImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const input = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    const allocator = sdk.getAllocator(handle);
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(allocator);

    var prev_hyphen = true;
    for (input) |c| {
        if (std.ascii.isAlphanumeric(c)) {
            buf.append(allocator, std.ascii.toLower(c)) catch return sdk.JSValue.undefined_val;
            prev_hyphen = false;
        } else if (!prev_hyphen) {
            buf.append(allocator, '-') catch return sdk.JSValue.undefined_val;
            prev_hyphen = true;
        }
    }

    const result = if (buf.items.len > 0 and buf.items[buf.items.len - 1] == '-')
        buf.items[0 .. buf.items.len - 1]
    else
        buf.items;

    return sdk.createString(handle, result) catch sdk.JSValue.undefined_val;
}

fn truncateImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.JSValue.undefined_val;
    const input = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;
    const max_len_i = sdk.extractInt(args[1]) orelse return sdk.JSValue.undefined_val;
    if (max_len_i < 0) return sdk.JSValue.undefined_val;
    const max_len: usize = @intCast(max_len_i);

    const suffix = if (args.len > 2) sdk.extractString(args[2]) orelse "..." else "...";

    if (input.len <= max_len) {
        return sdk.createString(handle, input) catch sdk.JSValue.undefined_val;
    }

    const cut = if (max_len > suffix.len) max_len - suffix.len else 0;
    const allocator = sdk.getAllocator(handle);

    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(allocator);
    buf.appendSlice(allocator, input[0..cut]) catch return sdk.JSValue.undefined_val;
    buf.appendSlice(allocator, suffix) catch return sdk.JSValue.undefined_val;

    return sdk.createString(handle, buf.items) catch sdk.JSValue.undefined_val;
}

fn maskImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const input = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    const visible: usize = if (args.len > 1) blk: {
        const n = sdk.extractInt(args[1]) orelse break :blk 4;
        if (n < 0) break :blk 4;
        break :blk @intCast(n);
    } else 4;

    const allocator = sdk.getAllocator(handle);
    const buf = allocator.alloc(u8, input.len) catch return sdk.JSValue.undefined_val;
    defer allocator.free(buf);

    const mask_count = if (input.len > visible) input.len - visible else input.len;
    @memset(buf[0..mask_count], '*');
    if (input.len > visible) {
        @memcpy(buf[mask_count..], input[mask_count..]);
    }

    return sdk.createString(handle, buf) catch sdk.JSValue.undefined_val;
}

test "matchEntity: known entities" {
    try std.testing.expectEqual(@as(u8, '&'), matchEntity("&amp;extra").?.char);
    try std.testing.expectEqual(@as(usize, 5), matchEntity("&amp;extra").?.len);
    try std.testing.expectEqual(@as(u8, '<'), matchEntity("&lt;").?.char);
    try std.testing.expectEqual(@as(u8, '>'), matchEntity("&gt;").?.char);
    try std.testing.expectEqual(@as(u8, '"'), matchEntity("&quot;").?.char);
    try std.testing.expectEqual(@as(u8, '\''), matchEntity("&#39;").?.char);
    try std.testing.expect(matchEntity("&unknown;") == null);
    try std.testing.expect(matchEntity("hello") == null);
}
