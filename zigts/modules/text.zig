//! zigttp:text - String escaping and transformation
//!
//! Exports:
//!   escapeHtml(str: string) -> string
//!     Escapes &, <, >, ", ' for safe HTML embedding.
//!
//!   unescapeHtml(str: string) -> string
//!     Reverses HTML entity escaping (&amp; &lt; &gt; &quot; &#39;).
//!
//!   slugify(str: string) -> string
//!     Converts to URL-safe slug: lowercase, hyphens, strip non-alphanumeric.
//!
//!   truncate(str: string, len: number, suffix?: string) -> string
//!     Truncates to len chars with suffix (default "..."). No-op if already short enough.
//!
//!   mask(str: string, visible?: number) -> string
//!     Masks all but last N chars (default 4) with '*'. Returns all '*' if too short.

const std = @import("std");
const context = @import("../context.zig");
const value = @import("../value.zig");
const util = @import("util.zig");
const mb = @import("../module_binding.zig");

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:text",
    .name = "text",
    .exports = &.{
        .{ .name = "escapeHtml", .func = escapeHtmlNative, .arg_count = 1,
           .returns = .string, .param_types = &.{.string},
           .return_labels = .{ .validated = true } },
        .{ .name = "unescapeHtml", .func = unescapeHtmlNative, .arg_count = 1,
           .returns = .string, .param_types = &.{.string} },
        .{ .name = "slugify", .func = slugifyNative, .arg_count = 1,
           .returns = .string, .param_types = &.{.string} },
        .{ .name = "truncate", .func = truncateNative, .arg_count = 3,
           .returns = .string, .param_types = &.{ .string, .number, .string } },
        .{ .name = "mask", .func = maskNative, .arg_count = 2,
           .returns = .string, .param_types = &.{ .string, .number },
           .return_labels = .{ .internal = true } },
    },
};

pub const exports = binding.toModuleExports();

// -------------------------------------------------------------------------
// escapeHtml
// -------------------------------------------------------------------------

fn escapeHtmlNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    if (args.len == 0) return value.JSValue.undefined_val;
    const input = util.extractString(args[0]) orelse return value.JSValue.undefined_val;

    // Fast path: no escapable chars
    if (std.mem.indexOfAny(u8, input, "&<>\"'") == null) {
        return ctx.createString(input) catch value.JSValue.undefined_val;
    }

    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(ctx.allocator);

    for (input) |c| {
        switch (c) {
            '&' => buf.appendSlice(ctx.allocator, "&amp;") catch return value.JSValue.undefined_val,
            '<' => buf.appendSlice(ctx.allocator, "&lt;") catch return value.JSValue.undefined_val,
            '>' => buf.appendSlice(ctx.allocator, "&gt;") catch return value.JSValue.undefined_val,
            '"' => buf.appendSlice(ctx.allocator, "&quot;") catch return value.JSValue.undefined_val,
            '\'' => buf.appendSlice(ctx.allocator, "&#39;") catch return value.JSValue.undefined_val,
            else => buf.append(ctx.allocator, c) catch return value.JSValue.undefined_val,
        }
    }

    return ctx.createString(buf.items) catch value.JSValue.undefined_val;
}

// -------------------------------------------------------------------------
// unescapeHtml
// -------------------------------------------------------------------------

fn unescapeHtmlNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    if (args.len == 0) return value.JSValue.undefined_val;
    const input = util.extractString(args[0]) orelse return value.JSValue.undefined_val;

    if (std.mem.indexOfScalar(u8, input, '&') == null) {
        return ctx.createString(input) catch value.JSValue.undefined_val;
    }

    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(ctx.allocator);

    var i: usize = 0;
    while (i < input.len) {
        if (input[i] == '&') {
            if (matchEntity(input[i..])) |entity| {
                buf.append(ctx.allocator, entity.char) catch return value.JSValue.undefined_val;
                i += entity.len;
                continue;
            }
        }
        buf.append(ctx.allocator, input[i]) catch return value.JSValue.undefined_val;
        i += 1;
    }

    return ctx.createString(buf.items) catch value.JSValue.undefined_val;
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

// -------------------------------------------------------------------------
// slugify
// -------------------------------------------------------------------------

fn slugifyNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    if (args.len == 0) return value.JSValue.undefined_val;
    const input = util.extractString(args[0]) orelse return value.JSValue.undefined_val;

    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(ctx.allocator);

    var prev_hyphen = true; // Prevent leading hyphen
    for (input) |c| {
        if (std.ascii.isAlphanumeric(c)) {
            buf.append(ctx.allocator, std.ascii.toLower(c)) catch return value.JSValue.undefined_val;
            prev_hyphen = false;
        } else if (!prev_hyphen) {
            buf.append(ctx.allocator, '-') catch return value.JSValue.undefined_val;
            prev_hyphen = true;
        }
    }

    // Strip trailing hyphen
    const result = if (buf.items.len > 0 and buf.items[buf.items.len - 1] == '-')
        buf.items[0 .. buf.items.len - 1]
    else
        buf.items;

    return ctx.createString(result) catch value.JSValue.undefined_val;
}

// -------------------------------------------------------------------------
// truncate
// -------------------------------------------------------------------------

fn truncateNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    if (args.len < 2) return value.JSValue.undefined_val;
    const input = util.extractString(args[0]) orelse return value.JSValue.undefined_val;
    const max_len_i = util.extractInt(args[1]) orelse return value.JSValue.undefined_val;
    if (max_len_i < 0) return value.JSValue.undefined_val;
    const max_len: usize = @intCast(max_len_i);

    const suffix = if (args.len > 2) util.extractString(args[2]) orelse "..." else "...";

    if (input.len <= max_len) {
        return ctx.createString(input) catch value.JSValue.undefined_val;
    }

    const cut = if (max_len > suffix.len) max_len - suffix.len else 0;

    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(ctx.allocator);
    buf.appendSlice(ctx.allocator, input[0..cut]) catch return value.JSValue.undefined_val;
    buf.appendSlice(ctx.allocator, suffix) catch return value.JSValue.undefined_val;

    return ctx.createString(buf.items) catch value.JSValue.undefined_val;
}

// -------------------------------------------------------------------------
// mask
// -------------------------------------------------------------------------

fn maskNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    if (args.len == 0) return value.JSValue.undefined_val;
    const input = util.extractString(args[0]) orelse return value.JSValue.undefined_val;

    const visible: usize = if (args.len > 1) blk: {
        const n = util.extractInt(args[1]) orelse break :blk 4;
        if (n < 0) break :blk 4;
        break :blk @intCast(n);
    } else 4;

    const buf = ctx.allocator.alloc(u8, input.len) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(buf);

    const mask_count = if (input.len > visible) input.len - visible else input.len;
    @memset(buf[0..mask_count], '*');
    if (input.len > visible) {
        @memcpy(buf[mask_count..], input[mask_count..]);
    }

    return ctx.createString(buf) catch value.JSValue.undefined_val;
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

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
