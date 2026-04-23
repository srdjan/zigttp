//! Baked-in prompt template catalog. Each entry is a markdown file alongside
//! this source at `packages/pi/src/prompts/<name>.md` with YAML frontmatter
//! carrying `name` and `description`. The body supports simple positional
//! argument substitution: {{1}}, {{2}}, ... are replaced with the nth arg,
//! and {{args}} joins all remaining args with spaces.
//!
//! Files are embedded via `@embedFile` and parsed at comptime. The lockdown
//! semantics match the skills catalog: no runtime file discovery, adding a
//! template requires dropping a `.md` file, listing it below, and rebuilding.

const std = @import("std");
const frontmatter = @import("../frontmatter.zig");

pub const Template = struct {
    name: []const u8,
    description: []const u8,
    body: []const u8,
};

const embedded_sources = [_][]const u8{
    @embedFile("explain.md"),
    @embedFile("review.md"),
    @embedFile("add-route.md"),
    @embedFile("add-env.md"),
    @embedFile("write-test.md"),
    @embedFile("fix.md"),
};

pub const catalog: [embedded_sources.len]Template = build: {
    var out: [embedded_sources.len]Template = undefined;
    for (embedded_sources, 0..) |src, i| {
        const doc = frontmatter.parseComptime(src);
        // Template body loses trailing whitespace so the final prompt does
        // not carry a dangling newline into the model request.
        out[i] = .{
            .name = doc.require("name"),
            .description = doc.require("description"),
            .body = std.mem.trimEnd(u8, doc.body, " \t\r\n"),
        };
    }
    break :build out;
};

pub fn findByName(name: []const u8) ?*const Template {
    for (&catalog) |*tmpl| {
        if (std.mem.eql(u8, tmpl.name, name)) return tmpl;
    }
    return null;
}

const testing = std.testing;

test "catalog has exactly the expected entries, in order" {
    const expected = [_][]const u8{
        "explain",
        "review",
        "add-route",
        "add-env",
        "write-test",
        "fix",
    };
    try testing.expectEqual(expected.len, catalog.len);
    for (expected, 0..) |name, i| {
        try testing.expectEqualStrings(name, catalog[i].name);
    }
}

test "every template body mentions at least one placeholder" {
    for (&catalog) |*t| {
        const has_placeholder = std.mem.indexOf(u8, t.body, "{{") != null;
        try testing.expect(has_placeholder);
    }
}

test "expand: no placeholders returns body unchanged" {
    const result = try expand(testing.allocator, "hello world", &.{});
    defer testing.allocator.free(result);
    try testing.expectEqualStrings("hello world", result);
}

test "expand: {{1}} substitutes first arg" {
    const args = [_][]const u8{"foo"};
    const result = try expand(testing.allocator, "before {{1}} after", &args);
    defer testing.allocator.free(result);
    try testing.expectEqualStrings("before foo after", result);
}

test "expand: {{2}} substitutes second arg" {
    const args = [_][]const u8{ "a", "b" };
    const result = try expand(testing.allocator, "{{1}} and {{2}}", &args);
    defer testing.allocator.free(result);
    try testing.expectEqualStrings("a and b", result);
}

test "expand: {{N}} beyond args.len produces empty string" {
    const args = [_][]const u8{"only"};
    const result = try expand(testing.allocator, "{{1}} and {{2}}", &args);
    defer testing.allocator.free(result);
    try testing.expectEqualStrings("only and ", result);
}

test "expand: {{args}} with multiple args joins with spaces" {
    const args = [_][]const u8{ "x", "y", "z" };
    const result = try expand(testing.allocator, "do {{args}} now", &args);
    defer testing.allocator.free(result);
    try testing.expectEqualStrings("do x y z now", result);
}

test "expand: {{args}} with zero args produces empty string" {
    const result = try expand(testing.allocator, "do {{args}} now", &.{});
    defer testing.allocator.free(result);
    try testing.expectEqualStrings("do  now", result);
}

test "expand: unclosed {{ is passed through verbatim" {
    const result = try expand(testing.allocator, "open {{ here", &.{});
    defer testing.allocator.free(result);
    try testing.expectEqualStrings("open {{ here", result);
}

test "expand: single { is passed through verbatim" {
    const result = try expand(testing.allocator, "a { b", &.{});
    defer testing.allocator.free(result);
    try testing.expectEqualStrings("a { b", result);
}

test "findByName returns null for unknown template" {
    try testing.expect(findByName("nonexistent") == null);
}

test "findByName returns a pointer for a known template" {
    const tmpl = findByName("explain") orelse return error.TestFailed;
    try testing.expectEqualStrings("explain", tmpl.name);
}

/// Expand `template_body` by substituting {{1}}, {{2}}, ..., {{args}}.
/// `args` are the trailing tokens after the template name.
/// Returns an allocated string the caller must free.
pub fn expand(allocator: std.mem.Allocator, template_body: []const u8, args: []const []const u8) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    var pos: usize = 0;
    while (pos < template_body.len) {
        const open = std.mem.indexOfScalarPos(u8, template_body, pos, '{') orelse {
            try w.writeAll(template_body[pos..]);
            break;
        };
        if (open + 1 >= template_body.len or template_body[open + 1] != '{') {
            try w.writeAll(template_body[pos .. open + 1]);
            pos = open + 1;
            continue;
        }
        const close = std.mem.indexOfPos(u8, template_body, open + 2, "}}") orelse {
            try w.writeAll(template_body[pos..]);
            break;
        };
        try w.writeAll(template_body[pos..open]);
        const placeholder = template_body[open + 2 .. close];
        if (std.mem.eql(u8, placeholder, "args")) {
            for (args, 0..) |arg, i| {
                if (i > 0) try w.writeByte(' ');
                try w.writeAll(arg);
            }
        } else {
            const idx = std.fmt.parseInt(usize, placeholder, 10) catch 0;
            if (idx > 0 and idx <= args.len) {
                try w.writeAll(args[idx - 1]);
            }
        }
        pos = close + 2;
    }

    buf = aw.toArrayList();
    return try buf.toOwnedSlice(allocator);
}
