//! Baked-in prompt template catalog. Templates support simple positional
//! argument substitution: {{1}}, {{2}}, ... are replaced with the nth argument,
//! and {{args}} is replaced with all remaining arguments joined by spaces.
//! No external files are read; content lives in this source file.

const std = @import("std");

pub const Template = struct {
    name: []const u8,
    description: []const u8,
    body: []const u8,
};

pub const catalog = [_]Template{
    .{
        .name = "explain",
        .description = "Explain a concept or code element. Usage: /template:explain <topic>",
        .body = "Explain {{args}} in the context of zigts/zigttp. Be concise and concrete. Include a minimal code example if relevant.",
    },
    .{
        .name = "review",
        .description = "Review a specific file for violations and quality. Usage: /template:review <file>",
        .body = "Review the file {{1}} for correctness, compiler compliance, and code quality. List any ZTS violations, missing error handling, or style issues. Suggest concrete fixes for each.",
    },
    .{
        .name = "add-route",
        .description = "Add a new route to the handler. Usage: /template:add-route <METHOD> <path>",
        .body = "Add a {{1}} {{2}} route to the current handler. Follow the existing patterns for request parsing and response formatting. Run /check after the edit to verify no violations were introduced.",
    },
    .{
        .name = "add-env",
        .description = "Add an env var to the contract and handler. Usage: /template:add-env <VAR_NAME>",
        .body = "Add the environment variable {{1}} to the handler. Import it via `zigttp:env`, add it to the env contract section, and use it where needed. Run /check after the edit.",
    },
    .{
        .name = "write-test",
        .description = "Write a test case for a handler path. Usage: /template:write-test <description>",
        .body = "Write a test case for: {{args}}. Use the JSONL test format with `request` and `expected` fields. Match the existing test file format in the workspace.",
    },
    .{
        .name = "fix",
        .description = "Fix a specific violation or error. Usage: /template:fix <error>",
        .body = "Fix the following issue in the handler: {{args}}. Explain the root cause, show the corrected code, and verify with /check after applying the fix.",
    },
};

pub fn findByName(name: []const u8) ?*const Template {
    for (&catalog) |*tmpl| {
        if (std.mem.eql(u8, tmpl.name, name)) return tmpl;
    }
    return null;
}

const testing = std.testing;

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
