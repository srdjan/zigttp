//! Baked-in skill catalog. Each entry is a markdown file alongside this
//! source at `packages/pi/src/skills/<name>.md` with YAML frontmatter
//! carrying `name` and `description`. Files are embedded at compile time
//! via `@embedFile` and frontmatter is parsed by the shared `frontmatter`
//! module. Skills cannot be added, replaced, or silently altered at runtime:
//! adding one requires dropping a new `.md` file, listing it in
//! `embedded_sources` below, and rebuilding.
//!
//! The `.md` files live under `src/skills/` (not a sibling `skills/` dir)
//! because `@embedFile` is restricted to the importing module's package
//! tree. This keeps the files inside `pi_app` without extending the module
//! system.

const std = @import("std");
const frontmatter = @import("../frontmatter.zig");

pub const Skill = struct {
    name: []const u8,
    description: []const u8,
    body: []const u8,
};

const embedded_sources = [_][]const u8{
    @embedFile("handler-scaffold.md"),
    @embedFile("fix-violations.md"),
    @embedFile("route-table.md"),
    @embedFile("auth-jwt.md"),
    @embedFile("sql-query.md"),
};

pub const catalog: [embedded_sources.len]Skill = build: {
    var out: [embedded_sources.len]Skill = undefined;
    for (embedded_sources, 0..) |src, i| {
        const doc = frontmatter.parseComptime(src);
        out[i] = .{
            .name = doc.require("name"),
            .description = doc.require("description"),
            .body = std.mem.trimEnd(u8, doc.body, " \t\r\n"),
        };
    }
    break :build out;
};

pub fn findByName(name: []const u8) ?*const Skill {
    for (&catalog) |*skill| {
        if (std.mem.eql(u8, skill.name, name)) return skill;
    }
    return null;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "catalog has exactly the expected entries, in order" {
    const expected = [_][]const u8{
        "handler-scaffold",
        "fix-violations",
        "route-table",
        "auth-jwt",
        "sql-query",
    };
    try testing.expectEqual(expected.len, catalog.len);
    for (expected, 0..) |name, i| {
        try testing.expectEqualStrings(name, catalog[i].name);
    }
}

test "findByName returns a hit for every entry and null for unknowns" {
    for (&catalog) |*skill| {
        const found = findByName(skill.name) orelse return error.TestExpected;
        try testing.expectEqualStrings(skill.name, found.name);
    }
    try testing.expect(findByName("does-not-exist") == null);
}

test "every skill body is non-empty and every description fits on one line" {
    for (&catalog) |*skill| {
        try testing.expect(skill.body.len > 0);
        try testing.expect(std.mem.indexOfScalar(u8, skill.description, '\n') == null);
    }
}

test "handler-scaffold body preserves the conventions list verbatim" {
    const skill = findByName("handler-scaffold") orelse return error.TestExpected;
    try testing.expect(std.mem.indexOf(u8, skill.body, "zigttp:decode") != null);
    try testing.expect(std.mem.indexOf(u8, skill.body, "No classes, no async/await") != null);
}
