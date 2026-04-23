//! Tiny frontmatter parser used to populate the skills and prompts catalogs
//! at compile time. Subset we actually need:
//!
//!   ---
//!   name: handler-scaffold
//!   description: one-line prose, trimmed
//!   ---
//!   <free-form body>
//!
//! Rules:
//!   - Source must begin with "---\n".
//!   - Frontmatter runs until the next "---\n" line.
//!   - Keys match `[a-z_][a-z0-9_-]*`; values are the trimmed rest of line.
//!   - No nested structures, lists, multi-line values, or comments.
//!   - A Document can only hold up to `MAX_FIELDS` entries; exceeding returns
//!     `error.TooManyFields`.
//!
//! Works at both runtime and comptime because the field store is a fixed
//! inline array; the normal `arr ++ [_]T{...}` trick is comptime-only and
//! doesn't survive a runtime test call.

const std = @import("std");

pub const MAX_FIELDS: usize = 8;

pub const Field = struct {
    key: []const u8,
    value: []const u8,
};

pub const Document = struct {
    fields_storage: [MAX_FIELDS]Field,
    field_count: usize,
    body: []const u8,

    pub fn fields(self: *const Document) []const Field {
        return self.fields_storage[0..self.field_count];
    }

    pub fn get(self: *const Document, key: []const u8) ?[]const u8 {
        for (self.fields()) |f| {
            if (std.mem.eql(u8, f.key, key)) return f.value;
        }
        return null;
    }

    pub fn require(self: *const Document, comptime key: []const u8) []const u8 {
        return self.get(key) orelse @compileError("frontmatter missing required key: " ++ key);
    }
};

pub const ParseError = error{
    MissingOpeningFence,
    MissingClosingFence,
    MissingColon,
    EmptyKey,
    TooManyFields,
};

/// Parse `src`. Slices in the returned Document alias `src` so the caller
/// must keep it alive. When invoked at comptime, `src` is typically
/// `@embedFile(...)` whose lifetime is the whole program.
pub fn parse(src: []const u8) ParseError!Document {
    const fence = "---\n";
    if (!std.mem.startsWith(u8, src, fence)) return error.MissingOpeningFence;

    const after_open = fence.len;
    const close_rel = std.mem.indexOf(u8, src[after_open..], fence) orelse
        return error.MissingClosingFence;
    const close_abs = after_open + close_rel;
    const body_start = close_abs + fence.len;

    var doc: Document = .{
        .fields_storage = undefined,
        .field_count = 0,
        .body = &.{},
    };

    var cursor: usize = after_open;
    while (cursor < close_abs) {
        const nl = std.mem.indexOfScalarPos(u8, src, cursor, '\n') orelse close_abs;
        const line = src[cursor..nl];
        cursor = nl + 1;
        if (line.len == 0) continue;

        const colon = std.mem.indexOfScalar(u8, line, ':') orelse return error.MissingColon;
        const key = std.mem.trim(u8, line[0..colon], " \t");
        const raw_value = std.mem.trim(u8, line[colon + 1 ..], " \t\r");
        // Quoted values let authors write descriptions containing colons
        // without confusing the "first colon is the separator" rule.
        const value = unquote(raw_value);
        if (key.len == 0) return error.EmptyKey;
        if (doc.field_count >= MAX_FIELDS) return error.TooManyFields;

        doc.fields_storage[doc.field_count] = .{ .key = key, .value = value };
        doc.field_count += 1;
    }

    // Trim leading newlines after the closing fence so authors can separate
    // the fence from the body with a blank line.
    var body = src[body_start..];
    while (body.len > 0 and (body[0] == '\n' or body[0] == '\r')) {
        body = body[1..];
    }
    doc.body = body;

    return doc;
}

/// Comptime-only entrypoint: any error becomes a `@compileError`, and the
/// branch quota is bumped because `std.mem.indexOf` walks long markdown
/// bodies char-by-char and the default 1000-branch ceiling is low.
///
/// Budget: each embedded `.md` consumes ~O(bytes) branches across the fence
/// search, line walk, and body scan. 50k gives ~10x headroom for a
/// 5KiB file; revisit if individual catalog entries grow much beyond that.
pub fn parseComptime(comptime src: []const u8) Document {
    @setEvalBranchQuota(50_000);
    return parse(src) catch |err| @compileError("frontmatter parse failed: " ++ @errorName(err));
}

fn unquote(v: []const u8) []const u8 {
    if (v.len < 2) return v;
    const first = v[0];
    const last = v[v.len - 1];
    if ((first == '"' and last == '"') or (first == '\'' and last == '\'')) {
        return v[1 .. v.len - 1];
    }
    return v;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "parse: basic key/value + body" {
    const src =
        \\---
        \\name: foo
        \\description: a short line
        \\---
        \\hello world
        \\
    ;
    const doc = try parse(src);
    try testing.expectEqualStrings("foo", doc.get("name").?);
    try testing.expectEqualStrings("a short line", doc.get("description").?);
    try testing.expectEqualStrings("hello world\n", doc.body);
}

test "parse: missing opening fence is an error" {
    const src = "name: foo\n---\nbody\n";
    try testing.expectError(error.MissingOpeningFence, parse(src));
}

test "parse: missing closing fence is an error" {
    const src = "---\nname: foo\nbody but no fence\n";
    try testing.expectError(error.MissingClosingFence, parse(src));
}

test "parse: missing colon on non-empty line is an error" {
    const src = "---\nname\n---\nbody\n";
    try testing.expectError(error.MissingColon, parse(src));
}

test "parse: empty key is an error" {
    const src = "---\n: value\n---\n";
    try testing.expectError(error.EmptyKey, parse(src));
}

test "parse: extra blank line between fence and body is stripped" {
    const src = "---\nname: a\n---\n\n\nbody content\n";
    const doc = try parse(src);
    try testing.expectEqualStrings("body content\n", doc.body);
}

test "parse: more than MAX_FIELDS entries is an error" {
    comptime var many: []const u8 = "---\n";
    comptime {
        var i: usize = 0;
        while (i < MAX_FIELDS + 1) : (i += 1) {
            many = many ++ std.fmt.comptimePrint("k{d}: v{d}\n", .{ i, i });
        }
        many = many ++ "---\nbody\n";
    }
    try testing.expectError(error.TooManyFields, parse(many));
}

test "parseComptime: runs at compile time" {
    const src =
        \\---
        \\name: x
        \\description: y
        \\---
        \\z
        \\
    ;
    const doc = comptime parseComptime(src);
    try testing.expectEqualStrings("x", doc.get("name").?);
    try testing.expectEqualStrings("y", doc.get("description").?);
    try testing.expectEqualStrings("z\n", doc.body);
}

test "parse: quoted value preserves embedded colons verbatim" {
    const src = "---\ndescription: \"Usage: /template:foo <arg>\"\n---\nbody\n";
    const doc = try parse(src);
    try testing.expectEqualStrings("Usage: /template:foo <arg>", doc.get("description").?);
}

test "parse: single-quoted value also strips its quotes" {
    const src = "---\nkey: 'colon: inside'\n---\n";
    const doc = try parse(src);
    try testing.expectEqualStrings("colon: inside", doc.get("key").?);
}

test "parse: mismatched quotes are left intact" {
    const src = "---\nkey: \"only one\n---\n";
    const doc = try parse(src);
    try testing.expectEqualStrings("\"only one", doc.get("key").?);
}

test "Document.get returns null for absent keys" {
    const src = "---\na: 1\n---\n";
    const doc = try parse(src);
    try testing.expect(doc.get("b") == null);
}
