//! TypeMap: Structured metadata for type annotations stripped by the TypeScript stripper.
//!
//! When the stripper runs, it records every type annotation it strips
//! as a TypeMapEntry. This allows downstream passes (type checker, type environment)
//! to reconstruct type information without modifying the parser or IR.
//!
//! Each entry records:
//! - What kind of annotation it is (type alias, param, return, etc.)
//! - Source byte offsets for the type text in the *original* (pre-strip) source
//! - Context position (line/col) of the owning declaration
//! - Name byte offsets for the identifier being annotated

const std = @import("std");

/// Classification of type annotation sites.
pub const TypeMapKind = enum(u8) {
    /// `type Foo = { ... }` - type alias declaration
    type_alias,
    /// `interface CacheFx { ... }` - interface declaration
    interface_decl,
    /// `distinct type Foo = string` - nominal/branded type declaration
    distinct_type,
    /// `const x: Type = ...` or `let x: Type = ...` - variable annotation
    var_annotation,
    /// `function f(x: Type, ...)` - parameter annotation
    param_annotation,
    /// `function f(...): Type` - return type annotation
    return_annotation,
    /// `function f(x: unknown): x is string` - type guard return annotation
    type_guard_annotation,
    /// `function f<T, U>(...)` or `type Foo<T> = ...` - generic parameters
    generic_params,
};

/// A single recorded type annotation.
pub const TypeMapEntry = struct {
    /// What kind of annotation this is.
    kind: TypeMapKind,
    /// Byte offset in original source where the type text starts (after the colon/equals).
    source_start: u32,
    /// Byte offset in original source where the type text ends.
    source_end: u32,
    /// Line number of the owning declaration (1-based).
    context_line: u32,
    /// Column of the owning declaration (1-based).
    context_col: u32,
    /// Byte offset of the name this type annotates (0 = none, e.g. for return types).
    name_start: u32,
    /// Byte offset end of the name.
    name_end: u32,
};

/// Collection of type annotations extracted during stripping.
pub const TypeMap = struct {
    entries: std.ArrayListUnmanaged(TypeMapEntry),
    /// Borrowed reference to the original (pre-strip) source text.
    /// Used by getTypeText to extract raw type strings.
    original_source: []const u8,

    pub fn init(original_source: []const u8) TypeMap {
        return .{
            .entries = .empty,
            .original_source = original_source,
        };
    }

    pub fn deinit(self: *TypeMap, allocator: std.mem.Allocator) void {
        self.entries.deinit(allocator);
    }

    /// Add a type annotation entry.
    pub fn addEntry(self: *TypeMap, allocator: std.mem.Allocator, entry: TypeMapEntry) !void {
        try self.entries.append(allocator, entry);
    }

    /// Extract the raw type text for an entry from the original source.
    pub fn getTypeText(self: *const TypeMap, entry: TypeMapEntry) []const u8 {
        if (entry.source_start >= self.original_source.len or
            entry.source_end > self.original_source.len or
            entry.source_start >= entry.source_end)
        {
            return "";
        }
        return self.original_source[entry.source_start..entry.source_end];
    }

    /// Extract the name text for an entry from the original source.
    pub fn getNameText(self: *const TypeMap, entry: TypeMapEntry) ?[]const u8 {
        if (entry.name_start == 0 and entry.name_end == 0) return null;
        if (entry.name_start >= self.original_source.len or
            entry.name_end > self.original_source.len or
            entry.name_start >= entry.name_end)
        {
            return null;
        }
        return self.original_source[entry.name_start..entry.name_end];
    }

    /// Return the number of recorded entries.
    pub fn count(self: *const TypeMap) usize {
        return self.entries.items.len;
    }
};

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "TypeMap basic operations" {
    const allocator = std.testing.allocator;
    const source = "const x: number = 42;";

    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    try tm.addEntry(allocator, .{
        .kind = .var_annotation,
        .source_start = 9, // "number"
        .source_end = 15,
        .context_line = 1,
        .context_col = 1,
        .name_start = 6, // "x"
        .name_end = 7,
    });

    try std.testing.expectEqual(@as(usize, 1), tm.count());
    try std.testing.expectEqualStrings("number", tm.getTypeText(tm.entries.items[0]));
    try std.testing.expectEqualStrings("x", tm.getNameText(tm.entries.items[0]).?);
}

test "TypeMap type alias" {
    const allocator = std.testing.allocator;
    const source = "type Config = { port: number; host: string };";

    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    try tm.addEntry(allocator, .{
        .kind = .type_alias,
        .source_start = 14, // "{ port: number; host: string }"
        .source_end = 44,
        .context_line = 1,
        .context_col = 1,
        .name_start = 5, // "Config"
        .name_end = 11,
    });

    try std.testing.expectEqualStrings("{ port: number; host: string }", tm.getTypeText(tm.entries.items[0]));
    try std.testing.expectEqualStrings("Config", tm.getNameText(tm.entries.items[0]).?);
}

test "TypeMap return annotation has no name" {
    const allocator = std.testing.allocator;
    const source = "function handler(req: Request): Response { }";

    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    try tm.addEntry(allocator, .{
        .kind = .return_annotation,
        .source_start = 32, // "Response"
        .source_end = 40,
        .context_line = 1,
        .context_col = 1,
        .name_start = 0,
        .name_end = 0,
    });

    try std.testing.expect(tm.getNameText(tm.entries.items[0]) == null);
    try std.testing.expectEqualStrings("Response", tm.getTypeText(tm.entries.items[0]));
}

test "TypeMap empty source edge cases" {
    const allocator = std.testing.allocator;
    const source = "";

    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    try tm.addEntry(allocator, .{
        .kind = .var_annotation,
        .source_start = 100,
        .source_end = 200,
        .context_line = 1,
        .context_col = 1,
        .name_start = 0,
        .name_end = 0,
    });

    try std.testing.expectEqualStrings("", tm.getTypeText(tm.entries.items[0]));
}
