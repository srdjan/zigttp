const std = @import("std");
const h = @import("helpers.zig");

const value = h.value;
const object = h.object;
const context = h.context;
const string = h.string;
const http = h.http;

const createResultOk = h.createResultOk;
const createResultErr = h.createResultErr;
const getStringDataCtx = h.getStringDataCtx;

// ============================================================================
// JSON methods
// ============================================================================

/// JSON.parse(text) - Parse JSON string to JS value (standard behavior)
/// Returns parsed value or undefined on error
pub fn jsonParse(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    const str_val = args[0];
    const text = getStringDataCtx(str_val, ctx) orelse return value.JSValue.undefined_val;

    return parseJsonValue(ctx, text) catch value.JSValue.undefined_val;
}

/// JSON.tryParse(text) - Parse JSON string to JS value, returns Result
/// Returns Result.ok(value) on success, Result.err(message) on failure
pub fn jsonTryParse(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) {
        const err_msg = string.createString(ctx.allocator, "JSON.tryParse requires a string argument") catch return value.JSValue.undefined_val;
        return createResultErr(ctx, value.JSValue.fromPtr(err_msg));
    }

    const str_val = args[0];
    const text = getStringDataCtx(str_val, ctx) orelse {
        const err_msg = string.createString(ctx.allocator, "JSON.tryParse argument must be a string") catch return value.JSValue.undefined_val;
        return createResultErr(ctx, value.JSValue.fromPtr(err_msg));
    };

    const parsed = parseJsonValue(ctx, text) catch {
        const err_msg = string.createString(ctx.allocator, "Invalid JSON") catch return value.JSValue.undefined_val;
        return createResultErr(ctx, value.JSValue.fromPtr(err_msg));
    };
    return createResultOk(ctx, parsed);
}

/// JSON.stringify(value) - Convert JS value to JSON string
pub fn jsonStringify(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    const val = args[0];

    // Use shared JSON serialization from http module
    const json_js = http.valueToJsonString(ctx, val) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(json_js);
}

// ============================================================================
// JSON Error
// ============================================================================

pub const JsonError = error{ InvalidJson, UnexpectedEof, OutOfMemory, NoRootClass, ArenaObjectEscape, NoHiddenClassPool };

// ============================================================================
// JSON Shape Cache - Avoids hidden class transitions in JSON.parse
// ============================================================================

/// Maximum properties to cache per object shape
const JSON_SHAPE_MAX_PROPS = 16;

/// Number of cache entries (power of 2 for fast modulo)
const JSON_SHAPE_CACHE_SIZE = 64;

/// Entry in the JSON shape cache
const JSONShapeCacheEntry = struct {
    /// Hash of the property atom sequence
    hash: u64 = 0,
    /// Number of properties in this shape
    prop_count: u8 = 0,
    /// Property atoms in order
    atoms: [JSON_SHAPE_MAX_PROPS]object.Atom = undefined,
    /// Cached hidden class index for this shape
    class_idx: object.HiddenClassIndex = .none,
    /// Whether this entry is valid
    valid: bool = false,
};

/// Thread-local JSON shape cache
/// Maps property-atom-sequence to pre-built HiddenClassIndex
/// IMPORTANT: Must be cleared when creating a new Context to avoid stale class indices
var json_shape_cache: [JSON_SHAPE_CACHE_SIZE]JSONShapeCacheEntry = [_]JSONShapeCacheEntry{.{}} ** JSON_SHAPE_CACHE_SIZE;

/// Clear the JSON shape cache. Must be called when creating a new Context
/// to avoid stale HiddenClassIndex references from previous contexts.
pub fn clearJsonShapeCache() void {
    json_shape_cache = [_]JSONShapeCacheEntry{.{}} ** JSON_SHAPE_CACHE_SIZE;
}

/// Compute hash for a sequence of atoms
fn hashAtomSequence(atoms: []const object.Atom) u64 {
    var hash: u64 = 0xcbf29ce484222325; // FNV-1a offset basis
    for (atoms) |atom| {
        hash ^= @intFromEnum(atom);
        hash *%= 0x100000001b3; // FNV-1a prime
    }
    return hash;
}

/// Look up a cached shape by atom sequence
fn lookupJsonShape(atoms: []const object.Atom) ?object.HiddenClassIndex {
    if (atoms.len == 0 or atoms.len > JSON_SHAPE_MAX_PROPS) return null;

    const hash = hashAtomSequence(atoms);
    const idx = hash % JSON_SHAPE_CACHE_SIZE;
    const entry = &json_shape_cache[idx];

    if (!entry.valid or entry.hash != hash or entry.prop_count != atoms.len) {
        return null;
    }

    // Verify atoms match exactly (hash collision check)
    for (atoms, 0..) |atom, i| {
        if (entry.atoms[i] != atom) return null;
    }

    return entry.class_idx;
}

/// Cache a shape for future lookups
fn cacheJsonShape(atoms: []const object.Atom, class_idx: object.HiddenClassIndex) void {
    if (atoms.len == 0 or atoms.len > JSON_SHAPE_MAX_PROPS) return;

    const hash = hashAtomSequence(atoms);
    const idx = hash % JSON_SHAPE_CACHE_SIZE;
    const entry = &json_shape_cache[idx];

    entry.hash = hash;
    entry.prop_count = @intCast(atoms.len);
    for (atoms, 0..) |atom, i| {
        entry.atoms[i] = atom;
    }
    entry.class_idx = class_idx;
    entry.valid = true;
}

/// Build a hidden class with all properties in one go
fn buildClassForAtoms(pool: *object.HiddenClassPool, atoms: []const object.Atom) !object.HiddenClassIndex {
    var class_idx = pool.getEmptyClass();
    for (atoms) |atom| {
        class_idx = try pool.addProperty(class_idx, atom);
    }
    return class_idx;
}

/// Skip JSON whitespace characters (space, newline, carriage return, tab)
inline fn skipJsonWhitespace(text: []const u8, pos: *usize) void {
    while (pos.* < text.len) {
        switch (text[pos.*]) {
            ' ', '\n', '\r', '\t' => pos.* += 1,
            else => break,
        }
    }
}

/// Parse a JSON value from text
pub fn parseJsonValue(ctx: *context.Context, text: []const u8) JsonError!value.JSValue {
    var pos: usize = 0;
    return parseJsonValueAt(ctx, text, &pos);
}

/// Parse JSON value at position
fn parseJsonValueAt(ctx: *context.Context, text: []const u8, pos: *usize) JsonError!value.JSValue {
    skipJsonWhitespace(text, pos);

    if (pos.* >= text.len) return error.UnexpectedEof;

    const c = text[pos.*];

    // Object
    if (c == '{') {
        return parseJsonObject(ctx, text, pos);
    }

    // Array
    if (c == '[') {
        return parseJsonArray(ctx, text, pos);
    }

    // String
    if (c == '"') {
        return parseJsonString(ctx, text, pos);
    }

    // Number
    if (c == '-' or (c >= '0' and c <= '9')) {
        return parseJsonNumber(text, pos);
    }

    // true
    if (text.len >= pos.* + 4 and std.mem.eql(u8, text[pos.*..][0..4], "true")) {
        pos.* += 4;
        return value.JSValue.true_val;
    }

    // false
    if (text.len >= pos.* + 5 and std.mem.eql(u8, text[pos.*..][0..5], "false")) {
        pos.* += 5;
        return value.JSValue.false_val;
    }

    // null -> undefined (zigttp has no user-facing null)
    if (text.len >= pos.* + 4 and std.mem.eql(u8, text[pos.*..][0..4], "null")) {
        pos.* += 4;
        return value.JSValue.undefined_val;
    }

    return error.InvalidJson;
}

/// Parse JSON object with shape caching
/// Buffers properties first, then creates object with cached shape to avoid hidden class transitions
fn parseJsonObject(ctx: *context.Context, text: []const u8, pos: *usize) JsonError!value.JSValue {
    pos.* += 1; // skip '{'

    skipJsonWhitespace(text, pos);

    // Empty object fast path
    if (pos.* < text.len and text[pos.*] == '}') {
        pos.* += 1;
        const obj = try ctx.createObject(null);
        return obj.toValue();
    }

    // Buffer for collecting properties before object creation
    var atoms: [JSON_SHAPE_MAX_PROPS]object.Atom = undefined;
    var values: [JSON_SHAPE_MAX_PROPS]value.JSValue = undefined;
    var prop_count: usize = 0;

    // Parse all properties into buffer
    while (pos.* < text.len) {
        skipJsonWhitespace(text, pos);

        // Parse key directly to atom (avoids JSString allocation)
        if (pos.* >= text.len or text[pos.*] != '"') return error.InvalidJson;
        const atom = try parseJsonKey(ctx, text, pos);

        skipJsonWhitespace(text, pos);

        // Expect ':'
        if (pos.* >= text.len or text[pos.*] != ':') return error.InvalidJson;
        pos.* += 1;

        // Parse value
        const val = try parseJsonValueAt(ctx, text, pos);

        if (prop_count < JSON_SHAPE_MAX_PROPS) {
            atoms[prop_count] = atom;
            values[prop_count] = val;
            prop_count += 1;
        } else {
            // Overflow: fall back to slow path for remaining properties
            // First create object with buffered properties
            const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
            const class_idx = lookupJsonShape(atoms[0..prop_count]) orelse
                try buildClassForAtoms(pool, atoms[0..prop_count]);

            const obj = try ctx.createObjectWithClass(class_idx, ctx.object_prototype);
            for (0..prop_count) |i| {
                obj.setSlot(@intCast(i), values[i]);
            }

            // Add overflow property via slow path
            try ctx.setPropertyChecked(obj, atom, val);

            // Continue with slow path for any remaining properties
            skipJsonWhitespace(text, pos);
            while (pos.* < text.len and text[pos.*] != '}') {
                if (text[pos.*] == ',') {
                    pos.* += 1;
                    skipJsonWhitespace(text, pos);
                    if (pos.* >= text.len or text[pos.*] != '"') return error.InvalidJson;
                    const a = try parseJsonKey(ctx, text, pos);
                    skipJsonWhitespace(text, pos);
                    if (pos.* >= text.len or text[pos.*] != ':') return error.InvalidJson;
                    pos.* += 1;
                    const v = try parseJsonValueAt(ctx, text, pos);
                    try ctx.setPropertyChecked(obj, a, v);
                    skipJsonWhitespace(text, pos);
                } else {
                    return error.InvalidJson;
                }
            }
            if (pos.* < text.len and text[pos.*] == '}') {
                pos.* += 1;
                return obj.toValue();
            }
            return error.InvalidJson;
        }

        skipJsonWhitespace(text, pos);

        // Check for ',' or '}'
        if (pos.* >= text.len) return error.InvalidJson;
        if (text[pos.*] == '}') {
            pos.* += 1;
            break;
        }
        if (text[pos.*] == ',') {
            pos.* += 1;
            continue;
        }
        return error.InvalidJson;
    }

    // Create object with cached or new shape
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const atom_slice = atoms[0..prop_count];

    // Try to find cached shape
    var class_idx = lookupJsonShape(atom_slice);
    if (class_idx == null) {
        // Build new shape and cache it
        class_idx = try buildClassForAtoms(pool, atom_slice);
        cacheJsonShape(atom_slice, class_idx.?);
    }

    // Create object with the shape - no hidden class transitions!
    const obj = try ctx.createObjectWithClass(class_idx.?, ctx.object_prototype);

    // Set all property values directly by slot
    for (0..prop_count) |i| {
        obj.setSlot(@intCast(i), values[i]);
    }

    return obj.toValue();
}

/// Parse JSON array
fn parseJsonArray(ctx: *context.Context, text: []const u8, pos: *usize) JsonError!value.JSValue {
    pos.* += 1; // skip '['

    const arr = try ctx.createArray();
    arr.prototype = ctx.array_prototype;

    skipJsonWhitespace(text, pos);

    if (pos.* < text.len and text[pos.*] == ']') {
        pos.* += 1;
        arr.setArrayLength(0);
        return arr.toValue();
    }

    var index: u32 = 0;
    while (pos.* < text.len) {
        // Parse element
        const elem = try parseJsonValueAt(ctx, text, pos);
        try ctx.setIndexChecked(arr, index, elem);
        index += 1;

        skipJsonWhitespace(text, pos);

        // Check for ',' or ']'
        if (pos.* >= text.len) return error.InvalidJson;
        if (text[pos.*] == ']') {
            pos.* += 1;
            arr.setArrayLength(index);
            return arr.toValue();
        }
        if (text[pos.*] == ',') {
            pos.* += 1;
            continue;
        }
        return error.InvalidJson;
    }

    return error.InvalidJson;
}

/// Parse JSON object key directly to atom without creating JSString
/// Optimization: avoids string allocation for property keys
fn parseJsonKey(ctx: *context.Context, text: []const u8, pos: *usize) JsonError!object.Atom {
    pos.* += 1; // skip opening '"'
    const start = pos.*;

    // Fast path: scan for closing quote without escapes
    while (pos.* < text.len) {
        const c = text[pos.*];
        if (c == '"') {
            // No escapes - intern directly from JSON text slice
            const key_slice = text[start..pos.*];
            pos.* += 1;
            return ctx.atoms.intern(key_slice) catch return error.OutOfMemory;
        }
        if (c == '\\') {
            // Has escapes - use slow path with temporary buffer
            return parseJsonKeyWithEscapes(ctx, text, pos, start);
        }
        pos.* += 1;
    }

    return error.InvalidJson;
}

/// Slow path for JSON keys with escape sequences
fn parseJsonKeyWithEscapes(ctx: *context.Context, text: []const u8, pos: *usize, start: usize) JsonError!object.Atom {
    // Build unescaped key string
    var buffer = std.ArrayList(u8).empty;
    defer buffer.deinit(ctx.allocator);

    buffer.appendSlice(ctx.allocator, text[start..pos.*]) catch return error.OutOfMemory;

    while (pos.* < text.len) {
        const c = text[pos.*];
        if (c == '"') {
            pos.* += 1;
            return ctx.atoms.intern(buffer.items) catch return error.OutOfMemory;
        }
        if (c == '\\') {
            pos.* += 1;
            if (pos.* >= text.len) return error.InvalidJson;
            const escaped = text[pos.*];
            pos.* += 1;
            const byte: u8 = switch (escaped) {
                '"' => '"',
                '\\' => '\\',
                '/' => '/',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                'b' => 0x08,
                'f' => 0x0C,
                else => return error.InvalidJson,
            };
            buffer.append(ctx.allocator, byte) catch return error.OutOfMemory;
        } else {
            buffer.append(ctx.allocator, c) catch return error.OutOfMemory;
            pos.* += 1;
        }
    }

    return error.InvalidJson;
}

/// Parse JSON string - fast path for strings without escapes
fn parseJsonString(ctx: *context.Context, text: []const u8, pos: *usize) JsonError!value.JSValue {
    pos.* += 1; // skip opening '"'
    const start = pos.*;

    // Fast path: scan for closing quote without escapes
    while (pos.* < text.len) {
        const c = text[pos.*];
        if (c == '"') {
            // No escapes found - create string directly from slice
            const str_slice = text[start..pos.*];
            pos.* += 1;
            return try ctx.createString(str_slice);
        }
        if (c == '\\') {
            // Has escapes - use slow path
            return parseJsonStringWithEscapes(ctx, text, pos, start);
        }
        pos.* += 1;
    }

    return error.InvalidJson;
}

/// Slow path for JSON strings with escape sequences
fn parseJsonStringWithEscapes(ctx: *context.Context, text: []const u8, pos: *usize, start: usize) JsonError!value.JSValue {
    // Copy the part before the first escape
    var buffer = std.ArrayList(u8).empty;
    defer buffer.deinit(ctx.allocator);

    try buffer.appendSlice(ctx.allocator, text[start..pos.*]);

    while (pos.* < text.len) {
        const c = text[pos.*];
        if (c == '"') {
            pos.* += 1;
            return try ctx.createString(buffer.items);
        }
        if (c == '\\') {
            pos.* += 1;
            if (pos.* >= text.len) return error.InvalidJson;
            const escaped = text[pos.*];
            pos.* += 1;
            switch (escaped) {
                '"' => try buffer.append(ctx.allocator, '"'),
                '\\' => try buffer.append(ctx.allocator, '\\'),
                '/' => try buffer.append(ctx.allocator, '/'),
                'n' => try buffer.append(ctx.allocator, '\n'),
                'r' => try buffer.append(ctx.allocator, '\r'),
                't' => try buffer.append(ctx.allocator, '\t'),
                'b' => try buffer.append(ctx.allocator, 0x08),
                'f' => try buffer.append(ctx.allocator, 0x0C),
                'u' => {
                    // Unicode escape \uXXXX
                    if (pos.* + 4 > text.len) return error.InvalidJson;
                    const hex = text[pos.*..][0..4];
                    pos.* += 4;
                    const code = std.fmt.parseInt(u16, hex, 16) catch return error.InvalidJson;
                    // UTF-8 encoding
                    if (code < 0x80) {
                        try buffer.append(ctx.allocator, @intCast(code));
                    } else if (code < 0x800) {
                        try buffer.append(ctx.allocator, @intCast(0xC0 | (code >> 6)));
                        try buffer.append(ctx.allocator, @intCast(0x80 | (code & 0x3F)));
                    } else {
                        try buffer.append(ctx.allocator, @intCast(0xE0 | (code >> 12)));
                        try buffer.append(ctx.allocator, @intCast(0x80 | ((code >> 6) & 0x3F)));
                        try buffer.append(ctx.allocator, @intCast(0x80 | (code & 0x3F)));
                    }
                },
                else => try buffer.append(ctx.allocator, escaped),
            }
        } else {
            try buffer.append(ctx.allocator, c);
            pos.* += 1;
        }
    }

    return error.InvalidJson;
}

/// Parse JSON number
fn parseJsonNumber(text: []const u8, pos: *usize) JsonError!value.JSValue {
    const start = pos.*;

    // Optional minus
    if (pos.* < text.len and text[pos.*] == '-') {
        pos.* += 1;
    }

    // Integer part
    if (pos.* < text.len and text[pos.*] == '0') {
        pos.* += 1;
    } else {
        while (pos.* < text.len and text[pos.*] >= '0' and text[pos.*] <= '9') {
            pos.* += 1;
        }
    }

    // Fractional part
    var is_float = false;
    if (pos.* < text.len and text[pos.*] == '.') {
        is_float = true;
        pos.* += 1;
        while (pos.* < text.len and text[pos.*] >= '0' and text[pos.*] <= '9') {
            pos.* += 1;
        }
    }

    // Exponent part
    if (pos.* < text.len and (text[pos.*] == 'e' or text[pos.*] == 'E')) {
        is_float = true;
        pos.* += 1;
        if (pos.* < text.len and (text[pos.*] == '+' or text[pos.*] == '-')) {
            pos.* += 1;
        }
        while (pos.* < text.len and text[pos.*] >= '0' and text[pos.*] <= '9') {
            pos.* += 1;
        }
    }

    const num_str = text[start..pos.*];

    if (is_float) {
        const f = std.fmt.parseFloat(f64, num_str) catch return error.InvalidJson;
        // For now, truncate to int if it fits
        if (f == @trunc(f) and f >= -2147483648 and f <= 2147483647) {
            return value.JSValue.fromInt(@intFromFloat(f));
        }
        // Would need heap allocation for float - return as int truncated
        return value.JSValue.fromInt(@intFromFloat(@trunc(f)));
    } else {
        const i = std.fmt.parseInt(i32, num_str, 10) catch {
            // Try as float if integer overflow
            const f = std.fmt.parseFloat(f64, num_str) catch return error.InvalidJson;
            return value.JSValue.fromInt(@intFromFloat(@trunc(f)));
        };
        return value.JSValue.fromInt(i);
    }
}
