//! Bytecode Serialization and Caching
//!
//! Enables content-addressable bytecode caching for faster FaaS cold starts.
//! Uses SHA-256 for source hashing and supports both in-memory and file-based caching.
//!
//! Phase 1: JSValue Constant Serialization
//! ----------------------------------------
//! Constants in bytecode include integers, floats, strings, and special values.
//! Each type is tagged and serialized with appropriate encoding:
//! - Integers: inline i32 (4 bytes)
//! - Floats: f64 bits (8 bytes)
//! - Strings: length (u16) + UTF-8 bytes
//! - Special: single byte enum (null, undefined, true, false)
//! - Nested functions: recursive bytecode serialization

const std = @import("std");
const bytecode = @import("bytecode.zig");
const value = @import("value.zig");
const string = @import("string.zig");
const heap = @import("heap.zig");

/// Cache key derived from source content
pub const CacheKey = [32]u8;

// ============================================================================
// Phase 1: Constant Serialization
// ============================================================================

/// Tag for serialized constant values
pub const ConstantTag = enum(u8) {
    int = 0, // followed by i32 (4 bytes)
    float = 1, // followed by f64 (8 bytes)
    string = 2, // followed by u16 length + UTF-8 bytes
    special = 3, // followed by u8 special code
    nested_function = 4, // followed by serialized FunctionBytecodeCompact
};

/// Special value codes for serialization
pub const SpecialCode = enum(u8) {
    null_val = 0,
    undefined_val = 1,
    true_val = 2,
    false_val = 3,
    exception_val = 4,
};

/// Serialize a slice of JSValue constants to a writer
pub fn serializeConstants(constants: []const value.JSValue, writer: anytype, allocator: std.mem.Allocator) SerializeError!void {
    try writer.writeInt(u16, @intCast(constants.len), .little);

    for (constants) |val| {
        try serializeConstant(val, writer, allocator);
    }
}

/// Error type for serialization operations
pub const SerializeError = error{
    NoSpaceLeft,
    OutOfMemory,
};

/// Serialize a single JSValue constant
fn serializeConstant(val: value.JSValue, writer: anytype, allocator: std.mem.Allocator) SerializeError!void {
    // Check special values first (no pointer dereference)
    if (val.isNull()) {
        try writer.writeByte(@intFromEnum(ConstantTag.special));
        try writer.writeByte(@intFromEnum(SpecialCode.null_val));
        return;
    }
    if (val.isUndefined()) {
        try writer.writeByte(@intFromEnum(ConstantTag.special));
        try writer.writeByte(@intFromEnum(SpecialCode.undefined_val));
        return;
    }
    if (val.isTrue()) {
        try writer.writeByte(@intFromEnum(ConstantTag.special));
        try writer.writeByte(@intFromEnum(SpecialCode.true_val));
        return;
    }
    if (val.isFalse()) {
        try writer.writeByte(@intFromEnum(ConstantTag.special));
        try writer.writeByte(@intFromEnum(SpecialCode.false_val));
        return;
    }
    if (val.isException()) {
        try writer.writeByte(@intFromEnum(ConstantTag.special));
        try writer.writeByte(@intFromEnum(SpecialCode.exception_val));
        return;
    }

    // Check inline integer (31-bit signed)
    if (val.isInt()) {
        try writer.writeByte(@intFromEnum(ConstantTag.int));
        try writer.writeInt(i32, val.getInt(), .little);
        return;
    }

    // Pointer-based values - check type via JSValue methods
    if (val.isFloat64()) {
        const f = val.toNumber() orelse 0.0;
        try writer.writeByte(@intFromEnum(ConstantTag.float));
        try writer.writeInt(u64, @bitCast(f), .little);
        return;
    }

    if (val.isString()) {
        const js_str = val.toPtr(string.JSString);
        const str_data = js_str.data();
        try writer.writeByte(@intFromEnum(ConstantTag.string));
        try writer.writeInt(u16, @intCast(str_data.len), .little);
        try writer.writeAll(str_data);
        return;
    }

    if (val.isFunction()) {
        const func = val.toPtr(bytecode.FunctionBytecode);
        try writer.writeByte(@intFromEnum(ConstantTag.nested_function));
        try serializeFunctionBytecode(func, writer, allocator);
        return;
    }

    // Unknown pointer type - serialize as undefined
    if (val.isPtr()) {
        try writer.writeByte(@intFromEnum(ConstantTag.special));
        try writer.writeByte(@intFromEnum(SpecialCode.undefined_val));
    }
}

/// Serialize FunctionBytecode (non-compact) to a writer
pub fn serializeFunctionBytecode(func: *const bytecode.FunctionBytecode, writer: anytype, allocator: std.mem.Allocator) SerializeError!void {
    // Write fixed fields
    try writer.writeInt(u32, func.name_atom, .little);
    try writer.writeInt(u16, func.arg_count, .little);
    try writer.writeInt(u16, func.local_count, .little);
    try writer.writeInt(u16, func.stack_size, .little);
    try writer.writeByte(@as(u8, @bitCast(func.flags)));
    try writer.writeByte(func.upvalue_count);

    // Write code
    try writer.writeInt(u32, @intCast(func.code.len), .little);
    try writer.writeAll(func.code);

    // Write upvalue info
    for (func.upvalue_info) |info| {
        // Encode: is_local in bit 7, index in bits 0-6
        const encoded: u8 = (@as(u8, @intFromBool(info.is_local)) << 7) | (info.index & 0x7F);
        try writer.writeByte(encoded);
    }

    // Recursively serialize constants
    try serializeConstants(func.constants, writer, allocator);
}

/// Error type for deserialization operations
pub const DeserializeError = error{
    EndOfStream,
    OutOfMemory,
    IncompleteRead,
};

/// Deserialize constants from a reader, reconstructing JSValues
pub fn deserializeConstants(
    reader: anytype,
    allocator: std.mem.Allocator,
    strings_table: ?*string.StringTable,
) DeserializeError![]value.JSValue {
    const count = try reader.readInt(u16, .little);
    const constants = try allocator.alloc(value.JSValue, count);
    errdefer allocator.free(constants);

    for (constants) |*slot| {
        slot.* = try deserializeConstant(reader, allocator, strings_table);
    }

    return constants;
}

/// Deserialize a single JSValue constant
fn deserializeConstant(
    reader: anytype,
    allocator: std.mem.Allocator,
    strings_table: ?*string.StringTable,
) DeserializeError!value.JSValue {
    const tag_byte = try reader.readByte();
    const tag: ConstantTag = @enumFromInt(tag_byte);

    switch (tag) {
        .int => {
            const val = try reader.readInt(i32, .little);
            return value.JSValue.fromInt(val);
        },
        .float => {
            const bits = try reader.readInt(u64, .little);
            const f: f64 = @bitCast(bits);
            // Allocate Float64Box
            const float_box = try allocator.create(value.JSValue.Float64Box);
            float_box.* = .{
                .header = heap.MemBlockHeader.init(.float64, @sizeOf(value.JSValue.Float64Box)),
                ._pad = 0,
                .value = f,
            };
            return value.JSValue.fromPtr(float_box);
        },
        .string => {
            const len = try reader.readInt(u16, .little);
            const bytes = try allocator.alloc(u8, len);
            defer allocator.free(bytes);
            const read_count = try reader.readAll(bytes);
            if (read_count != len) return error.IncompleteRead;

            // Intern string if table available, otherwise create directly
            const js_str = if (strings_table) |table|
                try table.intern(bytes)
            else
                try string.createString(allocator, bytes);

            return value.JSValue.fromPtr(js_str);
        },
        .special => {
            const code_byte = try reader.readByte();
            const code: SpecialCode = @enumFromInt(code_byte);
            return switch (code) {
                .null_val => value.JSValue.null_val,
                .undefined_val => value.JSValue.undefined_val,
                .true_val => value.JSValue.true_val,
                .false_val => value.JSValue.false_val,
                .exception_val => value.JSValue.exception_val,
            };
        },
        .nested_function => {
            const func = try deserializeFunctionBytecode(reader, allocator, strings_table);
            return value.JSValue.fromPtr(func);
        },
    }
}

/// Deserialize FunctionBytecode from a reader
pub fn deserializeFunctionBytecode(
    reader: anytype,
    allocator: std.mem.Allocator,
    strings_table: ?*string.StringTable,
) DeserializeError!*bytecode.FunctionBytecode {
    // Read fixed fields
    const name_atom = try reader.readInt(u32, .little);
    const arg_count = try reader.readInt(u16, .little);
    const local_count = try reader.readInt(u16, .little);
    const stack_size = try reader.readInt(u16, .little);
    const flags: bytecode.FunctionFlags = @bitCast(try reader.readByte());
    const upvalue_count = try reader.readByte();

    // Read code
    const code_len = try reader.readInt(u32, .little);
    const code = try allocator.alloc(u8, code_len);
    errdefer allocator.free(code);
    const code_read = try reader.readAll(code);
    if (code_read != code_len) return error.IncompleteRead;

    // Read upvalue info
    const upvalue_info = try allocator.alloc(bytecode.UpvalueInfo, upvalue_count);
    errdefer allocator.free(upvalue_info);
    for (upvalue_info) |*info| {
        const encoded = try reader.readByte();
        info.* = .{
            .is_local = (encoded & 0x80) != 0,
            .index = encoded & 0x7F,
        };
    }

    // Recursively deserialize constants
    const constants = try deserializeConstants(reader, allocator, strings_table);
    errdefer allocator.free(constants);

    // Allocate and populate FunctionBytecode
    const func = try allocator.create(bytecode.FunctionBytecode);
    func.* = .{
        .header = .{},
        .name_atom = name_atom,
        .arg_count = arg_count,
        .local_count = local_count,
        .stack_size = stack_size,
        .flags = flags,
        .upvalue_count = upvalue_count,
        .upvalue_info = upvalue_info,
        .code = code,
        .constants = constants,
        .source_map = null,
    };

    return func;
}

/// Bytecode serialization format version
pub const CACHE_VERSION: u32 = 1;

/// Cache file magic bytes "ZTSC" (zts cache)
pub const CACHE_MAGIC: u32 = 0x5A545343;

/// Cache entry header
pub const CacheHeader = extern struct {
    magic: u32 = CACHE_MAGIC,
    version: u32 = CACHE_VERSION,
    source_hash: CacheKey,
    bytecode_size: u32,
    checksum: u32, // CRC32 of bytecode data
};

/// Bytecode cache for storing and retrieving compiled functions
pub const BytecodeCache = struct {
    allocator: std.mem.Allocator,

    /// In-memory cache: source hash -> serialized bytecode
    cache: std.AutoHashMapUnmanaged(CacheKey, []const u8),

    /// Cache statistics
    hits: u64,
    misses: u64,

    pub fn init(allocator: std.mem.Allocator) BytecodeCache {
        return .{
            .allocator = allocator,
            .cache = .{},
            .hits = 0,
            .misses = 0,
        };
    }

    pub fn deinit(self: *BytecodeCache) void {
        // Free all cached bytecode
        var it = self.cache.valueIterator();
        while (it.next()) |bytes| {
            self.allocator.free(bytes.*);
        }
        self.cache.deinit(self.allocator);
    }

    /// Generate cache key from source code
    pub fn cacheKey(source: []const u8) CacheKey {
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        hasher.update(source);
        return hasher.finalResult();
    }

    /// Store bytecode in cache
    pub fn put(self: *BytecodeCache, key: CacheKey, func: *const bytecode.FunctionBytecodeCompact) !void {
        const bytes = func.asBytes();

        // Copy bytes to owned memory
        const owned = try self.allocator.dupe(u8, bytes);
        errdefer self.allocator.free(owned);

        // Remove old entry if exists
        if (self.cache.fetchRemove(key)) |old| {
            self.allocator.free(old.value);
        }

        try self.cache.put(self.allocator, key, owned);
    }

    /// Retrieve bytecode from cache
    pub fn get(self: *BytecodeCache, key: CacheKey) ?*const bytecode.FunctionBytecodeCompact {
        if (self.cache.get(key)) |bytes| {
            self.hits += 1;
            return @ptrCast(@alignCast(bytes.ptr));
        }
        self.misses += 1;
        return null;
    }

    /// Store raw serialized bytes in cache (Phase 1b: for JSValue constant serialization)
    pub fn putRaw(self: *BytecodeCache, key: CacheKey, data: []const u8) !void {
        // Copy bytes to owned memory
        const owned = try self.allocator.dupe(u8, data);
        errdefer self.allocator.free(owned);

        // Remove old entry if exists
        if (self.cache.fetchRemove(key)) |old| {
            self.allocator.free(old.value);
        }

        try self.cache.put(self.allocator, key, owned);
    }

    /// Get raw serialized bytes from cache (Phase 1b: for JSValue constant deserialization)
    pub fn getRaw(self: *BytecodeCache, key: CacheKey) ?[]const u8 {
        if (self.cache.get(key)) |bytes| {
            self.hits += 1;
            return bytes;
        }
        self.misses += 1;
        return null;
    }

    /// Check if key exists in cache
    pub fn contains(self: *const BytecodeCache, key: CacheKey) bool {
        return self.cache.contains(key);
    }

    /// Get cache hit rate
    pub fn hitRate(self: *const BytecodeCache) f64 {
        const total = self.hits + self.misses;
        if (total == 0) return 0.0;
        return @as(f64, @floatFromInt(self.hits)) / @as(f64, @floatFromInt(total));
    }

    /// Clear all cached entries
    pub fn clear(self: *BytecodeCache) void {
        var it = self.cache.valueIterator();
        while (it.next()) |bytes| {
            self.allocator.free(bytes.*);
        }
        self.cache.clearRetainingCapacity();
        self.hits = 0;
        self.misses = 0;
    }

    /// Number of cached entries
    pub fn count(self: *const BytecodeCache) usize {
        return self.cache.count();
    }
};

/// Serialize bytecode to a writer
pub fn serialize(func: *const bytecode.FunctionBytecodeCompact, source_hash: CacheKey, writer: anytype) !void {
    const bytes = func.asBytes();

    // Write header
    const header = CacheHeader{
        .source_hash = source_hash,
        .bytecode_size = @intCast(bytes.len),
        .checksum = std.hash.crc.Crc32IsoHdlc.hash(bytes),
    };

    try writer.writeAll(std.mem.asBytes(&header));
    try writer.writeAll(bytes);
}

/// Deserialize bytecode from a reader
pub fn deserialize(reader: anytype, allocator: std.mem.Allocator) !*bytecode.FunctionBytecodeCompact {
    // Read header
    var header: CacheHeader = undefined;
    const header_bytes = try reader.readBytesNoEof(@sizeOf(CacheHeader));
    header = @bitCast(header_bytes);

    // Validate magic and version
    if (header.magic != CACHE_MAGIC) {
        return error.InvalidMagic;
    }
    if (header.version != CACHE_VERSION) {
        return error.VersionMismatch;
    }

    // Allocate and read bytecode
    const alignment: std.mem.Alignment = .@"8";
    const bytes = try allocator.alignedAlloc(u8, alignment, header.bytecode_size);
    errdefer allocator.free(bytes);

    const read_count = try reader.readAll(bytes);
    if (read_count != header.bytecode_size) {
        return error.IncompleteRead;
    }

    // Verify checksum
    const computed_checksum = std.hash.crc.Crc32IsoHdlc.hash(bytes);
    if (computed_checksum != header.checksum) {
        return error.ChecksumMismatch;
    }

    return @ptrCast(@alignCast(bytes.ptr));
}

/// Validate cached bytecode integrity
pub fn validateCache(bytes: []const u8) bool {
    if (bytes.len < @sizeOf(CacheHeader)) return false;

    const header: *const CacheHeader = @ptrCast(@alignCast(bytes.ptr));

    if (header.magic != CACHE_MAGIC) return false;
    if (header.version != CACHE_VERSION) return false;

    const data_start = @sizeOf(CacheHeader);
    if (bytes.len < data_start + header.bytecode_size) return false;

    const data = bytes[data_start..][0..header.bytecode_size];
    const computed = std.hash.crc.Crc32IsoHdlc.hash(data);

    return computed == header.checksum;
}

test "BytecodeCache basic operations" {
    const allocator = std.testing.allocator;

    var cache = BytecodeCache.init(allocator);
    defer cache.deinit();

    // Create test bytecode
    const code = [_]u8{ 0x08, 0x53 }; // push_null, ret
    const constants = [_]u32{42};

    const func = try bytecode.FunctionBytecodeCompact.create(
        allocator,
        .{},
        0,
        0,
        1,
        2,
        .{},
        &code,
        &constants,
        &.{},
    );
    defer func.destroy(allocator);

    // Generate cache key
    const source = "function test() { return null; }";
    const key = BytecodeCache.cacheKey(source);

    // Store in cache
    try cache.put(key, func);
    try std.testing.expectEqual(@as(usize, 1), cache.count());

    // Retrieve from cache
    const retrieved = cache.get(key);
    try std.testing.expect(retrieved != null);
    try std.testing.expectEqual(func.code_len, retrieved.?.code_len);

    // Miss for different key
    const other_key = BytecodeCache.cacheKey("different source");
    const miss = cache.get(other_key);
    try std.testing.expect(miss == null);

    // Check stats
    try std.testing.expectEqual(@as(u64, 1), cache.hits);
    try std.testing.expectEqual(@as(u64, 1), cache.misses);
}

/// Simple slice writer for serialization
pub const SliceWriter = struct {
    buffer: []u8,
    pos: usize = 0,

    pub fn writeAll(self: *SliceWriter, data: []const u8) !void {
        if (self.pos + data.len > self.buffer.len) return error.NoSpaceLeft;
        @memcpy(self.buffer[self.pos..][0..data.len], data);
        self.pos += data.len;
    }

    pub fn writeByte(self: *SliceWriter, byte: u8) !void {
        if (self.pos >= self.buffer.len) return error.NoSpaceLeft;
        self.buffer[self.pos] = byte;
        self.pos += 1;
    }

    pub fn writeInt(self: *SliceWriter, comptime T: type, val: T, _: std.builtin.Endian) !void {
        const bytes = std.mem.asBytes(&val);
        try self.writeAll(bytes);
    }

    pub fn getWritten(self: *const SliceWriter) []const u8 {
        return self.buffer[0..self.pos];
    }
};

/// Simple slice reader for deserialization
pub const SliceReader = struct {
    data: []const u8,
    pos: usize = 0,

    pub fn readBytesNoEof(self: *SliceReader, comptime n: usize) ![n]u8 {
        if (self.pos + n > self.data.len) return error.EndOfStream;
        const result: *const [n]u8 = @ptrCast(self.data[self.pos..][0..n]);
        self.pos += n;
        return result.*;
    }

    pub fn readByte(self: *SliceReader) !u8 {
        if (self.pos >= self.data.len) return error.EndOfStream;
        const byte = self.data[self.pos];
        self.pos += 1;
        return byte;
    }

    pub fn readInt(self: *SliceReader, comptime T: type, _: std.builtin.Endian) !T {
        const size = @sizeOf(T);
        if (self.pos + size > self.data.len) return error.EndOfStream;
        const result: T = @bitCast(self.data[self.pos..][0..size].*);
        self.pos += size;
        return result;
    }

    pub fn readAll(self: *SliceReader, buffer: []u8) !usize {
        const available = self.data.len - self.pos;
        const to_read = @min(buffer.len, available);
        @memcpy(buffer[0..to_read], self.data[self.pos..][0..to_read]);
        self.pos += to_read;
        return to_read;
    }
};

test "BytecodeCache serialization roundtrip" {
    const allocator = std.testing.allocator;

    // Create test bytecode
    const code = [_]u8{ 0x01, 0x00, 0x02, 0x20, 0x53 };
    const constants = [_]u32{ 10, 20 };
    const upvalues = [_]bytecode.UpvalueInfo{
        .{ .is_local = true, .index = 0 },
    };

    const func = try bytecode.FunctionBytecodeCompact.create(
        allocator,
        .{},
        123, // name_atom
        2, // arg_count
        4, // local_count
        8, // stack_size
        .{ .is_generator = true },
        &code,
        &constants,
        &upvalues,
    );
    defer func.destroy(allocator);

    // Serialize
    const source_hash = BytecodeCache.cacheKey("test source");
    var buffer: [4096]u8 = undefined;
    var writer = SliceWriter{ .buffer = &buffer };

    try serialize(func, source_hash, &writer);

    // Deserialize
    const written = writer.getWritten();
    var reader = SliceReader{ .data = written };
    const restored = try deserialize(&reader, allocator);
    defer restored.destroy(allocator);

    // Verify fields match
    try std.testing.expectEqual(func.name_atom, restored.name_atom);
    try std.testing.expectEqual(func.arg_count, restored.arg_count);
    try std.testing.expectEqual(func.local_count, restored.local_count);
    try std.testing.expectEqual(func.code_len, restored.code_len);
    try std.testing.expectEqual(func.const_count, restored.const_count);
    try std.testing.expectEqual(func.upvalue_count, restored.upvalue_count);
}

test "BytecodeCache cache key determinism" {
    const source1 = "const x = 1;";
    const source2 = "const x = 1;";
    const source3 = "const y = 2;";

    const key1 = BytecodeCache.cacheKey(source1);
    const key2 = BytecodeCache.cacheKey(source2);
    const key3 = BytecodeCache.cacheKey(source3);

    // Same source should produce same key
    try std.testing.expectEqual(key1, key2);

    // Different source should produce different key
    try std.testing.expect(!std.mem.eql(u8, &key1, &key3));
}

test "validateCache detects corruption" {
    // Valid header with invalid checksum
    var bad_data: [100]u8 = undefined;
    @memset(&bad_data, 0);

    const header: *CacheHeader = @ptrCast(@alignCast(&bad_data));
    header.magic = CACHE_MAGIC;
    header.version = CACHE_VERSION;
    header.bytecode_size = 10;
    header.checksum = 0xDEADBEEF; // Wrong checksum

    try std.testing.expect(!validateCache(&bad_data));
}

// ============================================================================
// Constant Serialization Tests
// ============================================================================

test "constant serialization - integers" {
    const allocator = std.testing.allocator;

    // Create test constants
    const constants = [_]value.JSValue{
        value.JSValue.fromInt(0),
        value.JSValue.fromInt(42),
        value.JSValue.fromInt(-123),
        value.JSValue.fromInt(std.math.maxInt(i31)),
    };

    // Serialize
    var buffer: [1024]u8 = undefined;
    var writer = SliceWriter{ .buffer = &buffer };
    try serializeConstants(&constants, &writer, allocator);

    // Deserialize
    var reader = SliceReader{ .data = writer.getWritten() };
    const restored = try deserializeConstants(&reader, allocator, null);
    defer allocator.free(restored);

    // Verify
    try std.testing.expectEqual(constants.len, restored.len);
    for (constants, restored) |orig, rest| {
        try std.testing.expectEqual(orig.getInt(), rest.getInt());
    }
}

test "constant serialization - floats" {
    const allocator = std.testing.allocator;

    // Create float box constants
    const float_box1 = try allocator.create(value.JSValue.Float64Box);
    defer allocator.destroy(float_box1);
    float_box1.* = .{
        .header = heap.MemBlockHeader.init(.float64, @sizeOf(value.JSValue.Float64Box)),
        ._pad = 0,
        .value = 3.14159,
    };

    const float_box2 = try allocator.create(value.JSValue.Float64Box);
    defer allocator.destroy(float_box2);
    float_box2.* = .{
        .header = heap.MemBlockHeader.init(.float64, @sizeOf(value.JSValue.Float64Box)),
        ._pad = 0,
        .value = -2.71828,
    };

    const constants = [_]value.JSValue{
        value.JSValue.fromPtr(float_box1),
        value.JSValue.fromPtr(float_box2),
    };

    // Serialize
    var buffer: [1024]u8 = undefined;
    var writer = SliceWriter{ .buffer = &buffer };
    try serializeConstants(&constants, &writer, allocator);

    // Deserialize
    var reader = SliceReader{ .data = writer.getWritten() };
    const restored = try deserializeConstants(&reader, allocator, null);
    defer {
        for (restored) |v| {
            if (v.isPtr()) {
                const box = v.toPtr(value.JSValue.Float64Box);
                allocator.destroy(box);
            }
        }
        allocator.free(restored);
    }

    // Verify
    try std.testing.expectEqual(constants.len, restored.len);
    try std.testing.expectApproxEqAbs(@as(f64, 3.14159), restored[0].toNumber() orelse 0.0, 0.00001);
    try std.testing.expectApproxEqAbs(@as(f64, -2.71828), restored[1].toNumber() orelse 0.0, 0.00001);
}

test "constant serialization - strings" {
    const allocator = std.testing.allocator;

    // Create string constants
    const str1 = try string.createString(allocator, "hello");
    defer string.freeString(allocator, str1);
    const str2 = try string.createString(allocator, "world");
    defer string.freeString(allocator, str2);
    const str3 = try string.createString(allocator, "");
    defer string.freeString(allocator, str3);

    const constants = [_]value.JSValue{
        value.JSValue.fromPtr(str1),
        value.JSValue.fromPtr(str2),
        value.JSValue.fromPtr(str3),
    };

    // Serialize
    var buffer: [1024]u8 = undefined;
    var writer = SliceWriter{ .buffer = &buffer };
    try serializeConstants(&constants, &writer, allocator);

    // Deserialize
    var reader = SliceReader{ .data = writer.getWritten() };
    const restored = try deserializeConstants(&reader, allocator, null);
    defer {
        for (restored) |v| {
            if (v.isString()) {
                string.freeString(allocator, v.toPtr(string.JSString));
            }
        }
        allocator.free(restored);
    }

    // Verify
    try std.testing.expectEqual(constants.len, restored.len);
    try std.testing.expectEqualStrings("hello", restored[0].toPtr(string.JSString).data());
    try std.testing.expectEqualStrings("world", restored[1].toPtr(string.JSString).data());
    try std.testing.expectEqualStrings("", restored[2].toPtr(string.JSString).data());
}

test "constant serialization - special values" {
    const allocator = std.testing.allocator;

    const constants = [_]value.JSValue{
        value.JSValue.null_val,
        value.JSValue.undefined_val,
        value.JSValue.true_val,
        value.JSValue.false_val,
    };

    // Serialize
    var buffer: [1024]u8 = undefined;
    var writer = SliceWriter{ .buffer = &buffer };
    try serializeConstants(&constants, &writer, allocator);

    // Deserialize
    var reader = SliceReader{ .data = writer.getWritten() };
    const restored = try deserializeConstants(&reader, allocator, null);
    defer allocator.free(restored);

    // Verify
    try std.testing.expectEqual(constants.len, restored.len);
    try std.testing.expect(restored[0].isNull());
    try std.testing.expect(restored[1].isUndefined());
    try std.testing.expect(restored[2].isTrue());
    try std.testing.expect(restored[3].isFalse());
}

test "constant serialization - mixed types" {
    const allocator = std.testing.allocator;

    // Create mixed constants
    const float_box = try allocator.create(value.JSValue.Float64Box);
    defer allocator.destroy(float_box);
    float_box.* = .{
        .header = heap.MemBlockHeader.init(.float64, @sizeOf(value.JSValue.Float64Box)),
        ._pad = 0,
        .value = 99.9,
    };

    const str = try string.createString(allocator, "test");
    defer string.freeString(allocator, str);

    const constants = [_]value.JSValue{
        value.JSValue.fromInt(42),
        value.JSValue.null_val,
        value.JSValue.fromPtr(float_box),
        value.JSValue.fromPtr(str),
        value.JSValue.true_val,
    };

    // Serialize
    var buffer: [1024]u8 = undefined;
    var writer = SliceWriter{ .buffer = &buffer };
    try serializeConstants(&constants, &writer, allocator);

    // Deserialize
    var reader = SliceReader{ .data = writer.getWritten() };
    const restored = try deserializeConstants(&reader, allocator, null);
    defer {
        for (restored) |v| {
            if (v.isFloat64()) {
                allocator.destroy(v.toPtr(value.JSValue.Float64Box));
            } else if (v.isString()) {
                string.freeString(allocator, v.toPtr(string.JSString));
            }
        }
        allocator.free(restored);
    }

    // Verify
    try std.testing.expectEqual(@as(usize, 5), restored.len);
    try std.testing.expectEqual(@as(i32, 42), restored[0].getInt());
    try std.testing.expect(restored[1].isNull());
    try std.testing.expectApproxEqAbs(@as(f64, 99.9), restored[2].toNumber() orelse 0.0, 0.001);
    try std.testing.expectEqualStrings("test", restored[3].toPtr(string.JSString).data());
    try std.testing.expect(restored[4].isTrue());
}
