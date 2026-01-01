//! Bytecode Serialization and Caching
//!
//! Enables content-addressable bytecode caching for faster FaaS cold starts.
//! Uses SHA-256 for source hashing and supports both in-memory and file-based caching.

const std = @import("std");
const bytecode = @import("bytecode.zig");

/// Cache key derived from source content
pub const CacheKey = [32]u8;

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

/// Simple slice writer for testing
const SliceWriter = struct {
    buffer: []u8,
    pos: usize = 0,

    pub fn writeAll(self: *SliceWriter, data: []const u8) !void {
        if (self.pos + data.len > self.buffer.len) return error.NoSpaceLeft;
        @memcpy(self.buffer[self.pos..][0..data.len], data);
        self.pos += data.len;
    }

    pub fn getWritten(self: *const SliceWriter) []const u8 {
        return self.buffer[0..self.pos];
    }
};

/// Simple slice reader for testing
const SliceReader = struct {
    data: []const u8,
    pos: usize = 0,

    pub fn readBytesNoEof(self: *SliceReader, comptime n: usize) ![n]u8 {
        if (self.pos + n > self.data.len) return error.EndOfStream;
        const result: *const [n]u8 = @ptrCast(self.data[self.pos..][0..n]);
        self.pos += n;
        return result.*;
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
