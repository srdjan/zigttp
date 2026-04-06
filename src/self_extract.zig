const std = @import("std");
const builtin = @import("builtin");
const zigts = @import("zigts");
const handler_policy = zigts.handler_policy;

// -- Trailer format (32 bytes, little-endian, at end of file) --
//
// offset  size  field
// 0       8     payload_offset
// 8       8     payload_size
// 16      2     format_version
// 18      2     flags (bit 0: has_contract)
// 20      4     checksum (CRC-32 of payload)
// 24      8     magic

pub const MAGIC: u64 = 0x5A54_5042_4331_0000; // "ZTPBC1\0\0"
pub const FORMAT_VERSION: u16 = 1;
pub const TRAILER_SIZE: usize = 32;

// Section types in the payload
pub const Section = enum(u8) {
    bytecode = 1,
    deps = 2,
    contract = 3,
    policy = 4,
    metadata = 5,
};

pub const Payload = struct {
    bytecode: []const u8,
    dep_bytecodes: []const []const u8,
    contract_json: ?[]const u8,
    policy: handler_policy.RuntimePolicy,
    // Owned string slices backing the policy allow lists
    policy_strings: []const []const u8,

    pub fn deinit(self: *const Payload, allocator: std.mem.Allocator) void {
        allocator.free(self.bytecode);
        for (self.dep_bytecodes) |dep| allocator.free(dep);
        allocator.free(self.dep_bytecodes);
        if (self.contract_json) |c| allocator.free(c);
        // Free the values arrays inside each policy allow list
        if (self.policy.env.values.len > 0) allocator.free(self.policy.env.values);
        if (self.policy.egress.values.len > 0) allocator.free(self.policy.egress.values);
        if (self.policy.cache.values.len > 0) allocator.free(self.policy.cache.values);
        if (self.policy.sql.values.len > 0) allocator.free(self.policy.sql.values);
        // Free individual string contents
        for (self.policy_strings) |s| allocator.free(s);
        allocator.free(self.policy_strings);
    }
};

// -- Detection: read own executable, check for appended payload --

pub fn detect(allocator: std.mem.Allocator) !?Payload {
    const self_path = try getSelfExePath(allocator);
    defer allocator.free(self_path);

    const self_path_z = try allocator.dupeZ(u8, self_path);
    defer allocator.free(self_path_z);

    const fd = std.c.open(self_path_z, .{ .ACCMODE = .RDONLY }, @as(std.c.mode_t, 0));
    if (fd < 0) return null;
    defer _ = std.c.close(fd);

    // Get file size via fstat
    const file_size = getFileSize(fd) orelse return null;
    if (file_size < TRAILER_SIZE) return null;

    // Read trailer from end of file
    var trailer_buf: [TRAILER_SIZE]u8 = undefined;
    const trailer_offset: i64 = @intCast(file_size - TRAILER_SIZE);
    const bytes_read = std.c.pread(fd, &trailer_buf, TRAILER_SIZE, trailer_offset);
    if (bytes_read != TRAILER_SIZE) return null;

    // Parse trailer
    const magic = std.mem.readInt(u64, trailer_buf[24..32], .little);
    if (magic != MAGIC) return null;

    const payload_offset = std.mem.readInt(u64, trailer_buf[0..8], .little);
    const payload_size = std.mem.readInt(u64, trailer_buf[8..16], .little);
    const version = std.mem.readInt(u16, trailer_buf[16..18], .little);
    const checksum_expected = std.mem.readInt(u32, trailer_buf[20..24], .little);

    if (version > FORMAT_VERSION) return null;
    if (payload_offset + payload_size + TRAILER_SIZE != file_size) return null;
    if (payload_size > 100 * 1024 * 1024) return null; // 100MB sanity limit

    // Read payload
    const payload_data = try allocator.alloc(u8, @intCast(payload_size));
    defer allocator.free(payload_data);

    const payload_read = std.c.pread(fd, payload_data.ptr, payload_data.len, @intCast(payload_offset));
    if (payload_read < 0 or @as(usize, @intCast(payload_read)) != payload_data.len) return null;

    // Verify checksum
    const checksum_actual = std.hash.crc.Crc32.hash(payload_data);
    if (checksum_actual != checksum_expected) return null;

    // Parse sections
    return parsePayload(allocator, payload_data);
}

// -- Creation: copy base binary, append payload + trailer --

pub fn create(
    allocator: std.mem.Allocator,
    base_binary_path: []const u8,
    output_path: []const u8,
    bytecode: []const u8,
    dep_bytecodes: []const []const u8,
    contract_json: ?[]const u8,
    policy: *const handler_policy.RuntimePolicy,
) !void {
    // Read base binary (100MB limit matches payload sanity check)
    const base_data = try readFile(allocator, base_binary_path, 100 * 1024 * 1024);
    defer allocator.free(base_data);

    // Strip any existing trailer from the base binary (nested compile)
    const clean_size = getCleanBinarySize(base_data);

    // Serialize payload
    const payload = try serializePayload(allocator, bytecode, dep_bytecodes, contract_json, policy);
    defer allocator.free(payload);

    // Build trailer
    const payload_offset: u64 = @intCast(clean_size);
    const payload_size: u64 = @intCast(payload.len);
    const checksum = std.hash.crc.Crc32.hash(payload);

    const flags: u16 = if (contract_json != null) 1 else 0;

    var trailer: [TRAILER_SIZE]u8 = undefined;
    std.mem.writeInt(u64, trailer[0..8], payload_offset, .little);
    std.mem.writeInt(u64, trailer[8..16], payload_size, .little);
    std.mem.writeInt(u16, trailer[16..18], FORMAT_VERSION, .little);
    std.mem.writeInt(u16, trailer[18..20], flags, .little);
    std.mem.writeInt(u32, trailer[20..24], checksum, .little);
    std.mem.writeInt(u64, trailer[24..32], MAGIC, .little);

    // Write output: base binary + payload + trailer
    const output_path_z = try allocator.dupeZ(u8, output_path);
    defer allocator.free(output_path_z);

    const out_fd = std.c.open(output_path_z, .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true }, @as(std.c.mode_t, 0o755));
    if (out_fd < 0) return error.OpenFailed;
    defer _ = std.c.close(out_fd);

    try writeAll(out_fd, base_data[0..clean_size]);
    try writeAll(out_fd, payload);
    try writeAll(out_fd, &trailer);
}

/// Return the size of the binary without any appended payload.
/// If no trailer is found, returns the full length.
pub fn getCleanBinarySize(data: []const u8) usize {
    if (data.len < TRAILER_SIZE) return data.len;
    const trailer_start = data.len - TRAILER_SIZE;
    const magic = std.mem.readInt(u64, data[trailer_start + 24 ..][0..8], .little);
    if (magic != MAGIC) return data.len;

    const payload_offset = std.mem.readInt(u64, data[trailer_start..][0..8], .little);
    const payload_size = std.mem.readInt(u64, data[trailer_start + 8 ..][0..8], .little);

    if (payload_offset + payload_size + TRAILER_SIZE == data.len) {
        return @intCast(payload_offset);
    }
    return data.len;
}

// -- Payload serialization --

fn serializePayload(
    allocator: std.mem.Allocator,
    bytecode: []const u8,
    dep_bytecodes: []const []const u8,
    contract_json: ?[]const u8,
    policy: *const handler_policy.RuntimePolicy,
) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(allocator);

    // Count sections
    var section_count: u16 = 1; // bytecode always present
    if (dep_bytecodes.len > 0) section_count += 1;
    if (contract_json != null) section_count += 1;
    section_count += 1; // policy always present

    try buf.ensureTotalCapacity(allocator, bytecode.len + 256);
    try writeU16(&buf, allocator, section_count);

    // Section 1: bytecode
    try writeSection(&buf, allocator, .bytecode, bytecode);

    // Section 2: deps (if any)
    if (dep_bytecodes.len > 0) {
        var dep_buf: std.ArrayList(u8) = .empty;
        defer dep_buf.deinit(allocator);
        try writeU16(&dep_buf, allocator, @intCast(dep_bytecodes.len));
        for (dep_bytecodes) |dep| {
            try writeU32(&dep_buf, allocator, @intCast(dep.len));
            try dep_buf.appendSlice(allocator, dep);
        }
        try writeSection(&buf, allocator, .deps, dep_buf.items);
    }

    // Section 3: contract (if any)
    if (contract_json) |json| {
        try writeSection(&buf, allocator, .contract, json);
    }

    // Section 4: policy
    const policy_data = try serializePolicy(allocator, policy);
    defer allocator.free(policy_data);
    try writeSection(&buf, allocator, .policy, policy_data);

    return buf.toOwnedSlice(allocator);
}

fn parsePayload(allocator: std.mem.Allocator, data: []const u8) !?Payload {
    var pos: usize = 0;
    if (data.len < 2) return null;

    const section_count = readU16(data, &pos);

    var bytecode: ?[]const u8 = null;
    var dep_bytecodes: ?[]const []const u8 = null;
    var contract_json: ?[]const u8 = null;
    var policy: handler_policy.RuntimePolicy = .{};
    var policy_strings: std.ArrayList([]const u8) = .empty;
    errdefer {
        if (bytecode) |b| allocator.free(b);
        if (dep_bytecodes) |deps| {
            for (deps) |d| allocator.free(d);
            allocator.free(deps);
        }
        if (contract_json) |c| allocator.free(c);
        for (policy_strings.items) |s| allocator.free(s);
        policy_strings.deinit(allocator);
    }

    var i: u16 = 0;
    while (i < section_count) : (i += 1) {
        if (pos + 5 > data.len) return null;

        const section_type = data[pos];
        pos += 1;
        const section_size = readU32(data, &pos);
        if (pos + section_size > data.len) return null;
        const section_data = data[pos .. pos + section_size];
        pos += section_size;

        switch (section_type) {
            @intFromEnum(Section.bytecode) => {
                bytecode = try allocator.dupe(u8, section_data);
            },
            @intFromEnum(Section.deps) => {
                dep_bytecodes = try parseDeps(allocator, section_data);
            },
            @intFromEnum(Section.contract) => {
                contract_json = try allocator.dupe(u8, section_data);
            },
            @intFromEnum(Section.policy) => {
                policy = try deserializePolicy(allocator, section_data, &policy_strings);
            },
            else => {}, // skip unknown sections for forward compatibility
        }
    }

    if (bytecode == null) return null;

    return .{
        .bytecode = bytecode.?,
        .dep_bytecodes = dep_bytecodes orelse &.{},
        .contract_json = contract_json,
        .policy = policy,
        .policy_strings = try policy_strings.toOwnedSlice(allocator),
    };
}

fn parseDeps(allocator: std.mem.Allocator, data: []const u8) ![]const []const u8 {
    var pos: usize = 0;
    if (data.len < 2) return &.{};

    const count = readU16(data, &pos);
    var deps = try allocator.alloc([]const u8, count);
    var filled: usize = 0;
    errdefer {
        for (deps[0..filled]) |d| allocator.free(d);
        allocator.free(deps);
    }

    while (filled < count) : (filled += 1) {
        const dep_size = readU32(data, &pos);
        if (pos + dep_size > data.len) return error.InvalidPayload;
        deps[filled] = try allocator.dupe(u8, data[pos .. pos + dep_size]);
        pos += dep_size;
    }

    return deps;
}

// -- Policy serialization --
// Format: for each of env, egress, cache, sql:
//   [1 byte: enabled] [2 bytes: count] [for each: 2 bytes len + bytes]

fn serializePolicy(allocator: std.mem.Allocator, policy: *const handler_policy.RuntimePolicy) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(allocator);

    try serializeAllowList(&buf, allocator, policy.env);
    try serializeAllowList(&buf, allocator, policy.egress);
    try serializeAllowList(&buf, allocator, policy.cache);
    try serializeAllowList(&buf, allocator, .{ .enabled = policy.sql.enabled, .values = policy.sql.values });

    return buf.toOwnedSlice(allocator);
}

fn serializeAllowList(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, list: handler_policy.RuntimeAllowList) !void {
    try buf.append(allocator, if (list.enabled) 1 else 0);
    try writeU16(buf, allocator, @intCast(list.values.len));
    for (list.values) |v| {
        try writeU16(buf, allocator, @intCast(v.len));
        try buf.appendSlice(allocator, v);
    }
}

fn deserializePolicy(
    allocator: std.mem.Allocator,
    data: []const u8,
    strings: *std.ArrayList([]const u8),
) !handler_policy.RuntimePolicy {
    var pos: usize = 0;
    const env = try deserializeAllowList(allocator, data, &pos, strings);
    const egress = try deserializeAllowList(allocator, data, &pos, strings);
    const cache = try deserializeAllowList(allocator, data, &pos, strings);
    const sql_base = try deserializeAllowList(allocator, data, &pos, strings);

    return .{
        .env = env,
        .egress = egress,
        .cache = cache,
        .sql = .{ .enabled = sql_base.enabled, .values = sql_base.values },
    };
}

fn deserializeAllowList(
    allocator: std.mem.Allocator,
    data: []const u8,
    pos: *usize,
    strings: *std.ArrayList([]const u8),
) !handler_policy.RuntimeAllowList {
    if (pos.* >= data.len) return .{};
    const enabled = data[pos.*] != 0;
    pos.* += 1;

    const count = readU16(data, pos);
    var values = try allocator.alloc([]const u8, count);
    var filled: usize = 0;
    errdefer {
        for (values[0..filled]) |v| allocator.free(v);
        allocator.free(values);
    }

    while (filled < count) : (filled += 1) {
        const len = readU16(data, pos);
        const s = try allocator.dupe(u8, data[pos.* .. pos.* + len]);
        try strings.append(allocator, s);
        values[filled] = s;
        pos.* += len;
    }

    return .{ .enabled = enabled, .values = values };
}

// -- Platform: get own executable path --

pub fn getSelfExePath(allocator: std.mem.Allocator) ![]u8 {
    if (builtin.os.tag == .macos) {
        var path_buf: [std.c.PATH_MAX + 1]u8 = undefined;
        var buf_size: u32 = @intCast(path_buf.len);
        const rc = std.c._NSGetExecutablePath(&path_buf, &buf_size);
        if (rc != 0) return error.NameTooLong;
        const symlink_path = std.mem.sliceTo(&path_buf, 0);
        // Resolve symlinks to get the actual binary path
        return try resolveRealPath(allocator, symlink_path);
    } else if (builtin.os.tag == .linux) {
        var path_buf: [std.c.PATH_MAX + 1]u8 = undefined;
        const len = try std.posix.readlinkatZ(std.posix.AT.FDCWD, "/proc/self/exe", &path_buf);
        return try allocator.dupe(u8, path_buf[0..len]);
    } else {
        return error.UnsupportedPlatform;
    }
}

fn resolveRealPath(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);
    var resolved_buf: [std.c.PATH_MAX + 1]u8 = undefined;
    const resolved = std.c.realpath(path_z, &resolved_buf) orelse return try allocator.dupe(u8, path);
    const len = std.mem.len(resolved);
    return try allocator.dupe(u8, resolved[0..len]);
}

// -- File I/O helpers --

fn getFileSize(fd: std.c.fd_t) ?u64 {
    var stat: std.c.Stat = undefined;
    if (std.c.fstat(fd, &stat) != 0) return null;
    return @intCast(stat.size);
}

const readFile = zigts.file_io.readFile;

fn writeAll(fd: std.c.fd_t, data: []const u8) !void {
    var total: usize = 0;
    while (total < data.len) {
        const result = std.c.write(fd, data[total..].ptr, data.len - total);
        if (result < 0) return error.WriteFailure;
        if (result == 0) return error.WriteFailure;
        total += @intCast(result);
    }
}

// -- Binary format helpers --

fn writeU16(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, value: u16) !void {
    var bytes: [2]u8 = undefined;
    std.mem.writeInt(u16, &bytes, value, .little);
    try buf.appendSlice(allocator, &bytes);
}

fn writeU32(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, value: u32) !void {
    var bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &bytes, value, .little);
    try buf.appendSlice(allocator, &bytes);
}

fn writeSection(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, section_type: Section, data: []const u8) !void {
    try buf.append(allocator, @intFromEnum(section_type));
    try writeU32(buf, allocator, @intCast(data.len));
    try buf.appendSlice(allocator, data);
}

fn readU16(data: []const u8, pos: *usize) u16 {
    const val = std.mem.readInt(u16, data[pos.*..][0..2], .little);
    pos.* += 2;
    return val;
}

fn readU32(data: []const u8, pos: *usize) usize {
    const val = std.mem.readInt(u32, data[pos.*..][0..4], .little);
    pos.* += 4;
    return @intCast(val);
}

// -- Tests --

test "roundtrip: serialize and parse payload" {
    const allocator = std.testing.allocator;

    const bytecode = "test bytecode data";
    const contract = "{\"routes\":[]}";
    const policy = handler_policy.RuntimePolicy{};

    const serialized = try serializePayload(
        allocator,
        bytecode,
        &.{},
        contract,
        &policy,
    );
    defer allocator.free(serialized);

    const parsed = (try parsePayload(allocator, serialized)).?;
    defer parsed.deinit(allocator);

    try std.testing.expectEqualStrings(bytecode, parsed.bytecode);
    try std.testing.expectEqualStrings(contract, parsed.contract_json.?);
    try std.testing.expectEqual(@as(usize, 0), parsed.dep_bytecodes.len);
}

test "roundtrip: payload with deps" {
    const allocator = std.testing.allocator;

    const bytecode = "entry";
    const dep1 = "dep_module_1";
    const dep2 = "dep_module_2";
    const deps = [_][]const u8{ dep1, dep2 };
    const policy = handler_policy.RuntimePolicy{};

    const serialized = try serializePayload(allocator, bytecode, &deps, null, &policy);
    defer allocator.free(serialized);

    const parsed = (try parsePayload(allocator, serialized)).?;
    defer parsed.deinit(allocator);

    try std.testing.expectEqualStrings(bytecode, parsed.bytecode);
    try std.testing.expectEqual(@as(usize, 2), parsed.dep_bytecodes.len);
    try std.testing.expectEqualStrings(dep1, parsed.dep_bytecodes[0]);
    try std.testing.expectEqualStrings(dep2, parsed.dep_bytecodes[1]);
    try std.testing.expect(parsed.contract_json == null);
}

test "getCleanBinarySize: no trailer returns full size" {
    const data = "just some binary data without a trailer";
    try std.testing.expectEqual(data.len, getCleanBinarySize(data));
}

test "getCleanBinarySize: with trailer returns clean offset" {
    const base = "base binary content";
    const payload = "payload data";

    // Build a fake file: base + payload + trailer
    var file_buf: [256]u8 = undefined;
    @memcpy(file_buf[0..base.len], base);
    @memcpy(file_buf[base.len .. base.len + payload.len], payload);

    const trailer_start = base.len + payload.len;
    std.mem.writeInt(u64, file_buf[trailer_start..][0..8], @intCast(base.len), .little);
    std.mem.writeInt(u64, file_buf[trailer_start + 8 ..][0..8], @intCast(payload.len), .little);
    std.mem.writeInt(u16, file_buf[trailer_start + 16 ..][0..2], FORMAT_VERSION, .little);
    std.mem.writeInt(u16, file_buf[trailer_start + 18 ..][0..2], 0, .little);
    std.mem.writeInt(u32, file_buf[trailer_start + 20 ..][0..4], 0, .little);
    std.mem.writeInt(u64, file_buf[trailer_start + 24 ..][0..8], MAGIC, .little);

    const total = trailer_start + TRAILER_SIZE;
    try std.testing.expectEqual(base.len, getCleanBinarySize(file_buf[0..total]));
}
