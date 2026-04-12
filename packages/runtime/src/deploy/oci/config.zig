const std = @import("std");
const plan = @import("../plan.zig");

pub const Blob = struct {
    bytes: []u8,
    digest: []const u8,

    pub fn deinit(self: *Blob, allocator: std.mem.Allocator) void {
        allocator.free(self.bytes);
        allocator.free(self.digest);
    }
};

pub const Label = struct {
    key: []const u8,
    value: []const u8,
};

pub fn build(allocator: std.mem.Allocator, arch: plan.Arch, diff_id: []const u8, labels: []const Label) !Blob {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("architecture");
    try json.write(arch.ociArchitecture());
    try json.objectField("os");
    try json.write("linux");
    try json.objectField("config");
    try json.beginObject();
    try json.objectField("Entrypoint");
    try json.beginArray();
    const entrypoint = [_][]const u8{ "/zigttp-handler", "serve", "-q", "-h", "0.0.0.0", "-p", "3000" };
    for (&entrypoint) |item| try json.write(item);
    try json.endArray();
    if (labels.len > 0) {
        try json.objectField("Labels");
        try json.beginObject();
        for (labels) |label| {
            try json.objectField(label.key);
            try json.write(label.value);
        }
        try json.endObject();
    }
    try json.objectField("ExposedPorts");
    try json.beginObject();
    try json.objectField("3000/tcp");
    try json.beginObject();
    try json.endObject();
    try json.endObject();
    try json.endObject();
    try json.objectField("rootfs");
    try json.beginObject();
    try json.objectField("type");
    try json.write("layers");
    try json.objectField("diff_ids");
    try json.beginArray();
    try json.write(diff_id);
    try json.endArray();
    try json.endObject();
    try json.objectField("history");
    try json.beginArray();
    try json.beginObject();
    try json.objectField("created_by");
    try json.write("zigttp deploy");
    try json.endObject();
    try json.endArray();
    try json.endObject();
    const bytes = try aw.toOwnedSlice();
    errdefer allocator.free(bytes);
    return .{
        .bytes = bytes,
        .digest = try sha256DigestString(allocator, bytes),
    };
}

fn sha256DigestString(allocator: std.mem.Allocator, bytes: []const u8) ![]const u8 {
    var digest: [std.crypto.hash.sha2.Sha256.digest_length]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(bytes, &digest, .{});
    return std.fmt.allocPrint(allocator, "sha256:{s}", .{std.fmt.bytesToHex(digest, .lower)});
}
