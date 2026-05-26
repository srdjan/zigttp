const std = @import("std");
const config = @import("config.zig");

pub const Blob = struct {
    bytes: []u8,
    digest: []const u8,

    pub fn deinit(self: *Blob, allocator: std.mem.Allocator) void {
        allocator.free(self.bytes);
        allocator.free(self.digest);
    }
};

pub fn build(
    allocator: std.mem.Allocator,
    config_blob: *const config.Blob,
    layer_digest: []const u8,
    layer_size: usize,
) !Blob {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("schemaVersion");
    try json.write(@as(u32, 2));
    try json.objectField("mediaType");
    try json.write("application/vnd.oci.image.manifest.v1+json");
    try json.objectField("config");
    try writeDescriptor(&json, "application/vnd.oci.image.config.v1+json", config_blob.digest, config_blob.bytes.len);
    try json.objectField("layers");
    try json.beginArray();
    try writeDescriptor(&json, "application/vnd.oci.image.layer.v1.tar+gzip", layer_digest, layer_size);
    try json.endArray();
    try json.endObject();
    const bytes = try aw.toOwnedSlice();
    errdefer allocator.free(bytes);
    return .{
        .bytes = bytes,
        .digest = try sha256DigestString(allocator, bytes),
    };
}

fn writeDescriptor(json: *std.json.Stringify, media_type: []const u8, digest: []const u8, size: usize) !void {
    try json.beginObject();
    try json.objectField("mediaType");
    try json.write(media_type);
    try json.objectField("digest");
    try json.write(digest);
    try json.objectField("size");
    try json.write(size);
    try json.endObject();
}

fn sha256DigestString(allocator: std.mem.Allocator, bytes: []const u8) ![]const u8 {
    var digest: [std.crypto.hash.sha2.Sha256.digest_length]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(bytes, &digest, .{});
    return std.fmt.allocPrint(allocator, "sha256:{s}", .{std.fmt.bytesToHex(digest, .lower)});
}
