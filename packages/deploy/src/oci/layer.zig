const std = @import("std");
const tar = @import("tar.zig");

pub const File = struct {
    path: []const u8,
    mode: u32,
    mtime: i64 = 0,
    bytes: []const u8,
};

pub const Layer = struct {
    tar_bytes: []u8,
    gzip_bytes: []u8,
    diff_id: []const u8,
    digest: []const u8,
    size: usize,

    pub fn deinit(self: *Layer, allocator: std.mem.Allocator) void {
        allocator.free(self.tar_bytes);
        allocator.free(self.gzip_bytes);
        allocator.free(self.diff_id);
        allocator.free(self.digest);
    }
};

pub fn buildLayer(allocator: std.mem.Allocator, files: []const File) !Layer {
    var tar_writer: std.Io.Writer.Allocating = .init(allocator);
    defer tar_writer.deinit();
    for (files) |file| {
        try tar.writeFile(&tar_writer.writer, file.path, file.mode, file.mtime, file.bytes);
    }
    try tar.finish(&tar_writer.writer);
    const tar_bytes = try tar_writer.toOwnedSlice();
    errdefer allocator.free(tar_bytes);

    var gzip_writer = try std.Io.Writer.Allocating.initCapacity(allocator, tar_bytes.len / 2 + 128);
    defer gzip_writer.deinit();
    var history: [std.compress.flate.max_window_len]u8 = undefined;
    var compressor = try std.compress.flate.Compress.init(
        &gzip_writer.writer,
        &history,
        .gzip,
        std.compress.flate.Compress.Options.default,
    );
    try compressor.writer.writeAll(tar_bytes);
    try compressor.finish();
    const gzip_bytes = try gzip_writer.toOwnedSlice();
    errdefer allocator.free(gzip_bytes);

    return .{
        .tar_bytes = tar_bytes,
        .gzip_bytes = gzip_bytes,
        .diff_id = try sha256DigestString(allocator, tar_bytes),
        .digest = try sha256DigestString(allocator, gzip_bytes),
        .size = gzip_bytes.len,
    };
}

fn sha256DigestString(allocator: std.mem.Allocator, bytes: []const u8) ![]const u8 {
    var digest: [std.crypto.hash.sha2.Sha256.digest_length]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(bytes, &digest, .{});
    return std.fmt.allocPrint(allocator, "sha256:{s}", .{std.fmt.bytesToHex(digest, .lower)});
}

test "layer digests match payloads" {
    var layer = try buildLayer(std.testing.allocator, &.{
        .{ .path = "zigttp-handler", .mode = 0o755, .bytes = "binary" },
    });
    defer layer.deinit(std.testing.allocator);

    try std.testing.expect(std.mem.startsWith(u8, layer.diff_id, "sha256:"));
    try std.testing.expect(std.mem.startsWith(u8, layer.digest, "sha256:"));
}
