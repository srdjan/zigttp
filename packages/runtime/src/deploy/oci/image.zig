const std = @import("std");
const plan = @import("../plan.zig");
const layer = @import("layer.zig");
const config = @import("config.zig");
const manifest = @import("manifest.zig");

pub const OciImage = struct {
    config_blob: config.Blob,
    layer_blob: layer.Layer,
    manifest_blob: manifest.Blob,
    image_tag_ref: []const u8,
    image_digest_ref: []const u8,

    pub fn deinit(self: *OciImage, allocator: std.mem.Allocator) void {
        self.config_blob.deinit(allocator);
        self.layer_blob.deinit(allocator);
        self.manifest_blob.deinit(allocator);
        allocator.free(self.image_tag_ref);
        allocator.free(self.image_digest_ref);
    }
};

pub fn buildImage(
    allocator: std.mem.Allocator,
    registry: []const u8,
    tag: []const u8,
    arch: plan.Arch,
    binary: []const u8,
    labels: []const config.Label,
) !OciImage {
    var layer_blob = try layer.buildLayer(allocator, &.{
        .{ .path = "zigttp-handler", .mode = 0o755, .bytes = binary },
    });
    errdefer layer_blob.deinit(allocator);

    var config_blob = try config.build(allocator, arch, layer_blob.diff_id, labels);
    errdefer config_blob.deinit(allocator);

    var manifest_blob = try manifest.build(allocator, &config_blob, layer_blob.digest, layer_blob.size);
    errdefer manifest_blob.deinit(allocator);

    return .{
        .config_blob = config_blob,
        .layer_blob = layer_blob,
        .manifest_blob = manifest_blob,
        .image_tag_ref = try std.fmt.allocPrint(allocator, "{s}:{s}", .{ registry, tag }),
        .image_digest_ref = try std.fmt.allocPrint(allocator, "{s}@{s}", .{ registry, manifest_blob.digest }),
    };
}

test "image builder returns digest and tag refs" {
    var image = try buildImage(std.testing.allocator, "ghcr.io/acme/demo", "sha256-123456", .amd64, "binary", &.{});
    defer image.deinit(std.testing.allocator);

    try std.testing.expect(std.mem.endsWith(u8, image.image_tag_ref, ":sha256-123456"));
    try std.testing.expect(std.mem.indexOf(u8, image.image_digest_ref, "@sha256:") != null);
}
