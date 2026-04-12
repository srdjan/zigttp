const std = @import("std");

pub fn writeFile(
    writer: *std.Io.Writer,
    path: []const u8,
    mode: u32,
    mtime: i64,
    bytes: []const u8,
) !void {
    if (path.len == 0 or path.len > 100) return error.InvalidTarPath;

    var header: [512]u8 = @splat(0);
    @memcpy(header[0..path.len], path);
    try writeOctal(header[100..108], mode);
    try writeOctal(header[108..116], 0);
    try writeOctal(header[116..124], 0);
    try writeOctal(header[124..136], bytes.len);
    try writeOctal(header[136..148], @as(u64, @intCast(@max(mtime, 0))));
    @memset(header[148..156], ' ');
    header[156] = '0';
    @memcpy(header[257..263], "ustar\x00");
    @memcpy(header[263..265], "00");

    const checksum = blk: {
        var total: u32 = 0;
        for (header) |byte| total += byte;
        break :blk total;
    };
    try writeChecksum(header[148..156], checksum);

    try writer.writeAll(&header);
    try writer.writeAll(bytes);
    const padding = (512 - (bytes.len % 512)) % 512;
    if (padding > 0) {
        var zeroes: [512]u8 = @splat(0);
        try writer.writeAll(zeroes[0..padding]);
    }
}

pub fn finish(writer: *std.Io.Writer) !void {
    var zeroes: [1024]u8 = @splat(0);
    try writer.writeAll(&zeroes);
}

fn writeOctal(dest: []u8, value: u64) !void {
    if (dest.len < 2) return error.InvalidTarField;
    @memset(dest, 0);
    var buf: [32]u8 = undefined;
    const rendered = try std.fmt.bufPrint(&buf, "{o}", .{value});
    if (rendered.len + 1 > dest.len) return error.InvalidTarField;
    const start = dest.len - rendered.len - 1;
    @memset(dest[0..start], '0');
    @memcpy(dest[start .. start + rendered.len], rendered);
}

fn writeChecksum(dest: []u8, value: u32) !void {
    if (dest.len != 8) return error.InvalidTarField;
    @memset(dest, 0);
    var buf: [16]u8 = undefined;
    const rendered = try std.fmt.bufPrint(&buf, "{o}", .{value});
    if (rendered.len + 2 > dest.len) return error.InvalidTarField;
    const start = dest.len - rendered.len - 2;
    @memset(dest[0..start], '0');
    @memcpy(dest[start .. start + rendered.len], rendered);
    dest[dest.len - 2] = 0;
    dest[dest.len - 1] = ' ';
}

test "tar writer emits header and trailer" {
    var aw: std.Io.Writer.Allocating = try .initCapacity(std.testing.allocator, 2048);
    defer aw.deinit();
    try writeFile(&aw.writer, "zigttp-handler", 0o755, 0, "hello");
    try finish(&aw.writer);
    const bytes = try aw.toOwnedSlice();
    defer std.testing.allocator.free(bytes);

    try std.testing.expectEqualStrings("zigttp-handler", std.mem.sliceTo(bytes[0..100], 0));
    try std.testing.expectEqual(@as(u8, 0), bytes[bytes.len - 1]);
    try std.testing.expectEqual(@as(usize, 0), bytes.len % 512);
}
