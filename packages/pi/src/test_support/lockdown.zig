//! Lockdown seal test. Walks `packages/pi/src/` at test time and fails if
//! any `.zig` file contains a substring that would signal a runtime
//! extension-loader slipping in: external SYSTEM.md loaders, dynamic-
//! library loading, or a scan of `~/.zigttp/{skills,prompts,themes,
//! extensions,models.json}`.
//!
//! Rationale: the pi lockdown policy (docs: "One place, one file, one
//! rebuild.") relies on nothing being loaded at runtime that was not in
//! the source tree at build time. That's a class of invariant best
//! enforced in CI rather than by manual PR review.
//!
//! This test runs with cwd = project root (standard for `zig build test`).
//! It skips its own source so the needle literals below do not trigger
//! false positives. If the test itself moves, update `self_path`.

const std = @import("std");

const forbidden: []const []const u8 = &.{
    "~/.zigttp/skills",
    "~/.zigttp/prompts",
    "~/.zigttp/themes",
    "~/.zigttp/extensions",
    "~/.zigttp/models.json",
    "SYSTEM.md",
    "APPEND_SYSTEM.md",
    "dlopen",
    "dlsym",
    "LoadLibrary",
};

const scan_root = "packages/pi/src";
const self_path = "packages/pi/src/test_support/lockdown.zig";

const testing = std.testing;

test "lockdown: pi source tree contains no runtime-extension footprints" {
    const allocator = testing.allocator;

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var root = std.Io.Dir.cwd().openDir(io, scan_root, .{ .iterate = true }) catch |err| switch (err) {
        // A test cwd without `packages/pi/src/` means someone is running
        // tests from an unexpected location. Skip rather than spuriously
        // fail - a local-only tool misinvocation should not break CI.
        error.FileNotFound => return error.SkipZigTest,
        else => return err,
    };
    defer root.close(io);

    var walker = try root.walk(allocator);
    defer walker.deinit();

    while (try walker.next(io)) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.path, ".zig")) continue;

        const full_path = try std.fs.path.join(allocator, &.{ scan_root, entry.path });
        defer allocator.free(full_path);

        if (std.mem.eql(u8, full_path, self_path)) continue;

        const contents = try readFile(allocator, full_path);
        defer allocator.free(contents);

        for (forbidden) |needle| {
            if (std.mem.indexOf(u8, contents, needle)) |pos| {
                std.debug.print(
                    "\n[lockdown] forbidden substring \"{s}\" found in {s} at byte {d}\n",
                    .{ needle, full_path, pos },
                );
                return error.LockdownBreach;
            }
        }
    }
}

fn readFile(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = try std.posix.openatZ(std.posix.AT.FDCWD, path_z, .{ .ACCMODE = .RDONLY }, 0);
    defer std.Io.Threaded.closeFd(fd);

    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(allocator);
    var chunk: [4096]u8 = undefined;
    while (true) {
        const n = try std.posix.read(fd, &chunk);
        if (n == 0) break;
        try buf.appendSlice(allocator, chunk[0..n]);
    }
    return try buf.toOwnedSlice(allocator);
}
