//! Binary-path resolution helpers for the developer CLI.
//!
//! Extracted from dev_cli.zig. `dev`, `studio`, `demo`, and `doctor` all
//! need to re-enter either the developer CLI or the runtime binary, so
//! they consult these helpers to figure out which path to use. Pure
//! functions over a program-path string and the cwd — no shared state.

const std = @import("std");

/// `dev` and `studio` need the developer CLI because live reload and Studio
/// are intentionally not linked into `zttp-runtime`. Re-entering the same
/// binary with `serve` is safe: `serve` dispatches directly to runtime_cli.
pub fn resolveDeveloperServeBinary(allocator: std.mem.Allocator, program_path: []const u8) ![]const u8 {
    if (program_path.len > 0) return try allocator.dupe(u8, program_path);
    return try allocator.dupe(u8, "zttp");
}

/// `zttp demo` changes into the generated workspace before re-entering the
/// CLI. Keep bare names bare so PATH lookup still works, but resolve
/// cwd-relative executable paths while the original cwd is still known.
pub fn resolveReentryBinaryAfterChdir(
    allocator: std.mem.Allocator,
    program_path: []const u8,
    original_cwd: []const u8,
) ![]const u8 {
    if (std.fs.path.isAbsolute(program_path)) return try allocator.dupe(u8, program_path);
    if (std.fs.path.dirname(program_path) == null) return try allocator.dupe(u8, program_path);
    return try std.fs.path.resolve(allocator, &.{ original_cwd, program_path });
}

/// Find the `zttp` runtime binary.
/// Strategy: adjacent to this CLI binary (same directory), then PATH fallback.
pub fn resolveRuntimeBinary(allocator: std.mem.Allocator, program_path: []const u8) ![]const u8 {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    // program_path may be absolute or cwd-relative depending on how the user
    // invoked us. Dir.access handles both, so we don't need to canonicalize.
    const dir_name = std.fs.path.dirname(program_path) orelse ".";
    const cwd = std.Io.Dir.cwd();
    const candidates = [_][]const u8{ "zttp-runtime", "zttp" };
    for (candidates) |name| {
        const candidate = try std.fs.path.join(allocator, &.{ dir_name, name });
        errdefer allocator.free(candidate);
        if (std.mem.eql(u8, candidate, program_path)) {
            allocator.free(candidate);
            continue;
        }
        cwd.access(io, candidate, .{}) catch {
            allocator.free(candidate);
            continue;
        };
        return candidate;
    }

    return try allocator.dupe(u8, "zttp-runtime");
}
