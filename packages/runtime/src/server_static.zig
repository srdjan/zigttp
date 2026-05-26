//! Static file serving primitives extracted from server.zig.
//!
//! Pure helpers — no Server / ConnectionPool coupling. Cover path-safety
//! validation, content-type inference, canonical-root containment, and the
//! StaticFile lookup result type. The StaticFileCache (LRU + eviction)
//! stays in server.zig because it embeds a per-Server mutex and lives on
//! the server struct.

const std = @import("std");
const Io = std.Io;
const Dir = std.Io.Dir;
const response_mod = @import("server_response.zig");

pub const StaticFileLookup = union(enum) {
    forbidden,
    not_found,
    ok: StaticFile,
};

pub const StaticFile = struct {
    file: Io.File,
    full_path: []const u8,
    size: usize,
    mtime: Io.Timestamp,
    content_type: []const u8,
    etag: []const u8,
};

pub fn resolveStaticFile(
    allocator: std.mem.Allocator,
    io: Io,
    static_dir: []const u8,
    path: []const u8,
    path_buf: *[std.fs.max_path_bytes]u8,
    etag_buf: *[34]u8,
) !StaticFileLookup {
    if (!isPathSafe(path)) return .forbidden;

    const full_path = std.fmt.bufPrint(path_buf, "{s}/{s}", .{ static_dir, path }) catch return error.PathTooLong;
    if (!isCanonicalPathInsideRoot(allocator, io, static_dir, full_path)) return .forbidden;

    const file = Dir.openFile(Dir.cwd(), io, full_path, .{ .follow_symlinks = false }) catch return .not_found;
    errdefer file.close(io);

    const stat = try file.stat(io);
    const size: usize = std.math.cast(usize, stat.size) orelse return error.FileTooLarge;

    return .{ .ok = .{
        .file = file,
        .full_path = full_path,
        .size = size,
        .mtime = stat.mtime,
        .content_type = getContentType(path),
        .etag = response_mod.formatETag(stat.mtime, stat.size, etag_buf),
    } };
}

/// Validate that a path is safe (no traversal, no absolute paths)
pub fn isPathSafe(path: []const u8) bool {
    // Reject empty paths
    if (path.len == 0) return false;

    // Reject NUL bytes anywhere in the path. Matches the CRLF/NUL guard
    // for header names/values (commit 6e65bb7); a NUL byte is never
    // legitimately part of a static-file URL path.
    if (std.mem.indexOfScalar(u8, path, 0) != null) return false;

    // Reject absolute paths (Unix and Windows)
    if (path[0] == '/' or path[0] == '\\') return false;

    // Reject Windows drive letters (C:, D:, etc.)
    if (path.len >= 2 and path[1] == ':') return false;

    // Check each path component for traversal
    var iter = std.mem.splitAny(u8, path, "/\\");
    while (iter.next()) |component| {
        // Reject ".." components (parent directory traversal)
        if (std.mem.eql(u8, component, "..")) return false;
        // Reject "." components (current directory - can be used in attacks)
        if (std.mem.eql(u8, component, ".")) return false;
    }

    return true;
}

pub fn getContentType(path: []const u8) []const u8 {
    const ext = std.fs.path.extension(path);
    if (std.mem.eql(u8, ext, ".html") or std.mem.eql(u8, ext, ".htm")) return "text/html; charset=utf-8";
    if (std.mem.eql(u8, ext, ".css")) return "text/css; charset=utf-8";
    if (std.mem.eql(u8, ext, ".js")) return "application/javascript; charset=utf-8";
    if (std.mem.eql(u8, ext, ".json")) return "application/json";
    if (std.mem.eql(u8, ext, ".png")) return "image/png";
    if (std.mem.eql(u8, ext, ".jpg") or std.mem.eql(u8, ext, ".jpeg")) return "image/jpeg";
    if (std.mem.eql(u8, ext, ".gif")) return "image/gif";
    if (std.mem.eql(u8, ext, ".svg")) return "image/svg+xml";
    if (std.mem.eql(u8, ext, ".ico")) return "image/x-icon";
    if (std.mem.eql(u8, ext, ".woff")) return "font/woff";
    if (std.mem.eql(u8, ext, ".woff2")) return "font/woff2";
    if (std.mem.eql(u8, ext, ".ttf")) return "font/ttf";
    if (std.mem.eql(u8, ext, ".txt")) return "text/plain; charset=utf-8";
    if (std.mem.eql(u8, ext, ".xml")) return "application/xml";
    if (std.mem.eql(u8, ext, ".pdf")) return "application/pdf";
    return "application/octet-stream";
}

pub fn isCanonicalPathInsideRoot(allocator: std.mem.Allocator, io: Io, static_root: []const u8, full_path: []const u8) bool {
    const root_path = Dir.realPathFileAlloc(Dir.cwd(), io, static_root, allocator) catch return false;
    defer allocator.free(root_path);
    const candidate_path = Dir.realPathFileAlloc(Dir.cwd(), io, full_path, allocator) catch return false;
    defer allocator.free(candidate_path);

    const root: []const u8 = root_path;
    const candidate: []const u8 = candidate_path;
    const root_norm = if (root.len > 1) std.mem.trimEnd(u8, root, "/\\") else root;
    if (!std.mem.startsWith(u8, candidate, root_norm)) return false;
    if (candidate.len == root_norm.len) return true;
    const boundary = candidate[root_norm.len];
    return boundary == '/' or boundary == '\\';
}

// -------------------------------------------------------------------------
// Tests (moved from server.zig)
// -------------------------------------------------------------------------

test "get content type" {
    try std.testing.expectEqualStrings("text/html; charset=utf-8", getContentType("index.html"));
    try std.testing.expectEqualStrings("application/javascript; charset=utf-8", getContentType("app.js"));
    try std.testing.expectEqualStrings("application/json", getContentType("data.json"));
    try std.testing.expectEqualStrings("image/png", getContentType("logo.png"));
}

test "path safety validation" {
    // Safe paths
    try std.testing.expect(isPathSafe("index.html"));
    try std.testing.expect(isPathSafe("static/images/logo.png"));
    try std.testing.expect(isPathSafe("assets/css/style.css"));

    // Unsafe: directory traversal
    try std.testing.expect(!isPathSafe("../etc/passwd"));
    try std.testing.expect(!isPathSafe("foo/../bar"));
    try std.testing.expect(!isPathSafe(".."));

    // Unsafe: absolute paths
    try std.testing.expect(!isPathSafe("/etc/passwd"));
    try std.testing.expect(!isPathSafe("\\Windows\\System32"));

    // Unsafe: Windows drive letters
    try std.testing.expect(!isPathSafe("C:\\Windows"));
    try std.testing.expect(!isPathSafe("D:file.txt"));

    // Unsafe: current directory reference
    try std.testing.expect(!isPathSafe("."));
    try std.testing.expect(!isPathSafe("./hidden"));

    // Unsafe: empty path
    try std.testing.expect(!isPathSafe(""));

    // Unsafe: NUL byte anywhere in the path (matches CRLF/NUL header guard
    // from commit 6e65bb7; defense in depth at the filesystem boundary).
    try std.testing.expect(!isPathSafe("ok\x00/etc/passwd"));

    // Unsafe: mixed separators with traversal
    try std.testing.expect(!isPathSafe("foo\\..\\bar"));
    try std.testing.expect(!isPathSafe("..\\windows"));

    // Unsafe: traversal at non-leading positions
    try std.testing.expect(!isPathSafe("a/b/../../etc/passwd"));
    try std.testing.expect(!isPathSafe("a/b/c/.."));

    // Note on URL encoding: `..%2f..%2fetc/passwd` is NOT caught here
    // because isPathSafe splits on the literal `/` and `\\` only, and
    // %2f survives as a literal sub-string. The defense in depth is
    // isCanonicalPathInsideRoot + openFile resolving the literal path
    // name `..%2fetc..`, which does not exist on disk. This test pins
    // the current contract: pre-decoded traversal must be caught here,
    // post-decoded traversal relies on the filesystem layer.
    try std.testing.expect(isPathSafe("..%2fetc/passwd"));
}
