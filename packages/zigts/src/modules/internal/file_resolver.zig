//! File Import Path Resolution
//!
//! Pure functions for resolving relative import specifiers to absolute file paths.
//! Handles extension probing (.ts, .tsx, .js, .jsx) and path normalization.

const std = @import("std");

/// Extensions to probe when an import has no extension
const probe_extensions = [_][]const u8{ ".ts", ".tsx", ".js", ".jsx" };

/// Maximum path length to prevent allocation issues
const MAX_PATH_LEN = 4096;

/// Resolve an import specifier relative to the importing file's directory.
///
/// Returns an owned absolute path string. Caller must free with the same allocator.
///
/// If the specifier already has a recognized extension, it's used directly.
/// Otherwise, tries .ts, .tsx, .js, .jsx in order using the provided `fileExists` callback.
/// If no callback is provided (null), the first probe extension (.ts) is used.
pub fn resolve(
    allocator: std.mem.Allocator,
    specifier: []const u8,
    importing_file_dir: []const u8,
    fileExists: ?*const fn (path: []const u8) bool,
) ![]const u8 {
    // Join the importing directory with the specifier
    const joined = try joinPath(allocator, importing_file_dir, specifier);
    defer allocator.free(joined);

    // Normalize the path (resolve .. and .)
    const normalized = try normalizePath(allocator, joined);

    // If the specifier already has a recognized extension, return as-is
    if (hasRecognizedExtension(specifier)) {
        return normalized;
    }

    // Probe extensions
    defer allocator.free(normalized);

    if (fileExists) |exists_fn| {
        for (probe_extensions) |ext| {
            const candidate = try std.mem.concat(allocator, u8, &.{ normalized, ext });
            if (exists_fn(candidate)) {
                return candidate;
            }
            allocator.free(candidate);
        }
        // No extension matched; return with .ts as default
        return try std.mem.concat(allocator, u8, &.{ normalized, ".ts" });
    }

    // No existence check available; default to .ts
    return try std.mem.concat(allocator, u8, &.{ normalized, ".ts" });
}

/// Extract the directory component of a file path.
/// Returns everything up to and including the last '/'.
/// If no '/' found, returns "./"
pub fn dirName(path: []const u8) []const u8 {
    if (std.mem.lastIndexOfScalar(u8, path, '/')) |idx| {
        return path[0 .. idx + 1];
    }
    return "./";
}

/// Check if a specifier has a recognized JS/TS extension
fn hasRecognizedExtension(specifier: []const u8) bool {
    inline for (probe_extensions) |ext| {
        if (std.mem.endsWith(u8, specifier, ext)) return true;
    }
    return false;
}

/// Join two path segments, handling trailing/leading slashes
fn joinPath(allocator: std.mem.Allocator, base: []const u8, rel: []const u8) ![]const u8 {
    if (rel.len == 0) return try allocator.dupe(u8, base);

    // Absolute path overrides base
    if (rel[0] == '/') return try allocator.dupe(u8, rel);

    // Strip leading "./" from relative path
    var clean_rel = rel;
    if (clean_rel.len >= 2 and clean_rel[0] == '.' and clean_rel[1] == '/') {
        clean_rel = clean_rel[2..];
    }

    // Ensure base ends with '/'
    if (base.len > 0 and base[base.len - 1] == '/') {
        return try std.mem.concat(allocator, u8, &.{ base, clean_rel });
    }
    return try std.mem.concat(allocator, u8, &.{ base, "/", clean_rel });
}

/// Normalize a path by resolving '.' and '..' segments.
/// Returns an owned string.
fn normalizePath(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var segments = std.ArrayList([]const u8).empty;
    defer segments.deinit(allocator);

    var is_absolute = false;
    var remaining = path;

    if (remaining.len > 0 and remaining[0] == '/') {
        is_absolute = true;
        remaining = remaining[1..];
    }

    while (remaining.len > 0) {
        // Find next segment
        const sep_idx = std.mem.indexOfScalar(u8, remaining, '/');
        const segment = if (sep_idx) |idx| remaining[0..idx] else remaining;
        remaining = if (sep_idx) |idx| remaining[idx + 1 ..] else "";

        if (segment.len == 0 or std.mem.eql(u8, segment, ".")) {
            // Skip empty segments and "."
            continue;
        }

        if (std.mem.eql(u8, segment, "..")) {
            // Pop the last segment if possible
            if (segments.items.len > 0) {
                _ = segments.pop();
            }
            continue;
        }

        try segments.append(allocator, segment);
    }

    // Reconstruct the path
    var result = std.ArrayList(u8).empty;
    errdefer result.deinit(allocator);

    if (is_absolute) {
        try result.append(allocator, '/');
    }

    for (segments.items, 0..) |seg, i| {
        if (i > 0) try result.append(allocator, '/');
        try result.appendSlice(allocator, seg);
    }

    if (result.items.len == 0) {
        try result.append(allocator, '.');
    }

    return result.toOwnedSlice(allocator);
}

// ============================================================================
// Tests
// ============================================================================

test "resolve: specifier with extension" {
    const allocator = std.testing.allocator;
    const result = try resolve(allocator, "./utils.ts", "/app/src/", null);
    defer allocator.free(result);
    try std.testing.expectEqualStrings("/app/src/utils.ts", result);
}

test "resolve: specifier without extension defaults to .ts" {
    const allocator = std.testing.allocator;
    const result = try resolve(allocator, "./utils", "/app/src/", null);
    defer allocator.free(result);
    try std.testing.expectEqualStrings("/app/src/utils.ts", result);
}

test "resolve: parent directory traversal" {
    const allocator = std.testing.allocator;
    const result = try resolve(allocator, "../lib/helpers.ts", "/app/src/handlers/", null);
    defer allocator.free(result);
    try std.testing.expectEqualStrings("/app/src/lib/helpers.ts", result);
}

test "resolve: nested relative path" {
    const allocator = std.testing.allocator;
    const result = try resolve(allocator, "./lib/utils.ts", "/app/", null);
    defer allocator.free(result);
    try std.testing.expectEqualStrings("/app/lib/utils.ts", result);
}

test "resolve: extension probing with callback" {
    const S = struct {
        fn exists(path: []const u8) bool {
            return std.mem.endsWith(u8, path, "/app/utils.tsx");
        }
    };
    const allocator = std.testing.allocator;
    const result = try resolve(allocator, "./utils", "/app/", S.exists);
    defer allocator.free(result);
    try std.testing.expectEqualStrings("/app/utils.tsx", result);
}

test "dirName: file in directory" {
    try std.testing.expectEqualStrings("/app/src/", dirName("/app/src/handler.ts"));
}

test "dirName: root file" {
    try std.testing.expectEqualStrings("/", dirName("/handler.ts"));
}

test "dirName: no directory" {
    try std.testing.expectEqualStrings("./", dirName("handler.ts"));
}

test "normalizePath: resolves dots" {
    const allocator = std.testing.allocator;
    const result = try normalizePath(allocator, "/app/src/../lib/./utils");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("/app/lib/utils", result);
}

test "normalizePath: absolute path stays absolute" {
    const allocator = std.testing.allocator;
    const result = try normalizePath(allocator, "/app/src/utils");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("/app/src/utils", result);
}
