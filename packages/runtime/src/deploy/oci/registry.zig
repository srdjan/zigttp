const std = @import("std");
const http = @import("../http.zig");
const image = @import("image.zig");
const printer_mod = @import("../printer.zig");

const Progress = printer_mod.Progress;

pub const RegistryRef = struct {
    host: []const u8,
    repo: []const u8,

    pub fn deinit(self: *RegistryRef, allocator: std.mem.Allocator) void {
        allocator.free(self.host);
        allocator.free(self.repo);
    }
};

pub fn makeRegistryRef(allocator: std.mem.Allocator, host: []const u8, repo: []const u8) !RegistryRef {
    return .{
        .host = try allocator.dupe(u8, host),
        .repo = try allocator.dupe(u8, repo),
    };
}

pub fn push(
    allocator: std.mem.Allocator,
    registry_ref: *const RegistryRef,
    oci_image: *const image.OciImage,
    username: []const u8,
    password: []const u8,
    progress: ?*Progress,
) !void {
    const challenge = try http.request(allocator, .GET, try std.fmt.allocPrint(allocator, "https://{s}/v2/", .{registry_ref.host}), &.{}, null);
    defer challenge.deinit(allocator);

    const bearer_token = if (challenge.status == 401) blk: {
        const authenticate = challenge.header("www-authenticate") orelse return error.RegistryUnauthorized;
        const parsed = try parseBearerChallenge(allocator, authenticate);
        defer parsed.deinit(allocator);
        break :blk try fetchBearerToken(allocator, &parsed, registry_ref, username, password);
    } else null;
    defer if (bearer_token) |token| allocator.free(token);

    // Phase lines land on stderr. An image has exactly two blobs (config and
    // layer) plus the manifest; we number them 1/3, 2/3, 3/3 so the user can
    // see forward motion even when a single layer push takes minutes.
    try emitPushLine(progress, allocator, "  Pushing config ", oci_image.config_blob.digest, 1, 3);
    try uploadBlobIfMissing(allocator, registry_ref, bearer_token, oci_image.config_blob.digest, oci_image.config_blob.bytes, "application/vnd.oci.image.config.v1+json");

    try emitPushLine(progress, allocator, "  Pushing layer ", oci_image.layer_blob.digest, 2, 3);
    try uploadBlobIfMissing(allocator, registry_ref, bearer_token, oci_image.layer_blob.digest, oci_image.layer_blob.gzip_bytes, "application/vnd.oci.image.layer.v1.tar+gzip");

    const manifest_url = try std.fmt.allocPrint(allocator, "https://{s}/v2/{s}/manifests/{s}", .{
        registry_ref.host,
        registry_ref.repo,
        oci_image.manifest_blob.digest,
    });
    defer allocator.free(manifest_url);

    try emitPushLine(progress, allocator, "  Pushing manifest ", oci_image.manifest_blob.digest, 3, 3);
    var headers = std.ArrayList(http.Header).empty;
    defer headers.deinit(allocator);
    try headers.append(allocator, .{ .name = "content-type", .value = "application/vnd.oci.image.manifest.v1+json" });
    if (bearer_token) |token| {
        const auth = try std.fmt.allocPrint(allocator, "Bearer {s}", .{token});
        defer allocator.free(auth);
        try headers.append(allocator, .{ .name = "authorization", .value = auth });
        const response = try http.request(allocator, .PUT, manifest_url, headers.items, oci_image.manifest_blob.bytes);
        defer response.deinit(allocator);
        if (response.status == 401 or response.status == 403) return error.RegistryUnauthorized;
        if (response.status != 200 and response.status != 201) return error.ManifestUploadFailed;
    } else {
        const response = try http.request(allocator, .PUT, manifest_url, headers.items, oci_image.manifest_blob.bytes);
        defer response.deinit(allocator);
        if (response.status == 401 or response.status == 403) return error.RegistryUnauthorized;
        if (response.status != 200 and response.status != 201) return error.ManifestUploadFailed;
    }
}

const BearerChallenge = struct {
    realm: []const u8,
    service: ?[]const u8,
    scope: ?[]const u8,

    fn deinit(self: *const BearerChallenge, allocator: std.mem.Allocator) void {
        allocator.free(self.realm);
        if (self.service) |value| allocator.free(value);
        if (self.scope) |value| allocator.free(value);
    }
};

fn parseBearerChallenge(allocator: std.mem.Allocator, header_value: []const u8) !BearerChallenge {
    if (!std.mem.startsWith(u8, header_value, "Bearer ")) return error.InvalidRegistryAuth;
    const rest = header_value["Bearer ".len..];
    var realm: ?[]const u8 = null;
    var service: ?[]const u8 = null;
    var scope: ?[]const u8 = null;
    var parts = std.mem.splitScalar(u8, rest, ',');
    while (parts.next()) |part_raw| {
        const part = std.mem.trim(u8, part_raw, " ");
        const eq = std.mem.indexOfScalar(u8, part, '=') orelse continue;
        const key = part[0..eq];
        var value = std.mem.trim(u8, part[eq + 1 ..], "\"");
        value = std.mem.trim(u8, value, " ");
        if (std.mem.eql(u8, key, "realm")) {
            realm = try allocator.dupe(u8, value);
        } else if (std.mem.eql(u8, key, "service")) {
            service = try allocator.dupe(u8, value);
        } else if (std.mem.eql(u8, key, "scope")) {
            scope = try allocator.dupe(u8, value);
        }
    }
    return .{
        .realm = realm orelse return error.InvalidRegistryAuth,
        .service = service,
        .scope = scope,
    };
}

fn fetchBearerToken(
    allocator: std.mem.Allocator,
    challenge: *const BearerChallenge,
    registry_ref: *const RegistryRef,
    username: []const u8,
    password: []const u8,
) ![]u8 {
    const scope = challenge.scope orelse try std.fmt.allocPrint(allocator, "repository:{s}:pull,push", .{registry_ref.repo});
    defer if (challenge.scope == null) allocator.free(scope);
    const service = challenge.service orelse registry_ref.host;
    const url = try std.fmt.allocPrint(allocator, "{s}?service={s}&scope={s}", .{ challenge.realm, service, scope });
    defer allocator.free(url);

    const basic = try http.basicAuthHeader(allocator, username, password);
    defer allocator.free(basic);
    const response = try http.request(allocator, .GET, url, &.{
        .{ .name = "authorization", .value = basic },
        .{ .name = "accept", .value = "application/json" },
    }, null);
    defer response.deinit(allocator);
    if (response.status != 200) return error.RegistryUnauthorized;

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    const root = switch (parsed.value) {
        .object => parsed.value.object,
        else => return error.RegistryUnauthorized,
    };
    const token_value = root.get("token") orelse root.get("access_token") orelse return error.RegistryUnauthorized;
    if (token_value != .string) return error.RegistryUnauthorized;
    return allocator.dupe(u8, token_value.string);
}

fn uploadBlobIfMissing(
    allocator: std.mem.Allocator,
    registry_ref: *const RegistryRef,
    bearer_token: ?[]const u8,
    digest: []const u8,
    bytes: []const u8,
    media_type: []const u8,
) !void {
    const head_url = try std.fmt.allocPrint(allocator, "https://{s}/v2/{s}/blobs/{s}", .{
        registry_ref.host,
        registry_ref.repo,
        digest,
    });
    defer allocator.free(head_url);
    const headers = if (bearer_token) |token|
        &[_]http.Header{.{ .name = "authorization", .value = try std.fmt.allocPrint(allocator, "Bearer {s}", .{token}) }}
    else
        &[_]http.Header{};
    defer if (bearer_token != null) allocator.free(headers[0].value);

    const head = try http.request(allocator, .HEAD, head_url, headers, null);
    defer head.deinit(allocator);
    if (head.status == 200) return;
    if (head.status == 401 or head.status == 403) return error.RegistryUnauthorized;

    const start_url = try std.fmt.allocPrint(allocator, "https://{s}/v2/{s}/blobs/uploads/", .{
        registry_ref.host,
        registry_ref.repo,
    });
    defer allocator.free(start_url);
    const start = try http.request(allocator, .POST, start_url, headers, null);
    defer start.deinit(allocator);
    if (start.status == 401 or start.status == 403) return error.RegistryUnauthorized;
    if (start.status != 202 and start.status != 201) return error.BlobUploadFailed;
    const location = start.header("location") orelse return error.BlobUploadFailed;
    const absolute_location = try absoluteUrl(allocator, registry_ref.host, location);
    defer allocator.free(absolute_location);
    const put_url = if (std.mem.indexOfScalar(u8, absolute_location, '?') == null)
        try std.fmt.allocPrint(allocator, "{s}?digest={s}", .{ absolute_location, digest })
    else
        try std.fmt.allocPrint(allocator, "{s}&digest={s}", .{ absolute_location, digest });
    defer allocator.free(put_url);

    var put_headers = std.ArrayList(http.Header).empty;
    defer put_headers.deinit(allocator);
    try put_headers.append(allocator, .{ .name = "content-type", .value = media_type });

    const auth_value: ?[]u8 = if (bearer_token) |token|
        try std.fmt.allocPrint(allocator, "Bearer {s}", .{token})
    else
        null;
    defer if (auth_value) |value| allocator.free(value);
    if (auth_value) |value| {
        try put_headers.append(allocator, .{ .name = "authorization", .value = value });
    }

    const put = try http.request(allocator, .PUT, put_url, put_headers.items, bytes);
    defer put.deinit(allocator);
    if (put.status == 401 or put.status == 403) return error.RegistryUnauthorized;
    if (put.status != 201 and put.status != 202) return error.BlobUploadFailed;
}

// emitPushLine formats a progress line with a shortened digest and passes it
// through the Progress rate limiter. Short digests keep stderr uncluttered;
// users who need the full digest see it on stdout via `  Digest: ...`.
fn emitPushLine(
    progress: ?*Progress,
    allocator: std.mem.Allocator,
    prefix: []const u8,
    digest: []const u8,
    index: usize,
    total: usize,
) !void {
    const p = progress orelse return;
    const short = shortDigest(digest);
    const line = try std.fmt.allocPrint(allocator, "{s}{s}... ({d}/{d})", .{ prefix, short, index, total });
    defer allocator.free(line);
    p.emit(line);
}

fn shortDigest(digest: []const u8) []const u8 {
    // Digests are "sha256:<64hex>"; trim to "sha256:<first 7>..." so lines stay
    // short. If the digest is shorter than expected, fall back to the raw value.
    const colon = std.mem.indexOfScalar(u8, digest, ':') orelse return digest;
    const hex_start = colon + 1;
    if (digest.len < hex_start + 7) return digest;
    return digest[0 .. hex_start + 7];
}

fn absoluteUrl(allocator: std.mem.Allocator, host: []const u8, location: []const u8) ![]u8 {
    if (std.mem.startsWith(u8, location, "http://") or std.mem.startsWith(u8, location, "https://")) {
        return allocator.dupe(u8, location);
    }
    if (std.mem.startsWith(u8, location, "/")) {
        return std.fmt.allocPrint(allocator, "https://{s}{s}", .{ host, location });
    }
    return std.fmt.allocPrint(allocator, "https://{s}/{s}", .{ host, location });
}

test "make registry ref holds structured host and repo" {
    var ref = try makeRegistryRef(std.testing.allocator, "ghcr.io", "acme/demo");
    defer ref.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings("ghcr.io", ref.host);
    try std.testing.expectEqualStrings("acme/demo", ref.repo);
}
