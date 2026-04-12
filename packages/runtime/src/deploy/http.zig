const std = @import("std");
const redact = @import("redact.zig");

pub const Header = struct {
    name: []const u8,
    value: []const u8,
};

pub const Response = struct {
    status: u16,
    body: []u8,
    headers: []Header,

    pub fn deinit(self: *const Response, allocator: std.mem.Allocator) void {
        allocator.free(self.body);
        for (self.headers) |item| {
            allocator.free(item.name);
            allocator.free(item.value);
        }
        allocator.free(self.headers);
    }

    pub fn header(self: *const Response, name: []const u8) ?[]const u8 {
        for (self.headers) |item| {
            if (std.ascii.eqlIgnoreCase(item.name, name)) return item.value;
        }
        return null;
    }
};

pub fn request(
    allocator: std.mem.Allocator,
    method: std.http.Method,
    url: []const u8,
    headers: []const Header,
    body: ?[]const u8,
) !Response {
    const uri = try std.Uri.parse(url);
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();

    var client = std.http.Client{
        .allocator = allocator,
        .io = io_backend.io(),
    };
    defer client.deinit();

    const protocol = std.http.Client.Protocol.fromUri(uri) orelse return error.UnsupportedProtocol;
    var host_buf: [std.Io.net.HostName.max_len]u8 = undefined;
    const host = try uri.getHost(&host_buf);
    const connection = try client.connectTcpOptions(.{
        .host = host,
        .port = uri.port orelse switch (protocol) {
            .plain => 80,
            .tls => 443,
        },
        .protocol = protocol,
        .timeout = .none,
    });

    var extra_headers = try allocator.alloc(std.http.Header, headers.len);
    defer allocator.free(extra_headers);
    for (headers, 0..) |header, idx| {
        extra_headers[idx] = .{ .name = header.name, .value = header.value };
    }

    var req = try client.request(method, uri, .{
        .redirect_behavior = .unhandled,
        .keep_alive = false,
        .connection = connection,
        .extra_headers = extra_headers,
    });
    defer req.deinit();

    if (body) |payload| {
        req.transfer_encoding = .{ .content_length = payload.len };
        var req_body = try req.sendBodyUnflushed(&.{});
        try req_body.writer.writeAll(payload);
        try req_body.end();
        try req.connection.?.flush();
    } else {
        try req.sendBodiless();
    }

    var response = try req.receiveHead(&.{});
    const owned_headers = try snapshotHeaders(allocator, response.head.iterateHeaders());
    errdefer {
        for (owned_headers) |header| {
            allocator.free(header.name);
            allocator.free(header.value);
        }
        allocator.free(owned_headers);
    }

    var body_transfer: [64]u8 = undefined;
    var reader = response.reader(&body_transfer);
    const response_body = try reader.allocRemaining(allocator, .unlimited);

    return .{
        .status = @intFromEnum(response.head.status),
        .body = response_body,
        .headers = owned_headers,
    };
}

pub fn requestJson(
    allocator: std.mem.Allocator,
    method: std.http.Method,
    url: []const u8,
    bearer_token: ?[]const u8,
    body_json: ?[]const u8,
) !Response {
    var headers = std.ArrayList(Header).empty;
    defer headers.deinit(allocator);

    try headers.append(allocator, .{ .name = "accept", .value = "application/json" });
    if (body_json != null) {
        try headers.append(allocator, .{ .name = "content-type", .value = "application/json" });
    }
    if (bearer_token) |token| {
        const header_value = try std.fmt.allocPrint(allocator, "Bearer {s}", .{token});
        defer allocator.free(header_value);
        try headers.append(allocator, .{ .name = "authorization", .value = header_value });
        _ = redact.redactSecret(token);
    }

    return request(allocator, method, url, headers.items, body_json);
}

pub fn basicAuthHeader(allocator: std.mem.Allocator, username: []const u8, password: []const u8) ![]u8 {
    const raw = try std.fmt.allocPrint(allocator, "{s}:{s}", .{ username, password });
    defer allocator.free(raw);
    const encoded_len = std.base64.standard.Encoder.calcSize(raw.len);
    const encoded = try allocator.alloc(u8, encoded_len);
    errdefer allocator.free(encoded);
    _ = std.base64.standard.Encoder.encode(encoded, raw);
    return std.fmt.allocPrint(allocator, "Basic {s}", .{encoded});
}

fn snapshotHeaders(allocator: std.mem.Allocator, iter: anytype) ![]Header {
    var list = std.ArrayList(Header).empty;
    errdefer {
        for (list.items) |header| {
            allocator.free(header.name);
            allocator.free(header.value);
        }
        list.deinit(allocator);
    }

    var headers_iter = iter;
    while (headers_iter.next()) |header| {
        try list.append(allocator, .{
            .name = try allocator.dupe(u8, header.name),
            .value = try allocator.dupe(u8, header.value),
        });
    }
    return try list.toOwnedSlice(allocator);
}
