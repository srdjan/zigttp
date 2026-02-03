//! HTTP protocol types shared between server and runtime layers.
//!
//! These types define the HTTP request/response contract without
//! depending on the JavaScript engine (zts). The runtime layer
//! converts between these types and JS objects.

const std = @import("std");
const ascii = std.ascii;

/// A single query parameter key-value pair (references into string_storage)
pub const QueryParam = struct {
    key: []const u8,
    value: []const u8,
};

pub const HttpRequestView = struct {
    method: []const u8,
    url: []const u8,
    /// URL path without query string (e.g., "/api/process" from "/api/process?items=100")
    path: []const u8 = "",
    /// Parsed query parameters (references into string_storage)
    query_params: []const QueryParam = &.{},
    headers: std.ArrayListUnmanaged(HttpHeader),
    body: ?[]const u8,
};

pub const HttpRequestOwned = struct {
    method: []const u8,
    url: []const u8,
    /// URL path without query string (e.g., "/api/process" from "/api/process?items=100")
    path: []const u8 = "",
    /// Parsed query parameters (references into string_storage)
    query_params: []const QueryParam = &.{},
    headers: std.ArrayListUnmanaged(HttpHeader),
    body: ?[]const u8,

    pub fn deinit(self: *HttpRequestOwned, allocator: std.mem.Allocator) void {
        allocator.free(self.method);
        allocator.free(self.url);
        if (self.body) |b| allocator.free(b);
        for (self.headers.items) |header| {
            allocator.free(header.key);
            allocator.free(header.value);
        }
        self.headers.deinit(allocator);
    }

    pub fn asView(self: *const HttpRequestOwned) HttpRequestView {
        return .{
            .method = self.method,
            .url = self.url,
            .path = self.path,
            .query_params = self.query_params,
            .headers = self.headers,
            .body = self.body,
        };
    }
};

pub const HttpHeader = struct {
    key: []const u8,
    value: []const u8,
};

pub const ResponseHeader = struct {
    key: []const u8,
    value: []const u8,
    key_owned: bool,
    value_owned: bool,
};

pub const HttpResponse = struct {
    status: u16,
    headers: std.ArrayListUnmanaged(ResponseHeader),
    body: []const u8,
    body_owned: bool,
    /// Opaque pointer to the owner of borrowed body data.
    /// Keeps the owner alive while the response is in flight.
    /// In practice this is a *zts.JSString when body is borrowed from the JS heap.
    body_owner: ?*anyopaque,
    allocator: std.mem.Allocator,
    /// Pre-built raw HTTP response (status line + headers + body).
    /// When set, sendResponseSync can write this directly without any processing.
    prebuilt_raw: ?[]const u8 = null,

    pub fn init(allocator: std.mem.Allocator) HttpResponse {
        return .{
            .status = 200,
            .headers = .{},
            .body = "",
            .body_owned = false,
            .body_owner = null,
            .allocator = allocator,
            .prebuilt_raw = null,
        };
    }

    pub fn deinit(self: *HttpResponse) void {
        for (self.headers.items) |header| {
            if (header.key_owned) {
                self.allocator.free(header.key);
            }
            if (header.value_owned) {
                self.allocator.free(header.value);
            }
        }
        self.headers.deinit(self.allocator);
        if (self.body.len > 0 and self.body_owned) {
            self.allocator.free(self.body);
        }
        self.body_owner = null;
    }

    /// Add or update a header, duplicating key/value strings (caller does not retain ownership)
    pub fn putHeader(self: *HttpResponse, key: []const u8, val: []const u8) !void {
        try self.putHeaderInternal(key, val, true);
    }

    /// Add or update a header without duplicating strings (caller retains ownership)
    pub fn putHeaderBorrowed(self: *HttpResponse, key: []const u8, val: []const u8) !void {
        try self.putHeaderInternal(key, val, false);
    }

    fn putHeaderInternal(self: *HttpResponse, key: []const u8, val: []const u8, owned: bool) !void {
        const final_key = if (owned) try self.allocator.dupe(u8, key) else key;
        errdefer if (owned) self.allocator.free(final_key);
        const final_val = if (owned) try self.allocator.dupe(u8, val) else val;
        errdefer if (owned) self.allocator.free(final_val);

        for (self.headers.items) |*header| {
            if (ascii.eqlIgnoreCase(header.key, key)) {
                if (header.key_owned) self.allocator.free(header.key);
                if (header.value_owned) self.allocator.free(header.value);
                header.* = .{ .key = final_key, .value = final_val, .key_owned = owned, .value_owned = owned };
                return;
            }
        }
        try self.headers.append(self.allocator, .{ .key = final_key, .value = final_val, .key_owned = owned, .value_owned = owned });
    }

    pub fn setBodyOwned(self: *HttpResponse, bytes: []const u8) void {
        self.body = bytes;
        self.body_owned = true;
        self.body_owner = null;
    }

    /// Set body to borrowed data. The owner pointer keeps the data alive
    /// until the response is deinitialized.
    pub fn setBodyBorrowed(self: *HttpResponse, data: []const u8, owner: ?*anyopaque) void {
        self.body = data;
        self.body_owned = false;
        self.body_owner = owner;
    }
};
