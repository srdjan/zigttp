const std = @import("std");
const route_match = @import("route_match.zig");

pub const ResponseVariant = struct {
    status: u16,
    content_type: ?[]const u8 = null,
    schema_json: ?[]const u8 = null,
    dynamic: bool = false,

    pub fn deinit(self: *ResponseVariant, allocator: std.mem.Allocator) void {
        if (self.content_type) |content_type| allocator.free(content_type);
        if (self.schema_json) |schema_json| allocator.free(schema_json);
    }
};

pub const RouteInfo = struct {
    service_name: []const u8,
    handler_path: []const u8,
    method: []const u8,
    path: []const u8,
    required_path_params: []const []const u8,
    required_query_params: []const []const u8,
    required_header_params: []const []const u8,
    request_dynamic: bool = false,
    response_dynamic: bool = false,
    requires_body: bool = false,
    responses: []ResponseVariant,

    pub fn deinit(self: *RouteInfo, allocator: std.mem.Allocator) void {
        allocator.free(self.service_name);
        allocator.free(self.handler_path);
        allocator.free(self.method);
        allocator.free(self.path);
        for (self.required_path_params) |name| allocator.free(name);
        allocator.free(self.required_path_params);
        for (self.required_query_params) |name| allocator.free(name);
        allocator.free(self.required_query_params);
        for (self.required_header_params) |name| allocator.free(name);
        allocator.free(self.required_header_params);
        for (self.responses) |*response| response.deinit(allocator);
        allocator.free(self.responses);
    }
};

pub const ServiceTypeContext = struct {
    routes: []RouteInfo,

    pub fn deinit(self: *ServiceTypeContext, allocator: std.mem.Allocator) void {
        for (self.routes) |*route| route.deinit(allocator);
        allocator.free(self.routes);
    }

    pub fn lookupRoute(
        self: *const ServiceTypeContext,
        service_name: []const u8,
        method: []const u8,
        path: []const u8,
    ) ?*const RouteInfo {
        for (self.routes) |*route| {
            if (!std.mem.eql(u8, route.service_name, service_name)) continue;
            if (!std.ascii.eqlIgnoreCase(route.method, method)) continue;
            if (!route_match.pathsMatch(route.path, path)) continue;
            return route;
        }
        return null;
    }
};
