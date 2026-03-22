//! OpenAPI Manifest Renderer
//!
//! Renders an OpenAPI 3.1 document from compiler-proven handler contract facts.
//! The output only includes routes, schemas, and auth that zigttp can prove.

const std = @import("std");
const handler_contract = @import("zts").handler_contract;

const HandlerContract = handler_contract.HandlerContract;
const ApiRouteInfo = handler_contract.ApiRouteInfo;

pub const RenderOptions = struct {
    title: ?[]const u8 = null,
    version: []const u8 = "0.1.0",
};

pub fn writeOpenApiJson(
    writer: anytype,
    contract: *const HandlerContract,
    options: RenderOptions,
) !void {
    try writer.writeAll("{\n");
    try writer.writeAll("  \"openapi\": \"3.1.0\",\n");
    try writer.writeAll("  \"info\": {\n");
    try writer.writeAll("    \"title\": ");
    try writeJsonString(writer, options.title orelse deriveTitle(contract.handler.path));
    try writer.writeAll(",\n");
    try writer.writeAll("    \"version\": ");
    try writeJsonString(writer, options.version);
    try writer.writeAll("\n");
    try writer.writeAll("  },\n");

    if (contract.properties) |p| {
        try writer.writeAll("  \"x-zigttp-properties\": {\n");
        try writer.print("    \"pure\": {s},\n", .{if (p.pure) "true" else "false"});
        try writer.print("    \"readOnly\": {s},\n", .{if (p.read_only) "true" else "false"});
        try writer.print("    \"stateless\": {s},\n", .{if (p.stateless) "true" else "false"});
        try writer.print("    \"retrySafe\": {s},\n", .{if (p.retry_safe) "true" else "false"});
        try writer.print("    \"deterministic\": {s}\n", .{if (p.deterministic) "true" else "false"});
        try writer.writeAll("  },\n");
    }

    if (contract.api.auth.bearer or contract.api.auth.jwt) {
        try writer.writeAll("  \"security\": [\n");
        try writer.writeAll("    { \"bearerAuth\": [] }\n");
        try writer.writeAll("  ],\n");
    }

    try writer.writeAll("  \"paths\": {");
    var wrote_path = false;
    for (contract.api.routes.items, 0..) |route, i| {
        if (isDuplicatePath(contract.api.routes.items, i)) continue;
        if (wrote_path) try writer.writeAll(",");
        wrote_path = true;
        try writer.writeAll("\n    ");
        try writeOpenApiPathString(writer, route.path);
        try writer.writeAll(": {");

        var wrote_op = false;
        for (contract.api.routes.items) |candidate| {
            if (!std.mem.eql(u8, candidate.path, route.path)) continue;
            if (wrote_op) try writer.writeAll(",");
            wrote_op = true;

            try writer.writeAll("\n      \"");
            try writeLowerAscii(writer, candidate.method);
            try writer.writeAll("\": {\n");
            try writer.writeAll("        \"operationId\": ");
            try writeOperationId(writer, candidate);
            try writer.writeAll(",\n");

            if (candidate.path_params.items.len > 0) {
                try writer.writeAll("        \"parameters\": [\n");
                try writePathParameters(writer, &candidate);
                try writer.writeAll("\n        ],\n");
            }

            if (candidate.request_schema_refs.items.len == 1 and
                hasSchema(contract, candidate.request_schema_refs.items[0]))
            {
                try writer.writeAll("        \"requestBody\": {\n");
                try writer.writeAll("          \"required\": true,\n");
                try writer.writeAll("          \"content\": {\n");
                try writer.writeAll("            \"application/json\": {\n");
                try writer.writeAll("              \"schema\": {\n");
                try writer.writeAll("                \"$ref\": \"#/components/schemas/");
                try writeJsonStringContent(writer, candidate.request_schema_refs.items[0]);
                try writer.writeByte('"');
                try writer.writeAll("\n");
                try writer.writeAll("              }\n");
                try writer.writeAll("            }\n");
                try writer.writeAll("          }\n");
                try writer.writeAll("        },\n");
            } else if (candidate.request_schema_refs.items.len > 0 or candidate.request_schema_dynamic) {
                try writer.writeAll("        \"x-zigttp-requestSchemas\": [");
                for (candidate.request_schema_refs.items, 0..) |schema_ref, j| {
                    if (j > 0) try writer.writeAll(", ");
                    try writeJsonString(writer, schema_ref);
                }
                try writer.writeAll("],\n");
            }

            if (candidate.requires_bearer or candidate.requires_jwt) {
                try writer.writeAll("        \"security\": [\n");
                try writer.writeAll("          { \"bearerAuth\": [] }\n");
                try writer.writeAll("        ],\n");
            }

            try writer.writeAll("        \"responses\": {\n");
            try writeResponse(writer, &candidate);
            try writer.writeAll("\n        }");
            if (candidate.response_schema_dynamic) {
                try writer.writeAll(",\n");
                try writer.writeAll("        \"x-zigttp-responseSchemaDynamic\": true\n");
            } else {
                try writer.writeByte('\n');
            }
            try writer.writeAll("      }");
        }

        if (route.request_schema_dynamic or contract.api.routes_dynamic) {
            if (wrote_op) try writer.writeAll(",");
            try writer.writeAll("\n      \"x-zigttp-routesDynamic\": true");
        }

        if (wrote_op) try writer.writeAll("\n    ");
        try writer.writeAll("}");
    }
    if (wrote_path) {
        try writer.writeAll("\n  ");
    }
    try writer.writeAll("},\n");

    try writer.writeAll("  \"components\": {\n");
    if (contract.api.auth.bearer or contract.api.auth.jwt) {
        try writer.writeAll("    \"securitySchemes\": {\n");
        try writer.writeAll("      \"bearerAuth\": {\n");
        try writer.writeAll("        \"type\": \"http\",\n");
        try writer.writeAll("        \"scheme\": \"bearer\",\n");
        try writer.writeAll("        \"bearerFormat\": \"JWT\"\n");
        try writer.writeAll("      }\n");
        if (contract.api.schemas.items.len > 0) {
            try writer.writeAll("    },\n");
        } else {
            try writer.writeAll("    }\n");
        }
    }

    if (contract.api.schemas.items.len > 0) {
        try writer.writeAll("    \"schemas\": {");
        for (contract.api.schemas.items, 0..) |schema, i| {
            if (i > 0) try writer.writeAll(",");
            try writer.writeAll("\n      ");
            try writeJsonString(writer, schema.name);
            try writer.writeAll(": ");
            try writer.writeAll(schema.schema_json);
        }
        try writer.writeAll("\n    }\n");
    }
    try writer.writeAll("  },\n");

    try writer.writeAll("  \"x-zigttp\": {\n");
    try writer.print("    \"schemasDynamic\": {s},\n", .{if (contract.api.schemas_dynamic) "true" else "false"});
    try writer.print("    \"routesDynamic\": {s},\n", .{if (contract.api.routes_dynamic) "true" else "false"});
    try writer.writeAll("    \"requestSchemas\": [");
    for (contract.api.requests.schema_refs.items, 0..) |schema_ref, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, schema_ref);
    }
    try writer.writeAll("]\n");
    try writer.writeAll("  }\n");
    try writer.writeAll("}\n");
}

fn deriveTitle(path: []const u8) []const u8 {
    var start: usize = 0;
    for (path, 0..) |c, i| {
        if (c == '/') start = i + 1;
    }
    const name = path[start..];
    for (name, 0..) |c, i| {
        if (c == '.') return name[0..i];
    }
    return name;
}

fn isDuplicatePath(routes: []const ApiRouteInfo, idx: usize) bool {
    var i: usize = 0;
    while (i < idx) : (i += 1) {
        if (std.mem.eql(u8, routes[i].path, routes[idx].path)) return true;
    }
    return false;
}

fn hasSchema(contract: *const HandlerContract, schema_name: []const u8) bool {
    for (contract.api.schemas.items) |schema| {
        if (std.mem.eql(u8, schema.name, schema_name)) return true;
    }
    return false;
}

fn writeOperationId(writer: anytype, route: ApiRouteInfo) !void {
    try writer.writeByte('"');
    try writeLowerAscii(writer, route.method);
    try writer.writeByte('_');
    for (route.path) |c| {
        switch (c) {
            'a'...'z', 'A'...'Z', '0'...'9' => try writer.writeByte(std.ascii.toLower(c)),
            ':', '/', '-', '.', '{', '}' => try writer.writeByte('_'),
            else => {},
        }
    }
    try writer.writeByte('"');
}

fn writeResponse(writer: anytype, route: *const ApiRouteInfo) !void {
    if (route.response_status) |status| {
        try writer.writeAll("          \"");
        try writer.print("{d}", .{status});
        try writer.writeAll("\": {\n");
    } else {
        try writer.writeAll("          \"default\": {\n");
    }

    try writer.writeAll("            \"description\": ");
    if (route.response_status) |status| {
        try writer.writeByte('"');
        try writer.writeAll("HTTP ");
        try writer.print("{d}", .{status});
        try writer.writeAll(" response\"");
    } else {
        try writeJsonString(writer, "Handler response");
    }

    if (route.response_content_type) |content_type| {
        try writer.writeAll(",\n");
        try writer.writeAll("            \"content\": {\n");
        try writer.writeAll("              ");
        try writeJsonString(writer, content_type);
        try writer.writeAll(": {\n");
        try writer.writeAll("                \"schema\": ");
        try writeContentSchema(writer, route, content_type);
        try writer.writeAll("\n");
        try writer.writeAll("              }\n");
        try writer.writeAll("            }\n");
    } else {
        try writer.writeByte('\n');
    }

    try writer.writeAll("          }");
}

fn writeContentSchema(writer: anytype, route: *const ApiRouteInfo, content_type: []const u8) !void {
    if (std.mem.startsWith(u8, content_type, "application/json")) {
        if (route.response_schema_ref) |schema_ref| {
            try writer.writeAll("{\"$ref\":\"#/components/schemas/");
            try writeJsonStringContent(writer, schema_ref);
            try writer.writeAll("\"}");
            return;
        }
        if (route.response_schema_json) |schema_json| {
            try writer.writeAll(schema_json);
            return;
        }
        try writer.writeAll("{}");
        return;
    }

    try writer.writeAll("{\"type\":\"string\"}");
}

fn writePathParameters(writer: anytype, route: *const ApiRouteInfo) !void {
    for (route.path_params.items, 0..) |param, i| {
        if (i > 0) try writer.writeAll(",\n");
        try writer.writeAll("          {\n");
        try writer.writeAll("            \"name\": ");
        try writeJsonString(writer, param.name);
        try writer.writeAll(",\n");
        try writer.writeAll("            \"in\": ");
        try writeJsonString(writer, param.location);
        try writer.writeAll(",\n");
        try writer.writeAll("            \"required\": true,\n");
        try writer.writeAll("            \"schema\": ");
        try writer.writeAll(param.schema_json);
        try writer.writeAll("\n");
        try writer.writeAll("          }");
    }
}

fn writeOpenApiPathString(writer: anytype, path: []const u8) !void {
    try writer.writeByte('"');
    var i: usize = 0;
    while (i < path.len) : (i += 1) {
        if (path[i] == ':') {
            try writer.writeByte('{');
            i += 1;
            while (i < path.len and path[i] != '/') : (i += 1) {
                try writer.writeByte(path[i]);
            }
            try writer.writeByte('}');
            if (i < path.len and path[i] == '/') {
                try writer.writeByte('/');
            }
            continue;
        }
        if (path[i] == '"') {
            try writer.writeAll("\\\"");
        } else {
            try writer.writeByte(path[i]);
        }
    }
    try writer.writeByte('"');
}

fn writeLowerAscii(writer: anytype, s: []const u8) !void {
    for (s) |c| {
        try writer.writeByte(std.ascii.toLower(c));
    }
}

fn writeJsonString(writer: anytype, s: []const u8) !void {
    try writer.writeByte('"');
    try writeJsonStringContent(writer, s);
    try writer.writeByte('"');
}

fn writeJsonStringContent(writer: anytype, s: []const u8) !void {
    for (s) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            0x00...0x08, 0x0b...0x0c, 0x0e...0x1f => try writer.print("\\u{x:0>4}", .{@as(u16, c)}),
            else => try writer.writeByte(c),
        }
    }
}

test "writeOpenApiJson renders schema and route" {
    const allocator = std.testing.allocator;

    var schemas: std.ArrayList(handler_contract.ApiSchemaInfo) = .empty;
    try schemas.append(allocator, .{
        .name = try allocator.dupe(u8, "user"),
        .schema_json = try allocator.dupe(u8, "{\"type\":\"object\"}"),
    });

    var global_requests: std.ArrayList([]const u8) = .empty;
    try global_requests.append(allocator, try allocator.dupe(u8, "user"));

    var route_requests: std.ArrayList([]const u8) = .empty;
    try route_requests.append(allocator, try allocator.dupe(u8, "user"));

    var path_params: std.ArrayList(handler_contract.ApiParamInfo) = .empty;
    try path_params.append(allocator, .{
        .name = try allocator.dupe(u8, "id"),
        .location = "path",
        .schema_json = try allocator.dupe(u8, "{\"type\":\"string\"}"),
    });

    var routes: std.ArrayList(handler_contract.ApiRouteInfo) = .empty;
    try routes.append(allocator, .{
        .method = try allocator.dupe(u8, "POST"),
        .path = try allocator.dupe(u8, "/users/:id"),
        .request_schema_refs = route_requests,
        .request_schema_dynamic = false,
        .requires_bearer = true,
        .requires_jwt = true,
        .path_params = path_params,
        .response_status = 201,
        .response_content_type = try allocator.dupe(u8, "application/json"),
        .response_schema_json = try allocator.dupe(u8, "{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"}},\"required\":[\"id\"]}"),
    });

    var contract = HandlerContract{
        .handler = .{ .path = try allocator.dupe(u8, "examples/router.ts"), .line = 1, .column = 1 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = .empty, .dynamic = false },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .sql = handler_contract.emptySqlInfo(),
        .api = .{
            .schemas = schemas,
            .requests = .{ .schema_refs = global_requests, .dynamic = false },
            .auth = .{ .bearer = true, .jwt = true },
            .routes = routes,
            .schemas_dynamic = false,
            .routes_dynamic = false,
        },
        .verification = null,
        .aot = null,
    };
    defer contract.deinit(allocator);

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    try writeOpenApiJson(&aw.writer, &contract, .{});
    output = aw.toArrayList();

    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"openapi\": \"3.1.0\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"/users/{id}\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"post\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "#/components/schemas/user") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"parameters\": [") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"required\": true") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"properties\":{\"id\":{\"type\":\"string\"}}") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"bearerAuth\"") != null);
}
