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
        try writer.print("    \"deterministic\": {s},\n", .{if (p.deterministic) "true" else "false"});
        try writer.print("    \"injectionSafe\": {s},\n", .{if (p.injection_safe) "true" else "false"});
        try writer.print("    \"idempotent\": {s},\n", .{if (p.idempotent) "true" else "false"});
        try writer.print("    \"stateIsolated\": {s},\n", .{if (p.state_isolated) "true" else "false"});
        if (p.max_io_depth) |depth| {
            try writer.print("    \"maxIoDepth\": {d}\n", .{depth});
        } else {
            try writer.writeAll("    \"maxIoDepth\": null\n");
        }
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

            if (candidate.path_params.items.len > 0 or candidate.query_params.items.len > 0 or candidate.header_params.items.len > 0) {
                try writer.writeAll("        \"parameters\": [\n");
                try writeParameters(writer, &candidate);
                try writer.writeAll("\n        ],\n");
            }

            if (candidate.request_bodies.items.len > 0) {
                try writer.writeAll("        \"requestBody\": {\n");
                try writer.writeAll("          \"required\": true,\n");
                try writer.writeAll("          \"content\": {\n");
                try writeRequestBodies(writer, contract, &candidate);
                try writer.writeAll("          }\n");
                try writer.writeAll("        },\n");
            } else if (candidate.request_schema_refs.items.len > 0 or candidate.request_schema_dynamic or candidate.request_bodies_dynamic) {
                try writer.writeAll("        \"x-zigttp-requestSchemas\": [");
                for (candidate.request_schema_refs.items, 0..) |schema_ref, j| {
                    if (j > 0) try writer.writeAll(", ");
                    try writeJsonString(writer, schema_ref);
                }
                try writer.writeAll("],\n");
            }

            if (candidate.query_params_dynamic) {
                try writer.writeAll("        \"x-zigttp-queryParamsDynamic\": true,\n");
            }
            if (candidate.header_params_dynamic) {
                try writer.writeAll("        \"x-zigttp-headerParamsDynamic\": true,\n");
            }
            if (candidate.request_bodies_dynamic) {
                try writer.writeAll("        \"x-zigttp-requestBodiesDynamic\": true,\n");
            }

            if (candidate.requires_bearer or candidate.requires_jwt) {
                try writer.writeAll("        \"security\": [\n");
                try writer.writeAll("          { \"bearerAuth\": [] }\n");
                try writer.writeAll("        ],\n");
            }

            try writer.writeAll("        \"responses\": {\n");
            try writeResponses(writer, contract, &candidate);
            try writer.writeAll("\n        }");
            if (candidate.responses_dynamic or candidate.response_schema_dynamic) {
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

fn writeResponses(writer: anytype, contract: *const HandlerContract, route: *const ApiRouteInfo) !void {
    if (route.responses.items.len == 0) {
        try writeSingleResponse(writer, contract, route.response_status, route.response_content_type, route.response_schema_ref, route.response_schema_json);
        return;
    }

    for (route.responses.items, 0..) |response, idx| {
        if (idx > 0) try writer.writeAll(",\n");
        try writeSingleResponse(writer, contract, response.status, response.content_type, response.schema_ref, response.schema_json);
    }
}

fn writeSingleResponse(
    writer: anytype,
    contract: *const HandlerContract,
    status: ?u16,
    content_type: ?[]const u8,
    schema_ref: ?[]const u8,
    schema_json: ?[]const u8,
) !void {
    if (status) |value| {
        try writer.writeAll("          \"");
        try writer.print("{d}", .{value});
        try writer.writeAll("\": {\n");
    } else {
        try writer.writeAll("          \"default\": {\n");
    }

    try writer.writeAll("            \"description\": ");
    if (status) |value| {
        try writer.writeByte('"');
        try writer.writeAll("HTTP ");
        try writer.print("{d}", .{value});
        try writer.writeAll(" response\"");
    } else {
        try writeJsonString(writer, "Handler response");
    }

    if (content_type) |value| {
        try writer.writeAll(",\n");
        try writer.writeAll("            \"content\": {\n");
        try writer.writeAll("              ");
        try writeJsonString(writer, value);
        try writer.writeAll(": {\n");
        try writer.writeAll("                \"schema\": ");
        try writeContentSchema(writer, contract, value, schema_ref, schema_json);
        try writer.writeAll("\n");
        try writer.writeAll("              }\n");
        try writer.writeAll("            }\n");
    } else {
        try writer.writeByte('\n');
    }

    try writer.writeAll("          }");
}

fn writeContentSchema(
    writer: anytype,
    contract: *const HandlerContract,
    content_type: []const u8,
    schema_ref: ?[]const u8,
    schema_json: ?[]const u8,
) !void {
    if (std.mem.startsWith(u8, content_type, "application/json")) {
        if (schema_ref) |value| {
            if (!hasSchema(contract, value)) {
                try writer.writeAll("{}");
                return;
            }
            try writer.writeAll("{\"$ref\":\"#/components/schemas/");
            try writeJsonStringContent(writer, value);
            try writer.writeAll("\"}");
            return;
        }
        if (schema_json) |value| {
            try writer.writeAll(value);
            return;
        }
        try writer.writeAll("{}");
        return;
    }

    try writer.writeAll("{\"type\":\"string\"}");
}

fn writeParameters(writer: anytype, route: *const ApiRouteInfo) !void {
    var wrote_any = false;
    for (route.path_params.items) |param| {
        if (wrote_any) try writer.writeAll(",\n");
        wrote_any = true;
        try writeParameter(writer, &param);
    }
    for (route.query_params.items) |param| {
        if (wrote_any) try writer.writeAll(",\n");
        wrote_any = true;
        try writeParameter(writer, &param);
    }
    for (route.header_params.items) |param| {
        if (wrote_any) try writer.writeAll(",\n");
        wrote_any = true;
        try writeParameter(writer, &param);
    }
}

fn writeParameter(writer: anytype, param: *const handler_contract.ApiParamInfo) !void {
    try writer.writeAll("          {\n");
    try writer.writeAll("            \"name\": ");
    try writeJsonString(writer, param.name);
    try writer.writeAll(",\n");
    try writer.writeAll("            \"in\": ");
    try writeJsonString(writer, param.location);
    try writer.writeAll(",\n");
    try writer.print("            \"required\": {s},\n", .{if (std.mem.eql(u8, param.location, "path")) "true" else if (param.required) "true" else "false"});
    try writer.writeAll("            \"schema\": ");
    try writer.writeAll(param.schema_json);
    try writer.writeAll("\n");
    try writer.writeAll("          }");
}

fn writeRequestBodies(writer: anytype, contract: *const HandlerContract, route: *const ApiRouteInfo) !void {
    for (route.request_bodies.items, 0..) |body, idx| {
        if (idx > 0) try writer.writeAll(",\n");
        try writer.writeAll("            ");
        try writeJsonString(writer, body.content_type orelse "application/octet-stream");
        try writer.writeAll(": {\n");
        try writer.writeAll("              \"schema\": ");
        try writeContentSchema(writer, contract, body.content_type orelse "application/octet-stream", body.schema_ref, body.schema_json);
        try writer.writeAll("\n");
        try writer.writeAll("            }");
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

const writeJsonString = handler_contract.writeJsonString;
const writeJsonStringContent = handler_contract.writeJsonStringContent;

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
        .required = true,
        .schema_json = try allocator.dupe(u8, "{\"type\":\"string\"}"),
    });
    var query_params: std.ArrayList(handler_contract.ApiParamInfo) = .empty;
    try query_params.append(allocator, .{
        .name = try allocator.dupe(u8, "verbose"),
        .location = "query",
        .required = false,
        .schema_json = try allocator.dupe(u8, "{\"type\":\"string\"}"),
    });
    var header_params: std.ArrayList(handler_contract.ApiParamInfo) = .empty;
    try header_params.append(allocator, .{
        .name = try allocator.dupe(u8, "authorization"),
        .location = "header",
        .required = false,
        .schema_json = try allocator.dupe(u8, "{\"type\":\"string\"}"),
    });
    var request_bodies: std.ArrayList(handler_contract.ApiBodyInfo) = .empty;
    try request_bodies.append(allocator, .{
        .content_type = try allocator.dupe(u8, "application/json"),
        .schema_ref = try allocator.dupe(u8, "user"),
    });
    var responses: std.ArrayList(handler_contract.ApiResponseInfo) = .empty;
    try responses.append(allocator, .{
        .status = 201,
        .content_type = try allocator.dupe(u8, "application/json"),
        .schema_json = try allocator.dupe(u8, "{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"}},\"required\":[\"id\"]}"),
    });
    try responses.append(allocator, .{
        .status = 401,
        .content_type = try allocator.dupe(u8, "application/json"),
        .schema_json = try allocator.dupe(u8, "{\"type\":\"object\",\"properties\":{\"error\":{\"type\":\"string\"}},\"required\":[\"error\"]}"),
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
        .query_params = query_params,
        .header_params = header_params,
        .request_bodies = request_bodies,
        .responses = responses,
        .response_status = 201,
        .response_content_type = try allocator.dupe(u8, "application/json"),
        .response_schema_json = try allocator.dupe(u8, "{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"}},\"required\":[\"id\"]}"),
    });

    var contract = HandlerContract{
        .handler = .{ .path = try allocator.dupe(u8, "examples/routing/router.ts"), .line = 1, .column = 1 },
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
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"verbose\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"authorization\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"required\": true") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"required\": false") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"properties\":{\"id\":{\"type\":\"string\"}}") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"401\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"bearerAuth\"") != null);
}
