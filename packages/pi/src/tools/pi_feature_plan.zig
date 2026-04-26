//! pi_feature_plan - compile a structured feature mini-spec into a typed
//! handler authoring plan plus a compiler-verified candidate source.

const std = @import("std");
const zigts = @import("zigts");
const registry_mod = @import("../registry/registry.zig");
const ui_payload = @import("../ui_payload.zig");
const proof_enrichment = @import("../proof_enrichment.zig");
const common = @import("common.zig");

const json_utils = zigts.json_utils;

const name = "pi_feature_plan";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "feature-plan",
    .description =
    \\Generate a compiler-native feature plan for a handler route.
    \\Input is a structured mini-spec, not prose. v1 supports route
    \\generation for handlers: file, method, path, optional body_schema,
    \\response_schema, and status. The tool returns a typed plan DAG, a
    \\candidate source file, a unified diff, and compiler verification.
    ,
    .input_schema =
    \\{"type":"object","properties":{"kind":{"type":"string","enum":["route"]},"file":{"type":"string"},"method":{"type":"string"},"path":{"type":"string"},"body_schema":{"type":"string"},"response_schema":{"type":"string"},"status":{"type":"integer","minimum":100,"maximum":599}},"required":["kind","file","method","path"]}
    ,
    .decode_json = decodeJson,
    .execute = execute,
};

pub const RouteSpec = struct {
    file: []const u8,
    method: []const u8,
    path: []const u8,
    body_schema: ?[]const u8 = null,
    response_schema: ?[]const u8 = null,
    status: u16 = 200,
};

fn decodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    const trimmed = std.mem.trim(u8, args_json, " \t\r\n");
    if (trimmed.len == 0) return error.InvalidToolArgsJson;
    const out = try allocator.alloc([]const u8, 1);
    out[0] = try allocator.dupe(u8, trimmed);
    return out;
}

pub fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    var spec_arena = std.heap.ArenaAllocator.init(allocator);
    defer spec_arena.deinit();
    const spec = parseRouteSpecArgs(spec_arena.allocator(), args) catch |err| switch (err) {
        error.InvalidToolArgsJson => return registry_mod.ToolResult.err(allocator, usage()),
        else => return err,
    };
    if (!validRouteSpec(spec)) {
        return registry_mod.ToolResult.err(allocator, name ++ ": method must be an HTTP verb and path must start with /\n");
    }

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const absolute = try common.resolveInsideWorkspace(allocator, root, spec.file);
    defer allocator.free(absolute);
    const relative = common.relativeToRoot(root, absolute);

    const source = zigts.file_io.readFile(allocator, absolute, common.default_output_limit) catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": failed to read {s}: {s}\n",
            .{ absolute, @errorName(e) },
        );
    };
    defer allocator.free(source);

    const handler_name = try routeHandlerName(allocator, spec.method, spec.path);
    defer allocator.free(handler_name);

    const proposed = try synthesizeRoute(allocator, source, spec, handler_name);
    defer allocator.free(proposed);

    var analysis = try proof_enrichment.analyzePatch(
        allocator,
        root,
        relative,
        source,
        proposed,
        null,
    );
    defer analysis.deinit(allocator);

    const steps = try buildSteps(allocator, spec, handler_name, source);
    defer {
        for (steps) |*step| step.deinit(allocator);
        allocator.free(steps);
    }

    const verification_summary = try std.fmt.allocPrint(
        allocator,
        "{d} total, {d} new, {d} preexisting",
        .{ analysis.stats.total, analysis.stats.new, analysis.stats.preexisting orelse 0 },
    );
    defer allocator.free(verification_summary);

    const plan_id = try std.fmt.allocPrint(allocator, "route:{s}:{s}", .{ spec.method, spec.path });
    defer allocator.free(plan_id);

    var payload: ui_payload.UiPayload = .{ .feature_plan = try ui_payload.FeaturePlanPayload.init(
        allocator,
        plan_id,
        relative,
        "route",
        spec.method,
        spec.path,
        handler_name,
        steps,
        proposed,
        analysis.unified_diff,
        analysis.stats.new == 0,
        verification_summary,
        analysis.stats,
    ) };
    errdefer payload.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;
    try w.writeAll("{\"ok\":");
    try w.writeAll(if (analysis.stats.new == 0) "true" else "false");
    try w.writeAll(",\"plan_id\":");
    try json_utils.writeJsonString(w, plan_id);
    try w.writeAll(",\"file\":");
    try json_utils.writeJsonString(w, relative);
    try w.writeAll(",\"handler_name\":");
    try json_utils.writeJsonString(w, handler_name);
    try w.writeAll(",\"verification_summary\":");
    try json_utils.writeJsonString(w, verification_summary);
    try w.writeAll(",\"steps\":[");
    for (steps, 0..) |step, i| {
        if (i > 0) try w.writeByte(',');
        try w.writeAll("{\"id\":");
        try json_utils.writeJsonString(w, step.id);
        try w.writeAll(",\"title\":");
        try json_utils.writeJsonString(w, step.title);
        try w.writeAll(",\"detail\":");
        try json_utils.writeJsonString(w, step.detail);
        try w.writeByte('}');
    }
    try w.writeAll("]}\n");
    buf = aw.toArrayList();
    return .{
        .ok = analysis.stats.new == 0,
        .llm_text = try buf.toOwnedSlice(allocator),
        .ui_payload = payload,
    };
}

pub fn usage() []const u8 {
    return name ++ ": usage: /feature route file=<handler.ts> method=<GET|POST|...> path=</path> [body=<schema>] [response=<schema>] [status=<code>]\n";
}

pub fn parseRouteSpecArgs(allocator: std.mem.Allocator, args: []const []const u8) !RouteSpec {
    if (args.len == 0) return error.InvalidToolArgsJson;
    if (args.len == 1 and std.mem.indexOfScalar(u8, args[0], '{') != null) {
        return parseJsonSpec(allocator, args[0]);
    }
    return parseKvSpec(allocator, args);
}

fn parseJsonSpec(allocator: std.mem.Allocator, raw: []const u8) !RouteSpec {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, raw, .{});
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidToolArgsJson;
    const obj = parsed.value.object;
    const kind = ui_payload.getString(obj, "kind") orelse return error.InvalidToolArgsJson;
    if (!std.mem.eql(u8, kind, "route")) return error.InvalidToolArgsJson;
    const status = if (ui_payload.getUnsigned(obj, "status")) |value| blk: {
        if (value < 100 or value > 599) return error.InvalidToolArgsJson;
        break :blk @as(u16, @intCast(value));
    } else 200;
    return .{
        .file = try allocator.dupe(u8, ui_payload.getString(obj, "file") orelse return error.InvalidToolArgsJson),
        .method = try allocator.dupe(u8, ui_payload.getString(obj, "method") orelse return error.InvalidToolArgsJson),
        .path = try allocator.dupe(u8, ui_payload.getString(obj, "path") orelse return error.InvalidToolArgsJson),
        .body_schema = if (ui_payload.getString(obj, "body_schema")) |value| try allocator.dupe(u8, value) else null,
        .response_schema = if (ui_payload.getString(obj, "response_schema")) |value| try allocator.dupe(u8, value) else null,
        .status = status,
    };
}

fn parseKvSpec(allocator: std.mem.Allocator, args: []const []const u8) !RouteSpec {
    var spec: RouteSpec = .{ .file = "", .method = "", .path = "" };
    var start: usize = 0;
    if (std.mem.eql(u8, args[0], "route")) start = 1;
    for (args[start..]) |arg| {
        const eq = std.mem.indexOfScalar(u8, arg, '=') orelse return error.InvalidToolArgsJson;
        const key = arg[0..eq];
        const value = arg[eq + 1 ..];
        if (std.mem.eql(u8, key, "file")) spec.file = try allocator.dupe(u8, value) else if (std.mem.eql(u8, key, "method")) spec.method = try allocator.dupe(u8, value) else if (std.mem.eql(u8, key, "path")) spec.path = try allocator.dupe(u8, value) else if (std.mem.eql(u8, key, "body") or std.mem.eql(u8, key, "body_schema")) spec.body_schema = try allocator.dupe(u8, value) else if (std.mem.eql(u8, key, "response") or std.mem.eql(u8, key, "response_schema")) spec.response_schema = try allocator.dupe(u8, value) else if (std.mem.eql(u8, key, "status")) {
            const parsed = try std.fmt.parseInt(u16, value, 10);
            if (parsed < 100 or parsed > 599) return error.InvalidToolArgsJson;
            spec.status = parsed;
        } else return error.InvalidToolArgsJson;
    }
    if (spec.file.len == 0 or spec.method.len == 0 or spec.path.len == 0) return error.InvalidToolArgsJson;
    return spec;
}

pub fn validRouteSpec(spec: RouteSpec) bool {
    return validMethod(spec.method) and spec.path.len > 0 and spec.path[0] == '/';
}

fn validMethod(method: []const u8) bool {
    const methods = [_][]const u8{ "GET", "POST", "PUT", "PATCH", "DELETE", "HEAD", "OPTIONS" };
    for (methods) |candidate| {
        if (std.ascii.eqlIgnoreCase(method, candidate)) return true;
    }
    return false;
}

pub fn routeHandlerName(allocator: std.mem.Allocator, method: []const u8, path: []const u8) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    try out.appendSlice(allocator, "handle");
    try appendPascalToken(allocator, &out, method);
    var it = std.mem.tokenizeAny(u8, path, "/-_:{}");
    while (it.next()) |part| try appendPascalToken(allocator, &out, part);
    if (out.items.len == "handle".len + method.len) try out.appendSlice(allocator, "Root");
    return try out.toOwnedSlice(allocator);
}

fn appendPascalToken(allocator: std.mem.Allocator, out: *std.ArrayList(u8), token: []const u8) !void {
    var upper_next = true;
    for (token) |ch| {
        if (!std.ascii.isAlphanumeric(ch)) {
            upper_next = true;
            continue;
        }
        if (upper_next) {
            try out.append(allocator, std.ascii.toUpper(ch));
            upper_next = false;
        } else {
            try out.append(allocator, std.ascii.toLower(ch));
        }
    }
}

pub fn synthesizeRoute(
    allocator: std.mem.Allocator,
    source: []const u8,
    spec: RouteSpec,
    handler_name: []const u8,
) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);

    const has_router = std.mem.indexOf(u8, source, "zigttp:router") != null;
    const has_routes = std.mem.indexOf(u8, source, "const routes = {") != null;

    if (!has_router) {
        try out.appendSlice(allocator, "import { routerMatch } from \"zigttp:router\";\n");
    }
    if (spec.body_schema != null and std.mem.indexOf(u8, source, "zigttp:validate") == null) {
        try out.appendSlice(allocator, "import { validateJson } from \"zigttp:validate\";\n");
    }
    if (out.items.len > 0 and source.len > 0) try out.append(allocator, '\n');

    const stripped_handler: ?[]u8 = if (!has_router) try stripFunctionHandlerAlloc(allocator, source) else null;
    defer if (stripped_handler) |bytes| allocator.free(bytes);
    const source_without_handler = stripped_handler orelse source;

    if (has_routes) {
        try appendWithRouteEntry(allocator, &out, source_without_handler, spec, handler_name);
    } else {
        try out.appendSlice(allocator, source_without_handler);
        if (source_without_handler.len > 0 and source_without_handler[source_without_handler.len - 1] != '\n') try out.append(allocator, '\n');
        try out.append(allocator, '\n');
        try out.print(allocator, "const routes = {{\n    \"{s} {s}\": {s},\n}};\n\n", .{ spec.method, spec.path, handler_name });
    }

    if (!has_router) {
        try out.appendSlice(allocator,
            \\function handler(req) {
            \\    const found = routerMatch(routes, req);
            \\    if (found !== undefined) {
            \\        req.params = found.params;
            \\        return found.handler(req);
            \\    }
            \\    return Response.json({ error: "not_found" }, { status: 404 });
            \\}
            \\
            \\
        );
    } else {
        try out.append(allocator, '\n');
    }
    try out.print(allocator, "function {s}(req) {{\n", .{handler_name});
    if (spec.body_schema) |schema| {
        try out.print(
            allocator,
            "    const body = validateJson(\"{s}\", req.body);\n" ++
                "    if (!body.ok) return Response.json({{ errors: body.errors }}, {{ status: 400 }});\n" ++
                "    return Response.json({{ ok: true, data: body.value }}, {{ status: {d} }});\n",
            .{ schema, spec.status },
        );
    } else {
        try out.print(allocator, "    return Response.json({{ ok: true }}, {{ status: {d} }});\n", .{spec.status});
    }
    try out.appendSlice(allocator, "}\n");

    return try out.toOwnedSlice(allocator);
}

fn appendWithRouteEntry(
    allocator: std.mem.Allocator,
    out: *std.ArrayList(u8),
    source: []const u8,
    spec: RouteSpec,
    handler_name: []const u8,
) !void {
    const marker = "const routes = {";
    const start = std.mem.indexOf(u8, source, marker) orelse {
        try out.appendSlice(allocator, source);
        return;
    };
    const close = std.mem.indexOfPos(u8, source, start + marker.len, "};") orelse {
        try out.appendSlice(allocator, source);
        return;
    };
    try out.appendSlice(allocator, source[0..close]);
    if (close > 0 and source[close - 1] != '\n') try out.append(allocator, '\n');
    try out.print(allocator, "    \"{s} {s}\": {s},\n", .{ spec.method, spec.path, handler_name });
    try out.appendSlice(allocator, source[close..]);
    if (source.len > 0 and source[source.len - 1] != '\n') try out.append(allocator, '\n');
}

fn stripFunctionHandlerAlloc(allocator: std.mem.Allocator, source: []const u8) !?[]u8 {
    const start = std.mem.indexOf(u8, source, "function handler(") orelse return null;
    const open = std.mem.indexOfScalarPos(u8, source, start, '{') orelse return null;
    var depth: usize = 0;
    var i = open;
    while (i < source.len) : (i += 1) {
        switch (source[i]) {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if (depth == 0) {
                    var end = i + 1;
                    if (end < source.len and source[end] == '\n') end += 1;
                    const out = try allocator.alloc(u8, source.len - (end - start));
                    @memcpy(out[0..start], source[0..start]);
                    @memcpy(out[start .. start + source.len - end], source[end..]);
                    return out;
                }
            },
            else => {},
        }
    }
    return null;
}

pub fn buildSteps(
    allocator: std.mem.Allocator,
    spec: RouteSpec,
    handler_name: []const u8,
    source: []const u8,
) ![]ui_payload.FeaturePlanStep {
    var steps: std.ArrayList(ui_payload.FeaturePlanStep) = .empty;
    errdefer {
        for (steps.items) |*step| step.deinit(allocator);
        steps.deinit(allocator);
    }
    if (std.mem.indexOf(u8, source, "zigttp:router") == null) {
        try steps.append(allocator, try ui_payload.FeaturePlanStep.init(allocator, "ensure_router", "ensure router import", "Add routerMatch from zigttp:router."));
    }
    try steps.append(allocator, try ui_payload.FeaturePlanStep.init(allocator, "add_route_entry", "add route table entry", "Bind the method/path pair to the generated handler."));
    const handler_detail = try std.fmt.allocPrint(allocator, "Generate {s} for {s} {s}.", .{ handler_name, spec.method, spec.path });
    defer allocator.free(handler_detail);
    try steps.append(allocator, try ui_payload.FeaturePlanStep.init(allocator, "add_handler_fn", "add route handler", handler_detail));
    if (spec.body_schema) |schema| {
        const detail = try std.fmt.allocPrint(allocator, "Validate req.body with schema '{s}' before reading value.", .{schema});
        defer allocator.free(detail);
        try steps.append(allocator, try ui_payload.FeaturePlanStep.init(allocator, "validate_body", "validate request body", detail));
    }
    return try steps.toOwnedSlice(allocator);
}

const testing = std.testing;

test "routeHandlerName derives stable names" {
    const name1 = try routeHandlerName(testing.allocator, "POST", "/items/:id");
    defer testing.allocator.free(name1);
    try testing.expectEqualStrings("handlePostItemsId", name1);
}

test "synthesizeRoute replaces a plain handler with router dispatch" {
    const source =
        \\function helper() {
        \\    return "ok";
        \\}
        \\
        \\function handler(req) {
        \\    return Response.json({ old: helper() });
        \\}
        \\
    ;
    const handler_name = try routeHandlerName(testing.allocator, "POST", "/items");
    defer testing.allocator.free(handler_name);
    const proposed = try synthesizeRoute(testing.allocator, source, .{
        .file = "handler.ts",
        .method = "POST",
        .path = "/items",
        .body_schema = null,
        .status = 201,
    }, handler_name);
    defer testing.allocator.free(proposed);

    try testing.expect(std.mem.indexOf(u8, proposed, "import { routerMatch }") != null);
    try testing.expect(std.mem.indexOf(u8, proposed, "\"POST /items\": handlePostItems") != null);
    try testing.expectEqual(@as(?usize, null), std.mem.indexOf(u8, proposed, "{ old: helper() }"));

    var result = try @import("zigts_cli").edit_simulate.simulate(testing.allocator, .{
        .file = "handler.ts",
        .content = proposed,
        .before = source,
    });
    defer result.deinit(testing.allocator);
    try testing.expectEqual(@as(u32, 0), result.new_count);
}
