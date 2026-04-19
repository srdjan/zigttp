//! zigttp:router - Pattern-matching HTTP router

const std = @import("std");
const sdk = @import("zigttp-sdk");

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:router",
    .name = "router",
    .exports = &.{
        .{
            .name = "routerMatch",
            .module_func = routerMatchImpl,
            .arg_count = 2,
            .returns = .optional_object,
            .param_types = &.{ .object, .object },
            .traceable = false,
            .failure_severity = .expected,
            .contract_extractions = &.{.{ .category = .route_pattern }},
            .return_labels = .{ .user_input = true },
        },
    },
};

fn routerMatchImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.JSValue.undefined_val;

    const routes_val = args[0];
    const req_val = args[1];

    if (!sdk.isObject(routes_val) or !sdk.isObject(req_val)) return sdk.JSValue.undefined_val;

    const method_val = sdk.objectGet(handle, req_val, "method") orelse return sdk.JSValue.undefined_val;
    const method_str = sdk.extractString(method_val) orelse return sdk.JSValue.undefined_val;

    const path_val = sdk.objectGet(handle, req_val, "url") orelse
        sdk.objectGet(handle, req_val, "path") orelse return sdk.JSValue.undefined_val;
    const full_path = sdk.extractString(path_val) orelse return sdk.JSValue.undefined_val;

    const path_str = if (std.mem.indexOfScalar(u8, full_path, '?')) |qi| full_path[0..qi] else full_path;

    const keys = sdk.objectKeys(handle, routes_val) catch return sdk.JSValue.undefined_val;
    const key_count = sdk.arrayLength(keys) orelse 0;

    var i: u32 = 0;
    while (i < key_count) : (i += 1) {
        const key_val = sdk.arrayGet(handle, keys, i) orelse continue;
        const route_key = sdk.extractString(key_val) orelse continue;

        if (matchRoute(route_key, method_str, path_str)) |match_result| {
            const handler_val = sdk.objectGet(handle, routes_val, route_key) orelse continue;

            const params_obj = sdk.createObject(handle) catch return sdk.JSValue.undefined_val;
            for (0..match_result.count) |pi| {
                const param = match_result.params[pi];
                const val_str = sdk.createString(handle, param.value) catch continue;
                sdk.objectSet(handle, params_obj, param.name, val_str) catch continue;
            }

            const result_obj = sdk.createObject(handle) catch return sdk.JSValue.undefined_val;
            sdk.objectSet(handle, result_obj, "handler", handler_val) catch return sdk.JSValue.undefined_val;
            sdk.objectSet(handle, result_obj, "params", params_obj) catch return sdk.JSValue.undefined_val;
            return result_obj;
        }
    }

    return sdk.JSValue.undefined_val;
}

const Param = struct { name: []const u8, value: []const u8 };
const MatchResult = struct { params: [8]Param, count: u8 };

fn matchRoute(route_key: []const u8, method: []const u8, path: []const u8) ?MatchResult {
    const space_idx = std.mem.indexOfScalar(u8, route_key, ' ') orelse return null;
    const route_method = route_key[0..space_idx];
    const route_pattern = route_key[space_idx + 1 ..];

    if (!std.ascii.eqlIgnoreCase(route_method, method)) return null;

    var result = MatchResult{ .params = undefined, .count = 0 };

    var pattern_iter = std.mem.splitScalar(u8, route_pattern, '/');
    var path_iter = std.mem.splitScalar(u8, path, '/');

    while (true) {
        const pattern_seg = pattern_iter.next();
        const path_seg = path_iter.next();

        if (pattern_seg == null and path_seg == null) return result;
        if (pattern_seg == null or path_seg == null) return null;

        const ps = pattern_seg.?;
        const vs = path_seg.?;

        if (ps.len > 0 and ps[0] == ':') {
            if (result.count >= 8) return null;
            result.params[result.count] = .{ .name = ps[1..], .value = vs };
            result.count += 1;
        } else if (ps.len > 0 and ps[0] == '*') {
            return result;
        } else {
            if (!std.mem.eql(u8, ps, vs)) return null;
        }
    }
}

test "matchRoute exact" {
    const r = matchRoute("GET /", "GET", "/").?;
    try std.testing.expectEqual(@as(u8, 0), r.count);
}

test "matchRoute param" {
    const r = matchRoute("GET /users/:id", "GET", "/users/42").?;
    try std.testing.expectEqual(@as(u8, 1), r.count);
    try std.testing.expectEqualStrings("id", r.params[0].name);
    try std.testing.expectEqualStrings("42", r.params[0].value);
}

test "matchRoute method mismatch" {
    try std.testing.expect(matchRoute("GET /users", "POST", "/users") == null);
}

test "matchRoute wildcard" {
    const r = matchRoute("GET /static/*", "GET", "/static/a/b/c").?;
    _ = r;
}
