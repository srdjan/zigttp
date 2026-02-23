//! zigttp:router - Pattern-matching HTTP router
//!
//! Exports:
//!   routerMatch(routes: object, req: Request) -> { handler: function, params: object } | null
//!     Matches req.method + req.url against route patterns.
//!     Route keys are "METHOD /path/:param" strings.
//!     Returns the matched handler and extracted params, or null if no route matches.
//!
//! Usage pattern:
//!   import { routerMatch } from "zigttp:router";
//!
//!   const routes = {
//!     "GET /":          (req) => Response.text("home"),
//!     "GET /users/:id": (req) => Response.json({ id: req.params.id }),
//!     "POST /users":    (req) => Response.json({ created: true }, { status: 201 }),
//!   };
//!
//!   function handler(req) {
//!     const match = routerMatch(routes, req);
//!     if (match) {
//!       req.params = match.params;
//!       return match.handler(req);
//!     }
//!     return Response.json({ error: "Not Found" }, { status: 404 });
//!   }

const std = @import("std");
const object = @import("../object.zig");
const context = @import("../context.zig");
const value = @import("../value.zig");
const resolver = @import("resolver.zig");
const util = @import("util.zig");

/// Module exports
pub const exports = [_]resolver.ModuleExport{
    .{ .name = "routerMatch", .func = routerMatchNative, .arg_count = 2 },
};


/// routerMatch(routes, req) -> { handler, params } | null
fn routerMatchNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len < 2) return value.JSValue.null_val;

    const routes_val = args[0];
    const req_val = args[1];

    // Get the routes object
    if (!routes_val.isObject()) return value.JSValue.null_val;
    const routes_obj = routes_val.toPtr(object.JSObject);

    // Get request object
    if (!req_val.isObject()) return value.JSValue.null_val;
    const req_obj = req_val.toPtr(object.JSObject);

    const pool = ctx.hidden_class_pool orelse return value.JSValue.null_val;

    const method_val = req_obj.getProperty(pool, .method) orelse return value.JSValue.null_val;
    const method_str = util.extractString(method_val) orelse return value.JSValue.null_val;

    // Try url first (full URL path), fall back to path
    const path_val = req_obj.getProperty(pool, .url) orelse
        (req_obj.getProperty(pool, .path) orelse return value.JSValue.null_val);
    const full_path = util.extractString(path_val) orelse return value.JSValue.null_val;

    // Strip query string for matching
    const path_str = if (std.mem.indexOfScalar(u8, full_path, '?')) |qi| full_path[0..qi] else full_path;

    // Get route property names and iterate
    const keys = routes_obj.getOwnEnumerableKeys(ctx.allocator, pool) catch return value.JSValue.null_val;
    defer ctx.allocator.free(keys);

    for (keys) |atom| {
        // Get route key string from atom
        const route_key = atomToString(atom, &ctx.atoms) orelse continue;

        if (matchRoute(route_key, method_str, path_str)) |match_result| {
            // Get the handler function value
            const handler_val = routes_obj.getProperty(pool, atom) orelse continue;

            // Create params object
            const params_obj = object.JSObject.create(ctx.allocator, ctx.root_class_idx, null, pool) catch return value.JSValue.null_val;

            for (0..match_result.count) |pi| {
                const param = match_result.params[pi];
                const name_atom = ctx.atoms.intern(param.name) catch continue;
                const val_str = ctx.createString(param.value) catch continue;
                params_obj.setProperty(ctx.allocator, pool, name_atom, val_str) catch continue;
            }

            // Create result object { handler, params }
            const result_obj = object.JSObject.create(ctx.allocator, ctx.root_class_idx, null, pool) catch return value.JSValue.null_val;
            const handler_atom = ctx.atoms.intern("handler") catch return value.JSValue.null_val;
            const params_atom = ctx.atoms.intern("params") catch return value.JSValue.null_val;

            result_obj.setProperty(ctx.allocator, pool, handler_atom, handler_val) catch return value.JSValue.null_val;
            result_obj.setProperty(ctx.allocator, pool, params_atom, params_obj.toValue()) catch return value.JSValue.null_val;

            return result_obj.toValue();
        }
    }

    // No route matched
    return value.JSValue.null_val;
}

/// Get the string name for an atom (predefined or dynamic)
fn atomToString(atom: object.Atom, atoms: *context.AtomTable) ?[]const u8 {
    if (atom.isPredefined()) {
        return atom.toPredefinedName();
    }
    return atoms.getName(atom);
}

/// Parsed path parameter
const Param = struct {
    name: []const u8,
    value: []const u8,
};

/// Result of route matching
const MatchResult = struct {
    params: [8]Param,
    count: u8,
};

/// Match a route key like "GET /users/:id" against method + path.
/// Returns path parameters if matched, null if not.
fn matchRoute(route_key: []const u8, method: []const u8, path: []const u8) ?MatchResult {
    // Split route key into method and pattern
    const space_idx = std.mem.indexOfScalar(u8, route_key, ' ') orelse return null;
    const route_method = route_key[0..space_idx];
    const route_pattern = route_key[space_idx + 1 ..];

    // Method must match (case-insensitive)
    if (!std.ascii.eqlIgnoreCase(route_method, method)) return null;

    // Match path segments against pattern segments
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
            // Parameter segment
            if (result.count >= 8) return null;
            result.params[result.count] = .{
                .name = ps[1..],
                .value = vs,
            };
            result.count += 1;
        } else if (ps.len > 0 and ps[0] == '*') {
            // Wildcard - matches rest
            return result;
        } else {
            // Exact match
            if (!std.mem.eql(u8, ps, vs)) return null;
        }
    }
}
