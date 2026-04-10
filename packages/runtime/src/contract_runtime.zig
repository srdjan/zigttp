//! Runtime contract: lightweight parser for the embedded contract JSON.
//!
//! The compile-time contract extraction (handler_contract.zig) proves handler
//! properties, env vars, routes, and egress hosts. That contract is embedded in
//! self-extracting binaries but was previously unused at runtime ("v1 emission
//! only"). This module makes the runtime contract-aware: startup env validation,
//! route pre-filtering, and property-driven behavior.

const std = @import("std");
const zq = @import("zigts");
const HandlerContract = zq.HandlerContract;
const HandlerProperties = zq.handler_contract.HandlerProperties;

/// Proven handler properties extracted from the contract.
/// Each field is a mathematical proof, not an annotation.
pub const Properties = struct {
    pure: bool = false,
    read_only: bool = false,
    stateless: bool = false,
    retry_safe: bool = false,
    deterministic: bool = false,
    has_egress: bool = false,
    no_secret_leakage: bool = false,
    no_credential_leakage: bool = false,
    input_validated: bool = false,
    pii_contained: bool = false,
    injection_safe: bool = false,
    idempotent: bool = false,
    state_isolated: bool = false,
    max_io_depth: ?u32 = null,
    fault_covered: bool = false,
    result_safe: bool = false,
    optional_safe: bool = false,
};

/// A proven API route (method + path pattern).
pub const Route = struct {
    method: []const u8,
    path: []const u8,
};

/// Runtime view of the proven contract.
/// Owns all allocated memory; call deinit() when done.
pub const RuntimeContract = struct {
    env_vars: []const []const u8,
    env_dynamic: bool,
    routes: []const Route,
    routes_dynamic: bool,
    properties: Properties,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *RuntimeContract) void {
        for (self.env_vars) |v| self.allocator.free(v);
        self.allocator.free(self.env_vars);
        for (self.routes) |r| {
            self.allocator.free(r.method);
            self.allocator.free(r.path);
        }
        self.allocator.free(self.routes);
    }

    /// Check if a request method+path matches any proven route.
    /// Returns true if routes are dynamic (can't pre-filter) or if a match is found.
    pub fn matchesRoute(self: *const RuntimeContract, method: []const u8, path: []const u8) bool {
        if (self.routes_dynamic) return true;
        if (self.routes.len == 0) return true; // no route info extracted
        for (self.routes) |route| {
            if (std.ascii.eqlIgnoreCase(route.method, method) and matchPath(route.path, path)) {
                return true;
            }
        }
        return false;
    }
};

/// Parse a contract JSON blob into a RuntimeContract.
/// Extracts only the fields needed at runtime; ignores everything else.
pub fn parseContractJson(allocator: std.mem.Allocator, source: []const u8) !RuntimeContract {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, source, .{});
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidContract;
    const root = parsed.value.object;

    // Parse env.literal[]
    var env_vars: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (env_vars.items) |v| allocator.free(v);
        env_vars.deinit(allocator);
    }
    var env_dynamic = false;
    if (root.get("env")) |env_val| {
        if (env_val == .object) {
            const env_obj = env_val.object;
            if (env_obj.get("literal")) |literal_val| {
                if (literal_val == .array) {
                    for (literal_val.array.items) |item| {
                        if (item == .string) {
                            try env_vars.append(allocator, try allocator.dupe(u8, item.string));
                        }
                    }
                }
            }
            if (env_obj.get("dynamic")) |dyn_val| {
                env_dynamic = dyn_val == .bool and dyn_val.bool;
            }
        }
    }

    // Parse api.routes[] for method+path
    var routes: std.ArrayList(Route) = .empty;
    errdefer {
        for (routes.items) |r| {
            allocator.free(r.method);
            allocator.free(r.path);
        }
        routes.deinit(allocator);
    }
    var routes_dynamic = false;
    if (root.get("api")) |api_val| {
        if (api_val == .object) {
            const api_obj = api_val.object;
            if (api_obj.get("routesDynamic")) |dyn_val| {
                routes_dynamic = dyn_val == .bool and dyn_val.bool;
            }
            if (api_obj.get("routes")) |routes_val| {
                if (routes_val == .array) {
                    for (routes_val.array.items) |route_val| {
                        if (route_val != .object) continue;
                        const route_obj = route_val.object;
                        const method_val = route_obj.get("method") orelse continue;
                        const path_val = route_obj.get("path") orelse continue;
                        if (method_val != .string or path_val != .string) continue;
                        try routes.append(allocator, .{
                            .method = try allocator.dupe(u8, method_val.string),
                            .path = try allocator.dupe(u8, path_val.string),
                        });
                    }
                }
            }
        }
    }

    // Parse properties
    const properties = parseProperties(root);

    return .{
        .env_vars = try env_vars.toOwnedSlice(allocator),
        .env_dynamic = env_dynamic,
        .routes = try routes.toOwnedSlice(allocator),
        .routes_dynamic = routes_dynamic,
        .properties = properties,
        .allocator = allocator,
    };
}

fn parseProperties(root: std.json.ObjectMap) Properties {
    var props = Properties{};
    const prop_val = root.get("properties") orelse return props;
    if (prop_val != .object) return props;
    const obj = prop_val.object;

    props.pure = getBool(obj, "pure");
    props.read_only = getBool(obj, "readOnly");
    props.stateless = getBool(obj, "stateless");
    props.retry_safe = getBool(obj, "retrySafe");
    props.deterministic = getBool(obj, "deterministic");
    props.has_egress = getBool(obj, "hasEgress");
    props.no_secret_leakage = getBool(obj, "noSecretLeakage");
    props.no_credential_leakage = getBool(obj, "noCredentialLeakage");
    props.input_validated = getBool(obj, "inputValidated");
    props.pii_contained = getBool(obj, "piiContained");
    props.injection_safe = getBool(obj, "injectionSafe");
    props.idempotent = getBool(obj, "idempotent");
    props.state_isolated = getBool(obj, "stateIsolated");
    props.fault_covered = getBool(obj, "faultCovered");
    props.result_safe = getBool(obj, "resultSafe");
    props.optional_safe = getBool(obj, "optionalSafe");

    if (obj.get("maxIoDepth")) |depth_val| {
        if (depth_val == .integer) {
            props.max_io_depth = @intCast(depth_val.integer);
        }
    }

    return props;
}

fn getBool(obj: std.json.ObjectMap, key: []const u8) bool {
    const val = obj.get(key) orelse return false;
    return val == .bool and val.bool;
}

/// Match a route pattern (e.g. "/users/:id") against a request path (e.g. "/users/42").
/// Supports :param segments as wildcards.
fn matchPath(pattern: []const u8, path: []const u8) bool {
    var pat_iter = std.mem.splitScalar(u8, pattern, '/');
    var path_iter = std.mem.splitScalar(u8, path, '/');

    while (true) {
        const pat_seg = pat_iter.next();
        const path_seg = path_iter.next();

        if (pat_seg == null and path_seg == null) return true;
        if (pat_seg == null or path_seg == null) return false;

        const ps = pat_seg.?;
        const rs = path_seg.?;

        // :param is a wildcard segment
        if (ps.len > 0 and ps[0] == ':') continue;
        // * is a catch-all
        if (std.mem.eql(u8, ps, "*")) return true;

        if (!std.mem.eql(u8, ps, rs)) return false;
    }
}

/// Validate that all proven env vars are set. Returns list of missing var names.
pub fn validateEnvVars(allocator: std.mem.Allocator, contract: *const RuntimeContract) ![]const []const u8 {
    var missing: std.ArrayList([]const u8) = .empty;
    errdefer missing.deinit(allocator);

    for (contract.env_vars) |name| {
        const name_z = try allocator.dupeZ(u8, name);
        defer allocator.free(name_z);
        if (std.c.getenv(name_z) == null) {
            try missing.append(allocator, name);
        }
    }

    return try missing.toOwnedSlice(allocator);
}

/// Build a RuntimeContract directly from an engine HandlerContract.
/// Allocates all strings with the given allocator. Caller owns the result.
pub fn fromHandlerContract(allocator: std.mem.Allocator, hc: *const HandlerContract) !RuntimeContract {
    // Copy env vars
    var env_vars: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (env_vars.items) |v| allocator.free(v);
        env_vars.deinit(allocator);
    }
    for (hc.env.literal.items) |s| {
        try env_vars.append(allocator, try allocator.dupe(u8, s));
    }

    // Copy API routes (method + path)
    var routes: std.ArrayList(Route) = .empty;
    errdefer {
        for (routes.items) |r| {
            allocator.free(r.method);
            allocator.free(r.path);
        }
        routes.deinit(allocator);
    }
    for (hc.api.routes.items) |api_route| {
        try routes.append(allocator, .{
            .method = try allocator.dupe(u8, api_route.method),
            .path = try allocator.dupe(u8, api_route.path),
        });
    }

    // Convert properties
    const hp = hc.properties orelse HandlerProperties{
        .pure = false,
        .read_only = false,
        .stateless = false,
        .retry_safe = false,
        .deterministic = false,
        .has_egress = false,
    };

    return .{
        .env_vars = try env_vars.toOwnedSlice(allocator),
        .env_dynamic = hc.env.dynamic,
        .routes = try routes.toOwnedSlice(allocator),
        .routes_dynamic = hc.api.routes_dynamic,
        .properties = .{
            .pure = hp.pure,
            .read_only = hp.read_only,
            .stateless = hp.stateless,
            .retry_safe = hp.retry_safe,
            .deterministic = hp.deterministic,
            .has_egress = hp.has_egress,
            .no_secret_leakage = hp.no_secret_leakage,
            .no_credential_leakage = hp.no_credential_leakage,
            .input_validated = hp.input_validated,
            .pii_contained = hp.pii_contained,
            .injection_safe = hp.injection_safe,
            .idempotent = hp.idempotent,
            .state_isolated = hp.state_isolated,
            .max_io_depth = hp.max_io_depth,
            .fault_covered = hp.fault_covered,
            .result_safe = hp.result_safe,
            .optional_safe = hp.optional_safe,
        },
        .allocator = allocator,
    };
}

// ============================================================================
// Tests
// ============================================================================

test "parseContractJson minimal" {
    const allocator = std.testing.allocator;
    const source =
        \\{
        \\  "version": 10,
        \\  "handler": {"path": "handler.ts", "line": 1, "column": 0},
        \\  "routes": [],
        \\  "modules": [],
        \\  "functions": {},
        \\  "env": {"literal": ["JWT_SECRET", "DB_URL"], "dynamic": false},
        \\  "egress": {"hosts": [], "dynamic": false},
        \\  "serviceCalls": [],
        \\  "cache": {"namespaces": [], "dynamic": false},
        \\  "sql": {"backend": "sqlite", "queries": [], "dynamic": false},
        \\  "durable": {"used": false, "keys": {"literal": [], "dynamic": false}, "steps": [], "timers": false, "signals": {"literal": [], "dynamic": false}, "producerKeys": {"literal": [], "dynamic": false}},
        \\  "api": {"schemas": [], "requests": {"schemaRefs": [], "dynamic": false}, "auth": {"bearer": false, "jwt": false}, "routes": [], "schemasDynamic": false, "routesDynamic": false},
        \\  "verification": null,
        \\  "aot": null,
        \\  "faultCoverage": null,
        \\  "rateLimiting": null,
        \\  "properties": null,
        \\  "behaviors": [],
        \\  "behaviorsExhaustive": false
        \\}
    ;

    var contract = try parseContractJson(allocator, source);
    defer contract.deinit();

    try std.testing.expectEqual(@as(usize, 2), contract.env_vars.len);
    try std.testing.expectEqualStrings("JWT_SECRET", contract.env_vars[0]);
    try std.testing.expectEqualStrings("DB_URL", contract.env_vars[1]);
    try std.testing.expect(!contract.env_dynamic);
    try std.testing.expectEqual(@as(usize, 0), contract.routes.len);
    try std.testing.expect(!contract.properties.pure);
}

test "parseContractJson with properties and routes" {
    const allocator = std.testing.allocator;
    const source =
        \\{
        \\  "version": 10,
        \\  "handler": {"path": "handler.ts", "line": 1, "column": 0},
        \\  "routes": [],
        \\  "modules": [],
        \\  "functions": {},
        \\  "env": {"literal": ["API_KEY"], "dynamic": true},
        \\  "egress": {"hosts": ["api.stripe.com"], "dynamic": false},
        \\  "serviceCalls": [],
        \\  "cache": {"namespaces": [], "dynamic": false},
        \\  "sql": {"backend": "sqlite", "queries": [], "dynamic": false},
        \\  "durable": {"used": false, "keys": {"literal": [], "dynamic": false}, "steps": [], "timers": false, "signals": {"literal": [], "dynamic": false}, "producerKeys": {"literal": [], "dynamic": false}},
        \\  "api": {"schemas": [], "requests": {"schemaRefs": [], "dynamic": false}, "auth": {"bearer": true, "jwt": true}, "routes": [{"method": "GET", "path": "/health", "requestSchemaRefs": [], "requestSchemaDynamic": false, "requiresBearer": false, "requiresJwt": false, "pathParams": [], "queryParams": [], "headerParams": [], "queryParamsDynamic": false, "headerParamsDynamic": false, "requestBodies": [], "requestBodiesDynamic": false, "responses": [], "responsesDynamic": false, "responseStatus": 200, "responseContentType": "application/json", "responseSchemaRef": null, "responseSchema": null, "responseSchemaDynamic": false}, {"method": "POST", "path": "/users/:id", "requestSchemaRefs": [], "requestSchemaDynamic": false, "requiresBearer": true, "requiresJwt": true, "pathParams": [], "queryParams": [], "headerParams": [], "queryParamsDynamic": false, "headerParamsDynamic": false, "requestBodies": [], "requestBodiesDynamic": false, "responses": [], "responsesDynamic": false, "responseStatus": 201, "responseContentType": "application/json", "responseSchemaRef": null, "responseSchema": null, "responseSchemaDynamic": false}], "schemasDynamic": false, "routesDynamic": false},
        \\  "verification": {"exhaustiveReturns": true, "resultsSafe": true, "unreachableCode": true, "bytecodeVerified": true},
        \\  "aot": null,
        \\  "faultCoverage": null,
        \\  "rateLimiting": null,
        \\  "properties": {"pure": false, "readOnly": true, "stateless": true, "retrySafe": true, "deterministic": true, "hasEgress": true, "noSecretLeakage": true, "noCredentialLeakage": true, "inputValidated": false, "piiContained": false, "injectionSafe": true, "idempotent": false, "stateIsolated": true, "maxIoDepth": 3, "faultCovered": true, "resultSafe": true, "optionalSafe": true},
        \\  "behaviors": [],
        \\  "behaviorsExhaustive": false
        \\}
    ;

    var contract = try parseContractJson(allocator, source);
    defer contract.deinit();

    try std.testing.expectEqual(@as(usize, 1), contract.env_vars.len);
    try std.testing.expectEqualStrings("API_KEY", contract.env_vars[0]);
    try std.testing.expect(contract.env_dynamic);

    try std.testing.expectEqual(@as(usize, 2), contract.routes.len);
    try std.testing.expectEqualStrings("GET", contract.routes[0].method);
    try std.testing.expectEqualStrings("/health", contract.routes[0].path);
    try std.testing.expectEqualStrings("POST", contract.routes[1].method);
    try std.testing.expectEqualStrings("/users/:id", contract.routes[1].path);
    try std.testing.expect(!contract.routes_dynamic);

    const p = contract.properties;
    try std.testing.expect(!p.pure);
    try std.testing.expect(p.read_only);
    try std.testing.expect(p.stateless);
    try std.testing.expect(p.retry_safe);
    try std.testing.expect(p.deterministic);
    try std.testing.expect(p.has_egress);
    try std.testing.expect(p.no_secret_leakage);
    try std.testing.expect(p.injection_safe);
    try std.testing.expect(p.state_isolated);
    try std.testing.expectEqual(@as(u32, 3), p.max_io_depth.?);
    try std.testing.expect(p.fault_covered);
    try std.testing.expect(p.result_safe);
    try std.testing.expect(p.optional_safe);
}

test "matchPath exact" {
    try std.testing.expect(matchPath("/health", "/health"));
    try std.testing.expect(!matchPath("/health", "/users"));
    try std.testing.expect(!matchPath("/health", "/health/extra"));
}

test "matchPath with params" {
    try std.testing.expect(matchPath("/users/:id", "/users/42"));
    try std.testing.expect(matchPath("/users/:id", "/users/abc"));
    try std.testing.expect(!matchPath("/users/:id", "/users/42/extra"));
    try std.testing.expect(!matchPath("/users/:id", "/users"));
}

test "matchPath catch-all" {
    try std.testing.expect(matchPath("/api/*", "/api/anything"));
    try std.testing.expect(matchPath("/api/*", "/api/deep/path"));
}

test "matchesRoute with proven routes" {
    const allocator = std.testing.allocator;
    const method1 = try allocator.dupe(u8, "GET");
    const path1 = try allocator.dupe(u8, "/health");
    const method2 = try allocator.dupe(u8, "POST");
    const path2 = try allocator.dupe(u8, "/users/:id");
    var routes_buf = [_]Route{
        .{ .method = method1, .path = path1 },
        .{ .method = method2, .path = path2 },
    };

    var contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &routes_buf,
        .routes_dynamic = false,
        .properties = .{},
        .allocator = allocator,
    };

    try std.testing.expect(contract.matchesRoute("GET", "/health"));
    try std.testing.expect(contract.matchesRoute("POST", "/users/42"));
    try std.testing.expect(!contract.matchesRoute("DELETE", "/health"));
    try std.testing.expect(!contract.matchesRoute("GET", "/unknown"));

    // Don't call deinit - we used stack buffers, just free the duped strings
    allocator.free(method1);
    allocator.free(path1);
    allocator.free(method2);
    allocator.free(path2);
}

test "matchesRoute with dynamic routes passes everything" {
    var contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &.{},
        .routes_dynamic = true,
        .properties = .{},
        .allocator = std.testing.allocator,
    };

    try std.testing.expect(contract.matchesRoute("GET", "/anything"));
    try std.testing.expect(contract.matchesRoute("DELETE", "/whatever"));
}

test "matchesRoute with no routes passes everything" {
    var contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &.{},
        .routes_dynamic = false,
        .properties = .{},
        .allocator = std.testing.allocator,
    };

    try std.testing.expect(contract.matchesRoute("GET", "/anything"));
}

test "fromHandlerContract converts properties and env vars" {
    const allocator = std.testing.allocator;

    // Build a minimal HandlerContract
    var hc = HandlerContract{
        .handler = .{ .path = try allocator.dupe(u8, "handler.ts"), .line = 1, .column = 0 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{
            .literal = .empty,
            .dynamic = false,
        },
        .egress = .{
            .hosts = .empty,
            .urls = .empty,
            .dynamic = false,
        },
        .cache = .{
            .namespaces = .empty,
            .dynamic = false,
        },
        .sql = .{
            .backend = "sqlite",
            .queries = .empty,
            .dynamic = false,
        },
        .durable = .{
            .used = false,
            .keys = .{ .literal = .empty, .dynamic = false },
            .steps = .empty,
            .timers = false,
            .signals = .{ .literal = .empty, .dynamic = false },
            .producer_keys = .{ .literal = .empty, .dynamic = false },
        },
        .api = .{
            .schemas = .empty,
            .requests = .{ .schema_refs = .empty, .dynamic = false },
            .auth = .{ .bearer = false, .jwt = false },
            .routes = .empty,
            .schemas_dynamic = false,
            .routes_dynamic = false,
        },
        .verification = null,
        .aot = null,
        .properties = .{
            .pure = false,
            .read_only = true,
            .stateless = false,
            .retry_safe = true,
            .deterministic = true,
            .has_egress = false,
        },
    };
    defer hc.deinit(allocator);

    // Add an env var
    try hc.env.literal.append(allocator, try allocator.dupe(u8, "API_KEY"));

    var rc = try fromHandlerContract(allocator, &hc);
    defer rc.deinit();

    try std.testing.expectEqual(@as(usize, 1), rc.env_vars.len);
    try std.testing.expectEqualStrings("API_KEY", rc.env_vars[0]);
    try std.testing.expect(!rc.env_dynamic);
    try std.testing.expect(rc.properties.read_only);
    try std.testing.expect(rc.properties.retry_safe);
    try std.testing.expect(rc.properties.deterministic);
    try std.testing.expect(!rc.properties.pure);
}
