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
const ModuleCapability = zq.module_binding.ModuleCapability;
const capability_count = zq.module_binding.capability_count;

pub const CapabilityMatrix = zq.handler_contract.CapabilityMatrix;

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

/// How aggressively the runtime pool may reuse a warmed handler runtime.
pub const PoolingPolicy = enum {
    ephemeral, // one runtime per request
    reuse_bounded_by_count, // recycle after N requests
    reuse_bounded_by_ttl, // recycle after wall-clock TTL
    reuse_unbounded, // pure + deterministic + state-isolated only
};

pub const PoolingThresholds = struct {
    max_requests: u32 = 64,
    ttl_ns: u64 = 30 * std.time.ns_per_s,
};

/// Map proven contract properties to a lifecycle policy. Null falls back to
/// `.reuse_bounded_by_count`.
pub fn derivePoolingPolicy(contract: ?*const ValidatedRuntimeContract) PoolingPolicy {
    const rc = contract orelse return .reuse_bounded_by_count;
    const p = rc.properties();
    if (p.pure and p.deterministic and p.state_isolated) return .reuse_unbounded;
    if (p.read_only and p.state_isolated) return .reuse_bounded_by_ttl;
    return .reuse_bounded_by_count;
}

/// Runtime view of the proven contract.
/// Owns all allocated memory; call deinit() when done.
/// Mirrors `handler_contract.WebSocketInfo` for the parsed runtime
/// contract. The server consults `on_message` at accept time to decide
/// whether to look for an RFC 6455 Upgrade header on incoming requests.
pub const WebSocketInfo = struct {
    on_open: bool = false,
    on_message: bool = false,
    on_close: bool = false,
    on_error: bool = false,

    pub fn any(self: WebSocketInfo) bool {
        return self.on_open or self.on_message or self.on_close or self.on_error;
    }
};

pub const RuntimeContract = struct {
    env_vars: []const []const u8,
    env_dynamic: bool,
    routes: []const Route,
    routes_dynamic: bool,
    properties: Properties,
    websocket: WebSocketInfo = .{},
    /// Null when the embedded contract did not emit a sandbox block (old
    /// contract, or contract parse fell through). A non-null matrix with
    /// len == 0 is a legitimate state for handlers that import only
    /// capability-free modules (e.g. zigttp:router).
    capabilities: ?CapabilityMatrix = null,
    /// SHA-256 of the bytecode blob, stamped at build time. All-zero means
    /// the contract did not carry a sandbox block.
    artifact_sha256: [32]u8 = [_]u8{0} ** 32,
    /// SHA-256 of the zigts rule registry used to extract this contract.
    /// All-zero means the contract did not carry a sandbox block.
    policy_hash: [32]u8 = [_]u8{0} ** 32,
    modules: []const []const u8 = &.{},
    allocator: std.mem.Allocator,

    pub fn hasCapability(self: *const RuntimeContract, cap: ModuleCapability) bool {
        const caps = self.capabilities orelse return false;
        return caps.has(cap);
    }

    pub fn deinit(self: *RuntimeContract) void {
        for (self.env_vars) |v| self.allocator.free(v);
        self.allocator.free(self.env_vars);
        for (self.routes) |r| {
            self.allocator.free(r.method);
            self.allocator.free(r.path);
        }
        self.allocator.free(self.routes);
        for (self.modules) |m| self.allocator.free(m);
        self.allocator.free(self.modules);
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

// ---------------------------------------------------------------------------
// Raw / Validated newtype split (WS3)
//
// `parseContractJson` and `fromHandlerContract` return a `RawRuntimeContract` —
// a wrapper that only exposes deinit + summary inspection. The runtime hot path
// accepts `ValidatedRuntimeContract`, which is constructible only via
// `validate`. That gates the three integrity checks (capability matrix, policy
// hash, embedded artifact hash) at the type-signature level: a caller cannot
// accidentally hand an unvalidated contract to the request loop.
//
// Env-var presence stays a separate `validateEnvVars` call on Validated so the
// server can keep emitting one error line per missing var; environment state is
// a deployment concern, not a contract integrity invariant.
// ---------------------------------------------------------------------------

pub const RawRuntimeContract = struct {
    inner: RuntimeContract,

    pub fn deinit(self: *RawRuntimeContract) void {
        self.inner.deinit();
    }

    /// Read-only view for tools that only need summary fields (e.g. `attest`)
    /// without forcing validation.
    pub fn rawView(self: *const RawRuntimeContract) *const RuntimeContract {
        return &self.inner;
    }
};

pub const ValidatedRuntimeContract = struct {
    inner: RuntimeContract,

    pub fn deinit(self: *ValidatedRuntimeContract) void {
        self.inner.deinit();
    }

    pub fn view(self: *const ValidatedRuntimeContract) *const RuntimeContract {
        return &self.inner;
    }

    pub fn properties(self: *const ValidatedRuntimeContract) Properties {
        return self.inner.properties;
    }

    pub fn websocket(self: *const ValidatedRuntimeContract) WebSocketInfo {
        return self.inner.websocket;
    }

    pub fn hasCapability(self: *const ValidatedRuntimeContract, cap: ModuleCapability) bool {
        return self.inner.hasCapability(cap);
    }

    pub fn matchesRoute(
        self: *const ValidatedRuntimeContract,
        method: []const u8,
        path: []const u8,
    ) bool {
        return self.inner.matchesRoute(method, path);
    }
};

pub const ValidateOptions = struct {
    /// Embedded bytecode blob whose SHA-256 must match the contract's
    /// `artifact_sha256`. Skipping the check for a non-zero hash is unsafe;
    /// pass null only when the contract carries no artifact hash (live reload
    /// from in-memory HandlerContract) or when the caller is genuinely running
    /// without embedded bytecode.
    bytecode: ?[]const u8 = null,
};

/// Promote a Raw contract to Validated by enforcing integrity invariants:
/// capability-matrix drift, policy-hash drift, and artifact-hash drift. On
/// success, the inner storage transfers into the returned Validated; on
/// failure, it is freed.
pub fn validate(
    raw: RawRuntimeContract,
    opts: ValidateOptions,
) !ValidatedRuntimeContract {
    var inner = raw.inner;
    errdefer inner.deinit();
    try verifyCapabilityMatrix(&inner);
    try verifyPolicyHash(&inner);
    try verifyArtifactHash(&inner, opts.bytecode);
    return .{ .inner = inner };
}

/// Parse a contract JSON blob into a RuntimeContract.
/// Extracts only the fields needed at runtime; ignores everything else.
pub fn parseContractJson(allocator: std.mem.Allocator, source: []const u8) !RawRuntimeContract {
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

    var capabilities: ?CapabilityMatrix = null;
    var artifact_sha256 = [_]u8{0} ** 32;
    var policy_hash = [_]u8{0} ** 32;
    if (root.get("sandbox")) |sandbox_val| {
        if (sandbox_val == .object) {
            const obj = sandbox_val.object;
            capabilities = parseCapabilityMatrix(obj);
            readSandboxHex(obj, "artifactSha256", &artifact_sha256);
            readSandboxHex(obj, "policyHash", &policy_hash);
        }
    }

    // Parse websocket.{onOpen,onMessage,onClose,onError}
    var websocket: WebSocketInfo = .{};
    if (root.get("websocket")) |ws_val| {
        if (ws_val == .object) {
            websocket.on_open = getBool(ws_val.object, "onOpen");
            websocket.on_message = getBool(ws_val.object, "onMessage");
            websocket.on_close = getBool(ws_val.object, "onClose");
            websocket.on_error = getBool(ws_val.object, "onError");
        }
    }

    // Parse modules[] so the startup drift check has the handler's import set
    var modules: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (modules.items) |m| allocator.free(m);
        modules.deinit(allocator);
    }
    if (root.get("modules")) |mods_val| {
        if (mods_val == .array) {
            for (mods_val.array.items) |item| {
                if (item != .string) continue;
                try modules.append(allocator, try allocator.dupe(u8, item.string));
            }
        }
    }

    return .{ .inner = .{
        .env_vars = try env_vars.toOwnedSlice(allocator),
        .env_dynamic = env_dynamic,
        .routes = try routes.toOwnedSlice(allocator),
        .routes_dynamic = routes_dynamic,
        .properties = properties,
        .websocket = websocket,
        .capabilities = capabilities,
        .artifact_sha256 = artifact_sha256,
        .policy_hash = policy_hash,
        .modules = try modules.toOwnedSlice(allocator),
        .allocator = allocator,
    } };
}

fn readSandboxHex(obj: std.json.ObjectMap, key: []const u8, out: *[32]u8) void {
    const val = obj.get(key) orelse return;
    if (val != .string or val.string.len != 64) return;
    _ = std.fmt.hexToBytes(out, val.string) catch return;
}

fn parseCapabilityMatrix(obj: std.json.ObjectMap) CapabilityMatrix {
    var matrix: CapabilityMatrix = .{};
    if (obj.get("capabilities")) |caps_val| {
        if (caps_val == .array) {
            var seen = [_]bool{false} ** capability_count;
            for (caps_val.array.items) |item| {
                if (item != .string) continue;
                const cap = std.meta.stringToEnum(ModuleCapability, item.string) orelse continue;
                seen[@intFromEnum(cap)] = true;
            }
            for (std.enums.values(ModuleCapability)) |c| {
                if (seen[@intFromEnum(c)]) {
                    matrix.items[matrix.len] = c;
                    matrix.len += 1;
                }
            }
        }
    }

    var stored_hash = [_]u8{0} ** 32;
    readSandboxHex(obj, "capabilityHash", &stored_hash);
    matrix.hash = if (std.mem.allEqual(u8, &stored_hash, 0))
        zq.module_binding.capabilityHash(matrix.slice())
    else
        stored_hash;
    return matrix;
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
pub fn validateEnvVars(allocator: std.mem.Allocator, contract: *const ValidatedRuntimeContract) ![]const []const u8 {
    var missing: std.ArrayList([]const u8) = .empty;
    errdefer missing.deinit(allocator);

    for (contract.view().env_vars) |name| {
        const name_z = try allocator.dupeZ(u8, name);
        defer allocator.free(name_z);
        if (std.c.getenv(name_z) == null) {
            try missing.append(allocator, name);
        }
    }

    return try missing.toOwnedSlice(allocator);
}

/// Build a RawRuntimeContract directly from an engine HandlerContract.
/// Allocates all strings with the given allocator. Caller owns the result.
pub fn fromHandlerContract(allocator: std.mem.Allocator, hc: *const HandlerContract) !RawRuntimeContract {
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

    var modules: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (modules.items) |m| allocator.free(m);
        modules.deinit(allocator);
    }
    for (hc.modules.items) |m| {
        try modules.append(allocator, try allocator.dupe(u8, m));
    }

    return .{ .inner = .{
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
        .capabilities = hc.capabilities,
        .artifact_sha256 = hc.artifact_sha256,
        .policy_hash = hc.policy_hash,
        .modules = try modules.toOwnedSlice(allocator),
        .allocator = allocator,
    } };
}

/// Rebuild the capability matrix from the currently-linked registry for
/// this handler's imports. Compare against `contract.capabilities.hash` to
/// detect drift between a compiled contract and the runtime binary.
pub fn deriveLiveCapabilityMatrix(contract: *const RuntimeContract) CapabilityMatrix {
    return zq.handler_contract.computeCapabilityMatrix(contract.modules);
}

/// Verify the embedded matrix still matches what the linked registry would
/// produce. Returns error.CapabilityMatrixMismatch on drift. Skips silently
/// when the contract did not carry a sandbox block.
pub fn verifyCapabilityMatrix(contract: *const RuntimeContract) !void {
    const stored = contract.capabilities orelse return;
    const live = deriveLiveCapabilityMatrix(contract);
    if (!std.mem.eql(u8, &live.hash, &stored.hash)) {
        return error.CapabilityMatrixMismatch;
    }
}

/// Compare the embedded policy hash against the linked rule registry.
/// Returns error.PolicyHashMismatch on drift. All-zero means "not emitted",
/// which skips the check.
pub fn verifyPolicyHash(contract: *const RuntimeContract) !void {
    if (std.mem.allEqual(u8, &contract.policy_hash, 0)) return;
    const hex = zq.rule_registry.policyHash();
    var live: [32]u8 = undefined;
    _ = std.fmt.hexToBytes(&live, &hex) catch return error.PolicyHashMismatch;
    if (!std.mem.eql(u8, &live, &contract.policy_hash)) {
        return error.PolicyHashMismatch;
    }
}

/// Re-hash the embedded bytecode and compare against the contract. Skips
/// when the hash is absent (all-zero) or no bytecode was provided.
pub fn verifyArtifactHash(contract: *const RuntimeContract, bytecode: ?[]const u8) !void {
    if (std.mem.allEqual(u8, &contract.artifact_sha256, 0)) return;
    const blob = bytecode orelse return;
    var live: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(blob, &live, .{});
    if (!std.mem.eql(u8, &live, &contract.artifact_sha256)) {
        return error.ArtifactHashMismatch;
    }
}

// ============================================================================
// Tests
// ============================================================================

test "parseContractJson extracts websocket event presence flags" {
    const allocator = std.testing.allocator;
    const source =
        \\{
        \\  "version": 14,
        \\  "handler": {"path": "handler.ts", "line": 1, "column": 0},
        \\  "routes": [],
        \\  "modules": [],
        \\  "functions": {},
        \\  "env": {"literal": [], "dynamic": false},
        \\  "egress": {"hosts": [], "dynamic": false},
        \\  "serviceCalls": [],
        \\  "cache": {"namespaces": [], "dynamic": false},
        \\  "sql": {"backend": "sqlite", "queries": [], "dynamic": false},
        \\  "durable": {"used": false, "keys": {"literal": [], "dynamic": false}, "steps": [], "timers": false, "signals": {"literal": [], "dynamic": false}, "producerKeys": {"literal": [], "dynamic": false}},
        \\  "api": {"schemas": [], "requests": {"schemaRefs": [], "dynamic": false}, "auth": {"bearer": false, "jwt": false}, "routes": [], "schemasDynamic": false, "routesDynamic": false},
        \\  "verification": null,
        \\  "websocket": {"onOpen": true, "onMessage": true, "onClose": true, "onError": false},
        \\  "aot": null,
        \\  "faultCoverage": null,
        \\  "rateLimiting": null,
        \\  "properties": null,
        \\  "behaviors": [],
        \\  "behaviorsExhaustive": false
        \\}
    ;
    var raw = try parseContractJson(allocator, source);
    defer raw.deinit();
    const contract = &raw.inner;

    try std.testing.expect(contract.websocket.on_open);
    try std.testing.expect(contract.websocket.on_message);
    try std.testing.expect(contract.websocket.on_close);
    try std.testing.expect(!contract.websocket.on_error);
    try std.testing.expect(contract.websocket.any());
}

test "parseContractJson defaults websocket to all-false when section absent" {
    const allocator = std.testing.allocator;
    const source =
        \\{
        \\  "version": 14,
        \\  "handler": {"path": "handler.ts", "line": 1, "column": 0},
        \\  "routes": [],
        \\  "modules": [],
        \\  "functions": {},
        \\  "env": {"literal": [], "dynamic": false},
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
    var raw = try parseContractJson(allocator, source);
    defer raw.deinit();
    const contract = &raw.inner;

    try std.testing.expect(!contract.websocket.on_open);
    try std.testing.expect(!contract.websocket.on_message);
    try std.testing.expect(!contract.websocket.on_close);
    try std.testing.expect(!contract.websocket.on_error);
    try std.testing.expect(!contract.websocket.any());
}

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

    var raw = try parseContractJson(allocator, source);
    defer raw.deinit();
    const contract = &raw.inner;

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

    var raw = try parseContractJson(allocator, source);
    defer raw.deinit();
    const contract = &raw.inner;

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
        .scope = .{
            .used = false,
            .names = .empty,
            .dynamic = false,
            .max_depth = 0,
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

    var raw = try fromHandlerContract(allocator, &hc);
    defer raw.deinit();
    const rc = &raw.inner;

    try std.testing.expectEqual(@as(usize, 1), rc.env_vars.len);
    try std.testing.expectEqualStrings("API_KEY", rc.env_vars[0]);
    try std.testing.expect(!rc.env_dynamic);
    try std.testing.expect(rc.properties.read_only);
    try std.testing.expect(rc.properties.retry_safe);
    try std.testing.expect(rc.properties.deterministic);
    try std.testing.expect(!rc.properties.pure);
}

test "parseContractJson reads sandbox block" {
    const allocator = std.testing.allocator;
    const source =
        \\{
        \\  "version": 12,
        \\  "handler": {"path": "handler.ts", "line": 1, "column": 0},
        \\  "modules": ["zigttp:crypto", "zigttp:auth"],
        \\  "sandbox": {
        \\    "capabilities": ["clock", "crypto"],
        \\    "capabilityHash": "0000000000000000000000000000000000000000000000000000000000000000"
        \\  },
        \\  "env": {"literal": [], "dynamic": false},
        \\  "egress": {"hosts": [], "dynamic": false},
        \\  "api": {"routes": [], "routesDynamic": false}
        \\}
    ;
    var raw = try parseContractJson(allocator, source);
    defer raw.deinit();
    const contract = &raw.inner;

    try std.testing.expect(contract.capabilities != null);
    try std.testing.expectEqual(@as(u8, 2), contract.capabilities.?.len);
    try std.testing.expect(contract.hasCapability(.clock));
    try std.testing.expect(contract.hasCapability(.crypto));
    try std.testing.expect(!contract.hasCapability(.stderr));
    try std.testing.expectEqual(@as(usize, 2), contract.modules.len);
}

test "validate promotes Raw to Validated when integrity checks pass" {
    const allocator = std.testing.allocator;
    const source =
        \\{
        \\  "version": 13,
        \\  "handler": {"path": "handler.ts", "line": 1, "column": 0},
        \\  "modules": [],
        \\  "env": {"literal": [], "dynamic": false},
        \\  "egress": {"hosts": [], "dynamic": false},
        \\  "api": {"routes": [], "routesDynamic": false}
        \\}
    ;
    const raw = try parseContractJson(allocator, source);
    var validated = try validate(raw, .{});
    defer validated.deinit();

    try std.testing.expect(validated.view().capabilities == null);
}

test "validate rejects artifact-hash drift" {
    const allocator = std.testing.allocator;
    const source =
        \\{
        \\  "version": 13,
        \\  "handler": {"path": "handler.ts", "line": 1, "column": 0},
        \\  "modules": [],
        \\  "env": {"literal": [], "dynamic": false},
        \\  "egress": {"hosts": [], "dynamic": false},
        \\  "api": {"routes": [], "routesDynamic": false}
        \\}
    ;
    var raw = try parseContractJson(allocator, source);
    raw.inner.artifact_sha256 = [_]u8{0xAB} ** 32;
    try std.testing.expectError(
        error.ArtifactHashMismatch,
        validate(raw, .{ .bytecode = "totally different" }),
    );
}

test "parseContractJson returns null capabilities when sandbox block is absent" {
    const allocator = std.testing.allocator;
    const source =
        \\{
        \\  "version": 13,
        \\  "handler": {"path": "handler.ts", "line": 1, "column": 0},
        \\  "modules": [],
        \\  "env": {"literal": [], "dynamic": false},
        \\  "egress": {"hosts": [], "dynamic": false},
        \\  "api": {"routes": [], "routesDynamic": false}
        \\}
    ;
    var raw = try parseContractJson(allocator, source);
    defer raw.deinit();
    const contract = &raw.inner;
    try std.testing.expect(contract.capabilities == null);
    try std.testing.expect(!contract.hasCapability(.crypto));
}

test "verifyCapabilityMatrix passes for a live-derived matrix" {
    const allocator = std.testing.allocator;
    // Build modules and a matching matrix from the live registry
    var modules_list: std.ArrayList([]const u8) = .empty;
    errdefer modules_list.deinit(allocator);
    try modules_list.append(allocator, try allocator.dupe(u8, "zigttp:crypto"));
    try modules_list.append(allocator, try allocator.dupe(u8, "zigttp:auth"));

    var contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &.{},
        .routes_dynamic = false,
        .properties = .{},
        .modules = try modules_list.toOwnedSlice(allocator),
        .allocator = allocator,
    };
    defer contract.deinit();

    contract.capabilities = deriveLiveCapabilityMatrix(&contract);
    try std.testing.expectEqual(@as(u8, 2), contract.capabilities.?.len);

    try verifyCapabilityMatrix(&contract);
}

test "verifyCapabilityMatrix detects drift" {
    const allocator = std.testing.allocator;
    var modules_list: std.ArrayList([]const u8) = .empty;
    errdefer modules_list.deinit(allocator);
    try modules_list.append(allocator, try allocator.dupe(u8, "zigttp:crypto"));

    var contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &.{},
        .routes_dynamic = false,
        .properties = .{},
        .modules = try modules_list.toOwnedSlice(allocator),
        .allocator = allocator,
    };
    defer contract.deinit();

    // Seed a non-null matrix whose hash does not match the live derivation.
    contract.capabilities = CapabilityMatrix{
        .len = 1,
        .items = blk: {
            var items: [capability_count]ModuleCapability = undefined;
            items[0] = .crypto;
            break :blk items;
        },
        .hash = [_]u8{0xAA} ** 32,
    };

    try std.testing.expectError(
        error.CapabilityMatrixMismatch,
        verifyCapabilityMatrix(&contract),
    );
}

test "verifyCapabilityMatrix skips when matrix is absent" {
    var contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &.{},
        .routes_dynamic = false,
        .properties = .{},
        .allocator = std.testing.allocator,
    };
    try verifyCapabilityMatrix(&contract);
}

test "derivePoolingPolicy: null contract falls back to bounded count" {
    try std.testing.expectEqual(PoolingPolicy.reuse_bounded_by_count, derivePoolingPolicy(null));
}

test "derivePoolingPolicy: pure+deterministic+isolated = unbounded" {
    const contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &.{},
        .routes_dynamic = false,
        .properties = .{
            .pure = true,
            .deterministic = true,
            .state_isolated = true,
        },
        .allocator = std.testing.allocator,
    };
    var validated = ValidatedRuntimeContract{ .inner = contract };
    try std.testing.expectEqual(PoolingPolicy.reuse_unbounded, derivePoolingPolicy(&validated));
}

test "derivePoolingPolicy: read_only+isolated = ttl" {
    const contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &.{},
        .routes_dynamic = false,
        .properties = .{
            .read_only = true,
            .state_isolated = true,
        },
        .allocator = std.testing.allocator,
    };
    var validated = ValidatedRuntimeContract{ .inner = contract };
    try std.testing.expectEqual(PoolingPolicy.reuse_bounded_by_ttl, derivePoolingPolicy(&validated));
}

test "derivePoolingPolicy: has_egress = bounded count" {
    const contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &.{},
        .routes_dynamic = false,
        .properties = .{
            .has_egress = true,
            .state_isolated = true,
        },
        .allocator = std.testing.allocator,
    };
    var validated = ValidatedRuntimeContract{ .inner = contract };
    try std.testing.expectEqual(PoolingPolicy.reuse_bounded_by_count, derivePoolingPolicy(&validated));
}

test "verifyPolicyHash passes when hash matches live registry" {
    var contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &.{},
        .routes_dynamic = false,
        .properties = .{},
        .allocator = std.testing.allocator,
    };
    const hex = zq.rule_registry.policyHash();
    _ = try std.fmt.hexToBytes(&contract.policy_hash, &hex);
    try verifyPolicyHash(&contract);
}

test "verifyPolicyHash rejects drift" {
    var contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &.{},
        .routes_dynamic = false,
        .properties = .{},
        .policy_hash = [_]u8{0xAB} ** 32,
        .allocator = std.testing.allocator,
    };
    try std.testing.expectError(error.PolicyHashMismatch, verifyPolicyHash(&contract));
}

test "verifyPolicyHash skips when hash is absent" {
    var contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &.{},
        .routes_dynamic = false,
        .properties = .{},
        .allocator = std.testing.allocator,
    };
    try verifyPolicyHash(&contract);
}

test "verifyArtifactHash matches bytecode digest" {
    const bytecode = "some fake bytecode blob";
    var expected: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(bytecode, &expected, .{});
    var contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &.{},
        .routes_dynamic = false,
        .properties = .{},
        .artifact_sha256 = expected,
        .allocator = std.testing.allocator,
    };
    try verifyArtifactHash(&contract, bytecode);
}

test "verifyArtifactHash rejects tampered bytecode" {
    var contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &.{},
        .routes_dynamic = false,
        .properties = .{},
        .artifact_sha256 = [_]u8{0xFF} ** 32,
        .allocator = std.testing.allocator,
    };
    try std.testing.expectError(error.ArtifactHashMismatch, verifyArtifactHash(&contract, "anything"));
}

test "verifyArtifactHash skips when hash is absent" {
    var contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &.{},
        .routes_dynamic = false,
        .properties = .{},
        .allocator = std.testing.allocator,
    };
    try verifyArtifactHash(&contract, "anything");
}

test "derivePoolingPolicy: !state_isolated = bounded count" {
    const contract = RuntimeContract{
        .env_vars = &.{},
        .env_dynamic = false,
        .routes = &.{},
        .routes_dynamic = false,
        .properties = .{
            .pure = true,
            .deterministic = true,
            .state_isolated = false,
        },
        .allocator = std.testing.allocator,
    };
    var validated = ValidatedRuntimeContract{ .inner = contract };
    try std.testing.expectEqual(PoolingPolicy.reuse_bounded_by_count, derivePoolingPolicy(&validated));
}
