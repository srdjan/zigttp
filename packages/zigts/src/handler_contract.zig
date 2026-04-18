//! Handler Contract Manifest
//!
//! Extracts a machine-readable contract from a handler's IR describing what
//! the handler is allowed to do: which routes it serves, which virtual modules
//! it uses, which env vars / outbound hosts / cache namespaces it references.
//!
//! Extraction is a compile-time pass. At runtime, the contract is parsed by
//! contract_runtime.zig for startup env validation, route pre-filtering, and
//! property-driven behavior (see server.zig).
//!
//! Usage: called by precompile.zig when --contract is passed.
//!
//! All string data in HandlerContract is owned (duped). The contract outlives
//! the parser and atom table that produced it.

const std = @import("std");
const api_schema = @import("api_schema.zig");
const json_utils = @import("json_utils.zig");
const ir = @import("parser/ir.zig");
const object = @import("object.zig");
const context = @import("context.zig");
const module_binding = @import("module_binding.zig");
const builtin_modules = @import("builtin_modules.zig");
const bytecode = @import("bytecode.zig");
const handler_analyzer = @import("handler_analyzer.zig");
const type_checker_mod = @import("type_checker.zig");
const type_env_mod = @import("type_env.zig");
const type_pool_mod = @import("type_pool.zig");
const rule_registry = @import("rule_registry.zig");

fn currentPolicyHashRaw() [32]u8 {
    const hex = rule_registry.policyHash();
    var out: [32]u8 = undefined;
    _ = std.fmt.hexToBytes(&out, &hex) catch unreachable;
    return out;
}

const Node = ir.Node;
const NodeIndex = ir.NodeIndex;
const NodeTag = ir.NodeTag;
const IrView = ir.IrView;
const null_node = ir.null_node;
const HandlerPattern = bytecode.HandlerPattern;
const PatternDispatchTable = bytecode.PatternDispatchTable;
const TypeChecker = type_checker_mod.TypeChecker;
const TypeEnv = type_env_mod.TypeEnv;

// -------------------------------------------------------------------------
// Contract data types
// -------------------------------------------------------------------------

pub const HandlerLoc = struct {
    path: []const u8, // owned
    line: u32,
    column: u32,
};

pub const RouteInfo = struct {
    pattern: []const u8, // owned
    route_type: []const u8, // static literal, not owned
    field: []const u8, // static literal, not owned
    status: u16,
    content_type: []const u8, // static literal, not owned
    aot: bool,
};

pub const EnvInfo = struct {
    literal: std.ArrayList([]const u8), // each entry owned
    dynamic: bool,
};

pub const EgressInfo = struct {
    hosts: std.ArrayList([]const u8), // each entry owned
    urls: std.ArrayList([]const u8) = .empty, // full fetchSync URLs, each entry owned
    dynamic: bool,
};

pub const CacheInfo = struct {
    namespaces: std.ArrayList([]const u8), // each entry owned
    dynamic: bool,
};

pub const SqlQueryInfo = struct {
    name: []const u8, // owned
    statement: []const u8, // owned
    operation: []const u8 = "", // static literal after validation
    tables: std.ArrayList([]const u8) = .empty,

    pub fn deinit(self: *SqlQueryInfo, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        allocator.free(self.statement);
        for (self.tables.items) |table| allocator.free(table);
        self.tables.deinit(allocator);
    }
};

pub const SqlInfo = struct {
    backend: []const u8 = "sqlite",
    queries: std.ArrayList(SqlQueryInfo),
    dynamic: bool,

    pub fn deinit(self: *SqlInfo, allocator: std.mem.Allocator) void {
        for (self.queries.items) |*query| query.deinit(allocator);
        self.queries.deinit(allocator);
    }
};

pub const DurableKeyInfo = struct {
    literal: std.ArrayList([]const u8), // each entry owned
    dynamic: bool,
};

pub const DurableWorkflowProofLevel = enum {
    none,
    partial,
    complete,

    pub fn toString(self: DurableWorkflowProofLevel) []const u8 {
        return switch (self) {
            .none => "none",
            .partial => "partial",
            .complete => "complete",
        };
    }

    pub fn fromString(raw: []const u8) DurableWorkflowProofLevel {
        if (std.mem.eql(u8, raw, "complete")) return .complete;
        if (std.mem.eql(u8, raw, "partial")) return .partial;
        return .none;
    }
};

pub const DurableWorkflowNodeKind = enum {
    branch,
    step,
    step_with_timeout,
    sleep,
    sleep_until,
    wait_signal,
    signal,
    signal_at,
    return_response,

    pub fn toString(self: DurableWorkflowNodeKind) []const u8 {
        return switch (self) {
            .branch => "branch",
            .step => "step",
            .step_with_timeout => "stepWithTimeout",
            .sleep => "sleep",
            .sleep_until => "sleepUntil",
            .wait_signal => "waitSignal",
            .signal => "signal",
            .signal_at => "signalAt",
            .return_response => "return",
        };
    }

    pub fn fromString(raw: []const u8) DurableWorkflowNodeKind {
        if (std.mem.eql(u8, raw, "branch")) return .branch;
        if (std.mem.eql(u8, raw, "step")) return .step;
        if (std.mem.eql(u8, raw, "stepWithTimeout")) return .step_with_timeout;
        if (std.mem.eql(u8, raw, "sleep")) return .sleep;
        if (std.mem.eql(u8, raw, "sleepUntil")) return .sleep_until;
        if (std.mem.eql(u8, raw, "waitSignal")) return .wait_signal;
        if (std.mem.eql(u8, raw, "signal")) return .signal;
        if (std.mem.eql(u8, raw, "signalAt")) return .signal_at;
        return .return_response;
    }
};

pub const DurableWorkflowNode = struct {
    id: []const u8,
    kind: DurableWorkflowNodeKind,
    label: []const u8,
    detail: ?[]const u8 = null,
    status: ?u16 = null,

    pub fn deinit(self: *DurableWorkflowNode, allocator: std.mem.Allocator) void {
        allocator.free(self.id);
        allocator.free(self.label);
        if (self.detail) |detail| allocator.free(detail);
    }
};

pub const DurableWorkflowEdge = struct {
    from: []const u8,
    to: []const u8,
    condition: ?[]const u8 = null,

    pub fn deinit(self: *DurableWorkflowEdge, allocator: std.mem.Allocator) void {
        allocator.free(self.from);
        allocator.free(self.to);
        if (self.condition) |condition| allocator.free(condition);
    }
};

pub const DurableWorkflow = struct {
    workflow_id: ?[]const u8 = null,
    proof_level: DurableWorkflowProofLevel = .none,
    nodes: std.ArrayList(DurableWorkflowNode) = .empty,
    edges: std.ArrayList(DurableWorkflowEdge) = .empty,

    pub fn deinit(self: *DurableWorkflow, allocator: std.mem.Allocator) void {
        if (self.workflow_id) |workflow_id| allocator.free(workflow_id);
        for (self.nodes.items) |*node| node.deinit(allocator);
        self.nodes.deinit(allocator);
        for (self.edges.items) |*edge| edge.deinit(allocator);
        self.edges.deinit(allocator);
    }
};

pub const DurableInfo = struct {
    used: bool,
    keys: DurableKeyInfo,
    steps: std.ArrayList([]const u8), // each entry owned
    timers: bool = false,
    signals: DurableKeyInfo = .{ .literal = .empty, .dynamic = false },
    producer_keys: DurableKeyInfo = .{ .literal = .empty, .dynamic = false },
    workflow: DurableWorkflow = .{},

    pub fn deinit(self: *DurableInfo, allocator: std.mem.Allocator) void {
        for (self.keys.literal.items) |key| {
            allocator.free(key);
        }
        self.keys.literal.deinit(allocator);
        for (self.steps.items) |step| {
            allocator.free(step);
        }
        self.steps.deinit(allocator);
        for (self.signals.literal.items) |signal| {
            allocator.free(signal);
        }
        self.signals.literal.deinit(allocator);
        for (self.producer_keys.literal.items) |key| {
            allocator.free(key);
        }
        self.producer_keys.literal.deinit(allocator);
        self.workflow.deinit(allocator);
    }
};

pub const ScopeInfo = struct {
    used: bool,
    names: std.ArrayList([]const u8),
    dynamic: bool = false,
    max_depth: u32 = 0,

    pub fn deinit(self: *ScopeInfo, allocator: std.mem.Allocator) void {
        for (self.names.items) |name| allocator.free(name);
        self.names.deinit(allocator);
    }
};

/// WebSocket event-export presence. When a handler module exports any of
/// `onOpen`, `onMessage`, `onClose`, or `onError` as top-level named
/// functions, the contract records which ones are present. The runtime
/// uses this to decide whether to enable the WebSocket gateway at
/// startup; deploy manifests use it to emit per-platform WS routing.
/// All fields are zero-initialised, so a handler with no WS exports
/// produces `{on_open:false, on_message:false, on_close:false, on_error:false}`
/// — the section is always present for shape stability.
pub const WebSocketInfo = struct {
    on_open: bool = false,
    on_message: bool = false,
    on_close: bool = false,
    on_error: bool = false,

    pub fn any(self: WebSocketInfo) bool {
        return self.on_open or self.on_message or self.on_close or self.on_error;
    }
};

pub const ApiSchemaInfo = struct {
    name: []const u8, // owned
    schema_json: []const u8, // owned JSON source
};

pub const ApiRequestInfo = struct {
    schema_refs: std.ArrayList([]const u8), // each entry owned
    dynamic: bool,
};

pub const ApiAuthInfo = struct {
    bearer: bool,
    jwt: bool,
};

pub const ApiParamInfo = struct {
    name: []const u8, // owned
    location: []const u8 = "path", // static literal
    required: bool = false,
    schema_json: []const u8, // owned JSON source

    pub fn deinit(self: *ApiParamInfo, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        allocator.free(self.schema_json);
    }

    pub fn dupeOwned(self: ApiParamInfo, allocator: std.mem.Allocator) !ApiParamInfo {
        return .{
            .name = try allocator.dupe(u8, self.name),
            .location = self.location,
            .required = self.required,
            .schema_json = try allocator.dupe(u8, self.schema_json),
        };
    }
};

pub const ApiBodyInfo = struct {
    content_type: ?[]const u8 = null, // owned when present
    schema_ref: ?[]const u8 = null, // owned when present
    schema_json: ?[]const u8 = null, // owned when present
    dynamic: bool = false,

    pub fn deinit(self: *ApiBodyInfo, allocator: std.mem.Allocator) void {
        if (self.content_type) |content_type| allocator.free(content_type);
        if (self.schema_ref) |schema_ref| allocator.free(schema_ref);
        if (self.schema_json) |schema_json| allocator.free(schema_json);
    }

    pub fn dupeOwned(self: ApiBodyInfo, allocator: std.mem.Allocator) !ApiBodyInfo {
        return .{
            .content_type = try dupeOptionalString(allocator, self.content_type),
            .schema_ref = try dupeOptionalString(allocator, self.schema_ref),
            .schema_json = try dupeOptionalString(allocator, self.schema_json),
            .dynamic = self.dynamic,
        };
    }
};

pub const ApiResponseInfo = struct {
    status: ?u16 = null,
    content_type: ?[]const u8 = null, // owned when present
    schema_ref: ?[]const u8 = null, // owned when present
    schema_json: ?[]const u8 = null, // owned when present
    dynamic: bool = false,

    pub fn deinit(self: *ApiResponseInfo, allocator: std.mem.Allocator) void {
        if (self.content_type) |content_type| allocator.free(content_type);
        if (self.schema_ref) |schema_ref| allocator.free(schema_ref);
        if (self.schema_json) |schema_json| allocator.free(schema_json);
    }

    pub fn dupeOwned(self: ApiResponseInfo, allocator: std.mem.Allocator) !ApiResponseInfo {
        return .{
            .status = self.status,
            .content_type = try dupeOptionalString(allocator, self.content_type),
            .schema_ref = try dupeOptionalString(allocator, self.schema_ref),
            .schema_json = try dupeOptionalString(allocator, self.schema_json),
            .dynamic = self.dynamic,
        };
    }
};

pub const ApiRouteInfo = struct {
    method: []const u8, // owned
    path: []const u8, // owned
    request_schema_refs: std.ArrayList([]const u8), // each entry owned
    request_schema_dynamic: bool,
    requires_bearer: bool,
    requires_jwt: bool,
    path_params: std.ArrayList(ApiParamInfo) = .empty,
    query_params: std.ArrayList(ApiParamInfo) = .empty,
    header_params: std.ArrayList(ApiParamInfo) = .empty,
    query_params_dynamic: bool = false,
    header_params_dynamic: bool = false,
    request_bodies: std.ArrayList(ApiBodyInfo) = .empty,
    request_bodies_dynamic: bool = false,
    responses: std.ArrayList(ApiResponseInfo) = .empty,
    responses_dynamic: bool = false,
    // Legacy scalar response fields, kept for backward compatibility with
    // older contract.json consumers. backfillApiRouteCollections bridges
    // these into the responses collection during deserialization.
    response_status: ?u16 = null,
    response_content_type: ?[]const u8 = null, // owned when present
    response_schema_ref: ?[]const u8 = null, // owned when present
    response_schema_json: ?[]const u8 = null, // owned when present
    response_schema_dynamic: bool = false,

    pub fn deinit(self: *ApiRouteInfo, allocator: std.mem.Allocator) void {
        allocator.free(self.method);
        allocator.free(self.path);
        for (self.request_schema_refs.items) |schema_ref| {
            allocator.free(schema_ref);
        }
        self.request_schema_refs.deinit(allocator);
        for (self.path_params.items) |*param| {
            param.deinit(allocator);
        }
        self.path_params.deinit(allocator);
        for (self.query_params.items) |*param| {
            param.deinit(allocator);
        }
        self.query_params.deinit(allocator);
        for (self.header_params.items) |*param| {
            param.deinit(allocator);
        }
        self.header_params.deinit(allocator);
        for (self.request_bodies.items) |*body| {
            body.deinit(allocator);
        }
        self.request_bodies.deinit(allocator);
        for (self.responses.items) |*response| {
            response.deinit(allocator);
        }
        self.responses.deinit(allocator);
        if (self.response_content_type) |content_type| {
            allocator.free(content_type);
        }
        if (self.response_schema_ref) |schema_ref| {
            allocator.free(schema_ref);
        }
        if (self.response_schema_json) |schema_json| {
            allocator.free(schema_json);
        }
    }
};

pub const ApiInfo = struct {
    schemas: std.ArrayList(ApiSchemaInfo),
    requests: ApiRequestInfo,
    auth: ApiAuthInfo,
    routes: std.ArrayList(ApiRouteInfo),
    schemas_dynamic: bool,
    routes_dynamic: bool,

    pub fn deinit(self: *ApiInfo, allocator: std.mem.Allocator) void {
        for (self.schemas.items) |schema| {
            allocator.free(schema.name);
            allocator.free(schema.schema_json);
        }
        self.schemas.deinit(allocator);
        for (self.requests.schema_refs.items) |schema_ref| {
            allocator.free(schema_ref);
        }
        self.requests.schema_refs.deinit(allocator);
        for (self.routes.items) |*route| {
            route.deinit(allocator);
        }
        self.routes.deinit(allocator);
    }
};

pub fn emptyApiInfo() ApiInfo {
    return .{
        .schemas = .empty,
        .requests = .{ .schema_refs = .empty, .dynamic = false },
        .auth = .{ .bearer = false, .jwt = false },
        .routes = .empty,
        .schemas_dynamic = false,
        .routes_dynamic = false,
    };
}

pub fn emptySqlInfo() SqlInfo {
    return .{
        .backend = "sqlite",
        .queries = .empty,
        .dynamic = false,
    };
}

/// Create a minimal empty contract with the given handler path (owned by caller).
pub fn emptyContract(path: []const u8) HandlerContract {
    return .{
        .handler = .{ .path = path, .line = 1, .column = 0 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = .empty, .dynamic = false },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .sql = emptySqlInfo(),
        .durable = .{
            .used = false,
            .keys = .{ .literal = .empty, .dynamic = false },
            .steps = .empty,
        },
        .scope = .{
            .used = false,
            .names = .empty,
            .dynamic = false,
            .max_depth = 0,
        },
        .api = emptyApiInfo(),
        .verification = null,
        .aot = null,
    };
}

pub const VerificationInfo = struct {
    exhaustive_returns: bool,
    results_safe: bool,
    unreachable_code: bool,
    /// Bytecode passed structural verification (opcode validity, bounds, stack discipline)
    bytecode_verified: bool = false,
};

pub const AotInfo = struct {
    pattern_count: u32,
    has_default: bool,
};

pub const FaultCoverageInfo = struct {
    total_failable: u32,
    covered: u32,
    warnings: u32,

    pub fn isCovered(self: FaultCoverageInfo) bool {
        return self.covered == self.total_failable and self.total_failable > 0;
    }
};

/// Handler-level properties derived from effect classification of virtual
/// module function calls. Computed during contract extraction by checking
/// each imported function's EffectClass (read/write/none).
pub const HandlerProperties = struct {
    /// No virtual module calls. Pure function of request.
    pure: bool,
    /// Only read-classified functions. No state mutations.
    read_only: bool,
    /// Read-only AND no cacheGet. Independent of mutable state.
    stateless: bool,
    /// Read-only OR all writes within durable steps. Safe for Lambda retry.
    retry_safe: bool,
    /// No Date.now() or Math.random() usage.
    deterministic: bool,
    /// Uses fetchSync (conservative write).
    has_egress: bool,
    // --- Data flow provenance (from FlowChecker) ---
    /// No {secret} data reaches response bodies, headers, or external egress.
    no_secret_leakage: bool = true,
    /// No {credential} data reaches response bodies or logs.
    no_credential_leakage: bool = true,
    /// All {user_input} data passes through validation before external egress.
    input_validated: bool = true,
    /// {user_input} data does not flow to external egress hosts.
    pii_contained: bool = true,
    // --- Derived properties ---
    /// Deterministic AND (read_only OR all writes in durable steps).
    /// Safe for at-least-once delivery and automatic retry.
    idempotent: bool = false,
    /// Maximum number of I/O calls on any single execution path.
    /// Enables compile-time Lambda timeout derivation.
    max_io_depth: ?u32 = null,
    /// No unvalidated user input reaches sensitive sinks.
    /// Proves SQL injection and XSS prevention.
    injection_safe: bool = true,
    /// No module-scope variable mutations inside handler body.
    /// Proves cross-request data isolation.
    state_isolated: bool = true,
    // --- Fault coverage (from FaultCoverageChecker) ---
    /// Every failable I/O call site has an explicit failure path.
    fault_covered: bool = false,
    // --- Result and optional safety (from HandlerVerifier, when -Dverify is run) ---
    /// Proven: every result.ok access is guarded before use.
    /// False = unproven (verification not run). True = verified safe.
    result_safe: bool = false,
    /// Proven: every optional value from virtual modules is narrowed before use.
    /// False = unproven (verification not run). True = verified safe.
    optional_safe: bool = false,
};

pub const RateLimitInfo = struct {
    namespace: []const u8, // aliases into cache.namespaces (not separately owned)
    dynamic: bool,
};

// -------------------------------------------------------------------------
// Behavioral contract types
// -------------------------------------------------------------------------

/// A condition that distinguishes one execution path from another.
/// Conditions are derived from branch points in the handler's IR tree.
pub const PathCondition = struct {
    kind: Kind,
    module: ?[]const u8 = null, // for io conditions (owned)
    func: ?[]const u8 = null, // for io conditions (owned)
    value: ?[]const u8 = null, // for req conditions (owned)

    pub const Kind = enum {
        io_ok,
        io_fail,
        req_method,
        req_url,
    };

    pub fn deinit(self: *PathCondition, allocator: std.mem.Allocator) void {
        if (self.module) |m| allocator.free(m);
        if (self.func) |f| allocator.free(f);
        if (self.value) |v| allocator.free(v);
    }

    pub fn dupeOwned(self: PathCondition, allocator: std.mem.Allocator) !PathCondition {
        return .{
            .kind = self.kind,
            .module = try dupeOptionalString(allocator, self.module),
            .func = try dupeOptionalString(allocator, self.func),
            .value = try dupeOptionalString(allocator, self.value),
        };
    }
};

/// A single I/O call in a behavior path's sequence.
pub const PathIoCall = struct {
    module: []const u8, // owned
    func: []const u8, // owned
    /// Canonical argument signature used by the behavior-path canonicalizer.
    /// Pipe-delimited, one token per argument position:
    ///   "lit:<raw>"   - string literal
    ///   "int:<raw>"   - integer literal
    ///   "bool:true"   - boolean literal
    ///   "bool:false"
    ///   "?"           - dynamic or unknown
    /// `null` means the path generator did not record arguments for this
    /// call (older contracts, or a call site where we could not walk the
    /// arguments). A null signature disables all rewrites on this call.
    arg_signature: ?[]const u8 = null, // owned when non-null

    pub fn deinit(self: *PathIoCall, allocator: std.mem.Allocator) void {
        allocator.free(self.module);
        allocator.free(self.func);
        if (self.arg_signature) |sig| allocator.free(sig);
    }

    pub fn dupeOwned(self: PathIoCall, allocator: std.mem.Allocator) !PathIoCall {
        return .{
            .module = try allocator.dupe(u8, self.module),
            .func = try allocator.dupe(u8, self.func),
            .arg_signature = try dupeOptionalString(allocator, self.arg_signature),
        };
    }
};

/// One exhaustively-enumerated execution path through the handler.
/// Each path is a unique combination of (route, I/O outcomes, response).
pub const BehaviorPath = struct {
    route_method: []const u8, // owned
    route_pattern: []const u8, // owned
    conditions: std.ArrayList(PathCondition),
    io_sequence: std.ArrayList(PathIoCall),
    response_status: u16,
    io_depth: u32,
    is_failure_path: bool,

    pub fn deinit(self: *BehaviorPath, allocator: std.mem.Allocator) void {
        allocator.free(self.route_method);
        allocator.free(self.route_pattern);
        for (self.conditions.items) |*c| {
            @constCast(c).deinit(allocator);
        }
        self.conditions.deinit(allocator);
        for (self.io_sequence.items) |*io| {
            @constCast(io).deinit(allocator);
        }
        self.io_sequence.deinit(allocator);
    }

    pub fn dupeOwned(self: *const BehaviorPath, allocator: std.mem.Allocator) !BehaviorPath {
        var conditions: std.ArrayList(PathCondition) = .empty;
        errdefer {
            for (conditions.items) |*c| @constCast(c).deinit(allocator);
            conditions.deinit(allocator);
        }
        try conditions.ensureTotalCapacity(allocator, self.conditions.items.len);
        for (self.conditions.items) |c| {
            conditions.appendAssumeCapacity(try c.dupeOwned(allocator));
        }

        var io_sequence: std.ArrayList(PathIoCall) = .empty;
        errdefer {
            for (io_sequence.items) |*io| @constCast(io).deinit(allocator);
            io_sequence.deinit(allocator);
        }
        try io_sequence.ensureTotalCapacity(allocator, self.io_sequence.items.len);
        for (self.io_sequence.items) |io| {
            io_sequence.appendAssumeCapacity(try io.dupeOwned(allocator));
        }

        return .{
            .route_method = try allocator.dupe(u8, self.route_method),
            .route_pattern = try allocator.dupe(u8, self.route_pattern),
            .conditions = conditions,
            .io_sequence = io_sequence,
            .response_status = self.response_status,
            .io_depth = self.io_depth,
            .is_failure_path = self.is_failure_path,
        };
    }
};

pub const ServiceCallInfo = struct {
    service: []const u8, // owned
    route_pattern: []const u8, // owned
    dynamic: bool = false,
    path_params: std.ArrayList([]const u8) = .empty, // each entry owned
    path_params_dynamic: bool = false,
    query_keys: std.ArrayList([]const u8) = .empty, // each entry owned
    query_dynamic: bool = false,
    header_keys: std.ArrayList([]const u8) = .empty, // each entry owned
    header_dynamic: bool = false,
    has_body: bool = false,
    body_dynamic: bool = false,

    pub fn markAllDynamic(self: *ServiceCallInfo) void {
        self.path_params_dynamic = true;
        self.query_dynamic = true;
        self.header_dynamic = true;
        self.body_dynamic = true;
    }

    pub fn deinit(self: *ServiceCallInfo, allocator: std.mem.Allocator) void {
        allocator.free(self.service);
        allocator.free(self.route_pattern);
        for (self.path_params.items) |param| allocator.free(param);
        self.path_params.deinit(allocator);
        for (self.query_keys.items) |key| allocator.free(key);
        self.query_keys.deinit(allocator);
        for (self.header_keys.items) |key| allocator.free(key);
        self.header_keys.deinit(allocator);
    }
};

/// Aggregate of module capabilities required by a handler's imports.
/// Stable SHA-256 hash over the canonically-ordered tag names lets the
/// runtime detect drift between the embedded contract and the linked
/// module registry.
pub const CapabilityMatrix = struct {
    items: [module_binding.capability_count]module_binding.ModuleCapability = undefined,
    len: u8 = 0,
    hash: [32]u8 = [_]u8{0} ** 32,

    pub const empty: CapabilityMatrix = .{};

    pub fn slice(self: *const CapabilityMatrix) []const module_binding.ModuleCapability {
        return self.items[0..self.len];
    }

    pub fn has(self: *const CapabilityMatrix, cap: module_binding.ModuleCapability) bool {
        for (self.slice()) |c| {
            if (c == cap) return true;
        }
        return false;
    }
};

/// Union `required_capabilities` across every module resolved from
/// `specifiers`. Unknown specifiers (third-party modules not in the linked
/// registry) are silently skipped.
pub fn computeCapabilityMatrix(specifiers: []const []const u8) CapabilityMatrix {
    var matrix: CapabilityMatrix = .{};
    var seen = [_]bool{false} ** module_binding.capability_count;
    for (specifiers) |spec| {
        const binding = builtin_modules.fromSpecifier(spec) orelse continue;
        for (binding.required_capabilities) |c| {
            seen[@intFromEnum(c)] = true;
        }
    }
    var n: u8 = 0;
    for (std.enums.values(module_binding.ModuleCapability)) |c| {
        if (seen[@intFromEnum(c)]) {
            matrix.items[n] = c;
            n += 1;
        }
    }
    matrix.len = n;
    matrix.hash = module_binding.capabilityHash(matrix.slice());
    return matrix;
}

pub const HandlerContract = struct {
    version: u32 = 14,
    handler: HandlerLoc,
    routes: std.ArrayList(RouteInfo),
    modules: std.ArrayList([]const u8), // each entry owned
    functions: std.ArrayList(FunctionEntry),
    env: EnvInfo,
    egress: EgressInfo,
    service_calls: std.ArrayList(ServiceCallInfo) = .empty,
    cache: CacheInfo,
    sql: SqlInfo,
    durable: DurableInfo,
    scope: ScopeInfo,
    websocket: WebSocketInfo = .{},
    api: ApiInfo,
    verification: ?VerificationInfo,
    aot: ?AotInfo,
    fault_coverage: ?FaultCoverageInfo = null,
    rate_limiting: ?RateLimitInfo = null,
    properties: ?HandlerProperties = null,
    behaviors: std.ArrayList(BehaviorPath) = .empty,
    behaviors_exhaustive: bool = false,
    capabilities: CapabilityMatrix = .empty,
    /// SHA-256 of the compiled handler bytecode. Stamped by the build
    /// pipeline after compile and verified at runtime startup.
    artifact_sha256: [32]u8 = [_]u8{0} ** 32,
    /// SHA-256 of the zigts rule registry used to extract this contract.
    /// Populated in `build()`; verified at runtime against the registry the
    /// running binary was linked with.
    policy_hash: [32]u8 = [_]u8{0} ** 32,

    pub const FunctionEntry = struct {
        module: []const u8, // owned
        names: std.ArrayList([]const u8), // each entry owned
    };

    pub fn deinit(self: *HandlerContract, allocator: std.mem.Allocator) void {
        allocator.free(self.handler.path);
        for (self.routes.items) |route| {
            allocator.free(route.pattern);
        }
        self.routes.deinit(allocator);
        for (self.modules.items) |m| {
            allocator.free(m);
        }
        self.modules.deinit(allocator);
        for (self.functions.items) |*entry| {
            allocator.free(entry.module);
            for (entry.names.items) |n| {
                allocator.free(n);
            }
            entry.names.deinit(allocator);
        }
        self.functions.deinit(allocator);
        for (self.env.literal.items) |s| {
            allocator.free(s);
        }
        self.env.literal.deinit(allocator);
        for (self.egress.hosts.items) |s| {
            allocator.free(s);
        }
        self.egress.hosts.deinit(allocator);
        for (self.egress.urls.items) |s| {
            allocator.free(s);
        }
        self.egress.urls.deinit(allocator);
        for (self.service_calls.items) |*call| {
            call.deinit(allocator);
        }
        self.service_calls.deinit(allocator);
        for (self.cache.namespaces.items) |s| {
            allocator.free(s);
        }
        self.cache.namespaces.deinit(allocator);
        self.sql.deinit(allocator);
        self.durable.deinit(allocator);
        self.scope.deinit(allocator);
        self.api.deinit(allocator);
        for (self.behaviors.items) |*b| {
            @constCast(b).deinit(allocator);
        }
        self.behaviors.deinit(allocator);
    }
};

// -------------------------------------------------------------------------
// Contract builder
// -------------------------------------------------------------------------

/// Builds a HandlerContract by walking the IR once.
/// All strings stored in the contract are duped via the allocator, making
/// the contract safe to use after the parser and atom table are freed.
pub const ContractBuilder = struct {
    allocator: std.mem.Allocator,
    ir_view: IrView,
    atoms: ?*context.AtomTable,
    type_env: ?*const TypeEnv,
    type_checker: ?*const TypeChecker,

    // Binding tracking: maps local slot -> function binding metadata for call-site analysis.
    // Populated during scanImports from the module binding registry.
    generic_bindings: std.ArrayList(GenericBinding),

    // Collected data (all strings are duped/owned)
    modules_list: std.ArrayList([]const u8),
    functions_map: std.ArrayList(HandlerContract.FunctionEntry),
    env_literals: std.ArrayList([]const u8),
    env_dynamic: bool,
    egress_hosts: std.ArrayList([]const u8),
    egress_urls: std.ArrayList([]const u8),
    egress_dynamic: bool,
    service_calls: std.ArrayList(ServiceCallInfo),
    cache_namespaces: std.ArrayList([]const u8),
    cache_dynamic: bool,
    sql_queries: std.ArrayList(SqlQueryInfo),
    sql_dynamic: bool,
    scope_used: bool,
    scope_names: std.ArrayList([]const u8),
    scope_dynamic: bool,
    scope_max_depth: u32 = 0,
    websocket: WebSocketInfo = .{},
    durable_used: bool,
    durable_key_literals: std.ArrayList([]const u8),
    durable_key_dynamic: bool,
    durable_step_names: std.ArrayList([]const u8),
    durable_step_dynamic: bool = false,
    durable_timers: bool = false,
    durable_signal_names: std.ArrayList([]const u8) = .empty,
    durable_signal_dynamic: bool = false,
    durable_producer_key_literals: std.ArrayList([]const u8) = .empty,
    durable_producer_key_dynamic: bool = false,
    durable_workflow: DurableWorkflow = .{},
    durable_run_count: u32 = 0,
    api_schemas: std.ArrayList(ApiSchemaInfo),
    api_request_schema_refs: std.ArrayList([]const u8),
    api_request_schema_dynamic: bool,
    api_routes: std.ArrayList(ApiRouteInfo),
    api_bearer_auth: bool,
    api_jwt_auth: bool,
    api_schemas_dynamic: bool,
    api_routes_dynamic: bool,

    // Effect tracking
    has_nondeterministic_builtin: bool = false,

    const EffectSummary = struct {
        has_any_call: bool = false,
        io: module_binding.EffectClass = .none,
        has_bare_write: bool = false,
        has_cache_read: bool = false,
        has_egress: bool = false,

        fn includeCall(self: *EffectSummary, effect: module_binding.EffectClass, is_durable: bool) void {
            switch (effect) {
                .write => {
                    self.io = .write;
                    if (!is_durable) self.has_bare_write = true;
                },
                .read => {
                    if (self.io == .none) self.io = .read;
                },
                .none => {},
            }
        }

        fn includeEgress(self: *EffectSummary) void {
            // fetchSync is a conservative bare write because the HTTP method
            // is not known during contract extraction.
            self.has_egress = true;
            self.has_any_call = true;
            self.io = .write;
            self.has_bare_write = true;
        }
    };

    /// A tracked binding from scanImports: maps a local variable slot to its
    /// FunctionBinding metadata from the module registry.
    const GenericBinding = struct {
        slot: u16,
        module_specifier: []const u8,
        binding_name: []const u8,
        extractions: []const module_binding.ContractExtraction,
        flags: module_binding.ContractFlags,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        ir_view: IrView,
        atoms: ?*context.AtomTable,
        type_env: ?*const TypeEnv,
        type_checker: ?*const TypeChecker,
    ) ContractBuilder {
        return .{
            .allocator = allocator,
            .ir_view = ir_view,
            .atoms = atoms,
            .type_env = type_env,
            .type_checker = type_checker,
            .generic_bindings = .empty,
            .modules_list = .empty,
            .functions_map = .empty,
            .env_literals = .empty,
            .env_dynamic = false,
            .egress_hosts = .empty,
            .egress_urls = .empty,
            .egress_dynamic = false,
            .service_calls = .empty,
            .cache_namespaces = .empty,
            .cache_dynamic = false,
            .sql_queries = .empty,
            .sql_dynamic = false,
            .scope_used = false,
            .scope_names = .empty,
            .scope_dynamic = false,
            .durable_used = false,
            .durable_key_literals = .empty,
            .durable_key_dynamic = false,
            .durable_step_names = .empty,
            .api_schemas = .empty,
            .api_request_schema_refs = .empty,
            .api_request_schema_dynamic = false,
            .api_routes = .empty,
            .api_bearer_auth = false,
            .api_jwt_auth = false,
            .api_schemas_dynamic = false,
            .api_routes_dynamic = false,
        };
    }

    /// Free all builder-owned resources. Safe to call whether or not build()
    /// was called: if build() moved the lists into a HandlerContract, the
    /// items slices are empty and these loops are no-ops.
    pub fn deinit(self: *ContractBuilder) void {
        self.generic_bindings.deinit(self.allocator);
        for (self.env_literals.items) |s| self.allocator.free(s);
        self.env_literals.deinit(self.allocator);
        for (self.egress_hosts.items) |s| self.allocator.free(s);
        self.egress_hosts.deinit(self.allocator);
        for (self.egress_urls.items) |s| self.allocator.free(s);
        self.egress_urls.deinit(self.allocator);
        for (self.service_calls.items) |*call| call.deinit(self.allocator);
        self.service_calls.deinit(self.allocator);
        for (self.cache_namespaces.items) |s| self.allocator.free(s);
        self.cache_namespaces.deinit(self.allocator);
        for (self.sql_queries.items) |*query| query.deinit(self.allocator);
        self.sql_queries.deinit(self.allocator);
        for (self.scope_names.items) |s| self.allocator.free(s);
        self.scope_names.deinit(self.allocator);
        for (self.durable_key_literals.items) |s| self.allocator.free(s);
        self.durable_key_literals.deinit(self.allocator);
        for (self.durable_step_names.items) |s| self.allocator.free(s);
        self.durable_step_names.deinit(self.allocator);
        for (self.durable_signal_names.items) |s| self.allocator.free(s);
        self.durable_signal_names.deinit(self.allocator);
        for (self.durable_producer_key_literals.items) |s| self.allocator.free(s);
        self.durable_producer_key_literals.deinit(self.allocator);
        self.durable_workflow.deinit(self.allocator);
        for (self.modules_list.items) |s| self.allocator.free(s);
        self.modules_list.deinit(self.allocator);
        for (self.functions_map.items) |*entry| {
            self.allocator.free(entry.module);
            for (entry.names.items) |n| self.allocator.free(n);
            entry.names.deinit(self.allocator);
        }
        self.functions_map.deinit(self.allocator);
        for (self.api_schemas.items) |schema| {
            self.allocator.free(schema.name);
            self.allocator.free(schema.schema_json);
        }
        self.api_schemas.deinit(self.allocator);
        for (self.api_request_schema_refs.items) |schema_ref| {
            self.allocator.free(schema_ref);
        }
        self.api_request_schema_refs.deinit(self.allocator);
        for (self.api_routes.items) |*route| {
            route.deinit(self.allocator);
        }
        self.api_routes.deinit(self.allocator);
    }

    /// Build the contract from the IR. Single-pass walk over all nodes.
    /// The returned HandlerContract owns all its string data.
    pub fn build(
        self: *ContractBuilder,
        handler_path: []const u8,
        handler_loc: ?ir.SourceLocation,
        handler_fn: ?NodeIndex,
        dispatch: ?*const PatternDispatchTable,
        default_response: bool,
        verification: ?VerificationInfo,
    ) !HandlerContract {
        // Phase 1: Scan imports to discover modules, functions, and binding slots
        try self.scanImports();

        // Phase 2: Scan all call sites for env/fetchSync/cache usage
        try self.scanCallSites();

        // Phase 2b: Scan top-level function declarations for WebSocket event
        // exports (onOpen/onMessage/onClose/onError).
        try self.scanWebSocketExports();

        if (handler_fn) |hf| {
            try self.extractScopeUsage(hf);
            try self.extractDurableWorkflow(handler_path, hf);
        }

        // Phase 3: Compute handler effect properties
        const properties = self.computeProperties();

        // Phase 3b: Detect rate limiting (guard composition + cacheIncr)
        const rate_limiting = self.detectRateLimiting();

        // Build routes from dispatch table
        var routes: std.ArrayList(RouteInfo) = .empty;
        errdefer {
            for (routes.items) |r| self.allocator.free(r.pattern);
            routes.deinit(self.allocator);
        }
        if (dispatch) |d| {
            for (d.patterns) |pattern| {
                const is_aot = switch (pattern.pattern_type) {
                    .exact => true,
                    .prefix => pattern.response_template_prefix != null,
                    else => false,
                };
                if (!is_aot) continue;

                const pattern_dupe = try self.allocator.dupe(u8, pattern.url_bytes);
                errdefer self.allocator.free(pattern_dupe);

                try routes.append(self.allocator, .{
                    .pattern = pattern_dupe,
                    .route_type = switch (pattern.pattern_type) {
                        .exact => "exact",
                        .prefix => "prefix",
                        else => "unknown",
                    },
                    .field = switch (pattern.url_atom) {
                        .path => "path",
                        else => "url",
                    },
                    .status = pattern.status,
                    .content_type = contentTypeFor(pattern.content_type_idx),
                    .aot = is_aot,
                });
            }
        }

        // Compute AOT info
        var aot_info: ?AotInfo = null;
        if (dispatch) |d| {
            var pattern_count: u32 = 0;
            for (d.patterns) |pattern| {
                switch (pattern.pattern_type) {
                    .exact => pattern_count += 1,
                    .prefix => {
                        if (pattern.response_template_prefix != null) pattern_count += 1;
                    },
                    else => {},
                }
            }
            aot_info = .{
                .pattern_count = pattern_count,
                .has_default = default_response,
            };
        }

        var contract = HandlerContract{
            .handler = .{
                .path = try self.allocator.dupe(u8, handler_path),
                .line = if (handler_loc) |loc| loc.line else 0,
                .column = if (handler_loc) |loc| loc.column else 0,
            },
            .routes = routes,
            .modules = self.modules_list,
            .functions = self.functions_map,
            .env = .{
                .literal = self.env_literals,
                .dynamic = self.env_dynamic,
            },
            .egress = .{
                .hosts = self.egress_hosts,
                .urls = self.egress_urls,
                .dynamic = self.egress_dynamic,
            },
            .service_calls = self.service_calls,
            .cache = .{
                .namespaces = self.cache_namespaces,
                .dynamic = self.cache_dynamic,
            },
            .sql = .{
                .backend = "sqlite",
                .queries = self.sql_queries,
                .dynamic = self.sql_dynamic,
            },
            .durable = .{
                .used = self.durable_used,
                .keys = .{
                    .literal = self.durable_key_literals,
                    .dynamic = self.durable_key_dynamic,
                },
                .steps = self.durable_step_names,
                .timers = self.durable_timers,
                .signals = .{
                    .literal = self.durable_signal_names,
                    .dynamic = self.durable_signal_dynamic,
                },
                .producer_keys = .{
                    .literal = self.durable_producer_key_literals,
                    .dynamic = self.durable_producer_key_dynamic,
                },
                .workflow = self.durable_workflow,
            },
            .scope = .{
                .used = self.scope_used,
                .names = self.scope_names,
                .dynamic = self.scope_dynamic,
                .max_depth = self.scope_max_depth,
            },
            .websocket = self.websocket,
            .api = .{
                .schemas = self.api_schemas,
                .requests = .{
                    .schema_refs = self.api_request_schema_refs,
                    .dynamic = self.api_request_schema_dynamic,
                },
                .auth = .{
                    .bearer = self.api_bearer_auth,
                    .jwt = self.api_jwt_auth,
                },
                .routes = self.api_routes,
                .schemas_dynamic = self.api_schemas_dynamic,
                .routes_dynamic = self.api_routes_dynamic,
            },
            .verification = verification,
            .aot = aot_info,
            .rate_limiting = rate_limiting,
            .properties = properties,
        };

        contract.capabilities = computeCapabilityMatrix(contract.modules.items);
        contract.policy_hash = currentPolicyHashRaw();

        // Clear moved lists so deinit() won't double-free
        self.modules_list = .empty;
        self.functions_map = .empty;
        self.env_literals = .empty;
        self.egress_hosts = .empty;
        self.egress_urls = .empty;
        self.service_calls = .empty;
        self.cache_namespaces = .empty;
        self.sql_queries = .empty;
        self.scope_names = .empty;
        self.durable_key_literals = .empty;
        self.durable_step_names = .empty;
        self.durable_signal_names = .empty;
        self.durable_producer_key_literals = .empty;
        self.durable_workflow = .{};
        self.api_schemas = .empty;
        self.api_request_schema_refs = .empty;
        self.api_routes = .empty;

        return contract;
    }

    // -----------------------------------------------------------------
    // Phase 1: Import scanning
    // -----------------------------------------------------------------

    /// Scan top-level function declarations for the four reserved WebSocket
    /// event names. A match sets the corresponding `websocket` flag so the
    /// contract's downstream consumers (runtime gateway, deploy manifest)
    /// can decide whether WebSocket support is needed without re-parsing.
    fn scanWebSocketExports(self: *ContractBuilder) !void {
        const node_count = self.ir_view.nodeCount();
        var idx_usize: usize = 0;
        while (idx_usize < node_count) : (idx_usize += 1) {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            if (tag != .function_decl and tag != .var_decl) continue;

            const decl = self.ir_view.getVarDecl(idx) orelse continue;
            if (decl.binding.kind != .global) continue;

            const init_tag = self.ir_view.getTag(decl.init) orelse continue;
            if (init_tag != .function_expr and init_tag != .arrow_function) continue;

            const name = self.resolveAtomName(decl.binding.slot) orelse continue;
            if (std.mem.eql(u8, name, "onOpen")) {
                self.websocket.on_open = true;
            } else if (std.mem.eql(u8, name, "onMessage")) {
                self.websocket.on_message = true;
            } else if (std.mem.eql(u8, name, "onClose")) {
                self.websocket.on_close = true;
            } else if (std.mem.eql(u8, name, "onError")) {
                self.websocket.on_error = true;
            }
        }
    }

    fn scanImports(self: *ContractBuilder) !void {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            if (tag != .import_decl) continue;

            const import_decl = self.ir_view.getImportDecl(idx) orelse continue;
            const module_str = self.ir_view.getString(import_decl.module_idx) orelse continue;

            // Only track virtual modules
            if (builtin_modules.fromSpecifier(module_str) == null) continue;

            // Add module to list (deduplicated, duped)
            if (!containsString(self.modules_list.items, module_str)) {
                const duped = try self.allocator.dupe(u8, module_str);
                errdefer self.allocator.free(duped);
                try self.modules_list.append(self.allocator, duped);
            }

            // Scan specifiers to track function names and binding slots
            var func_names: std.ArrayList([]const u8) = .empty;
            var func_module_str: []const u8 = "";

            var j: u8 = 0;
            while (j < import_decl.specifiers_count) : (j += 1) {
                const spec_idx = self.ir_view.getListIndex(import_decl.specifiers_start, j);
                const spec = self.ir_view.getImportSpec(spec_idx) orelse continue;
                const imported_name = self.resolveAtomName(spec.imported_atom) orelse continue;

                const name_duped = try self.allocator.dupe(u8, imported_name);
                errdefer self.allocator.free(name_duped);
                try func_names.append(self.allocator, name_duped);
                func_module_str = module_str;

                // Look up the function in the binding registry and track its
                // contract extraction rules and flags for scanCallSites.
                if (builtin_modules.findExport(module_str, imported_name)) |entry| {
                    const has_extractions = entry.func.contract_extractions.len > 0;
                    const has_flags = entry.func.contract_flags.sets_scope_used or
                        entry.func.contract_flags.sets_durable_used or
                        entry.func.contract_flags.sets_durable_timers or
                        entry.func.contract_flags.sets_bearer_auth or
                        entry.func.contract_flags.sets_jwt_auth;
                    if (has_extractions or has_flags) {
                        try self.generic_bindings.append(self.allocator, .{
                            .slot = spec.local_binding.slot,
                            .module_specifier = entry.binding.specifier,
                            .binding_name = imported_name,
                            .extractions = entry.func.contract_extractions,
                            .flags = entry.func.contract_flags,
                        });
                    }
                }
            }

            if (func_names.items.len > 0) {
                // Merge with existing entry for same module, or add new
                var merged = false;
                for (self.functions_map.items) |*existing| {
                    if (std.mem.eql(u8, existing.module, func_module_str)) {
                        for (func_names.items) |name| {
                            if (!containsString(existing.names.items, name)) {
                                try existing.names.append(self.allocator, name);
                            } else {
                                self.allocator.free(name);
                            }
                        }
                        func_names.deinit(self.allocator);
                        merged = true;
                        break;
                    }
                }
                if (!merged) {
                    const module_duped = try self.allocator.dupe(u8, func_module_str);
                    errdefer self.allocator.free(module_duped);
                    try self.functions_map.append(self.allocator, .{
                        .module = module_duped,
                        .names = func_names,
                    });
                }
            } else {
                func_names.deinit(self.allocator);
            }
        }
    }

    // -----------------------------------------------------------------
    // Phase 2: Call site scanning
    // -----------------------------------------------------------------

    fn scanCallSites(self: *ContractBuilder) !void {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            if (tag != .call) continue;

            const call = self.ir_view.getCall(idx) orelse continue;

            // Check what the callee is
            const callee_tag = self.ir_view.getTag(call.callee) orelse continue;

            if (callee_tag == .identifier) {
                const binding = self.ir_view.getBinding(call.callee) orelse continue;

                // Check for fetchSync (undeclared global)
                if (binding.kind == .undeclared_global) {
                    const name = self.resolveAtomName(binding.slot) orelse continue;
                    if (std.mem.eql(u8, name, "fetchSync")) {
                        try self.extractLiteralArg(call, &self.egress_hosts, &self.egress_dynamic, &extractHost);
                        try self.extractLiteralArg(call, &self.egress_urls, &self.egress_dynamic, null);
                        continue;
                    }
                }

                // Check generic bindings from the module binding registry
                for (self.generic_bindings.items) |gb| {
                    if (gb.slot != binding.slot) continue;

                    // Apply contract flags
                    if (gb.flags.sets_scope_used) self.scope_used = true;
                    if (gb.flags.sets_durable_used) self.durable_used = true;
                    if (gb.flags.sets_durable_timers) self.durable_timers = true;
                    if (gb.flags.sets_bearer_auth) self.api_bearer_auth = true;
                    if (gb.flags.sets_jwt_auth) self.api_jwt_auth = true;

                    // Process extraction rules
                    for (gb.extractions) |ext| {
                        switch (ext.category) {
                            // Custom extractors for complex multi-arg patterns
                            .sql_registration => try self.extractSqlRegistration(call),
                            .schema_compile => try self.extractSchemaCompile(call),
                            .route_pattern => try self.extractApiRoutesFromCall(call),
                            .service_call => try self.extractServiceCall(call),
                            // Generic: extract literal from arg N into category bucket
                            else => {
                                if (self.getCategoryTarget(ext.category)) |target| {
                                    const transform: ?*const fn ([]const u8) []const u8 =
                                        if (ext.transform) |t| switch (t) {
                                            .extract_host => &extractHost,
                                            .identity => null,
                                        } else null;
                                    try self.extractLiteralArgAt(call, ext.arg_position, target.list, target.dynamic, transform);
                                }
                            },
                        }
                    }
                    break;
                }
            }

            // Detect nondeterministic builtins: Date.now(), Math.random()
            if (!self.has_nondeterministic_builtin and callee_tag == .member_access) {
                const member = self.ir_view.getMember(call.callee) orelse continue;
                const obj_tag = self.ir_view.getTag(member.object) orelse continue;
                if (obj_tag == .identifier) {
                    const binding = self.ir_view.getBinding(member.object) orelse continue;
                    if (binding.kind == .global or binding.kind == .undeclared_global) {
                        const obj_name = self.resolveAtomName(binding.slot) orelse continue;
                        const prop_name = self.resolveAtomName(member.property) orelse continue;
                        if ((std.mem.eql(u8, obj_name, "Date") and std.mem.eql(u8, prop_name, "now")) or
                            (std.mem.eql(u8, obj_name, "Math") and std.mem.eql(u8, prop_name, "random")))
                        {
                            self.has_nondeterministic_builtin = true;
                        }
                    }
                }
            }
        }
    }

    /// Extract a literal string from the first argument of a call and append
    /// it (deduped, owned) to `target`. If the argument is non-literal, set
    /// `dynamic_flag`. An optional `transform` narrows the extracted string
    /// (e.g. extractHost for URLs) - returning empty means "treat as dynamic".
    fn extractLiteralArg(
        self: *ContractBuilder,
        call: Node.CallExpr,
        target: *std.ArrayList([]const u8),
        dynamic_flag: *bool,
        transform: ?*const fn ([]const u8) []const u8,
    ) !void {
        try self.extractLiteralArgAt(call, 0, target, dynamic_flag, transform);
    }

    fn extractLiteralArgAt(
        self: *ContractBuilder,
        call: Node.CallExpr,
        arg_pos: u8,
        target: *std.ArrayList([]const u8),
        dynamic_flag: *bool,
        transform: ?*const fn ([]const u8) []const u8,
    ) !void {
        if (call.args_count <= arg_pos) return;

        const arg_idx = self.ir_view.getListIndex(call.args_start, arg_pos);
        const arg_tag = self.ir_view.getTag(arg_idx) orelse return;

        if (arg_tag == .lit_string) {
            const str_idx = self.ir_view.getStringIdx(arg_idx) orelse return;
            const raw = self.ir_view.getString(str_idx) orelse return;
            const value = if (transform) |t| t(raw) else raw;
            if (value.len > 0) {
                if (!containsString(target.items, value)) {
                    const duped = try self.allocator.dupe(u8, value);
                    errdefer self.allocator.free(duped);
                    try target.append(self.allocator, duped);
                }
            } else {
                dynamic_flag.* = true;
            }
        } else {
            dynamic_flag.* = true;
        }
    }

    // -----------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------

    /// Map a ContractCategory to the data field pair it writes to.
    const CategoryTarget = struct {
        list: *std.ArrayList([]const u8),
        dynamic: *bool,
    };

    /// Check if a binding slot is tracked for a specific contract category.
    fn isBindingCategory(self: *const ContractBuilder, slot: u16, category: module_binding.ContractCategory) bool {
        for (self.generic_bindings.items) |gb| {
            if (gb.slot != slot) continue;
            for (gb.extractions) |ext| {
                if (ext.category == category) return true;
            }
        }
        return false;
    }

    fn getCategoryTarget(self: *ContractBuilder, category: module_binding.ContractCategory) ?CategoryTarget {
        return switch (category) {
            .env => .{ .list = &self.env_literals, .dynamic = &self.env_dynamic },
            .cache_namespace => .{ .list = &self.cache_namespaces, .dynamic = &self.cache_dynamic },
            .scope_name => .{ .list = &self.scope_names, .dynamic = &self.scope_dynamic },
            .durable_key => .{ .list = &self.durable_key_literals, .dynamic = &self.durable_key_dynamic },
            .durable_step => .{ .list = &self.durable_step_names, .dynamic = &self.durable_step_dynamic },
            .durable_signal => .{ .list = &self.durable_signal_names, .dynamic = &self.durable_signal_dynamic },
            .durable_producer_key => .{ .list = &self.durable_producer_key_literals, .dynamic = &self.durable_producer_key_dynamic },
            .request_schema => .{ .list = &self.api_request_schema_refs, .dynamic = &self.api_request_schema_dynamic },
            .fetch_host => .{ .list = &self.egress_hosts, .dynamic = &self.egress_dynamic },
            // Custom categories are dispatched directly, not via generic target
            .sql_registration, .schema_compile, .route_pattern, .service_call, .cookie_name, .cors_origin, .rate_limit_key => null,
        };
    }

    fn extractScopeUsage(self: *ContractBuilder, handler_fn: NodeIndex) !void {
        const func = self.ir_view.getFunction(handler_fn) orelse return;
        try self.walkScopeDepth(func.body, 0);
    }

    fn walkScopeDepth(self: *ContractBuilder, node_idx: NodeIndex, depth: u32) !void {
        if (node_idx == null_node) return;

        const tag = self.ir_view.getTag(node_idx) orelse return;
        switch (tag) {
            .program, .block => {
                const block = self.ir_view.getBlock(node_idx) orelse return;
                var i: u16 = 0;
                while (i < block.stmts_count) : (i += 1) {
                    try self.walkScopeDepth(self.ir_view.getListIndex(block.stmts_start, i), depth);
                }
            },
            .expr_stmt, .throw_stmt, .return_stmt, .assert_stmt => {
                if (self.ir_view.getOptValue(node_idx)) |value_node| {
                    try self.walkScopeDepth(value_node, depth);
                }
            },
            .var_decl, .function_decl => {
                const decl = self.ir_view.getVarDecl(node_idx) orelse return;
                if (decl.init != null_node) try self.walkScopeDepth(decl.init, depth);
            },
            .if_stmt => {
                const if_stmt = self.ir_view.getIfStmt(node_idx) orelse return;
                try self.walkScopeDepth(if_stmt.condition, depth);
                try self.walkScopeDepth(if_stmt.then_branch, depth);
                try self.walkScopeDepth(if_stmt.else_branch, depth);
            },
            .switch_stmt => {
                const switch_stmt = self.ir_view.getSwitchStmt(node_idx) orelse return;
                try self.walkScopeDepth(switch_stmt.discriminant, depth);
                var i: u8 = 0;
                while (i < switch_stmt.cases_count) : (i += 1) {
                    try self.walkScopeDepth(self.ir_view.getListIndex(switch_stmt.cases_start, i), depth);
                }
            },
            .case_clause => {
                const case_clause = self.ir_view.getCaseClause(node_idx) orelse return;
                try self.walkScopeDepth(case_clause.test_expr, depth);
                var i: u16 = 0;
                while (i < case_clause.body_count) : (i += 1) {
                    try self.walkScopeDepth(self.ir_view.getListIndex(case_clause.body_start, i), depth);
                }
            },
            .for_of_stmt, .for_in_stmt => {
                const for_iter = self.ir_view.getForIter(node_idx) orelse return;
                try self.walkScopeDepth(for_iter.iterable, depth);
                try self.walkScopeDepth(for_iter.body, depth);
            },
            .binary_op => {
                const binary = self.ir_view.getBinary(node_idx) orelse return;
                try self.walkScopeDepth(binary.left, depth);
                try self.walkScopeDepth(binary.right, depth);
            },
            .unary_op => {
                const unary = self.ir_view.getUnary(node_idx) orelse return;
                try self.walkScopeDepth(unary.operand, depth);
            },
            .ternary => {
                const ternary = self.ir_view.getTernary(node_idx) orelse return;
                try self.walkScopeDepth(ternary.condition, depth);
                try self.walkScopeDepth(ternary.then_branch, depth);
                try self.walkScopeDepth(ternary.else_branch, depth);
            },
            .assignment => {
                const assignment = self.ir_view.getAssignment(node_idx) orelse return;
                try self.walkScopeDepth(assignment.target, depth);
                try self.walkScopeDepth(assignment.value, depth);
            },
            .member_access, .computed_access, .optional_chain => {
                const member = self.ir_view.getMember(node_idx) orelse return;
                try self.walkScopeDepth(member.object, depth);
                if (member.computed != null_node) try self.walkScopeDepth(member.computed, depth);
            },
            .array_literal => {
                const array = self.ir_view.getArray(node_idx) orelse return;
                var i: u16 = 0;
                while (i < array.elements_count) : (i += 1) {
                    try self.walkScopeDepth(self.ir_view.getListIndex(array.elements_start, i), depth);
                }
            },
            .object_literal => {
                const obj = self.ir_view.getObject(node_idx) orelse return;
                var i: u16 = 0;
                while (i < obj.properties_count) : (i += 1) {
                    try self.walkScopeDepth(self.ir_view.getListIndex(obj.properties_start, i), depth);
                }
            },
            .object_property => {
                const prop = self.ir_view.getProperty(node_idx) orelse return;
                if (prop.is_computed) try self.walkScopeDepth(prop.key, depth);
                try self.walkScopeDepth(prop.value, depth);
            },
            .call, .method_call, .optional_call => {
                const call = self.ir_view.getCall(node_idx) orelse return;

                if (self.isModuleBindingName(call.callee, "scope")) {
                    self.scope_used = true;

                    const next_depth = depth + 1;
                    if (next_depth > self.scope_max_depth) self.scope_max_depth = next_depth;

                    if (call.args_count > 1) {
                        const callback_idx = self.ir_view.getListIndex(call.args_start, 1);
                        if (self.resolveFunctionNode(callback_idx)) |fn_node| {
                            const fn_expr = self.ir_view.getFunction(fn_node) orelse return;
                            try self.walkScopeDepth(fn_expr.body, next_depth);
                        } else {
                            self.scope_dynamic = true;
                        }
                    } else {
                        self.scope_dynamic = true;
                    }
                    return;
                }

                try self.walkScopeDepth(call.callee, depth);
                var i: u8 = 0;
                while (i < call.args_count) : (i += 1) {
                    try self.walkScopeDepth(self.ir_view.getListIndex(call.args_start, i), depth);
                }
            },
            else => {},
        }
    }

    fn isModuleBindingName(self: *const ContractBuilder, callee: NodeIndex, expected: []const u8) bool {
        const tag = self.ir_view.getTag(callee) orelse return false;
        if (tag != .identifier) return false;

        const binding = self.ir_view.getBinding(callee) orelse return false;
        for (self.generic_bindings.items) |gb| {
            if (gb.slot == binding.slot and std.mem.eql(u8, gb.binding_name, expected)) return true;
        }

        if (binding.kind == .global or binding.kind == .undeclared_global) {
            const name = self.resolveAtomName(binding.slot) orelse return false;
            return std.mem.eql(u8, name, expected);
        }

        return false;
    }

    const WorkflowCursor = struct {
        node_id: []const u8,
        condition: ?[]const u8 = null,

        fn deinit(self: *WorkflowCursor, allocator: std.mem.Allocator) void {
            if (self.condition) |condition| allocator.free(condition);
        }
    };

    fn extractDurableWorkflow(self: *ContractBuilder, handler_path: []const u8, handler_fn: NodeIndex) !void {
        if (!self.durable_used) return;

        const func = self.ir_view.getFunction(handler_fn) orelse return;
        const run_call = self.findDurableRunCall(func.body) orelse {
            self.durable_workflow.proof_level = .none;
            return;
        };

        self.durable_workflow.workflow_id = try self.buildWorkflowId(handler_path, handler_fn, run_call.call_node);
        self.durable_workflow.proof_level = if (self.durable_key_dynamic or self.durable_step_dynamic or self.durable_signal_dynamic or self.durable_run_count > 1)
            .partial
        else
            .complete;

        const callback_fn = self.ir_view.getFunction(run_call.callback_node) orelse {
            self.markWorkflowPartial();
            return;
        };

        var cursors: std.ArrayList(WorkflowCursor) = .empty;
        defer self.deinitWorkflowCursors(&cursors);
        try cursors.append(self.allocator, .{
            .node_id = "start",
            .condition = null,
        });

        try self.walkWorkflowBlock(callback_fn.body, &cursors);
    }

    const RunCallInfo = struct {
        call_node: NodeIndex,
        callback_node: NodeIndex,
    };

    fn findDurableRunCall(self: *ContractBuilder, node: NodeIndex) ?RunCallInfo {
        if (node == null_node) return null;
        const tag = self.ir_view.getTag(node) orelse return null;

        switch (tag) {
            .block, .program => {
                const block = self.ir_view.getBlock(node) orelse return null;
                var i: u16 = 0;
                while (i < block.stmts_count) : (i += 1) {
                    const stmt_idx = self.ir_view.getListIndex(block.stmts_start, i);
                    if (self.findDurableRunCall(stmt_idx)) |info| return info;
                }
            },
            .return_stmt, .expr_stmt => {
                if (self.ir_view.getOptValue(node)) |value| {
                    return self.findDurableRunCall(value);
                }
            },
            .var_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return null;
                if (decl.init != null_node) {
                    return self.findDurableRunCall(decl.init);
                }
            },
            .if_stmt => {
                const if_stmt = self.ir_view.getIfStmt(node) orelse return null;
                if (self.findDurableRunCall(if_stmt.then_branch)) |info| return info;
                if (if_stmt.else_branch != null_node) {
                    if (self.findDurableRunCall(if_stmt.else_branch)) |info| return info;
                }
            },
            .call => {
                const call = self.ir_view.getCall(node) orelse return null;
                if (self.isModuleBindingName(call.callee, "run")) {
                    self.durable_run_count += 1;
                    if (call.args_count >= 2) {
                        const callback_node = self.ir_view.getListIndex(call.args_start, 1);
                        if (self.ir_view.getTag(callback_node) == .function_expr or self.ir_view.getTag(callback_node) == .arrow_function) {
                            return .{
                                .call_node = node,
                                .callback_node = callback_node,
                            };
                        }
                    }
                }
            },
            else => {},
        }

        return null;
    }

    fn walkWorkflowBlock(self: *ContractBuilder, node: NodeIndex, cursors: *std.ArrayList(WorkflowCursor)) anyerror!void {
        if (node == null_node or cursors.items.len == 0) return;
        const tag = self.ir_view.getTag(node) orelse return;
        if (tag != .block and tag != .program) {
            try self.walkWorkflowStatement(node, cursors);
            return;
        }

        const block = self.ir_view.getBlock(node) orelse return;
        var i: u16 = 0;
        while (i < block.stmts_count and cursors.items.len > 0) : (i += 1) {
            try self.walkWorkflowStatement(self.ir_view.getListIndex(block.stmts_start, i), cursors);
        }
    }

    fn walkWorkflowStatement(self: *ContractBuilder, node: NodeIndex, cursors: *std.ArrayList(WorkflowCursor)) anyerror!void {
        if (node == null_node or cursors.items.len == 0) return;
        const tag = self.ir_view.getTag(node) orelse return;

        switch (tag) {
            .block, .program => try self.walkWorkflowBlock(node, cursors),
            .if_stmt => try self.walkWorkflowIf(node, cursors),
            .return_stmt => try self.appendReturnWorkflowNode(node, cursors),
            .expr_stmt => {
                if (self.ir_view.getOptValue(node)) |expr| {
                    _ = try self.appendWorkflowCall(expr, cursors);
                }
            },
            .var_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return;
                if (decl.init != null_node) {
                    _ = try self.appendWorkflowCall(decl.init, cursors);
                }
            },
            .match_expr, .switch_stmt, .for_stmt, .for_of_stmt, .for_in_stmt, .while_stmt, .do_while_stmt => {
                self.markWorkflowPartial();
            },
            else => {},
        }
    }

    fn walkWorkflowIf(self: *ContractBuilder, node: NodeIndex, cursors: *std.ArrayList(WorkflowCursor)) anyerror!void {
        const if_stmt = self.ir_view.getIfStmt(node) orelse return;
        const line = if (self.ir_view.getLoc(node)) |loc| loc.line else 0;
        const column = if (self.ir_view.getLoc(node)) |loc| loc.column else 0;

        const branch_node_id = try self.appendWorkflowNode(
            .branch,
            try std.fmt.allocPrint(self.allocator, "if:{d}:{d}", .{ line, column }),
            null,
            null,
            cursors,
        );

        var then_cursors: std.ArrayList(WorkflowCursor) = .empty;
        defer self.deinitWorkflowCursors(&then_cursors);
        try then_cursors.append(self.allocator, .{
            .node_id = branch_node_id,
            .condition = try std.fmt.allocPrint(self.allocator, "then@{d}:{d}", .{ line, column }),
        });
        try self.walkWorkflowStatement(if_stmt.then_branch, &then_cursors);

        var else_cursors: std.ArrayList(WorkflowCursor) = .empty;
        defer self.deinitWorkflowCursors(&else_cursors);
        try else_cursors.append(self.allocator, .{
            .node_id = branch_node_id,
            .condition = try std.fmt.allocPrint(self.allocator, "else@{d}:{d}", .{ line, column }),
        });
        if (if_stmt.else_branch != null_node) {
            try self.walkWorkflowStatement(if_stmt.else_branch, &else_cursors);
        }

        self.deinitWorkflowCursors(cursors);
        for (then_cursors.items) |cursor| {
            try cursors.append(self.allocator, .{
                .node_id = cursor.node_id,
                .condition = if (cursor.condition) |condition| try self.allocator.dupe(u8, condition) else null,
            });
        }
        for (else_cursors.items) |cursor| {
            try cursors.append(self.allocator, .{
                .node_id = cursor.node_id,
                .condition = if (cursor.condition) |condition| try self.allocator.dupe(u8, condition) else null,
            });
        }
    }

    fn appendWorkflowCall(self: *ContractBuilder, expr: NodeIndex, cursors: *std.ArrayList(WorkflowCursor)) anyerror!bool {
        const tag = self.ir_view.getTag(expr) orelse return false;
        if (tag != .call) return false;

        const call = self.ir_view.getCall(expr) orelse return false;
        const workflow_call = self.describeWorkflowCall(call) orelse return false;
        _ = try self.appendWorkflowNode(
            workflow_call.kind,
            workflow_call.label,
            workflow_call.detail,
            null,
            cursors,
        );
        return true;
    }

    const WorkflowCall = struct {
        kind: DurableWorkflowNodeKind,
        label: []const u8,
        detail: ?[]const u8 = null,
    };

    fn describeWorkflowCall(self: *ContractBuilder, call: Node.CallExpr) ?WorkflowCall {
        if (self.isModuleBindingName(call.callee, "step")) {
            const label = if (call.args_count > 0)
                self.workflowStringLabel(call.args_start, 0, "<dynamic step>")
            else
                self.allocator.dupe(u8, "step") catch return null;
            return .{ .kind = .step, .label = label };
        }
        if (self.isModuleBindingName(call.callee, "stepWithTimeout")) {
            const label = if (call.args_count > 0)
                self.workflowStringLabel(call.args_start, 0, "<dynamic step>")
            else
                self.allocator.dupe(u8, "stepWithTimeout") catch return null;
            return .{
                .kind = .step_with_timeout,
                .label = label,
                .detail = self.workflowNumberDetail(call.args_start, 1, "timeoutMs"),
            };
        }
        if (self.isModuleBindingName(call.callee, "sleep")) {
            return .{
                .kind = .sleep,
                .label = self.allocator.dupe(u8, "sleep") catch return null,
                .detail = self.workflowNumberDetail(call.args_start, 0, "delayMs"),
            };
        }
        if (self.isModuleBindingName(call.callee, "sleepUntil")) {
            return .{
                .kind = .sleep_until,
                .label = self.allocator.dupe(u8, "sleepUntil") catch return null,
                .detail = self.workflowNumberDetail(call.args_start, 0, "untilMs"),
            };
        }
        if (self.isModuleBindingName(call.callee, "waitSignal")) {
            const label = if (call.args_count > 0)
                self.workflowStringLabel(call.args_start, 0, "<dynamic signal>")
            else
                self.allocator.dupe(u8, "waitSignal") catch return null;
            return .{ .kind = .wait_signal, .label = label };
        }
        if (self.isModuleBindingName(call.callee, "signal")) {
            const label = if (call.args_count > 1)
                self.workflowStringLabel(call.args_start, 1, "<dynamic signal>")
            else
                self.allocator.dupe(u8, "signal") catch return null;
            return .{ .kind = .signal, .label = label };
        }
        if (self.isModuleBindingName(call.callee, "signalAt")) {
            const label = if (call.args_count > 1)
                self.workflowStringLabel(call.args_start, 1, "<dynamic signal>")
            else
                self.allocator.dupe(u8, "signalAt") catch return null;
            return .{
                .kind = .signal_at,
                .label = label,
                .detail = self.workflowNumberDetail(call.args_start, 2, "atMs"),
            };
        }
        return null;
    }

    fn appendReturnWorkflowNode(self: *ContractBuilder, node: NodeIndex, cursors: *std.ArrayList(WorkflowCursor)) !void {
        const status = self.extractWorkflowReturnStatus(node);
        const label = if (status) |code|
            try std.fmt.allocPrint(self.allocator, "Response {d}", .{code})
        else
            try self.allocator.dupe(u8, "Response");
        _ = try self.appendWorkflowNode(.return_response, label, null, status, cursors);
        self.deinitWorkflowCursors(cursors);
    }

    fn appendWorkflowNode(
        self: *ContractBuilder,
        kind: DurableWorkflowNodeKind,
        label: []const u8,
        detail: ?[]const u8,
        status: ?u16,
        cursors: *std.ArrayList(WorkflowCursor),
    ) ![]const u8 {
        const id = try std.fmt.allocPrint(self.allocator, "n{d}", .{self.durable_workflow.nodes.items.len + 1});
        for (cursors.items) |cursor| {
            try self.durable_workflow.edges.append(self.allocator, .{
                .from = try self.allocator.dupe(u8, cursor.node_id),
                .to = try self.allocator.dupe(u8, id),
                .condition = if (cursor.condition) |condition| try self.allocator.dupe(u8, condition) else null,
            });
        }
        try self.durable_workflow.nodes.append(self.allocator, .{
            .id = id,
            .kind = kind,
            .label = label,
            .detail = detail,
            .status = status,
        });

        self.deinitWorkflowCursors(cursors);
        try cursors.append(self.allocator, .{
            .node_id = id,
            .condition = null,
        });
        return id;
    }

    fn deinitWorkflowCursors(self: *ContractBuilder, cursors: *std.ArrayList(WorkflowCursor)) void {
        for (cursors.items) |*cursor| cursor.deinit(self.allocator);
        cursors.deinit(self.allocator);
        cursors.* = .empty;
    }

    fn workflowStringLabel(
        self: *ContractBuilder,
        args_start: NodeIndex,
        arg_pos: u8,
        fallback: []const u8,
    ) []const u8 {
        const arg_idx = self.ir_view.getListIndex(args_start, arg_pos);
        if (self.getLiteralString(arg_idx)) |label| {
            return self.allocator.dupe(u8, label) catch fallback;
        }
        self.markWorkflowPartial();
        return self.allocator.dupe(u8, fallback) catch fallback;
    }

    fn workflowNumberDetail(self: *ContractBuilder, args_start: NodeIndex, arg_pos: u8, field_name: []const u8) ?[]const u8 {
        const arg_idx = self.ir_view.getListIndex(args_start, arg_pos);
        if (self.getLiteralNumber(arg_idx)) |num| {
            return std.fmt.allocPrint(self.allocator, "{s}={d}", .{ field_name, num }) catch null;
        }
        self.markWorkflowPartial();
        return null;
    }

    fn getLiteralNumber(self: *const ContractBuilder, node_idx: NodeIndex) ?i64 {
        const tag = self.ir_view.getTag(node_idx) orelse return null;
        return switch (tag) {
            .lit_int => if (self.ir_view.getIntValue(node_idx)) |value| @as(i64, value) else null,
            .lit_float => blk: {
                const float_idx = self.ir_view.getFloatIdx(node_idx) orelse break :blk null;
                const value = self.ir_view.getFloat(float_idx) orelse break :blk null;
                if (@floor(value) != value) break :blk null;
                break :blk @intFromFloat(value);
            },
            else => null,
        };
    }

    fn extractWorkflowReturnStatus(self: *const ContractBuilder, node: NodeIndex) ?u16 {
        const ret_val = self.ir_view.getOptValue(node) orelse return null;
        const ret_tag = self.ir_view.getTag(ret_val) orelse return null;
        if (ret_tag != .call and ret_tag != .method_call) return null;

        const call = self.ir_view.getCall(ret_val) orelse return null;
        if (!self.isResponseHelper(call.callee)) return null;
        if (call.args_count < 2) return 200;
        const status = self.extractStatusFromOptionsNode(self.ir_view.getListIndex(call.args_start, 1)) orelse return 200;
        return if (status >= 100 and status <= 599) status else 200;
    }

    fn isResponseHelper(self: *const ContractBuilder, callee: NodeIndex) bool {
        const tag = self.ir_view.getTag(callee) orelse return false;
        if (tag != .member_access) return false;
        const member = self.ir_view.getMember(callee) orelse return false;

        const obj_tag = self.ir_view.getTag(member.object) orelse return false;
        if (obj_tag != .identifier) return false;
        const binding = self.ir_view.getBinding(member.object) orelse return false;
        if (binding.kind != .undeclared_global) return false;

        const obj_name = self.resolveAtomName(binding.slot) orelse return false;
        if (!std.mem.eql(u8, obj_name, "Response")) return false;

        const method_name = self.resolveAtomName(member.property) orelse return false;
        return std.mem.eql(u8, method_name, "json") or
            std.mem.eql(u8, method_name, "text") or
            std.mem.eql(u8, method_name, "html") or
            std.mem.eql(u8, method_name, "redirect");
    }

    fn buildWorkflowId(self: *ContractBuilder, handler_path: []const u8, handler_fn: NodeIndex, run_call_node: NodeIndex) ![]const u8 {
        const func = self.ir_view.getFunction(handler_fn) orelse return std.fmt.allocPrint(self.allocator, "{s}:workflow", .{handler_path});
        const func_name = if (func.name_atom != 0)
            (self.resolveAtomName(func.name_atom) orelse "handler")
        else
            "handler";
        const line = if (self.ir_view.getLoc(run_call_node)) |loc| loc.line else 0;
        const column = if (self.ir_view.getLoc(run_call_node)) |loc| loc.column else 0;
        return std.fmt.allocPrint(self.allocator, "{s}:{s}:{d}:{d}", .{
            handler_path,
            func_name,
            line,
            column,
        });
    }

    fn markWorkflowPartial(self: *ContractBuilder) void {
        self.durable_workflow.proof_level = .partial;
    }

    fn resolveAtomName(self: *const ContractBuilder, atom_idx: u16) ?[]const u8 {
        // Try predefined atoms first
        const atom: object.Atom = @enumFromInt(atom_idx);
        if (atom.toPredefinedName()) |name| return name;

        // Try dynamic atom table
        if (self.atoms) |table| {
            return table.getName(atom);
        }
        return null;
    }

    fn extractSchemaCompile(self: *ContractBuilder, call: Node.CallExpr) !void {
        if (call.args_count < 2) {
            self.api_schemas_dynamic = true;
            return;
        }

        const name_idx = self.ir_view.getListIndex(call.args_start, 0);
        const name = self.getLiteralString(name_idx) orelse {
            self.api_schemas_dynamic = true;
            return;
        };

        const schema_idx = self.ir_view.getListIndex(call.args_start, 1);
        const schema_json = (try self.extractSchemaJson(schema_idx)) orelse {
            self.api_schemas_dynamic = true;
            return;
        };
        errdefer self.allocator.free(schema_json);

        try self.upsertApiSchema(name, schema_json);
    }

    fn extractSqlRegistration(self: *ContractBuilder, call: Node.CallExpr) !void {
        if (call.args_count < 2) {
            self.sql_dynamic = true;
            return;
        }

        const name_idx = self.ir_view.getListIndex(call.args_start, 0);
        const query_name = self.getLiteralString(name_idx) orelse {
            self.sql_dynamic = true;
            return;
        };

        const stmt_idx = self.ir_view.getListIndex(call.args_start, 1);
        const statement = self.getLiteralString(stmt_idx) orelse {
            self.sql_dynamic = true;
            return;
        };

        for (self.sql_queries.items) |query| {
            if (!std.mem.eql(u8, query.name, query_name)) continue;
            if (std.mem.eql(u8, query.statement, statement)) return;
            return error.DuplicateSqlQueryName;
        }

        try self.sql_queries.append(self.allocator, .{
            .name = try self.allocator.dupe(u8, query_name),
            .statement = try self.allocator.dupe(u8, statement),
            .operation = "",
            .tables = .empty,
        });
    }

    fn extractSchemaJson(self: *ContractBuilder, node_idx: NodeIndex) !?[]u8 {
        const tag = self.ir_view.getTag(node_idx) orelse return null;
        switch (tag) {
            .lit_string => {
                const raw = self.getLiteralString(node_idx) orelse return null;
                var parsed = std.json.parseFromSlice(std.json.Value, self.allocator, raw, .{}) catch return null;
                defer parsed.deinit();
                return try self.allocator.dupe(u8, raw);
            },
            .call => {
                const json_arg = self.getJsonStringifyArg(node_idx) orelse return null;
                return try self.serializeJsonLiteral(json_arg);
            },
            else => return null,
        }
    }

    fn getJsonStringifyArg(self: *const ContractBuilder, node_idx: NodeIndex) ?NodeIndex {
        const call = self.ir_view.getCall(node_idx) orelse return null;
        if (call.args_count != 1) return null;

        const callee_tag = self.ir_view.getTag(call.callee) orelse return null;
        if (callee_tag != .member_access) return null;

        const member = self.ir_view.getMember(call.callee) orelse return null;
        if (member.property != @intFromEnum(object.Atom.stringify)) return null;

        const obj_tag = self.ir_view.getTag(member.object) orelse return null;
        if (obj_tag != .identifier) return null;

        const binding = self.ir_view.getBinding(member.object) orelse return null;
        if (binding.kind != .global and binding.kind != .undeclared_global) return null;
        if (binding.slot != @intFromEnum(object.Atom.JSON)) return null;

        return self.ir_view.getListIndex(call.args_start, 0);
    }

    fn serializeJsonLiteral(self: *ContractBuilder, node_idx: NodeIndex) !?[]u8 {
        var output: std.ArrayList(u8) = .empty;
        errdefer output.deinit(self.allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(self.allocator, &output);
        const ok = try self.writeJsonLiteralNode(node_idx, &aw.writer);
        if (!ok) {
            output.deinit(self.allocator);
            return null;
        }
        output = aw.toArrayList();
        return try output.toOwnedSlice(self.allocator);
    }

    fn writeJsonLiteralNode(self: *ContractBuilder, node_idx: NodeIndex, writer: anytype) !bool {
        const tag = self.ir_view.getTag(node_idx) orelse return false;
        switch (tag) {
            .lit_int => {
                const value = self.ir_view.getIntValue(node_idx) orelse return false;
                try writer.print("{d}", .{value});
                return true;
            },
            .lit_float => {
                const float_idx = self.ir_view.getFloatIdx(node_idx) orelse return false;
                const value = self.ir_view.getFloat(float_idx) orelse return false;
                try writer.print("{d}", .{value});
                return true;
            },
            .lit_string => {
                const value = self.getLiteralString(node_idx) orelse return false;
                try writeJsonString(writer, value);
                return true;
            },
            .lit_bool => {
                const value = self.ir_view.getBoolValue(node_idx) orelse return false;
                try writer.writeAll(if (value) "true" else "false");
                return true;
            },
            .lit_null => {
                try writer.writeAll("null");
                return true;
            },
            .unary_op => {
                const unary = self.ir_view.getUnary(node_idx) orelse return false;
                if (unary.op != .neg) return false;
                try writer.writeByte('-');
                return self.writeJsonLiteralNode(unary.operand, writer);
            },
            .array_literal => {
                const arr = self.ir_view.getArray(node_idx) orelse return false;
                try writer.writeByte('[');
                var i: u16 = 0;
                while (i < arr.elements_count) : (i += 1) {
                    if (i > 0) try writer.writeAll(", ");
                    const elem_idx = self.ir_view.getListIndex(arr.elements_start, i);
                    if (!try self.writeJsonLiteralNode(elem_idx, writer)) return false;
                }
                try writer.writeByte(']');
                return true;
            },
            .object_literal => {
                const obj = self.ir_view.getObject(node_idx) orelse return false;
                try writer.writeByte('{');
                var i: u16 = 0;
                while (i < obj.properties_count) : (i += 1) {
                    const prop_idx = self.ir_view.getListIndex(obj.properties_start, i);
                    const prop_tag = self.ir_view.getTag(prop_idx) orelse return false;
                    if (prop_tag != .object_property) return false;

                    const prop = self.ir_view.getProperty(prop_idx) orelse return false;
                    const key = self.getObjectPropertyKey(prop.key) orelse return false;

                    if (i > 0) try writer.writeAll(", ");
                    try writeJsonString(writer, key);
                    try writer.writeAll(": ");
                    if (!try self.writeJsonLiteralNode(prop.value, writer)) return false;
                }
                try writer.writeByte('}');
                return true;
            },
            else => return false,
        }
    }

    fn getObjectPropertyKey(self: *const ContractBuilder, key_idx: NodeIndex) ?[]const u8 {
        const tag = self.ir_view.getTag(key_idx) orelse return null;
        return switch (tag) {
            .lit_string => self.getLiteralString(key_idx),
            .identifier => blk: {
                const binding = self.ir_view.getBinding(key_idx) orelse break :blk null;
                break :blk self.resolveAtomName(binding.slot);
            },
            else => null,
        };
    }

    fn getLiteralString(self: *const ContractBuilder, node_idx: NodeIndex) ?[]const u8 {
        const tag = self.ir_view.getTag(node_idx) orelse return null;
        if (tag != .lit_string) return null;
        const str_idx = self.ir_view.getStringIdx(node_idx) orelse return null;
        return self.ir_view.getString(str_idx);
    }

    fn upsertApiSchema(self: *ContractBuilder, name: []const u8, schema_json: []const u8) !void {
        for (self.api_schemas.items) |*schema| {
            if (std.mem.eql(u8, schema.name, name)) {
                self.allocator.free(schema.schema_json);
                schema.schema_json = schema_json;
                return;
            }
        }

        try self.api_schemas.append(self.allocator, .{
            .name = try self.allocator.dupe(u8, name),
            .schema_json = schema_json,
        });
    }

    fn extractApiRoutesFromCall(self: *ContractBuilder, call: Node.CallExpr) !void {
        if (call.args_count < 1) {
            self.api_routes_dynamic = true;
            return;
        }

        const routes_arg = self.ir_view.getListIndex(call.args_start, 0);
        const routes_obj = self.resolveObjectLiteralNode(routes_arg) orelse {
            self.api_routes_dynamic = true;
            return;
        };

        try self.extractApiRoutesFromObject(routes_obj);
    }

    fn extractServiceCall(self: *ContractBuilder, call: Node.CallExpr) !void {
        var service_call = ServiceCallInfo{
            .service = try self.allocator.dupe(u8, ""),
            .route_pattern = try self.allocator.dupe(u8, ""),
        };
        errdefer service_call.deinit(self.allocator);

        if (call.args_count < 2) {
            service_call.dynamic = true;
            try self.service_calls.append(self.allocator, service_call);
            return;
        }

        const service_idx = self.ir_view.getListIndex(call.args_start, 0);
        if (self.getLiteralString(service_idx)) |service_name| {
            self.allocator.free(service_call.service);
            service_call.service = try self.allocator.dupe(u8, service_name);
        } else {
            service_call.dynamic = true;
        }

        const route_idx = self.ir_view.getListIndex(call.args_start, 1);
        if (self.getLiteralString(route_idx)) |route_pattern| {
            self.allocator.free(service_call.route_pattern);
            service_call.route_pattern = try self.allocator.dupe(u8, route_pattern);
        } else {
            service_call.dynamic = true;
        }

        if (call.args_count > 2) {
            const init_idx = self.ir_view.getListIndex(call.args_start, 2);
            try self.extractServiceCallInit(init_idx, &service_call);
        }

        try self.service_calls.append(self.allocator, service_call);
    }

    fn extractServiceCallInit(self: *ContractBuilder, init_idx: NodeIndex, service_call: *ServiceCallInfo) !void {
        const tag = self.ir_view.getTag(init_idx) orelse {
            service_call.markAllDynamic();
            return;
        };
        if (tag == .lit_null or tag == .lit_undefined) return;

        const init_obj = self.resolveObjectLiteralNode(init_idx) orelse {
            service_call.markAllDynamic();
            return;
        };
        const obj = self.ir_view.getObject(init_obj) orelse return;

        var i: u16 = 0;
        while (i < obj.properties_count) : (i += 1) {
            const prop_idx = self.ir_view.getListIndex(obj.properties_start, i);
            const prop = self.ir_view.getProperty(prop_idx) orelse continue;
            const key = self.getObjectPropertyKey(prop.key) orelse continue;

            if (std.mem.eql(u8, key, "params")) {
                try self.extractServiceObjectKeys(prop.value, &service_call.path_params, &service_call.path_params_dynamic);
            } else if (std.mem.eql(u8, key, "query")) {
                try self.extractServiceObjectKeys(prop.value, &service_call.query_keys, &service_call.query_dynamic);
            } else if (std.mem.eql(u8, key, "headers")) {
                try self.extractServiceObjectKeys(prop.value, &service_call.header_keys, &service_call.header_dynamic);
            } else if (std.mem.eql(u8, key, "body")) {
                const body_tag = self.ir_view.getTag(prop.value) orelse {
                    service_call.has_body = true;
                    service_call.body_dynamic = true;
                    continue;
                };
                if (body_tag == .lit_null or body_tag == .lit_undefined) continue;
                service_call.has_body = true;
                if (body_tag != .lit_string) {
                    service_call.body_dynamic = true;
                }
            }
        }
    }

    fn extractServiceObjectKeys(
        self: *ContractBuilder,
        node_idx: NodeIndex,
        target: *std.ArrayList([]const u8),
        dynamic_flag: *bool,
    ) !void {
        const obj_idx = self.resolveObjectLiteralNode(node_idx) orelse {
            dynamic_flag.* = true;
            return;
        };
        const obj = self.ir_view.getObject(obj_idx) orelse {
            dynamic_flag.* = true;
            return;
        };

        var i: u16 = 0;
        while (i < obj.properties_count) : (i += 1) {
            const prop_idx = self.ir_view.getListIndex(obj.properties_start, i);
            const prop_tag = self.ir_view.getTag(prop_idx) orelse {
                dynamic_flag.* = true;
                continue;
            };
            if (prop_tag != .object_property) {
                dynamic_flag.* = true;
                continue;
            }

            const prop = self.ir_view.getProperty(prop_idx) orelse {
                dynamic_flag.* = true;
                continue;
            };
            const key = self.getObjectPropertyKey(prop.key) orelse {
                dynamic_flag.* = true;
                continue;
            };

            if (!containsString(target.items, key)) {
                try target.append(self.allocator, try self.allocator.dupe(u8, key));
            }
        }
    }

    fn resolveObjectLiteralNode(self: *const ContractBuilder, node_idx: NodeIndex) ?NodeIndex {
        const tag = self.ir_view.getTag(node_idx) orelse return null;
        switch (tag) {
            .object_literal => return node_idx,
            .identifier => {
                const binding = self.ir_view.getBinding(node_idx) orelse return null;
                return self.findObjectLiteralBinding(binding.slot);
            },
            else => return null,
        }
    }

    fn findObjectLiteralBinding(self: *const ContractBuilder, slot: u16) ?NodeIndex {
        const init_node = self.findBindingInitNode(slot) orelse return null;
        if (self.ir_view.getTag(init_node) == .object_literal) return init_node;
        return null;
    }

    fn findBindingInitNode(self: *const ContractBuilder, slot: u16) ?NodeIndex {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            if (tag != .var_decl) continue;

            const decl = self.ir_view.getVarDecl(idx) orelse continue;
            if (decl.binding.slot != slot or decl.init == null_node) continue;
            return decl.init;
        }
        return null;
    }

    fn findFunctionNodeByBinding(self: *const ContractBuilder, slot: u16) ?NodeIndex {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            switch (tag) {
                .function_decl => {
                    const decl = self.ir_view.getVarDecl(idx) orelse continue;
                    if (decl.binding.slot != slot or decl.init == null_node) continue;
                    const init_tag = self.ir_view.getTag(decl.init) orelse continue;
                    if (init_tag == .function_expr or init_tag == .arrow_function) return decl.init;
                },
                .var_decl => {
                    const decl = self.ir_view.getVarDecl(idx) orelse continue;
                    if (decl.binding.slot != slot or decl.init == null_node) continue;
                    const init_tag = self.ir_view.getTag(decl.init) orelse continue;
                    if (init_tag == .function_expr or init_tag == .arrow_function) return decl.init;
                },
                else => {},
            }
        }
        return null;
    }

    fn resolveFunctionNode(self: *const ContractBuilder, node_idx: NodeIndex) ?NodeIndex {
        const tag = self.ir_view.getTag(node_idx) orelse return null;
        switch (tag) {
            .function_decl => {
                const decl = self.ir_view.getVarDecl(node_idx) orelse return null;
                return if (decl.init != null_node) decl.init else null;
            },
            .function_expr, .arrow_function => return node_idx,
            .identifier => {
                const binding = self.ir_view.getBinding(node_idx) orelse return null;
                return self.findFunctionNodeByBinding(binding.slot);
            },
            else => return null,
        }
    }

    fn extractApiRoutesFromObject(self: *ContractBuilder, object_idx: NodeIndex) !void {
        const obj = self.ir_view.getObject(object_idx) orelse return;

        var i: u16 = 0;
        while (i < obj.properties_count) : (i += 1) {
            const prop_idx = self.ir_view.getListIndex(obj.properties_start, i);
            const prop_tag = self.ir_view.getTag(prop_idx) orelse continue;
            if (prop_tag != .object_property) continue;

            const prop = self.ir_view.getProperty(prop_idx) orelse continue;
            const route_key = self.getObjectPropertyKey(prop.key) orelse {
                self.api_routes_dynamic = true;
                continue;
            };
            const parsed = parseRouteKey(route_key) orelse {
                self.api_routes_dynamic = true;
                continue;
            };

            if (self.hasApiRoute(parsed.method, parsed.path)) continue;

            var route = ApiRouteInfo{
                .method = try self.allocator.dupe(u8, parsed.method),
                .path = try self.allocator.dupe(u8, parsed.path),
                .request_schema_refs = .empty,
                .request_schema_dynamic = false,
                .requires_bearer = false,
                .requires_jwt = false,
            };
            errdefer route.deinit(self.allocator);
            try self.appendPathParams(&route);

            if (self.resolveFunctionNode(prop.value)) |fn_node| {
                try self.populateApiRouteFacts(&route, fn_node);
            } else {
                self.api_routes_dynamic = true;
            }

            try self.api_routes.append(self.allocator, route);
        }
    }

    fn hasApiRoute(self: *const ContractBuilder, method: []const u8, path: []const u8) bool {
        for (self.api_routes.items) |route| {
            if (std.mem.eql(u8, route.method, method) and std.mem.eql(u8, route.path, path)) {
                return true;
            }
        }
        return false;
    }

    fn populateApiRouteFacts(self: *ContractBuilder, route: *ApiRouteInfo, fn_node: NodeIndex) !void {
        var analyzer = handler_analyzer.HandlerAnalyzer.init(self.allocator, self.ir_view, self.atoms);
        defer analyzer.deinit();

        if (try analyzer.analyzeDirectReturn(fn_node)) |response| {
            defer if (response.body.len > 0) self.allocator.free(response.body);
            route.response_status = response.status;
            route.response_content_type = try self.allocator.dupe(u8, contentTypeFor(response.content_type_idx));
        }

        const func = self.ir_view.getFunction(fn_node) orelse return;
        const request_binding_slot = self.findRequestBindingSlot(func);
        try self.scanFunctionNodeForApiFacts(func.body, request_binding_slot, route);
        try self.syncRouteRequestBodies(route);
        try self.syncLegacyRouteResponse(route);
    }

    fn findRequestBindingSlot(self: *const ContractBuilder, func: Node.FunctionExpr) ?u16 {
        if (func.params_count == 0) return null;
        const req_param = self.ir_view.getListIndex(func.params_start, 0);
        const req_tag = self.ir_view.getTag(req_param) orelse return null;
        if (req_tag == .identifier) {
            const binding = self.ir_view.getBinding(req_param) orelse return null;
            if (binding.kind == .local or binding.kind == .argument) return binding.slot;
            return null;
        }
        if (req_tag == .pattern_element) {
            const elem = self.ir_view.getPatternElem(req_param) orelse return null;
            if (elem.binding.kind == .local or elem.binding.kind == .argument) return elem.binding.slot;
        }
        return null;
    }

    fn scanFunctionNodeForApiFacts(
        self: *ContractBuilder,
        node_idx: NodeIndex,
        request_binding_slot: ?u16,
        route: *ApiRouteInfo,
    ) !void {
        if (node_idx == null_node) return;

        const tag = self.ir_view.getTag(node_idx) orelse return;
        switch (tag) {
            .program, .block => {
                const block = self.ir_view.getBlock(node_idx) orelse return;
                var i: u16 = 0;
                while (i < block.stmts_count) : (i += 1) {
                    try self.scanFunctionNodeForApiFacts(self.ir_view.getListIndex(block.stmts_start, i), request_binding_slot, route);
                }
            },
            .expr_stmt, .throw_stmt => {
                if (self.ir_view.getOptValue(node_idx)) |value_node| {
                    try self.scanFunctionNodeForApiFacts(value_node, request_binding_slot, route);
                }
            },
            .return_stmt => {
                if (self.ir_view.getOptValue(node_idx)) |value_node| {
                    try self.captureApiResponse(value_node, route);
                    try self.scanFunctionNodeForApiFacts(value_node, request_binding_slot, route);
                }
            },
            .var_decl => {
                const decl = self.ir_view.getVarDecl(node_idx) orelse return;
                if (decl.init != null_node) try self.scanFunctionNodeForApiFacts(decl.init, request_binding_slot, route);
            },
            .if_stmt => {
                const if_stmt = self.ir_view.getIfStmt(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(if_stmt.condition, request_binding_slot, route);
                try self.scanFunctionNodeForApiFacts(if_stmt.then_branch, request_binding_slot, route);
                try self.scanFunctionNodeForApiFacts(if_stmt.else_branch, request_binding_slot, route);
            },
            .switch_stmt => {
                const switch_stmt = self.ir_view.getSwitchStmt(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(switch_stmt.discriminant, request_binding_slot, route);
                var i: u8 = 0;
                while (i < switch_stmt.cases_count) : (i += 1) {
                    try self.scanFunctionNodeForApiFacts(self.ir_view.getListIndex(switch_stmt.cases_start, i), request_binding_slot, route);
                }
            },
            .case_clause => {
                const case_clause = self.ir_view.getCaseClause(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(case_clause.test_expr, request_binding_slot, route);
                var i: u16 = 0;
                while (i < case_clause.body_count) : (i += 1) {
                    try self.scanFunctionNodeForApiFacts(self.ir_view.getListIndex(case_clause.body_start, i), request_binding_slot, route);
                }
            },
            .for_of_stmt, .for_in_stmt => {
                const for_iter = self.ir_view.getForIter(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(for_iter.iterable, request_binding_slot, route);
                try self.scanFunctionNodeForApiFacts(for_iter.body, request_binding_slot, route);
            },
            .binary_op => {
                const binary = self.ir_view.getBinary(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(binary.left, request_binding_slot, route);
                try self.scanFunctionNodeForApiFacts(binary.right, request_binding_slot, route);
            },
            .unary_op => {
                const unary = self.ir_view.getUnary(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(unary.operand, request_binding_slot, route);
            },
            .ternary => {
                const ternary = self.ir_view.getTernary(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(ternary.condition, request_binding_slot, route);
                try self.scanFunctionNodeForApiFacts(ternary.then_branch, request_binding_slot, route);
                try self.scanFunctionNodeForApiFacts(ternary.else_branch, request_binding_slot, route);
            },
            .assignment => {
                const assignment = self.ir_view.getAssignment(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(assignment.target, request_binding_slot, route);
                try self.scanFunctionNodeForApiFacts(assignment.value, request_binding_slot, route);
            },
            .array_literal => {
                const array = self.ir_view.getArray(node_idx) orelse return;
                var i: u16 = 0;
                while (i < array.elements_count) : (i += 1) {
                    try self.scanFunctionNodeForApiFacts(self.ir_view.getListIndex(array.elements_start, i), request_binding_slot, route);
                }
            },
            .object_literal => {
                const obj = self.ir_view.getObject(node_idx) orelse return;
                var i: u16 = 0;
                while (i < obj.properties_count) : (i += 1) {
                    try self.scanFunctionNodeForApiFacts(self.ir_view.getListIndex(obj.properties_start, i), request_binding_slot, route);
                }
            },
            .object_property => {
                const prop = self.ir_view.getProperty(node_idx) orelse return;
                if (prop.is_computed) try self.scanFunctionNodeForApiFacts(prop.key, request_binding_slot, route);
                try self.scanFunctionNodeForApiFacts(prop.value, request_binding_slot, route);
            },
            .member_access, .computed_access, .optional_chain => {
                try self.captureApiRequestAccess(node_idx, request_binding_slot, route);
                const member = self.ir_view.getMember(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(member.object, request_binding_slot, route);
                try self.scanFunctionNodeForApiFacts(member.computed, request_binding_slot, route);
            },
            .call, .method_call, .optional_call => {
                const call = self.ir_view.getCall(node_idx) orelse return;
                try self.captureApiHeaderGetter(call, request_binding_slot, route);
                const callee_tag = self.ir_view.getTag(call.callee) orelse return;
                if (callee_tag == .identifier) {
                    const binding = self.ir_view.getBinding(call.callee) orelse return;
                    if (self.isBindingCategory(binding.slot, .request_schema)) {
                        const fn_name = self.resolveAtomName(binding.slot);
                        const is_decode_query = fn_name != null and std.mem.eql(u8, fn_name.?, "decodeQuery");

                        if (!is_decode_query) {
                            try self.extractLiteralArg(call, &route.request_schema_refs, &route.request_schema_dynamic, null);
                        }

                        if (call.args_count > 0) {
                            const schema_idx = self.ir_view.getListIndex(call.args_start, 0);
                            if (self.getLiteralString(schema_idx)) |schema_ref| {
                                try self.classifyRequestSchemaCall(route, fn_name, schema_ref);
                            } else if (is_decode_query) {
                                route.query_params_dynamic = true;
                            }
                        }
                    } else {
                        // Check for auth flags via generic bindings
                        for (self.generic_bindings.items) |gb| {
                            if (gb.slot == binding.slot) {
                                if (gb.flags.sets_bearer_auth) route.requires_bearer = true;
                                if (gb.flags.sets_jwt_auth) route.requires_jwt = true;
                                break;
                            }
                        }
                    }
                }
                try self.scanFunctionNodeForApiFacts(call.callee, request_binding_slot, route);
                var i: u8 = 0;
                while (i < call.args_count) : (i += 1) {
                    try self.scanFunctionNodeForApiFacts(self.ir_view.getListIndex(call.args_start, i), request_binding_slot, route);
                }
            },
            .match_expr => {
                const match_expr = self.ir_view.getMatchExpr(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(match_expr.discriminant, request_binding_slot, route);
                var i: u8 = 0;
                while (i < match_expr.arms_count) : (i += 1) {
                    try self.scanFunctionNodeForApiFacts(self.ir_view.getListIndex(match_expr.arms_start, i), request_binding_slot, route);
                }
            },
            .match_arm => {
                const arm = self.ir_view.getMatchArm(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(arm.body, request_binding_slot, route);
            },
            .break_stmt, .continue_stmt => {},
            .function_decl, .function_expr, .arrow_function => return,
            else => {},
        }
    }

    const ResponseSchemaCandidate = struct {
        status: ?u16 = null,
        content_type: ?[]const u8 = null, // static literal
        schema_ref: ?[]const u8 = null, // owned
        schema_json: ?[]u8 = null, // owned
        dynamic: bool = false,

        fn deinit(self: *ResponseSchemaCandidate, allocator: std.mem.Allocator) void {
            if (self.schema_ref) |schema_ref| allocator.free(schema_ref);
            if (self.schema_json) |schema_json| allocator.free(schema_json);
        }
    };

    fn appendPathParams(self: *ContractBuilder, route: *ApiRouteInfo) !void {
        var i: usize = 0;
        while (i < route.path.len) : (i += 1) {
            if (route.path[i] != ':') continue;

            const start = i + 1;
            var end = start;
            while (end < route.path.len and route.path[end] != '/') : (end += 1) {}
            if (end <= start) continue;

            const name = route.path[start..end];
            if (containsApiParam(route.path_params.items, name)) continue;

            try route.path_params.append(self.allocator, .{
                .name = try self.allocator.dupe(u8, name),
                .location = "path",
                .required = true,
                .schema_json = try self.allocator.dupe(u8, "{\"type\":\"string\"}"),
            });
            i = end;
        }
    }

    fn captureApiResponse(self: *ContractBuilder, node_idx: NodeIndex, route: *ApiRouteInfo) !void {
        var candidate = (try self.analyzeApiResponse(node_idx)) orelse return;
        defer candidate.deinit(self.allocator);
        try self.mergeResponseCandidate(route, &candidate);
    }

    fn analyzeApiResponse(self: *ContractBuilder, node_idx: NodeIndex) !?ResponseSchemaCandidate {
        const tag = self.ir_view.getTag(node_idx) orelse return null;
        if (tag != .call) return null;

        const call = self.ir_view.getCall(node_idx) orelse return null;
        const callee_tag = self.ir_view.getTag(call.callee) orelse return null;
        if (callee_tag != .member_access) return null;

        const member = self.ir_view.getMember(call.callee) orelse return null;
        const obj_tag = self.ir_view.getTag(member.object) orelse return null;
        if (obj_tag != .identifier) return null;

        const binding = self.ir_view.getBinding(member.object) orelse return null;
        if (binding.kind != .global and binding.kind != .undeclared_global) return null;
        if (binding.slot != @intFromEnum(object.Atom.Response)) return null;

        var candidate = ResponseSchemaCandidate{};
        errdefer candidate.deinit(self.allocator);

        switch (@as(object.Atom, @enumFromInt(member.property))) {
            .json => {
                candidate.status = 200;
                candidate.content_type = "application/json";
                if (call.args_count >= 2) {
                    const options_idx = self.ir_view.getListIndex(call.args_start, 1);
                    candidate.status = self.extractStatusFromOptionsNode(options_idx);
                }
                if (call.args_count == 0) {
                    candidate.schema_json = try self.allocator.dupe(u8, "{\"type\":\"object\"}");
                    return candidate;
                }

                const payload_idx = self.ir_view.getListIndex(call.args_start, 0);
                if (try self.extractResponseSchemaRef(payload_idx)) |schema_ref| {
                    candidate.schema_ref = schema_ref;
                    return candidate;
                }
                if (try self.extractResponseSchemaJson(payload_idx)) |schema_json| {
                    candidate.schema_json = schema_json;
                    return candidate;
                }

                candidate.dynamic = true;
                return candidate;
            },
            .text => {
                candidate.status = 200;
                candidate.content_type = "text/plain; charset=utf-8";
                if (call.args_count >= 2) {
                    const options_idx = self.ir_view.getListIndex(call.args_start, 1);
                    candidate.status = self.extractStatusFromOptionsNode(options_idx);
                }
                return candidate;
            },
            .html => {
                candidate.status = 200;
                candidate.content_type = "text/html; charset=utf-8";
                if (call.args_count >= 2) {
                    const options_idx = self.ir_view.getListIndex(call.args_start, 1);
                    candidate.status = self.extractStatusFromOptionsNode(options_idx);
                }
                return candidate;
            },
            .rawJson => {
                candidate.status = 200;
                candidate.content_type = "application/json";
                if (call.args_count >= 2) {
                    const options_idx = self.ir_view.getListIndex(call.args_start, 1);
                    candidate.status = self.extractStatusFromOptionsNode(options_idx);
                }
                return candidate;
            },
            else => return null,
        }
    }

    fn mergeResponseCandidate(
        self: *ContractBuilder,
        route: *ApiRouteInfo,
        candidate: *ResponseSchemaCandidate,
    ) !void {
        if (candidate.dynamic) route.responses_dynamic = true;
        try self.appendResponseVariant(route, candidate);
    }

    fn captureApiRequestAccess(
        self: *ContractBuilder,
        node_idx: NodeIndex,
        request_binding_slot: ?u16,
        route: *ApiRouteInfo,
    ) !void {
        const request_slot = request_binding_slot orelse return;
        const tag = self.ir_view.getTag(node_idx) orelse return;
        if (tag != .member_access and tag != .computed_access and tag != .optional_chain) return;

        const member = self.ir_view.getMember(node_idx) orelse return;
        const root_property = self.requestRootProperty(member.object, request_slot) orelse return;
        const leaf_name = self.memberAccessName(member) orelse {
            if (std.mem.eql(u8, root_property, "query")) {
                route.query_params_dynamic = true;
            } else if (std.mem.eql(u8, root_property, "headers")) {
                route.header_params_dynamic = true;
            }
            return;
        };

        if (std.mem.eql(u8, root_property, "query")) {
            try self.appendApiParam(&route.query_params, leaf_name, "query", false, false);
            return;
        }
        if (std.mem.eql(u8, root_property, "headers")) {
            try self.appendApiParam(&route.header_params, leaf_name, "header", false, true);
        }
    }

    fn captureApiHeaderGetter(
        self: *ContractBuilder,
        call: Node.CallExpr,
        request_binding_slot: ?u16,
        route: *ApiRouteInfo,
    ) !void {
        const request_slot = request_binding_slot orelse return;
        const callee_tag = self.ir_view.getTag(call.callee) orelse return;
        if (callee_tag != .member_access and callee_tag != .computed_access and callee_tag != .optional_chain) return;

        const member = self.ir_view.getMember(call.callee) orelse return;
        const method_name = self.memberAccessName(member) orelse return;
        if (!std.mem.eql(u8, method_name, "get")) return;

        const root_property = self.requestRootProperty(member.object, request_slot) orelse return;
        if (!std.mem.eql(u8, root_property, "headers")) return;

        if (call.args_count == 0) {
            route.header_params_dynamic = true;
            return;
        }

        const name_idx = self.ir_view.getListIndex(call.args_start, 0);
        const name = self.getLiteralString(name_idx) orelse {
            route.header_params_dynamic = true;
            return;
        };
        try self.appendApiParam(&route.header_params, name, "header", false, true);
    }

    fn requestRootProperty(
        self: *const ContractBuilder,
        node_idx: NodeIndex,
        request_binding_slot: u16,
    ) ?[]const u8 {
        const tag = self.ir_view.getTag(node_idx) orelse return null;
        if (tag != .member_access and tag != .computed_access and tag != .optional_chain) return null;
        const member = self.ir_view.getMember(node_idx) orelse return null;
        if (!self.isRequestIdentifier(member.object, request_binding_slot)) return null;
        return self.memberAccessName(member);
    }

    fn isRequestIdentifier(self: *const ContractBuilder, node_idx: NodeIndex, request_binding_slot: u16) bool {
        const tag = self.ir_view.getTag(node_idx) orelse return false;
        if (tag != .identifier) return false;
        const binding = self.ir_view.getBinding(node_idx) orelse return false;
        return binding.slot == request_binding_slot;
    }

    fn memberAccessName(self: *const ContractBuilder, member: Node.MemberExpr) ?[]const u8 {
        if (member.computed != null_node) return self.getLiteralString(member.computed);
        return self.resolveAtomName(member.property);
    }

    fn appendApiParam(
        self: *ContractBuilder,
        params: *std.ArrayList(ApiParamInfo),
        raw_name: []const u8,
        location: []const u8,
        required: bool,
        lowercase_name: bool,
    ) !void {
        const needle = if (lowercase_name)
            try lowerAsciiOwned(self.allocator, raw_name)
        else
            try self.allocator.dupe(u8, raw_name);
        defer self.allocator.free(needle);

        if (containsApiParam(params.items, needle)) return;

        try params.append(self.allocator, .{
            .name = try self.allocator.dupe(u8, needle),
            .location = location,
            .required = required,
            .schema_json = try self.allocator.dupe(u8, "{\"type\":\"string\"}"),
        });
    }

    fn syncRouteRequestBodies(self: *ContractBuilder, route: *ApiRouteInfo) !void {
        route.request_bodies_dynamic = route.request_bodies_dynamic or route.request_schema_dynamic;
        if (route.request_bodies.items.len > 0) return;

        for (route.request_schema_refs.items) |schema_ref| {
            if (containsRequestBodySchemaRef(route.request_bodies.items, schema_ref)) continue;
            try route.request_bodies.append(self.allocator, .{
                .content_type = try self.allocator.dupe(u8, "application/json"),
                .schema_ref = try self.allocator.dupe(u8, schema_ref),
            });
        }
    }

    fn classifyRequestSchemaCall(
        self: *ContractBuilder,
        route: *ApiRouteInfo,
        fn_name: ?[]const u8,
        schema_ref: []const u8,
    ) !void {
        const name = fn_name orelse return;
        if (std.mem.eql(u8, name, "validateJson") or
            std.mem.eql(u8, name, "coerceJson") or
            std.mem.eql(u8, name, "decodeJson"))
        {
            try self.appendRequestBodySchemaRef(route, "application/json", schema_ref);
        } else if (std.mem.eql(u8, name, "decodeForm")) {
            try self.appendRequestBodySchemaRef(route, "application/x-www-form-urlencoded", schema_ref);
        } else if (std.mem.eql(u8, name, "decodeQuery")) {
            try self.appendQueryParamsFromSchema(route, schema_ref);
        }
    }

    fn appendRequestBodySchemaRef(
        self: *ContractBuilder,
        route: *ApiRouteInfo,
        content_type: []const u8,
        schema_ref: []const u8,
    ) !void {
        for (route.request_bodies.items) |body| {
            const body_content_type = body.content_type orelse continue;
            const body_schema_ref = body.schema_ref orelse continue;
            if (std.mem.eql(u8, body_content_type, content_type) and std.mem.eql(u8, body_schema_ref, schema_ref)) {
                return;
            }
        }

        try route.request_bodies.append(self.allocator, .{
            .content_type = try self.allocator.dupe(u8, content_type),
            .schema_ref = try self.allocator.dupe(u8, schema_ref),
        });
    }

    fn appendQueryParamsFromSchema(self: *ContractBuilder, route: *ApiRouteInfo, schema_ref: []const u8) !void {
        const schema_json = for (self.api_schemas.items) |schema| {
            if (std.mem.eql(u8, schema.name, schema_ref)) break schema.schema_json;
        } else return;

        var parsed = std.json.parseFromSlice(std.json.Value, self.allocator, schema_json, .{}) catch return;
        defer parsed.deinit();
        if (parsed.value != .object) return;

        const props = parsed.value.object.get("properties") orelse return;
        if (props != .object) return;

        var required_names: std.ArrayList([]const u8) = .empty;
        defer required_names.deinit(self.allocator);
        if (parsed.value.object.get("required")) |required_val| {
            if (required_val == .array) {
                for (required_val.array.items) |item| {
                    if (item != .string) continue;
                    required_names.append(self.allocator, item.string) catch {};
                }
            }
        }

        var it = props.object.iterator();
        while (it.next()) |entry| {
            if (!schemaValueSupportsQueryParam(entry.value_ptr.*)) {
                route.query_params_dynamic = true;
                return;
            }

            if (containsApiParam(route.query_params.items, entry.key_ptr.*)) continue;
            const field_schema_json = serializeJsonValue(self.allocator, entry.value_ptr.*) catch {
                route.query_params_dynamic = true;
                return;
            };
            errdefer self.allocator.free(field_schema_json);

            try route.query_params.append(self.allocator, .{
                .name = try self.allocator.dupe(u8, entry.key_ptr.*),
                .location = "query",
                .required = containsString(required_names.items, entry.key_ptr.*),
                .schema_json = field_schema_json,
            });
        }
    }

    fn schemaValueSupportsQueryParam(value_json: std.json.Value) bool {
        if (value_json != .object) return false;
        const obj = value_json.object;

        if (obj.get("enum")) |enum_val| {
            if (enum_val != .array or enum_val.array.items.len == 0) return false;
            for (enum_val.array.items) |item| {
                switch (item) {
                    .string, .integer, .float, .bool => {},
                    else => return false,
                }
            }
            return true;
        }

        const type_val = obj.get("type") orelse return false;
        if (type_val != .string) return false;
        return std.mem.eql(u8, type_val.string, "string") or
            std.mem.eql(u8, type_val.string, "number") or
            std.mem.eql(u8, type_val.string, "integer") or
            std.mem.eql(u8, type_val.string, "boolean");
    }

    fn serializeJsonValue(allocator: std.mem.Allocator, value_json: std.json.Value) ![]u8 {
        var output: std.ArrayList(u8) = .empty;
        errdefer output.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);
        try writeJsonValue(value_json, &aw.writer);
        output = aw.toArrayList();
        return try output.toOwnedSlice(allocator);
    }

    fn writeJsonValue(value_json: std.json.Value, writer: anytype) !void {
        switch (value_json) {
            .null => try writer.writeAll("null"),
            .bool => |b| try writer.writeAll(if (b) "true" else "false"),
            .integer => |i| try writer.print("{d}", .{i}),
            .float => |f| try writer.print("{d}", .{f}),
            .string => |s| try writeJsonString(writer, s),
            .array => |arr| {
                try writer.writeByte('[');
                for (arr.items, 0..) |item, idx| {
                    if (idx > 0) try writer.writeAll(",");
                    try writeJsonValue(item, writer);
                }
                try writer.writeByte(']');
            },
            .object => |obj| {
                try writer.writeByte('{');
                var it = obj.iterator();
                var idx: usize = 0;
                while (it.next()) |entry| : (idx += 1) {
                    if (idx > 0) try writer.writeAll(",");
                    try writeJsonString(writer, entry.key_ptr.*);
                    try writer.writeAll(":");
                    try writeJsonValue(entry.value_ptr.*, writer);
                }
                try writer.writeByte('}');
            },
            else => return error.UnsupportedJsonValue,
        }
    }

    fn syncLegacyRouteResponse(self: *ContractBuilder, route: *ApiRouteInfo) !void {
        route.response_status = null;
        if (route.response_content_type) |content_type| self.allocator.free(content_type);
        route.response_content_type = null;
        if (route.response_schema_ref) |schema_ref| self.allocator.free(schema_ref);
        route.response_schema_ref = null;
        if (route.response_schema_json) |schema_json| self.allocator.free(schema_json);
        route.response_schema_json = null;
        route.response_schema_dynamic = route.responses_dynamic;

        if (route.responses.items.len != 1) {
            if (route.responses.items.len > 1) route.response_schema_dynamic = true;
            return;
        }

        const response = route.responses.items[0];
        if (response.dynamic) {
            route.response_schema_dynamic = true;
            return;
        }

        route.response_status = response.status;
        if (response.content_type) |content_type| {
            route.response_content_type = try self.allocator.dupe(u8, content_type);
        }
        if (response.schema_ref) |schema_ref| {
            route.response_schema_ref = try self.allocator.dupe(u8, schema_ref);
        }
        if (response.schema_json) |schema_json| {
            route.response_schema_json = try self.allocator.dupe(u8, schema_json);
        }
    }

    fn appendResponseVariant(
        self: *ContractBuilder,
        route: *ApiRouteInfo,
        candidate: *const ResponseSchemaCandidate,
    ) !void {
        for (route.responses.items) |existing| {
            if (responseVariantMatches(existing, candidate)) return;
        }

        try route.responses.append(self.allocator, .{
            .status = candidate.status,
            .content_type = if (candidate.content_type) |content_type|
                try self.allocator.dupe(u8, content_type)
            else
                null,
            .schema_ref = if (candidate.schema_ref) |schema_ref|
                try self.allocator.dupe(u8, schema_ref)
            else
                null,
            .schema_json = if (candidate.schema_json) |schema_json|
                try self.allocator.dupe(u8, schema_json)
            else
                null,
            .dynamic = candidate.dynamic,
        });
    }

    fn extractResponseSchemaRef(self: *ContractBuilder, node_idx: NodeIndex) !?[]u8 {
        const tag = self.ir_view.getTag(node_idx) orelse return null;
        switch (tag) {
            .member_access => {
                const member = self.ir_view.getMember(node_idx) orelse return null;
                const prop_name = self.resolveAtomName(member.property) orelse return null;
                if (!std.mem.eql(u8, prop_name, "value")) return null;

                const obj_tag = self.ir_view.getTag(member.object) orelse return null;
                if (obj_tag != .identifier) return null;
                const binding = self.ir_view.getBinding(member.object) orelse return null;
                return try self.lookupSchemaRefForBinding(binding.slot);
            },
            .identifier => {
                const binding = self.ir_view.getBinding(node_idx) orelse return null;
                const init_node = self.findBindingInitNode(binding.slot) orelse return null;
                return try self.extractResponseSchemaRef(init_node);
            },
            else => return null,
        }
    }

    fn lookupSchemaRefForBinding(self: *ContractBuilder, slot: u16) !?[]u8 {
        const init_node = self.findBindingInitNode(slot) orelse return null;
        const tag = self.ir_view.getTag(init_node) orelse return null;
        if (tag != .call) return null;

        const call = self.ir_view.getCall(init_node) orelse return null;
        const callee_tag = self.ir_view.getTag(call.callee) orelse return null;
        if (callee_tag != .identifier) return null;

        const binding = self.ir_view.getBinding(call.callee) orelse return null;
        if (!self.isBindingCategory(binding.slot, .request_schema)) return null;
        if (call.args_count == 0) return null;

        const schema_idx = self.ir_view.getListIndex(call.args_start, 0);
        const schema_name = self.getLiteralString(schema_idx) orelse return null;
        return try self.allocator.dupe(u8, schema_name);
    }

    fn extractResponseSchemaJson(self: *ContractBuilder, node_idx: NodeIndex) !?[]u8 {
        if (self.type_checker == null or self.type_env == null) return null;

        const inferred = self.type_checker.?.inferType(node_idx);
        if (inferred != type_pool_mod.null_type_idx) {
            if (try api_schema.schemaFromType(self.allocator, self.type_env.?, inferred)) |schema_json| {
                return schema_json;
            }
        }

        const tag = self.ir_view.getTag(node_idx) orelse return null;
        if (tag == .identifier) {
            const binding = self.ir_view.getBinding(node_idx) orelse return null;
            const init_node = self.findBindingInitNode(binding.slot) orelse return null;
            return try self.extractResponseSchemaJson(init_node);
        }

        return null;
    }

    fn extractStatusFromOptionsNode(self: *const ContractBuilder, node_idx: NodeIndex) ?u16 {
        const tag = self.ir_view.getTag(node_idx) orelse return null;
        if (tag != .object_literal) return null;

        const obj = self.ir_view.getObject(node_idx) orelse return null;
        var i: u16 = 0;
        while (i < obj.properties_count) : (i += 1) {
            const prop_idx = self.ir_view.getListIndex(obj.properties_start, i);
            const prop = self.ir_view.getProperty(prop_idx) orelse continue;
            const key = self.getObjectPropertyKey(prop.key) orelse continue;
            if (!std.mem.eql(u8, key, "status")) continue;

            const value_tag = self.ir_view.getTag(prop.value) orelse return null;
            switch (value_tag) {
                .lit_int => {
                    const value = self.ir_view.getIntValue(prop.value) orelse return null;
                    if (value < 0 or value > std.math.maxInt(u16)) return null;
                    return @intCast(value);
                },
                else => return null,
            }
        }
        return null;
    }

    // -----------------------------------------------------------------
    // Phase 3: Effect classification
    // -----------------------------------------------------------------

    /// Summarize the effect facts that handler property derivation depends on.
    fn computeEffectSummary(self: *const ContractBuilder) EffectSummary {
        var summary = EffectSummary{};

        for (self.functions_map.items) |entry| {
            const binding = builtin_modules.fromSpecifier(entry.module) orelse continue;
            const is_durable = std.mem.eql(u8, binding.specifier, "zigttp:durable");
            const is_cache = std.mem.eql(u8, binding.specifier, "zigttp:cache");

            for (entry.names.items) |func_name| {
                summary.has_any_call = true;

                for (binding.exports) |exp| {
                    if (std.mem.eql(u8, exp.name, func_name)) {
                        summary.includeCall(exp.effect, is_durable);
                        break;
                    }
                }

                if (is_cache and std.mem.eql(u8, func_name, "cacheGet")) {
                    summary.has_cache_read = true;
                }
            }
        }

        if (self.egress_hosts.items.len > 0 or self.egress_dynamic) summary.includeEgress();

        return summary;
    }

    /// Derive handler-level properties from the aggregate effect summary.
    fn computeProperties(self: *const ContractBuilder) HandlerProperties {
        const s = self.computeEffectSummary();

        const read_only = s.io != .write;
        const durable_only_writes = s.io == .write and self.durable_used and !s.has_bare_write;
        const deterministic = !self.has_nondeterministic_builtin;
        // Scope cleanup callbacks run exactly once on unwind; retrying the request
        // would re-run them, violating at-most-once guarantees for resource cleanup.
        const retry_safe = !self.scope_used and (read_only or durable_only_writes);

        return .{
            .pure = !s.has_any_call and !s.has_egress,
            .read_only = read_only,
            .stateless = read_only and !s.has_cache_read,
            .retry_safe = retry_safe,
            .deterministic = deterministic,
            .has_egress = s.has_egress,
            .idempotent = deterministic and retry_safe,
        };
    }

    /// Detect rate limiting: guard composition + cacheIncr usage.
    /// When a handler uses zigttp:compose and calls cacheIncr, the cache
    /// namespace is extracted as rate limiting metadata.
    /// Guard composition with cacheIncr indicates rate limiting: guards
    /// short-circuit before the handler, and cacheIncr atomically increments
    /// a counter - the standard rate-limit-and-reject flow.
    fn detectRateLimiting(self: *const ContractBuilder) ?RateLimitInfo {
        if (!containsString(self.modules_list.items, "zigttp:compose")) return null;

        var uses_cache_incr = false;
        for (self.functions_map.items) |entry| {
            if (std.mem.eql(u8, entry.module, "zigttp:cache") and containsString(entry.names.items, "cacheIncr")) {
                uses_cache_incr = true;
                break;
            }
        }
        if (!uses_cache_incr) return null;

        if (self.cache_namespaces.items.len > 0) {
            return .{
                .namespace = self.cache_namespaces.items[0],
                .dynamic = self.cache_dynamic,
            };
        }

        return .{
            .namespace = "",
            .dynamic = true,
        };
    }
};

pub const containsString = json_utils.containsString;

fn containsApiParam(items: []const ApiParamInfo, needle: []const u8) bool {
    for (items) |item| {
        if (std.mem.eql(u8, item.name, needle)) return true;
    }
    return false;
}

fn containsRequestBodySchemaRef(items: []const ApiBodyInfo, needle: []const u8) bool {
    for (items) |item| {
        if (item.schema_ref) |schema_ref| {
            if (std.mem.eql(u8, schema_ref, needle)) return true;
        }
    }
    return false;
}

fn responseVariantMatches(existing: ApiResponseInfo, candidate: *const ContractBuilder.ResponseSchemaCandidate) bool {
    return existing.status == candidate.status and
        eqlOptionalString(existing.content_type, candidate.content_type) and
        eqlOptionalString(existing.schema_ref, candidate.schema_ref) and
        eqlOptionalString(existing.schema_json, candidate.schema_json) and
        existing.dynamic == candidate.dynamic;
}

fn eqlOptionalString(a: ?[]const u8, b: ?[]const u8) bool {
    if (a == null and b == null) return true;
    if (a == null or b == null) return false;
    return std.mem.eql(u8, a.?, b.?);
}

fn lowerAsciiOwned(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var out = try allocator.alloc(u8, input.len);
    for (input, 0..) |c, i| {
        out[i] = std.ascii.toLower(c);
    }
    return out;
}

const ParsedRouteKey = struct {
    method: []const u8,
    path: []const u8,
};

fn parseRouteKey(raw: []const u8) ?ParsedRouteKey {
    const sep = std.mem.indexOfScalar(u8, raw, ' ') orelse return null;
    if (sep == 0 or sep + 1 >= raw.len) return null;

    const method = raw[0..sep];
    const path = raw[sep + 1 ..];
    if (path.len == 0 or path[0] != '/') return null;

    return .{
        .method = method,
        .path = path,
    };
}

// -------------------------------------------------------------------------
// Host extraction from URL strings
// -------------------------------------------------------------------------

/// Extract hostname from a URL string (e.g. "https://api.example.com/path" -> "api.example.com")
pub fn extractHost(url: []const u8) []const u8 {
    // Skip scheme
    var start: usize = 0;
    if (std.mem.indexOf(u8, url, "://")) |scheme_end| {
        start = scheme_end + 3;
    } else {
        return "";
    }

    // Find end of host (first / or : after scheme, or end of string)
    var end = start;
    while (end < url.len) : (end += 1) {
        if (url[end] == '/' or url[end] == ':') break;
    }

    if (end <= start) return "";
    return url[start..end];
}

fn contentTypeFor(idx: u8) []const u8 {
    return switch (idx) {
        0 => "application/json",
        1 => "text/plain; charset=utf-8",
        else => "text/html; charset=utf-8",
    };
}

// -------------------------------------------------------------------------
// JSON deserialization
// -------------------------------------------------------------------------

/// Parse a HandlerContract from a JSON byte string.
/// All strings in the returned contract are owned (duped via allocator).
/// The caller owns the returned contract and must call deinit().
pub fn parseFromJson(allocator: std.mem.Allocator, json_bytes: []const u8) !HandlerContract {
    var contract = HandlerContract{
        .handler = .{ .path = &.{}, .line = 0, .column = 0 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = .empty, .dynamic = false },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .service_calls = .empty,
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .sql = emptySqlInfo(),
        .durable = .{
            .used = false,
            .keys = .{ .literal = .empty, .dynamic = false },
            .steps = .empty,
        },
        .scope = .{
            .used = false,
            .names = .empty,
            .dynamic = false,
            .max_depth = 0,
        },
        .api = emptyApiInfo(),
        .verification = null,
        .aot = null,
    };
    errdefer contract.deinit(allocator);

    var parser = JsonParser.init(json_bytes);

    // Expect opening {
    parser.skipWhitespace();
    if (!parser.consume('{')) return error.InvalidJson;

    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;

        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;
        parser.skipWhitespace();

        if (std.mem.eql(u8, key, "handler")) {
            try parseHandlerLoc(&parser, allocator, &contract);
        } else if (std.mem.eql(u8, key, "version")) {
            contract.version = parser.readU32() orelse contract.version;
        } else if (std.mem.eql(u8, key, "routes")) {
            try parseRoutes(&parser, allocator, &contract);
        } else if (std.mem.eql(u8, key, "env")) {
            try parseDynamicSection(&parser, allocator, "literal", &contract.env.literal, &contract.env.dynamic);
        } else if (std.mem.eql(u8, key, "egress")) {
            try parseEgressSection(&parser, allocator, &contract);
        } else if (std.mem.eql(u8, key, "serviceCalls")) {
            try parseServiceCalls(&parser, allocator, &contract);
        } else if (std.mem.eql(u8, key, "cache")) {
            try parseDynamicSection(&parser, allocator, "namespaces", &contract.cache.namespaces, &contract.cache.dynamic);
        } else if (std.mem.eql(u8, key, "sql")) {
            try parseSqlSection(&parser, allocator, &contract);
        } else if (std.mem.eql(u8, key, "durable")) {
            try parseDurableSection(&parser, allocator, &contract);
        } else if (std.mem.eql(u8, key, "scope")) {
            try parseScopeSection(&parser, allocator, &contract);
        } else if (std.mem.eql(u8, key, "api")) {
            try parseApiSection(&parser, allocator, &contract);
        } else if (std.mem.eql(u8, key, "verification")) {
            try parseVerification(&parser, &contract);
        } else if (std.mem.eql(u8, key, "faultCoverage")) {
            contract.fault_coverage = try parseFaultCoverage(&parser);
        } else if (std.mem.eql(u8, key, "rateLimiting")) {
            parser.skipWhitespace();
            if (parser.readNull()) {
                contract.rate_limiting = null;
            } else {
                contract.rate_limiting = try parseRateLimiting(&parser);
            }
        } else if (std.mem.eql(u8, key, "properties")) {
            contract.properties = try parseProperties(&parser);
        } else if (std.mem.eql(u8, key, "sandbox")) {
            try parseSandbox(&parser, &contract);
        } else if (std.mem.eql(u8, key, "behaviors")) {
            try parseBehaviors(&parser, allocator, &contract);
        } else if (std.mem.eql(u8, key, "behaviorsExhaustive")) {
            contract.behaviors_exhaustive = parser.readBool() orelse false;
        } else {
            // Skip unknown fields
            parser.skipValue();
        }
    }

    return contract;
}

pub const JsonParser = struct {
    data: []const u8,
    pos: usize = 0,

    pub fn init(data: []const u8) JsonParser {
        return .{ .data = data };
    }

    pub fn peek(self: *JsonParser) u8 {
        if (self.pos >= self.data.len) return 0;
        return self.data[self.pos];
    }

    pub fn advance(self: *JsonParser) u8 {
        if (self.pos >= self.data.len) return 0;
        const c = self.data[self.pos];
        self.pos += 1;
        return c;
    }

    pub fn consume(self: *JsonParser, expected: u8) bool {
        self.skipWhitespace();
        if (self.pos < self.data.len and self.data[self.pos] == expected) {
            self.pos += 1;
            return true;
        }
        return false;
    }

    pub fn skipWhitespace(self: *JsonParser) void {
        while (self.pos < self.data.len) {
            switch (self.data[self.pos]) {
                ' ', '\t', '\n', '\r' => self.pos += 1,
                else => break,
            }
        }
    }

    /// Read a JSON string value (including quotes). Returns the unquoted content.
    /// The returned slice points into the source data (zero-copy for non-escaped strings).
    pub fn readString(self: *JsonParser) ?[]const u8 {
        self.skipWhitespace();
        if (self.pos >= self.data.len or self.data[self.pos] != '"') return null;
        self.pos += 1; // skip opening quote
        const start = self.pos;
        while (self.pos < self.data.len and self.data[self.pos] != '"') {
            if (self.data[self.pos] == '\\') {
                self.pos += 1; // skip escaped char
            }
            if (self.pos < self.data.len) self.pos += 1;
        }
        const end = self.pos;
        if (self.pos < self.data.len) self.pos += 1; // skip closing quote
        return self.data[start..end];
    }

    /// Read a JSON number as u32.
    pub fn readU32(self: *JsonParser) ?u32 {
        self.skipWhitespace();
        var val: u32 = 0;
        var found = false;
        while (self.pos < self.data.len and self.data[self.pos] >= '0' and self.data[self.pos] <= '9') {
            val = val * 10 + @as(u32, self.data[self.pos] - '0');
            self.pos += 1;
            found = true;
        }
        if (!found) return null;
        return val;
    }

    /// Read a JSON number as u16.
    fn readU16(self: *JsonParser) ?u16 {
        const val = self.readU32() orelse return null;
        if (val > std.math.maxInt(u16)) return null;
        return @intCast(val);
    }

    /// Read a JSON boolean.
    fn readBool(self: *JsonParser) ?bool {
        self.skipWhitespace();
        if (self.pos + 4 <= self.data.len and std.mem.eql(u8, self.data[self.pos..][0..4], "true")) {
            self.pos += 4;
            return true;
        }
        if (self.pos + 5 <= self.data.len and std.mem.eql(u8, self.data[self.pos..][0..5], "false")) {
            self.pos += 5;
            return false;
        }
        return null;
    }

    /// Skip "null" literal.
    fn readNull(self: *JsonParser) bool {
        self.skipWhitespace();
        if (self.pos + 4 <= self.data.len and std.mem.eql(u8, self.data[self.pos..][0..4], "null")) {
            self.pos += 4;
            return true;
        }
        return false;
    }

    /// Skip any JSON value (string, number, bool, null, object, array).
    pub fn skipValue(self: *JsonParser) void {
        self.skipWhitespace();
        if (self.pos >= self.data.len) return;
        switch (self.data[self.pos]) {
            '"' => _ = self.readString(),
            '{' => self.skipObject(),
            '[' => self.skipArray(),
            't', 'f' => _ = self.readBool(),
            'n' => _ = self.readNull(),
            else => {
                // number or unknown - skip digits/signs
                while (self.pos < self.data.len) {
                    switch (self.data[self.pos]) {
                        '0'...'9', '-', '.', 'e', 'E', '+' => self.pos += 1,
                        else => break,
                    }
                }
            },
        }
    }

    fn readRawValue(self: *JsonParser) ?[]const u8 {
        self.skipWhitespace();
        const start = self.pos;
        self.skipValue();
        const end = self.pos;
        if (end < start) return null;
        return self.data[start..end];
    }

    fn skipObject(self: *JsonParser) void {
        if (!self.consume('{')) return;
        while (self.pos < self.data.len and self.data[self.pos] != '}') {
            if (self.data[self.pos] == ',') {
                self.pos += 1;
                continue;
            }
            self.skipWhitespace();
            _ = self.readString(); // key
            self.skipWhitespace();
            _ = self.consume(':');
            self.skipValue(); // value (handles nested objects/arrays)
            self.skipWhitespace();
        }
        if (self.pos < self.data.len) self.pos += 1; // skip }
    }

    fn skipArray(self: *JsonParser) void {
        if (!self.consume('[')) return;
        self.skipWhitespace();
        while (self.pos < self.data.len and self.data[self.pos] != ']') {
            if (self.data[self.pos] == ',') {
                self.pos += 1;
                self.skipWhitespace();
                continue;
            }
            self.skipValue(); // handles nested objects/arrays
            self.skipWhitespace();
        }
        if (self.pos < self.data.len) self.pos += 1; // skip ]
    }
};

fn parseHandlerLoc(parser: *JsonParser, allocator: std.mem.Allocator, contract: *HandlerContract) !void {
    if (!parser.consume('{')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;

        if (std.mem.eql(u8, key, "path")) {
            const path = parser.readString() orelse return error.InvalidJson;
            contract.handler.path = try allocator.dupe(u8, path);
        } else if (std.mem.eql(u8, key, "line")) {
            contract.handler.line = parser.readU32() orelse 0;
        } else if (std.mem.eql(u8, key, "column")) {
            contract.handler.column = parser.readU32() orelse 0;
        } else {
            parser.skipValue();
        }
    }
}

fn parseRoutes(parser: *JsonParser, allocator: std.mem.Allocator, contract: *HandlerContract) !void {
    if (!parser.consume('[')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        if (!parser.consume('{')) return error.InvalidJson;

        var pattern: []const u8 = "";
        var route_type: []const u8 = "exact";
        var field: []const u8 = "path";
        var status: u16 = 200;
        var content_type: []const u8 = "application/json";
        var aot: bool = false;

        while (true) {
            parser.skipWhitespace();
            if (parser.peek() == '}') {
                _ = parser.advance();
                break;
            }
            if (parser.peek() == ',') _ = parser.advance();
            parser.skipWhitespace();

            const key = parser.readString() orelse return error.InvalidJson;
            parser.skipWhitespace();
            if (!parser.consume(':')) return error.InvalidJson;

            if (std.mem.eql(u8, key, "pattern")) {
                pattern = parser.readString() orelse "";
            } else if (std.mem.eql(u8, key, "type")) {
                route_type = parser.readString() orelse "exact";
            } else if (std.mem.eql(u8, key, "field")) {
                field = parser.readString() orelse "path";
            } else if (std.mem.eql(u8, key, "status")) {
                status = parser.readU16() orelse 200;
            } else if (std.mem.eql(u8, key, "contentType")) {
                content_type = parser.readString() orelse "application/json";
            } else if (std.mem.eql(u8, key, "aot")) {
                aot = parser.readBool() orelse false;
            } else {
                parser.skipValue();
            }
        }

        try contract.routes.append(allocator, .{
            .pattern = try allocator.dupe(u8, pattern),
            .route_type = toStaticRouteType(route_type),
            .field = toStaticField(field),
            .status = status,
            .content_type = toStaticContentType(content_type),
            .aot = aot,
        });
    }
}

/// Parse a JSON object with a string-array field and a "dynamic" boolean.
/// Used for env, egress, and cache sections which share the same structure.
fn parseDynamicSection(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    list_key: []const u8,
    list: *std.ArrayList([]const u8),
    dynamic: *bool,
) !void {
    if (!parser.consume('{')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;

        if (std.mem.eql(u8, key, list_key)) {
            try parseStringArray(parser, allocator, list);
        } else if (std.mem.eql(u8, key, "dynamic")) {
            dynamic.* = parser.readBool() orelse false;
        } else {
            parser.skipValue();
        }
    }
}

fn parseEgressSection(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    contract: *HandlerContract,
) !void {
    if (!parser.consume('{')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;

        if (std.mem.eql(u8, key, "hosts")) {
            try parseStringArray(parser, allocator, &contract.egress.hosts);
        } else if (std.mem.eql(u8, key, "urls")) {
            try parseStringArray(parser, allocator, &contract.egress.urls);
        } else if (std.mem.eql(u8, key, "dynamic")) {
            contract.egress.dynamic = parser.readBool() orelse false;
        } else {
            parser.skipValue();
        }
    }
}

fn parseServiceCalls(parser: *JsonParser, allocator: std.mem.Allocator, contract: *HandlerContract) !void {
    parser.skipWhitespace();
    if (!parser.consume('[')) return error.InvalidJson;

    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }

        if (!parser.consume('{')) return error.InvalidJson;
        var service_call = ServiceCallInfo{
            .service = try allocator.dupe(u8, ""),
            .route_pattern = try allocator.dupe(u8, ""),
        };
        errdefer service_call.deinit(allocator);

        while (true) {
            parser.skipWhitespace();
            if (parser.peek() == '}') {
                _ = parser.advance();
                break;
            }
            if (parser.peek() == ',') _ = parser.advance();
            parser.skipWhitespace();

            const key = parser.readString() orelse return error.InvalidJson;
            parser.skipWhitespace();
            if (!parser.consume(':')) return error.InvalidJson;

            if (std.mem.eql(u8, key, "service")) {
                allocator.free(service_call.service);
                service_call.service = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else if (std.mem.eql(u8, key, "route")) {
                allocator.free(service_call.route_pattern);
                service_call.route_pattern = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else if (std.mem.eql(u8, key, "dynamic")) {
                service_call.dynamic = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "pathParams")) {
                try parseStringArray(parser, allocator, &service_call.path_params);
            } else if (std.mem.eql(u8, key, "pathParamsDynamic")) {
                service_call.path_params_dynamic = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "queryKeys")) {
                try parseStringArray(parser, allocator, &service_call.query_keys);
            } else if (std.mem.eql(u8, key, "queryDynamic")) {
                service_call.query_dynamic = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "headerKeys")) {
                try parseStringArray(parser, allocator, &service_call.header_keys);
            } else if (std.mem.eql(u8, key, "headerDynamic")) {
                service_call.header_dynamic = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "hasBody")) {
                service_call.has_body = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "bodyDynamic")) {
                service_call.body_dynamic = parser.readBool() orelse false;
            } else {
                parser.skipValue();
            }
        }

        try contract.service_calls.append(allocator, service_call);
    }
}

fn parseSqlSection(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    contract: *HandlerContract,
) !void {
    if (!parser.consume('{')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;

        if (std.mem.eql(u8, key, "backend")) {
            _ = parser.readString() orelse "sqlite";
        } else if (std.mem.eql(u8, key, "queries")) {
            try parseSqlQueries(parser, allocator, &contract.sql.queries);
        } else if (std.mem.eql(u8, key, "dynamic")) {
            contract.sql.dynamic = parser.readBool() orelse false;
        } else {
            parser.skipValue();
        }
    }
}

fn parseSqlQueries(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    list: *std.ArrayList(SqlQueryInfo),
) !void {
    if (!parser.consume('[')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();
        if (!parser.consume('{')) return error.InvalidJson;

        var query = SqlQueryInfo{
            .name = try allocator.dupe(u8, ""),
            .statement = try allocator.dupe(u8, ""),
            .operation = "",
            .tables = .empty,
        };
        errdefer query.deinit(allocator);

        while (true) {
            parser.skipWhitespace();
            if (parser.peek() == '}') {
                _ = parser.advance();
                break;
            }
            if (parser.peek() == ',') _ = parser.advance();
            parser.skipWhitespace();

            const key = parser.readString() orelse return error.InvalidJson;
            parser.skipWhitespace();
            if (!parser.consume(':')) return error.InvalidJson;

            if (std.mem.eql(u8, key, "name")) {
                allocator.free(query.name);
                query.name = try allocator.dupe(u8, parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "operation")) {
                query.operation = parseOwnedStaticOperation(parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "tables")) {
                try parseStringArray(parser, allocator, &query.tables);
            } else {
                parser.skipValue();
            }
        }

        allocator.free(query.statement);
        query.statement = try allocator.dupe(u8, "");
        try list.append(allocator, query);
    }
}

fn parseOwnedStaticOperation(raw: []const u8) []const u8 {
    if (std.mem.eql(u8, raw, "select")) return "select";
    if (std.mem.eql(u8, raw, "insert")) return "insert";
    if (std.mem.eql(u8, raw, "update")) return "update";
    if (std.mem.eql(u8, raw, "delete")) return "delete";
    return "";
}

fn parseDurableSection(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    contract: *HandlerContract,
) !void {
    if (!parser.consume('{')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;

        if (std.mem.eql(u8, key, "used")) {
            contract.durable.used = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "keys")) {
            try parseDynamicSection(
                parser,
                allocator,
                "literal",
                &contract.durable.keys.literal,
                &contract.durable.keys.dynamic,
            );
        } else if (std.mem.eql(u8, key, "steps")) {
            try parseStringArray(parser, allocator, &contract.durable.steps);
        } else if (std.mem.eql(u8, key, "timers")) {
            contract.durable.timers = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "signals")) {
            try parseDynamicSection(
                parser,
                allocator,
                "literal",
                &contract.durable.signals.literal,
                &contract.durable.signals.dynamic,
            );
        } else if (std.mem.eql(u8, key, "producerKeys")) {
            try parseDynamicSection(
                parser,
                allocator,
                "literal",
                &contract.durable.producer_keys.literal,
                &contract.durable.producer_keys.dynamic,
            );
        } else if (std.mem.eql(u8, key, "workflow")) {
            try parseDurableWorkflow(parser, allocator, &contract.durable.workflow);
        } else {
            parser.skipValue();
        }
    }
}

fn parseScopeSection(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    contract: *HandlerContract,
) !void {
    if (!parser.consume('{')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;

        if (std.mem.eql(u8, key, "used")) {
            contract.scope.used = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "names")) {
            try parseStringArray(parser, allocator, &contract.scope.names);
        } else if (std.mem.eql(u8, key, "dynamic")) {
            contract.scope.dynamic = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "maxDepth")) {
            contract.scope.max_depth = parser.readU32() orelse 0;
        } else {
            parser.skipValue();
        }
    }
}

fn parseDurableWorkflow(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    workflow: *DurableWorkflow,
) !void {
    if (parser.readNull()) return;
    if (!parser.consume('{')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;

        if (std.mem.eql(u8, key, "workflowId")) {
            if (workflow.workflow_id) |existing| allocator.free(existing);
            workflow.workflow_id = if (parser.readNull()) null else try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
        } else if (std.mem.eql(u8, key, "proofLevel")) {
            workflow.proof_level = DurableWorkflowProofLevel.fromString(parser.readString() orelse "none");
        } else if (std.mem.eql(u8, key, "nodes")) {
            try parseDurableWorkflowNodes(parser, allocator, &workflow.nodes);
        } else if (std.mem.eql(u8, key, "edges")) {
            try parseDurableWorkflowEdges(parser, allocator, &workflow.edges);
        } else {
            parser.skipValue();
        }
    }
}

fn parseDurableWorkflowNodes(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    list: *std.ArrayList(DurableWorkflowNode),
) !void {
    if (!parser.consume('[')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();
        if (!parser.consume('{')) return error.InvalidJson;

        var node = DurableWorkflowNode{
            .id = try allocator.dupe(u8, ""),
            .kind = .branch,
            .label = try allocator.dupe(u8, ""),
        };
        errdefer node.deinit(allocator);

        while (true) {
            parser.skipWhitespace();
            if (parser.peek() == '}') {
                _ = parser.advance();
                break;
            }
            if (parser.peek() == ',') _ = parser.advance();
            parser.skipWhitespace();

            const key = parser.readString() orelse return error.InvalidJson;
            parser.skipWhitespace();
            if (!parser.consume(':')) return error.InvalidJson;

            if (std.mem.eql(u8, key, "id")) {
                allocator.free(node.id);
                node.id = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else if (std.mem.eql(u8, key, "kind")) {
                node.kind = DurableWorkflowNodeKind.fromString(parser.readString() orelse "branch");
            } else if (std.mem.eql(u8, key, "label")) {
                allocator.free(node.label);
                node.label = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else if (std.mem.eql(u8, key, "detail")) {
                if (node.detail) |detail| allocator.free(detail);
                node.detail = if (parser.readNull()) null else try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else if (std.mem.eql(u8, key, "status")) {
                node.status = if (parser.readNull()) null else parser.readU16() orelse return error.InvalidJson;
            } else {
                parser.skipValue();
            }
        }

        try list.append(allocator, node);
    }
}

fn parseDurableWorkflowEdges(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    list: *std.ArrayList(DurableWorkflowEdge),
) !void {
    if (!parser.consume('[')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();
        if (!parser.consume('{')) return error.InvalidJson;

        var edge = DurableWorkflowEdge{
            .from = try allocator.dupe(u8, ""),
            .to = try allocator.dupe(u8, ""),
        };
        errdefer edge.deinit(allocator);

        while (true) {
            parser.skipWhitespace();
            if (parser.peek() == '}') {
                _ = parser.advance();
                break;
            }
            if (parser.peek() == ',') _ = parser.advance();
            parser.skipWhitespace();

            const key = parser.readString() orelse return error.InvalidJson;
            parser.skipWhitespace();
            if (!parser.consume(':')) return error.InvalidJson;

            if (std.mem.eql(u8, key, "from")) {
                allocator.free(edge.from);
                edge.from = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else if (std.mem.eql(u8, key, "to")) {
                allocator.free(edge.to);
                edge.to = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else if (std.mem.eql(u8, key, "condition")) {
                if (edge.condition) |condition| allocator.free(condition);
                edge.condition = if (parser.readNull()) null else try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else {
                parser.skipValue();
            }
        }

        try list.append(allocator, edge);
    }
}

fn parseApiSection(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    contract: *HandlerContract,
) !void {
    if (!parser.consume('{')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;

        if (std.mem.eql(u8, key, "schemas")) {
            try parseApiSchemas(parser, allocator, &contract.api.schemas);
        } else if (std.mem.eql(u8, key, "requests")) {
            try parseApiRequests(parser, allocator, &contract.api.requests);
        } else if (std.mem.eql(u8, key, "auth")) {
            try parseApiAuth(parser, &contract.api.auth);
        } else if (std.mem.eql(u8, key, "routes")) {
            try parseApiRoutes(parser, allocator, &contract.api.routes);
        } else if (std.mem.eql(u8, key, "schemasDynamic")) {
            contract.api.schemas_dynamic = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "routesDynamic")) {
            contract.api.routes_dynamic = parser.readBool() orelse false;
        } else {
            parser.skipValue();
        }
    }
}

fn parseApiSchemas(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    list: *std.ArrayList(ApiSchemaInfo),
) !void {
    if (!parser.consume('[')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();
        if (!parser.consume('{')) return error.InvalidJson;

        var schema = ApiSchemaInfo{
            .name = try allocator.dupe(u8, ""),
            .schema_json = try allocator.dupe(u8, "{}"),
        };
        errdefer {
            allocator.free(schema.name);
            allocator.free(schema.schema_json);
        }

        while (true) {
            parser.skipWhitespace();
            if (parser.peek() == '}') {
                _ = parser.advance();
                break;
            }
            if (parser.peek() == ',') _ = parser.advance();
            parser.skipWhitespace();

            const key = parser.readString() orelse return error.InvalidJson;
            parser.skipWhitespace();
            if (!parser.consume(':')) return error.InvalidJson;

            if (std.mem.eql(u8, key, "name")) {
                allocator.free(schema.name);
                schema.name = try allocator.dupe(u8, parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "schema")) {
                const raw = parser.readRawValue() orelse return error.InvalidJson;
                allocator.free(schema.schema_json);
                schema.schema_json = try allocator.dupe(u8, raw);
            } else {
                parser.skipValue();
            }
        }

        try list.append(allocator, schema);
    }
}

fn parseApiRequests(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    requests: *ApiRequestInfo,
) !void {
    if (!parser.consume('{')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;

        if (std.mem.eql(u8, key, "schemaRefs")) {
            try parseStringArray(parser, allocator, &requests.schema_refs);
        } else if (std.mem.eql(u8, key, "dynamic")) {
            requests.dynamic = parser.readBool() orelse false;
        } else {
            parser.skipValue();
        }
    }
}

fn parseApiAuth(parser: *JsonParser, auth: *ApiAuthInfo) !void {
    if (!parser.consume('{')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;

        if (std.mem.eql(u8, key, "bearer")) {
            auth.bearer = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "jwt")) {
            auth.jwt = parser.readBool() orelse false;
        } else {
            parser.skipValue();
        }
    }
}

fn parseApiRoutes(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    list: *std.ArrayList(ApiRouteInfo),
) !void {
    if (!parser.consume('[')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();
        if (!parser.consume('{')) return error.InvalidJson;

        var route = ApiRouteInfo{
            .method = try allocator.dupe(u8, ""),
            .path = try allocator.dupe(u8, ""),
            .request_schema_refs = .empty,
            .request_schema_dynamic = false,
            .requires_bearer = false,
            .requires_jwt = false,
        };
        errdefer route.deinit(allocator);

        while (true) {
            parser.skipWhitespace();
            if (parser.peek() == '}') {
                _ = parser.advance();
                break;
            }
            if (parser.peek() == ',') _ = parser.advance();
            parser.skipWhitespace();

            const key = parser.readString() orelse return error.InvalidJson;
            parser.skipWhitespace();
            if (!parser.consume(':')) return error.InvalidJson;

            if (std.mem.eql(u8, key, "method")) {
                allocator.free(route.method);
                route.method = try allocator.dupe(u8, parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "path")) {
                allocator.free(route.path);
                route.path = try allocator.dupe(u8, parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "requestSchemaRefs")) {
                try parseStringArray(parser, allocator, &route.request_schema_refs);
            } else if (std.mem.eql(u8, key, "requestSchemaDynamic")) {
                route.request_schema_dynamic = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "requiresBearer")) {
                route.requires_bearer = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "requiresJwt")) {
                route.requires_jwt = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "pathParams")) {
                try parseApiParams(parser, allocator, &route.path_params);
            } else if (std.mem.eql(u8, key, "queryParams")) {
                try parseApiParams(parser, allocator, &route.query_params);
            } else if (std.mem.eql(u8, key, "headerParams")) {
                try parseApiParams(parser, allocator, &route.header_params);
            } else if (std.mem.eql(u8, key, "queryParamsDynamic")) {
                route.query_params_dynamic = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "headerParamsDynamic")) {
                route.header_params_dynamic = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "requestBodies")) {
                try parseApiBodies(parser, allocator, &route.request_bodies);
            } else if (std.mem.eql(u8, key, "requestBodiesDynamic")) {
                route.request_bodies_dynamic = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "responses")) {
                try parseApiResponses(parser, allocator, &route.responses);
            } else if (std.mem.eql(u8, key, "responsesDynamic")) {
                route.responses_dynamic = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "responseStatus")) {
                route.response_status = if (parser.readNull()) null else (parser.readU16() orelse null);
            } else if (std.mem.eql(u8, key, "responseContentType")) {
                route.response_content_type = if (parser.readNull()) null else try allocator.dupe(u8, parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "responseSchemaRef")) {
                route.response_schema_ref = if (parser.readNull()) null else try allocator.dupe(u8, parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "responseSchema")) {
                if (!parser.readNull()) {
                    const raw = parser.readRawValue() orelse return error.InvalidJson;
                    route.response_schema_json = try allocator.dupe(u8, raw);
                }
            } else if (std.mem.eql(u8, key, "responseSchemaDynamic")) {
                route.response_schema_dynamic = parser.readBool() orelse false;
            } else {
                parser.skipValue();
            }
        }

        try backfillApiRouteCollections(allocator, &route);
        try list.append(allocator, route);
    }
}

fn parseApiParams(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    list: *std.ArrayList(ApiParamInfo),
) !void {
    if (!parser.consume('[')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();
        if (!parser.consume('{')) return error.InvalidJson;

        var param = ApiParamInfo{
            .name = try allocator.dupe(u8, ""),
            .location = "path",
            .required = false,
            .schema_json = try allocator.dupe(u8, "{\"type\":\"string\"}"),
        };
        errdefer param.deinit(allocator);

        while (true) {
            parser.skipWhitespace();
            if (parser.peek() == '}') {
                _ = parser.advance();
                break;
            }
            if (parser.peek() == ',') _ = parser.advance();
            parser.skipWhitespace();

            const key = parser.readString() orelse return error.InvalidJson;
            parser.skipWhitespace();
            if (!parser.consume(':')) return error.InvalidJson;

            if (std.mem.eql(u8, key, "name")) {
                allocator.free(param.name);
                param.name = try allocator.dupe(u8, parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "location")) {
                const raw = parser.readString() orelse "path";
                param.location = if (std.mem.eql(u8, raw, "query"))
                    "query"
                else if (std.mem.eql(u8, raw, "header"))
                    "header"
                else
                    "path";
            } else if (std.mem.eql(u8, key, "required")) {
                param.required = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "schema")) {
                const raw = parser.readRawValue() orelse return error.InvalidJson;
                allocator.free(param.schema_json);
                param.schema_json = try allocator.dupe(u8, raw);
            } else {
                parser.skipValue();
            }
        }

        try list.append(allocator, param);
    }
}

fn parseApiBodies(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    list: *std.ArrayList(ApiBodyInfo),
) !void {
    if (!parser.consume('[')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();
        if (!parser.consume('{')) return error.InvalidJson;

        var body = ApiBodyInfo{};
        errdefer body.deinit(allocator);

        while (true) {
            parser.skipWhitespace();
            if (parser.peek() == '}') {
                _ = parser.advance();
                break;
            }
            if (parser.peek() == ',') _ = parser.advance();
            parser.skipWhitespace();

            const key = parser.readString() orelse return error.InvalidJson;
            parser.skipWhitespace();
            if (!parser.consume(':')) return error.InvalidJson;

            if (std.mem.eql(u8, key, "contentType")) {
                body.content_type = if (parser.readNull()) null else try allocator.dupe(u8, parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "schemaRef")) {
                body.schema_ref = if (parser.readNull()) null else try allocator.dupe(u8, parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "schema")) {
                if (!parser.readNull()) {
                    const raw = parser.readRawValue() orelse return error.InvalidJson;
                    body.schema_json = try allocator.dupe(u8, raw);
                }
            } else if (std.mem.eql(u8, key, "dynamic")) {
                body.dynamic = parser.readBool() orelse false;
            } else {
                parser.skipValue();
            }
        }

        try list.append(allocator, body);
    }
}

fn parseApiResponses(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    list: *std.ArrayList(ApiResponseInfo),
) !void {
    if (!parser.consume('[')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();
        if (!parser.consume('{')) return error.InvalidJson;

        var response = ApiResponseInfo{};
        errdefer response.deinit(allocator);

        while (true) {
            parser.skipWhitespace();
            if (parser.peek() == '}') {
                _ = parser.advance();
                break;
            }
            if (parser.peek() == ',') _ = parser.advance();
            parser.skipWhitespace();

            const key = parser.readString() orelse return error.InvalidJson;
            parser.skipWhitespace();
            if (!parser.consume(':')) return error.InvalidJson;

            if (std.mem.eql(u8, key, "status")) {
                response.status = if (parser.readNull()) null else (parser.readU16() orelse null);
            } else if (std.mem.eql(u8, key, "contentType")) {
                response.content_type = if (parser.readNull()) null else try allocator.dupe(u8, parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "schemaRef")) {
                response.schema_ref = if (parser.readNull()) null else try allocator.dupe(u8, parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "schema")) {
                if (!parser.readNull()) {
                    const raw = parser.readRawValue() orelse return error.InvalidJson;
                    response.schema_json = try allocator.dupe(u8, raw);
                }
            } else if (std.mem.eql(u8, key, "dynamic")) {
                response.dynamic = parser.readBool() orelse false;
            } else {
                parser.skipValue();
            }
        }

        try list.append(allocator, response);
    }
}

fn backfillApiRouteCollections(allocator: std.mem.Allocator, route: *ApiRouteInfo) !void {
    if (route.request_bodies.items.len == 0) {
        route.request_bodies_dynamic = route.request_bodies_dynamic or route.request_schema_dynamic;
        for (route.request_schema_refs.items) |schema_ref| {
            if (containsRequestBodySchemaRef(route.request_bodies.items, schema_ref)) continue;
            try route.request_bodies.append(allocator, .{
                .content_type = try allocator.dupe(u8, "application/json"),
                .schema_ref = try allocator.dupe(u8, schema_ref),
            });
        }
    }

    // Backfill from legacy scalar response fields when no collection entries exist.
    // These scalar fields are kept for backward compatibility with older contract.json consumers.
    if (route.responses.items.len == 0 and
        (route.response_status != null or route.response_content_type != null or route.response_schema_ref != null or route.response_schema_json != null or route.response_schema_dynamic))
    {
        try route.responses.append(allocator, .{
            .status = route.response_status,
            .content_type = try dupeOptionalString(allocator, route.response_content_type),
            .schema_ref = try dupeOptionalString(allocator, route.response_schema_ref),
            .schema_json = try dupeOptionalString(allocator, route.response_schema_json),
            .dynamic = route.response_schema_dynamic,
        });
        route.responses_dynamic = route.responses_dynamic or route.response_schema_dynamic;
    }
}

fn parseVerification(parser: *JsonParser, contract: *HandlerContract) !void {
    parser.skipWhitespace();
    if (parser.readNull()) {
        contract.verification = null;
        return;
    }

    if (!parser.consume('{')) return error.InvalidJson;
    var info = VerificationInfo{
        .exhaustive_returns = false,
        .results_safe = false,
        .unreachable_code = false,
        .bytecode_verified = false,
    };

    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;

        if (std.mem.eql(u8, key, "exhaustiveReturns")) {
            info.exhaustive_returns = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "resultsSafe")) {
            info.results_safe = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "unreachableCode")) {
            info.unreachable_code = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "bytecodeVerified")) {
            info.bytecode_verified = parser.readBool() orelse false;
        } else {
            parser.skipValue();
        }
    }

    contract.verification = info;
}

fn parseProperties(parser: *JsonParser) !?HandlerProperties {
    parser.skipWhitespace();
    if (parser.readNull()) return null;

    if (!parser.consume('{')) return error.InvalidJson;
    var props = HandlerProperties{
        .pure = false,
        .read_only = false,
        .stateless = false,
        .retry_safe = false,
        .deterministic = false,
        .has_egress = false,
    };

    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;

        if (std.mem.eql(u8, key, "pure")) {
            props.pure = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "readOnly")) {
            props.read_only = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "stateless")) {
            props.stateless = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "retrySafe")) {
            props.retry_safe = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "deterministic")) {
            props.deterministic = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "hasEgress")) {
            props.has_egress = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "noSecretLeakage")) {
            props.no_secret_leakage = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "noCredentialLeakage")) {
            props.no_credential_leakage = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "inputValidated")) {
            props.input_validated = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "piiContained")) {
            props.pii_contained = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "injectionSafe")) {
            props.injection_safe = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "idempotent")) {
            props.idempotent = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "stateIsolated")) {
            props.state_isolated = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "maxIoDepth")) {
            if (parser.readNull()) {
                props.max_io_depth = null;
            } else {
                props.max_io_depth = parser.readU32();
            }
        } else if (std.mem.eql(u8, key, "faultCovered")) {
            props.fault_covered = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "resultSafe")) {
            props.result_safe = parser.readBool() orelse false;
        } else if (std.mem.eql(u8, key, "optionalSafe")) {
            props.optional_safe = parser.readBool() orelse false;
        } else {
            parser.skipValue();
        }
    }

    return props;
}

fn parseSandbox(parser: *JsonParser, contract: *HandlerContract) !void {
    parser.skipWhitespace();
    if (parser.readNull()) return;
    if (!parser.consume('{')) return error.InvalidJson;

    var seen = [_]bool{false} ** module_binding.capability_count;
    var matrix: CapabilityMatrix = .{};
    var have_cap_hash = false;

    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;
        parser.skipWhitespace();

        if (std.mem.eql(u8, key, "capabilities")) {
            if (!parser.consume('[')) return error.InvalidJson;
            while (true) {
                parser.skipWhitespace();
                if (parser.peek() == ']') {
                    _ = parser.advance();
                    break;
                }
                if (parser.peek() == ',') _ = parser.advance();
                parser.skipWhitespace();
                const name = parser.readString() orelse return error.InvalidJson;
                const cap = std.meta.stringToEnum(module_binding.ModuleCapability, name) orelse continue;
                seen[@intFromEnum(cap)] = true;
            }
        } else if (std.mem.eql(u8, key, "capabilityHash")) {
            const hex = parser.readString() orelse return error.InvalidJson;
            if (hex.len == 64) {
                _ = std.fmt.hexToBytes(&matrix.hash, hex) catch return error.InvalidJson;
                have_cap_hash = true;
            }
        } else if (std.mem.eql(u8, key, "policyHash")) {
            const hex = parser.readString() orelse return error.InvalidJson;
            if (hex.len == 64) {
                _ = std.fmt.hexToBytes(&contract.policy_hash, hex) catch return error.InvalidJson;
            }
        } else if (std.mem.eql(u8, key, "artifactSha256")) {
            const hex = parser.readString() orelse return error.InvalidJson;
            if (hex.len == 64) {
                _ = std.fmt.hexToBytes(&contract.artifact_sha256, hex) catch return error.InvalidJson;
            }
        } else {
            parser.skipValue();
        }
    }

    for (std.enums.values(module_binding.ModuleCapability)) |c| {
        if (seen[@intFromEnum(c)]) {
            matrix.items[matrix.len] = c;
            matrix.len += 1;
        }
    }
    if (!have_cap_hash) {
        matrix.hash = module_binding.capabilityHash(matrix.slice());
    }
    contract.capabilities = matrix;
}

fn parseBehaviors(parser: *JsonParser, allocator: std.mem.Allocator, contract: *HandlerContract) !void {
    parser.skipWhitespace();
    if (parser.readNull()) return;
    if (!parser.consume('[')) return error.InvalidJson;

    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }

        if (!parser.consume('{')) return error.InvalidJson;

        var path = BehaviorPath{
            .route_method = &.{},
            .route_pattern = &.{},
            .conditions = .empty,
            .io_sequence = .empty,
            .response_status = 0,
            .io_depth = 0,
            .is_failure_path = false,
        };
        errdefer path.deinit(allocator);

        while (true) {
            parser.skipWhitespace();
            if (parser.peek() == '}') {
                _ = parser.advance();
                break;
            }
            if (parser.peek() == ',') _ = parser.advance();
            parser.skipWhitespace();

            const key = parser.readString() orelse return error.InvalidJson;
            parser.skipWhitespace();
            if (!parser.consume(':')) return error.InvalidJson;

            if (std.mem.eql(u8, key, "method")) {
                path.route_method = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else if (std.mem.eql(u8, key, "pattern")) {
                path.route_pattern = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else if (std.mem.eql(u8, key, "status")) {
                path.response_status = @intCast(parser.readU32() orelse 0);
            } else if (std.mem.eql(u8, key, "ioDepth")) {
                path.io_depth = parser.readU32() orelse 0;
            } else if (std.mem.eql(u8, key, "failurePath")) {
                path.is_failure_path = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "conditions")) {
                try parseBehaviorConditions(parser, allocator, &path.conditions);
            } else if (std.mem.eql(u8, key, "ioSequence")) {
                try parseBehaviorIoSequence(parser, allocator, &path.io_sequence);
            } else {
                parser.skipValue();
            }
        }

        try contract.behaviors.append(allocator, path);
    }
}

fn parseBehaviorConditions(parser: *JsonParser, allocator: std.mem.Allocator, conditions: *std.ArrayList(PathCondition)) !void {
    parser.skipWhitespace();
    if (!parser.consume('[')) return error.InvalidJson;

    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }

        if (!parser.consume('{')) return error.InvalidJson;

        var cond = PathCondition{ .kind = .io_ok };
        errdefer cond.deinit(allocator);

        while (true) {
            parser.skipWhitespace();
            if (parser.peek() == '}') {
                _ = parser.advance();
                break;
            }
            if (parser.peek() == ',') _ = parser.advance();
            parser.skipWhitespace();

            const key = parser.readString() orelse return error.InvalidJson;
            parser.skipWhitespace();
            if (!parser.consume(':')) return error.InvalidJson;

            if (std.mem.eql(u8, key, "kind")) {
                const kind_str = parser.readString() orelse return error.InvalidJson;
                cond.kind = std.meta.stringToEnum(PathCondition.Kind, kind_str) orelse .io_ok;
            } else if (std.mem.eql(u8, key, "module")) {
                cond.module = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else if (std.mem.eql(u8, key, "func")) {
                cond.func = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else if (std.mem.eql(u8, key, "value")) {
                cond.value = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else {
                parser.skipValue();
            }
        }

        try conditions.append(allocator, cond);
    }
}

fn parseBehaviorIoSequence(parser: *JsonParser, allocator: std.mem.Allocator, io_seq: *std.ArrayList(PathIoCall)) !void {
    parser.skipWhitespace();
    if (!parser.consume('[')) return error.InvalidJson;

    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }

        if (!parser.consume('{')) return error.InvalidJson;

        var io_call = PathIoCall{ .module = &.{}, .func = &.{} };
        errdefer io_call.deinit(allocator);

        while (true) {
            parser.skipWhitespace();
            if (parser.peek() == '}') {
                _ = parser.advance();
                break;
            }
            if (parser.peek() == ',') _ = parser.advance();
            parser.skipWhitespace();

            const key = parser.readString() orelse return error.InvalidJson;
            parser.skipWhitespace();
            if (!parser.consume(':')) return error.InvalidJson;

            if (std.mem.eql(u8, key, "module")) {
                io_call.module = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else if (std.mem.eql(u8, key, "func")) {
                io_call.func = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else if (std.mem.eql(u8, key, "args")) {
                // Optional arg signature for canonicalization. Older contracts
                // without this key leave arg_signature null.
                parser.skipWhitespace();
                if (parser.readNull()) {
                    io_call.arg_signature = null;
                } else {
                    io_call.arg_signature = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
                }
            } else {
                parser.skipValue();
            }
        }

        try io_seq.append(allocator, io_call);
    }
}

fn parseFaultCoverage(parser: *JsonParser) !?FaultCoverageInfo {
    parser.skipWhitespace();
    if (parser.readNull()) return null;

    if (!parser.consume('{')) return error.InvalidJson;
    var info = FaultCoverageInfo{
        .total_failable = 0,
        .covered = 0,
        .warnings = 0,
    };

    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;

        if (std.mem.eql(u8, key, "totalFailable")) {
            info.total_failable = parser.readU32() orelse 0;
        } else if (std.mem.eql(u8, key, "covered")) {
            info.covered = parser.readU32() orelse 0;
        } else if (std.mem.eql(u8, key, "warnings")) {
            info.warnings = parser.readU32() orelse 0;
        } else {
            parser.skipValue();
        }
    }

    return info;
}

fn parseRateLimiting(parser: *JsonParser) !RateLimitInfo {
    if (!parser.consume('{')) return error.InvalidJson;
    var info = RateLimitInfo{ .namespace = "", .dynamic = true };

    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;

        if (std.mem.eql(u8, key, "namespace")) {
            info.namespace = parser.readString() orelse "";
        } else if (std.mem.eql(u8, key, "dynamic")) {
            info.dynamic = parser.readBool() orelse true;
        } else {
            parser.skipValue();
        }
    }
    return info;
}

/// Map parsed route_type strings to static literals so they outlive the JSON source.
fn toStaticRouteType(s: []const u8) []const u8 {
    if (std.mem.eql(u8, s, "exact")) return "exact";
    if (std.mem.eql(u8, s, "prefix")) return "prefix";
    return "unknown";
}

fn toStaticField(s: []const u8) []const u8 {
    if (std.mem.eql(u8, s, "path")) return "path";
    if (std.mem.eql(u8, s, "url")) return "url";
    return "path";
}

fn toStaticContentType(s: []const u8) []const u8 {
    if (std.mem.eql(u8, s, "application/json")) return "application/json";
    if (std.mem.eql(u8, s, "text/plain; charset=utf-8")) return "text/plain; charset=utf-8";
    if (std.mem.eql(u8, s, "text/html; charset=utf-8")) return "text/html; charset=utf-8";
    return "application/json";
}

fn parseStringArray(parser: *JsonParser, allocator: std.mem.Allocator, list: *std.ArrayList([]const u8)) !void {
    if (!parser.consume('[')) return error.InvalidJson;
    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == ']') {
            _ = parser.advance();
            break;
        }
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const val = parser.readString() orelse return error.InvalidJson;
        try list.append(allocator, try allocator.dupe(u8, val));
    }
}

// -------------------------------------------------------------------------
// JSON serialization
// -------------------------------------------------------------------------

/// Write the contract as JSON to a writer.
pub fn writeContractJson(contract: *const HandlerContract, writer: anytype) !void {
    try writer.writeAll("{\n");

    // version
    try writer.print("  \"version\": {d},\n", .{contract.version});

    // handler
    try writer.writeAll("  \"handler\": {\n");
    try writer.writeAll("    \"path\": ");
    try writeJsonString(writer, contract.handler.path);
    try writer.writeAll(",\n");
    try writer.print("    \"line\": {d},\n", .{contract.handler.line});
    try writer.print("    \"column\": {d}\n", .{contract.handler.column});
    try writer.writeAll("  },\n");

    // routes
    try writer.writeAll("  \"routes\": [");
    for (contract.routes.items, 0..) |route, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("\n    {\n");
        try writer.writeAll("      \"pattern\": ");
        try writeJsonString(writer, route.pattern);
        try writer.writeAll(",\n");
        try writer.writeAll("      \"type\": ");
        try writeJsonString(writer, route.route_type);
        try writer.writeAll(",\n");
        try writer.writeAll("      \"field\": ");
        try writeJsonString(writer, route.field);
        try writer.writeAll(",\n");
        try writer.print("      \"status\": {d},\n", .{route.status});
        try writer.writeAll("      \"contentType\": ");
        try writeJsonString(writer, route.content_type);
        try writer.writeAll(",\n");
        try writer.print("      \"aot\": {s}\n", .{if (route.aot) "true" else "false"});
        try writer.writeAll("    }");
    }
    if (contract.routes.items.len > 0) {
        try writer.writeAll("\n  ");
    }
    try writer.writeAll("],\n");

    // modules
    try writer.writeAll("  \"modules\": [");
    for (contract.modules.items, 0..) |mod, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, mod);
    }
    try writer.writeAll("],\n");

    try writer.writeAll("  \"sandbox\": {\n");
    try writer.writeAll("    \"capabilities\": [");
    for (contract.capabilities.slice(), 0..) |cap, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, @tagName(cap));
    }
    try writer.writeAll("],\n");
    try writer.writeAll("    \"capabilityHash\": ");
    try json_utils.writeJsonHex(writer, contract.capabilities.hash);
    try writer.writeAll(",\n");
    try writer.writeAll("    \"policyHash\": ");
    try json_utils.writeJsonHex(writer, contract.policy_hash);
    try writer.writeAll(",\n");
    try writer.writeAll("    \"artifactSha256\": ");
    try json_utils.writeJsonHex(writer, contract.artifact_sha256);
    try writer.writeAll("\n");
    try writer.writeAll("  },\n");

    // functions
    try writer.writeAll("  \"functions\": {");
    for (contract.functions.items, 0..) |entry, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("\n    ");
        try writeJsonString(writer, entry.module);
        try writer.writeAll(": [");
        for (entry.names.items, 0..) |name, j| {
            if (j > 0) try writer.writeAll(", ");
            try writeJsonString(writer, name);
        }
        try writer.writeAll("]");
    }
    if (contract.functions.items.len > 0) {
        try writer.writeAll("\n  ");
    }
    try writer.writeAll("},\n");

    // env
    try writer.writeAll("  \"env\": {\n");
    try writer.writeAll("    \"literal\": [");
    for (contract.env.literal.items, 0..) |name, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, name);
    }
    try writer.writeAll("],\n");
    try writer.print("    \"dynamic\": {s}\n", .{if (contract.env.dynamic) "true" else "false"});
    try writer.writeAll("  },\n");

    // egress
    try writer.writeAll("  \"egress\": {\n");
    try writer.writeAll("    \"hosts\": [");
    for (contract.egress.hosts.items, 0..) |host, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, host);
    }
    try writer.writeAll("],\n");
    if (contract.egress.urls.items.len > 0) {
        try writer.writeAll("    \"urls\": [");
        for (contract.egress.urls.items, 0..) |url, i| {
            if (i > 0) try writer.writeAll(", ");
            try writeJsonString(writer, url);
        }
        try writer.writeAll("],\n");
    }
    try writer.print("    \"dynamic\": {s}\n", .{if (contract.egress.dynamic) "true" else "false"});
    try writer.writeAll("  },\n");

    // serviceCalls
    try writer.writeAll("  \"serviceCalls\": [");
    for (contract.service_calls.items, 0..) |service_call, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("\n    {\n");
        try writer.writeAll("      \"service\": ");
        try writeJsonString(writer, service_call.service);
        try writer.writeAll(",\n");
        try writer.writeAll("      \"route\": ");
        try writeJsonString(writer, service_call.route_pattern);
        try writer.writeAll(",\n");
        try writer.print("      \"dynamic\": {s},\n", .{if (service_call.dynamic) "true" else "false"});
        try writer.writeAll("      \"pathParams\": [");
        for (service_call.path_params.items, 0..) |name, j| {
            if (j > 0) try writer.writeAll(", ");
            try writeJsonString(writer, name);
        }
        try writer.writeAll("],\n");
        try writer.print("      \"pathParamsDynamic\": {s},\n", .{if (service_call.path_params_dynamic) "true" else "false"});
        try writer.writeAll("      \"queryKeys\": [");
        for (service_call.query_keys.items, 0..) |name, j| {
            if (j > 0) try writer.writeAll(", ");
            try writeJsonString(writer, name);
        }
        try writer.writeAll("],\n");
        try writer.print("      \"queryDynamic\": {s},\n", .{if (service_call.query_dynamic) "true" else "false"});
        try writer.writeAll("      \"headerKeys\": [");
        for (service_call.header_keys.items, 0..) |name, j| {
            if (j > 0) try writer.writeAll(", ");
            try writeJsonString(writer, name);
        }
        try writer.writeAll("],\n");
        try writer.print("      \"headerDynamic\": {s},\n", .{if (service_call.header_dynamic) "true" else "false"});
        try writer.print("      \"hasBody\": {s},\n", .{if (service_call.has_body) "true" else "false"});
        try writer.print("      \"bodyDynamic\": {s}\n", .{if (service_call.body_dynamic) "true" else "false"});
        try writer.writeAll("    }");
    }
    if (contract.service_calls.items.len > 0) {
        try writer.writeAll("\n  ");
    }
    try writer.writeAll("],\n");

    // cache
    try writer.writeAll("  \"cache\": {\n");
    try writer.writeAll("    \"namespaces\": [");
    for (contract.cache.namespaces.items, 0..) |ns, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, ns);
    }
    try writer.writeAll("],\n");
    try writer.print("    \"dynamic\": {s}\n", .{if (contract.cache.dynamic) "true" else "false"});
    try writer.writeAll("  },\n");

    // sql
    try writer.writeAll("  \"sql\": {\n");
    try writer.writeAll("    \"backend\": \"sqlite\",\n");
    try writer.writeAll("    \"queries\": [");
    for (contract.sql.queries.items, 0..) |query, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("\n      {\n");
        try writer.writeAll("        \"name\": ");
        try writeJsonString(writer, query.name);
        try writer.writeAll(",\n");
        try writer.writeAll("        \"operation\": ");
        try writeJsonString(writer, query.operation);
        try writer.writeAll(",\n");
        try writer.writeAll("        \"tables\": [");
        for (query.tables.items, 0..) |table, j| {
            if (j > 0) try writer.writeAll(", ");
            try writeJsonString(writer, table);
        }
        try writer.writeAll("]\n");
        try writer.writeAll("      }");
    }
    if (contract.sql.queries.items.len > 0) {
        try writer.writeAll("\n    ");
    }
    try writer.writeAll("],\n");
    try writer.print("    \"dynamic\": {s}\n", .{if (contract.sql.dynamic) "true" else "false"});
    try writer.writeAll("  },\n");

    // durable
    try writer.writeAll("  \"durable\": {\n");
    try writer.print("    \"used\": {s},\n", .{if (contract.durable.used) "true" else "false"});
    try writer.writeAll("    \"keys\": {\n");
    try writer.writeAll("      \"literal\": [");
    for (contract.durable.keys.literal.items, 0..) |key, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, key);
    }
    try writer.writeAll("],\n");
    try writer.print("      \"dynamic\": {s}\n", .{if (contract.durable.keys.dynamic) "true" else "false"});
    try writer.writeAll("    },\n");
    try writer.writeAll("    \"steps\": [");
    for (contract.durable.steps.items, 0..) |step, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, step);
    }
    try writer.writeAll("],\n");
    try writer.print("    \"timers\": {s},\n", .{if (contract.durable.timers) "true" else "false"});
    try writer.writeAll("    \"signals\": {\n");
    try writer.writeAll("      \"literal\": [");
    for (contract.durable.signals.literal.items, 0..) |signal, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, signal);
    }
    try writer.writeAll("],\n");
    try writer.print("      \"dynamic\": {s}\n", .{if (contract.durable.signals.dynamic) "true" else "false"});
    try writer.writeAll("    },\n");
    try writer.writeAll("    \"producerKeys\": {\n");
    try writer.writeAll("      \"literal\": [");
    for (contract.durable.producer_keys.literal.items, 0..) |key, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, key);
    }
    try writer.writeAll("],\n");
    try writer.print("      \"dynamic\": {s}\n", .{if (contract.durable.producer_keys.dynamic) "true" else "false"});
    try writer.writeAll("    },\n");
    try writer.writeAll("    \"workflow\": {\n");
    try writer.writeAll("      \"workflowId\": ");
    if (contract.durable.workflow.workflow_id) |workflow_id| {
        try writeJsonString(writer, workflow_id);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(",\n");
    try writer.writeAll("      \"proofLevel\": ");
    try writeJsonString(writer, contract.durable.workflow.proof_level.toString());
    try writer.writeAll(",\n");
    try writer.writeAll("      \"nodes\": [");
    for (contract.durable.workflow.nodes.items, 0..) |node, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("\n        {\n");
        try writer.writeAll("          \"id\": ");
        try writeJsonString(writer, node.id);
        try writer.writeAll(",\n");
        try writer.writeAll("          \"kind\": ");
        try writeJsonString(writer, node.kind.toString());
        try writer.writeAll(",\n");
        try writer.writeAll("          \"label\": ");
        try writeJsonString(writer, node.label);
        try writer.writeAll(",\n");
        try writer.writeAll("          \"detail\": ");
        if (node.detail) |detail| {
            try writeJsonString(writer, detail);
        } else {
            try writer.writeAll("null");
        }
        try writer.writeAll(",\n");
        try writer.writeAll("          \"status\": ");
        if (node.status) |status| {
            try writer.print("{d}", .{status});
        } else {
            try writer.writeAll("null");
        }
        try writer.writeAll("\n        }");
    }
    if (contract.durable.workflow.nodes.items.len > 0) {
        try writer.writeAll("\n      ");
    }
    try writer.writeAll("],\n");
    try writer.writeAll("      \"edges\": [");
    for (contract.durable.workflow.edges.items, 0..) |edge, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("\n        {\n");
        try writer.writeAll("          \"from\": ");
        try writeJsonString(writer, edge.from);
        try writer.writeAll(",\n");
        try writer.writeAll("          \"to\": ");
        try writeJsonString(writer, edge.to);
        try writer.writeAll(",\n");
        try writer.writeAll("          \"condition\": ");
        if (edge.condition) |condition| {
            try writeJsonString(writer, condition);
        } else {
            try writer.writeAll("null");
        }
        try writer.writeAll("\n        }");
    }
    if (contract.durable.workflow.edges.items.len > 0) {
        try writer.writeAll("\n      ");
    }
    try writer.writeAll("]\n");
    try writer.writeAll("    }\n");
    try writer.writeAll("  },\n");

    // scope
    try writer.writeAll("  \"scope\": {\n");
    try writer.print("    \"used\": {s},\n", .{if (contract.scope.used) "true" else "false"});
    try writer.writeAll("    \"names\": [");
    for (contract.scope.names.items, 0..) |name, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, name);
    }
    try writer.writeAll("],\n");
    try writer.print("    \"dynamic\": {s},\n", .{if (contract.scope.dynamic) "true" else "false"});
    try writer.print("    \"maxDepth\": {d}\n", .{contract.scope.max_depth});
    try writer.writeAll("  },\n");

    // api
    try writer.writeAll("  \"api\": {\n");

    try writer.writeAll("    \"schemas\": [");
    for (contract.api.schemas.items, 0..) |schema, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("\n      {\n");
        try writer.writeAll("        \"name\": ");
        try writeJsonString(writer, schema.name);
        try writer.writeAll(",\n");
        try writer.writeAll("        \"schema\": ");
        try writer.writeAll(schema.schema_json);
        try writer.writeAll("\n      }");
    }
    if (contract.api.schemas.items.len > 0) {
        try writer.writeAll("\n    ");
    }
    try writer.writeAll("],\n");

    try writer.writeAll("    \"requests\": {\n");
    try writer.writeAll("      \"schemaRefs\": [");
    for (contract.api.requests.schema_refs.items, 0..) |schema_ref, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, schema_ref);
    }
    try writer.writeAll("],\n");
    try writer.print("      \"dynamic\": {s}\n", .{if (contract.api.requests.dynamic) "true" else "false"});
    try writer.writeAll("    },\n");

    try writer.writeAll("    \"auth\": {\n");
    try writer.print("      \"bearer\": {s},\n", .{if (contract.api.auth.bearer) "true" else "false"});
    try writer.print("      \"jwt\": {s}\n", .{if (contract.api.auth.jwt) "true" else "false"});
    try writer.writeAll("    },\n");

    try writer.writeAll("    \"routes\": [");
    for (contract.api.routes.items, 0..) |route, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("\n      {\n");
        try writer.writeAll("        \"method\": ");
        try writeJsonString(writer, route.method);
        try writer.writeAll(",\n");
        try writer.writeAll("        \"path\": ");
        try writeJsonString(writer, route.path);
        try writer.writeAll(",\n");
        try writer.writeAll("        \"requestSchemaRefs\": [");
        for (route.request_schema_refs.items, 0..) |schema_ref, j| {
            if (j > 0) try writer.writeAll(", ");
            try writeJsonString(writer, schema_ref);
        }
        try writer.writeAll("],\n");
        try writer.print("        \"requestSchemaDynamic\": {s},\n", .{if (route.request_schema_dynamic) "true" else "false"});
        try writer.print("        \"requiresBearer\": {s},\n", .{if (route.requires_bearer) "true" else "false"});
        try writer.print("        \"requiresJwt\": {s},\n", .{if (route.requires_jwt) "true" else "false"});
        try writer.writeAll("        \"pathParams\": [");
        for (route.path_params.items, 0..) |param, j| {
            if (j > 0) try writer.writeAll(",");
            try writeApiParamJson(writer, &param);
        }
        if (route.path_params.items.len > 0) {
            try writer.writeAll("\n        ");
        }
        try writer.writeAll("],\n");
        try writer.writeAll("        \"queryParams\": [");
        for (route.query_params.items, 0..) |param, j| {
            if (j > 0) try writer.writeAll(",");
            try writeApiParamJson(writer, &param);
        }
        if (route.query_params.items.len > 0) {
            try writer.writeAll("\n        ");
        }
        try writer.writeAll("],\n");
        try writer.writeAll("        \"headerParams\": [");
        for (route.header_params.items, 0..) |param, j| {
            if (j > 0) try writer.writeAll(",");
            try writeApiParamJson(writer, &param);
        }
        if (route.header_params.items.len > 0) {
            try writer.writeAll("\n        ");
        }
        try writer.writeAll("],\n");
        try writer.print("        \"queryParamsDynamic\": {s},\n", .{if (route.query_params_dynamic) "true" else "false"});
        try writer.print("        \"headerParamsDynamic\": {s},\n", .{if (route.header_params_dynamic) "true" else "false"});
        try writer.writeAll("        \"requestBodies\": [");
        for (route.request_bodies.items, 0..) |body, j| {
            if (j > 0) try writer.writeAll(",");
            try writeApiBodyJson(writer, &body);
        }
        if (route.request_bodies.items.len > 0) {
            try writer.writeAll("\n        ");
        }
        try writer.writeAll("],\n");
        try writer.print("        \"requestBodiesDynamic\": {s},\n", .{if (route.request_bodies_dynamic) "true" else "false"});
        try writer.writeAll("        \"responses\": [");
        for (route.responses.items, 0..) |response, j| {
            if (j > 0) try writer.writeAll(",");
            try writeApiResponseJson(writer, &response);
        }
        if (route.responses.items.len > 0) {
            try writer.writeAll("\n        ");
        }
        try writer.writeAll("],\n");
        try writer.print("        \"responsesDynamic\": {s},\n", .{if (route.responses_dynamic) "true" else "false"});
        try writer.writeAll("        \"responseStatus\": ");
        if (route.response_status) |status| {
            try writer.print("{d}", .{status});
        } else {
            try writer.writeAll("null");
        }
        try writer.writeAll(",\n");
        try writer.writeAll("        \"responseContentType\": ");
        if (route.response_content_type) |content_type| {
            try writeJsonString(writer, content_type);
        } else {
            try writer.writeAll("null");
        }
        try writer.writeAll(",\n");
        try writer.writeAll("        \"responseSchemaRef\": ");
        if (route.response_schema_ref) |schema_ref| {
            try writeJsonString(writer, schema_ref);
        } else {
            try writer.writeAll("null");
        }
        try writer.writeAll(",\n");
        try writer.writeAll("        \"responseSchema\": ");
        if (route.response_schema_json) |schema_json| {
            try writer.writeAll(schema_json);
        } else {
            try writer.writeAll("null");
        }
        try writer.writeAll(",\n");
        try writer.print("        \"responseSchemaDynamic\": {s}\n", .{if (route.response_schema_dynamic) "true" else "false"});
        try writer.writeAll("      }");
    }
    if (contract.api.routes.items.len > 0) {
        try writer.writeAll("\n    ");
    }
    try writer.writeAll("],\n");
    try writer.print("    \"schemasDynamic\": {s},\n", .{if (contract.api.schemas_dynamic) "true" else "false"});
    try writer.print("    \"routesDynamic\": {s}\n", .{if (contract.api.routes_dynamic) "true" else "false"});
    try writer.writeAll("  },\n");

    // verification (optional)
    if (contract.verification) |v| {
        try writer.writeAll("  \"verification\": {\n");
        try writer.print("    \"exhaustiveReturns\": {s},\n", .{if (v.exhaustive_returns) "true" else "false"});
        try writer.print("    \"resultsSafe\": {s},\n", .{if (v.results_safe) "true" else "false"});
        try writer.print("    \"unreachableCode\": {s},\n", .{if (v.unreachable_code) "true" else "false"});
        try writer.print("    \"bytecodeVerified\": {s}\n", .{if (v.bytecode_verified) "true" else "false"});
        try writer.writeAll("  },\n");
    } else {
        try writer.writeAll("  \"verification\": null,\n");
    }

    // websocket: always present for shape stability — handlers without
    // any ws event exports still emit the section with all flags false.
    try writer.writeAll("  \"websocket\": {");
    try writer.print("\"onOpen\": {s}, ", .{if (contract.websocket.on_open) "true" else "false"});
    try writer.print("\"onMessage\": {s}, ", .{if (contract.websocket.on_message) "true" else "false"});
    try writer.print("\"onClose\": {s}, ", .{if (contract.websocket.on_close) "true" else "false"});
    try writer.print("\"onError\": {s}", .{if (contract.websocket.on_error) "true" else "false"});
    try writer.writeAll("},\n");

    // aot (optional)
    if (contract.aot) |a| {
        try writer.writeAll("  \"aot\": {\n");
        try writer.print("    \"patternCount\": {d},\n", .{a.pattern_count});
        try writer.print("    \"hasDefault\": {s}\n", .{if (a.has_default) "true" else "false"});
        try writer.writeAll("  },\n");
    } else {
        try writer.writeAll("  \"aot\": null,\n");
    }

    // faultCoverage (optional)
    if (contract.fault_coverage) |fc| {
        try writer.writeAll("  \"faultCoverage\": {\n");
        try writer.print("    \"totalFailable\": {d},\n", .{fc.total_failable});
        try writer.print("    \"covered\": {d},\n", .{fc.covered});
        try writer.print("    \"warnings\": {d},\n", .{fc.warnings});
        try writer.print("    \"isCovered\": {s}\n", .{if (fc.isCovered()) "true" else "false"});
        try writer.writeAll("  },\n");
    } else {
        try writer.writeAll("  \"faultCoverage\": null,\n");
    }

    // rateLimiting (optional)
    if (contract.rate_limiting) |rl| {
        try writer.writeAll("  \"rateLimiting\": {\n");
        try writer.writeAll("    \"namespace\": ");
        try writeJsonString(writer, rl.namespace);
        try writer.print(",\n    \"dynamic\": {s}\n", .{if (rl.dynamic) "true" else "false"});
        try writer.writeAll("  },\n");
    } else {
        try writer.writeAll("  \"rateLimiting\": null,\n");
    }

    // properties (optional)
    if (contract.properties) |p| {
        try writer.writeAll("  \"properties\": {\n");
        try writer.print("    \"pure\": {s},\n", .{if (p.pure) "true" else "false"});
        try writer.print("    \"readOnly\": {s},\n", .{if (p.read_only) "true" else "false"});
        try writer.print("    \"stateless\": {s},\n", .{if (p.stateless) "true" else "false"});
        try writer.print("    \"retrySafe\": {s},\n", .{if (p.retry_safe) "true" else "false"});
        try writer.print("    \"deterministic\": {s},\n", .{if (p.deterministic) "true" else "false"});
        try writer.print("    \"hasEgress\": {s},\n", .{if (p.has_egress) "true" else "false"});
        try writer.print("    \"noSecretLeakage\": {s},\n", .{if (p.no_secret_leakage) "true" else "false"});
        try writer.print("    \"noCredentialLeakage\": {s},\n", .{if (p.no_credential_leakage) "true" else "false"});
        try writer.print("    \"inputValidated\": {s},\n", .{if (p.input_validated) "true" else "false"});
        try writer.print("    \"piiContained\": {s},\n", .{if (p.pii_contained) "true" else "false"});
        try writer.print("    \"injectionSafe\": {s},\n", .{if (p.injection_safe) "true" else "false"});
        try writer.print("    \"idempotent\": {s},\n", .{if (p.idempotent) "true" else "false"});
        try writer.print("    \"stateIsolated\": {s},\n", .{if (p.state_isolated) "true" else "false"});
        if (p.max_io_depth) |depth| {
            try writer.print("    \"maxIoDepth\": {d},\n", .{depth});
        } else {
            try writer.writeAll("    \"maxIoDepth\": null,\n");
        }
        try writer.print("    \"faultCovered\": {s},\n", .{if (p.fault_covered) "true" else "false"});
        try writer.print("    \"resultSafe\": {s},\n", .{if (p.result_safe) "true" else "false"});
        try writer.print("    \"optionalSafe\": {s}\n", .{if (p.optional_safe) "true" else "false"});
        try writer.writeAll("  },\n");
    } else {
        try writer.writeAll("  \"properties\": null,\n");
    }

    // behaviors (optional)
    if (contract.behaviors.items.len > 0) {
        try writer.writeAll("  \"behaviors\": [\n");
        for (contract.behaviors.items, 0..) |path, i| {
            if (i > 0) try writer.writeAll(",\n");
            try writer.writeAll("    {\n");
            try writer.writeAll("      \"method\": ");
            try writeJsonString(writer, path.route_method);
            try writer.writeAll(",\n");
            try writer.writeAll("      \"pattern\": ");
            try writeJsonString(writer, path.route_pattern);
            try writer.writeAll(",\n");
            try writer.print("      \"status\": {d},\n", .{path.response_status});
            try writer.print("      \"ioDepth\": {d},\n", .{path.io_depth});
            try writer.print("      \"failurePath\": {s},\n", .{if (path.is_failure_path) "true" else "false"});

            // conditions
            try writer.writeAll("      \"conditions\": [");
            for (path.conditions.items, 0..) |cond, j| {
                if (j > 0) try writer.writeAll(", ");
                try writer.writeAll("{\"kind\": ");
                try writeJsonString(writer, @tagName(cond.kind));
                if (cond.module) |m| {
                    try writer.writeAll(", \"module\": ");
                    try writeJsonString(writer, m);
                }
                if (cond.func) |f| {
                    try writer.writeAll(", \"func\": ");
                    try writeJsonString(writer, f);
                }
                if (cond.value) |v| {
                    try writer.writeAll(", \"value\": ");
                    try writeJsonString(writer, v);
                }
                try writer.writeByte('}');
            }
            try writer.writeAll("],\n");

            // io_sequence
            try writer.writeAll("      \"ioSequence\": [");
            for (path.io_sequence.items, 0..) |io, j| {
                if (j > 0) try writer.writeAll(", ");
                try writer.writeAll("{\"module\": ");
                try writeJsonString(writer, io.module);
                try writer.writeAll(", \"func\": ");
                try writeJsonString(writer, io.func);
                if (io.arg_signature) |sig| {
                    try writer.writeAll(", \"args\": ");
                    try writeJsonString(writer, sig);
                }
                try writer.writeByte('}');
            }
            try writer.writeAll("]\n");
            try writer.writeAll("    }");
        }
        try writer.writeAll("\n  ],\n");
        try writer.print("  \"behaviorsExhaustive\": {s}\n", .{if (contract.behaviors_exhaustive) "true" else "false"});
    } else {
        try writer.writeAll("  \"behaviors\": [],\n");
        try writer.print("  \"behaviorsExhaustive\": {s}\n", .{if (contract.behaviors_exhaustive) "true" else "false"});
    }

    try writer.writeAll("}\n");
}

pub fn dupeOptionalString(allocator: std.mem.Allocator, s: ?[]const u8) !?[]const u8 {
    return if (s) |v| try allocator.dupe(u8, v) else null;
}

pub const writeJsonStringContent = json_utils.writeJsonStringContent;
pub const writeJsonString = json_utils.writeJsonString;

fn writeApiParamJson(writer: anytype, param: *const ApiParamInfo) !void {
    try writer.writeAll("\n          {\n");
    try writer.writeAll("            \"name\": ");
    try writeJsonString(writer, param.name);
    try writer.writeAll(",\n");
    try writer.writeAll("            \"location\": ");
    try writeJsonString(writer, param.location);
    try writer.writeAll(",\n");
    try writer.print("            \"required\": {s},\n", .{if (param.required) "true" else "false"});
    try writer.writeAll("            \"schema\": ");
    try writer.writeAll(param.schema_json);
    try writer.writeAll("\n          }");
}

fn writeApiBodyJson(writer: anytype, body: *const ApiBodyInfo) !void {
    try writer.writeAll("\n          {\n");
    try writer.writeAll("            \"contentType\": ");
    if (body.content_type) |content_type| {
        try writeJsonString(writer, content_type);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(",\n");
    try writer.writeAll("            \"schemaRef\": ");
    if (body.schema_ref) |schema_ref| {
        try writeJsonString(writer, schema_ref);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(",\n");
    try writer.writeAll("            \"schema\": ");
    if (body.schema_json) |schema_json| {
        try writer.writeAll(schema_json);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(",\n");
    try writer.print("            \"dynamic\": {s}\n", .{if (body.dynamic) "true" else "false"});
    try writer.writeAll("          }");
}

fn writeApiResponseJson(writer: anytype, response: *const ApiResponseInfo) !void {
    try writer.writeAll("\n          {\n");
    try writer.writeAll("            \"status\": ");
    if (response.status) |status| {
        try writer.print("{d}", .{status});
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(",\n");
    try writer.writeAll("            \"contentType\": ");
    if (response.content_type) |content_type| {
        try writeJsonString(writer, content_type);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(",\n");
    try writer.writeAll("            \"schemaRef\": ");
    if (response.schema_ref) |schema_ref| {
        try writeJsonString(writer, schema_ref);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(",\n");
    try writer.writeAll("            \"schema\": ");
    if (response.schema_json) |schema_json| {
        try writer.writeAll(schema_json);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(",\n");
    try writer.print("            \"dynamic\": {s}\n", .{if (response.dynamic) "true" else "false"});
    try writer.writeAll("          }");
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

test "parseFromJson minimal" {
    const allocator = std.testing.allocator;
    const json =
        \\{
        \\  "version": 2,
        \\  "handler": { "path": "handler.ts", "line": 1, "column": 0 },
        \\  "routes": [],
        \\  "modules": [],
        \\  "functions": {},
        \\  "env": { "literal": [], "dynamic": false },
        \\  "egress": { "hosts": [], "dynamic": false },
        \\  "cache": { "namespaces": [], "dynamic": false },
        \\  "api": {},
        \\  "verification": null,
        \\  "aot": null
        \\}
    ;

    var contract = try parseFromJson(allocator, json);
    defer contract.deinit(allocator);

    try std.testing.expectEqualStrings("handler.ts", contract.handler.path);
    try std.testing.expectEqual(@as(u32, 1), contract.handler.line);
    try std.testing.expectEqual(@as(usize, 0), contract.routes.items.len);
    try std.testing.expect(!contract.env.dynamic);
    try std.testing.expect(!contract.durable.used);
    try std.testing.expect(contract.verification == null);
}

test "parseFromJson with data" {
    const allocator = std.testing.allocator;
    const json =
        \\{
        \\  "version": 2,
        \\  "handler": { "path": "handler.ts", "line": 5, "column": 2 },
        \\  "routes": [
        \\    { "pattern": "/health", "type": "exact", "field": "path", "status": 200, "contentType": "application/json", "aot": true },
        \\    { "pattern": "/api", "type": "prefix", "field": "path", "status": 200, "contentType": "application/json", "aot": true }
        \\  ],
        \\  "modules": ["zigttp:env"],
        \\  "functions": {},
        \\  "env": { "literal": ["JWT_SECRET", "API_KEY"], "dynamic": false },
        \\  "egress": { "hosts": ["api.stripe.com"], "dynamic": true },
        \\  "cache": { "namespaces": ["sessions"], "dynamic": false },
        \\  "api": {},
        \\  "verification": {
        \\    "exhaustiveReturns": true,
        \\    "resultsSafe": true,
        \\    "unreachableCode": false,
        \\    "bytecodeVerified": true
        \\  },
        \\  "aot": null
        \\}
    ;

    var contract = try parseFromJson(allocator, json);
    defer contract.deinit(allocator);

    try std.testing.expectEqualStrings("handler.ts", contract.handler.path);
    try std.testing.expectEqual(@as(u32, 5), contract.handler.line);
    try std.testing.expectEqual(@as(usize, 2), contract.routes.items.len);
    try std.testing.expectEqualStrings("/health", contract.routes.items[0].pattern);
    try std.testing.expectEqualStrings("exact", contract.routes.items[0].route_type);
    try std.testing.expectEqualStrings("/api", contract.routes.items[1].pattern);
    try std.testing.expectEqualStrings("prefix", contract.routes.items[1].route_type);
    try std.testing.expectEqual(@as(usize, 2), contract.env.literal.items.len);
    try std.testing.expectEqualStrings("JWT_SECRET", contract.env.literal.items[0]);
    try std.testing.expectEqualStrings("API_KEY", contract.env.literal.items[1]);
    try std.testing.expect(!contract.env.dynamic);
    try std.testing.expectEqual(@as(usize, 1), contract.egress.hosts.items.len);
    try std.testing.expect(contract.egress.dynamic);
    try std.testing.expectEqual(@as(usize, 1), contract.cache.namespaces.items.len);
    try std.testing.expect(!contract.durable.used);
    try std.testing.expect(contract.verification != null);
    try std.testing.expect(contract.verification.?.exhaustive_returns);
    try std.testing.expect(contract.verification.?.bytecode_verified);
}

test "parseFromJson roundtrip" {
    const allocator = std.testing.allocator;

    // Create a contract, serialize it, parse it back, verify fields match
    const path = try allocator.dupe(u8, "test.ts");
    var env_lit: std.ArrayList([]const u8) = .empty;
    try env_lit.append(allocator, try allocator.dupe(u8, "SECRET"));
    var service_calls: std.ArrayList(ServiceCallInfo) = .empty;
    try service_calls.append(allocator, .{
        .service = try allocator.dupe(u8, "users"),
        .route_pattern = try allocator.dupe(u8, "GET /api/users/:id"),
        .path_params = blk: {
            var params: std.ArrayList([]const u8) = .empty;
            try params.append(allocator, try allocator.dupe(u8, "id"));
            break :blk params;
        },
        .query_keys = blk: {
            var keys: std.ArrayList([]const u8) = .empty;
            try keys.append(allocator, try allocator.dupe(u8, "expand"));
            break :blk keys;
        },
        .header_keys = blk: {
            var keys: std.ArrayList([]const u8) = .empty;
            try keys.append(allocator, try allocator.dupe(u8, "x-auth"));
            break :blk keys;
        },
        .has_body = false,
    });

    var original = HandlerContract{
        .handler = .{ .path = path, .line = 10, .column = 5 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = env_lit, .dynamic = true },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .service_calls = service_calls,
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .sql = emptySqlInfo(),
        .durable = .{
            .used = true,
            .keys = .{ .literal = .empty, .dynamic = true },
            .steps = .empty,
        },
        .scope = .{
            .used = true,
            .names = .empty,
            .dynamic = false,
            .max_depth = 1,
        },
        .api = emptyApiInfo(),
        .verification = .{
            .exhaustive_returns = true,
            .results_safe = false,
            .unreachable_code = false,
            .bytecode_verified = true,
        },
        .aot = null,
    };
    defer original.deinit(allocator);

    // Serialize
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);
    try writeContractJson(&original, &aw.writer);
    output = aw.toArrayList();

    // Parse back
    var parsed = try parseFromJson(allocator, output.items);
    defer parsed.deinit(allocator);

    try std.testing.expectEqualStrings("test.ts", parsed.handler.path);
    try std.testing.expectEqual(@as(u32, 10), parsed.handler.line);
    try std.testing.expectEqual(@as(usize, 1), parsed.env.literal.items.len);
    try std.testing.expectEqualStrings("SECRET", parsed.env.literal.items[0]);
    try std.testing.expect(parsed.env.dynamic);
    try std.testing.expectEqual(@as(usize, 1), parsed.service_calls.items.len);
    try std.testing.expectEqualStrings("users", parsed.service_calls.items[0].service);
    try std.testing.expectEqualStrings("GET /api/users/:id", parsed.service_calls.items[0].route_pattern);
    try std.testing.expectEqual(@as(usize, 1), parsed.service_calls.items[0].path_params.items.len);
    try std.testing.expectEqualStrings("id", parsed.service_calls.items[0].path_params.items[0]);
    try std.testing.expect(parsed.durable.used);
    try std.testing.expect(parsed.durable.keys.dynamic);
    try std.testing.expect(parsed.scope.used);
    try std.testing.expectEqual(@as(u32, 1), parsed.scope.max_depth);
    try std.testing.expect(parsed.verification != null);
    try std.testing.expect(parsed.verification.?.exhaustive_returns);
    try std.testing.expect(!parsed.verification.?.results_safe);
    try std.testing.expect(parsed.verification.?.bytecode_verified);
}

test "parseFromJson roundtrip preserves durable workflow" {
    const allocator = std.testing.allocator;

    var workflow_nodes: std.ArrayList(DurableWorkflowNode) = .empty;
    try workflow_nodes.append(allocator, .{
        .id = try allocator.dupe(u8, "n1"),
        .kind = .step,
        .label = try allocator.dupe(u8, "charge"),
        .detail = try allocator.dupe(u8, "timeoutMs=1000"),
    });
    try workflow_nodes.append(allocator, .{
        .id = try allocator.dupe(u8, "n2"),
        .kind = .return_response,
        .label = try allocator.dupe(u8, "Response 202"),
        .status = 202,
    });

    var workflow_edges: std.ArrayList(DurableWorkflowEdge) = .empty;
    try workflow_edges.append(allocator, .{
        .from = try allocator.dupe(u8, "start"),
        .to = try allocator.dupe(u8, "n1"),
    });
    try workflow_edges.append(allocator, .{
        .from = try allocator.dupe(u8, "n1"),
        .to = try allocator.dupe(u8, "n2"),
        .condition = try allocator.dupe(u8, "then@4:7"),
    });

    const path = try allocator.dupe(u8, "workflow.ts");

    var original = HandlerContract{
        .handler = .{ .path = path, .line = 3, .column = 1 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = .empty, .dynamic = false },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .service_calls = .empty,
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .sql = emptySqlInfo(),
        .durable = .{
            .used = true,
            .keys = .{ .literal = .empty, .dynamic = false },
            .steps = .empty,
            .workflow = .{
                .workflow_id = try allocator.dupe(u8, "workflow.ts:handler:4:10"),
                .proof_level = .complete,
                .nodes = workflow_nodes,
                .edges = workflow_edges,
            },
        },
        .scope = .{
            .used = false,
            .names = .empty,
            .dynamic = false,
            .max_depth = 0,
        },
        .api = emptyApiInfo(),
        .verification = null,
        .aot = null,
    };
    defer original.deinit(allocator);

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);
    try writeContractJson(&original, &aw.writer);
    output = aw.toArrayList();

    var parsed = try parseFromJson(allocator, output.items);
    defer parsed.deinit(allocator);

    try std.testing.expectEqualStrings("workflow.ts:handler:4:10", parsed.durable.workflow.workflow_id.?);
    try std.testing.expectEqual(DurableWorkflowProofLevel.complete, parsed.durable.workflow.proof_level);
    try std.testing.expectEqual(@as(usize, 2), parsed.durable.workflow.nodes.items.len);
    try std.testing.expectEqual(DurableWorkflowNodeKind.step, parsed.durable.workflow.nodes.items[0].kind);
    try std.testing.expectEqualStrings("charge", parsed.durable.workflow.nodes.items[0].label);
    try std.testing.expectEqualStrings("timeoutMs=1000", parsed.durable.workflow.nodes.items[0].detail.?);
    try std.testing.expectEqual(@as(u16, 202), parsed.durable.workflow.nodes.items[1].status.?);
    try std.testing.expectEqual(@as(usize, 2), parsed.durable.workflow.edges.items.len);
    try std.testing.expectEqualStrings("then@4:7", parsed.durable.workflow.edges.items[1].condition.?);
}

test "parseFromJson roundtrip preserves api route response schema" {
    const allocator = std.testing.allocator;

    var path_params: std.ArrayList(ApiParamInfo) = .empty;
    try path_params.append(allocator, .{
        .name = try allocator.dupe(u8, "id"),
        .location = "path",
        .required = true,
        .schema_json = try allocator.dupe(u8, "{\"type\":\"string\"}"),
    });
    var query_params: std.ArrayList(ApiParamInfo) = .empty;
    try query_params.append(allocator, .{
        .name = try allocator.dupe(u8, "verbose"),
        .location = "query",
        .required = false,
        .schema_json = try allocator.dupe(u8, "{\"type\":\"string\"}"),
    });
    var header_params: std.ArrayList(ApiParamInfo) = .empty;
    try header_params.append(allocator, .{
        .name = try allocator.dupe(u8, "authorization"),
        .location = "header",
        .required = false,
        .schema_json = try allocator.dupe(u8, "{\"type\":\"string\"}"),
    });
    var request_bodies: std.ArrayList(ApiBodyInfo) = .empty;
    try request_bodies.append(allocator, .{
        .content_type = try allocator.dupe(u8, "application/json"),
        .schema_ref = try allocator.dupe(u8, "user.create"),
    });
    var responses: std.ArrayList(ApiResponseInfo) = .empty;
    try responses.append(allocator, .{
        .status = 200,
        .content_type = try allocator.dupe(u8, "application/json"),
        .schema_ref = try allocator.dupe(u8, "user"),
    });

    var routes: std.ArrayList(ApiRouteInfo) = .empty;
    try routes.append(allocator, .{
        .method = try allocator.dupe(u8, "GET"),
        .path = try allocator.dupe(u8, "/users/:id"),
        .request_schema_refs = blk: {
            var refs: std.ArrayList([]const u8) = .empty;
            try refs.append(allocator, try allocator.dupe(u8, "user.create"));
            break :blk refs;
        },
        .request_schema_dynamic = false,
        .requires_bearer = false,
        .requires_jwt = false,
        .path_params = path_params,
        .query_params = query_params,
        .header_params = header_params,
        .request_bodies = request_bodies,
        .responses = responses,
        .response_status = 200,
        .response_content_type = try allocator.dupe(u8, "application/json"),
        .response_schema_ref = try allocator.dupe(u8, "user"),
        .response_schema_dynamic = false,
    });

    var contract = HandlerContract{
        .handler = .{ .path = try allocator.dupe(u8, "api.ts"), .line = 1, .column = 0 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = .empty, .dynamic = false },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .sql = emptySqlInfo(),
        .durable = .{
            .used = false,
            .keys = .{ .literal = .empty, .dynamic = false },
            .steps = .empty,
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
    try writeContractJson(&contract, &aw.writer);
    output = aw.toArrayList();

    var parsed = try parseFromJson(allocator, output.items);
    defer parsed.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 1), parsed.api.routes.items.len);
    const route = parsed.api.routes.items[0];
    try std.testing.expectEqualStrings("GET", route.method);
    try std.testing.expectEqualStrings("/users/:id", route.path);
    try std.testing.expectEqual(@as(usize, 1), route.path_params.items.len);
    try std.testing.expectEqualStrings("id", route.path_params.items[0].name);
    try std.testing.expect(route.path_params.items[0].required);
    try std.testing.expectEqual(@as(usize, 1), route.query_params.items.len);
    try std.testing.expectEqualStrings("verbose", route.query_params.items[0].name);
    try std.testing.expectEqual(@as(usize, 1), route.header_params.items.len);
    try std.testing.expectEqualStrings("authorization", route.header_params.items[0].name);
    try std.testing.expectEqual(@as(usize, 1), route.request_bodies.items.len);
    try std.testing.expectEqualStrings("user.create", route.request_bodies.items[0].schema_ref.?);
    try std.testing.expectEqual(@as(usize, 1), route.responses.items.len);
    try std.testing.expectEqual(@as(u16, 200), route.responses.items[0].status.?);
    try std.testing.expectEqualStrings("user", route.response_schema_ref.?);
}

test "extractHost from URL" {
    try std.testing.expectEqualStrings("api.example.com", extractHost("https://api.example.com/path"));
    try std.testing.expectEqualStrings("api.example.com", extractHost("http://api.example.com:8080/path"));
    try std.testing.expectEqualStrings("localhost", extractHost("http://localhost/test"));
    try std.testing.expectEqualStrings("", extractHost("not-a-url"));
    try std.testing.expectEqualStrings("", extractHost(""));
}

test "writeJsonString escapes correctly" {
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &output);

    try writeJsonString(&aw.writer, "hello \"world\"");
    output = aw.toArrayList();
    try std.testing.expectEqualStrings("\"hello \\\"world\\\"\"", output.items);
}

test "writeContractJson minimal" {
    const allocator = std.testing.allocator;

    const path = try allocator.dupe(u8, "handler.ts");

    var contract = HandlerContract{
        .handler = .{ .path = path, .line = 1, .column = 0 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = .empty, .dynamic = false },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .sql = emptySqlInfo(),
        .durable = .{
            .used = false,
            .keys = .{ .literal = .empty, .dynamic = false },
            .steps = .empty,
        },
        .scope = .{
            .used = false,
            .names = .empty,
            .dynamic = false,
            .max_depth = 0,
        },
        .api = emptyApiInfo(),
        .verification = null,
        .aot = null,
    };
    defer contract.deinit(allocator);

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    try writeContractJson(&contract, &aw.writer);
    output = aw.toArrayList();

    // Should be valid-looking JSON with expected fields
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"version\": 14") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"handler.ts\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"modules\": []") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"serviceCalls\": []") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"durable\": {") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"scope\": {") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"api\": {") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"verification\": null") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"properties\": null") != null);
}

test "writeContractJson with data" {
    const allocator = std.testing.allocator;

    const path = try allocator.dupe(u8, "handler.ts");

    var modules: std.ArrayList([]const u8) = .empty;
    const mod_str = try allocator.dupe(u8, "zigttp:auth");
    try modules.append(allocator, mod_str);

    var func_names: std.ArrayList([]const u8) = .empty;
    const fn_str = try allocator.dupe(u8, "jwtVerify");
    try func_names.append(allocator, fn_str);

    var functions: std.ArrayList(HandlerContract.FunctionEntry) = .empty;
    const func_mod = try allocator.dupe(u8, "zigttp:auth");
    try functions.append(allocator, .{
        .module = func_mod,
        .names = func_names,
    });

    var env_lit: std.ArrayList([]const u8) = .empty;
    const env_str = try allocator.dupe(u8, "JWT_SECRET");
    try env_lit.append(allocator, env_str);

    var hosts: std.ArrayList([]const u8) = .empty;
    const host_str = try allocator.dupe(u8, "api.example.com");
    try hosts.append(allocator, host_str);

    var namespaces: std.ArrayList([]const u8) = .empty;
    const ns_str = try allocator.dupe(u8, "sessions");
    try namespaces.append(allocator, ns_str);

    var schemas: std.ArrayList(ApiSchemaInfo) = .empty;
    try schemas.append(allocator, .{
        .name = try allocator.dupe(u8, "user"),
        .schema_json = try allocator.dupe(u8, "{\"type\":\"object\"}"),
    });

    var request_schema_refs: std.ArrayList([]const u8) = .empty;
    try request_schema_refs.append(allocator, try allocator.dupe(u8, "user"));

    var durable_keys: std.ArrayList([]const u8) = .empty;
    try durable_keys.append(allocator, try allocator.dupe(u8, "order:123"));

    var durable_steps: std.ArrayList([]const u8) = .empty;
    try durable_steps.append(allocator, try allocator.dupe(u8, "charge"));

    var contract = HandlerContract{
        .handler = .{ .path = path, .line = 20, .column = 1 },
        .routes = .empty,
        .modules = modules,
        .functions = functions,
        .env = .{ .literal = env_lit, .dynamic = false },
        .egress = .{ .hosts = hosts, .dynamic = true },
        .cache = .{ .namespaces = namespaces, .dynamic = false },
        .sql = emptySqlInfo(),
        .durable = .{
            .used = true,
            .keys = .{ .literal = durable_keys, .dynamic = false },
            .steps = durable_steps,
        },
        .scope = .{
            .used = false,
            .names = .empty,
            .dynamic = false,
            .max_depth = 0,
        },
        .api = .{
            .schemas = schemas,
            .requests = .{ .schema_refs = request_schema_refs, .dynamic = false },
            .auth = .{ .bearer = true, .jwt = true },
            .routes = .empty,
            .schemas_dynamic = false,
            .routes_dynamic = false,
        },
        .verification = .{
            .exhaustive_returns = true,
            .results_safe = true,
            .unreachable_code = false,
        },
        .aot = .{ .pattern_count = 3, .has_default = true },
    };
    defer contract.deinit(allocator);

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    try writeContractJson(&contract, &aw.writer);
    output = aw.toArrayList();

    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"zigttp:auth\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"jwtVerify\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"JWT_SECRET\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"api.example.com\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"sessions\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"order:123\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"charge\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"schemaRefs\": [\"user\"]") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"bearer\": true") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"exhaustiveReturns\": true") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"patternCount\": 3") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"dynamic\": true") != null);
}

test "behaviors serialization roundtrip" {
    const allocator = std.testing.allocator;

    const json =
        \\{
        \\  "version": 9,
        \\  "handler": { "path": "handler.ts", "line": 1, "column": 0 },
        \\  "routes": [],
        \\  "modules": [],
        \\  "functions": {},
        \\  "env": { "literal": [], "dynamic": false },
        \\  "egress": { "hosts": [], "dynamic": false },
        \\  "cache": { "namespaces": [], "dynamic": false },
        \\  "api": {},
        \\  "verification": null,
        \\  "aot": null,
        \\  "behaviors": [
        \\    {
        \\      "method": "GET",
        \\      "pattern": "/users/:id",
        \\      "status": 200,
        \\      "ioDepth": 2,
        \\      "failurePath": false,
        \\      "conditions": [
        \\        {"kind": "io_ok", "module": "auth", "func": "jwtVerify"},
        \\        {"kind": "io_ok", "module": "cache", "func": "cacheGet"}
        \\      ],
        \\      "ioSequence": [
        \\        {"module": "auth", "func": "jwtVerify"},
        \\        {"module": "cache", "func": "cacheGet"}
        \\      ]
        \\    },
        \\    {
        \\      "method": "GET",
        \\      "pattern": "/users/:id",
        \\      "status": 401,
        \\      "ioDepth": 1,
        \\      "failurePath": true,
        \\      "conditions": [
        \\        {"kind": "io_fail", "module": "auth", "func": "jwtVerify"}
        \\      ],
        \\      "ioSequence": [
        \\        {"module": "auth", "func": "jwtVerify"}
        \\      ]
        \\    }
        \\  ],
        \\  "behaviorsExhaustive": true
        \\}
    ;

    var contract = try parseFromJson(allocator, json);
    defer contract.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 2), contract.behaviors.items.len);
    try std.testing.expect(contract.behaviors_exhaustive);

    const path0 = contract.behaviors.items[0];
    try std.testing.expectEqualStrings("GET", path0.route_method);
    try std.testing.expectEqualStrings("/users/:id", path0.route_pattern);
    try std.testing.expectEqual(@as(u16, 200), path0.response_status);
    try std.testing.expectEqual(@as(u32, 2), path0.io_depth);
    try std.testing.expect(!path0.is_failure_path);
    try std.testing.expectEqual(@as(usize, 2), path0.conditions.items.len);
    try std.testing.expectEqual(PathCondition.Kind.io_ok, path0.conditions.items[0].kind);
    try std.testing.expectEqualStrings("auth", path0.conditions.items[0].module.?);
    try std.testing.expectEqualStrings("jwtVerify", path0.conditions.items[0].func.?);
    try std.testing.expectEqual(@as(usize, 2), path0.io_sequence.items.len);

    const path1 = contract.behaviors.items[1];
    try std.testing.expectEqual(@as(u16, 401), path1.response_status);
    try std.testing.expect(path1.is_failure_path);
    try std.testing.expectEqual(PathCondition.Kind.io_fail, path1.conditions.items[0].kind);
}

fn appendTrackedFunction(
    builder: *ContractBuilder,
    module_name: []const u8,
    func_name: []const u8,
) !void {
    var names: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (names.items) |name| builder.allocator.free(name);
        names.deinit(builder.allocator);
    }

    try names.append(builder.allocator, try builder.allocator.dupe(u8, func_name));

    try builder.functions_map.append(builder.allocator, .{
        .module = try builder.allocator.dupe(u8, module_name),
        .names = names,
    });
}

test "computeProperties pure handler stays pure" {
    var builder = ContractBuilder.init(std.testing.allocator, undefined, null, null, null);
    defer builder.deinit();

    const props = builder.computeProperties();

    try std.testing.expect(props.pure);
    try std.testing.expect(props.read_only);
    try std.testing.expect(props.stateless);
    try std.testing.expect(props.retry_safe);
    try std.testing.expect(props.deterministic);
    try std.testing.expect(!props.has_egress);
    try std.testing.expect(props.idempotent);
}

test "computeProperties cache read is read-only but not stateless" {
    var builder = ContractBuilder.init(std.testing.allocator, undefined, null, null, null);
    defer builder.deinit();

    try appendTrackedFunction(&builder, "zigttp:cache", "cacheGet");

    const props = builder.computeProperties();

    try std.testing.expect(!props.pure);
    try std.testing.expect(props.read_only);
    try std.testing.expect(!props.stateless);
    try std.testing.expect(props.retry_safe);
    try std.testing.expect(props.deterministic);
    try std.testing.expect(!props.has_egress);
    try std.testing.expect(props.idempotent);
}

test "computeProperties bare writes are not retry safe" {
    var builder = ContractBuilder.init(std.testing.allocator, undefined, null, null, null);
    defer builder.deinit();

    try appendTrackedFunction(&builder, "zigttp:cache", "cacheSet");

    const props = builder.computeProperties();

    try std.testing.expect(!props.pure);
    try std.testing.expect(!props.read_only);
    try std.testing.expect(!props.stateless);
    try std.testing.expect(!props.retry_safe);
    try std.testing.expect(props.deterministic);
    try std.testing.expect(!props.has_egress);
    try std.testing.expect(!props.idempotent);
}

test "computeProperties durable-only writes stay retry safe" {
    var builder = ContractBuilder.init(std.testing.allocator, undefined, null, null, null);
    defer builder.deinit();

    try appendTrackedFunction(&builder, "zigttp:durable", "step");
    builder.durable_used = true;

    const props = builder.computeProperties();

    try std.testing.expect(!props.pure);
    try std.testing.expect(!props.read_only);
    try std.testing.expect(!props.stateless);
    try std.testing.expect(props.retry_safe);
    try std.testing.expect(props.deterministic);
    try std.testing.expect(!props.has_egress);
    try std.testing.expect(props.idempotent);
}

test "computeProperties nondeterministic builtins clear idempotence" {
    var builder = ContractBuilder.init(std.testing.allocator, undefined, null, null, null);
    defer builder.deinit();

    try appendTrackedFunction(&builder, "zigttp:cache", "cacheGet");
    builder.has_nondeterministic_builtin = true;

    const props = builder.computeProperties();

    try std.testing.expect(!props.pure);
    try std.testing.expect(props.read_only);
    try std.testing.expect(!props.stateless);
    try std.testing.expect(props.retry_safe);
    try std.testing.expect(!props.deterministic);
    try std.testing.expect(!props.has_egress);
    try std.testing.expect(!props.idempotent);
}

test "computeProperties egress is conservative write" {
    var builder = ContractBuilder.init(std.testing.allocator, undefined, null, null, null);
    defer builder.deinit();

    builder.egress_dynamic = true;

    const props = builder.computeProperties();

    try std.testing.expect(!props.pure);
    try std.testing.expect(!props.read_only);
    try std.testing.expect(!props.stateless);
    try std.testing.expect(!props.retry_safe);
    try std.testing.expect(props.deterministic);
    try std.testing.expect(props.has_egress);
    try std.testing.expect(!props.idempotent);
}

test "scanImports includes zigttp-ext modules in contract function map" {
    const allocator = std.testing.allocator;
    const source =
        \\import { double } from "zigttp-ext:math";
        \\const value = double(21);
    ;

    var parser = @import("parser/parse.zig").Parser.init(allocator, source);
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    parser.setAtomTable(&atoms);
    defer parser.deinit();

    _ = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);

    var builder = ContractBuilder.init(allocator, ir_view, &atoms, null, null);
    defer builder.deinit();

    try builder.scanImports();

    try std.testing.expect(containsString(builder.modules_list.items, "zigttp-ext:math"));
    try std.testing.expectEqual(@as(usize, 1), builder.functions_map.items.len);
    try std.testing.expectEqualStrings("zigttp-ext:math", builder.functions_map.items[0].module);
    try std.testing.expect(containsString(builder.functions_map.items[0].names.items, "double"));
}

test "computeCapabilityMatrix unions crypto and auth into canonical set" {
    const specs = [_][]const u8{ "zigttp:crypto", "zigttp:auth" };
    const matrix = computeCapabilityMatrix(&specs);

    // auth needs crypto + clock, crypto needs crypto. Union = {crypto, clock}.
    // Canonical order is enum-declaration order: clock before crypto.
    try std.testing.expectEqual(@as(u8, 2), matrix.len);
    try std.testing.expectEqual(module_binding.ModuleCapability.clock, matrix.items[0]);
    try std.testing.expectEqual(module_binding.ModuleCapability.crypto, matrix.items[1]);

    // Hash is non-zero and deterministic
    try std.testing.expect(!std.mem.allEqual(u8, &matrix.hash, 0));
    const again = computeCapabilityMatrix(&specs);
    try std.testing.expectEqualSlices(u8, &matrix.hash, &again.hash);
}

test "computeCapabilityMatrix is order-independent for hash" {
    const specs_ab = [_][]const u8{ "zigttp:crypto", "zigttp:auth" };
    const specs_ba = [_][]const u8{ "zigttp:auth", "zigttp:crypto" };
    const a = computeCapabilityMatrix(&specs_ab);
    const b = computeCapabilityMatrix(&specs_ba);
    try std.testing.expectEqualSlices(u8, &a.hash, &b.hash);
    try std.testing.expectEqual(a.len, b.len);
}

test "computeCapabilityMatrix skips unknown specifiers" {
    const specs = [_][]const u8{ "zigttp:crypto", "zigttp:does-not-exist" };
    const matrix = computeCapabilityMatrix(&specs);
    try std.testing.expectEqual(@as(u8, 1), matrix.len);
    try std.testing.expectEqual(module_binding.ModuleCapability.crypto, matrix.items[0]);
}

test "computeCapabilityMatrix empty input has empty len and zero hash-of-empty" {
    const matrix = computeCapabilityMatrix(&.{});
    try std.testing.expectEqual(@as(u8, 0), matrix.len);
    // Hash of an empty canonical list must still be deterministic
    const again = computeCapabilityMatrix(&.{});
    try std.testing.expectEqualSlices(u8, &matrix.hash, &again.hash);
}

test "CapabilityMatrix.has finds present and misses absent" {
    const specs = [_][]const u8{"zigttp:log"};
    const matrix = computeCapabilityMatrix(&specs);
    try std.testing.expect(matrix.has(.stderr));
    try std.testing.expect(matrix.has(.clock));
    try std.testing.expect(!matrix.has(.crypto));
}

test "sandbox block roundtrips through writeContractJson and parseFromJson" {
    const allocator = std.testing.allocator;

    // Build a contract with modules set so build-then-serialize has caps
    var modules: std.ArrayList([]const u8) = .empty;
    try modules.append(allocator, try allocator.dupe(u8, "zigttp:crypto"));
    try modules.append(allocator, try allocator.dupe(u8, "zigttp:auth"));

    var contract = HandlerContract{
        .handler = .{ .path = try allocator.dupe(u8, "handler.ts"), .line = 1, .column = 0 },
        .routes = .empty,
        .modules = modules,
        .functions = .empty,
        .env = .{ .literal = .empty, .dynamic = false },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .sql = emptySqlInfo(),
        .durable = .{
            .used = false,
            .keys = .{ .literal = .empty, .dynamic = false },
            .steps = .empty,
        },
        .scope = .{
            .used = false,
            .names = .empty,
            .dynamic = false,
            .max_depth = 0,
        },
        .api = emptyApiInfo(),
        .verification = null,
        .aot = null,
    };
    contract.capabilities = computeCapabilityMatrix(contract.modules.items);
    defer contract.deinit(allocator);

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);
    try writeContractJson(&contract, &aw.writer);
    output = aw.toArrayList();

    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"sandbox\":") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"capabilityHash\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"clock\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"crypto\"") != null);

    var parsed = try parseFromJson(allocator, output.items);
    defer parsed.deinit(allocator);

    try std.testing.expectEqual(contract.capabilities.len, parsed.capabilities.len);
    try std.testing.expectEqualSlices(u8, &contract.capabilities.hash, &parsed.capabilities.hash);
    try std.testing.expect(parsed.capabilities.has(.clock));
    try std.testing.expect(parsed.capabilities.has(.crypto));
}
