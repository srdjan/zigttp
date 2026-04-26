//! Contract data types for `HandlerContract`. Extracted from
//! handler_contract.zig as the third step of the split. handler_contract.zig
//! re-exports every public name from this file so existing callers
//! (`handler_contract.HandlerContract`, `handler_contract.ApiRouteInfo`, etc.)
//! continue to resolve. Constructors that are tightly coupled to the types
//! they build (`emptyApiInfo`, `emptySqlInfo`, `emptyContract`,
//! `computeCapabilityMatrix`) live here too.

const std = @import("std");
const module_binding = @import("module_binding.zig");
const builtin_modules = @import("builtin_modules.zig");

fn dupeOptionalString(allocator: std.mem.Allocator, s: ?[]const u8) !?[]const u8 {
    return if (s) |v| try allocator.dupe(u8, v) else null;
}

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

pub const SchemaSpec = union(enum) {
    none,
    ref: []const u8, // owned
    inline_json: []const u8, // owned
    dynamic,

    pub fn schemaRef(self: SchemaSpec) ?[]const u8 {
        return switch (self) {
            .ref => |s| s,
            else => null,
        };
    }

    pub fn schemaJson(self: SchemaSpec) ?[]const u8 {
        return switch (self) {
            .inline_json => |s| s,
            else => null,
        };
    }

    pub fn isDynamic(self: SchemaSpec) bool {
        return self == .dynamic;
    }

    pub fn deinitOwned(self: SchemaSpec, allocator: std.mem.Allocator) void {
        switch (self) {
            .ref => |s| allocator.free(s),
            .inline_json => |s| allocator.free(s),
            .none, .dynamic => {},
        }
    }

    pub fn dupeOwned(self: SchemaSpec, allocator: std.mem.Allocator) !SchemaSpec {
        return switch (self) {
            .none => .none,
            .ref => |s| .{ .ref = try allocator.dupe(u8, s) },
            .inline_json => |s| .{ .inline_json = try allocator.dupe(u8, s) },
            .dynamic => .dynamic,
        };
    }
};

pub const ApiBodyInfo = struct {
    content_type: ?[]const u8 = null, // owned when present
    schema: SchemaSpec = .none,

    pub fn deinit(self: *ApiBodyInfo, allocator: std.mem.Allocator) void {
        if (self.content_type) |content_type| allocator.free(content_type);
        self.schema.deinitOwned(allocator);
    }

    pub fn dupeOwned(self: ApiBodyInfo, allocator: std.mem.Allocator) !ApiBodyInfo {
        return .{
            .content_type = try dupeOptionalString(allocator, self.content_type),
            .schema = try self.schema.dupeOwned(allocator),
        };
    }
};

pub const ApiResponseInfo = struct {
    status: ?u16 = null,
    content_type: ?[]const u8 = null, // owned when present
    schema: SchemaSpec = .none,

    pub fn deinit(self: *ApiResponseInfo, allocator: std.mem.Allocator) void {
        if (self.content_type) |content_type| allocator.free(content_type);
        self.schema.deinitOwned(allocator);
    }

    pub fn dupeOwned(self: ApiResponseInfo, allocator: std.mem.Allocator) !ApiResponseInfo {
        return .{
            .status = self.status,
            .content_type = try dupeOptionalString(allocator, self.content_type),
            .schema = try self.schema.dupeOwned(allocator),
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
    /// Service or route_pattern is computed at runtime. When true, the
    /// per-surface fields below carry no useful information.
    dynamic: bool = false,
    path_params: KnownList = .{ .complete = .empty },
    query_keys: KnownList = .{ .complete = .empty },
    header_keys: KnownList = .{ .complete = .empty },
    body: BodySpec = .none,

    /// A statically-enumerated list of string keys, or "opaque" when the
    /// analyzer could not prove the set is complete. The system linker
    /// treats `.dynamic` as "cannot prove" and fails closed on required keys.
    pub const KnownList = union(enum) {
        complete: std.ArrayList([]const u8), // each entry owned
        dynamic,

        pub fn isDynamic(self: KnownList) bool {
            return self == .dynamic;
        }

        pub fn items(self: KnownList) []const []const u8 {
            return switch (self) {
                .complete => |list| list.items,
                .dynamic => &.{},
            };
        }

        pub fn deinitOwned(self: *KnownList, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .complete => |*list| {
                    for (list.items) |s| allocator.free(s);
                    list.deinit(allocator);
                },
                .dynamic => {},
            }
        }

        pub fn markDynamic(self: *KnownList, allocator: std.mem.Allocator) void {
            self.deinitOwned(allocator);
            self.* = .dynamic;
        }
    };

    /// Three-state body presence: no body field, body present (statically
    /// inspectable), or body present but opaque at compile time. Replaces the
    /// prior `has_body: bool` + `body_dynamic: bool` pair where some
    /// combinations were meaningless.
    pub const BodySpec = union(enum) {
        none,
        present,
        dynamic,

        pub fn isDynamic(self: BodySpec) bool {
            return self == .dynamic;
        }

        pub fn isPresent(self: BodySpec) bool {
            return self != .none;
        }
    };

    pub fn markAllDynamic(self: *ServiceCallInfo, allocator: std.mem.Allocator) void {
        self.path_params.markDynamic(allocator);
        self.query_keys.markDynamic(allocator);
        self.header_keys.markDynamic(allocator);
        self.body = .dynamic;
    }

    pub fn deinit(self: *ServiceCallInfo, allocator: std.mem.Allocator) void {
        allocator.free(self.service);
        allocator.free(self.route_pattern);
        self.path_params.deinitOwned(allocator);
        self.query_keys.deinitOwned(allocator);
        self.header_keys.deinitOwned(allocator);
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
    /// SHA-256 of the embedded Wasm policy artifact (Phase 2). All-zeros
    /// means no Wasm policy is present; the runtime falls back to the
    /// in-process LocalPolicyChecker. Non-zero values are verified at startup
    /// against the artifact on disk before the WasmPool is populated.
    wasm_policy_hash: [32]u8 = [_]u8{0} ** 32,

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
