//! Handler Contract Manifest
//!
//! Extracts a machine-readable contract from a handler's IR describing what
//! the handler is allowed to do: which routes it serves, which virtual modules
//! it uses, which env vars / outbound hosts / cache namespaces it references.
//!
//! This is purely an emission pass (v1). It does not enforce anything at runtime.
//! Enforcement and doc generation are v2 concerns.
//!
//! Usage: called by precompile.zig when --contract is passed.
//!
//! All string data in HandlerContract is owned (duped). The contract outlives
//! the parser and atom table that produced it.

const std = @import("std");
const ir = @import("parser/ir.zig");
const object = @import("object.zig");
const context = @import("context.zig");
const modules_resolver = @import("modules/resolver.zig");
const bytecode = @import("bytecode.zig");
const handler_analyzer = @import("handler_analyzer.zig");

const Node = ir.Node;
const NodeIndex = ir.NodeIndex;
const NodeTag = ir.NodeTag;
const IrView = ir.IrView;
const null_node = ir.null_node;
const HandlerPattern = bytecode.HandlerPattern;
const PatternDispatchTable = bytecode.PatternDispatchTable;

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
    dynamic: bool,
};

pub const CacheInfo = struct {
    namespaces: std.ArrayList([]const u8), // each entry owned
    dynamic: bool,
};

pub const DurableKeyInfo = struct {
    literal: std.ArrayList([]const u8), // each entry owned
    dynamic: bool,
};

pub const DurableInfo = struct {
    used: bool,
    keys: DurableKeyInfo,
    steps: std.ArrayList([]const u8), // each entry owned

    pub fn deinit(self: *DurableInfo, allocator: std.mem.Allocator) void {
        for (self.keys.literal.items) |key| {
            allocator.free(key);
        }
        self.keys.literal.deinit(allocator);
        for (self.steps.items) |step| {
            allocator.free(step);
        }
        self.steps.deinit(allocator);
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

pub const ApiRouteInfo = struct {
    method: []const u8, // owned
    path: []const u8, // owned
    request_schema_refs: std.ArrayList([]const u8), // each entry owned
    request_schema_dynamic: bool,
    requires_bearer: bool,
    requires_jwt: bool,
    response_status: ?u16 = null,
    response_content_type: ?[]const u8 = null, // owned when present

    pub fn deinit(self: *ApiRouteInfo, allocator: std.mem.Allocator) void {
        allocator.free(self.method);
        allocator.free(self.path);
        for (self.request_schema_refs.items) |schema_ref| {
            allocator.free(schema_ref);
        }
        self.request_schema_refs.deinit(allocator);
        if (self.response_content_type) |content_type| {
            allocator.free(content_type);
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

pub const HandlerContract = struct {
    version: u32 = 3,
    handler: HandlerLoc,
    routes: std.ArrayList(RouteInfo),
    modules: std.ArrayList([]const u8), // each entry owned
    functions: std.ArrayList(FunctionEntry),
    env: EnvInfo,
    egress: EgressInfo,
    cache: CacheInfo,
    durable: DurableInfo,
    api: ApiInfo,
    verification: ?VerificationInfo,
    aot: ?AotInfo,

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
        for (self.cache.namespaces.items) |s| {
            allocator.free(s);
        }
        self.cache.namespaces.deinit(allocator);
        self.durable.deinit(allocator);
        self.api.deinit(allocator);
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

    // Binding tracking: maps local slot -> category for call-site analysis
    env_binding_slots: std.ArrayList(u16),
    cache_binding_slots: std.ArrayList(CacheBinding),
    durable_run_binding_slots: std.ArrayList(u16),
    durable_step_binding_slots: std.ArrayList(u16),
    schema_compile_binding_slots: std.ArrayList(u16),
    request_schema_binding_slots: std.ArrayList(u16),
    parse_bearer_binding_slots: std.ArrayList(u16),
    jwt_verify_binding_slots: std.ArrayList(u16),
    router_match_binding_slots: std.ArrayList(u16),

    // Collected data (all strings are duped/owned)
    modules_list: std.ArrayList([]const u8),
    functions_map: std.ArrayList(HandlerContract.FunctionEntry),
    env_literals: std.ArrayList([]const u8),
    env_dynamic: bool,
    egress_hosts: std.ArrayList([]const u8),
    egress_dynamic: bool,
    cache_namespaces: std.ArrayList([]const u8),
    cache_dynamic: bool,
    durable_used: bool,
    durable_key_literals: std.ArrayList([]const u8),
    durable_key_dynamic: bool,
    durable_step_names: std.ArrayList([]const u8),
    api_schemas: std.ArrayList(ApiSchemaInfo),
    api_request_schema_refs: std.ArrayList([]const u8),
    api_request_schema_dynamic: bool,
    api_routes: std.ArrayList(ApiRouteInfo),
    api_bearer_auth: bool,
    api_jwt_auth: bool,
    api_schemas_dynamic: bool,
    api_routes_dynamic: bool,

    const CacheBinding = struct {
        slot: u16,
        has_namespace_arg: bool, // true for cacheGet/Set/Delete/Incr, false for cacheStats
    };

    pub fn init(allocator: std.mem.Allocator, ir_view: IrView, atoms: ?*context.AtomTable) ContractBuilder {
        return .{
            .allocator = allocator,
            .ir_view = ir_view,
            .atoms = atoms,
            .env_binding_slots = .empty,
            .cache_binding_slots = .empty,
            .durable_run_binding_slots = .empty,
            .durable_step_binding_slots = .empty,
            .schema_compile_binding_slots = .empty,
            .request_schema_binding_slots = .empty,
            .parse_bearer_binding_slots = .empty,
            .jwt_verify_binding_slots = .empty,
            .router_match_binding_slots = .empty,
            .modules_list = .empty,
            .functions_map = .empty,
            .env_literals = .empty,
            .env_dynamic = false,
            .egress_hosts = .empty,
            .egress_dynamic = false,
            .cache_namespaces = .empty,
            .cache_dynamic = false,
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
        self.env_binding_slots.deinit(self.allocator);
        self.cache_binding_slots.deinit(self.allocator);
        self.durable_run_binding_slots.deinit(self.allocator);
        self.durable_step_binding_slots.deinit(self.allocator);
        self.schema_compile_binding_slots.deinit(self.allocator);
        self.request_schema_binding_slots.deinit(self.allocator);
        self.parse_bearer_binding_slots.deinit(self.allocator);
        self.jwt_verify_binding_slots.deinit(self.allocator);
        self.router_match_binding_slots.deinit(self.allocator);
        for (self.env_literals.items) |s| self.allocator.free(s);
        self.env_literals.deinit(self.allocator);
        for (self.egress_hosts.items) |s| self.allocator.free(s);
        self.egress_hosts.deinit(self.allocator);
        for (self.cache_namespaces.items) |s| self.allocator.free(s);
        self.cache_namespaces.deinit(self.allocator);
        for (self.durable_key_literals.items) |s| self.allocator.free(s);
        self.durable_key_literals.deinit(self.allocator);
        for (self.durable_step_names.items) |s| self.allocator.free(s);
        self.durable_step_names.deinit(self.allocator);
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
        dispatch: ?*const PatternDispatchTable,
        default_response: bool,
        verification: ?VerificationInfo,
    ) !HandlerContract {
        // Phase 1: Scan imports to discover modules, functions, and binding slots
        try self.scanImports();

        // Phase 2: Scan all call sites for env/fetchSync/cache usage
        try self.scanCallSites();

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

        const contract = HandlerContract{
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
                .dynamic = self.egress_dynamic,
            },
            .cache = .{
                .namespaces = self.cache_namespaces,
                .dynamic = self.cache_dynamic,
            },
            .durable = .{
                .used = self.durable_used,
                .keys = .{
                    .literal = self.durable_key_literals,
                    .dynamic = self.durable_key_dynamic,
                },
                .steps = self.durable_step_names,
            },
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
        };

        // Clear moved lists so deinit() won't double-free
        self.modules_list = .empty;
        self.functions_map = .empty;
        self.env_literals = .empty;
        self.egress_hosts = .empty;
        self.cache_namespaces = .empty;
        self.durable_key_literals = .empty;
        self.durable_step_names = .empty;
        self.api_schemas = .empty;
        self.api_request_schema_refs = .empty;
        self.api_routes = .empty;

        return contract;
    }

    // -----------------------------------------------------------------
    // Phase 1: Import scanning
    // -----------------------------------------------------------------

    fn scanImports(self: *ContractBuilder) !void {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            if (tag != .import_decl) continue;

            const import_decl = self.ir_view.getImportDecl(idx) orelse continue;
            const module_str = self.ir_view.getString(import_decl.module_idx) orelse continue;

            // Only track virtual modules
            const vm = modules_resolver.VirtualModule.fromSpecifier(module_str) orelse continue;

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

                // Track env bindings
                if (vm == .env and std.mem.eql(u8, imported_name, "env")) {
                    try self.env_binding_slots.append(self.allocator, spec.local_binding.slot);
                }

                // Track validate bindings
                if (vm == .validate) {
                    if (std.mem.eql(u8, imported_name, "schemaCompile")) {
                        try self.schema_compile_binding_slots.append(self.allocator, spec.local_binding.slot);
                    }
                    if (std.mem.eql(u8, imported_name, "validateJson") or
                        std.mem.eql(u8, imported_name, "validateObject") or
                        std.mem.eql(u8, imported_name, "coerceJson"))
                    {
                        try self.request_schema_binding_slots.append(self.allocator, spec.local_binding.slot);
                    }
                }

                // Track auth bindings
                if (vm == .auth) {
                    if (std.mem.eql(u8, imported_name, "parseBearer")) {
                        try self.parse_bearer_binding_slots.append(self.allocator, spec.local_binding.slot);
                    }
                    if (std.mem.eql(u8, imported_name, "jwtVerify")) {
                        try self.jwt_verify_binding_slots.append(self.allocator, spec.local_binding.slot);
                    }
                }

                // Track cache bindings (functions with namespace as first arg)
                if (vm == .cache) {
                    const has_ns = std.mem.eql(u8, imported_name, "cacheGet") or
                        std.mem.eql(u8, imported_name, "cacheSet") or
                        std.mem.eql(u8, imported_name, "cacheDelete") or
                        std.mem.eql(u8, imported_name, "cacheIncr");
                    try self.cache_binding_slots.append(self.allocator, .{
                        .slot = spec.local_binding.slot,
                        .has_namespace_arg = has_ns,
                    });
                }

                if (vm == .durable) {
                    if (std.mem.eql(u8, imported_name, "run")) {
                        try self.durable_run_binding_slots.append(self.allocator, spec.local_binding.slot);
                    }
                    if (std.mem.eql(u8, imported_name, "step")) {
                        try self.durable_step_binding_slots.append(self.allocator, spec.local_binding.slot);
                    }
                }

                if (vm == .router and std.mem.eql(u8, imported_name, "routerMatch")) {
                    try self.router_match_binding_slots.append(self.allocator, spec.local_binding.slot);
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
                        continue;
                    }
                }

                // Check for env() calls
                if (self.isEnvBinding(binding.slot)) {
                    try self.extractLiteralArg(call, &self.env_literals, &self.env_dynamic, null);
                    continue;
                }

                // Check for cache calls
                if (self.isCacheBindingWithNamespace(binding.slot)) {
                    try self.extractLiteralArg(call, &self.cache_namespaces, &self.cache_dynamic, null);
                    continue;
                }

                if (self.isDurableRunBinding(binding.slot)) {
                    self.durable_used = true;
                    try self.extractLiteralArg(call, &self.durable_key_literals, &self.durable_key_dynamic, null);
                    continue;
                }

                if (self.isDurableStepBinding(binding.slot)) {
                    self.durable_used = true;
                    try self.extractLiteralArgStatic(call, &self.durable_step_names);
                    continue;
                }

                // Check for schemaCompile() calls
                if (self.isSchemaCompileBinding(binding.slot)) {
                    try self.extractSchemaCompile(call);
                    continue;
                }

                // Track validateJson()/coerceJson()/validateObject() schema refs
                if (self.isRequestSchemaBinding(binding.slot)) {
                    try self.extractLiteralArg(call, &self.api_request_schema_refs, &self.api_request_schema_dynamic, null);
                    continue;
                }

                // Track auth usage
                if (self.isParseBearerBinding(binding.slot)) {
                    self.api_bearer_auth = true;
                    continue;
                }
                if (self.isJwtVerifyBinding(binding.slot)) {
                    self.api_jwt_auth = true;
                    continue;
                }

                // Extract routerMatch() API routes
                if (self.isRouterMatchBinding(binding.slot)) {
                    try self.extractApiRoutesFromCall(call);
                    continue;
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
        if (call.args_count < 1) return;

        const arg_idx = self.ir_view.getListIndex(call.args_start, 0);
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

    fn extractLiteralArgStatic(
        self: *ContractBuilder,
        call: Node.CallExpr,
        target: *std.ArrayList([]const u8),
    ) !void {
        if (call.args_count < 1) return;

        const arg_idx = self.ir_view.getListIndex(call.args_start, 0);
        const arg_tag = self.ir_view.getTag(arg_idx) orelse return;
        if (arg_tag != .lit_string) return;

        const str_idx = self.ir_view.getStringIdx(arg_idx) orelse return;
        const raw = self.ir_view.getString(str_idx) orelse return;
        if (!containsString(target.items, raw)) {
            const duped = try self.allocator.dupe(u8, raw);
            errdefer self.allocator.free(duped);
            try target.append(self.allocator, duped);
        }
    }

    // -----------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------

    fn isEnvBinding(self: *const ContractBuilder, slot: u16) bool {
        return containsSlot(self.env_binding_slots.items, slot);
    }

    fn isCacheBindingWithNamespace(self: *const ContractBuilder, slot: u16) bool {
        for (self.cache_binding_slots.items) |cb| {
            if (cb.slot == slot and cb.has_namespace_arg) return true;
        }
        return false;
    }

    fn isSchemaCompileBinding(self: *const ContractBuilder, slot: u16) bool {
        return containsSlot(self.schema_compile_binding_slots.items, slot);
    }

    fn isDurableRunBinding(self: *const ContractBuilder, slot: u16) bool {
        return containsSlot(self.durable_run_binding_slots.items, slot);
    }

    fn isDurableStepBinding(self: *const ContractBuilder, slot: u16) bool {
        return containsSlot(self.durable_step_binding_slots.items, slot);
    }

    fn isRequestSchemaBinding(self: *const ContractBuilder, slot: u16) bool {
        return containsSlot(self.request_schema_binding_slots.items, slot);
    }

    fn isParseBearerBinding(self: *const ContractBuilder, slot: u16) bool {
        return containsSlot(self.parse_bearer_binding_slots.items, slot);
    }

    fn isJwtVerifyBinding(self: *const ContractBuilder, slot: u16) bool {
        return containsSlot(self.jwt_verify_binding_slots.items, slot);
    }

    fn isRouterMatchBinding(self: *const ContractBuilder, slot: u16) bool {
        return containsSlot(self.router_match_binding_slots.items, slot);
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
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            if (tag != .var_decl) continue;

            const decl = self.ir_view.getVarDecl(idx) orelse continue;
            if (decl.binding.slot != slot) continue;
            if (decl.init == null_node) continue;
            if (self.ir_view.getTag(decl.init)) |init_tag| {
                if (init_tag == .object_literal) return decl.init;
            }
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
                    const func = self.ir_view.getFunction(idx) orelse continue;
                    if (func.name_atom == slot) return idx;
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
            .function_decl, .function_expr, .arrow_function => return node_idx,
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
        try self.scanFunctionNodeForApiFacts(func.body, route);
    }

    fn scanFunctionNodeForApiFacts(self: *ContractBuilder, node_idx: NodeIndex, route: *ApiRouteInfo) !void {
        if (node_idx == null_node) return;

        const tag = self.ir_view.getTag(node_idx) orelse return;
        switch (tag) {
            .program, .block => {
                const block = self.ir_view.getBlock(node_idx) orelse return;
                var i: u16 = 0;
                while (i < block.stmts_count) : (i += 1) {
                    try self.scanFunctionNodeForApiFacts(self.ir_view.getListIndex(block.stmts_start, i), route);
                }
            },
            .expr_stmt, .return_stmt, .throw_stmt => {
                if (self.ir_view.getOptValue(node_idx)) |value_node| {
                    try self.scanFunctionNodeForApiFacts(value_node, route);
                }
            },
            .var_decl => {
                const decl = self.ir_view.getVarDecl(node_idx) orelse return;
                if (decl.init != null_node) try self.scanFunctionNodeForApiFacts(decl.init, route);
            },
            .if_stmt => {
                const if_stmt = self.ir_view.getIfStmt(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(if_stmt.condition, route);
                try self.scanFunctionNodeForApiFacts(if_stmt.then_branch, route);
                try self.scanFunctionNodeForApiFacts(if_stmt.else_branch, route);
            },
            .switch_stmt => {
                const switch_stmt = self.ir_view.getSwitchStmt(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(switch_stmt.discriminant, route);
                var i: u8 = 0;
                while (i < switch_stmt.cases_count) : (i += 1) {
                    try self.scanFunctionNodeForApiFacts(self.ir_view.getListIndex(switch_stmt.cases_start, i), route);
                }
            },
            .case_clause => {
                const case_clause = self.ir_view.getCaseClause(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(case_clause.test_expr, route);
                var i: u16 = 0;
                while (i < case_clause.body_count) : (i += 1) {
                    try self.scanFunctionNodeForApiFacts(self.ir_view.getListIndex(case_clause.body_start, i), route);
                }
            },
            .for_of_stmt, .for_in_stmt => {
                const for_iter = self.ir_view.getForIter(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(for_iter.iterable, route);
                try self.scanFunctionNodeForApiFacts(for_iter.body, route);
            },
            .binary_op => {
                const binary = self.ir_view.getBinary(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(binary.left, route);
                try self.scanFunctionNodeForApiFacts(binary.right, route);
            },
            .unary_op => {
                const unary = self.ir_view.getUnary(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(unary.operand, route);
            },
            .ternary => {
                const ternary = self.ir_view.getTernary(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(ternary.condition, route);
                try self.scanFunctionNodeForApiFacts(ternary.then_branch, route);
                try self.scanFunctionNodeForApiFacts(ternary.else_branch, route);
            },
            .assignment => {
                const assignment = self.ir_view.getAssignment(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(assignment.target, route);
                try self.scanFunctionNodeForApiFacts(assignment.value, route);
            },
            .array_literal => {
                const array = self.ir_view.getArray(node_idx) orelse return;
                var i: u16 = 0;
                while (i < array.elements_count) : (i += 1) {
                    try self.scanFunctionNodeForApiFacts(self.ir_view.getListIndex(array.elements_start, i), route);
                }
            },
            .object_literal => {
                const obj = self.ir_view.getObject(node_idx) orelse return;
                var i: u16 = 0;
                while (i < obj.properties_count) : (i += 1) {
                    try self.scanFunctionNodeForApiFacts(self.ir_view.getListIndex(obj.properties_start, i), route);
                }
            },
            .object_property => {
                const prop = self.ir_view.getProperty(node_idx) orelse return;
                if (prop.is_computed) try self.scanFunctionNodeForApiFacts(prop.key, route);
                try self.scanFunctionNodeForApiFacts(prop.value, route);
            },
            .member_access, .computed_access, .optional_chain => {
                const member = self.ir_view.getMember(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(member.object, route);
                try self.scanFunctionNodeForApiFacts(member.computed, route);
            },
            .call, .method_call, .optional_call => {
                const call = self.ir_view.getCall(node_idx) orelse return;
                const callee_tag = self.ir_view.getTag(call.callee) orelse return;
                if (callee_tag == .identifier) {
                    const binding = self.ir_view.getBinding(call.callee) orelse return;
                    if (self.isRequestSchemaBinding(binding.slot)) {
                        try self.extractLiteralArg(call, &route.request_schema_refs, &route.request_schema_dynamic, null);
                    } else if (self.isParseBearerBinding(binding.slot)) {
                        route.requires_bearer = true;
                    } else if (self.isJwtVerifyBinding(binding.slot)) {
                        route.requires_jwt = true;
                    }
                }
                try self.scanFunctionNodeForApiFacts(call.callee, route);
                var i: u8 = 0;
                while (i < call.args_count) : (i += 1) {
                    try self.scanFunctionNodeForApiFacts(self.ir_view.getListIndex(call.args_start, i), route);
                }
            },
            .match_expr => {
                const match_expr = self.ir_view.getMatchExpr(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(match_expr.discriminant, route);
                var i: u8 = 0;
                while (i < match_expr.arms_count) : (i += 1) {
                    try self.scanFunctionNodeForApiFacts(self.ir_view.getListIndex(match_expr.arms_start, i), route);
                }
            },
            .match_arm => {
                const arm = self.ir_view.getMatchArm(node_idx) orelse return;
                try self.scanFunctionNodeForApiFacts(arm.body, route);
            },
            .function_decl, .function_expr, .arrow_function => return,
            else => {},
        }
    }
};

fn containsString(items: []const []const u8, needle: []const u8) bool {
    for (items) |item| {
        if (std.mem.eql(u8, item, needle)) return true;
    }
    return false;
}

fn containsSlot(items: []const u16, needle: u16) bool {
    for (items) |item| {
        if (item == needle) return true;
    }
    return false;
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
fn extractHost(url: []const u8) []const u8 {
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
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .durable = .{
            .used = false,
            .keys = .{ .literal = .empty, .dynamic = false },
            .steps = .empty,
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
        } else if (std.mem.eql(u8, key, "routes")) {
            try parseRoutes(&parser, allocator, &contract);
        } else if (std.mem.eql(u8, key, "env")) {
            try parseDynamicSection(&parser, allocator, "literal", &contract.env.literal, &contract.env.dynamic);
        } else if (std.mem.eql(u8, key, "egress")) {
            try parseDynamicSection(&parser, allocator, "hosts", &contract.egress.hosts, &contract.egress.dynamic);
        } else if (std.mem.eql(u8, key, "cache")) {
            try parseDynamicSection(&parser, allocator, "namespaces", &contract.cache.namespaces, &contract.cache.dynamic);
        } else if (std.mem.eql(u8, key, "durable")) {
            try parseDurableSection(&parser, allocator, &contract);
        } else if (std.mem.eql(u8, key, "verification")) {
            try parseVerification(&parser, &contract);
        } else {
            // Skip unknown fields
            parser.skipValue();
        }
    }

    return contract;
}

const JsonParser = struct {
    data: []const u8,
    pos: usize = 0,

    fn init(data: []const u8) JsonParser {
        return .{ .data = data };
    }

    fn peek(self: *JsonParser) u8 {
        if (self.pos >= self.data.len) return 0;
        return self.data[self.pos];
    }

    fn advance(self: *JsonParser) u8 {
        if (self.pos >= self.data.len) return 0;
        const c = self.data[self.pos];
        self.pos += 1;
        return c;
    }

    fn consume(self: *JsonParser, expected: u8) bool {
        self.skipWhitespace();
        if (self.pos < self.data.len and self.data[self.pos] == expected) {
            self.pos += 1;
            return true;
        }
        return false;
    }

    fn skipWhitespace(self: *JsonParser) void {
        while (self.pos < self.data.len) {
            switch (self.data[self.pos]) {
                ' ', '\t', '\n', '\r' => self.pos += 1,
                else => break,
            }
        }
    }

    /// Read a JSON string value (including quotes). Returns the unquoted content.
    /// The returned slice points into the source data (zero-copy for non-escaped strings).
    fn readString(self: *JsonParser) ?[]const u8 {
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
    fn readU32(self: *JsonParser) ?u32 {
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
    fn skipValue(self: *JsonParser) void {
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
        } else {
            parser.skipValue();
        }
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
    try writer.print("    \"dynamic\": {s}\n", .{if (contract.egress.dynamic) "true" else "false"});
    try writer.writeAll("  },\n");

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
    try writer.writeAll("]\n");
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
        try writer.writeAll("\n      }");
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

    // aot (optional)
    if (contract.aot) |a| {
        try writer.writeAll("  \"aot\": {\n");
        try writer.print("    \"patternCount\": {d},\n", .{a.pattern_count});
        try writer.print("    \"hasDefault\": {s}\n", .{if (a.has_default) "true" else "false"});
        try writer.writeAll("  }\n");
    } else {
        try writer.writeAll("  \"aot\": null\n");
    }

    try writer.writeAll("}\n");
}

pub fn writeJsonString(writer: anytype, s: []const u8) !void {
    try writer.writeByte('"');
    for (s) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            0x00...0x08, 0x0b...0x0c, 0x0e...0x1f => {
                try writer.print("\\u{x:0>4}", .{@as(u16, c)});
            },
            else => try writer.writeByte(c),
        }
    }
    try writer.writeByte('"');
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

    var original = HandlerContract{
        .handler = .{ .path = path, .line = 10, .column = 5 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = env_lit, .dynamic = true },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .durable = .{
            .used = true,
            .keys = .{ .literal = .empty, .dynamic = true },
            .steps = .empty,
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
    try std.testing.expect(parsed.durable.used);
    try std.testing.expect(parsed.durable.keys.dynamic);
    try std.testing.expect(parsed.verification != null);
    try std.testing.expect(parsed.verification.?.exhaustive_returns);
    try std.testing.expect(!parsed.verification.?.results_safe);
    try std.testing.expect(parsed.verification.?.bytecode_verified);
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
        .durable = .{
            .used = false,
            .keys = .{ .literal = .empty, .dynamic = false },
            .steps = .empty,
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
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"version\": 3") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"handler.ts\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"modules\": []") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"durable\": {") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"api\": {") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"verification\": null") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"aot\": null") != null);
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
        .durable = .{
            .used = true,
            .keys = .{ .literal = durable_keys, .dynamic = false },
            .steps = durable_steps,
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
