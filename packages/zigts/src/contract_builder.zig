//! `ContractBuilder` — walks the IR once and produces a HandlerContract.
//! Final extraction of the handler_contract.zig split. handler_contract.zig
//! re-exports `ContractBuilder` so `handler_contract.ContractBuilder` still
//! resolves for external callers (precompile.zig, ws_consistency.zig,
//! root.zig). The builder's tightly-coupled private helpers
//! (containsApiParam, responseVariantMatches, schemaSpecFromCandidate,
//! ParsedRouteKey, contentTypeFor, currentPolicyHashRaw, etc.) move with
//! it; only `extractHost` and `containsString` stay in handler_contract.zig
//! because they're referenced by other split files and external callers.

const std = @import("std");
const handler_contract = @import("handler_contract.zig");
const contract_types = @import("contract_types.zig");
const api_schema = @import("api_schema.zig");
const json_utils = @import("json_utils.zig");
const ir = @import("parser/ir.zig");
const object = @import("object.zig");
const context = @import("context.zig");
const module_binding = @import("module_binding.zig");
const builtin_modules = @import("builtin_modules.zig");
const manifest_registry_mod = @import("manifest_registry.zig");
const module_manifest = @import("module_manifest.zig");
const bytecode = @import("bytecode.zig");
const handler_analyzer = @import("handler_analyzer.zig");
const type_checker_mod = @import("type_checker.zig");
const type_env_mod = @import("type_env.zig");
const type_pool_mod = @import("type_pool.zig");
const rule_registry = @import("rule_registry.zig");
const spec_discharge = @import("spec_discharge.zig");
const intent_extractor = @import("intent_extractor.zig");
const effect_inference = @import("effect_inference.zig");
const function_specs = @import("function_specs.zig");
const JsParser = @import("parser/root.zig").JsParser;

const Node = ir.Node;
const NodeIndex = ir.NodeIndex;
const NodeTag = ir.NodeTag;
const IrView = ir.IrView;
const null_node = ir.null_node;
const HandlerPattern = bytecode.HandlerPattern;
const PatternDispatchTable = bytecode.PatternDispatchTable;
const TypeChecker = type_checker_mod.TypeChecker;
const TypeEnv = type_env_mod.TypeEnv;

const HandlerContract = contract_types.HandlerContract;
const RouteInfo = contract_types.RouteInfo;
const SqlQueryInfo = contract_types.SqlQueryInfo;
const DurableWorkflow = contract_types.DurableWorkflow;
const DurableWorkflowNodeKind = contract_types.DurableWorkflowNodeKind;
const WebSocketInfo = contract_types.WebSocketInfo;
const ApiSchemaInfo = contract_types.ApiSchemaInfo;
const ApiParamInfo = contract_types.ApiParamInfo;
const SchemaSpec = contract_types.SchemaSpec;
const ApiBodyInfo = contract_types.ApiBodyInfo;
const ApiResponseInfo = contract_types.ApiResponseInfo;
const ApiRouteInfo = contract_types.ApiRouteInfo;
const VerificationInfo = contract_types.VerificationInfo;
const AotInfo = contract_types.AotInfo;
const FaultCoverageInfo = contract_types.FaultCoverageInfo;
const HandlerProperties = contract_types.HandlerProperties;
const RateLimitInfo = contract_types.RateLimitInfo;
const ServiceCallInfo = contract_types.ServiceCallInfo;
const computeCapabilityMatrix = contract_types.computeCapabilityMatrix;

const containsString = json_utils.containsString;
const writeJsonString = json_utils.writeJsonString;
const extractHost = handler_contract.extractHost;

fn currentPolicyHashRaw() [32]u8 {
    const hex = rule_registry.policyHash();
    var out: [32]u8 = undefined;
    _ = std.fmt.hexToBytes(&out, &hex) catch unreachable;
    return out;
}

/// Builds a HandlerContract by walking the IR once.
/// All strings stored in the contract are duped via the allocator, making
/// the contract safe to use after the parser and atom table are freed.
pub const ContractBuilder = struct {
    allocator: std.mem.Allocator,
    ir_view: IrView,
    atoms: ?*context.AtomTable,
    type_env: ?*const TypeEnv,
    type_checker: ?*const TypeChecker,
    /// Partner virtual-module manifests registered for this compile session.
    /// Borrowed; the registry must outlive the builder.
    manifest_registry: ?*const manifest_registry_mod.Registry = null,

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

    // Partner extension tracking: bindings discovered in scanImports for
    // partner-registered modules, and the per-specifier extracted facts.
    extension_bindings: std.ArrayList(ExtensionBinding) = .empty,
    extensions: std.StringHashMapUnmanaged(contract_types.ExtensionContract) = .empty,

    // Effect tracking
    has_nondeterministic_builtin: bool = false,
    /// First call site that broke determinism. Captured for the live-reload
    /// HUD's "Why" line. Borrowed snippet (static string).
    nondeterministic_cause: ?contract_types.PropertyCause = null,

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

    /// Like GenericBinding but for partner-registered modules. The extraction
    /// rules borrow from the live ManifestRegistry (which outlives the
    /// builder), so no extra storage is allocated here.
    const ExtensionBinding = struct {
        slot: u16,
        module_specifier: []const u8,
        binding_name: []const u8,
        extractions: []const module_manifest.ContractExtractionRule,
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
        self.extension_bindings.deinit(self.allocator);
        var ext_it = self.extensions.iterator();
        while (ext_it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.deinit(self.allocator);
        }
        self.extensions.deinit(self.allocator);
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
        root: NodeIndex,
        dispatch: ?*const PatternDispatchTable,
        default_response: bool,
        verification: ?VerificationInfo,
    ) !HandlerContract {
        // Proof-carrying functions: infer per-function effect rows across the
        // call graph. The handler's composed row tightens computeProperties;
        // the full table drives capsule discharge after the contract is built.
        var effects = effect_inference.Analyzer.initWithManifestRegistry(
            self.allocator,
            self.ir_view,
            self.atoms,
            self.manifest_registry,
        );
        defer effects.deinit();
        try effects.analyze(root);

        // Phase 1: Scan imports to discover modules, functions, and binding slots
        try self.scanImports();

        // Phase 2: Scan all call sites for env/fetchSync/cache usage
        try self.scanCallSites();

        // Phase 2b: Scan top-level function declarations for WebSocket event
        // exports (onOpen/onMessage/onClose/onError).
        try self.scanWebSocketExports();

        // Phase 2c: Run module-level WebSocket consistency checks
        // (ZTS320, ZTS321). Warnings go to stderr so `zig build` users
        // see them in the build log; errors do not abort the contract
        // build — the downstream verifier owns the hard-fail policy.
        try self.emitWebSocketConsistencyDiagnostics();

        // Author-declared intent assertions. Extraction is strict and
        // fails to `intent.dynamic = true` on any non-literal form rather
        // than degrading, preserving the deterministic-extraction line.
        const intent_value = try self.extractIntentAssertions();

        if (handler_fn) |hf| {
            try self.extractScopeUsage(hf);
            try self.extractDurableWorkflow(handler_path, hf);
        }

        // Phase 3: Compute handler effect properties.
        const properties = try self.computeProperties(effects.lookup("handler"), handler_fn);

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
            .intent = intent_value,
            .property_provenance = .{
                .deterministic = self.nondeterministic_cause,
            },
            .extensions = self.extensions,
        };

        contract.capabilities = computeCapabilityMatrix(contract.modules.items);
        contract.policy_hash = currentPolicyHashRaw();

        // Phase 4: Resolve active specs. An explicit handler Spec<...>
        // narrows the set; otherwise every supported v1 spec is active.
        // Downstream surfaces read declared_specs as the mandatory active
        // set when discharging ZTS500/ZTS501/ZTS502.
        try self.populateDeclaredSpecs(&contract, handler_loc);

        // Phase 4b: Proof-carrying functions. Discharge each helper's
        // `Proof<...>` capsule, append helper ZTS500/ZTS502 and caller-demand
        // ZTS606 to spec_diagnostics, and project a CapsuleSummary per helper.
        try self.dischargeFunctionCapsules(&contract, &effects);

        // Phase 4c: handler capability budget. Discharge the handler's
        // `Effects<...>` budget against its inferred capability row and
        // attribute every over-budget capability (ZTS506 / ZTS607).
        try self.dischargeCapabilityBudget(&contract, &effects, handler_loc);

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
        self.extensions = .empty;

        return contract;
    }

    // -----------------------------------------------------------------
    // Phase 4: Author-declared spec extraction
    // -----------------------------------------------------------------

    /// Populate `contract.declared_specs` with the active handler spec set.
    /// An explicit `Response & Spec<...>` (or alias-hop equivalent) narrows
    /// the active set to the declared names. When the handler declares no
    /// `Spec<...>`, every supported v1 spec is active by default.
    fn populateDeclaredSpecs(
        self: *ContractBuilder,
        contract: *HandlerContract,
        handler_loc: ?ir.SourceLocation,
    ) !void {
        var raw_names: std.ArrayListUnmanaged([]const u8) = .empty;
        defer raw_names.deinit(self.allocator);

        if (self.type_env) |env| {
            if (handler_loc) |loc| {
                if (loc.line != 0) {
                    if (env.getFnSigByLoc(loc.line)) |sig| {
                        if (sig.return_type != type_pool_mod.null_type_idx) {
                            env.extractSpecMembers(sig.return_type, &raw_names);
                        }
                    }
                }
            }
        }

        // Owned string copies; sorted for stable contract.json output and
        // de-duplicated to keep ledger entries idempotent across saves.
        var owned: std.ArrayList([]const u8) = .empty;
        errdefer {
            for (owned.items) |s| self.allocator.free(s);
            owned.deinit(self.allocator);
        }

        const declared_specs_implicit = raw_names.items.len == 0;
        const active_names: []const []const u8 = if (declared_specs_implicit)
            &spec_discharge.v1_spec_names
        else
            raw_names.items;

        outer: for (active_names) |raw| {
            for (owned.items) |existing| {
                if (std.mem.eql(u8, existing, raw)) continue :outer;
            }
            const dup = try self.allocator.dupe(u8, raw);
            errdefer self.allocator.free(dup);
            try owned.append(self.allocator, dup);
        }

        std.mem.sort([]const u8, owned.items, {}, struct {
            fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
                return std.mem.order(u8, lhs, rhs) == .lt;
            }
        }.lessThan);

        contract.declared_specs = owned;
        contract.declared_specs_implicit = declared_specs_implicit;

        // Discharge: produce ZTS500 / ZTS501 / ZTS502 diagnostics into
        // contract.spec_diagnostics. Downstream consumers (zigts check,
        // proof HUD, ledger) read from there. The verifier does not see
        // these because it runs before the contract is built.
        contract.spec_diagnostics = try spec_discharge.dischargeSpecs(
            self.allocator,
            contract.declared_specs.items,
            contract.properties,
            contract.modules.items,
            declared_specs_implicit,
        );
    }

    /// Proof-carrying functions: discharge every helper's `Proof<...>`
    /// capsule against the facts effect inference proved, append the
    /// resulting diagnostics to `contract.spec_diagnostics`, and project a
    /// `CapsuleSummary` per helper into `contract.function_capsules`.
    ///
    /// Two diagnostic kinds are produced. A helper that declared a capsule
    /// property it does not satisfy gets ZTS500 / ZTS502 (its own failure).
    /// A helper that breaks a capsule property the handler's `Spec` demands,
    /// while carrying no capsule declaring that property, gets ZTS606: the
    /// handler's proof cannot compose across that call boundary.
    fn dischargeFunctionCapsules(
        self: *ContractBuilder,
        contract: *HandlerContract,
        analyzer: *const effect_inference.Analyzer,
    ) !void {
        var table = try function_specs.discharge(
            self.allocator,
            analyzer,
            self.type_env,
            self.ir_view,
        );
        defer table.deinit(self.allocator);

        // Capsule properties the handler's Spec demands: the intersection of
        // the declared spec set with the v1 capsule property set.
        var handler_demands = std.EnumSet(spec_discharge.CapsuleProperty).initEmpty();
        for (contract.declared_specs.items) |name| {
            if (spec_discharge.CapsuleProperty.fromName(name)) |p| handler_demands.insert(p);
        }

        const reachable = try handlerReachability(self.allocator, analyzer);
        defer if (reachable) |r| self.allocator.free(r);

        for (table.capsules.items) |*cap| {
            const reachable_from_handler = helperReachable(analyzer, reachable, cap.name);
            // The helper's own capsule discharge (ZTS500 / ZTS502),
            // attributed to the helper.
            for (cap.diagnostics.items) |d| {
                var copy = try d.clone(self.allocator);
                errdefer copy.deinit(self.allocator);
                if (copy.function == null) copy.function = try self.allocator.dupe(u8, cap.name);
                try contract.spec_diagnostics.append(self.allocator, copy);
            }

            // The helper's own `Effects<...>` ceiling discharge
            // (ZTS503 / ZTS504 / ZTS505), attributed to the helper.
            for (cap.effect_diagnostics.items) |d| {
                var copy = try d.clone(self.allocator);
                errdefer copy.deinit(self.allocator);
                if (copy.function == null) copy.function = try self.allocator.dupe(u8, cap.name);
                try contract.spec_diagnostics.append(self.allocator, copy);
            }

            // ZTS606: the handler demands a capsule property this helper
            // breaks, and the helper carries no capsule declaring it. A
            // helper that declared the property already owns a ZTS500, so
            // skipping declared properties avoids double-reporting.
            if (reachable_from_handler) {
                inline for (.{
                    spec_discharge.CapsuleProperty.total,
                    spec_discharge.CapsuleProperty.pure,
                    spec_discharge.CapsuleProperty.read_only,
                    spec_discharge.CapsuleProperty.deterministic,
                }) |prop| {
                    const pname = @tagName(prop);
                    if (handler_demands.contains(prop) and
                        !cap.proven.holds(prop) and
                        !containsString(cap.declared.items, pname))
                    {
                        try contract.spec_diagnostics.append(
                            self.allocator,
                            try makeMissingCapsule(self.allocator, pname, cap.name),
                        );
                    }
                }
            }

            var summary = contract_types.CapsuleSummary{
                .function = try self.allocator.dupe(u8, cap.name),
                .line = cap.line,
                .proven_total = cap.proven.total,
                .proven_pure = cap.proven.pure,
                .proven_read_only = cap.proven.read_only,
                .proven_deterministic = cap.proven.deterministic,
                .discharged = cap.discharged(),
                .exported = cap.exported,
                .handler_reachable = reachable_from_handler,
            };
            errdefer summary.deinit(self.allocator);
            for (cap.declared.items) |name| {
                try summary.declared.append(self.allocator, try self.allocator.dupe(u8, name));
            }
            try contract.function_capsules.append(self.allocator, summary);

            var effect_summary = contract_types.EffectCapsuleSummary{
                .function = try self.allocator.dupe(u8, cap.name),
                .line = cap.line,
                .discharged = cap.effectDischarged(),
                .exported = cap.exported,
                .handler_reachable = reachable_from_handler,
            };
            errdefer effect_summary.deinit(self.allocator);
            for (cap.effect_declared.items) |name| {
                try effect_summary.declared.append(self.allocator, try self.allocator.dupe(u8, name));
            }
            var caps_it = cap.inferred_caps.iterator();
            while (caps_it.next()) |c| {
                try effect_summary.inferred.append(
                    self.allocator,
                    try self.allocator.dupe(u8, @tagName(c)),
                );
            }
            try contract.function_effect_capsules.append(self.allocator, effect_summary);
        }
    }

    fn handlerReachability(
        allocator: std.mem.Allocator,
        analyzer: *const effect_inference.Analyzer,
    ) !?[]bool {
        const functions = analyzer.all();
        const h = findFunctionIndex(functions, "handler") orelse return null;
        var reachable = try allocator.alloc(bool, functions.len);
        errdefer allocator.free(reachable);
        @memset(reachable, false);

        var stack: std.ArrayListUnmanaged(usize) = .empty;
        defer stack.deinit(allocator);
        reachable[h] = true;
        try stack.append(allocator, h);
        while (stack.pop()) |cur| {
            for (analyzer.calleesOf(cur)) |callee| {
                if (callee < reachable.len and !reachable[callee]) {
                    reachable[callee] = true;
                    try stack.append(allocator, callee);
                }
            }
        }
        return reachable;
    }

    fn helperReachable(
        analyzer: *const effect_inference.Analyzer,
        reachable: ?[]const bool,
        name: []const u8,
    ) bool {
        const r = reachable orelse return false;
        if (findFunctionIndex(analyzer.all(), name)) |idx| {
            return idx < r.len and r[idx];
        }
        return false;
    }

    fn findFunctionIndex(functions: []const effect_inference.FunctionEffect, name: []const u8) ?usize {
        for (functions, 0..) |fe, i| {
            if (std.mem.eql(u8, fe.name, name)) return i;
        }
        return null;
    }

    /// Build a ZTS606 diagnostic for a helper that breaks a handler-demanded
    /// capsule property without declaring a capsule for it.
    fn makeMissingCapsule(
        allocator: std.mem.Allocator,
        property: []const u8,
        func: []const u8,
    ) !contract_types.SpecDiagnostic {
        const spec_name = try allocator.dupe(u8, property);
        errdefer allocator.free(spec_name);
        const owned_func = try allocator.dupe(u8, func);
        errdefer allocator.free(owned_func);
        const suggestion = try std.fmt.allocPrint(
            allocator,
            "annotate `{s}` with `Proof<T, \"{s}\">` and satisfy it, or inline the helper into the handler.",
            .{ func, property },
        );
        return .{
            .kind = .missing_capsule,
            .spec_name = spec_name,
            .suggestion = suggestion,
            .function = owned_func,
        };
    }

    /// Phase 4c: discharge the handler's capability budget. The handler
    /// declares its budget with an `Effects<...>` on its return type, exactly
    /// as a helper declares a ceiling - but the handler's budget also bounds
    /// every helper it reaches. The budget is checked `inferred ⊆ declared`.
    /// A capability the handler reaches directly and outside the budget gets
    /// ZTS506; one a reachable helper introduces gets ZTS607, attributed to
    /// that helper. The check runs only against inferred facts - no capability
    /// is ever assumed.
    fn dischargeCapabilityBudget(
        self: *ContractBuilder,
        contract: *HandlerContract,
        analyzer: *const effect_inference.Analyzer,
        handler_loc: ?ir.SourceLocation,
    ) !void {
        const env = self.type_env orelse return;
        const loc = handler_loc orelse return;
        if (loc.line == 0) return;

        const sig = env.getFnSigByLoc(loc.line) orelse return;
        if (sig.return_type == type_pool_mod.null_type_idx) return;

        var raw: std.ArrayListUnmanaged([]const u8) = .empty;
        defer raw.deinit(self.allocator);
        env.extractEffectMembers(sig.return_type, &raw);
        if (raw.items.len == 0) return; // no budget declared, no check

        // Parse the budget. An unknown name is the handler-level analogue of
        // ZTS504; it is reported and dropped from the budget set.
        var budget = effect_inference.CapabilitySet.initEmpty();
        for (raw.items) |name| {
            if (std.meta.stringToEnum(module_binding.ModuleCapability, name)) |cap| {
                budget.insert(cap);
            } else {
                const spec_name = try self.allocator.dupe(u8, name);
                errdefer self.allocator.free(spec_name);
                const suggestion = try spec_discharge.suggestionForUnknownCapability(self.allocator, name);
                errdefer if (suggestion) |s| self.allocator.free(s);
                try contract.spec_diagnostics.append(self.allocator, .{
                    .kind = .effect_unknown_capability,
                    .spec_name = spec_name,
                    .suggestion = suggestion,
                });
            }
        }

        // Record the declared budget (valid names only) so the json writer
        // can serialise `sandbox.declaredBudget`.
        contract.capability_budget = capabilityMatrixFromSet(budget);

        // An all-invalid budget: the ZTS504s above are the actionable errors;
        // do not cascade ZTS506 / ZTS607 against an empty budget.
        if (budget.count() == 0) return;

        // Locate the handler's FunctionEffect and index.
        const functions = analyzer.all();
        const h = findFunctionIndex(functions, "handler") orelse return;
        const hfe = functions[h];

        // Reachability: the budget bounds only helpers the handler can reach.
        var reachable = try self.allocator.alloc(bool, functions.len);
        defer self.allocator.free(reachable);
        @memset(reachable, false);
        var stack: std.ArrayListUnmanaged(usize) = .empty;
        defer stack.deinit(self.allocator);
        reachable[h] = true;
        try stack.append(self.allocator, h);
        while (stack.pop()) |cur| {
            for (analyzer.calleesOf(cur)) |callee| {
                if (callee < reachable.len and !reachable[callee]) {
                    reachable[callee] = true;
                    try stack.append(self.allocator, callee);
                }
            }
        }

        // Every capability in the handler's transitive row that the budget
        // omits is a violation, attributed to its direct source.
        var cap_it = hfe.row.capabilities.iterator();
        while (cap_it.next()) |cap| {
            if (budget.contains(cap)) continue;
            if (hfe.direct_caps.contains(cap)) {
                try contract.spec_diagnostics.append(
                    self.allocator,
                    try makeBudgetExceeded(self.allocator, @tagName(cap)),
                );
            }
            for (functions, 0..) |fe, i| {
                if (i == h) continue;
                if (!reachable[i]) continue;
                if (!fe.direct_caps.contains(cap)) continue;
                try contract.spec_diagnostics.append(
                    self.allocator,
                    try makeHelperBudgetExceeded(self.allocator, @tagName(cap), fe.name),
                );
            }
        }
    }

    /// Project a capability set into the contract's `CapabilityMatrix` shape,
    /// in canonical enum order with a stable hash.
    fn capabilityMatrixFromSet(set: effect_inference.CapabilitySet) contract_types.CapabilityMatrix {
        var matrix: contract_types.CapabilityMatrix = .{};
        var n: u8 = 0;
        for (std.enums.values(module_binding.ModuleCapability)) |c| {
            if (set.contains(c)) {
                matrix.items[n] = c;
                n += 1;
            }
        }
        matrix.len = n;
        matrix.hash = module_binding.capabilityHash(matrix.slice());
        return matrix;
    }

    /// Build a ZTS506 diagnostic: the handler reaches a capability directly
    /// that is outside its declared `Effects<...>` budget.
    fn makeBudgetExceeded(
        allocator: std.mem.Allocator,
        capability: []const u8,
    ) !contract_types.SpecDiagnostic {
        const spec_name = try allocator.dupe(u8, capability);
        errdefer allocator.free(spec_name);
        const suggestion = try std.fmt.allocPrint(
            allocator,
            "add `{s}` to the handler's `Effects<...>` budget, or remove the call that reaches it.",
            .{capability},
        );
        return .{
            .kind = .budget_exceeded,
            .spec_name = spec_name,
            .suggestion = suggestion,
        };
    }

    /// Build a ZTS607 diagnostic: a handler-reachable helper reaches a
    /// capability outside the handler's declared `Effects<...>` budget.
    fn makeHelperBudgetExceeded(
        allocator: std.mem.Allocator,
        capability: []const u8,
        func: []const u8,
    ) !contract_types.SpecDiagnostic {
        const spec_name = try allocator.dupe(u8, capability);
        errdefer allocator.free(spec_name);
        const owned_func = try allocator.dupe(u8, func);
        errdefer allocator.free(owned_func);
        const suggestion = try std.fmt.allocPrint(
            allocator,
            "helper `{s}` reaches `{s}`; add it to the handler's `Effects<...>` budget, or remove the call.",
            .{ func, capability },
        );
        return .{
            .kind = .helper_budget_exceeded,
            .spec_name = spec_name,
            .suggestion = suggestion,
            .function = owned_func,
        };
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

    fn emitWebSocketConsistencyDiagnostics(self: *ContractBuilder) !void {
        const ws_consistency = @import("ws_consistency.zig");
        const inputs = ws_consistency.Inputs{
            .imports_websocket_module = containsString(self.modules_list.items, "zigttp:websocket"),
            .exports_on_open = self.websocket.on_open,
            .exports_on_message = self.websocket.on_message,
            .exports_on_close = self.websocket.on_close,
            .exports_on_error = self.websocket.on_error,
        };

        var findings: std.ArrayList(ws_consistency.Finding) = .empty;
        defer findings.deinit(self.allocator);
        try ws_consistency.check(self.allocator, inputs, &findings);

        for (findings.items) |f| {
            const code: []const u8 = switch (f.kind) {
                .websocket_import_without_events => "ZTS320",
                .websocket_events_without_import => "ZTS321",
                else => "ZTS???",
            };
            std.log.warn("{s} [{s}]: {s} ({s})", .{
                f.severity.label(),
                code,
                f.message,
                f.help,
            });
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

            // Only track virtual modules: either built-in or partner-registered.
            if (builtin_modules.fromSpecifier(module_str) == null) {
                if (self.manifest_registry == null or self.manifest_registry.?.fromSpecifier(module_str) == null) continue;
            }

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
                } else if (self.manifest_registry) |registry| {
                    if (registry.findExport(module_str, imported_name)) |partner_exp| {
                        if (partner_exp.contract_extractions.items.len > 0) {
                            try self.extension_bindings.append(self.allocator, .{
                                .slot = spec.local_binding.slot,
                                .module_specifier = registry.fromSpecifier(module_str).?.specifier,
                                .binding_name = imported_name,
                                .extractions = partner_exp.contract_extractions.items,
                            });
                        }
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
                        try self.extractLiteralArg(call, .{ .list = &self.egress_hosts, .dynamic = &self.egress_dynamic }, &extractHost);
                        try self.extractLiteralArg(call, .{ .list = &self.egress_urls, .dynamic = &self.egress_dynamic }, null);
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
                                    try self.extractLiteralArgAt(call, ext.arg_position, target, transform);
                                }
                            },
                        }
                    }
                    break;
                }

                // Partner extractions route literals into the per-specifier
                // extensions map. `fetch_host` rules also mirror into the
                // top-level egress.hosts so runtime egress policy enforcement
                // sees one uniform list.
                for (self.extension_bindings.items) |eb| {
                    if (eb.slot != binding.slot) continue;
                    for (eb.extractions) |rule| {
                        try self.applyExtensionExtraction(call, eb.module_specifier, rule);
                    }
                    break;
                }
            }

            // Detect nondeterministic builtins: Date.now(), Math.random()
            if (!self.has_nondeterministic_builtin and callee_tag == .member_access and !self.isInsideDurableStepCallback(idx)) {
                const member = self.ir_view.getMember(call.callee) orelse continue;
                const obj_tag = self.ir_view.getTag(member.object) orelse continue;
                if (obj_tag == .identifier) {
                    const binding = self.ir_view.getBinding(member.object) orelse continue;
                    if (binding.kind == .global or binding.kind == .undeclared_global) {
                        const obj_name = self.resolveAtomName(binding.slot) orelse continue;
                        const prop_name = self.resolveAtomName(member.property) orelse continue;
                        const is_date_now = std.mem.eql(u8, obj_name, "Date") and std.mem.eql(u8, prop_name, "now");
                        const is_math_random = std.mem.eql(u8, obj_name, "Math") and std.mem.eql(u8, prop_name, "random");
                        if (is_date_now or is_math_random) {
                            self.has_nondeterministic_builtin = true;
                            // Capture the first call site so the HUD can print
                            // "-deterministic at handler.ts:N: Date.now()".
                            const loc = self.ir_view.getLoc(call.callee) orelse self.ir_view.getLoc(member.object);
                            if (loc) |l| {
                                self.nondeterministic_cause = .{
                                    .line = l.line,
                                    .column = l.column,
                                    .snippet = if (is_date_now) "Date.now()" else "Math.random()",
                                };
                            }
                        }
                    }
                }
            }
        }
    }

    fn isInsideDurableStepCallback(self: *ContractBuilder, target: NodeIndex) bool {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            if (self.ir_view.getTag(idx) != .call) continue;
            const call = self.ir_view.getCall(idx) orelse continue;
            if (!self.isDurableStepCall(call) or call.args_count < 2) continue;
            const callback = self.ir_view.getListIndex(call.args_start, 1);
            if (!self.isFunctionNode(callback)) continue;
            if (self.subtreeContains(callback, target)) return true;
        }
        return false;
    }

    fn isDurableStepCall(self: *ContractBuilder, call: Node.CallExpr) bool {
        if (self.ir_view.getTag(call.callee) != .identifier) return false;
        const binding = self.ir_view.getBinding(call.callee) orelse return false;
        for (self.generic_bindings.items) |gb| {
            if (gb.slot != binding.slot) continue;
            return std.mem.eql(u8, gb.module_specifier, "zigttp:durable") and
                std.mem.eql(u8, gb.binding_name, "step");
        }
        return false;
    }

    fn isFunctionNode(self: *const ContractBuilder, node: NodeIndex) bool {
        const tag = self.ir_view.getTag(node) orelse return false;
        return tag == .function_decl or tag == .function_expr or tag == .arrow_function;
    }

    fn subtreeContains(self: *ContractBuilder, root: NodeIndex, target: NodeIndex) bool {
        if (root == null_node) return false;
        if (root == target) return true;
        const tag = self.ir_view.getTag(root) orelse return false;
        switch (tag) {
            .program, .block => {
                const block = self.ir_view.getBlock(root) orelse return false;
                for (0..block.stmts_count) |i| {
                    if (self.subtreeContains(self.ir_view.getListIndex(block.stmts_start, @intCast(i)), target)) return true;
                }
            },
            .return_stmt, .expr_stmt => {
                if (self.ir_view.getOptValue(root)) |value| return self.subtreeContains(value, target);
            },
            .var_decl => {
                const decl = self.ir_view.getVarDecl(root) orelse return false;
                return self.subtreeContains(decl.pattern, target) or self.subtreeContains(decl.init, target);
            },
            .if_stmt => {
                const stmt = self.ir_view.getIfStmt(root) orelse return false;
                return self.subtreeContains(stmt.condition, target) or
                    self.subtreeContains(stmt.then_branch, target) or
                    self.subtreeContains(stmt.else_branch, target);
            },
            .for_of_stmt => {
                const stmt = self.ir_view.getForIter(root) orelse return false;
                return self.subtreeContains(stmt.pattern, target) or
                    self.subtreeContains(stmt.iterable, target) or
                    self.subtreeContains(stmt.body, target);
            },
            .binary_op => {
                const expr = self.ir_view.getBinary(root) orelse return false;
                return self.subtreeContains(expr.left, target) or self.subtreeContains(expr.right, target);
            },
            .unary_op, .spread => {
                const expr = self.ir_view.getUnary(root) orelse return false;
                return self.subtreeContains(expr.operand, target);
            },
            .ternary => {
                const expr = self.ir_view.getTernary(root) orelse return false;
                return self.subtreeContains(expr.condition, target) or
                    self.subtreeContains(expr.then_branch, target) or
                    self.subtreeContains(expr.else_branch, target);
            },
            .call, .method_call => {
                const call = self.ir_view.getCall(root) orelse return false;
                if (self.subtreeContains(call.callee, target)) return true;
                for (0..call.args_count) |i| {
                    if (self.subtreeContains(self.ir_view.getListIndex(call.args_start, @intCast(i)), target)) return true;
                }
            },
            .member_access, .optional_chain, .computed_access => {
                const member = self.ir_view.getMember(root) orelse return false;
                return self.subtreeContains(member.object, target) or self.subtreeContains(member.computed, target);
            },
            .assignment => {
                const assign = self.ir_view.getAssignment(root) orelse return false;
                return self.subtreeContains(assign.target, target) or self.subtreeContains(assign.value, target);
            },
            .array_literal => {
                const arr = self.ir_view.getArray(root) orelse return false;
                for (0..arr.elements_count) |i| {
                    if (self.subtreeContains(self.ir_view.getListIndex(arr.elements_start, @intCast(i)), target)) return true;
                }
            },
            .object_literal => {
                const obj = self.ir_view.getObject(root) orelse return false;
                for (0..obj.properties_count) |i| {
                    const prop_idx = self.ir_view.getListIndex(obj.properties_start, @intCast(i));
                    if (self.subtreeContains(prop_idx, target)) return true;
                }
            },
            .object_property => {
                const prop = self.ir_view.getProperty(root) orelse return false;
                return self.subtreeContains(prop.key, target) or self.subtreeContains(prop.value, target);
            },
            .function_decl, .function_expr, .arrow_function => {
                const func = self.ir_view.getFunction(root) orelse return false;
                return self.subtreeContains(func.body, target);
            },
            .template_literal => {
                const tmpl = self.ir_view.getTemplate(root) orelse return false;
                for (0..tmpl.parts_count) |i| {
                    if (self.subtreeContains(self.ir_view.getListIndex(tmpl.parts_start, @intCast(i)), target)) return true;
                }
            },
            .match_expr => {
                const match = self.ir_view.getMatchExpr(root) orelse return false;
                if (self.subtreeContains(match.discriminant, target)) return true;
                for (0..match.arms_count) |i| {
                    if (self.subtreeContains(self.ir_view.getListIndex(match.arms_start, @intCast(i)), target)) return true;
                }
            },
            .match_arm => {
                const arm = self.ir_view.getMatchArm(root) orelse return false;
                return self.subtreeContains(arm.pattern, target) or self.subtreeContains(arm.body, target);
            },
            else => {},
        }
        return false;
    }

    /// Extract a literal string from the first argument of a call and append
    /// it (deduped, owned) to `target`. If the argument is non-literal, set
    /// `dynamic_flag`. An optional `transform` narrows the extracted string
    /// (e.g. extractHost for URLs) - returning empty means "treat as dynamic".
    fn extractLiteralArg(
        self: *ContractBuilder,
        call: Node.CallExpr,
        target: CategoryTarget,
        transform: ?*const fn ([]const u8) []const u8,
    ) !void {
        try self.extractLiteralArgAt(call, 0, target, transform);
    }

    /// Apply one partner-declared extraction rule to a partner-module call
    /// site. Routes a literal argument into the per-specifier extensions
    /// store. `fetch_host` rules additionally mirror the host into the
    /// top-level `egress_hosts` so runtime egress policy sees one uniform
    /// list of allowed hosts.
    fn applyExtensionExtraction(
        self: *ContractBuilder,
        call: Node.CallExpr,
        specifier: []const u8,
        rule: module_manifest.ContractExtractionRule,
    ) !void {
        const transform: ?*const fn ([]const u8) []const u8 =
            if (rule.transform) |t| switch (t) {
                .extract_host => &extractHost,
                .identity => null,
            } else null;

        const bucket = try self.getOrCreateExtensionEntry(specifier);

        switch (rule.category) {
            .fetch_host => {
                // Per-extension copy AND top-level mirror.
                try self.extractLiteralArgAt(call, rule.arg_position, .{ .list = &bucket.egress_hosts, .dynamic = &bucket.egress_dynamic }, transform);
                try self.extractLiteralArgAt(call, rule.arg_position, .{ .list = &self.egress_hosts, .dynamic = &self.egress_dynamic }, transform);
                // Mirror the full URL into egress_urls as well (matching the bare
                // fetchSync path), so system_linker can resolve cross-handler
                // internal calls made through the `zigttp:fetch` module import -
                // otherwise "every internal fetch matches a declared route"
                // holds vacuously for module-import callers.
                try self.extractLiteralArgAt(call, rule.arg_position, .{ .list = &self.egress_urls, .dynamic = &self.egress_dynamic }, null);
            },
            .extension_specific => {
                const tag = rule.extension_category orelse return;
                const cat_bucket = try self.getOrCreateCategoryBucket(bucket, tag);
                try self.extractLiteralArgAt(call, rule.arg_position, .{ .list = &cat_bucket.literals, .dynamic = &cat_bucket.dynamic }, transform);
            },
            else => {
                // Other built-in categories from partners route under a
                // category key matching the variant tag name. This keeps the
                // section uniform and lets partners reuse names like
                // `cache_namespace` for proof of cache use without rerouting
                // through built-in policy.
                const tag = @tagName(rule.category);
                const cat_bucket = try self.getOrCreateCategoryBucket(bucket, tag);
                try self.extractLiteralArgAt(call, rule.arg_position, .{ .list = &cat_bucket.literals, .dynamic = &cat_bucket.dynamic }, transform);
            },
        }
    }

    fn getOrCreateExtensionEntry(
        self: *ContractBuilder,
        specifier: []const u8,
    ) !*contract_types.ExtensionContract {
        const entry = try getOrPutDuped(contract_types.ExtensionContract, &self.extensions, self.allocator, specifier);
        // Mirror the manifest's top-level section declaration onto the
        // contract entry the first time we touch this specifier. Subsequent
        // calls are idempotent. The manifest may live longer than this
        // builder so we own the copy.
        if (entry.contract_section == null) {
            const registry = self.manifest_registry orelse return entry;
            const manifest = registry.fromSpecifier(specifier) orelse return entry;
            if (manifest.contract_section) |name| {
                entry.contract_section = try self.allocator.dupe(u8, name);
            }
        }
        return entry;
    }

    fn getOrCreateCategoryBucket(
        self: *ContractBuilder,
        bucket: *contract_types.ExtensionContract,
        tag: []const u8,
    ) !*contract_types.ExtensionCategoryBucket {
        return getOrPutDuped(contract_types.ExtensionCategoryBucket, &bucket.categories, self.allocator, tag);
    }

    /// Insert-or-find on a string-keyed map where the key must be owned by
    /// the map. On miss, duplicates the key with the allocator and
    /// default-initializes the value; on duplicate-alloc failure rolls back
    /// the entry so the map stays consistent.
    fn getOrPutDuped(
        comptime V: type,
        map: *std.StringHashMapUnmanaged(V),
        allocator: std.mem.Allocator,
        key: []const u8,
    ) !*V {
        const gop = try map.getOrPut(allocator, key);
        if (!gop.found_existing) {
            const owned_key = allocator.dupe(u8, key) catch |err| {
                _ = map.remove(key);
                return err;
            };
            gop.key_ptr.* = owned_key;
            gop.value_ptr.* = .{};
        }
        return gop.value_ptr;
    }

    fn extractLiteralArgAt(
        self: *ContractBuilder,
        call: Node.CallExpr,
        arg_pos: u8,
        target: CategoryTarget,
        transform: ?*const fn ([]const u8) []const u8,
    ) !void {
        if (call.args_count <= arg_pos) return;

        const arg_idx = self.ir_view.getListIndex(call.args_start, arg_pos);
        const arg_tag = self.ir_view.getTag(arg_idx) orelse return;

        // Non-string arg -> dynamic; the value cannot be proven at
        // build time. Early-return keeps the literal happy path flat.
        if (arg_tag != .lit_string) {
            target.dynamic.* = true;
            return;
        }

        const str_idx = self.ir_view.getStringIdx(arg_idx) orelse return;
        const raw = self.ir_view.getString(str_idx) orelse return;
        const value = if (transform) |t| t(raw) else raw;

        // Empty post-transform (e.g. extractHost on a URL with no host)
        // is also a dynamic signal.
        if (value.len == 0) {
            target.dynamic.* = true;
            return;
        }

        if (containsString(target.list.items, value)) return;

        const duped = try self.allocator.dupe(u8, value);
        errdefer self.allocator.free(duped);
        try target.list.append(self.allocator, duped);
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
            // Partner-declared categories route through the extensions store, not the built-in target table.
            .extension_specific => null,
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
                // @floatFromInt(maxInt(i64)) rounds up to 2^63 in f64; use strict
                // < on the max side so values that overflow i64 return null.
                if (value >= @as(f64, @floatFromInt(std.math.maxInt(i64))) or
                    value < @as(f64, @floatFromInt(std.math.minInt(i64)))) break :blk null;
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

    fn extractIntentAssertions(self: *ContractBuilder) !?contract_types.IntentInfo {
        return intent_extractor.extract(.{
            .allocator = self.allocator,
            .ir_view = self.ir_view,
            .resolver = intentAtomResolver,
            .resolver_ctx = @ptrCast(self),
        });
    }

    fn intentAtomResolver(atom_idx: u16, ctx: *const anyopaque) ?[]const u8 {
        const self: *const ContractBuilder = @ptrCast(@alignCast(ctx));
        return self.resolveAtomName(atom_idx);
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
            service_call.markAllDynamic(self.allocator);
            return;
        };
        if (tag == .lit_null or tag == .lit_undefined) return;

        const init_obj = self.resolveObjectLiteralNode(init_idx) orelse {
            service_call.markAllDynamic(self.allocator);
            return;
        };
        const obj = self.ir_view.getObject(init_obj) orelse return;

        var i: u16 = 0;
        while (i < obj.properties_count) : (i += 1) {
            const prop_idx = self.ir_view.getListIndex(obj.properties_start, i);
            const prop = self.ir_view.getProperty(prop_idx) orelse continue;
            const key = self.getObjectPropertyKey(prop.key) orelse continue;

            if (std.mem.eql(u8, key, "params")) {
                service_call.path_params.deinitOwned(self.allocator);
                service_call.path_params = try self.extractKnownList(prop.value);
            } else if (std.mem.eql(u8, key, "query")) {
                service_call.query_keys.deinitOwned(self.allocator);
                service_call.query_keys = try self.extractKnownList(prop.value);
            } else if (std.mem.eql(u8, key, "headers")) {
                service_call.header_keys.deinitOwned(self.allocator);
                service_call.header_keys = try self.extractKnownList(prop.value);
            } else if (std.mem.eql(u8, key, "body")) {
                const body_tag = self.ir_view.getTag(prop.value) orelse {
                    service_call.body = .dynamic;
                    continue;
                };
                if (body_tag == .lit_null or body_tag == .lit_undefined) continue;
                service_call.body = if (body_tag == .lit_string) .present else .dynamic;
            }
        }
    }

    fn extractKnownList(self: *ContractBuilder, node_idx: NodeIndex) !ServiceCallInfo.KnownList {
        var items: std.ArrayList([]const u8) = .empty;
        errdefer {
            for (items.items) |s| self.allocator.free(s);
            items.deinit(self.allocator);
        }
        var dynamic = false;
        try self.extractServiceObjectKeys(node_idx, &items, &dynamic);
        if (dynamic) {
            for (items.items) |s| self.allocator.free(s);
            items.deinit(self.allocator);
            return .dynamic;
        }
        return .{ .complete = items };
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
                            try self.extractLiteralArg(call, .{ .list = &route.request_schema_refs, .dynamic = &route.request_schema_dynamic }, null);
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
                .schema = .{ .ref = try self.allocator.dupe(u8, schema_ref) },
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
            const body_schema_ref = body.schema.schemaRef() orelse continue;
            if (std.mem.eql(u8, body_content_type, content_type) and std.mem.eql(u8, body_schema_ref, schema_ref)) {
                return;
            }
        }

        try route.request_bodies.append(self.allocator, .{
            .content_type = try self.allocator.dupe(u8, content_type),
            .schema = .{ .ref = try self.allocator.dupe(u8, schema_ref) },
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
        if (response.schema.isDynamic()) {
            route.response_schema_dynamic = true;
            return;
        }

        route.response_status = response.status;
        if (response.content_type) |content_type| {
            route.response_content_type = try self.allocator.dupe(u8, content_type);
        }
        if (response.schema.schemaRef()) |schema_ref| {
            route.response_schema_ref = try self.allocator.dupe(u8, schema_ref);
        }
        if (response.schema.schemaJson()) |schema_json| {
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
            .schema = try schemaSpecFromCandidate(self.allocator, candidate),
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
    fn computeEffectSummary(self: *const ContractBuilder, handler_fn: ?NodeIndex) !EffectSummary {
        if (handler_fn) |hf| {
            var summary = EffectSummary{};
            var seen_functions: std.AutoHashMapUnmanaged(NodeIndex, void) = .empty;
            defer seen_functions.deinit(self.allocator);
            try self.includeReachableFunctionEffects(hf, &summary, &seen_functions);
            return summary;
        }

        return self.computeGlobalEffectSummary();
    }

    fn computeGlobalEffectSummary(self: *const ContractBuilder) EffectSummary {
        var summary = EffectSummary{};

        for (self.functions_map.items) |entry| {
            if (builtin_modules.fromSpecifier(entry.module)) |binding| {
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
            } else if (self.manifest_registry) |registry| {
                // Partner-registered module: read effect class from the manifest.
                // Treat extension-declared writes as bare writes (no durable
                // sequencing assumed); deterministic-by-default applies via
                // the verifier's existing logic.
                const manifest = registry.fromSpecifier(entry.module) orelse continue;
                for (entry.names.items) |func_name| {
                    summary.has_any_call = true;
                    for (manifest.exports.items) |exp| {
                        if (std.mem.eql(u8, exp.name, func_name)) {
                            summary.includeCall(exp.effect, false);
                            break;
                        }
                    }
                }
            }
        }

        if (self.egress_hosts.items.len > 0 or self.egress_dynamic) summary.includeEgress();

        return summary;
    }

    fn includeReachableFunctionEffects(
        self: *const ContractBuilder,
        node: NodeIndex,
        summary: *EffectSummary,
        seen_functions: *std.AutoHashMapUnmanaged(NodeIndex, void),
    ) std.mem.Allocator.Error!void {
        const fn_node = self.resolveFunctionNode(node) orelse return;
        const gop = try seen_functions.getOrPut(self.allocator, fn_node);
        if (gop.found_existing) return;

        const func = self.ir_view.getFunction(fn_node) orelse return;
        try self.includeReachableNodeEffects(func.body, summary, seen_functions);
    }

    fn includeReachableNodeEffects(
        self: *const ContractBuilder,
        node: NodeIndex,
        summary: *EffectSummary,
        seen_functions: *std.AutoHashMapUnmanaged(NodeIndex, void),
    ) std.mem.Allocator.Error!void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;
        switch (tag) {
            .program, .block => {
                const block = self.ir_view.getBlock(node) orelse return;
                for (0..block.stmts_count) |i| {
                    try self.includeReachableNodeEffects(
                        self.ir_view.getListIndex(block.stmts_start, @intCast(i)),
                        summary,
                        seen_functions,
                    );
                }
            },
            .if_stmt => {
                const stmt = self.ir_view.getIfStmt(node) orelse return;
                try self.includeReachableNodeEffects(stmt.condition, summary, seen_functions);
                try self.includeReachableNodeEffects(stmt.then_branch, summary, seen_functions);
                try self.includeReachableNodeEffects(stmt.else_branch, summary, seen_functions);
            },
            .for_of_stmt => {
                const stmt = self.ir_view.getForIter(node) orelse return;
                try self.includeReachableNodeEffects(stmt.iterable, summary, seen_functions);
                try self.includeReachableNodeEffects(stmt.body, summary, seen_functions);
            },
            .return_stmt, .expr_stmt => {
                if (self.ir_view.getOptValue(node)) |value| {
                    try self.includeReachableNodeEffects(value, summary, seen_functions);
                }
            },
            .var_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return;
                if (decl.init != null_node and !self.isFunctionNode(decl.init)) {
                    try self.includeReachableNodeEffects(decl.init, summary, seen_functions);
                }
            },
            .function_decl => {},
            .binary_op => {
                const bin = self.ir_view.getBinary(node) orelse return;
                try self.includeReachableNodeEffects(bin.left, summary, seen_functions);
                try self.includeReachableNodeEffects(bin.right, summary, seen_functions);
            },
            .unary_op, .spread => {
                const un = self.ir_view.getUnary(node) orelse return;
                try self.includeReachableNodeEffects(un.operand, summary, seen_functions);
            },
            .ternary => {
                const ternary = self.ir_view.getTernary(node) orelse return;
                try self.includeReachableNodeEffects(ternary.condition, summary, seen_functions);
                try self.includeReachableNodeEffects(ternary.then_branch, summary, seen_functions);
                try self.includeReachableNodeEffects(ternary.else_branch, summary, seen_functions);
            },
            .call, .method_call => {
                try self.includeReachableCallEffects(node, summary, seen_functions);
                const call = self.ir_view.getCall(node) orelse return;
                try self.includeReachableNodeEffects(call.callee, summary, seen_functions);
                for (0..call.args_count) |i| {
                    try self.includeReachableNodeEffects(
                        self.ir_view.getListIndex(call.args_start, @intCast(i)),
                        summary,
                        seen_functions,
                    );
                }
            },
            .member_access, .optional_chain, .computed_access => {
                const member = self.ir_view.getMember(node) orelse return;
                try self.includeReachableNodeEffects(member.object, summary, seen_functions);
                try self.includeReachableNodeEffects(member.computed, summary, seen_functions);
            },
            .assignment => {
                const assign = self.ir_view.getAssignment(node) orelse return;
                try self.includeReachableNodeEffects(assign.target, summary, seen_functions);
                try self.includeReachableNodeEffects(assign.value, summary, seen_functions);
            },
            .array_literal => {
                const arr = self.ir_view.getArray(node) orelse return;
                for (0..arr.elements_count) |i| {
                    try self.includeReachableNodeEffects(
                        self.ir_view.getListIndex(arr.elements_start, @intCast(i)),
                        summary,
                        seen_functions,
                    );
                }
            },
            .object_literal => {
                const obj = self.ir_view.getObject(node) orelse return;
                for (0..obj.properties_count) |i| {
                    const prop_idx = self.ir_view.getListIndex(obj.properties_start, @intCast(i));
                    const prop = self.ir_view.getProperty(prop_idx) orelse continue;
                    try self.includeReachableNodeEffects(prop.value, summary, seen_functions);
                }
            },
            .template_literal => {
                const tmpl = self.ir_view.getTemplate(node) orelse return;
                for (0..tmpl.parts_count) |i| {
                    const part = self.ir_view.getListIndex(tmpl.parts_start, @intCast(i));
                    if (self.ir_view.getOptValue(part)) |value| {
                        try self.includeReachableNodeEffects(value, summary, seen_functions);
                    }
                }
            },
            .match_expr => {
                const match = self.ir_view.getMatchExpr(node) orelse return;
                try self.includeReachableNodeEffects(match.discriminant, summary, seen_functions);
                for (0..match.arms_count) |i| {
                    const arm_idx = self.ir_view.getListIndex(match.arms_start, @intCast(i));
                    const arm = self.ir_view.getMatchArm(arm_idx) orelse continue;
                    try self.includeReachableNodeEffects(arm.body, summary, seen_functions);
                }
            },
            .function_expr, .arrow_function => {
                const func = self.ir_view.getFunction(node) orelse return;
                try self.includeReachableNodeEffects(func.body, summary, seen_functions);
            },
            else => {},
        }
    }

    fn includeReachableCallEffects(
        self: *const ContractBuilder,
        node: NodeIndex,
        summary: *EffectSummary,
        seen_functions: *std.AutoHashMapUnmanaged(NodeIndex, void),
    ) std.mem.Allocator.Error!void {
        const call = self.ir_view.getCall(node) orelse return;
        if (self.ir_view.getTag(call.callee) != .identifier) return;
        const binding = self.ir_view.getBinding(call.callee) orelse return;

        if (binding.kind == .undeclared_global) {
            const name = self.resolveAtomName(binding.slot) orelse return;
            if (std.mem.eql(u8, name, "fetchSync")) {
                summary.includeEgress();
                return;
            }
        }

        for (self.generic_bindings.items) |gb| {
            if (gb.slot != binding.slot) continue;
            summary.has_any_call = true;
            if (builtin_modules.fromSpecifier(gb.module_specifier)) |module| {
                const is_durable = std.mem.eql(u8, module.specifier, "zigttp:durable");
                for (module.exports) |exp| {
                    if (std.mem.eql(u8, exp.name, gb.binding_name)) {
                        summary.includeCall(exp.effect, is_durable);
                        break;
                    }
                }
            }
            if (std.mem.eql(u8, gb.module_specifier, "zigttp:cache") and
                std.mem.eql(u8, gb.binding_name, "cacheGet"))
            {
                summary.has_cache_read = true;
            }
            return;
        }

        if (self.manifest_registry) |registry| {
            for (self.extension_bindings.items) |eb| {
                if (eb.slot != binding.slot) continue;
                summary.has_any_call = true;
                const manifest = registry.fromSpecifier(eb.module_specifier) orelse return;
                for (manifest.exports.items) |exp| {
                    if (std.mem.eql(u8, exp.name, eb.binding_name)) {
                        summary.includeCall(exp.effect, false);
                        break;
                    }
                }
                return;
            }
        }

        if (self.findFunctionNodeByBinding(binding.slot)) |fn_node| {
            try self.includeReachableFunctionEffects(fn_node, summary, seen_functions);
        }
    }

    /// Derive handler-level properties from the aggregate effect summary.
    /// `handler_row`, when present, is the handler's call-graph composed
    /// effect row: intersecting with it makes a property the handler claims
    /// also account for every helper it transitively calls. This is a
    /// monotone tightening - it can only make a property falser, never
    /// unsoundly true - so the proof composes across helper boundaries.
    fn computeProperties(
        self: *const ContractBuilder,
        handler_row: ?effect_inference.EffectRow,
        handler_fn: ?NodeIndex,
    ) !HandlerProperties {
        const s = try self.computeEffectSummary(handler_fn);

        var read_only = s.io != .write;
        var deterministic = !self.has_nondeterministic_builtin;
        var pure = !s.has_any_call and !s.has_egress;

        if (handler_row) |row| {
            deterministic = deterministic and row.deterministic;
            read_only = read_only and row.readOnly();
            pure = pure and row.pure;
        }

        const durable_only_writes = s.io == .write and self.durable_used and !s.has_bare_write;
        // Scope cleanup callbacks run exactly once on unwind; retrying the request
        // would re-run them, violating at-most-once guarantees for resource cleanup.
        const retry_safe = !self.scope_used and (read_only or durable_only_writes);

        return .{
            .pure = pure,
            .read_only = read_only,
            .stateless = read_only and !s.has_cache_read,
            .retry_safe = retry_safe,
            .deterministic = deterministic,
            .has_egress = s.has_egress,
            .idempotent = deterministic and retry_safe,
            // The contract builder runs only after strict checking passed
            // (canonical-profile violations are hard `check` errors that block
            // contract construction upstream), so a built contract is always in
            // Canonical Normal Form. See HandlerProperties.canonical.
            .canonical = true,
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

fn containsApiParam(items: []const ApiParamInfo, needle: []const u8) bool {
    for (items) |item| {
        if (std.mem.eql(u8, item.name, needle)) return true;
    }
    return false;
}

fn containsRequestBodySchemaRef(items: []const ApiBodyInfo, needle: []const u8) bool {
    for (items) |item| {
        if (item.schema.schemaRef()) |schema_ref| {
            if (std.mem.eql(u8, schema_ref, needle)) return true;
        }
    }
    return false;
}

fn responseVariantMatches(existing: ApiResponseInfo, candidate: *const ContractBuilder.ResponseSchemaCandidate) bool {
    return existing.status == candidate.status and
        eqlOptionalString(existing.content_type, candidate.content_type) and
        eqlOptionalString(existing.schema.schemaRef(), candidate.schema_ref) and
        eqlOptionalString(existing.schema.schemaJson(), candidate.schema_json) and
        existing.schema.isDynamic() == candidate.dynamic;
}

/// Mirror ContractBuilder.ResponseSchemaCandidate's separate fields into the
/// SchemaSpec union with the same precedence used by the JSON parser.
fn schemaSpecFromCandidate(
    allocator: std.mem.Allocator,
    candidate: *const ContractBuilder.ResponseSchemaCandidate,
) !SchemaSpec {
    if (candidate.dynamic) return .dynamic;
    if (candidate.schema_json) |s| return .{ .inline_json = try allocator.dupe(u8, s) };
    if (candidate.schema_ref) |s| return .{ .ref = try allocator.dupe(u8, s) };
    return .none;
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

fn contentTypeFor(idx: u8) []const u8 {
    return switch (idx) {
        0 => "application/json",
        1 => "text/plain; charset=utf-8",
        else => "text/html; charset=utf-8",
    };
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

fn expectBuiltinExportEffect(
    module_name: []const u8,
    func_name: []const u8,
    effect: module_binding.EffectClass,
) !void {
    const binding = builtin_modules.fromSpecifier(module_name) orelse return error.MissingModule;
    for (binding.exports) |exp| {
        if (std.mem.eql(u8, exp.name, func_name)) {
            try std.testing.expectEqual(effect, exp.effect);
            return;
        }
    }
    return error.MissingExport;
}

fn buildTestContract(source: []const u8) !HandlerContract {
    const allocator = std.testing.allocator;
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();

    var parser = JsParser.init(allocator, source);
    defer parser.deinit();
    parser.setAtomTable(&atoms);

    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var builder = ContractBuilder.init(allocator, view, &atoms, null, null);
    defer builder.deinit();

    const handler_fn = findTestFunctionNode(view, &atoms, "handler");
    return try builder.build("handler.ts", null, handler_fn, root, null, false, null);
}

fn findTestFunctionNode(view: IrView, atoms: *context.AtomTable, name: []const u8) ?NodeIndex {
    const node_count = view.nodeCount();
    for (0..node_count) |idx_usize| {
        const idx: NodeIndex = @intCast(idx_usize);
        const tag = view.getTag(idx) orelse continue;
        switch (tag) {
            .function_decl, .var_decl => {
                const decl = view.getVarDecl(idx) orelse continue;
                const binding_name = resolveTestAtomName(atoms, decl.binding.slot) orelse continue;
                if (!std.mem.eql(u8, binding_name, name)) continue;
                if (decl.init == null_node) return null;
                const init_tag = view.getTag(decl.init) orelse return null;
                if (init_tag == .function_expr or init_tag == .arrow_function) return decl.init;
                return null;
            },
            else => {},
        }
    }
    return null;
}

fn resolveTestAtomName(atoms: *context.AtomTable, atom_idx: u16) ?[]const u8 {
    const atom: object.Atom = @enumFromInt(atom_idx);
    if (atom.isPredefined()) return atom.toPredefinedName();
    return atoms.getName(atom);
}

test "computeProperties pure handler stays pure" {
    var builder = ContractBuilder.init(std.testing.allocator, undefined, null, null, null);
    defer builder.deinit();

    const props = try builder.computeProperties(null, null);

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

    const props = try builder.computeProperties(null, null);

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

    const props = try builder.computeProperties(null, null);

    try std.testing.expect(!props.pure);
    try std.testing.expect(!props.read_only);
    try std.testing.expect(!props.stateless);
    try std.testing.expect(!props.retry_safe);
    try std.testing.expect(props.deterministic);
    try std.testing.expect(!props.has_egress);
    try std.testing.expect(!props.idempotent);
}

test "computeProperties registration functions are request-path writes" {
    var builder = ContractBuilder.init(std.testing.allocator, undefined, null, null, null);
    defer builder.deinit();

    try appendTrackedFunction(&builder, "zigttp:validate", "schemaCompile");

    const props = try builder.computeProperties(null, null);
    try std.testing.expect(!props.read_only);
    try std.testing.expect(!props.retry_safe);
}

test "registration module exports declare write effects" {
    try expectBuiltinExportEffect("zigttp:sql", "sql", .write);
    try expectBuiltinExportEffect("zigttp:validate", "schemaCompile", .write);
    try expectBuiltinExportEffect("zigttp:validate", "schemaDrop", .write);
}

test "top-level registration does not demote handler request properties" {
    const source =
        \\import { schemaCompile, validateJson } from "zigttp:validate";
        \\schemaCompile("todo", "{\"type\":\"object\"}");
        \\function handler(req) {
        \\  const parsed = validateJson("todo", req.body);
        \\  if (!parsed.ok) return Response.json({ error: "invalid" }, { status: 400 });
        \\  return Response.json(parsed.value);
        \\}
    ;
    var contract = try buildTestContract(source);
    defer contract.deinit(std.testing.allocator);

    const props = contract.properties orelse return error.MissingProperties;
    try std.testing.expect(props.read_only);
    try std.testing.expect(props.retry_safe);
    try std.testing.expect(props.idempotent);
}

test "handler-body registration remains a request-path write" {
    const source =
        \\import { schemaCompile } from "zigttp:validate";
        \\function handler(req) {
        \\  schemaCompile("todo", "{\"type\":\"object\"}");
        \\  return Response.json({ ok: true });
        \\}
    ;
    var contract = try buildTestContract(source);
    defer contract.deinit(std.testing.allocator);

    const props = contract.properties orelse return error.MissingProperties;
    try std.testing.expect(!props.read_only);
    try std.testing.expect(!props.retry_safe);
    try std.testing.expect(!props.idempotent);
}

test "computeProperties durable-only writes stay retry safe" {
    var builder = ContractBuilder.init(std.testing.allocator, undefined, null, null, null);
    defer builder.deinit();

    try appendTrackedFunction(&builder, "zigttp:durable", "step");
    builder.durable_used = true;

    const props = try builder.computeProperties(null, null);

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

    const props = try builder.computeProperties(null, null);

    try std.testing.expect(!props.pure);
    try std.testing.expect(props.read_only);
    try std.testing.expect(!props.stateless);
    try std.testing.expect(props.retry_safe);
    try std.testing.expect(!props.deterministic);
    try std.testing.expect(!props.has_egress);
    try std.testing.expect(!props.idempotent);
}

test "contract builder preserves determinism for durable step callback" {
    const source =
        \\import { step } from "zigttp:durable";
        \\function handler(req) { return step("ts", () => Date.now()); }
    ;
    var contract = try buildTestContract(source);
    defer contract.deinit(std.testing.allocator);

    const props = contract.properties orelse return error.MissingProperties;
    try std.testing.expect(props.deterministic);
    try std.testing.expect(props.idempotent);
}

test "contract builder does not exempt eager durable step argument" {
    const source =
        \\import { step } from "zigttp:durable";
        \\function handler(req) { return step("ts", Date.now()); }
    ;
    var contract = try buildTestContract(source);
    defer contract.deinit(std.testing.allocator);

    const props = contract.properties orelse return error.MissingProperties;
    try std.testing.expect(!props.deterministic);
    try std.testing.expect(!props.idempotent);
}

test "computeProperties egress is conservative write" {
    var builder = ContractBuilder.init(std.testing.allocator, undefined, null, null, null);
    defer builder.deinit();

    builder.egress_dynamic = true;

    const props = try builder.computeProperties(null, null);

    try std.testing.expect(!props.pure);
    try std.testing.expect(!props.read_only);
    try std.testing.expect(!props.stateless);
    try std.testing.expect(!props.retry_safe);
    try std.testing.expect(props.deterministic);
    try std.testing.expect(props.has_egress);
    try std.testing.expect(!props.idempotent);
}

test "registered partner manifest contributes effect class to handler properties" {
    const allocator = std.testing.allocator;

    // Partner manifest declares a write-effect export. The handler imports it
    // and calls it; the builder should reflect non-read-only properties.
    const manifest_json =
        \\{
        \\  "schemaVersion": 1,
        \\  "specifier": "zigttp-ext:stripe",
        \\  "backend": "native-zig",
        \\  "requiredCapabilities": ["network"],
        \\  "exports": [
        \\    { "name": "chargeCard", "effect": "write", "returns": "result" }
        \\  ]
        \\}
    ;
    var manifest = try module_manifest.parse(allocator, manifest_json);
    errdefer manifest.deinit(allocator);

    var registry = manifest_registry_mod.Registry.init(allocator);
    defer registry.deinit();
    try registry.register(manifest);

    const source =
        \\import { chargeCard } from "zigttp-ext:stripe";
        \\const r = chargeCard("tok");
    ;

    var parser = @import("parser/parse.zig").Parser.init(allocator, source);
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    parser.setAtomTable(&atoms);
    defer parser.deinit();

    _ = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);

    var builder = ContractBuilder.init(allocator, ir_view, &atoms, null, null);
    builder.manifest_registry = &registry;
    defer builder.deinit();

    try builder.scanImports();

    try std.testing.expect(containsString(builder.modules_list.items, "zigttp-ext:stripe"));
    try std.testing.expectEqual(@as(usize, 1), builder.functions_map.items.len);
    try std.testing.expectEqualStrings("zigttp-ext:stripe", builder.functions_map.items[0].module);
    try std.testing.expect(containsString(builder.functions_map.items[0].names.items, "chargeCard"));

    const props = try builder.computeProperties(null, null);
    try std.testing.expect(!props.read_only);
    try std.testing.expect(!props.idempotent);
    try std.testing.expect(!props.pure);
}

test "partner manifest contractExtractions populate extensions section" {
    const allocator = std.testing.allocator;

    // Partner declares a fetch_host rule on arg 0 and an extension_specific
    // rule (category = payment_gateway) on arg 1.
    const manifest_json =
        \\{
        \\  "schemaVersion": 1,
        \\  "specifier": "zigttp-ext:stripe",
        \\  "exports": [
        \\    {
        \\      "name": "charge",
        \\      "effect": "write",
        \\      "returns": "result",
        \\      "contractExtractions": [
        \\        { "category": "fetch_host", "argPosition": 0 },
        \\        { "category": "extension_specific", "extensionCategory": "payment_gateway", "argPosition": 1 }
        \\      ]
        \\    }
        \\  ]
        \\}
    ;
    var manifest = try module_manifest.parse(allocator, manifest_json);
    errdefer manifest.deinit(allocator);

    var registry = manifest_registry_mod.Registry.init(allocator);
    defer registry.deinit();
    try registry.register(manifest);

    const source =
        \\import { charge } from "zigttp-ext:stripe";
        \\const r = charge("api.stripe.com", "card_charge");
    ;

    var parser = @import("parser/parse.zig").Parser.init(allocator, source);
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    parser.setAtomTable(&atoms);
    defer parser.deinit();

    _ = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);

    var builder = ContractBuilder.init(allocator, ir_view, &atoms, null, null);
    builder.manifest_registry = &registry;
    defer builder.deinit();

    try builder.scanImports();
    try builder.scanCallSites();

    // Top-level egress mirrors the partner-declared fetch_host (shared-section
    // product decision: write to both).
    try std.testing.expect(containsString(builder.egress_hosts.items, "api.stripe.com"));

    // Per-extension copy lives under extensions["zigttp-ext:stripe"].
    const ext = builder.extensions.get("zigttp-ext:stripe") orelse return error.TestExpectedExtension;
    try std.testing.expect(containsString(ext.egress_hosts.items, "api.stripe.com"));

    // The payment_gateway category bucket holds the second arg's literal.
    const bucket = ext.categories.get("payment_gateway") orelse return error.TestExpectedCategory;
    try std.testing.expect(containsString(bucket.literals.items, "card_charge"));
}

test "builtin zigttp:fetch extracts the Open-Meteo egress host from a literal url" {
    const allocator = std.testing.allocator;

    // Mirrors examples/fetch/weather-forecasts.ts: a single keyless fetch to
    // Open-Meteo. The built-in fetch binding declares a fetch_host/extract_host
    // contract extraction on arg 0, so the host must land in egress_hosts as the
    // sole proven host with dynamic=false. This locks the demo's headline proof.
    const source =
        \\import { fetch } from "zigttp:fetch";
        \\const r = fetch("https://api.open-meteo.com/v1/forecast?latitude=52.52&longitude=13.41&current=temperature_2m&timezone=auto", { headers: { "Accept": "application/json" } });
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
    try builder.scanCallSites();

    try std.testing.expect(containsString(builder.egress_hosts.items, "api.open-meteo.com"));
    // Exactly one proven host, statically known (not dynamic).
    try std.testing.expectEqual(@as(usize, 1), builder.egress_hosts.items.len);
    try std.testing.expect(!builder.egress_dynamic);
}

test "missing manifest registry skips partner imports" {
    const allocator = std.testing.allocator;

    const source =
        \\import { unknownFn } from "zigttp-ext:unknown";
        \\const r = unknownFn();
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

    try std.testing.expect(!containsString(builder.modules_list.items, "zigttp-ext:unknown"));
    try std.testing.expectEqual(@as(usize, 0), builder.functions_map.items.len);
}
