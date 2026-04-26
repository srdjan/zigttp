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
const contract_json_writer = @import("contract_json_writer.zig");
const contract_json_parser = @import("contract_json_parser.zig");
const contract_types = @import("contract_types.zig");
const contract_builder = @import("contract_builder.zig");


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
// Contract data types (extracted to contract_types.zig)
// -------------------------------------------------------------------------
//
// Every public name is re-exported below so existing call sites
// (`handler_contract.HandlerContract`, `handler_contract.ApiRouteInfo`, etc.)
// resolve unchanged. Internal references in this file (ContractBuilder,
// ContractDiff, tests) also bind through these re-exports.

pub const HandlerLoc = contract_types.HandlerLoc;
pub const RouteInfo = contract_types.RouteInfo;
pub const EnvInfo = contract_types.EnvInfo;
pub const EgressInfo = contract_types.EgressInfo;
pub const CacheInfo = contract_types.CacheInfo;
pub const SqlQueryInfo = contract_types.SqlQueryInfo;
pub const SqlInfo = contract_types.SqlInfo;
pub const DurableKeyInfo = contract_types.DurableKeyInfo;
pub const DurableWorkflowProofLevel = contract_types.DurableWorkflowProofLevel;
pub const DurableWorkflowNodeKind = contract_types.DurableWorkflowNodeKind;
pub const DurableWorkflowNode = contract_types.DurableWorkflowNode;
pub const DurableWorkflowEdge = contract_types.DurableWorkflowEdge;
pub const DurableWorkflow = contract_types.DurableWorkflow;
pub const DurableInfo = contract_types.DurableInfo;
pub const ScopeInfo = contract_types.ScopeInfo;
pub const WebSocketInfo = contract_types.WebSocketInfo;
pub const ApiSchemaInfo = contract_types.ApiSchemaInfo;
pub const ApiRequestInfo = contract_types.ApiRequestInfo;
pub const ApiAuthInfo = contract_types.ApiAuthInfo;
pub const ApiParamInfo = contract_types.ApiParamInfo;
pub const SchemaSpec = contract_types.SchemaSpec;
pub const ApiBodyInfo = contract_types.ApiBodyInfo;
pub const ApiResponseInfo = contract_types.ApiResponseInfo;
pub const ApiRouteInfo = contract_types.ApiRouteInfo;
pub const ApiInfo = contract_types.ApiInfo;
pub const emptyApiInfo = contract_types.emptyApiInfo;
pub const emptySqlInfo = contract_types.emptySqlInfo;
pub const emptyContract = contract_types.emptyContract;
pub const VerificationInfo = contract_types.VerificationInfo;
pub const AotInfo = contract_types.AotInfo;
pub const FaultCoverageInfo = contract_types.FaultCoverageInfo;
pub const HandlerProperties = contract_types.HandlerProperties;
pub const RateLimitInfo = contract_types.RateLimitInfo;
pub const PathCondition = contract_types.PathCondition;
pub const PathIoCall = contract_types.PathIoCall;
pub const BehaviorPath = contract_types.BehaviorPath;
pub const ServiceCallInfo = contract_types.ServiceCallInfo;
pub const CapabilityMatrix = contract_types.CapabilityMatrix;
pub const computeCapabilityMatrix = contract_types.computeCapabilityMatrix;
pub const HandlerContract = contract_types.HandlerContract;

// -------------------------------------------------------------------------
// Contract builder (extracted to contract_builder.zig)
// -------------------------------------------------------------------------
//
// `ContractBuilder` and its tightly-coupled private helpers
// (containsApiParam, containsRequestBodySchemaRef, responseVariantMatches,
// schemaSpecFromCandidate, eqlOptionalString, lowerAsciiOwned,
// ParsedRouteKey/parseRouteKey, contentTypeFor, currentPolicyHashRaw)
// live in `contract_builder.zig`. Re-exports keep
// `handler_contract.ContractBuilder` working for external callers
// (precompile.zig, root.zig, ws_consistency.zig).

pub const ContractBuilder = contract_builder.ContractBuilder;

pub const containsString = json_utils.containsString;


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


// -------------------------------------------------------------------------
// JSON deserialization
// -------------------------------------------------------------------------
//
// `parseFromJson` + `JsonParser` + per-section parser helpers live in
// `contract_json_parser.zig`. Re-exports keep external callers
// (prove.zig, prove_upgrade.zig, system_linker.zig) and the round-trip
// tests below the cut on the same namespace.

pub const parseFromJson = contract_json_parser.parseFromJson;
pub const JsonParser = contract_json_parser.JsonParser;

// -------------------------------------------------------------------------
// JSON serialization
// -------------------------------------------------------------------------
//
// `writeContractJson` and its private emit helpers live in
// `contract_json_writer.zig`; this re-export keeps the existing call sites
// (zigts_cli, tests below) on the same namespace.

pub const writeContractJson = contract_json_writer.writeContractJson;

pub fn dupeOptionalString(allocator: std.mem.Allocator, s: ?[]const u8) !?[]const u8 {
    return if (s) |v| try allocator.dupe(u8, v) else null;
}

pub const writeJsonStringContent = json_utils.writeJsonStringContent;
pub const writeJsonString = json_utils.writeJsonString;

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
        .path_params = .{ .complete = blk: {
            var params: std.ArrayList([]const u8) = .empty;
            try params.append(allocator, try allocator.dupe(u8, "id"));
            break :blk params;
        } },
        .query_keys = .{ .complete = blk: {
            var keys: std.ArrayList([]const u8) = .empty;
            try keys.append(allocator, try allocator.dupe(u8, "expand"));
            break :blk keys;
        } },
        .header_keys = .{ .complete = blk: {
            var keys: std.ArrayList([]const u8) = .empty;
            try keys.append(allocator, try allocator.dupe(u8, "x-auth"));
            break :blk keys;
        } },
        .body = .none,
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
    try std.testing.expectEqual(@as(usize, 1), parsed.service_calls.items[0].path_params.items().len);
    try std.testing.expectEqualStrings("id", parsed.service_calls.items[0].path_params.items()[0]);
    try std.testing.expect(parsed.durable.used);
    try std.testing.expect(parsed.durable.keys.dynamic);
    try std.testing.expect(parsed.scope.used);
    try std.testing.expectEqual(@as(u32, 1), parsed.scope.max_depth);
    try std.testing.expect(parsed.verification != null);
    try std.testing.expect(parsed.verification.?.exhaustive_returns);
    try std.testing.expect(!parsed.verification.?.results_safe);
    try std.testing.expect(parsed.verification.?.bytecode_verified);
}

test "parseFromJson roundtrip preserves dynamic service call keys" {
    const allocator = std.testing.allocator;

    const path = try allocator.dupe(u8, "dynamic.ts");
    var service_calls: std.ArrayList(ServiceCallInfo) = .empty;
    try service_calls.append(allocator, .{
        .service = try allocator.dupe(u8, "search"),
        .route_pattern = try allocator.dupe(u8, "GET /search"),
        .path_params = .{ .complete = .empty },
        .query_keys = .dynamic,
        .header_keys = .dynamic,
        .body = .none,
    });

    var original = HandlerContract{
        .handler = .{ .path = path, .line = 1, .column = 1 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = .empty, .dynamic = false },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .service_calls = service_calls,
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
    defer original.deinit(allocator);

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);
    try writeContractJson(&original, &aw.writer);
    output = aw.toArrayList();

    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"queryDynamic\": true") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"headerDynamic\": true") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "queryKeysDynamic") == null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "headerKeysDynamic") == null);

    var parsed = try parseFromJson(allocator, output.items);
    defer parsed.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 1), parsed.service_calls.items.len);
    try std.testing.expect(parsed.service_calls.items[0].query_keys.isDynamic());
    try std.testing.expect(parsed.service_calls.items[0].header_keys.isDynamic());
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
        .schema = .{ .ref = try allocator.dupe(u8, "user.create") },
    });
    var responses: std.ArrayList(ApiResponseInfo) = .empty;
    try responses.append(allocator, .{
        .status = 200,
        .content_type = try allocator.dupe(u8, "application/json"),
        .schema = .{ .ref = try allocator.dupe(u8, "user") },
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
    try std.testing.expectEqualStrings("user.create", route.request_bodies.items[0].schema.schemaRef().?);
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
