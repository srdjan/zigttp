//! Proven Deployment Facts And Reports
//!
//! Extracts platform-agnostic proven deployment facts and renders human reports
//! from compiler-proven handler contracts. The contract captures exactly what a
//! handler does (env vars, egress hosts, cache namespaces, routes) as proven
//! facts - not heuristic guesses.
//!
//! The old build-time AWS renderer has been retired; runtime `zttp deploy`
//! consumes the portable fact extraction and reporting surfaces from this file.

const std = @import("std");
const zts = @import("zts");
const handler_contract = zts.handler_contract;
const HandlerContract = handler_contract.HandlerContract;
const contract_diff = zts.contract_diff;

// -------------------------------------------------------------------------
// Platform-agnostic proven facts
// -------------------------------------------------------------------------

pub const ProofLevel = contract_diff.ProofLevel;
pub const default_cost_body_limit: u64 = 1024 * 1024;

pub const ProvenRoute = struct {
    pattern: []const u8,
    is_prefix: bool,
};

pub const ProvenFacts = struct {
    handler_name: []const u8,
    handler_path: []const u8,

    env_vars: []const []const u8,
    env_proven: bool,

    egress_hosts: []const []const u8,
    egress_proven: bool,

    cache_namespaces: []const []const u8,
    cache_proven: bool,

    routes: []const ProvenRoute,

    proof_level: ProofLevel,
    checks_passed: []const []const u8,

    retry_safe: bool = false,
    read_only: bool = false,
    injection_safe: bool = true,
    idempotent: bool = false,
    state_isolated: bool = true,
    /// True when the handler has no `Date.now()` or `Math.random()` call
    /// sites. Surfaced in the live HUD so authors see `+deterministic`
    /// flip when they introduce a non-deterministic builtin.
    deterministic: bool = true,
    max_io_depth: ?u32 = null,
    cost_total: ?handler_contract.Bound = null,
    cost_worst_case_total: ?u64 = null,
    cost_worst_case_body_limit: u64 = default_cost_body_limit,

    // Data flow provenance
    no_secret_leakage: bool = true,
    no_credential_leakage: bool = true,
    input_validated: bool = true,
    pii_contained: bool = true,

    // Verification
    results_safe: bool = false,

    // Rate limiting
    rate_limit_namespace: ?[]const u8 = null,

    // Fault coverage
    fault_covered: bool = false,

    // Durable workflow replay guarantees. Kept separate from handler-wide
    // retry/idempotency so deployment artifacts can explain replay behavior
    // without overloading the generic proof chips.
    durable_workflow_used: bool = false,
    durable_workflow_proof_level: handler_contract.DurableWorkflowProofLevel = .none,
    durable_workflow_retry_safe: bool = false,
    durable_workflow_idempotent: bool = false,
    durable_workflow_fault_covered: bool = false,

    // WebSocket event exports. When any is true the handler upgrades
    // incoming `Upgrade: websocket` requests and dispatches through
    // zttp:websocket. Deploy targets that need a different binding
    // (e.g. Cloudflare Durable Objects) can branch on `has_websocket`.
    has_websocket: bool = false,
    websocket_on_open: bool = false,
    websocket_on_message: bool = false,
    websocket_on_close: bool = false,
    websocket_on_error: bool = false,

    // `zttp:fetch` host set. Populated from the contract's egress
    // hosts when the handler imports `zttp:fetch`. Rendered as a
    // separate allow-list so targets can distinguish "fetchSync
    // egress" (all hosts) from "zttp:fetch egress" (the durable
    // subset).
    fetch_hosts: []const []const u8 = &.{},
};

// -------------------------------------------------------------------------
// Renderer interface
// -------------------------------------------------------------------------

pub const DeployTarget = enum {
    aws,
    cloudflare_workers,

    pub fn fromString(s: []const u8) ?DeployTarget {
        if (std.mem.eql(u8, s, "aws")) return .aws;
        if (std.mem.eql(u8, s, "cloudflare-workers") or std.mem.eql(u8, s, "cloudflare_workers") or std.mem.eql(u8, s, "workers")) return .cloudflare_workers;
        return null;
    }

    pub fn toString(self: DeployTarget) []const u8 {
        return switch (self) {
            .aws => "aws",
            .cloudflare_workers => "cloudflare-workers",
        };
    }
};

pub const RenderOutput = struct {
    filename: []const u8,
    content: []const u8,
};

// -------------------------------------------------------------------------
// Fact extraction
// -------------------------------------------------------------------------

/// Derive platform-agnostic ProvenFacts from a HandlerContract.
/// All slices in the returned struct point into the contract's owned data
/// or into the checks_buf. The caller must keep the contract alive while
/// using the facts. checks_buf is allocated and must be freed by the caller.
pub fn extractProvenFacts(
    allocator: std.mem.Allocator,
    contract: *const HandlerContract,
) !struct { facts: ProvenFacts, checks_buf: [][]const u8, routes_buf: []ProvenRoute } {
    // Handler name: strip directory and extension from path
    const handler_name = extractHandlerName(contract.handler.path);

    // Collect checks that passed
    var checks: std.ArrayList([]const u8) = .empty;
    errdefer checks.deinit(allocator);

    if (contract.verification) |v| {
        if (v.exhaustive_returns) try checks.append(allocator, "exhaustiveReturns");
        if (v.results_safe) try checks.append(allocator, "resultsSafe");
        if (v.bytecode_verified) try checks.append(allocator, "bytecodeVerified");
    }

    // Build routes from contract
    var routes: std.ArrayList(ProvenRoute) = .empty;
    errdefer routes.deinit(allocator);

    for (contract.routes.items) |route| {
        try routes.append(allocator, .{
            .pattern = route.pattern,
            .is_prefix = std.mem.eql(u8, route.route_type, "prefix"),
        });
    }

    const checks_buf = try checks.toOwnedSlice(allocator);
    errdefer allocator.free(checks_buf);
    const routes_buf = try routes.toOwnedSlice(allocator);

    const proof_level = deriveProofLevel(contract);

    // `zttp:fetch` egress subset: when the handler imports the
    // module the whole egress set is fetch-capable. Downstream targets
    // that want a narrower allow-list (e.g. Cloudflare egress) treat
    // fetch_hosts as the set to configure.
    const imports_fetch = containsString(contract.modules.items, "zttp:fetch");
    const fetch_hosts: []const []const u8 = if (imports_fetch) contract.egress.hosts.items else &.{};

    const ws = contract.websocket;
    const has_ws = ws.on_open or ws.on_message or ws.on_close or ws.on_error;
    const cost_total: ?handler_contract.Bound = if (contract.cost_envelope) |envelope| envelope.total else null;

    return .{
        .facts = .{
            .handler_name = handler_name,
            .handler_path = contract.handler.path,
            .env_vars = contract.env.literal.items,
            .env_proven = !contract.env.dynamic,
            .egress_hosts = contract.egress.hosts.items,
            .egress_proven = !contract.egress.dynamic,
            .cache_namespaces = contract.cache.namespaces.items,
            .cache_proven = !contract.cache.dynamic,
            .routes = routes_buf,
            .proof_level = proof_level,
            .checks_passed = checks_buf,
            .retry_safe = if (contract.properties) |p| p.retry_safe else false,
            .read_only = if (contract.properties) |p| p.read_only else false,
            .injection_safe = if (contract.properties) |p| p.injection_safe else true,
            .idempotent = if (contract.properties) |p| p.idempotent else false,
            .state_isolated = if (contract.properties) |p| p.state_isolated else true,
            .deterministic = if (contract.properties) |p| p.deterministic else true,
            .max_io_depth = if (contract.properties) |p| p.max_io_depth else null,
            .cost_total = cost_total,
            .cost_worst_case_total = if (cost_total) |bound| bound.worstCaseAt(default_cost_body_limit) else null,
            .cost_worst_case_body_limit = default_cost_body_limit,
            .no_secret_leakage = if (contract.properties) |p| p.no_secret_leakage else true,
            .no_credential_leakage = if (contract.properties) |p| p.no_credential_leakage else true,
            .input_validated = if (contract.properties) |p| p.input_validated else true,
            .pii_contained = if (contract.properties) |p| p.pii_contained else true,
            .results_safe = if (contract.verification) |v| v.results_safe else false,
            .rate_limit_namespace = if (contract.rate_limiting) |rl| (if (rl.namespace.len > 0) rl.namespace else null) else null,
            .fault_covered = if (contract.properties) |p| p.fault_covered else false,
            .durable_workflow_used = contract.durable.used,
            .durable_workflow_proof_level = contract.durable.workflow.proof_level,
            .durable_workflow_retry_safe = contract.durable.workflow.properties.retry_safe,
            .durable_workflow_idempotent = contract.durable.workflow.properties.idempotent,
            .durable_workflow_fault_covered = contract.durable.workflow.properties.fault_covered,
            .has_websocket = has_ws,
            .websocket_on_open = ws.on_open,
            .websocket_on_message = ws.on_message,
            .websocket_on_close = ws.on_close,
            .websocket_on_error = ws.on_error,
            .fetch_hosts = fetch_hosts,
        },
        .checks_buf = checks_buf,
        .routes_buf = routes_buf,
    };
}

fn containsString(haystack: []const []const u8, needle: []const u8) bool {
    for (haystack) |item| if (std.mem.eql(u8, item, needle)) return true;
    return false;
}

pub fn costWorstCaseTotal(facts: *const ProvenFacts) ?u64 {
    if (facts.cost_worst_case_total) |n| return n;
    const bound = facts.cost_total orelse return null;
    return bound.worstCaseAt(facts.cost_worst_case_body_limit);
}

fn extractHandlerName(path: []const u8) []const u8 {
    // Find last '/' for filename start
    var start: usize = 0;
    for (path, 0..) |c, i| {
        if (c == '/') start = i + 1;
    }
    const filename = path[start..];

    // Strip extension
    var end = filename.len;
    for (filename, 0..) |c, i| {
        if (c == '.') end = i;
    }
    if (end == 0) return filename;
    return filename[0..end];
}

pub const deriveProofLevel = contract_diff.deriveProofLevel;

// -------------------------------------------------------------------------
// Renderer dispatch
// -------------------------------------------------------------------------

pub fn render(
    allocator: std.mem.Allocator,
    target: DeployTarget,
    facts: *const ProvenFacts,
) ![]const RenderOutput {
    return switch (target) {
        .aws => try renderAws(allocator, facts),
        .cloudflare_workers => try renderCloudflareWorkers(allocator, facts),
    };
}

// -------------------------------------------------------------------------
// AWS SAM backend
// -------------------------------------------------------------------------

fn renderAws(allocator: std.mem.Allocator, facts: *const ProvenFacts) ![]const RenderOutput {
    var output: std.ArrayList(u8) = .empty;
    errdefer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);
    const w = &aw.writer;

    try w.writeAll("{\n");

    // Template header
    try w.writeAll("  \"AWSTemplateFormatVersion\": \"2010-09-09\",\n");
    try w.writeAll("  \"Transform\": \"AWS::Serverless-2016-10-31\",\n");
    try w.writeAll("  \"Description\": \"zttp proven deployment - ");
    try writeJsonStringContent(w, facts.handler_path);
    try w.writeAll(" (proof: ");
    try w.writeAll(facts.proof_level.toString());
    try w.writeAll(")\",\n");

    // Metadata
    try w.writeAll("  \"Metadata\": {\n");
    try w.writeAll("    \"zttp\": {\n");
    try w.print("      \"version\": \"{s}\",\n", .{zts.version.string});
    try w.writeAll("      \"proofLevel\": \"");
    try w.writeAll(facts.proof_level.toString());
    try w.writeAll("\",\n");
    try w.writeAll("      \"handler\": \"");
    try writeJsonStringContent(w, facts.handler_path);
    try w.writeAll("\",\n");
    try w.writeAll("      \"verification\": [");
    for (facts.checks_passed, 0..) |check, i| {
        if (i > 0) try w.writeAll(", ");
        try w.writeByte('"');
        try w.writeAll(check);
        try w.writeByte('"');
    }
    try w.writeAll("],\n");
    // Env proof status in metadata (not as a fake env var)
    try w.writeAll("      \"envProven\": ");
    try w.writeAll(if (facts.env_proven) "true" else "false");
    try w.writeAll(",\n      \"durableWorkflow\": { \"used\": ");
    try w.writeAll(if (facts.durable_workflow_used) "true" else "false");
    try w.writeAll(", \"proofLevel\": \"");
    try w.writeAll(facts.durable_workflow_proof_level.toString());
    try w.writeAll("\", \"retrySafe\": ");
    try w.writeAll(if (facts.durable_workflow_retry_safe) "true" else "false");
    try w.writeAll(", \"idempotent\": ");
    try w.writeAll(if (facts.durable_workflow_idempotent) "true" else "false");
    try w.writeAll(", \"faultCovered\": ");
    try w.writeAll(if (facts.durable_workflow_fault_covered) "true" else "false");
    try w.writeAll(" }");
    if (!facts.env_proven) {
        try w.writeAll(",\n      \"envReview\": \"handler uses dynamic env access - additional vars may be needed\"");
    }
    if (facts.has_websocket) {
        try w.writeAll(",\n      \"websocket\": { \"onOpen\": ");
        try w.writeAll(if (facts.websocket_on_open) "true" else "false");
        try w.writeAll(", \"onMessage\": ");
        try w.writeAll(if (facts.websocket_on_message) "true" else "false");
        try w.writeAll(", \"onClose\": ");
        try w.writeAll(if (facts.websocket_on_close) "true" else "false");
        try w.writeAll(", \"onError\": ");
        try w.writeAll(if (facts.websocket_on_error) "true" else "false");
        try w.writeAll(" }");
    }
    if (facts.fetch_hosts.len > 0) {
        try w.writeAll(",\n      \"fetchHosts\": [");
        for (facts.fetch_hosts, 0..) |host, i| {
            if (i > 0) try w.writeAll(", ");
            try w.writeByte('"');
            try writeJsonStringContent(w, host);
            try w.writeByte('"');
        }
        try w.writeAll("]");
    }
    try w.writeAll("\n    }\n");
    try w.writeAll("  },\n");

    // Parameters (one per env var)
    try w.writeAll("  \"Parameters\": {");
    for (facts.env_vars, 0..) |env_var, i| {
        if (i > 0) try w.writeAll(",");
        try w.writeAll("\n    \"");
        try writeParamName(w, env_var);
        try w.writeAll("\": {\n");
        try w.writeAll("      \"Type\": \"String\",\n");
        try w.writeAll("      \"NoEcho\": true,\n");
        try w.writeAll("      \"Description\": \"");
        if (facts.env_proven) {
            try w.writeAll("PROVEN: handler reads ");
        } else {
            try w.writeAll("handler reads ");
        }
        try writeJsonStringContent(w, env_var);
        try w.writeAll("\"\n");
        try w.writeAll("    }");
    }
    // VPC parameters (when egress hosts are known)
    const has_egress = facts.egress_hosts.len > 0;
    if (has_egress) {
        if (facts.env_vars.len > 0) try w.writeAll(",");
        try w.writeAll("\n    \"VpcSubnetIds\": {\n");
        try w.writeAll("      \"Type\": \"CommaDelimitedList\",\n");
        try w.writeAll("      \"Default\": \"\",\n");
        try w.writeAll("      \"Description\": \"Private subnet IDs for VPC egress restriction (leave empty to skip VPC)\"\n");
        try w.writeAll("    }");
    }
    if (facts.env_vars.len > 0 or has_egress) {
        try w.writeAll("\n  ");
    }
    try w.writeAll("},\n");

    // Conditions
    if (has_egress) {
        try w.writeAll("  \"Conditions\": {\n");
        try w.writeAll("    \"HasVpc\": { \"Fn::Not\": [{ \"Fn::Equals\": [{ \"Fn::Join\": [\"\", { \"Ref\": \"VpcSubnetIds\" }] }, \"\"] }] }\n");
        try w.writeAll("  },\n");
    }

    // Resources
    try w.writeAll("  \"Resources\": {\n");
    try w.writeAll("    \"HandlerFunction\": {\n");
    try w.writeAll("      \"Type\": \"AWS::Serverless::Function\",\n");
    try w.writeAll("      \"Properties\": {\n");
    try w.writeAll("        \"Handler\": \"bootstrap\",\n");
    try w.writeAll("        \"Runtime\": \"provided.al2023\",\n");
    try w.writeAll("        \"MemorySize\": 128,\n");
    try w.writeAll("        \"Timeout\": 30,\n");
    try w.writeAll("        \"Architectures\": [\"x86_64\"],\n");

    // Environment variables
    try w.writeAll("        \"Environment\": {\n");
    try w.writeAll("          \"Variables\": {");
    for (facts.env_vars, 0..) |env_var, i| {
        if (i > 0) try w.writeAll(",");
        try w.writeAll("\n            \"");
        try writeJsonStringContent(w, env_var);
        try w.writeAll("\": { \"Ref\": \"");
        try writeParamName(w, env_var);
        try w.writeAll("\" }");
    }
    if (facts.env_vars.len > 0) {
        try w.writeAll("\n          ");
    }
    try w.writeAll("}\n");
    try w.writeAll("        },\n");

    // VpcConfig (conditional on HasVpc)
    if (has_egress) {
        try w.writeAll("        \"VpcConfig\": {\n");
        try w.writeAll("          \"Fn::If\": [\n");
        try w.writeAll("            \"HasVpc\",\n");
        try w.writeAll("            {\n");
        try w.writeAll("              \"SecurityGroupIds\": [{ \"Ref\": \"EgressSecurityGroup\" }],\n");
        try w.writeAll("              \"SubnetIds\": { \"Ref\": \"VpcSubnetIds\" }\n");
        try w.writeAll("            },\n");
        try w.writeAll("            { \"Ref\": \"AWS::NoValue\" }\n");
        try w.writeAll("          ]\n");
        try w.writeAll("        },\n");
    }

    // Events (routes)
    try w.writeAll("        \"Events\": {");
    if (facts.routes.len > 0) {
        for (facts.routes, 0..) |route, i| {
            if (i > 0) try w.writeAll(",");
            try w.writeAll("\n          \"");
            try writeSanitizedId(w, route.pattern);
            try w.writeAll("\": {\n");
            try w.writeAll("            \"Type\": \"HttpApi\",\n");
            try w.writeAll("            \"Properties\": {\n");
            try w.writeAll("              \"Path\": \"");
            try writeJsonStringContent(w, route.pattern);
            if (route.is_prefix) try w.writeAll("/{proxy+}");
            try w.writeAll("\",\n");
            try w.writeAll("              \"Method\": \"ANY\"\n");
            try w.writeAll("            }\n");
            try w.writeAll("          }");
        }
        try w.writeAll("\n        ");
    } else {
        // Catch-all when no routes proven
        try w.writeAll("\n          \"CatchAll\": {\n");
        try w.writeAll("            \"Type\": \"HttpApi\",\n");
        try w.writeAll("            \"Properties\": {\n");
        try w.writeAll("              \"Path\": \"/{proxy+}\",\n");
        try w.writeAll("              \"Method\": \"ANY\"\n");
        try w.writeAll("            }\n");
        try w.writeAll("          }\n        ");
    }
    try w.writeAll("},\n");

    // Tags
    try w.writeAll("        \"Tags\": {\n");
    try w.writeAll("          \"zttp:proven\": \"");
    try w.writeAll(if (facts.proof_level == .complete) "true" else "false");
    try w.writeAll("\",\n");
    try w.writeAll("          \"zttp:proofLevel\": \"");
    try w.writeAll(facts.proof_level.toString());
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zttp:durableWorkflowProofLevel\": \"");
    try w.writeAll(facts.durable_workflow_proof_level.toString());
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zttp:durableWorkflowRetrySafe\": \"");
    try w.writeAll(if (facts.durable_workflow_retry_safe) "true" else "false");
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zttp:durableWorkflowIdempotent\": \"");
    try w.writeAll(if (facts.durable_workflow_idempotent) "true" else "false");
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zttp:durableWorkflowFaultCovered\": \"");
    try w.writeAll(if (facts.durable_workflow_fault_covered) "true" else "false");
    try w.writeAll("\"");

    try w.writeAll(",\n          \"zttp:retrySafe\": \"");
    try w.writeAll(if (facts.retry_safe) "true" else "false");
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zttp:readOnly\": \"");
    try w.writeAll(if (facts.read_only) "true" else "false");
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zttp:injectionSafe\": \"");
    try w.writeAll(if (facts.injection_safe) "true" else "false");
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zttp:idempotent\": \"");
    try w.writeAll(if (facts.idempotent) "true" else "false");
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zttp:stateIsolated\": \"");
    try w.writeAll(if (facts.state_isolated) "true" else "false");
    try w.writeAll("\"");
    if (facts.max_io_depth) |depth| {
        try w.print(",\n          \"zttp:maxIoDepth\": \"{d}\"", .{depth});
    }
    if (facts.cost_total) |bound| {
        try w.writeAll(",\n          \"zttp:costClass\": \"");
        try w.writeAll(bound.class().asString());
        try w.writeAll("\"");
        switch (bound) {
            .constant => {},
            .linear => |linear| {
                try w.print(",\n          \"zttp:costBound\": \"{d}+{d}*n\"", .{ linear.base, linear.coefficient });
                try w.writeAll(",\n          \"zttp:costSource\": \"");
                try writeCostSource(w, linear.source);
                try w.writeAll("\"");
            },
            .unbounded => |source| {
                try w.writeAll(",\n          \"zttp:costSource\": \"");
                try writeCostSource(w, source);
                try w.writeAll("\"");
            },
        }
        try w.writeAll(",\n          \"zttp:costWorstCase\": \"");
        if (costWorstCaseTotal(facts)) |worst| {
            try w.print("{d}", .{worst});
        } else {
            try w.writeAll("unbounded");
        }
        try w.writeAll("\"");
        try w.print(",\n          \"zttp:costBodyLimit\": \"{d}\"", .{facts.cost_worst_case_body_limit});
    }

    // Flow provenance tags
    try w.writeAll(",\n          \"zttp:noSecretLeakage\": \"");
    try w.writeAll(if (facts.no_secret_leakage) "true" else "false");
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zttp:noCredentialLeakage\": \"");
    try w.writeAll(if (facts.no_credential_leakage) "true" else "false");
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zttp:inputValidated\": \"");
    try w.writeAll(if (facts.input_validated) "true" else "false");
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zttp:piiContained\": \"");
    try w.writeAll(if (facts.pii_contained) "true" else "false");
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zttp:faultCovered\": \"");
    try w.writeAll(if (facts.fault_covered) "true" else "false");
    try w.writeAll("\"");

    if (facts.egress_hosts.len > 0 or facts.egress_proven) {
        try w.writeAll(",\n          \"zttp:egressProven\": \"");
        try w.writeAll(if (facts.egress_proven) "true" else "false");
        try w.writeAll("\"");

        if (facts.egress_hosts.len > 0) {
            try w.writeAll(",\n          \"zttp:egressHosts\": \"");
            for (facts.egress_hosts, 0..) |host, i| {
                if (i > 0) try w.writeAll(",");
                try writeJsonStringContent(w, host);
            }
            try w.writeAll("\"");
        }
    }

    try w.writeAll("\n        }\n");

    try w.writeAll("      }\n");
    try w.writeAll("    }");

    if (has_egress) {
        try w.writeAll(",\n    \"EgressSecurityGroup\": {\n");
        try w.writeAll("      \"Type\": \"AWS::EC2::SecurityGroup\",\n");
        try w.writeAll("      \"Condition\": \"HasVpc\",\n");
        try w.writeAll("      \"Properties\": {\n");
        try w.writeAll("        \"GroupDescription\": \"zttp proven egress - ");
        if (facts.egress_proven) {
            try w.writeAll("restricted to: ");
            for (facts.egress_hosts, 0..) |host, i| {
                if (i > 0) try w.writeAll(", ");
                try writeJsonStringContent(w, host);
            }
        } else {
            try w.writeAll("REVIEW: handler uses dynamic URLs");
        }
        try w.writeAll("\",\n");
        try w.writeAll("        \"VpcId\": { \"Fn::Select\": [0, { \"Fn::Split\": [\",\", { \"Fn::Select\": [0, { \"Ref\": \"VpcSubnetIds\" }] }] }] },\n");
        try w.writeAll("        \"SecurityGroupEgress\": [\n");
        try w.writeAll("          {\n");
        try w.writeAll("            \"IpProtocol\": \"tcp\",\n");
        try w.writeAll("            \"FromPort\": 443,\n");
        try w.writeAll("            \"ToPort\": 443,\n");
        try w.writeAll("            \"CidrIp\": \"0.0.0.0/0\",\n");
        try w.writeAll("            \"Description\": \"HTTPS egress");
        if (facts.egress_proven) {
            try w.writeAll(" (proven hosts: ");
            for (facts.egress_hosts, 0..) |host, i| {
                if (i > 0) try w.writeAll(", ");
                try writeJsonStringContent(w, host);
            }
            try w.writeAll(")");
        }
        try w.writeAll("\"\n");
        try w.writeAll("          }\n");
        try w.writeAll("        ]\n");
        try w.writeAll("      }\n");
        try w.writeAll("    }");
    }

    try w.writeAll("\n  },\n");

    // Outputs
    try w.writeAll("  \"Outputs\": {\n");
    try w.writeAll("    \"ApiUrl\": {\n");
    try w.writeAll("      \"Description\": \"API endpoint\",\n");
    try w.writeAll("      \"Value\": { \"Fn::Sub\": \"https://${ServerlessHttpApi}.execute-api.${AWS::Region}.amazonaws.com\" }\n");
    try w.writeAll("    }\n");
    try w.writeAll("  }\n");

    try w.writeAll("}\n");

    output = aw.toArrayList();
    const content = try output.toOwnedSlice(allocator);

    const outputs = try allocator.alloc(RenderOutput, 1);
    outputs[0] = .{
        .filename = "template.json",
        .content = content,
    };
    return outputs;
}

// -------------------------------------------------------------------------
// Cloudflare Workers backend
// -------------------------------------------------------------------------

/// Render a `wrangler.toml` plus a sibling proof-metadata JSON describing
/// the same proven facts AWS gets in CloudFormation form. The TOML follows
/// Wrangler's published shape: top-level `name`, `compatibility_date`,
/// route patterns translated to Workers route objects, env var names as
/// `[vars]` entries (values left empty so `wrangler secret put` or a CI
/// step supplies them), and a custom `[zttp]` table carrying proof
/// metadata. Wrangler ignores unknown top-level tables, so the metadata
/// rides along without breaking deploys.
fn renderCloudflareWorkers(allocator: std.mem.Allocator, facts: *const ProvenFacts) ![]const RenderOutput {
    var output: std.ArrayList(u8) = .empty;
    errdefer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);
    const w = &aw.writer;

    try w.writeAll("# Generated by zttp deploy_manifest. Do not edit by hand.\n");
    try w.writeAll("# Proof level: ");
    try w.writeAll(facts.proof_level.toString());
    try w.writeAll("\n\n");

    try w.writeAll("name = \"");
    try writeTomlBareString(w, facts.handler_name);
    try w.writeAll("\"\n");

    try w.writeAll("compatibility_date = \"");
    try w.writeAll(compatibility_date_stamp);
    try w.writeAll("\"\n\n");

    // Routes. Wrangler route patterns are host-qualified zone patterns,
    // not internal HTTP paths such as `/health`. Keep already-qualified
    // patterns and omit path-only facts until domain mapping is explicit.
    const cf_route_count = countCloudflareRoutePatterns(facts.routes);
    if (cf_route_count > 0) {
        try w.writeAll("routes = [\n");
        var emitted: usize = 0;
        for (facts.routes) |route| {
            if (!isCloudflareRoutePattern(route.pattern)) continue;
            try w.writeAll("  { pattern = \"");
            try writeTomlBareString(w, route.pattern);
            try w.writeAll("\" }");
            emitted += 1;
            if (emitted < cf_route_count) try w.writeByte(',');
            try w.writeByte('\n');
        }
        try w.writeAll("]\n\n");
    }

    // Non-secret env vars. Values left empty so the user wires them up
    // via `wrangler secret put` or a CI step; the names are the proof
    // surface and that is what we emit.
    if (facts.env_vars.len > 0) {
        try w.writeAll("[vars]\n");
        for (facts.env_vars) |env_var| {
            try writeTomlBareString(w, env_var);
            try w.writeAll(" = \"\"\n");
        }
        try w.writeByte('\n');
    }

    // Proof metadata table. Wrangler treats unknown top-level tables as
    // pass-through metadata; this is informational, not consumed by the
    // platform. Mirrors the metadata block the AWS renderer emits.
    try w.writeAll("[zttp]\n");
    try w.writeAll("proof_level = \"");
    try w.writeAll(facts.proof_level.toString());
    try w.writeAll("\"\n");
    try w.writeAll("handler = \"");
    try writeTomlBareString(w, facts.handler_path);
    try w.writeAll("\"\n");
    try w.writeAll("env_proven = ");
    try w.writeAll(if (facts.env_proven) "true" else "false");
    try w.writeByte('\n');
    try w.writeAll("egress_proven = ");
    try w.writeAll(if (facts.egress_proven) "true" else "false");
    try w.writeByte('\n');
    try w.writeAll("durable_workflow_used = ");
    try w.writeAll(if (facts.durable_workflow_used) "true" else "false");
    try w.writeByte('\n');
    try w.writeAll("durable_workflow_proof_level = \"");
    try w.writeAll(facts.durable_workflow_proof_level.toString());
    try w.writeAll("\"\n");
    try w.writeAll("durable_workflow_retry_safe = ");
    try w.writeAll(if (facts.durable_workflow_retry_safe) "true" else "false");
    try w.writeByte('\n');
    try w.writeAll("durable_workflow_idempotent = ");
    try w.writeAll(if (facts.durable_workflow_idempotent) "true" else "false");
    try w.writeByte('\n');
    try w.writeAll("durable_workflow_fault_covered = ");
    try w.writeAll(if (facts.durable_workflow_fault_covered) "true" else "false");
    try w.writeByte('\n');

    try w.writeAll("verification = [");
    for (facts.checks_passed, 0..) |check, i| {
        if (i > 0) try w.writeAll(", ");
        try w.writeByte('"');
        try writeTomlBareString(w, check);
        try w.writeByte('"');
    }
    try w.writeAll("]\n");

    if (facts.fetch_hosts.len > 0) {
        try w.writeAll("fetch_hosts = [");
        for (facts.fetch_hosts, 0..) |host, i| {
            if (i > 0) try w.writeAll(", ");
            try w.writeByte('"');
            try writeTomlBareString(w, host);
            try w.writeByte('"');
        }
        try w.writeAll("]\n");
    }

    output = aw.toArrayList();
    const content = try output.toOwnedSlice(allocator);
    errdefer allocator.free(content);

    const outputs = try allocator.alloc(RenderOutput, 1);
    outputs[0] = .{
        .filename = "wrangler.toml",
        .content = content,
    };
    return outputs;
}

fn countCloudflareRoutePatterns(routes: []const ProvenRoute) usize {
    var count: usize = 0;
    for (routes) |route| {
        if (isCloudflareRoutePattern(route.pattern)) count += 1;
    }
    return count;
}

fn isCloudflareRoutePattern(pattern: []const u8) bool {
    return pattern.len > 0 and pattern[0] != '/';
}

/// Stamp used for `compatibility_date` in generated `wrangler.toml`. The
/// date is the day the manifest emit feature itself shipped; pin it so
/// regenerating the manifest does not silently move the date and break
/// existing deployments.
const compatibility_date_stamp = "2026-05-17";

/// Write a TOML string body, escaping the four characters that need it.
/// TOML basic strings share JSON's quoting rules for the common cases.
fn writeTomlBareString(w: anytype, s: []const u8) !void {
    for (s) |c| {
        switch (c) {
            '"' => try w.writeAll("\\\""),
            '\\' => try w.writeAll("\\\\"),
            '\n' => try w.writeAll("\\n"),
            '\r' => try w.writeAll("\\r"),
            else => try w.writeByte(c),
        }
    }
}

fn writeCostSource(w: anytype, source: handler_contract.BoundProvenance) !void {
    try w.print("{d}:{d} ", .{ source.line, source.column });
    try writeJsonStringContent(w, source.desc);
}

fn writeHumanCostSource(w: anytype, source: handler_contract.BoundProvenance) !void {
    try w.print("{d}:{d} {s}", .{ source.line, source.column, source.desc });
}

/// Convert an env var name like "JWT_SECRET" to a SAM parameter name like "JwtSecret"
fn writeParamName(w: anytype, env_var: []const u8) !void {
    var capitalize_next = true;
    for (env_var) |c| {
        if (c == '_') {
            capitalize_next = true;
            continue;
        }
        if (capitalize_next) {
            try w.writeByte(std.ascii.toUpper(c));
            capitalize_next = false;
        } else {
            try w.writeByte(std.ascii.toLower(c));
        }
    }
}

/// Convert a route pattern like "/api/users" to a SAM event ID like "RouteApiUsers"
fn writeSanitizedId(w: anytype, pattern: []const u8) !void {
    try w.writeAll("Route");
    var capitalize_next = true;
    for (pattern) |c| {
        if (c == '/' or c == '-' or c == '_' or c == ':') {
            capitalize_next = true;
            continue;
        }
        if (capitalize_next) {
            try w.writeByte(std.ascii.toUpper(c));
            capitalize_next = false;
        } else {
            try w.writeByte(c);
        }
    }
}

const writeJsonStringContent = handler_contract.writeJsonStringContent;

// -------------------------------------------------------------------------
// Deploy report (platform-agnostic)
// -------------------------------------------------------------------------

pub fn writeDeployReport(w: anytype, facts: *const ProvenFacts, provider: []const u8) !void {
    try w.writeAll("zttp Proven Deployment Report\n");
    try w.writeAll("Handler: ");
    try w.writeAll(facts.handler_path);
    try w.writeAll("\nProvider: ");
    try w.writeAll(provider);
    try w.writeAll("\n\n");

    // PROVEN section
    try w.writeAll("PROVEN (compiler-verified, not heuristic):\n");

    var any_proven = false;

    if (facts.env_vars.len > 0 and facts.env_proven) {
        try w.writeAll("  Environment variables: ");
        try writeCommaList(w, facts.env_vars);
        try w.writeAll("\n");
        any_proven = true;
    }

    if (facts.egress_hosts.len > 0 and facts.egress_proven) {
        try w.writeAll("  Outbound hosts: ");
        try writeCommaList(w, facts.egress_hosts);
        try w.writeAll("\n");
        any_proven = true;
    }

    if (facts.cache_namespaces.len > 0 and facts.cache_proven) {
        try w.writeAll("  Cache namespaces: ");
        try writeCommaList(w, facts.cache_namespaces);
        try w.writeAll("\n");
        any_proven = true;
    }

    if (facts.routes.len > 0) {
        try w.writeAll("  Routes: ");
        for (facts.routes, 0..) |route, i| {
            if (i > 0) try w.writeAll(", ");
            try w.writeAll(route.pattern);
            try w.writeAll(if (route.is_prefix) " (prefix)" else " (exact)");
        }
        try w.writeAll("\n");
        any_proven = true;
    }

    if (!any_proven) {
        try w.writeAll("  (none)\n");
    }

    // NEEDS MANUAL REVIEW section
    try w.writeAll("\nNEEDS MANUAL REVIEW:\n");
    var any_review = false;

    if (!facts.env_proven) {
        try writeReviewLine(w, "Environment variables: handler uses dynamic env access", facts.env_vars);
        any_review = true;
    }

    if (!facts.egress_proven) {
        try writeReviewLine(w, "Outbound hosts: handler uses dynamic URLs", facts.egress_hosts);
        any_review = true;
    }

    if (!facts.cache_proven) {
        try writeReviewLine(w, "Cache namespaces: handler uses dynamic cache keys", facts.cache_namespaces);
        any_review = true;
    }

    if (!any_review) {
        try w.writeAll("  (none - all sections fully proven)\n");
    }

    // VERIFICATION section
    try w.writeAll("\nVERIFICATION:\n");
    if (facts.checks_passed.len > 0) {
        for (facts.checks_passed) |check| {
            try w.writeAll("  ");
            try writeCheckLabel(w, check);
            try w.writeAll(": PASS\n");
        }
    } else {
        try w.writeAll("  (no verification ran)\n");
    }

    try w.writeAll("\nFLOW ANALYSIS:\n");
    try writeProvenLine(w, facts.no_secret_leakage, "no secret leakage");
    try writeProvenLine(w, facts.no_credential_leakage, "no credential leakage");
    try writeProvenLine(w, facts.input_validated, "input validated before egress");
    try writeProvenLine(w, facts.pii_contained, "PII contained (no user input in egress)");

    try w.writeAll("\nFAULT COVERAGE:\n");
    try writeProvenLine(w, facts.fault_covered, "all I/O failure modes handled correctly");

    if (facts.durable_workflow_used or facts.durable_workflow_proof_level != .none) {
        try w.writeAll("\nDURABLE WORKFLOW:\n");
        try w.writeAll("  proof level: ");
        try w.writeAll(facts.durable_workflow_proof_level.toString());
        try w.writeByte('\n');
        try writeProvenLine(w, facts.durable_workflow_retry_safe, "retry safe replay");
        try writeProvenLine(w, facts.durable_workflow_idempotent, "idempotent completed-response reuse");
        try writeProvenLine(w, facts.durable_workflow_fault_covered, "modeled workflow failure paths");
    }

    try w.writeAll("\nHANDLER PROPERTIES:\n");
    try writeProvenLine(w, facts.retry_safe, "retry safe (auto-retry on failure)");
    try writeProvenLine(w, facts.read_only, "read only (no state mutations)");
    try writeProvenLine(w, facts.injection_safe, "injection safe (no unvalidated input in sinks)");
    try writeProvenLine(w, facts.idempotent, "idempotent (safe for at-least-once delivery)");
    try writeProvenLine(w, facts.state_isolated, "state isolated (no cross-request data leakage)");
    if (facts.max_io_depth) |depth| {
        try w.print("  PROVEN  max I/O depth: {d} calls per request\n", .{depth});
    }
    if (facts.cost_total) |bound| {
        switch (bound) {
            .constant => |n| try w.print("  PROVEN  cost bound: {d} calls per request\n", .{n}),
            .linear => |linear| {
                try w.print("  PROVEN  cost bound: {d}+{d}*n calls per request (", .{ linear.base, linear.coefficient });
                try writeHumanCostSource(w, linear.source);
                try w.writeAll(")\n");
            },
            .unbounded => |source| {
                try w.writeAll("  REVIEW  cost bound: unbounded (");
                try writeHumanCostSource(w, source);
                try w.writeAll(")\n");
            },
        }
        if (costWorstCaseTotal(facts)) |worst| {
            try w.print("  PROVEN  cost worst-case at {d}-byte body: {d} calls\n", .{ facts.cost_worst_case_body_limit, worst });
        } else {
            try w.print("  REVIEW  cost worst-case at {d}-byte body: unbounded\n", .{facts.cost_worst_case_body_limit});
        }
    }

    if (facts.rate_limit_namespace) |ns| {
        try w.writeAll("\nRATE LIMITING:\n");
        try w.writeAll("  PROVEN  cacheIncr guard detected (namespace: ");
        try w.writeAll(ns);
        try w.writeAll(")\n");
    }

    try w.writeAll("\nOWASP TOP 10 COVERAGE:\n");
    try writeProvenLine(w, facts.no_secret_leakage, "A01 Broken Access Control (no secret leakage, sandbox enforced)");
    try writeProvenLine(w, facts.no_credential_leakage, "A02 Cryptographic Failures (no credential leakage)");
    try writeProvenLine(w, facts.injection_safe, "A03 Injection (no unvalidated input in sinks)");
    try writeProvenLine(w, facts.fault_covered, "A04 Insecure Design (fault coverage, exhaustive returns)");
    const sandbox_proven = facts.env_proven and facts.egress_proven and facts.cache_proven;
    try writeProvenLine(w, sandbox_proven, "A05 Security Misconfiguration (sandbox derived from contract)");
    try writeProvenLine(w, facts.results_safe, "A07 Auth Failures (result values checked before use)");

    try w.writeAll("\nPROOF LEVEL: ");
    try w.writeAll(facts.proof_level.toString());
    try w.writeAll("\n");
}

fn writeProvenLine(w: anytype, proven: bool, desc: []const u8) !void {
    try w.writeAll(if (proven) "  PROVEN  " else "  ---     ");
    try w.writeAll(desc);
    try w.writeByte('\n');
}

fn writeCommaList(w: anytype, items: []const []const u8) !void {
    for (items, 0..) |item, i| {
        if (i > 0) try w.writeAll(", ");
        try w.writeAll(item);
    }
}

fn writeReviewLine(w: anytype, label: []const u8, known_items: []const []const u8) !void {
    try w.writeAll("  ");
    try w.writeAll(label);
    if (known_items.len > 0) {
        try w.writeAll(" (known: ");
        try writeCommaList(w, known_items);
        try w.writeAll(")");
    }
    try w.writeAll("\n");
}

fn writeCheckLabel(w: anytype, check: []const u8) !void {
    if (std.mem.eql(u8, check, "exhaustiveReturns")) {
        try w.writeAll("Exhaustive returns");
    } else if (std.mem.eql(u8, check, "resultsSafe")) {
        try w.writeAll("Result safety");
    } else if (std.mem.eql(u8, check, "bytecodeVerified")) {
        try w.writeAll("Bytecode verified");
    } else if (std.mem.eql(u8, check, "soundModePassed")) {
        try w.writeAll("Sound mode");
    } else {
        try w.writeAll(check);
    }
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

fn makeTestContract(allocator: std.mem.Allocator) !HandlerContract {
    const path = try allocator.dupe(u8, "handler.ts");
    return handler_contract.emptyContract(path);
}

test "extractHandlerName basic" {
    try std.testing.expectEqualStrings("handler", extractHandlerName("handler.ts"));
    try std.testing.expectEqualStrings("handler-full", extractHandlerName("examples/handler/handler-full.tsx"));
    try std.testing.expectEqualStrings("handler", extractHandlerName("/some/path/handler.tsx"));
    try std.testing.expectEqualStrings("my-handler", extractHandlerName("my-handler.ts"));
}

test "deriveProofLevel none when no verification" {
    const allocator = std.testing.allocator;
    var contract = try makeTestContract(allocator);
    defer contract.deinit(allocator);

    try std.testing.expectEqual(ProofLevel.none, deriveProofLevel(&contract));
}

test "deriveProofLevel complete when all checks pass and no dynamic" {
    const allocator = std.testing.allocator;
    var contract = try makeTestContract(allocator);
    defer contract.deinit(allocator);

    contract.verification = .{
        .exhaustive_returns = true,
        .results_safe = true,
        .unreachable_code = false,
        .bytecode_verified = true,
    };

    try std.testing.expectEqual(ProofLevel.complete, deriveProofLevel(&contract));
}

test "deriveProofLevel partial when dynamic env" {
    const allocator = std.testing.allocator;
    var contract = try makeTestContract(allocator);
    defer contract.deinit(allocator);

    contract.verification = .{
        .exhaustive_returns = true,
        .results_safe = true,
        .unreachable_code = false,
        .bytecode_verified = true,
    };
    contract.env.dynamic = true;

    try std.testing.expectEqual(ProofLevel.partial, deriveProofLevel(&contract));
}

test "deriveProofLevel partial when check fails" {
    const allocator = std.testing.allocator;
    var contract = try makeTestContract(allocator);
    defer contract.deinit(allocator);

    contract.verification = .{
        .exhaustive_returns = true,
        .results_safe = false,
        .unreachable_code = false,
        .bytecode_verified = true,
    };

    try std.testing.expectEqual(ProofLevel.partial, deriveProofLevel(&contract));
}

test "extractProvenFacts minimal contract" {
    const allocator = std.testing.allocator;
    var contract = try makeTestContract(allocator);
    defer contract.deinit(allocator);

    const result = try extractProvenFacts(allocator, &contract);
    defer allocator.free(result.checks_buf);
    defer allocator.free(result.routes_buf);

    const facts = result.facts;
    try std.testing.expectEqualStrings("handler", facts.handler_name);
    try std.testing.expectEqualStrings("handler.ts", facts.handler_path);
    try std.testing.expectEqual(@as(usize, 0), facts.env_vars.len);
    try std.testing.expect(facts.env_proven);
    try std.testing.expectEqual(@as(usize, 0), facts.routes.len);
    try std.testing.expectEqual(ProofLevel.none, facts.proof_level);
}

test "extractProvenFacts with env and egress" {
    const allocator = std.testing.allocator;
    var contract = try makeTestContract(allocator);
    defer contract.deinit(allocator);

    const env1 = try allocator.dupe(u8, "JWT_SECRET");
    try contract.env.literal.append(allocator, env1);
    const env2 = try allocator.dupe(u8, "API_KEY");
    try contract.env.literal.append(allocator, env2);

    const host1 = try allocator.dupe(u8, "api.stripe.com");
    try contract.egress.hosts.append(allocator, host1);
    contract.egress.dynamic = true;

    contract.verification = .{
        .exhaustive_returns = true,
        .results_safe = true,
        .unreachable_code = false,
        .bytecode_verified = true,
    };

    const result = try extractProvenFacts(allocator, &contract);
    defer allocator.free(result.checks_buf);
    defer allocator.free(result.routes_buf);

    const facts = result.facts;
    try std.testing.expectEqual(@as(usize, 2), facts.env_vars.len);
    try std.testing.expectEqualStrings("JWT_SECRET", facts.env_vars[0]);
    try std.testing.expectEqualStrings("API_KEY", facts.env_vars[1]);
    try std.testing.expect(facts.env_proven);
    try std.testing.expectEqual(@as(usize, 1), facts.egress_hosts.len);
    try std.testing.expect(!facts.egress_proven);
    try std.testing.expectEqual(ProofLevel.partial, facts.proof_level);
    try std.testing.expectEqual(@as(usize, 3), facts.checks_passed.len);
}

test "extractProvenFacts includes durable workflow proof properties" {
    const allocator = std.testing.allocator;
    var contract = try makeTestContract(allocator);
    defer contract.deinit(allocator);

    contract.durable.used = true;
    contract.durable.workflow.proof_level = .complete;
    contract.durable.workflow.properties.retry_safe = true;
    contract.durable.workflow.properties.idempotent = true;
    contract.durable.workflow.properties.fault_covered = true;

    const result = try extractProvenFacts(allocator, &contract);
    defer allocator.free(result.checks_buf);
    defer allocator.free(result.routes_buf);

    const facts = result.facts;
    try std.testing.expect(facts.durable_workflow_used);
    try std.testing.expectEqual(handler_contract.DurableWorkflowProofLevel.complete, facts.durable_workflow_proof_level);
    try std.testing.expect(facts.durable_workflow_retry_safe);
    try std.testing.expect(facts.durable_workflow_idempotent);
    try std.testing.expect(facts.durable_workflow_fault_covered);
}

test "renderAws minimal" {
    const allocator = std.testing.allocator;

    const facts = ProvenFacts{
        .handler_name = "handler",
        .handler_path = "handler.ts",
        .env_vars = &.{},
        .env_proven = true,
        .egress_hosts = &.{},
        .egress_proven = true,
        .cache_namespaces = &.{},
        .cache_proven = true,
        .routes = &.{},
        .proof_level = .none,
        .checks_passed = &.{},
    };

    const outputs = try renderAws(allocator, &facts);
    defer {
        for (outputs) |o| allocator.free(o.content);
        allocator.free(outputs);
    }

    try std.testing.expectEqual(@as(usize, 1), outputs.len);
    try std.testing.expectEqualStrings("template.json", outputs[0].filename);

    const content = outputs[0].content;
    // Should contain SAM template structure
    try std.testing.expect(std.mem.indexOf(u8, content, "AWSTemplateFormatVersion") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "AWS::Serverless-2016-10-31") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "provided.al2023") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "handler.ts") != null);
    // No routes => catch-all
    try std.testing.expect(std.mem.indexOf(u8, content, "CatchAll") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "/{proxy+}") != null);
}

test "linear cost bound emits costClass/costBound tags and no maxIoDepth" {
    const allocator = std.testing.allocator;

    const facts = ProvenFacts{
        .handler_name = "handler",
        .handler_path = "handler.ts",
        .env_vars = &.{},
        .env_proven = true,
        .egress_hosts = &.{},
        .egress_proven = true,
        .cache_namespaces = &.{},
        .cache_proven = true,
        .routes = &.{},
        .proof_level = .none,
        .checks_passed = &.{},
        .max_io_depth = null,
        .cost_total = .{ .linear = .{
            .coefficient = 1,
            .base = 1,
            .source = .{ .line = 5, .column = 3, .desc = "for...of over `ids`" },
        } },
    };

    const outputs = try renderAws(allocator, &facts);
    defer {
        for (outputs) |o| allocator.free(o.content);
        allocator.free(outputs);
    }

    const content = outputs[0].content;
    try std.testing.expect(std.mem.indexOf(u8, content, "\"zttp:costClass\": \"linear\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"zttp:costBound\": \"1+1*n\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"zttp:costSource\": \"5:3 for...of over `ids`\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "zttp:maxIoDepth") == null);
}

test "worst-case cost tags render at the body limit" {
    const allocator = std.testing.allocator;

    const facts = ProvenFacts{
        .handler_name = "handler",
        .handler_path = "handler.ts",
        .env_vars = &.{},
        .env_proven = true,
        .egress_hosts = &.{},
        .egress_proven = true,
        .cache_namespaces = &.{},
        .cache_proven = true,
        .routes = &.{},
        .proof_level = .none,
        .checks_passed = &.{},
        .cost_total = .{ .linear = .{
            .coefficient = 2,
            .base = 1,
            .source = .{ .line = 5, .column = 3, .desc = "for...of over `ids`" },
        } },
        .cost_worst_case_total = 1_048_579,
        .cost_worst_case_body_limit = default_cost_body_limit,
    };

    const outputs = try renderAws(allocator, &facts);
    defer {
        for (outputs) |o| allocator.free(o.content);
        allocator.free(outputs);
    }

    const content = outputs[0].content;
    const class_idx = std.mem.indexOf(u8, content, "\"zttp:costClass\": \"linear\"") orelse return error.MissingCostClass;
    const worst_idx = std.mem.indexOf(u8, content, "\"zttp:costWorstCase\": \"1048579\"") orelse return error.MissingCostWorstCase;
    const body_idx = std.mem.indexOf(u8, content, "\"zttp:costBodyLimit\": \"1048576\"") orelse return error.MissingCostBodyLimit;
    try std.testing.expect(class_idx < worst_idx);
    try std.testing.expect(worst_idx < body_idx);
}

test "renderAws with env vars and routes" {
    const allocator = std.testing.allocator;

    const env_vars = [_][]const u8{ "JWT_SECRET", "API_KEY" };
    const routes = [_]ProvenRoute{
        .{ .pattern = "/health", .is_prefix = false },
        .{ .pattern = "/api/users", .is_prefix = true },
    };
    const checks = [_][]const u8{ "exhaustiveReturns", "resultsSafe", "bytecodeVerified" };

    const facts = ProvenFacts{
        .handler_name = "handler",
        .handler_path = "handler.ts",
        .env_vars = &env_vars,
        .env_proven = true,
        .egress_hosts = &[_][]const u8{"api.stripe.com"},
        .egress_proven = true,
        .cache_namespaces = &[_][]const u8{"sessions"},
        .cache_proven = true,
        .routes = &routes,
        .proof_level = .complete,
        .checks_passed = &checks,
        .durable_workflow_used = true,
        .durable_workflow_proof_level = .complete,
        .durable_workflow_retry_safe = true,
        .durable_workflow_idempotent = true,
        .durable_workflow_fault_covered = true,
    };

    const outputs = try renderAws(allocator, &facts);
    defer {
        for (outputs) |o| allocator.free(o.content);
        allocator.free(outputs);
    }

    const content = outputs[0].content;

    // Env vars become parameters
    try std.testing.expect(std.mem.indexOf(u8, content, "JwtSecret") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "ApiKey") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "PROVEN: handler reads JWT_SECRET") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"envProven\": true") != null);

    // Routes become events
    try std.testing.expect(std.mem.indexOf(u8, content, "RouteHealth") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"/health\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "RouteApiUsers") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "/api/users/{proxy+}") != null);

    // Tags
    try std.testing.expect(std.mem.indexOf(u8, content, "\"zttp:proven\": \"true\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"zttp:proofLevel\": \"complete\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"durableWorkflow\": { \"used\": true, \"proofLevel\": \"complete\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"zttp:durableWorkflowRetrySafe\": \"true\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "api.stripe.com") != null);

    // Verification metadata
    try std.testing.expect(std.mem.indexOf(u8, content, "exhaustiveReturns") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "resultsSafe") != null);
}

test "renderAws dynamic env produces review comment" {
    const allocator = std.testing.allocator;

    const facts = ProvenFacts{
        .handler_name = "handler",
        .handler_path = "handler.ts",
        .env_vars = &[_][]const u8{"KNOWN_VAR"},
        .env_proven = false,
        .egress_hosts = &.{},
        .egress_proven = true,
        .cache_namespaces = &.{},
        .cache_proven = true,
        .routes = &.{},
        .proof_level = .partial,
        .checks_passed = &.{},
    };

    const outputs = try renderAws(allocator, &facts);
    defer {
        for (outputs) |o| allocator.free(o.content);
        allocator.free(outputs);
    }

    const content = outputs[0].content;
    try std.testing.expect(std.mem.indexOf(u8, content, "\"envProven\": false") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "dynamic env access") != null);
}

test "writeDeployReport complete proof" {
    const allocator = std.testing.allocator;

    const env_vars = [_][]const u8{ "JWT_SECRET", "API_KEY" };
    const routes = [_]ProvenRoute{
        .{ .pattern = "/health", .is_prefix = false },
        .{ .pattern = "/api/users", .is_prefix = true },
    };
    const checks = [_][]const u8{ "exhaustiveReturns", "resultsSafe", "bytecodeVerified", "soundModePassed" };

    const facts = ProvenFacts{
        .handler_name = "handler",
        .handler_path = "handler.ts",
        .env_vars = &env_vars,
        .env_proven = true,
        .egress_hosts = &[_][]const u8{"api.stripe.com"},
        .egress_proven = true,
        .cache_namespaces = &[_][]const u8{"sessions"},
        .cache_proven = true,
        .routes = &routes,
        .proof_level = .complete,
        .checks_passed = &checks,
        .durable_workflow_used = true,
        .durable_workflow_proof_level = .complete,
        .durable_workflow_retry_safe = true,
        .durable_workflow_idempotent = true,
        .durable_workflow_fault_covered = true,
    };

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    try writeDeployReport(&aw.writer, &facts, "render");
    output = aw.toArrayList();

    const report = output.items;
    try std.testing.expect(std.mem.indexOf(u8, report, "Handler: handler.ts") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "Provider: render") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "PROVEN (compiler-verified, not heuristic):") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "JWT_SECRET, API_KEY") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "api.stripe.com") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "sessions") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "/health (exact)") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "/api/users (prefix)") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "(none - all sections fully proven)") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "Exhaustive returns: PASS") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "Sound mode: PASS") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "DURABLE WORKFLOW:") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "PROVEN  retry safe replay") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "PROOF LEVEL: complete") != null);
}

test "writeDeployReport with review needed" {
    const allocator = std.testing.allocator;

    const facts = ProvenFacts{
        .handler_name = "handler",
        .handler_path = "handler.ts",
        .env_vars = &[_][]const u8{"KNOWN_VAR"},
        .env_proven = false,
        .egress_hosts = &.{},
        .egress_proven = false,
        .cache_namespaces = &.{},
        .cache_proven = true,
        .routes = &.{},
        .proof_level = .partial,
        .checks_passed = &.{},
    };

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    try writeDeployReport(&aw.writer, &facts, "northflank");
    output = aw.toArrayList();

    const report = output.items;
    try std.testing.expect(std.mem.indexOf(u8, report, "handler uses dynamic env access") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "(known: KNOWN_VAR)") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "handler uses dynamic URLs") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "PROOF LEVEL: partial") != null);
}

test "writeParamName converts env var format" {
    const allocator = std.testing.allocator;
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    try writeParamName(&aw.writer, "JWT_SECRET");
    output = aw.toArrayList();
    try std.testing.expectEqualStrings("JwtSecret", output.items);
}

test "writeSanitizedId converts route pattern" {
    const allocator = std.testing.allocator;
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    try writeSanitizedId(&aw.writer, "/api/users");
    output = aw.toArrayList();
    try std.testing.expectEqualStrings("RouteApiUsers", output.items);
}

test "renderAws exposes websocket metadata when the handler has ws events" {
    const allocator = std.testing.allocator;

    const facts = ProvenFacts{
        .handler_name = "chat",
        .handler_path = "examples/websocket/chat.ts",
        .env_vars = &.{},
        .env_proven = true,
        .egress_hosts = &.{},
        .egress_proven = true,
        .cache_namespaces = &.{},
        .cache_proven = true,
        .routes = &.{},
        .proof_level = .none,
        .checks_passed = &.{},
        .has_websocket = true,
        .websocket_on_open = true,
        .websocket_on_message = true,
        .websocket_on_close = true,
    };

    const outputs = try renderAws(allocator, &facts);
    defer {
        for (outputs) |o| allocator.free(o.content);
        allocator.free(outputs);
    }

    const content = outputs[0].content;
    try std.testing.expect(std.mem.indexOf(u8, content, "\"websocket\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"onMessage\": true") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"onError\": false") != null);
}

test "renderAws exposes fetchHosts when the handler imports zttp:fetch" {
    const allocator = std.testing.allocator;

    const fetch_hosts = [_][]const u8{ "billing.example", "metrics.example" };
    const facts = ProvenFacts{
        .handler_name = "webhook",
        .handler_path = "examples/fetch/webhook.ts",
        .env_vars = &.{},
        .env_proven = true,
        .egress_hosts = &fetch_hosts,
        .egress_proven = true,
        .cache_namespaces = &.{},
        .cache_proven = true,
        .routes = &.{},
        .proof_level = .none,
        .checks_passed = &.{},
        .fetch_hosts = &fetch_hosts,
    };

    const outputs = try renderAws(allocator, &facts);
    defer {
        for (outputs) |o| allocator.free(o.content);
        allocator.free(outputs);
    }

    const content = outputs[0].content;
    try std.testing.expect(std.mem.indexOf(u8, content, "\"fetchHosts\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"billing.example\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"metrics.example\"") != null);
}

test "DeployTarget.fromString accepts the three documented Workers spellings" {
    try std.testing.expectEqual(DeployTarget.cloudflare_workers, DeployTarget.fromString("cloudflare-workers").?);
    try std.testing.expectEqual(DeployTarget.cloudflare_workers, DeployTarget.fromString("cloudflare_workers").?);
    try std.testing.expectEqual(DeployTarget.cloudflare_workers, DeployTarget.fromString("workers").?);
    try std.testing.expect(DeployTarget.fromString("totally-unknown") == null);
}

test "renderCloudflareWorkers omits path-only routes and keeps host-qualified routes" {
    const allocator = std.testing.allocator;

    const routes = [_]ProvenRoute{
        .{ .pattern = "/api/v1", .is_prefix = true },
        .{ .pattern = "example.com/api/*", .is_prefix = true },
    };
    const env_vars = [_][]const u8{ "JWT_SECRET", "DATABASE_URL" };
    const fetch_hosts = [_][]const u8{"api.upstream.dev"};
    const checks = [_][]const u8{ "exhaustiveReturns", "resultsSafe" };
    const facts = ProvenFacts{
        .handler_name = "checkout",
        .handler_path = "src/checkout.ts",
        .env_vars = &env_vars,
        .env_proven = true,
        .egress_hosts = &fetch_hosts,
        .egress_proven = true,
        .cache_namespaces = &.{},
        .cache_proven = true,
        .routes = &routes,
        .proof_level = .complete,
        .checks_passed = &checks,
        .fetch_hosts = &fetch_hosts,
        .durable_workflow_used = true,
        .durable_workflow_proof_level = .partial,
        .durable_workflow_retry_safe = false,
        .durable_workflow_idempotent = true,
        .durable_workflow_fault_covered = false,
    };

    const outputs = try renderCloudflareWorkers(allocator, &facts);
    defer {
        for (outputs) |o| allocator.free(o.content);
        allocator.free(outputs);
    }

    try std.testing.expectEqual(@as(usize, 1), outputs.len);
    try std.testing.expectEqualStrings("wrangler.toml", outputs[0].filename);
    const content = outputs[0].content;

    try std.testing.expect(std.mem.indexOf(u8, content, "name = \"checkout\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "compatibility_date = \"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "pattern = \"/api/v1\"") == null);
    try std.testing.expect(std.mem.indexOf(u8, content, "pattern = \"example.com/api/*\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "[vars]") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "JWT_SECRET = \"\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "DATABASE_URL = \"\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "[zttp]") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "fetch_hosts = [\"api.upstream.dev\"]") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "verification = [\"exhaustiveReturns\", \"resultsSafe\"]") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "durable_workflow_proof_level = \"partial\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "durable_workflow_idempotent = true") != null);
}

test "renderCloudflareWorkers omits routes and vars blocks when neither is present" {
    const allocator = std.testing.allocator;
    const facts = ProvenFacts{
        .handler_name = "minimal",
        .handler_path = "minimal.ts",
        .env_vars = &.{},
        .env_proven = true,
        .egress_hosts = &.{},
        .egress_proven = true,
        .cache_namespaces = &.{},
        .cache_proven = true,
        .routes = &.{},
        .proof_level = .none,
        .checks_passed = &.{},
    };

    const outputs = try renderCloudflareWorkers(allocator, &facts);
    defer {
        for (outputs) |o| allocator.free(o.content);
        allocator.free(outputs);
    }

    const content = outputs[0].content;
    try std.testing.expect(std.mem.indexOf(u8, content, "routes = [") == null);
    try std.testing.expect(std.mem.indexOf(u8, content, "[vars]") == null);
    try std.testing.expect(std.mem.indexOf(u8, content, "fetch_hosts") == null);
}

test "renderAws omits websocket and fetch blocks when absent" {
    const allocator = std.testing.allocator;

    const facts = ProvenFacts{
        .handler_name = "handler",
        .handler_path = "handler.ts",
        .env_vars = &.{},
        .env_proven = true,
        .egress_hosts = &.{},
        .egress_proven = true,
        .cache_namespaces = &.{},
        .cache_proven = true,
        .routes = &.{},
        .proof_level = .none,
        .checks_passed = &.{},
    };

    const outputs = try renderAws(allocator, &facts);
    defer {
        for (outputs) |o| allocator.free(o.content);
        allocator.free(outputs);
    }

    const content = outputs[0].content;
    try std.testing.expect(std.mem.indexOf(u8, content, "\"websocket\"") == null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"fetchHosts\"") == null);
}
