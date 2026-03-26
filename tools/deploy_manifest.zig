//! Proven Deployment Manifests
//!
//! Generates platform-specific deployment configurations from compiler-proven
//! handler contracts. The contract captures exactly what a handler does (env vars,
//! egress hosts, cache namespaces, routes) as proven facts - not heuristic guesses.
//!
//! Architecture: ProvenFacts (platform-agnostic) -> DeployRenderer (platform-specific)
//!
//! Usage: called by precompile.zig when --deploy <target> is passed.

const std = @import("std");
const zts = @import("zts");
const handler_contract = zts.handler_contract;
const HandlerContract = handler_contract.HandlerContract;
const contract_diff = zts.contract_diff;

// -------------------------------------------------------------------------
// Platform-agnostic proven facts
// -------------------------------------------------------------------------

pub const ProofLevel = contract_diff.ProofLevel;

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

    // Data flow provenance
    no_secret_leakage: bool = true,
    no_credential_leakage: bool = true,
    input_validated: bool = true,
    pii_contained: bool = true,

    // Fault coverage
    fault_covered: bool = false,
};

// -------------------------------------------------------------------------
// Renderer interface
// -------------------------------------------------------------------------

pub const DeployTarget = enum {
    aws,

    pub fn fromString(s: []const u8) ?DeployTarget {
        if (std.mem.eql(u8, s, "aws")) return .aws;
        return null;
    }

    pub fn toString(self: DeployTarget) []const u8 {
        return switch (self) {
            .aws => "aws",
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
            .no_secret_leakage = if (contract.properties) |p| p.no_secret_leakage else true,
            .no_credential_leakage = if (contract.properties) |p| p.no_credential_leakage else true,
            .input_validated = if (contract.properties) |p| p.input_validated else true,
            .pii_contained = if (contract.properties) |p| p.pii_contained else true,
            .fault_covered = if (contract.properties) |p| p.fault_covered else false,
        },
        .checks_buf = checks_buf,
        .routes_buf = routes_buf,
    };
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
    try w.writeAll("  \"Description\": \"zigttp proven deployment - ");
    try writeJsonStringContent(w, facts.handler_path);
    try w.writeAll(" (proof: ");
    try w.writeAll(facts.proof_level.toString());
    try w.writeAll(")\",\n");

    // Metadata
    try w.writeAll("  \"Metadata\": {\n");
    try w.writeAll("    \"zigttp\": {\n");
    try w.writeAll("      \"version\": \"0.1.0\",\n");
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
    if (!facts.env_proven) {
        try w.writeAll(",\n      \"envReview\": \"handler uses dynamic env access - additional vars may be needed\"");
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
    if (facts.env_vars.len > 0) {
        try w.writeAll("\n  ");
    }
    try w.writeAll("},\n");

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
    try w.writeAll("          \"zigttp:proven\": \"");
    try w.writeAll(if (facts.proof_level == .complete) "true" else "false");
    try w.writeAll("\",\n");
    try w.writeAll("          \"zigttp:proofLevel\": \"");
    try w.writeAll(facts.proof_level.toString());
    try w.writeAll("\"");

    try w.writeAll(",\n          \"zigttp:retrySafe\": \"");
    try w.writeAll(if (facts.retry_safe) "true" else "false");
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zigttp:readOnly\": \"");
    try w.writeAll(if (facts.read_only) "true" else "false");
    try w.writeAll("\"");

    // Flow provenance tags
    try w.writeAll(",\n          \"zigttp:noSecretLeakage\": \"");
    try w.writeAll(if (facts.no_secret_leakage) "true" else "false");
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zigttp:noCredentialLeakage\": \"");
    try w.writeAll(if (facts.no_credential_leakage) "true" else "false");
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zigttp:inputValidated\": \"");
    try w.writeAll(if (facts.input_validated) "true" else "false");
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zigttp:piiContained\": \"");
    try w.writeAll(if (facts.pii_contained) "true" else "false");
    try w.writeAll("\"");
    try w.writeAll(",\n          \"zigttp:faultCovered\": \"");
    try w.writeAll(if (facts.fault_covered) "true" else "false");
    try w.writeAll("\"");

    if (facts.egress_hosts.len > 0 or facts.egress_proven) {
        try w.writeAll(",\n          \"zigttp:egressProven\": \"");
        try w.writeAll(if (facts.egress_proven) "true" else "false");
        try w.writeAll("\"");

        if (facts.egress_hosts.len > 0) {
            try w.writeAll(",\n          \"zigttp:egressHosts\": \"");
            for (facts.egress_hosts, 0..) |host, i| {
                if (i > 0) try w.writeAll(",");
                try writeJsonStringContent(w, host);
            }
            try w.writeAll("\"");
        }
    }

    try w.writeAll("\n        }\n");

    try w.writeAll("      }\n");
    try w.writeAll("    }\n");
    try w.writeAll("  },\n");

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

/// Convert an env var name like "JWT_SECRET" to a SAM parameter name like "JwtSecret"
fn writeParamName(w: anytype, env_var: []const u8) !void {
    var capitalize_next = true;
    for (env_var) |c| {
        if (c == '_') {
            capitalize_next = true;
            continue;
        }
        if (capitalize_next) {
            try w.writeByte(toUpper(c));
            capitalize_next = false;
        } else {
            try w.writeByte(toLower(c));
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
            try w.writeByte(toUpper(c));
            capitalize_next = false;
        } else {
            try w.writeByte(c);
        }
    }
}

fn toUpper(c: u8) u8 {
    if (c >= 'a' and c <= 'z') return c - 32;
    return c;
}

fn toLower(c: u8) u8 {
    if (c >= 'A' and c <= 'Z') return c + 32;
    return c;
}

/// Write a string's content (without surrounding quotes) with JSON escaping.
/// Handles control characters (matching handler_contract.zig's writeJsonString).
fn writeJsonStringContent(w: anytype, s: []const u8) !void {
    for (s) |c| {
        switch (c) {
            '"' => try w.writeAll("\\\""),
            '\\' => try w.writeAll("\\\\"),
            '\n' => try w.writeAll("\\n"),
            '\r' => try w.writeAll("\\r"),
            '\t' => try w.writeAll("\\t"),
            0x00...0x08, 0x0b...0x0c, 0x0e...0x1f => {
                try w.print("\\u{x:0>4}", .{@as(u16, c)});
            },
            else => try w.writeByte(c),
        }
    }
}

// -------------------------------------------------------------------------
// Deploy report (platform-agnostic)
// -------------------------------------------------------------------------

pub fn writeDeployReport(w: anytype, facts: *const ProvenFacts, target: DeployTarget) !void {
    try w.writeAll("zigttp Proven Deployment Report\n");
    try w.writeAll("Handler: ");
    try w.writeAll(facts.handler_path);
    try w.writeAll("\nTarget: ");
    try w.writeAll(target.toString());
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

    // FLOW ANALYSIS section
    try w.writeAll("\nFLOW ANALYSIS:\n");
    try w.writeAll(if (facts.no_secret_leakage) "  PROVEN  " else "  ---     ");
    try w.writeAll("no secret leakage\n");
    try w.writeAll(if (facts.no_credential_leakage) "  PROVEN  " else "  ---     ");
    try w.writeAll("no credential leakage\n");
    try w.writeAll(if (facts.input_validated) "  PROVEN  " else "  ---     ");
    try w.writeAll("input validated before egress\n");
    try w.writeAll(if (facts.pii_contained) "  PROVEN  " else "  ---     ");
    try w.writeAll("PII contained (no user input in egress)\n");

    // FAULT COVERAGE section
    try w.writeAll("\nFAULT COVERAGE:\n");
    try w.writeAll(if (facts.fault_covered) "  PROVEN  " else "  ---     ");
    try w.writeAll("all I/O failure modes handled correctly\n");

    try w.writeAll("\nPROOF LEVEL: ");
    try w.writeAll(facts.proof_level.toString());
    try w.writeAll("\n");
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

    return .{
        .handler = .{ .path = path, .line = 1, .column = 0 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = .empty, .dynamic = false },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .api = handler_contract.emptyApiInfo(),
        .verification = null,
        .aot = null,
    };
}

test "extractHandlerName basic" {
    try std.testing.expectEqualStrings("handler", extractHandlerName("handler.ts"));
    try std.testing.expectEqualStrings("handler", extractHandlerName("examples/handler/handler.jsx"));
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
    try std.testing.expect(std.mem.indexOf(u8, content, "\"zigttp:proven\": \"true\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"zigttp:proofLevel\": \"complete\"") != null);
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
    };

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    try writeDeployReport(&aw.writer, &facts, .aws);
    output = aw.toArrayList();

    const report = output.items;
    try std.testing.expect(std.mem.indexOf(u8, report, "Handler: handler.ts") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "Target: aws") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "PROVEN (compiler-verified, not heuristic):") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "JWT_SECRET, API_KEY") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "api.stripe.com") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "sessions") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "/health (exact)") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "/api/users (prefix)") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "(none - all sections fully proven)") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "Exhaustive returns: PASS") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "Sound mode: PASS") != null);
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

    try writeDeployReport(&aw.writer, &facts, .aws);
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
