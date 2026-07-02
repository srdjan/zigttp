//! JSON serializer for `HandlerContract`. Wire-format owner — any change here
//! must round-trip through `parseFromJson` (handler_contract.zig). Extracted
//! from handler_contract.zig as the first step of splitting that 7k-line file
//! along its natural seams.

const std = @import("std");
const json_utils = @import("json_utils.zig");
const handler_contract = @import("handler_contract.zig");
const contract_types = @import("contract_types.zig");

const HandlerContract = handler_contract.HandlerContract;
const ApiParamInfo = handler_contract.ApiParamInfo;
const ApiBodyInfo = handler_contract.ApiBodyInfo;
const ApiResponseInfo = handler_contract.ApiResponseInfo;
const ServiceCallInfo = handler_contract.ServiceCallInfo;

const writeJsonString = json_utils.writeJsonString;

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
    try writer.writeAll("    \"declaredBudget\": [");
    for (contract.capability_budget.slice(), 0..) |cap, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, @tagName(cap));
    }
    try writer.writeAll("],\n");
    try writer.writeAll("    \"policyHash\": ");
    try json_utils.writeJsonHex(writer, contract.policy_hash);
    try writer.writeAll(",\n");
    try writer.writeAll("    \"wasmPolicyHash\": ");
    try json_utils.writeJsonHex(writer, contract.wasm_policy_hash);
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
        try writeKnownListJson(writer, "pathParams", "pathParamsDynamic", service_call.path_params);
        try writeKnownListJson(writer, "queryKeys", "queryDynamic", service_call.query_keys);
        try writeKnownListJson(writer, "headerKeys", "headerDynamic", service_call.header_keys);
        try writer.print("      \"hasBody\": {s},\n", .{if (service_call.body.isPresent()) "true" else "false"});
        try writer.print("      \"bodyDynamic\": {s}\n", .{if (service_call.body.isDynamic()) "true" else "false"});
        try writer.writeAll("    }");
    }
    if (contract.service_calls.items.len > 0) {
        try writer.writeAll("\n  ");
    }
    try writer.writeAll("],\n");

    // affordances (hypermedia resource() links resolved by the system linker)
    try writer.writeAll("  \"affordances\": [");
    for (contract.affordances.items, 0..) |aff, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("\n    {\n");
        try writer.writeAll("      \"rel\": ");
        try writeJsonString(writer, aff.rel);
        try writer.writeAll(",\n");
        try writer.writeAll("      \"method\": ");
        try writeJsonString(writer, aff.method);
        try writer.writeAll(",\n");
        try writer.writeAll("      \"href\": ");
        try writeJsonString(writer, aff.href);
        try writer.writeAll(",\n");
        try writer.print("      \"templated\": {s},\n", .{if (aff.templated) "true" else "false"});
        try writer.print("      \"dynamic\": {s}\n", .{if (aff.dynamic) "true" else "false"});
        try writer.writeAll("    }");
    }
    if (contract.affordances.items.len > 0) {
        try writer.writeAll("\n  ");
    }
    try writer.writeAll("],\n");
    try writer.print("  \"affordancesDynamic\": {s},\n", .{if (contract.affordances_dynamic) "true" else "false"});

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
        // Persist the statement text: it is the identity discriminator that lets
        // `prove` classify a same-name query whose body changed as breaking.
        try writer.writeAll("        \"statement\": ");
        try writeJsonString(writer, query.statement);
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
    try writer.writeAll("      \"properties\": {\n");
    try writer.print("        \"retrySafe\": {s},\n", .{if (contract.durable.workflow.properties.retry_safe) "true" else "false"});
    try writer.print("        \"idempotent\": {s},\n", .{if (contract.durable.workflow.properties.idempotent) "true" else "false"});
    try writer.print("        \"faultCovered\": {s},\n", .{if (contract.durable.workflow.properties.fault_covered) "true" else "false"});
    try writer.writeAll("        \"reasons\": [");
    for (contract.durable.workflow.properties.reasons.items, 0..) |reason, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, reason);
    }
    try writer.writeAll("]\n");
    try writer.writeAll("      },\n");
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
        try writeBooleanProperties(p, writer);
        if (p.max_io_depth) |depth| {
            try writer.print("    \"maxIoDepth\": {d}\n", .{depth});
        } else {
            try writer.writeAll("    \"maxIoDepth\": null\n");
        }
        try writer.writeAll("  },\n");

        // proven_specs: canonical, ordered list of property names the
        // compiler currently proves true. This is the field cross-build
        // ratchet checks diff against; the unsorted properties object above
        // stays for backwards-compat consumers.
        var spec_buf: [contract_types.HandlerProperties.max_proven_specs]?[]const u8 = undefined;
        const proven_count = p.provenSpecNames(&spec_buf);
        try writer.writeAll("  \"provenSpecs\": [");
        for (spec_buf[0..proven_count], 0..) |name_opt, i| {
            if (name_opt) |nm| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("\"{s}\"", .{nm});
            }
        }
        try writer.writeAll("],\n");
    } else {
        try writer.writeAll("  \"properties\": null,\n");
        try writer.writeAll("  \"provenSpecs\": [],\n");
    }

    // intent (optional) - author-declared assertions outside the proof boundary
    if (contract.intent) |intent| {
        try writer.writeAll("  \"intent\": {\n");
        try writer.print("    \"dynamic\": {s},\n", .{if (intent.dynamic) "true" else "false"});
        try writer.writeAll("    \"assertions\": [");
        for (intent.assertions.items, 0..) |assertion, i| {
            if (i > 0) try writer.writeAll(", ");
            try writer.writeAll("\n      {");
            try writer.writeAll("\n        \"name\": ");
            try writeJsonString(writer, assertion.name);
            try writer.writeAll(",\n        \"method\": ");
            try writeJsonString(writer, assertion.method);
            try writer.writeAll(",\n        \"path\": ");
            try writeJsonString(writer, assertion.path);
            try writer.writeAll(",\n        \"requestBodyJson\": ");
            if (assertion.request_body_json) |b| {
                try writeJsonString(writer, b);
            } else try writer.writeAll("null");
            try writer.writeAll(",\n        \"expectedStatus\": ");
            if (assertion.expected_status) |s| {
                try writer.print("{d}", .{s});
            } else try writer.writeAll("null");
            try writer.writeAll(",\n        \"expectedBodyJson\": ");
            if (assertion.expected_body_json) |b| {
                try writeJsonString(writer, b);
            } else try writer.writeAll("null");
            try writer.writeAll(",\n        \"expectedHeaders\": [");
            for (assertion.expected_headers.items, 0..) |h, j| {
                if (j > 0) try writer.writeAll(", ");
                try writer.writeAll("{\"name\": ");
                try writeJsonString(writer, h.name);
                try writer.writeAll(", \"value\": ");
                try writeJsonString(writer, h.value);
                try writer.writeByte('}');
            }
            try writer.writeAll("],");
            try writer.print("\n        \"sourceLine\": {d},", .{assertion.source_line});
            try writer.print("\n        \"sourceColumn\": {d}", .{assertion.source_column});
            try writer.writeAll("\n      }");
        }
        if (intent.assertions.items.len > 0) try writer.writeByte('\n');
        try writer.writeAll("    ]\n");
        try writer.writeAll("  },\n");
    } else {
        try writer.writeAll("  \"intent\": null,\n");
    }

    // sagas - every saga([...]) call site found, with its compensation-
    // coverage proof verdict (ZTS510). Empty array when the handler never
    // imports zigttp:workflow's saga.
    try writer.writeAll("  \"sagas\": [");
    for (contract.sagas.items, 0..) |saga, i| {
        if (i > 0) try writer.writeAll(", ");
        try writer.writeAll("\n    {\n");
        try writer.print("      \"dynamic\": {s},\n", .{if (saga.dynamic) "true" else "false"});
        try writer.print("      \"compensationProven\": {s},\n", .{if (saga.compensationProven()) "true" else "false"});
        try writer.writeAll("      \"steps\": [");
        for (saga.steps.items, 0..) |step, j| {
            if (j > 0) try writer.writeAll(", ");
            try writer.writeAll("{\"name\": ");
            try writeJsonString(writer, step.name);
            try writer.print(", \"hasCompensate\": {s}}}", .{if (step.has_compensate) "true" else "false"});
        }
        try writer.writeAll("],\n");
        try writer.print("      \"sourceLine\": {d},\n", .{saga.source_line});
        try writer.print("      \"sourceColumn\": {d}\n", .{saga.source_column});
        try writer.writeAll("    }");
    }
    if (contract.sagas.items.len > 0) try writer.writeByte('\n');
    try writer.writeAll("  ],\n");

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
        try writer.print("  \"behaviorsExhaustive\": {s},\n", .{if (contract.behaviors_exhaustive) "true" else "false"});
    } else {
        try writer.writeAll("  \"behaviors\": [],\n");
        try writer.print("  \"behaviorsExhaustive\": {s},\n", .{if (contract.behaviors_exhaustive) "true" else "false"});
    }

    // declaredSpecs: effective active spec names. Source `Spec<...>`
    // narrows this set; without one it contains every supported v1 spec.
    try writer.writeAll("  \"declaredSpecs\": [");
    for (contract.declared_specs.items, 0..) |s, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, s);
    }
    try writer.writeAll("],\n");

    // specDiagnostics: per-spec discharge results emitted by the
    // verifier (ZTS500/501/502). Empty when every declared spec is
    // satisfied.
    try writer.writeAll("  \"specDiagnostics\": [");
    for (contract.spec_diagnostics.items, 0..) |d, i| {
        if (i > 0) try writer.writeAll(", ");
        try writer.writeAll("\n    {");
        try writer.writeAll("\n      \"kind\": ");
        try writeJsonString(writer, @tagName(d.kind));
        try writer.writeAll(",\n      \"code\": ");
        try writeJsonString(writer, d.kind.code());
        try writer.writeAll(",\n      \"specName\": ");
        try writeJsonString(writer, d.spec_name);
        if (d.function) |func| {
            try writer.writeAll(",\n      \"function\": ");
            try writeJsonString(writer, func);
        }
        if (d.incompatible_module) |module_name| {
            try writer.writeAll(",\n      \"incompatibleModule\": ");
            try writeJsonString(writer, module_name);
        }
        if (d.suggestion) |suggestion| {
            try writer.writeAll(",\n      \"suggestion\": ");
            try writeJsonString(writer, suggestion);
        }
        try writer.writeAll("\n    }");
    }
    if (contract.spec_diagnostics.items.len > 0) try writer.writeAll("\n  ");
    try writer.writeAll("],\n");

    try writeExtensionsJson(writer, contract);
    try writePartnerContractSections(writer, contract);

    try writer.writeAll("\n}\n");
}

/// Emit the `extensions` section: per-specifier facts produced by partner
/// virtual-module manifests. Keys are stable (sorted) so contract diff stays
/// deterministic. Always emitted, even when empty, so downstream readers
/// don't need a presence check.
fn writeExtensionsJson(
    writer: anytype,
    contract: *const HandlerContract,
) !void {
    try writer.writeAll("  \"extensions\": {");
    if (contract.extensions.count() == 0) {
        try writer.writeAll("}");
        return;
    }

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const spec_keys = try sortedHashMapKeys(a, &contract.extensions);
    for (spec_keys, 0..) |spec, i| {
        const ext = contract.extensions.getPtr(spec).?;
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("\n    ");
        try writeJsonString(writer, spec);
        try writer.writeAll(": {\n      \"egressHosts\": [");
        for (ext.egress_hosts.items, 0..) |host, j| {
            if (j > 0) try writer.writeAll(", ");
            try writeJsonString(writer, host);
        }
        try writer.writeAll("],\n");
        try writer.print("      \"egressDynamic\": {s},\n", .{if (ext.egress_dynamic) "true" else "false"});
        try writer.writeAll("      \"categories\": {");

        const tag_keys = try sortedHashMapKeys(a, &ext.categories);
        for (tag_keys, 0..) |tag, j| {
            const bucket = ext.categories.getPtr(tag).?;
            if (j > 0) try writer.writeAll(",");
            try writer.writeAll("\n        ");
            try writeJsonString(writer, tag);
            try writer.writeAll(": { \"literals\": [");
            for (bucket.literals.items, 0..) |lit, k| {
                if (k > 0) try writer.writeAll(", ");
                try writeJsonString(writer, lit);
            }
            try writer.print("], \"dynamic\": {s} }}", .{if (bucket.dynamic) "true" else "false"});
        }

        if (tag_keys.len > 0) try writer.writeAll("\n      ");
        try writer.writeAll("}");

        if (ext.contract_section) |section| {
            try writer.writeAll(",\n      \"contractSection\": ");
            try writeJsonString(writer, section);
        }

        try writer.writeAll("\n    }");
    }
    try writer.writeAll("\n  }");
}

/// Emit each partner-declared top-level section (one per extension with a
/// non-null `contract_section`). Each section is keyed by the partner's
/// chosen name and mirrors the source extension's category buckets, giving
/// partner audit tools parity with built-in sections like `cache` and
/// `durable` without forcing them to crawl the `extensions` namespace.
fn writePartnerContractSections(
    writer: anytype,
    contract: *const HandlerContract,
) !void {
    if (contract.extensions.count() == 0) return;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const spec_keys = try sortedHashMapKeys(a, &contract.extensions);
    for (spec_keys) |spec| {
        const ext = contract.extensions.getPtr(spec).?;
        const section = ext.contract_section orelse continue;

        try writer.writeAll(",\n  ");
        try writeJsonString(writer, section);
        try writer.writeAll(": {\n    \"sourceSpecifier\": ");
        try writeJsonString(writer, spec);
        try writer.writeAll(",\n    \"categories\": {");

        const tag_keys = try sortedHashMapKeys(a, &ext.categories);
        for (tag_keys, 0..) |tag, j| {
            const bucket = ext.categories.getPtr(tag).?;
            if (j > 0) try writer.writeAll(",");
            try writer.writeAll("\n      ");
            try writeJsonString(writer, tag);
            try writer.writeAll(": { \"literals\": [");
            for (bucket.literals.items, 0..) |lit, k| {
                if (k > 0) try writer.writeAll(", ");
                try writeJsonString(writer, lit);
            }
            try writer.print("], \"dynamic\": {s} }}", .{if (bucket.dynamic) "true" else "false"});
        }

        if (tag_keys.len > 0) try writer.writeAll("\n    ");
        try writer.writeAll("}\n  }");
    }
}

/// Snapshot the keys of a string-keyed StringHashMapUnmanaged into a sorted
/// slice. The slice borrows the map's key strings; callers must not mutate
/// the map while iterating.
fn sortedHashMapKeys(
    allocator: std.mem.Allocator,
    map: anytype,
) ![]const []const u8 {
    var keys = try allocator.alloc([]const u8, map.count());
    var it = map.iterator();
    var i: usize = 0;
    while (it.next()) |entry| : (i += 1) {
        keys[i] = entry.key_ptr.*;
    }
    std.mem.sort([]const u8, keys, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.lessThan(u8, a, b);
        }
    }.lessThan);
    return keys;
}

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

/// Emit a KnownList as the legacy array plus dynamic boolean, preserving
/// wire-format compatibility with older contract.json readers. `.dynamic`
/// writes an empty array and dynamic=true.
fn writeKnownListJson(
    writer: anytype,
    comptime field: []const u8,
    comptime dynamic_field: []const u8,
    list: ServiceCallInfo.KnownList,
) !void {
    try writer.writeAll("      \"" ++ field ++ "\": [");
    switch (list) {
        .complete => |entries| for (entries.items, 0..) |name, j| {
            if (j > 0) try writer.writeAll(", ");
            try writeJsonString(writer, name);
        },
        .dynamic => {},
    }
    try writer.writeAll("],\n");
    try writer.print("      \"" ++ dynamic_field ++ "\": {s},\n", .{if (list.isDynamic()) "true" else "false"});
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
    if (body.schema.schemaRef()) |schema_ref| {
        try writeJsonString(writer, schema_ref);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(",\n");
    try writer.writeAll("            \"schema\": ");
    if (body.schema.schemaJson()) |schema_json| {
        try writer.writeAll(schema_json);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(",\n");
    try writer.print("            \"dynamic\": {s}\n", .{if (body.schema.isDynamic()) "true" else "false"});
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
    if (response.schema.schemaRef()) |schema_ref| {
        try writeJsonString(writer, schema_ref);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(",\n");
    try writer.writeAll("            \"schema\": ");
    if (response.schema.schemaJson()) |schema_json| {
        try writer.writeAll(schema_json);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(",\n");
    try writer.print("            \"dynamic\": {s}\n", .{if (response.schema.isDynamic()) "true" else "false"});
    try writer.writeAll("          }");
}

/// Emit every boolean field of `HandlerProperties` as `"<camelCase>": true|false,\n`.
/// `maxIoDepth` is written separately by the caller because it is an optional
/// integer rather than a bool. Driven by `@typeInfo`, so adding a boolean
/// property field automatically appears in the JSON output; the camelCase
/// key comes from `HandlerProperties.camelKeyFor`.
fn writeBooleanProperties(p: contract_types.HandlerProperties, writer: anytype) !void {
    inline for (@typeInfo(contract_types.HandlerProperties).@"struct".fields) |field| {
        if (field.type == bool) {
            const camel = comptime contract_types.HandlerProperties.camelKeyFor(field.name).?;
            const value = @field(p, field.name);
            try writer.print("    \"{s}\": {s},\n", .{ camel, if (value) "true" else "false" });
        }
    }
}
