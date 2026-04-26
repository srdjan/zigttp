//! JSON deserializer for `HandlerContract`. Wire-format owner alongside
//! contract_json_writer.zig — round-trip is verified by tests in
//! handler_contract.zig. Extracted from handler_contract.zig as the second
//! step of the split. parseFromJson + JsonParser + the per-section parsing
//! helpers (~1850 lines) live here; the original file keeps a re-export so
//! `handler_contract.parseFromJson` and `handler_contract.JsonParser` still
//! resolve for external callers (prove.zig, prove_upgrade.zig,
//! system_linker.zig).

const std = @import("std");
const handler_contract = @import("handler_contract.zig");
const module_binding = @import("module_binding.zig");

const HandlerContract = handler_contract.HandlerContract;
const HandlerProperties = handler_contract.HandlerProperties;
const ServiceCallInfo = handler_contract.ServiceCallInfo;
const ApiInfo = handler_contract.ApiInfo;
const ApiAuthInfo = handler_contract.ApiAuthInfo;
const ApiRouteInfo = handler_contract.ApiRouteInfo;
const ApiParamInfo = handler_contract.ApiParamInfo;
const ApiBodyInfo = handler_contract.ApiBodyInfo;
const ApiResponseInfo = handler_contract.ApiResponseInfo;
const ApiSchemaInfo = handler_contract.ApiSchemaInfo;
const ApiRequestInfo = handler_contract.ApiRequestInfo;
const SchemaSpec = handler_contract.SchemaSpec;
const FaultCoverageInfo = handler_contract.FaultCoverageInfo;
const RateLimitInfo = handler_contract.RateLimitInfo;
const PathCondition = handler_contract.PathCondition;
const PathIoCall = handler_contract.PathIoCall;
const BehaviorPath = handler_contract.BehaviorPath;
const DurableWorkflow = handler_contract.DurableWorkflow;
const DurableWorkflowNode = handler_contract.DurableWorkflowNode;
const DurableWorkflowEdge = handler_contract.DurableWorkflowEdge;
const DurableWorkflowNodeKind = handler_contract.DurableWorkflowNodeKind;
const DurableWorkflowProofLevel = handler_contract.DurableWorkflowProofLevel;
const RouteInfo = handler_contract.RouteInfo;
const SqlQueryInfo = handler_contract.SqlQueryInfo;
const VerificationInfo = handler_contract.VerificationInfo;
const AotInfo = handler_contract.AotInfo;
const CapabilityMatrix = handler_contract.CapabilityMatrix;
const ModuleCapability = module_binding.ModuleCapability;
const capabilityHash = module_binding.capabilityHash;
const capability_count = module_binding.capability_count;

const emptyApiInfo = handler_contract.emptyApiInfo;
const emptySqlInfo = handler_contract.emptySqlInfo;
const dupeOptionalString = handler_contract.dupeOptionalString;

fn containsRequestBodySchemaRef(items: []const ApiBodyInfo, needle: []const u8) bool {
    for (items) |item| {
        if (item.schema.schemaRef()) |schema_ref| {
            if (std.mem.eql(u8, schema_ref, needle)) return true;
        }
    }
    return false;
}

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

        var service: []const u8 = try allocator.dupe(u8, "");
        var route_pattern: []const u8 = try allocator.dupe(u8, "");
        var dynamic_flag = false;
        var path_params: std.ArrayList([]const u8) = .empty;
        var path_params_dynamic = false;
        var query_keys: std.ArrayList([]const u8) = .empty;
        var query_dynamic_flag = false;
        var header_keys: std.ArrayList([]const u8) = .empty;
        var header_dynamic_flag = false;
        var has_body = false;
        var body_dynamic = false;
        errdefer {
            allocator.free(service);
            allocator.free(route_pattern);
            for (path_params.items) |s| allocator.free(s);
            path_params.deinit(allocator);
            for (query_keys.items) |s| allocator.free(s);
            query_keys.deinit(allocator);
            for (header_keys.items) |s| allocator.free(s);
            header_keys.deinit(allocator);
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

            if (std.mem.eql(u8, key, "service")) {
                allocator.free(service);
                service = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else if (std.mem.eql(u8, key, "route")) {
                allocator.free(route_pattern);
                route_pattern = try allocator.dupe(u8, parser.readString() orelse return error.InvalidJson);
            } else if (std.mem.eql(u8, key, "dynamic")) {
                dynamic_flag = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "pathParams")) {
                try parseStringArray(parser, allocator, &path_params);
            } else if (std.mem.eql(u8, key, "pathParamsDynamic")) {
                path_params_dynamic = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "queryKeys")) {
                try parseStringArray(parser, allocator, &query_keys);
            } else if (std.mem.eql(u8, key, "queryDynamic")) {
                query_dynamic_flag = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "headerKeys")) {
                try parseStringArray(parser, allocator, &header_keys);
            } else if (std.mem.eql(u8, key, "headerDynamic")) {
                header_dynamic_flag = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "hasBody")) {
                has_body = parser.readBool() orelse false;
            } else if (std.mem.eql(u8, key, "bodyDynamic")) {
                body_dynamic = parser.readBool() orelse false;
            } else {
                parser.skipValue();
            }
        }

        try contract.service_calls.append(allocator, .{
            .service = service,
            .route_pattern = route_pattern,
            .dynamic = dynamic_flag,
            .path_params = pickKnownList(allocator, &path_params, path_params_dynamic),
            .query_keys = pickKnownList(allocator, &query_keys, query_dynamic_flag),
            .header_keys = pickKnownList(allocator, &header_keys, header_dynamic_flag),
            .body = pickBodySpec(has_body, body_dynamic),
        });
        // Successful append moves ownership; null the locals so errdefer above
        // is a no-op.
        service = "";
        route_pattern = "";
        path_params = .empty;
        query_keys = .empty;
        header_keys = .empty;
    }
}

/// Discriminate accumulator into a KnownList. When the dynamic flag is set we
/// drop any partially-enumerated keys, since the linker treats `.dynamic` as
/// "cannot prove" and ignores partial state anyway.
fn pickKnownList(
    allocator: std.mem.Allocator,
    items: *std.ArrayList([]const u8),
    is_dynamic: bool,
) ServiceCallInfo.KnownList {
    if (is_dynamic) {
        for (items.items) |s| allocator.free(s);
        items.deinit(allocator);
        items.* = .empty;
        return .dynamic;
    }
    const taken = items.*;
    items.* = .empty;
    return .{ .complete = taken };
}

fn pickBodySpec(has_body: bool, body_dynamic: bool) ServiceCallInfo.BodySpec {
    if (body_dynamic) return .dynamic;
    if (has_body) return .present;
    return .none;
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

        var content_type: ?[]const u8 = null;
        var schema_ref: ?[]const u8 = null;
        var schema_json: ?[]const u8 = null;
        var is_dynamic = false;
        errdefer {
            if (content_type) |s| allocator.free(s);
            if (schema_ref) |s| allocator.free(s);
            if (schema_json) |s| allocator.free(s);
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

            if (std.mem.eql(u8, key, "contentType")) {
                content_type = if (parser.readNull()) null else try allocator.dupe(u8, parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "schemaRef")) {
                schema_ref = if (parser.readNull()) null else try allocator.dupe(u8, parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "schema")) {
                if (!parser.readNull()) {
                    const raw = parser.readRawValue() orelse return error.InvalidJson;
                    schema_json = try allocator.dupe(u8, raw);
                }
            } else if (std.mem.eql(u8, key, "dynamic")) {
                is_dynamic = parser.readBool() orelse false;
            } else {
                parser.skipValue();
            }
        }

        const schema = pickSchemaSpec(allocator, &schema_ref, &schema_json, is_dynamic);

        try list.append(allocator, .{
            .content_type = content_type,
            .schema = schema,
        });
        content_type = null;
    }
}

/// Discriminate accumulator into a SchemaSpec with precedence dynamic >
/// inline_json > ref > none. Older contracts may have populated more than one
/// schema field; we keep the strongest and free the rest, then null the
/// accumulator slots so the caller's errdefer doesn't double-free.
fn pickSchemaSpec(
    allocator: std.mem.Allocator,
    schema_ref: *?[]const u8,
    schema_json: *?[]const u8,
    is_dynamic: bool,
) SchemaSpec {
    if (is_dynamic) {
        if (schema_ref.*) |s| allocator.free(s);
        if (schema_json.*) |s| allocator.free(s);
        schema_ref.* = null;
        schema_json.* = null;
        return .dynamic;
    }
    if (schema_json.*) |s| {
        if (schema_ref.*) |r| allocator.free(r);
        schema_ref.* = null;
        schema_json.* = null;
        return .{ .inline_json = s };
    }
    if (schema_ref.*) |s| {
        schema_ref.* = null;
        return .{ .ref = s };
    }
    return .none;
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

        var status: ?u16 = null;
        var content_type: ?[]const u8 = null;
        var schema_ref: ?[]const u8 = null;
        var schema_json: ?[]const u8 = null;
        var is_dynamic = false;
        errdefer {
            if (content_type) |s| allocator.free(s);
            if (schema_ref) |s| allocator.free(s);
            if (schema_json) |s| allocator.free(s);
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

            if (std.mem.eql(u8, key, "status")) {
                status = if (parser.readNull()) null else (parser.readU16() orelse null);
            } else if (std.mem.eql(u8, key, "contentType")) {
                content_type = if (parser.readNull()) null else try allocator.dupe(u8, parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "schemaRef")) {
                schema_ref = if (parser.readNull()) null else try allocator.dupe(u8, parser.readString() orelse "");
            } else if (std.mem.eql(u8, key, "schema")) {
                if (!parser.readNull()) {
                    const raw = parser.readRawValue() orelse return error.InvalidJson;
                    schema_json = try allocator.dupe(u8, raw);
                }
            } else if (std.mem.eql(u8, key, "dynamic")) {
                is_dynamic = parser.readBool() orelse false;
            } else {
                parser.skipValue();
            }
        }

        const schema = pickSchemaSpec(allocator, &schema_ref, &schema_json, is_dynamic);

        try list.append(allocator, .{
            .status = status,
            .content_type = content_type,
            .schema = schema,
        });
        content_type = null;
    }
}

fn backfillApiRouteCollections(allocator: std.mem.Allocator, route: *ApiRouteInfo) !void {
    if (route.request_bodies.items.len == 0) {
        route.request_bodies_dynamic = route.request_bodies_dynamic or route.request_schema_dynamic;
        for (route.request_schema_refs.items) |schema_ref| {
            if (containsRequestBodySchemaRef(route.request_bodies.items, schema_ref)) continue;
            try route.request_bodies.append(allocator, .{
                .content_type = try allocator.dupe(u8, "application/json"),
                .schema = .{ .ref = try allocator.dupe(u8, schema_ref) },
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
            .schema = try schemaSpecFromLegacyFields(
                allocator,
                route.response_schema_ref,
                route.response_schema_json,
                route.response_schema_dynamic,
            ),
        });
        route.responses_dynamic = route.responses_dynamic or route.response_schema_dynamic;
    }
}

/// Mirror legacy scalar route response fields into a SchemaSpec, preserving
/// precedence dynamic > inline_json > ref > none and duplicating owned bytes
/// since the legacy fields stay alive on the route.
fn schemaSpecFromLegacyFields(
    allocator: std.mem.Allocator,
    schema_ref: ?[]const u8,
    schema_json: ?[]const u8,
    is_dynamic: bool,
) !SchemaSpec {
    if (is_dynamic) return .dynamic;
    if (schema_json) |s| return .{ .inline_json = try allocator.dupe(u8, s) };
    if (schema_ref) |s| return .{ .ref = try allocator.dupe(u8, s) };
    return .none;
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
        } else if (std.mem.eql(u8, key, "wasmPolicyHash")) {
            const hex = parser.readString() orelse return error.InvalidJson;
            if (hex.len == 64) {
                _ = std.fmt.hexToBytes(&contract.wasm_policy_hash, hex) catch return error.InvalidJson;
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
