//! Handler Property Expectation Checker
//!
//! Validates that compiler-proven handler properties match declared expectations.
//! Expectations are authored as JSON alongside handler source and checked against
//! the HandlerContract produced by the compiler.
//!
//! Usage: called by precompile.zig when --expect-properties <path> is passed.
//!
//! JSON format:
//!   {
//!     "routes": [
//!       {
//!         "path": "/api/orders",
//!         "method": "GET",
//!         "expected": { "state_isolated": true, "injection_safe": true, "read_only": true }
//!       }
//!     ]
//!   }

const std = @import("std");
const zigts = @import("zigts");
const handler_contract = zigts.handler_contract;
const HandlerContract = handler_contract.HandlerContract;
const HandlerProperties = handler_contract.HandlerProperties;
const readFilePosix = zigts.file_io.readFile;

// -------------------------------------------------------------------------
// Result types
// -------------------------------------------------------------------------

pub const PropertyMismatch = struct {
    route_path: []const u8,
    route_method: []const u8,
    property: []const u8,
    expected: bool,
    actual: bool,
};

pub const ExpectationResult = struct {
    mismatches: []const PropertyMismatch,
    checked_routes: u32,
    passed: bool,
};

// -------------------------------------------------------------------------
// Parsed expectation types
// -------------------------------------------------------------------------

const PropertyExpectation = struct {
    field_name: []const u8, // struct field name (snake_case)
    expected: bool,
};

const RouteExpectation = struct {
    path: []const u8,
    method: []const u8,
    properties: []const PropertyExpectation,
};

const Expectations = struct {
    routes: []const RouteExpectation,
};

// -------------------------------------------------------------------------
// Bool field names for HandlerProperties (compile-time list)
// -------------------------------------------------------------------------

const bool_field_names = blk: {
    const fields = std.meta.fields(HandlerProperties);
    var count: usize = 0;
    for (fields) |f| {
        if (f.type == bool) count += 1;
    }
    var names: [count][]const u8 = undefined;
    var i: usize = 0;
    for (fields) |f| {
        if (f.type == bool) {
            names[i] = f.name;
            i += 1;
        }
    }
    break :blk names;
};

// -------------------------------------------------------------------------
// JSON parser (same pattern as handler_contract.zig)
// -------------------------------------------------------------------------

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

    fn readString(self: *JsonParser) ?[]const u8 {
        self.skipWhitespace();
        if (self.pos >= self.data.len or self.data[self.pos] != '"') return null;
        self.pos += 1;
        const start = self.pos;
        while (self.pos < self.data.len and self.data[self.pos] != '"') {
            if (self.data[self.pos] == '\\') self.pos += 1;
            if (self.pos < self.data.len) self.pos += 1;
        }
        const end = self.pos;
        if (self.pos < self.data.len) self.pos += 1;
        return self.data[start..end];
    }

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

    fn skipValue(self: *JsonParser) void {
        self.skipWhitespace();
        if (self.pos >= self.data.len) return;
        switch (self.data[self.pos]) {
            '"' => _ = self.readString(),
            '{' => self.skipObject(),
            '[' => self.skipArray(),
            't', 'f' => _ = self.readBool(),
            'n' => {
                if (self.pos + 4 <= self.data.len and std.mem.eql(u8, self.data[self.pos..][0..4], "null")) {
                    self.pos += 4;
                }
            },
            else => {
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
            _ = self.readString();
            self.skipWhitespace();
            _ = self.consume(':');
            self.skipValue();
            self.skipWhitespace();
        }
        if (self.pos < self.data.len) self.pos += 1;
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
            self.skipValue();
            self.skipWhitespace();
        }
        if (self.pos < self.data.len) self.pos += 1;
    }
};

// -------------------------------------------------------------------------
// Parsing
// -------------------------------------------------------------------------

/// Parse a property expectations JSON file.
/// All returned slices point into json_bytes (zero-copy).
/// Caller must free the returned route and property slices via allocator.
pub fn parseExpectations(allocator: std.mem.Allocator, json_bytes: []const u8) !Expectations {
    var parser = JsonParser.init(json_bytes);

    if (!parser.consume('{')) return error.InvalidJson;

    var routes: std.ArrayList(RouteExpectation) = .empty;
    errdefer {
        for (routes.items) |route| allocator.free(route.properties);
        routes.deinit(allocator);
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

        if (std.mem.eql(u8, key, "routes")) {
            try parseRouteExpectations(&parser, allocator, &routes);
        } else {
            parser.skipValue();
        }
    }

    return .{
        .routes = try routes.toOwnedSlice(allocator),
    };
}

fn parseRouteExpectations(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    routes: *std.ArrayList(RouteExpectation),
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

        var path: []const u8 = "";
        var method: []const u8 = "";
        var properties: std.ArrayList(PropertyExpectation) = .empty;
        errdefer properties.deinit(allocator);

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
                path = parser.readString() orelse return error.InvalidJson;
            } else if (std.mem.eql(u8, key, "method")) {
                method = parser.readString() orelse return error.InvalidJson;
            } else if (std.mem.eql(u8, key, "expected")) {
                try parseExpectedProperties(parser, allocator, &properties);
            } else {
                parser.skipValue();
            }
        }

        try routes.append(allocator, .{
            .path = path,
            .method = method,
            .properties = try properties.toOwnedSlice(allocator),
        });
    }
}

fn parseExpectedProperties(
    parser: *JsonParser,
    allocator: std.mem.Allocator,
    properties: *std.ArrayList(PropertyExpectation),
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

        const value = parser.readBool() orelse return error.InvalidJson;

        if (isKnownBoolProperty(key)) {
            try properties.append(allocator, .{
                .field_name = key,
                .expected = value,
            });
            continue;
        }

        std.debug.print("Warning: unknown property '{s}' in expectations, skipping\n", .{key});
    }
}

// -------------------------------------------------------------------------
// Checking
// -------------------------------------------------------------------------

/// Compare parsed expectations against a HandlerContract.
/// Returns an ExpectationResult with any mismatches.
/// The contract.properties being null means all properties are unknown - skip checking.
pub fn checkExpectations(
    allocator: std.mem.Allocator,
    expectations: *const Expectations,
    contract: *const HandlerContract,
) !ExpectationResult {
    var mismatches: std.ArrayList(PropertyMismatch) = .empty;
    errdefer mismatches.deinit(allocator);

    var checked: u32 = 0;

    for (expectations.routes) |route_exp| {
        if (!routeExists(contract, route_exp)) {
            std.debug.print("Warning: route {s} {s} not found in contract, skipping\n", .{
                route_exp.method, route_exp.path,
            });
            continue;
        }

        checked += 1;

        // If properties are null (unknown), skip property checks for this route
        const props = contract.properties orelse continue;

        for (route_exp.properties) |prop_exp| {
            // Use inline for to match field name and read the actual value
            const actual = getBoolProperty(&props, prop_exp.field_name) orelse continue;

            if (actual != prop_exp.expected) {
                try mismatches.append(allocator, makeMismatch(route_exp, prop_exp, actual));
            }
        }
    }

    const mismatches_slice = try mismatches.toOwnedSlice(allocator);
    return .{
        .mismatches = mismatches_slice,
        .checked_routes = checked,
        .passed = mismatches_slice.len == 0,
    };
}

fn isKnownBoolProperty(name: []const u8) bool {
    for (bool_field_names) |field_name| {
        if (std.mem.eql(u8, name, field_name)) return true;
    }
    return false;
}

fn routeExists(contract: *const HandlerContract, route_exp: RouteExpectation) bool {
    for (contract.api.routes.items) |api_route| {
        if (std.mem.eql(u8, api_route.path, route_exp.path) and
            std.mem.eql(u8, api_route.method, route_exp.method))
        {
            return true;
        }
    }

    for (contract.routes.items) |route| {
        if (std.mem.eql(u8, route.pattern, route_exp.path)) return true;
    }

    return false;
}

fn makeMismatch(
    route_exp: RouteExpectation,
    prop_exp: PropertyExpectation,
    actual: bool,
) PropertyMismatch {
    return .{
        .route_path = route_exp.path,
        .route_method = route_exp.method,
        .property = prop_exp.field_name,
        .expected = prop_exp.expected,
        .actual = actual,
    };
}

/// Look up a bool field on HandlerProperties by runtime name.
/// Uses inline for over struct fields to generate a comptime dispatch.
fn getBoolProperty(props: *const HandlerProperties, name: []const u8) ?bool {
    inline for (std.meta.fields(HandlerProperties)) |field| {
        if (field.type == bool) {
            if (std.mem.eql(u8, name, field.name)) {
                return @field(props, field.name);
            }
        }
    }
    return null;
}

// -------------------------------------------------------------------------
// Output
// -------------------------------------------------------------------------

/// Print expectation results to stderr in human-readable form.
pub fn printExpectationResults(result: *const ExpectationResult) void {
    std.debug.print("\n--- Property Expectations ---\n", .{});
    std.debug.print("Routes checked: {d}\n", .{result.checked_routes});

    if (result.passed) {
        std.debug.print("Result: PASS (all expectations met)\n\n", .{});
        return;
    }

    std.debug.print("Result: FAIL ({d} mismatch{s})\n\n", .{
        result.mismatches.len,
        if (result.mismatches.len == 1) "" else "es",
    });

    for (result.mismatches) |m| {
        std.debug.print("  {s} {s}\n", .{ m.route_method, m.route_path });
        std.debug.print("    {s}: expected={}, actual={}\n\n", .{
            m.property,
            m.expected,
            m.actual,
        });
    }
}

// -------------------------------------------------------------------------
// Convenience: read + parse + check in one call
// -------------------------------------------------------------------------

/// Read an expectations JSON file, parse it, and check against a contract.
/// Returns the result. Caller owns the returned mismatches slice.
pub fn checkExpectationsFromFile(
    allocator: std.mem.Allocator,
    path: []const u8,
    contract: *const HandlerContract,
) !ExpectationResult {
    const json_bytes = readFilePosix(allocator, path, 1024 * 1024) catch |err| {
        std.debug.print("Error reading expectations file '{s}': {}\n", .{ path, err });
        return err;
    };
    defer allocator.free(json_bytes);

    const expectations = try parseExpectations(allocator, json_bytes);
    defer freeExpectations(allocator, &expectations);

    return try checkExpectations(allocator, &expectations, contract);
}

fn freeExpectations(allocator: std.mem.Allocator, expectations: *const Expectations) void {
    for (expectations.routes) |route| {
        allocator.free(route.properties);
    }
    allocator.free(expectations.routes);
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

test "parseExpectations: basic route" {
    const allocator = std.testing.allocator;
    const json =
        \\{
        \\  "routes": [
        \\    {
        \\      "path": "/api/orders",
        \\      "method": "GET",
        \\      "expected": { "read_only": true, "state_isolated": true }
        \\    }
        \\  ]
        \\}
    ;

    const expectations = try parseExpectations(allocator, json);
    defer freeExpectations(allocator, &expectations);

    try std.testing.expectEqual(@as(usize, 1), expectations.routes.len);
    const route = expectations.routes[0];
    try std.testing.expectEqualStrings("/api/orders", route.path);
    try std.testing.expectEqualStrings("GET", route.method);
    try std.testing.expectEqual(@as(usize, 2), route.properties.len);
    try std.testing.expectEqualStrings("read_only", route.properties[0].field_name);
    try std.testing.expect(route.properties[0].expected);
    try std.testing.expectEqualStrings("state_isolated", route.properties[1].field_name);
    try std.testing.expect(route.properties[1].expected);
}

test "parseExpectations: multiple routes" {
    const allocator = std.testing.allocator;
    const json =
        \\{
        \\  "routes": [
        \\    { "path": "/api/a", "method": "GET", "expected": { "pure": true } },
        \\    { "path": "/api/b", "method": "POST", "expected": { "idempotent": false } }
        \\  ]
        \\}
    ;

    const expectations = try parseExpectations(allocator, json);
    defer freeExpectations(allocator, &expectations);

    try std.testing.expectEqual(@as(usize, 2), expectations.routes.len);
    try std.testing.expectEqualStrings("/api/a", expectations.routes[0].path);
    try std.testing.expectEqualStrings("/api/b", expectations.routes[1].path);
    try std.testing.expectEqualStrings("POST", expectations.routes[1].method);
}

test "getBoolProperty: reads correct fields" {
    const props = HandlerProperties{
        .pure = true,
        .read_only = false,
        .stateless = false,
        .retry_safe = true,
        .deterministic = false,
        .has_egress = false,
    };

    try std.testing.expectEqual(true, getBoolProperty(&props, "pure"));
    try std.testing.expectEqual(false, getBoolProperty(&props, "read_only"));
    try std.testing.expectEqual(true, getBoolProperty(&props, "retry_safe"));
    try std.testing.expectEqual(true, getBoolProperty(&props, "injection_safe")); // default true
    try std.testing.expectEqual(null, getBoolProperty(&props, "nonexistent"));
}

test "checkExpectations: pass when properties match" {
    const allocator = std.testing.allocator;

    const route_props = [_]PropertyExpectation{
        .{ .field_name = "read_only", .expected = true },
        .{ .field_name = "state_isolated", .expected = true },
    };
    const route_exps = [_]RouteExpectation{
        .{ .path = "/api/orders", .method = "GET", .properties = &route_props },
    };
    const expectations = Expectations{ .routes = &route_exps };

    // Build a minimal contract with matching route and properties
    var contract = emptyContract();
    defer contract.deinit(allocator);

    try contract.api.routes.append(allocator, .{
        .method = try allocator.dupe(u8, "GET"),
        .path = try allocator.dupe(u8, "/api/orders"),
        .request_schema_refs = .empty,
        .request_schema_dynamic = false,
        .requires_bearer = false,
        .requires_jwt = false,
    });

    contract.properties = .{
        .pure = false,
        .read_only = true,
        .stateless = false,
        .retry_safe = false,
        .deterministic = false,
        .has_egress = false,
        .state_isolated = true,
    };

    const result = try checkExpectations(allocator, &expectations, &contract);
    defer allocator.free(result.mismatches);

    try std.testing.expect(result.passed);
    try std.testing.expectEqual(@as(u32, 1), result.checked_routes);
    try std.testing.expectEqual(@as(usize, 0), result.mismatches.len);
}

test "checkExpectations: fail on mismatch" {
    const allocator = std.testing.allocator;

    const route_props = [_]PropertyExpectation{
        .{ .field_name = "read_only", .expected = true },
    };
    const route_exps = [_]RouteExpectation{
        .{ .path = "/api/orders", .method = "POST", .properties = &route_props },
    };
    const expectations = Expectations{ .routes = &route_exps };

    var contract = emptyContract();
    defer contract.deinit(allocator);

    try contract.api.routes.append(allocator, .{
        .method = try allocator.dupe(u8, "POST"),
        .path = try allocator.dupe(u8, "/api/orders"),
        .request_schema_refs = .empty,
        .request_schema_dynamic = false,
        .requires_bearer = false,
        .requires_jwt = false,
    });

    contract.properties = .{
        .pure = false,
        .read_only = false, // mismatch: expected true
        .stateless = false,
        .retry_safe = false,
        .deterministic = false,
        .has_egress = false,
    };

    const result = try checkExpectations(allocator, &expectations, &contract);
    defer allocator.free(result.mismatches);

    try std.testing.expect(!result.passed);
    try std.testing.expectEqual(@as(usize, 1), result.mismatches.len);
    try std.testing.expectEqualStrings("read_only", result.mismatches[0].property);
    try std.testing.expectEqual(true, result.mismatches[0].expected);
    try std.testing.expectEqual(false, result.mismatches[0].actual);
}

test "checkExpectations: null properties skips checks" {
    const allocator = std.testing.allocator;

    const route_props = [_]PropertyExpectation{
        .{ .field_name = "pure", .expected = true },
    };
    const route_exps = [_]RouteExpectation{
        .{ .path = "/api/orders", .method = "GET", .properties = &route_props },
    };
    const expectations = Expectations{ .routes = &route_exps };

    var contract = emptyContract();
    defer contract.deinit(allocator);

    try contract.api.routes.append(allocator, .{
        .method = try allocator.dupe(u8, "GET"),
        .path = try allocator.dupe(u8, "/api/orders"),
        .request_schema_refs = .empty,
        .request_schema_dynamic = false,
        .requires_bearer = false,
        .requires_jwt = false,
    });

    // properties is null (default) - all unknown
    const result = try checkExpectations(allocator, &expectations, &contract);
    defer allocator.free(result.mismatches);

    try std.testing.expect(result.passed);
    try std.testing.expectEqual(@as(u32, 1), result.checked_routes);
    try std.testing.expectEqual(@as(usize, 0), result.mismatches.len);
}

fn emptyContract() HandlerContract {
    return handler_contract.emptyContract(&.{});
}
