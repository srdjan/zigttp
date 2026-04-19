//! zigttp:validate - JSON Schema validation

const std = @import("std");
const sdk = @import("zigttp-sdk");

pub const MODULE_STATE_SLOT: usize = 4;

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:validate",
    .name = "validate",
    .stateful = true,
    .exports = &.{
        .{ .name = "schemaCompile", .module_func = schemaCompileImpl, .arg_count = 2, .returns = .boolean, .param_types = &.{.string}, .traceable = false, .contract_extractions = &.{.{ .category = .schema_compile }} },
        .{
            .name = "validateJson",
            .module_func = validateJsonImpl,
            .arg_count = 2,
            .returns = .result,
            .param_types = &.{ .string, .string },
            .failure_severity = .critical,
            .contract_extractions = &.{.{ .category = .request_schema }},
            .return_labels = .{ .validated = true },
            .laws = &.{.pure},
        },
        .{
            .name = "validateObject",
            .module_func = validateObjectImpl,
            .arg_count = 2,
            .returns = .result,
            .param_types = &.{ .string, .string },
            .failure_severity = .critical,
            .contract_extractions = &.{.{ .category = .request_schema }},
            .return_labels = .{ .validated = true },
            .laws = &.{.pure},
        },
        .{
            .name = "coerceJson",
            .module_func = coerceJsonImpl,
            .arg_count = 2,
            .returns = .result,
            .param_types = &.{ .string, .string },
            .failure_severity = .critical,
            .contract_extractions = &.{.{ .category = .request_schema }},
            .return_labels = .{ .validated = true },
            .laws = &.{.pure},
        },
        .{ .name = "schemaDrop", .module_func = schemaDropImpl, .arg_count = 1, .returns = .boolean, .param_types = &.{.string}, .traceable = false },
    },
};

const SchemaType = enum { string, number, integer, boolean, array, object_type, null_type };

const FormatType = enum {
    email,
    uuid,
    iso_date,
    iso_datetime,

    fn fromString(s: []const u8) ?FormatType {
        if (std.mem.eql(u8, s, "email")) return .email;
        if (std.mem.eql(u8, s, "uuid")) return .uuid;
        if (std.mem.eql(u8, s, "iso-date")) return .iso_date;
        if (std.mem.eql(u8, s, "iso-datetime")) return .iso_datetime;
        return null;
    }
};

const CompiledSchema = struct {
    schema_type: ?SchemaType = null,
    min_length: ?u32 = null,
    max_length: ?u32 = null,
    minimum: ?f64 = null,
    maximum: ?f64 = null,
    format: ?FormatType = null,
    required: ?[]const []const u8 = null,
    properties: ?std.StringHashMap(*CompiledSchema) = null,
    items: ?*CompiledSchema = null,
    enum_values: ?[]const []const u8 = null,

    fn deinit(self: *CompiledSchema, allocator: std.mem.Allocator) void {
        if (self.required) |r| {
            for (r) |s| allocator.free(s);
            allocator.free(r);
        }
        if (self.properties) |*props| {
            var it = props.iterator();
            while (it.next()) |entry| {
                entry.value_ptr.*.deinit(allocator);
                allocator.destroy(entry.value_ptr.*);
                allocator.free(entry.key_ptr.*);
            }
            props.deinit();
        }
        if (self.items) |child| {
            child.deinit(allocator);
            allocator.destroy(child);
        }
        if (self.enum_values) |enums| {
            for (enums) |s| allocator.free(s);
            allocator.free(enums);
        }
    }
};

pub const SchemaRegistry = struct {
    schemas: std.StringHashMap(*CompiledSchema),
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) SchemaRegistry {
        return .{
            .schemas = std.StringHashMap(*CompiledSchema).init(allocator),
            .allocator = allocator,
        };
    }

    fn deinitSelf(self: *SchemaRegistry) void {
        var it = self.schemas.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit(self.allocator);
            self.allocator.destroy(entry.value_ptr.*);
            self.allocator.free(entry.key_ptr.*);
        }
        self.schemas.deinit();
    }

    fn sdkDeinit(ptr: *anyopaque) callconv(.c) void {
        const reg: *SchemaRegistry = @ptrCast(@alignCast(ptr));
        const allocator = reg.allocator;
        reg.deinitSelf();
        allocator.destroy(reg);
    }
};

pub fn getOrCreateRegistry(handle: *sdk.ModuleHandle) !*SchemaRegistry {
    if (sdk.getModuleState(handle, SchemaRegistry, MODULE_STATE_SLOT)) |reg| return reg;
    const allocator = sdk.getAllocator(handle);
    const reg = try allocator.create(SchemaRegistry);
    reg.* = SchemaRegistry.init(allocator);
    try sdk.setModuleState(handle, MODULE_STATE_SLOT, @ptrCast(reg), SchemaRegistry.sdkDeinit);
    return reg;
}

fn schemaCompileImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.JSValue.false_val;
    const name = sdk.extractString(args[0]) orelse return sdk.JSValue.false_val;
    const schema_json = sdk.extractString(args[1]) orelse return sdk.JSValue.false_val;

    const reg = getOrCreateRegistry(handle) catch return sdk.JSValue.false_val;
    const allocator = reg.allocator;

    const parsed = std.json.parseFromSlice(std.json.Value, allocator, schema_json, .{}) catch
        return sdk.JSValue.false_val;
    defer parsed.deinit();

    const schema = compileSchemaFromJson(allocator, parsed.value) catch return sdk.JSValue.false_val;

    const name_owned = allocator.dupe(u8, name) catch {
        schema.deinit(allocator);
        allocator.destroy(schema);
        return sdk.JSValue.false_val;
    };

    if (reg.schemas.fetchPut(name_owned, schema) catch {
        allocator.free(name_owned);
        schema.deinit(allocator);
        allocator.destroy(schema);
        return sdk.JSValue.false_val;
    }) |old| {
        old.value.deinit(allocator);
        allocator.destroy(old.value);
        allocator.free(old.key);
    }

    return sdk.JSValue.true_val;
}

pub fn decodeJson(
    handle: *sdk.ModuleHandle,
    name: []const u8,
    json_str: []const u8,
    coerce: bool,
) !sdk.JSValue {
    const parsed_val = sdk.parseJson(handle, json_str) catch
        return sdk.resultErr(handle, "invalid JSON");
    return decodeValue(handle, name, parsed_val, coerce);
}

pub fn decodeObject(
    handle: *sdk.ModuleHandle,
    name: []const u8,
    input: sdk.JSValue,
    coerce: bool,
) !sdk.JSValue {
    return decodeValue(handle, name, input, coerce);
}

fn decodeValue(
    handle: *sdk.ModuleHandle,
    name: []const u8,
    input: sdk.JSValue,
    coerce: bool,
) !sdk.JSValue {
    const reg = getOrCreateRegistry(handle) catch return sdk.resultErr(handle, "internal error");
    const schema = reg.schemas.get(name) orelse return sdk.resultErr(handle, "schema not found");
    const candidate = if (coerce) coerceValue(handle, schema, input) else input;
    return validateValueAgainstSchema(handle, schema, candidate, "");
}

fn validateJsonImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.resultErr(handle, "missing arguments");
    const name = sdk.extractString(args[0]) orelse return sdk.resultErr(handle, "name must be a string");
    const json_str = sdk.extractString(args[1]) orelse return sdk.resultErr(handle, "json must be a string");
    return decodeJson(handle, name, json_str, false);
}

fn validateObjectImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.resultErr(handle, "missing arguments");
    const name = sdk.extractString(args[0]) orelse return sdk.resultErr(handle, "name must be a string");
    return decodeObject(handle, name, args[1], false);
}

fn coerceJsonImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.resultErr(handle, "missing arguments");
    const name = sdk.extractString(args[0]) orelse return sdk.resultErr(handle, "name must be a string");
    const json_str = sdk.extractString(args[1]) orelse return sdk.resultErr(handle, "json must be a string");
    return decodeJson(handle, name, json_str, true);
}

fn schemaDropImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.false_val;
    const name = sdk.extractString(args[0]) orelse return sdk.JSValue.false_val;
    const reg = sdk.getModuleState(handle, SchemaRegistry, MODULE_STATE_SLOT) orelse return sdk.JSValue.false_val;
    if (reg.schemas.fetchRemove(name)) |entry| {
        entry.value.deinit(reg.allocator);
        reg.allocator.destroy(entry.value);
        reg.allocator.free(entry.key);
        return sdk.JSValue.true_val;
    }
    return sdk.JSValue.false_val;
}

fn compileSchemaFromJson(allocator: std.mem.Allocator, json_val: std.json.Value) !*CompiledSchema {
    const schema = try allocator.create(CompiledSchema);
    errdefer allocator.destroy(schema);
    schema.* = .{};

    const obj = switch (json_val) {
        .object => |o| o,
        else => return schema,
    };

    if (obj.get("type")) |type_val| switch (type_val) {
        .string => |s| schema.schema_type = parseSchemaType(s),
        else => {},
    };
    if (obj.get("minLength")) |v| schema.min_length = jsonToU32(v);
    if (obj.get("maxLength")) |v| schema.max_length = jsonToU32(v);
    if (obj.get("minimum")) |v| schema.minimum = jsonToF64(v);
    if (obj.get("maximum")) |v| schema.maximum = jsonToF64(v);

    if (obj.get("required")) |req_val| switch (req_val) {
        .array => |arr| {
            var list: std.ArrayList([]const u8) = .empty;
            errdefer {
                for (list.items) |item| allocator.free(item);
                list.deinit(allocator);
            }
            for (arr.items) |item| switch (item) {
                .string => |s| try list.append(allocator, try allocator.dupe(u8, s)),
                else => {},
            };
            schema.required = try list.toOwnedSlice(allocator);
        },
        else => {},
    };

    if (obj.get("properties")) |props_val| switch (props_val) {
        .object => |props_obj| {
            var props = std.StringHashMap(*CompiledSchema).init(allocator);
            errdefer {
                var it = props.iterator();
                while (it.next()) |entry| {
                    entry.value_ptr.*.deinit(allocator);
                    allocator.destroy(entry.value_ptr.*);
                    allocator.free(entry.key_ptr.*);
                }
                props.deinit();
            }
            var it = props_obj.iterator();
            while (it.next()) |entry| {
                const child = try compileSchemaFromJson(allocator, entry.value_ptr.*);
                const key = try allocator.dupe(u8, entry.key_ptr.*);
                try props.put(key, child);
            }
            schema.properties = props;
        },
        else => {},
    };

    if (obj.get("items")) |items_val| {
        schema.items = try compileSchemaFromJson(allocator, items_val);
    }

    if (obj.get("enum")) |enum_val| switch (enum_val) {
        .array => |arr| {
            var list: std.ArrayList([]const u8) = .empty;
            errdefer {
                for (list.items) |item| allocator.free(item);
                list.deinit(allocator);
            }
            for (arr.items) |item| {
                const serialized = jsonValueToString(allocator, item) catch continue;
                try list.append(allocator, serialized);
            }
            schema.enum_values = try list.toOwnedSlice(allocator);
        },
        else => {},
    };

    if (obj.get("format")) |format_val| switch (format_val) {
        .string => |s| schema.format = FormatType.fromString(s),
        else => {},
    };

    return schema;
}

fn parseSchemaType(s: []const u8) ?SchemaType {
    if (std.mem.eql(u8, s, "string")) return .string;
    if (std.mem.eql(u8, s, "number")) return .number;
    if (std.mem.eql(u8, s, "integer")) return .integer;
    if (std.mem.eql(u8, s, "boolean")) return .boolean;
    if (std.mem.eql(u8, s, "array")) return .array;
    if (std.mem.eql(u8, s, "object")) return .object_type;
    if (std.mem.eql(u8, s, "null")) return .null_type;
    return null;
}

fn jsonToU32(v: std.json.Value) ?u32 {
    return switch (v) {
        .integer => |i| if (i >= 0) @intCast(i) else null,
        .float => |f| blk: {
            const i: i64 = @intFromFloat(f);
            break :blk if (i >= 0) @intCast(i) else null;
        },
        else => null,
    };
}

fn jsonToF64(v: std.json.Value) ?f64 {
    return switch (v) {
        .integer => |i| @floatFromInt(i),
        .float => |f| f,
        else => null,
    };
}

fn jsonValueToString(allocator: std.mem.Allocator, val: std.json.Value) ![]const u8 {
    return switch (val) {
        .string => |s| try std.fmt.allocPrint(allocator, "\"{s}\"", .{s}),
        .integer => |i| try std.fmt.allocPrint(allocator, "{d}", .{i}),
        .float => |f| try std.fmt.allocPrint(allocator, "{d}", .{f}),
        .bool => |b| try allocator.dupe(u8, if (b) "true" else "false"),
        .null => try allocator.dupe(u8, "null"),
        else => error.OutOfMemory,
    };
}

const ValidationError = struct {
    path: []const u8,
    message: []const u8,
};

fn validateValueAgainstSchema(
    handle: *sdk.ModuleHandle,
    schema: *const CompiledSchema,
    val: sdk.JSValue,
    path: []const u8,
) !sdk.JSValue {
    const allocator = sdk.getAllocator(handle);
    var errors: std.ArrayList(ValidationError) = .empty;
    defer errors.deinit(allocator);

    validateRecursive(handle, schema, val, path, &errors);

    if (errors.items.len == 0) return sdk.resultOk(handle, val);

    const err_array = try sdk.createArray(handle);
    for (errors.items, 0..) |err_item, i| {
        const err_obj = try sdk.createObject(handle);
        const path_val = try sdk.createString(handle, err_item.path);
        const msg_val = try sdk.createString(handle, err_item.message);
        try sdk.objectSet(handle, err_obj, "path", path_val);
        try sdk.objectSet(handle, err_obj, "message", msg_val);
        try sdk.arraySet(handle, err_array, @intCast(i), err_obj);
    }
    return sdk.resultErrs(handle, err_array);
}

fn validateFormat(fmt: FormatType, str: []const u8) bool {
    return switch (fmt) {
        .email => validateEmail(str),
        .uuid => validateUuid(str),
        .iso_date => validateIsoDate(str),
        .iso_datetime => validateIsoDatetime(str),
    };
}

fn validateEmail(str: []const u8) bool {
    var at_index: ?usize = null;
    for (str, 0..) |c, i| {
        if (c == '@') {
            if (at_index != null) return false;
            at_index = i;
        }
    }
    const at = at_index orelse return false;
    if (at == 0) return false;
    const domain = str[at + 1 ..];
    if (domain.len == 0) return false;
    return std.mem.indexOfScalar(u8, domain, '.') != null;
}

fn validateUuid(str: []const u8) bool {
    if (str.len != 36) return false;
    for (str, 0..) |c, i| {
        if (i == 8 or i == 13 or i == 18 or i == 23) {
            if (c != '-') return false;
        } else if (!isHexDigit(c)) return false;
    }
    return true;
}

fn validateIsoDate(str: []const u8) bool {
    if (str.len != 10) return false;
    return isDigit(str[0]) and isDigit(str[1]) and isDigit(str[2]) and isDigit(str[3]) and
        str[4] == '-' and
        isDigit(str[5]) and isDigit(str[6]) and
        str[7] == '-' and
        isDigit(str[8]) and isDigit(str[9]);
}

fn validateIsoDatetime(str: []const u8) bool {
    if (str.len < 19) return false;
    if (!validateIsoDate(str[0..10])) return false;
    if (str[10] != 'T') return false;
    if (!isDigit(str[11]) or !isDigit(str[12])) return false;
    if (str[13] != ':') return false;
    if (!isDigit(str[14]) or !isDigit(str[15])) return false;
    if (str[16] != ':') return false;
    if (!isDigit(str[17]) or !isDigit(str[18])) return false;
    return true;
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isHexDigit(c: u8) bool {
    return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}

fn validateRecursive(
    handle: *sdk.ModuleHandle,
    schema: *const CompiledSchema,
    val: sdk.JSValue,
    path: []const u8,
    errors: *std.ArrayList(ValidationError),
) void {
    const allocator = sdk.getAllocator(handle);

    if (schema.schema_type) |expected_type| {
        const type_ok = switch (expected_type) {
            .string => sdk.isString(val),
            .number => val.isNumber(),
            .integer => val.isInt() or (val.isNumber() and isWholeNumber(sdk.extractFloat(val) orelse 0)),
            .boolean => val.isBool(),
            .array => sdk.isArray(val),
            .object_type => sdk.isObject(val) and !sdk.isArray(val),
            .null_type => val.isNull(),
        };
        if (!type_ok) {
            errors.append(allocator, .{ .path = path, .message = "type mismatch" }) catch {};
            return;
        }
    }

    if (schema.schema_type != null and schema.schema_type.? == .string) {
        if (sdk.extractString(val)) |str| {
            if (schema.min_length) |min| {
                if (str.len < min) errors.append(allocator, .{ .path = path, .message = "string too short" }) catch {};
            }
            if (schema.max_length) |max| {
                if (str.len > max) errors.append(allocator, .{ .path = path, .message = "string too long" }) catch {};
            }
            if (schema.format) |fmt| {
                if (!validateFormat(fmt, str)) {
                    const msg: []const u8 = switch (fmt) {
                        .email => "invalid email format",
                        .uuid => "invalid uuid format",
                        .iso_date => "invalid iso-date format",
                        .iso_datetime => "invalid iso-datetime format",
                    };
                    errors.append(allocator, .{ .path = path, .message = msg }) catch {};
                }
            }
        }
    }

    if (schema.schema_type != null and (schema.schema_type.? == .number or schema.schema_type.? == .integer)) {
        if (sdk.extractFloat(val)) |num| {
            if (schema.minimum) |min| {
                if (num < min) errors.append(allocator, .{ .path = path, .message = "below minimum" }) catch {};
            }
            if (schema.maximum) |max| {
                if (num > max) errors.append(allocator, .{ .path = path, .message = "above maximum" }) catch {};
            }
        }
    }

    if (schema.enum_values) |enums| {
        const val_str = jsValueToEnumString(allocator, val) catch null;
        if (val_str) |vs| {
            defer allocator.free(vs);
            var found = false;
            for (enums) |e| {
                if (std.mem.eql(u8, vs, e)) {
                    found = true;
                    break;
                }
            }
            if (!found) errors.append(allocator, .{ .path = path, .message = "value not in enum" }) catch {};
        } else {
            errors.append(allocator, .{ .path = path, .message = "value not in enum" }) catch {};
        }
    }

    if (sdk.isObject(val) and !sdk.isArray(val)) {
        if (schema.required) |req| {
            for (req) |field_name| {
                if (sdk.objectGet(handle, val, field_name) == null) {
                    errors.append(allocator, .{ .path = path, .message = "missing required field" }) catch {};
                }
            }
        }
        if (schema.properties) |props| {
            var it = props.iterator();
            while (it.next()) |entry| {
                if (sdk.objectGet(handle, val, entry.key_ptr.*)) |prop_val| {
                    const child_path = buildPath(allocator, path, entry.key_ptr.*) catch path;
                    defer if (child_path.ptr != path.ptr) allocator.free(child_path);
                    validateRecursive(handle, entry.value_ptr.*, prop_val, child_path, errors);
                }
            }
        }
    }

    if (sdk.isArray(val) and schema.items != null) {
        const len = sdk.arrayLength(val) orelse return;
        const items_schema = schema.items.?;
        for (0..len) |i| {
            if (sdk.arrayGet(handle, val, @intCast(i))) |elem| {
                const child_path = buildIndexPath(allocator, path, i) catch path;
                defer if (child_path.ptr != path.ptr) allocator.free(child_path);
                validateRecursive(handle, items_schema, elem, child_path, errors);
            }
        }
    }
}

fn isWholeNumber(f: f64) bool {
    return f == @as(f64, @floatFromInt(@as(i64, @intFromFloat(f))));
}

fn buildPath(allocator: std.mem.Allocator, prefix: []const u8, field: []const u8) ![]const u8 {
    if (prefix.len == 0) return try allocator.dupe(u8, field);
    return try std.fmt.allocPrint(allocator, "{s}.{s}", .{ prefix, field });
}

fn buildIndexPath(allocator: std.mem.Allocator, prefix: []const u8, index: usize) ![]const u8 {
    if (prefix.len == 0) return try std.fmt.allocPrint(allocator, "[{d}]", .{index});
    return try std.fmt.allocPrint(allocator, "{s}[{d}]", .{ prefix, index });
}

fn jsValueToEnumString(allocator: std.mem.Allocator, val: sdk.JSValue) ![]const u8 {
    if (val.isNull()) return try allocator.dupe(u8, "null");
    if (val.isBool()) return try allocator.dupe(u8, if (val.isTrue()) "true" else "false");
    if (val.isInt()) return try std.fmt.allocPrint(allocator, "{d}", .{val.getInt()});
    if (sdk.extractFloat(val)) |f| return try std.fmt.allocPrint(allocator, "{d}", .{f});
    if (sdk.extractString(val)) |str| return try std.fmt.allocPrint(allocator, "\"{s}\"", .{str});
    return error.OutOfMemory;
}

pub fn coerceValue(handle: *sdk.ModuleHandle, schema: *const CompiledSchema, val: sdk.JSValue) sdk.JSValue {
    const expected_type = schema.schema_type orelse return val;

    switch (expected_type) {
        .number, .integer => {
            if (sdk.extractString(val)) |str| return parseNumberString(str);
        },
        .boolean => {
            if (sdk.extractString(val)) |str| {
                if (std.mem.eql(u8, str, "true")) return sdk.JSValue.true_val;
                if (std.mem.eql(u8, str, "false")) return sdk.JSValue.false_val;
            }
        },
        .string => {
            if (val.isInt()) {
                var buf: [32]u8 = undefined;
                const s = std.fmt.bufPrint(&buf, "{d}", .{val.getInt()}) catch return val;
                return sdk.createString(handle, s) catch val;
            }
            if (sdk.extractFloat(val)) |f| {
                var buf: [32]u8 = undefined;
                const s = std.fmt.bufPrint(&buf, "{d}", .{f}) catch return val;
                return sdk.createString(handle, s) catch val;
            }
        },
        .object_type => {
            if (sdk.isObject(val) and !sdk.isArray(val)) {
                if (schema.properties) |props| {
                    var it = props.iterator();
                    while (it.next()) |entry| {
                        if (sdk.objectGet(handle, val, entry.key_ptr.*)) |prop_val| {
                            const coerced = coerceValue(handle, entry.value_ptr.*, prop_val);
                            sdk.objectSet(handle, val, entry.key_ptr.*, coerced) catch {};
                        }
                    }
                }
            }
        },
        .array => {
            if (sdk.isArray(val) and schema.items != null) {
                const item_schema = schema.items.?;
                const len = sdk.arrayLength(val) orelse return val;
                var i: u32 = 0;
                while (i < len) : (i += 1) {
                    if (sdk.arrayGet(handle, val, i)) |elem| {
                        sdk.arraySet(handle, val, i, coerceValue(handle, item_schema, elem)) catch {};
                    }
                }
            }
        },
        else => {},
    }

    return val;
}

fn parseNumberString(str: []const u8) sdk.JSValue {
    if (std.fmt.parseInt(i32, str, 10)) |i| return sdk.JSValue.fromInt(i) else |_| {}
    if (std.fmt.parseFloat(f64, str)) |f| return sdk.JSValue.fromFloat(f) else |_| {}
    return sdk.JSValue.undefined_val;
}

// ============================================================================
// Tests (pure — no runtime needed)
// ============================================================================

test "schema compilation: basic types" {
    const allocator = std.testing.allocator;
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, "{\"type\":\"string\",\"minLength\":1,\"maxLength\":100}", .{});
    defer parsed.deinit();
    const schema = try compileSchemaFromJson(allocator, parsed.value);
    defer {
        schema.deinit(allocator);
        allocator.destroy(schema);
    }
    try std.testing.expect(schema.schema_type.? == .string);
    try std.testing.expect(schema.min_length.? == 1);
    try std.testing.expect(schema.max_length.? == 100);
}

test "schema compilation: object with properties" {
    const allocator = std.testing.allocator;
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator,
        \\{"type":"object","required":["name"],"properties":{"name":{"type":"string"},"age":{"type":"integer","minimum":0}}}
    , .{});
    defer parsed.deinit();
    const schema = try compileSchemaFromJson(allocator, parsed.value);
    defer {
        schema.deinit(allocator);
        allocator.destroy(schema);
    }
    try std.testing.expect(schema.schema_type.? == .object_type);
    try std.testing.expect(schema.required.?.len == 1);
    try std.testing.expectEqualStrings("name", schema.required.?[0]);
    try std.testing.expect(schema.properties.?.count() == 2);
}

test "schema compilation: array with items" {
    const allocator = std.testing.allocator;
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, "{\"type\":\"array\",\"items\":{\"type\":\"number\",\"minimum\":0,\"maximum\":100}}", .{});
    defer parsed.deinit();
    const schema = try compileSchemaFromJson(allocator, parsed.value);
    defer {
        schema.deinit(allocator);
        allocator.destroy(schema);
    }
    try std.testing.expect(schema.schema_type.? == .array);
    try std.testing.expect(schema.items.?.schema_type.? == .number);
}

test "schema compilation: enum values" {
    const allocator = std.testing.allocator;
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, "{\"type\":\"string\",\"enum\":[\"active\",\"inactive\",\"pending\"]}", .{});
    defer parsed.deinit();
    const schema = try compileSchemaFromJson(allocator, parsed.value);
    defer {
        schema.deinit(allocator);
        allocator.destroy(schema);
    }
    try std.testing.expect(schema.enum_values.?.len == 3);
}

test "parseNumberString" {
    try std.testing.expect(parseNumberString("42").isInt());
    try std.testing.expect(parseNumberString("42").getInt() == 42);
    try std.testing.expect(parseNumberString("3.14").isNumber());
    try std.testing.expect(parseNumberString("not a number").isUndefined());
}

test "parseSchemaType" {
    try std.testing.expect(parseSchemaType("string").? == .string);
    try std.testing.expect(parseSchemaType("number").? == .number);
    try std.testing.expect(parseSchemaType("integer").? == .integer);
    try std.testing.expect(parseSchemaType("boolean").? == .boolean);
    try std.testing.expect(parseSchemaType("array").? == .array);
    try std.testing.expect(parseSchemaType("object").? == .object_type);
    try std.testing.expect(parseSchemaType("null").? == .null_type);
    try std.testing.expect(parseSchemaType("unknown") == null);
}

test "FormatType.fromString" {
    try std.testing.expect(FormatType.fromString("email").? == .email);
    try std.testing.expect(FormatType.fromString("uuid").? == .uuid);
    try std.testing.expect(FormatType.fromString("iso-date").? == .iso_date);
    try std.testing.expect(FormatType.fromString("iso-datetime").? == .iso_datetime);
    try std.testing.expect(FormatType.fromString("uri") == null);
}

test "validateEmail" {
    try std.testing.expect(validateEmail("user@example.com"));
    try std.testing.expect(validateEmail("a@b.co"));
    try std.testing.expect(!validateEmail(""));
    try std.testing.expect(!validateEmail("noatsign"));
    try std.testing.expect(!validateEmail("@nodomain.com"));
    try std.testing.expect(!validateEmail("user@"));
    try std.testing.expect(!validateEmail("user@nodot"));
    try std.testing.expect(!validateEmail("user@@double.com"));
}

test "validateUuid" {
    try std.testing.expect(validateUuid("550e8400-e29b-41d4-a716-446655440000"));
    try std.testing.expect(validateUuid("00000000-0000-0000-0000-000000000000"));
    try std.testing.expect(!validateUuid(""));
    try std.testing.expect(!validateUuid("not-a-uuid"));
    try std.testing.expect(!validateUuid("550e8400e29b41d4a716446655440000"));
}

test "validateIsoDate" {
    try std.testing.expect(validateIsoDate("2024-01-15"));
    try std.testing.expect(!validateIsoDate(""));
    try std.testing.expect(!validateIsoDate("2024-1-15"));
    try std.testing.expect(!validateIsoDate("01-15-2024"));
    try std.testing.expect(!validateIsoDate("2024/01/15"));
}

test "validateIsoDatetime" {
    try std.testing.expect(validateIsoDatetime("2024-01-15T10:30:00"));
    try std.testing.expect(validateIsoDatetime("2024-01-15T10:30:00Z"));
    try std.testing.expect(validateIsoDatetime("2024-01-15T10:30:00+05:30"));
    try std.testing.expect(!validateIsoDatetime(""));
    try std.testing.expect(!validateIsoDatetime("2024-01-15"));
    try std.testing.expect(!validateIsoDatetime("2024-01-15 10:30:00"));
    try std.testing.expect(!validateIsoDatetime("2024-01-15T10:30"));
}
