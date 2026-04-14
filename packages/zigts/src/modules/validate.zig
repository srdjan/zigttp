//! zigttp:validate - JSON Schema validation
//!
//! Exports:
//!   schemaCompile(name: string, schema_json: string) -> boolean
//!     Compile a JSON Schema into the per-runtime registry.
//!
//!   validateJson(name: string, json: string) -> { ok: true, value: parsed } | { ok: false, errors: [...] }
//!     Parse JSON and validate against a compiled schema.
//!
//!   validateObject(name: string, obj: any) -> { ok: true, value: obj } | { ok: false, errors: [...] }
//!     Validate an existing JS value against a compiled schema.
//!
//!   coerceJson(name: string, json: string) -> { ok: true, value: parsed } | { ok: false, errors: [...] }
//!     Parse JSON, coerce types to match schema, then validate.
//!
//!   schemaDrop(name: string) -> boolean
//!     Remove a compiled schema from the registry.
//!
//! Supported JSON Schema subset:
//!   type, required, properties, minLength, maxLength, minimum, maximum, enum, items, format
//!
//! Supported format values (string type only):
//!   email, uuid, iso-date, iso-datetime

const std = @import("std");
const context = @import("../context.zig");
const value = @import("../value.zig");
const object = @import("../object.zig");
const builtins = @import("../builtins/root.zig");
const resolver = @import("resolver.zig");
const util = @import("util.zig");
const mb = @import("../module_binding.zig");

const MODULE_STATE_SLOT = @intFromEnum(@import("../module_slots.zig").Slot.validate);

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:validate",
    .name = "validate",
    .stateful = true,
    .exports = &.{
        // schemaCompile and schemaDrop mutate the module-global schema
        // registry. They are traceable=false and never appear in
        // BehaviorPath.io_sequence, so no law is declared: canonicalization
        // would have nothing to rewrite.
        .{ .name = "schemaCompile", .func = schemaCompileNative, .arg_count = 2, .returns = .boolean, .param_types = &.{.string}, .traceable = false, .contract_extractions = &.{.{ .category = .schema_compile }} },
        .{
            .name = "validateJson",
            .func = validateJsonNative,
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
            .func = validateObjectNative,
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
            .func = coerceJsonNative,
            .arg_count = 2,
            .returns = .result,
            .param_types = &.{ .string, .string },
            .failure_severity = .critical,
            .contract_extractions = &.{.{ .category = .request_schema }},
            .return_labels = .{ .validated = true },
            .laws = &.{.pure},
        },
        .{ .name = "schemaDrop", .func = schemaDropNative, .arg_count = 1, .returns = .boolean, .param_types = &.{.string}, .traceable = false },
    },
};

pub const exports = binding.toModuleExports();

// ============================================================================
// Schema types
// ============================================================================

const SchemaType = enum {
    string,
    number,
    integer,
    boolean,
    array,
    object_type,
    null_type,
};

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
        return null; // Unknown formats silently ignored per JSON Schema spec
    }
};

const CompiledSchema = struct {
    schema_type: ?SchemaType = null,
    min_length: ?u32 = null,
    max_length: ?u32 = null,
    minimum: ?f64 = null,
    maximum: ?f64 = null,
    format: ?FormatType = null,
    properties: ?std.StringHashMap(*CompiledSchema) = null,
    required: ?[][]const u8 = null,
    items: ?*CompiledSchema = null,
    enum_values: ?[][]const u8 = null,

    fn deinit(self: *CompiledSchema, allocator: std.mem.Allocator) void {
        if (self.properties) |*props| {
            var it = props.iterator();
            while (it.next()) |entry| {
                entry.value_ptr.*.deinit(allocator);
                allocator.destroy(entry.value_ptr.*);
                allocator.free(entry.key_ptr.*);
            }
            props.deinit();
        }
        if (self.required) |req| {
            for (req) |r| allocator.free(r);
            allocator.free(req);
        }
        if (self.items) |items_schema| {
            items_schema.deinit(allocator);
            allocator.destroy(items_schema);
        }
        if (self.enum_values) |enums| {
            for (enums) |e| allocator.free(e);
            allocator.free(enums);
        }
    }
};

// ============================================================================
// Schema Registry (per-runtime state)
// ============================================================================

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

    /// Opaque deinit for module_state cleanup
    fn deinitOpaque(ptr: *anyopaque, _: std.mem.Allocator) void {
        const reg: *SchemaRegistry = @ptrCast(@alignCast(ptr));
        reg.deinitSelf();
        reg.allocator.destroy(reg);
    }
};

/// Get or create the per-runtime SchemaRegistry
pub fn getOrCreateRegistry(ctx: *context.Context) !*SchemaRegistry {
    if (ctx.getModuleState(SchemaRegistry, MODULE_STATE_SLOT)) |reg| {
        return reg;
    }

    // Lazy init
    const reg = try ctx.allocator.create(SchemaRegistry);
    reg.* = SchemaRegistry.init(ctx.allocator);
    ctx.setModuleState(MODULE_STATE_SLOT, @ptrCast(reg), &SchemaRegistry.deinitOpaque);
    return reg;
}

// ============================================================================
// Native function implementations
// ============================================================================

/// schemaCompile(name, schema_json) -> boolean
fn schemaCompileNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len < 2) return value.JSValue.false_val;
    const name = util.extractString(args[0]) orelse return value.JSValue.false_val;
    const schema_json = util.extractString(args[1]) orelse return value.JSValue.false_val;

    const reg = getOrCreateRegistry(ctx) catch return value.JSValue.false_val;

    // Parse JSON schema using Zig's JSON parser
    const parsed = std.json.parseFromSlice(std.json.Value, ctx.allocator, schema_json, .{}) catch
        return value.JSValue.false_val;
    defer parsed.deinit();

    // Compile schema from parsed JSON
    const schema = compileSchemaFromJson(ctx.allocator, parsed.value) catch return value.JSValue.false_val;

    // Store in registry (replace if exists)
    const name_owned = ctx.allocator.dupe(u8, name) catch {
        schema.deinit(ctx.allocator);
        ctx.allocator.destroy(schema);
        return value.JSValue.false_val;
    };

    if (reg.schemas.fetchPut(name_owned, schema) catch {
        ctx.allocator.free(name_owned);
        schema.deinit(ctx.allocator);
        ctx.allocator.destroy(schema);
        return value.JSValue.false_val;
    }) |old| {
        old.value.deinit(ctx.allocator);
        ctx.allocator.destroy(old.value);
        ctx.allocator.free(old.key);
    }

    return value.JSValue.true_val;
}

pub fn decodeJson(
    ctx: *context.Context,
    name: []const u8,
    json_str: []const u8,
    coerce: bool,
) !value.JSValue {
    const parsed_val = builtins.parseJsonValue(ctx, json_str) catch
        return util.createPlainResultErr(ctx, "invalid JSON");
    return decodeValue(ctx, name, parsed_val, coerce);
}

pub fn decodeObject(
    ctx: *context.Context,
    name: []const u8,
    input: value.JSValue,
    coerce: bool,
) !value.JSValue {
    return decodeValue(ctx, name, input, coerce);
}

fn decodeValue(
    ctx: *context.Context,
    name: []const u8,
    input: value.JSValue,
    coerce: bool,
) !value.JSValue {
    const reg = getOrCreateRegistry(ctx) catch return util.createPlainResultErr(ctx, "internal error");
    const schema = reg.schemas.get(name) orelse return util.createPlainResultErr(ctx, "schema not found");
    const candidate = if (coerce) coerceValue(ctx, schema, input) else input;
    return validateValueAgainstSchema(ctx, schema, candidate, "");
}

/// validateJson(name, json) -> { ok: true, value } | { ok: false, errors }
fn validateJsonNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len < 2) return util.createPlainResultErr(ctx, "missing arguments");
    const name = util.extractString(args[0]) orelse return util.createPlainResultErr(ctx, "name must be a string");
    const json_str = util.extractString(args[1]) orelse return util.createPlainResultErr(ctx, "json must be a string");

    return decodeJson(ctx, name, json_str, false);
}

/// validateObject(name, obj) -> { ok: true, value } | { ok: false, errors }
fn validateObjectNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len < 2) return util.createPlainResultErr(ctx, "missing arguments");
    const name = util.extractString(args[0]) orelse return util.createPlainResultErr(ctx, "name must be a string");

    return decodeObject(ctx, name, args[1], false);
}

/// coerceJson(name, json) -> { ok: true, value } | { ok: false, errors }
fn coerceJsonNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len < 2) return util.createPlainResultErr(ctx, "missing arguments");
    const name = util.extractString(args[0]) orelse return util.createPlainResultErr(ctx, "name must be a string");
    const json_str = util.extractString(args[1]) orelse return util.createPlainResultErr(ctx, "json must be a string");

    return decodeJson(ctx, name, json_str, true);
}

/// schemaDrop(name) -> boolean
fn schemaDropNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len == 0) return value.JSValue.false_val;
    const name = util.extractString(args[0]) orelse return value.JSValue.false_val;

    const reg = ctx.getModuleState(SchemaRegistry, MODULE_STATE_SLOT) orelse return value.JSValue.false_val;

    if (reg.schemas.fetchRemove(name)) |entry| {
        entry.value.deinit(reg.allocator);
        reg.allocator.destroy(entry.value);
        reg.allocator.free(entry.key);
        return value.JSValue.true_val;
    }

    return value.JSValue.false_val;
}

// ============================================================================
// Schema compilation from std.json.Value
// ============================================================================

fn compileSchemaFromJson(allocator: std.mem.Allocator, json_val: std.json.Value) !*CompiledSchema {
    const schema = try allocator.create(CompiledSchema);
    errdefer allocator.destroy(schema);
    schema.* = .{};

    const obj = switch (json_val) {
        .object => |o| o,
        else => return schema, // Empty schema matches everything
    };

    // type
    if (obj.get("type")) |type_val| {
        switch (type_val) {
            .string => |s| {
                schema.schema_type = parseSchemaType(s);
            },
            else => {},
        }
    }

    // minLength / maxLength
    if (obj.get("minLength")) |v| schema.min_length = jsonToU32(v);
    if (obj.get("maxLength")) |v| schema.max_length = jsonToU32(v);

    // minimum / maximum
    if (obj.get("minimum")) |v| schema.minimum = jsonToF64(v);
    if (obj.get("maximum")) |v| schema.maximum = jsonToF64(v);

    // required
    if (obj.get("required")) |req_val| {
        switch (req_val) {
            .array => |arr| {
                var list: std.ArrayList([]const u8) = .empty;
                errdefer {
                    for (list.items) |item| allocator.free(item);
                    list.deinit(allocator);
                }
                for (arr.items) |item| {
                    switch (item) {
                        .string => |s| try list.append(allocator, try allocator.dupe(u8, s)),
                        else => {},
                    }
                }
                schema.required = try list.toOwnedSlice(allocator);
            },
            else => {},
        }
    }

    // properties (recursive)
    if (obj.get("properties")) |props_val| {
        switch (props_val) {
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
        }
    }

    // items (recursive)
    if (obj.get("items")) |items_val| {
        schema.items = try compileSchemaFromJson(allocator, items_val);
    }

    // enum
    if (obj.get("enum")) |enum_val| {
        switch (enum_val) {
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
        }
    }

    // format (only meaningful for strings)
    if (obj.get("format")) |format_val| {
        switch (format_val) {
            .string => |s| {
                schema.format = FormatType.fromString(s);
            },
            else => {},
        }
    }

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

/// Serialize a std.json.Value to a string for enum comparison
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

// ============================================================================
// Validation logic
// ============================================================================

const ValidationError = struct {
    path: []const u8,
    message: []const u8,
};

fn validateValueAgainstSchema(ctx: *context.Context, schema: *const CompiledSchema, val: value.JSValue, path: []const u8) !value.JSValue {
    var errors: std.ArrayList(ValidationError) = .empty;
    defer errors.deinit(ctx.allocator);

    validateRecursive(ctx, schema, val, path, &errors);

    if (errors.items.len == 0) {
        return util.createPlainResultOk(ctx, val);
    }

    // Convert errors to JS array
    const err_array = try ctx.createArray();
    for (errors.items, 0..) |err_item, i| {
        const err_obj = try ctx.createObject(null);
        const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

        const path_atom = object.Atom.path;
        const msg_atom = object.Atom.message;

        const path_val = try ctx.createString(err_item.path);
        const msg_val = try ctx.createString(err_item.message);
        try err_obj.setProperty(ctx.allocator, pool, path_atom, path_val);
        try err_obj.setProperty(ctx.allocator, pool, msg_atom, msg_val);

        try ctx.setIndexChecked(err_array, @intCast(i), err_obj.toValue());
    }
    err_array.setArrayLength(@intCast(errors.items.len));

    return util.createPlainResultErrs(ctx, err_array.toValue());
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
    // Must have exactly one @, non-empty local part, non-empty domain with a dot
    var at_index: ?usize = null;
    for (str, 0..) |c, i| {
        if (c == '@') {
            if (at_index != null) return false; // multiple @
            at_index = i;
        }
    }
    const at = at_index orelse return false; // no @
    if (at == 0) return false; // empty local part
    const domain = str[at + 1 ..];
    if (domain.len == 0) return false; // empty domain
    // Domain must contain at least one dot
    return std.mem.indexOfScalar(u8, domain, '.') != null;
}

fn validateUuid(str: []const u8) bool {
    // 8-4-4-4-12 hex format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
    if (str.len != 36) return false;
    for (str, 0..) |c, i| {
        if (i == 8 or i == 13 or i == 18 or i == 23) {
            if (c != '-') return false;
        } else {
            if (!isHexDigit(c)) return false;
        }
    }
    return true;
}

fn validateIsoDate(str: []const u8) bool {
    // YYYY-MM-DD
    if (str.len != 10) return false;
    return isDigit(str[0]) and isDigit(str[1]) and isDigit(str[2]) and isDigit(str[3]) and
        str[4] == '-' and
        isDigit(str[5]) and isDigit(str[6]) and
        str[7] == '-' and
        isDigit(str[8]) and isDigit(str[9]);
}

fn validateIsoDatetime(str: []const u8) bool {
    // Minimum: YYYY-MM-DDTHH:MM:SS (19 chars)
    if (str.len < 19) return false;
    // Date part
    if (!validateIsoDate(str[0..10])) return false;
    // T separator
    if (str[10] != 'T') return false;
    // HH:MM:SS
    if (!isDigit(str[11]) or !isDigit(str[12])) return false;
    if (str[13] != ':') return false;
    if (!isDigit(str[14]) or !isDigit(str[15])) return false;
    if (str[16] != ':') return false;
    if (!isDigit(str[17]) or !isDigit(str[18])) return false;
    // Optional timezone suffix (Z, +HH:MM, -HH:MM) or fractional seconds
    // Accept anything after the base pattern per JSON Schema's lenient format spec
    return true;
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isHexDigit(c: u8) bool {
    return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}

fn validateRecursive(ctx: *context.Context, schema: *const CompiledSchema, val: value.JSValue, path: []const u8, errors: *std.ArrayList(ValidationError)) void {
    // Type check
    if (schema.schema_type) |expected_type| {
        const type_ok = switch (expected_type) {
            .string => val.isString() or val.isStringSlice() or val.isRope(),
            .number => val.isNumber() or val.isInt(),
            .integer => val.isInt() or (val.isNumber() and isWholeNumber(val.getFloat64())),
            .boolean => val.isBool(),
            .array => val.isArray(),
            .object_type => val.isObject() and !val.isArray(),
            .null_type => val.isNull(),
        };
        if (!type_ok) {
            errors.append(ctx.allocator, .{ .path = path, .message = "type mismatch" }) catch {};
            return; // Stop validating on type mismatch
        }
    }

    // String constraints
    if (schema.schema_type != null and schema.schema_type.? == .string) {
        if (util.extractString(val)) |str| {
            if (schema.min_length) |min| {
                if (str.len < min) {
                    errors.append(ctx.allocator, .{ .path = path, .message = "string too short" }) catch {};
                }
            }
            if (schema.max_length) |max| {
                if (str.len > max) {
                    errors.append(ctx.allocator, .{ .path = path, .message = "string too long" }) catch {};
                }
            }
            // Format validation
            if (schema.format) |fmt| {
                if (!validateFormat(fmt, str)) {
                    const msg: []const u8 = switch (fmt) {
                        .email => "invalid email format",
                        .uuid => "invalid uuid format",
                        .iso_date => "invalid iso-date format",
                        .iso_datetime => "invalid iso-datetime format",
                    };
                    errors.append(ctx.allocator, .{ .path = path, .message = msg }) catch {};
                }
            }
        }
    }

    // Number constraints
    if (schema.schema_type != null and (schema.schema_type.? == .number or schema.schema_type.? == .integer)) {
        if (util.extractFloat(val)) |num| {
            if (schema.minimum) |min| {
                if (num < min) {
                    errors.append(ctx.allocator, .{ .path = path, .message = "below minimum" }) catch {};
                }
            }
            if (schema.maximum) |max| {
                if (num > max) {
                    errors.append(ctx.allocator, .{ .path = path, .message = "above maximum" }) catch {};
                }
            }
        }
    }

    // Enum check
    if (schema.enum_values) |enums| {
        const val_str = jsValueToEnumString(ctx, val) catch null;
        if (val_str) |vs| {
            var found = false;
            for (enums) |e| {
                if (std.mem.eql(u8, vs, e)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                errors.append(ctx.allocator, .{ .path = path, .message = "value not in enum" }) catch {};
            }
        } else {
            errors.append(ctx.allocator, .{ .path = path, .message = "value not in enum" }) catch {};
        }
    }

    // Object: required + properties
    if (val.isObject() and !val.isArray()) {
        const obj = val.toPtr(object.JSObject);
        const pool = ctx.hidden_class_pool orelse return;

        // Check required fields
        if (schema.required) |req| {
            for (req) |field_name| {
                const atom = ctx.atoms.intern(field_name) catch continue;
                if (obj.getProperty(pool, atom) == null) {
                    errors.append(ctx.allocator, .{ .path = path, .message = "missing required field" }) catch {};
                }
            }
        }

        // Validate declared properties
        if (schema.properties) |props| {
            var it = props.iterator();
            while (it.next()) |entry| {
                const prop_atom = ctx.atoms.intern(entry.key_ptr.*) catch continue;
                if (obj.getProperty(pool, prop_atom)) |prop_val| {
                    // Build child path
                    const child_path = buildPath(ctx.allocator, path, entry.key_ptr.*) catch path;
                    validateRecursive(ctx, entry.value_ptr.*, prop_val, child_path, errors);
                }
            }
        }
    }

    // Array: items
    if (val.isArray() and schema.items != null) {
        const arr_obj = val.toPtr(object.JSObject);
        const len = arr_obj.getArrayLength();
        const items_schema = schema.items.?;
        for (0..len) |i| {
            if (arr_obj.getIndex(@intCast(i))) |elem| {
                const child_path = buildIndexPath(ctx.allocator, path, i) catch path;
                validateRecursive(ctx, items_schema, elem, child_path, errors);
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

/// Convert a JSValue to its enum-comparable string representation
fn jsValueToEnumString(ctx: *context.Context, val: value.JSValue) ![]const u8 {
    if (val.isNull()) return "null";
    if (val.isBool()) return if (val.isTrue()) "true" else "false";
    if (val.isInt()) {
        // Use context's json_writer to format
        var buf: [32]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, "{d}", .{val.getInt()}) catch return error.OutOfMemory;
        return s;
    }
    if (val.isNumber()) {
        var buf: [32]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, "{d}", .{val.getFloat64()}) catch return error.OutOfMemory;
        return s;
    }
    if (util.extractString(val)) |str| {
        return std.fmt.allocPrint(ctx.allocator, "\"{s}\"", .{str});
    }
    return error.OutOfMemory;
}

// ============================================================================
// Coercion logic
// ============================================================================

pub fn coerceValue(ctx: *context.Context, schema: *const CompiledSchema, val: value.JSValue) value.JSValue {
    const expected_type = schema.schema_type orelse return val;

    switch (expected_type) {
        .number, .integer => {
            // String -> number coercion
            if (util.extractString(val)) |str| {
                return parseNumberString(str);
            }
        },
        .boolean => {
            // String -> boolean coercion
            if (util.extractString(val)) |str| {
                if (std.mem.eql(u8, str, "true")) return value.JSValue.true_val;
                if (std.mem.eql(u8, str, "false")) return value.JSValue.false_val;
            }
        },
        .string => {
            // Number -> string coercion
            if (val.isInt()) {
                var buf: [32]u8 = undefined;
                const s = std.fmt.bufPrint(&buf, "{d}", .{val.getInt()}) catch return val;
                return ctx.createString(s) catch val;
            }
            if (val.isNumber()) {
                var buf: [32]u8 = undefined;
                const s = std.fmt.bufPrint(&buf, "{d}", .{val.getFloat64()}) catch return val;
                return ctx.createString(s) catch val;
            }
        },
        .object_type => {
            if (val.isObject() and !val.isArray()) {
                const obj = val.toPtr(object.JSObject);
                const pool = ctx.hidden_class_pool orelse return val;
                if (schema.properties) |props| {
                    var it = props.iterator();
                    while (it.next()) |entry| {
                        const atom = ctx.atoms.intern(entry.key_ptr.*) catch continue;
                        if (obj.getProperty(pool, atom)) |prop_val| {
                            const coerced = coerceValue(ctx, entry.value_ptr.*, prop_val);
                            ctx.setPropertyChecked(obj, atom, coerced) catch {};
                        }
                    }
                }
            }
        },
        .array => {
            if (val.isArray() and schema.items != null) {
                const arr = val.toPtr(object.JSObject);
                const item_schema = schema.items.?;
                const len = arr.getArrayLength();
                var i: u32 = 0;
                while (i < len) : (i += 1) {
                    if (arr.getIndex(i)) |elem| {
                        ctx.setIndexChecked(arr, i, coerceValue(ctx, item_schema, elem)) catch {};
                    }
                }
            }
        },
        else => {},
    }

    return val;
}

fn parseNumberString(str: []const u8) value.JSValue {
    // Try integer first
    if (std.fmt.parseInt(i32, str, 10)) |i| {
        return value.JSValue.fromInt(i);
    } else |_| {}

    // Try float
    if (std.fmt.parseFloat(f64, str)) |f| {
        return value.JSValue.fromFloat(f);
    } else |_| {}

    return value.JSValue.undefined_val;
}

// ============================================================================
// Tests
// ============================================================================

test "schema compilation: basic types" {
    const allocator = std.testing.allocator;
    const schema_json = "{\"type\":\"string\",\"minLength\":1,\"maxLength\":100}";
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, schema_json, .{});
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
    const schema_json =
        \\{"type":"object","required":["name"],"properties":{"name":{"type":"string"},"age":{"type":"integer","minimum":0}}}
    ;
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, schema_json, .{});
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

    const name_schema = schema.properties.?.get("name").?;
    try std.testing.expect(name_schema.schema_type.? == .string);

    const age_schema = schema.properties.?.get("age").?;
    try std.testing.expect(age_schema.schema_type.? == .integer);
    try std.testing.expect(age_schema.minimum.? == 0);
}

test "schema compilation: array with items" {
    const allocator = std.testing.allocator;
    const schema_json = "{\"type\":\"array\",\"items\":{\"type\":\"number\",\"minimum\":0,\"maximum\":100}}";
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, schema_json, .{});
    defer parsed.deinit();

    const schema = try compileSchemaFromJson(allocator, parsed.value);
    defer {
        schema.deinit(allocator);
        allocator.destroy(schema);
    }

    try std.testing.expect(schema.schema_type.? == .array);
    try std.testing.expect(schema.items != null);
    try std.testing.expect(schema.items.?.schema_type.? == .number);
    try std.testing.expect(schema.items.?.minimum.? == 0);
    try std.testing.expect(schema.items.?.maximum.? == 100);
}

test "schema compilation: enum values" {
    const allocator = std.testing.allocator;
    const schema_json = "{\"type\":\"string\",\"enum\":[\"active\",\"inactive\",\"pending\"]}";
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, schema_json, .{});
    defer parsed.deinit();

    const schema = try compileSchemaFromJson(allocator, parsed.value);
    defer {
        schema.deinit(allocator);
        allocator.destroy(schema);
    }

    try std.testing.expect(schema.schema_type.? == .string);
    try std.testing.expect(schema.enum_values.?.len == 3);
    try std.testing.expectEqualStrings("\"active\"", schema.enum_values.?[0]);
    try std.testing.expectEqualStrings("\"inactive\"", schema.enum_values.?[1]);
    try std.testing.expectEqualStrings("\"pending\"", schema.enum_values.?[2]);
}

test "parseNumberString: integer" {
    const val = parseNumberString("42");
    try std.testing.expect(val.isInt());
    try std.testing.expect(val.getInt() == 42);
}

test "parseNumberString: float" {
    const val = parseNumberString("3.14");
    try std.testing.expect(val.isNumber());
    const f = val.getFloat64();
    try std.testing.expect(f > 3.13 and f < 3.15);
}

test "parseNumberString: invalid" {
    const val = parseNumberString("not a number");
    try std.testing.expect(val.isUndefined());
}

test "parseSchemaType: all types" {
    try std.testing.expect(parseSchemaType("string").? == .string);
    try std.testing.expect(parseSchemaType("number").? == .number);
    try std.testing.expect(parseSchemaType("integer").? == .integer);
    try std.testing.expect(parseSchemaType("boolean").? == .boolean);
    try std.testing.expect(parseSchemaType("array").? == .array);
    try std.testing.expect(parseSchemaType("object").? == .object_type);
    try std.testing.expect(parseSchemaType("null").? == .null_type);
    try std.testing.expect(parseSchemaType("unknown") == null);
}

test "FormatType.fromString: known formats" {
    try std.testing.expect(FormatType.fromString("email").? == .email);
    try std.testing.expect(FormatType.fromString("uuid").? == .uuid);
    try std.testing.expect(FormatType.fromString("iso-date").? == .iso_date);
    try std.testing.expect(FormatType.fromString("iso-datetime").? == .iso_datetime);
    try std.testing.expect(FormatType.fromString("unknown") == null);
    try std.testing.expect(FormatType.fromString("uri") == null);
}

test "schema compilation: format field" {
    const allocator = std.testing.allocator;
    const schema_json = "{\"type\":\"string\",\"format\":\"email\"}";
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, schema_json, .{});
    defer parsed.deinit();

    const schema = try compileSchemaFromJson(allocator, parsed.value);
    defer {
        schema.deinit(allocator);
        allocator.destroy(schema);
    }

    try std.testing.expect(schema.schema_type.? == .string);
    try std.testing.expect(schema.format.? == .email);
}

test "schema compilation: unknown format ignored" {
    const allocator = std.testing.allocator;
    const schema_json = "{\"type\":\"string\",\"format\":\"uri\"}";
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, schema_json, .{});
    defer parsed.deinit();

    const schema = try compileSchemaFromJson(allocator, parsed.value);
    defer {
        schema.deinit(allocator);
        allocator.destroy(schema);
    }

    try std.testing.expect(schema.schema_type.? == .string);
    try std.testing.expect(schema.format == null);
}

test "schema compilation: format on property" {
    const allocator = std.testing.allocator;
    const schema_json =
        \\{"type":"object","properties":{"email":{"type":"string","format":"email"},"id":{"type":"string","format":"uuid"}}}
    ;
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, schema_json, .{});
    defer parsed.deinit();

    const schema = try compileSchemaFromJson(allocator, parsed.value);
    defer {
        schema.deinit(allocator);
        allocator.destroy(schema);
    }

    const email_schema = schema.properties.?.get("email").?;
    try std.testing.expect(email_schema.format.? == .email);

    const id_schema = schema.properties.?.get("id").?;
    try std.testing.expect(id_schema.format.? == .uuid);
}

test "validateEmail: valid addresses" {
    try std.testing.expect(validateEmail("user@example.com"));
    try std.testing.expect(validateEmail("a@b.co"));
    try std.testing.expect(validateEmail("user+tag@sub.domain.org"));
}

test "validateEmail: invalid addresses" {
    try std.testing.expect(!validateEmail(""));
    try std.testing.expect(!validateEmail("noatsign"));
    try std.testing.expect(!validateEmail("@nodomain.com"));
    try std.testing.expect(!validateEmail("user@"));
    try std.testing.expect(!validateEmail("user@nodot"));
    try std.testing.expect(!validateEmail("user@@double.com"));
}

test "validateUuid: valid UUIDs" {
    try std.testing.expect(validateUuid("550e8400-e29b-41d4-a716-446655440000"));
    try std.testing.expect(validateUuid("00000000-0000-0000-0000-000000000000"));
    try std.testing.expect(validateUuid("ABCDEF01-2345-6789-abcd-ef0123456789"));
}

test "validateUuid: invalid UUIDs" {
    try std.testing.expect(!validateUuid(""));
    try std.testing.expect(!validateUuid("not-a-uuid"));
    try std.testing.expect(!validateUuid("550e8400e29b41d4a716446655440000")); // no dashes
    try std.testing.expect(!validateUuid("550e8400-e29b-41d4-a716-44665544000")); // too short
    try std.testing.expect(!validateUuid("550e8400-e29b-41d4-a716-4466554400000")); // too long
    try std.testing.expect(!validateUuid("550e8400-e29b-41d4-a716-44665544000g")); // non-hex char
}

test "validateIsoDate: valid dates" {
    try std.testing.expect(validateIsoDate("2024-01-15"));
    try std.testing.expect(validateIsoDate("0000-00-00"));
    try std.testing.expect(validateIsoDate("9999-12-31"));
}

test "validateIsoDate: invalid dates" {
    try std.testing.expect(!validateIsoDate(""));
    try std.testing.expect(!validateIsoDate("2024-1-15")); // single digit month
    try std.testing.expect(!validateIsoDate("01-15-2024")); // wrong order
    try std.testing.expect(!validateIsoDate("2024/01/15")); // wrong separator
    try std.testing.expect(!validateIsoDate("2024-01-1")); // too short
}

test "validateIsoDatetime: valid datetimes" {
    try std.testing.expect(validateIsoDatetime("2024-01-15T10:30:00"));
    try std.testing.expect(validateIsoDatetime("2024-01-15T10:30:00Z"));
    try std.testing.expect(validateIsoDatetime("2024-01-15T10:30:00+05:30"));
    try std.testing.expect(validateIsoDatetime("2024-01-15T10:30:00.123Z"));
}

test "validateIsoDatetime: invalid datetimes" {
    try std.testing.expect(!validateIsoDatetime("")); // empty
    try std.testing.expect(!validateIsoDatetime("2024-01-15")); // date only
    try std.testing.expect(!validateIsoDatetime("2024-01-15 10:30:00")); // space instead of T
    try std.testing.expect(!validateIsoDatetime("2024-01-15T10:30")); // missing seconds
    try std.testing.expect(!validateIsoDatetime("2024-01-15T1:30:00")); // single digit hour
}
