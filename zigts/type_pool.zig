//! Type Pool: Structured type representation for the Sound Edition type system.
//!
//! Types are stored in a flat, indexed pool for cache-friendly access. Complex types
//! (records, functions, unions) store children via (start, count) pairs into separate
//! field/param/member arrays.
//!
//! Includes:
//! - Recursive descent parser for type expression strings
//! - Structural subtyping via isAssignableTo
//! - Display formatting for diagnostics

const std = @import("std");

// ---------------------------------------------------------------------------
// Type indices
// ---------------------------------------------------------------------------

/// Index into the TypePool node array.
pub const TypeIndex = u16;

/// Sentinel for "no type" / "unknown".
pub const null_type_idx: TypeIndex = std.math.maxInt(TypeIndex);

// ---------------------------------------------------------------------------
// Type tags
// ---------------------------------------------------------------------------

pub const TypeTag = enum(u8) {
    // Primitives
    t_boolean,
    t_number,
    t_string,
    t_null,
    t_undefined,
    t_void,
    t_never,
    t_unknown_type, // TypeScript 'unknown' type

    // Compound
    t_record, // { field: Type; ... }
    t_array, // T[]
    t_tuple, // [T, U]
    t_function, // (params) => ReturnType
    t_union, // T | U | V

    // Literals
    t_literal_string, // "GET", "POST"
    t_literal_number, // 200, 404
    t_literal_bool, // true, false

    // References
    t_ref, // named type reference (Foo, Bar)
    t_generic_param, // type variable (T in <T>)
    t_generic_app, // generic application (Array<number>)

    // Template literal type
    t_template_literal, // `/api/${string}` (pattern of literal + type slot parts)

    // Optional shorthand
    t_nullable, // T | undefined (wraps inner type)
};

// ---------------------------------------------------------------------------
// Type data payloads
// ---------------------------------------------------------------------------

/// Record (object) type field.
pub const RecordField = struct {
    /// Name of the field (stored as byte offset/len into name pool, or atom index).
    name_start: u16,
    name_len: u8,
    type_idx: TypeIndex,
    optional: bool,
    readonly: bool = false,
};

/// Function parameter.
pub const FuncParam = struct {
    name_start: u16,
    name_len: u8,
    type_idx: TypeIndex,
    optional: bool,
};

/// Template literal type part: either a literal string segment or a type slot.
pub const TemplatePart = struct {
    kind: enum(u1) { literal, type_slot },
    /// For literal: name_start/name_len in name pool. For type_slot: type_idx.
    name_start: u16 = 0,
    name_len: u8 = 0,
    type_idx: TypeIndex = null_type_idx,
};

/// Packed payload for a TypeNode, interpreted based on the tag.
pub const TypeData = packed struct(u32) {
    a: u16 = 0, // meaning depends on tag
    b: u16 = 0, // meaning depends on tag
};

/// A single type in the pool.
pub const TypeNode = struct {
    tag: TypeTag,
    data: TypeData,
    /// For nominal types (capability interfaces), prevents structural forgery.
    nominal: bool = false,
};

// ---------------------------------------------------------------------------
// Type Pool
// ---------------------------------------------------------------------------

pub const TypePool = struct {
    nodes: std.ArrayListUnmanaged(TypeNode),
    fields: std.ArrayListUnmanaged(RecordField),
    params: std.ArrayListUnmanaged(FuncParam),
    /// Union members, tuple elements, generic type args
    members: std.ArrayListUnmanaged(TypeIndex),
    /// Name storage for record fields and function params
    names: std.ArrayListUnmanaged(u8),
    /// Template literal type parts storage
    template_parts: std.ArrayListUnmanaged(TemplatePart),

    // Pre-allocated primitive indices (populated during init)
    idx_boolean: TypeIndex = null_type_idx,
    idx_number: TypeIndex = null_type_idx,
    idx_string: TypeIndex = null_type_idx,
    idx_null: TypeIndex = null_type_idx,
    idx_undefined: TypeIndex = null_type_idx,
    idx_void: TypeIndex = null_type_idx,
    idx_never: TypeIndex = null_type_idx,
    idx_unknown: TypeIndex = null_type_idx,

    pub fn init(allocator: std.mem.Allocator) TypePool {
        var pool = TypePool{
            .nodes = .empty,
            .fields = .empty,
            .params = .empty,
            .members = .empty,
            .names = .empty,
            .template_parts = .empty,
        };
        // Pre-allocate primitive types
        pool.idx_boolean = pool.addNode(allocator, .{ .tag = .t_boolean, .data = .{} });
        pool.idx_number = pool.addNode(allocator, .{ .tag = .t_number, .data = .{} });
        pool.idx_string = pool.addNode(allocator, .{ .tag = .t_string, .data = .{} });
        pool.idx_null = pool.addNode(allocator, .{ .tag = .t_null, .data = .{} });
        pool.idx_undefined = pool.addNode(allocator, .{ .tag = .t_undefined, .data = .{} });
        pool.idx_void = pool.addNode(allocator, .{ .tag = .t_void, .data = .{} });
        pool.idx_never = pool.addNode(allocator, .{ .tag = .t_never, .data = .{} });
        pool.idx_unknown = pool.addNode(allocator, .{ .tag = .t_unknown_type, .data = .{} });
        return pool;
    }

    pub fn deinit(self: *TypePool, allocator: std.mem.Allocator) void {
        self.nodes.deinit(allocator);
        self.fields.deinit(allocator);
        self.params.deinit(allocator);
        self.members.deinit(allocator);
        self.names.deinit(allocator);
        self.template_parts.deinit(allocator);
    }

    // -------------------------------------------------------------------
    // Node creation
    // -------------------------------------------------------------------

    fn addNode(self: *TypePool, allocator: std.mem.Allocator, node: TypeNode) TypeIndex {
        const idx: TypeIndex = @intCast(self.nodes.items.len);
        self.nodes.append(allocator, node) catch return null_type_idx;
        return idx;
    }

    /// Store a name string and return (start, len).
    pub fn addName(self: *TypePool, allocator: std.mem.Allocator, name: []const u8) struct { start: u16, len: u8 } {
        const start: u16 = @intCast(self.names.items.len);
        const len: u8 = @intCast(@min(name.len, 255));
        self.names.appendSlice(allocator, name[0..len]) catch {};
        return .{ .start = start, .len = len };
    }

    /// Get a name string by (start, len).
    pub fn getName(self: *const TypePool, start: u16, len: u8) []const u8 {
        if (start + len > self.names.items.len) return "";
        return self.names.items[start .. start + len];
    }

    /// Get the tag for a type index.
    pub fn getTag(self: *const TypePool, idx: TypeIndex) ?TypeTag {
        if (idx == null_type_idx or idx >= self.nodes.items.len) return null;
        return self.nodes.items[idx].tag;
    }

    /// Widen a literal type to its base type. Returns the index unchanged
    /// for non-literal types. Used to prevent let bindings from being locked
    /// to a specific literal value.
    pub fn widenLiteral(self: *const TypePool, idx: TypeIndex) TypeIndex {
        const tag = self.getTag(idx) orelse return idx;
        return switch (tag) {
            .t_literal_string => self.idx_string,
            .t_literal_number => self.idx_number,
            .t_literal_bool => self.idx_boolean,
            else => idx,
        };
    }

    /// Get the data for a type index.
    pub fn getData(self: *const TypePool, idx: TypeIndex) ?TypeData {
        if (idx == null_type_idx or idx >= self.nodes.items.len) return null;
        return self.nodes.items[idx].data;
    }

    /// Check if a type is nominal.
    pub fn isNominal(self: *const TypePool, idx: TypeIndex) bool {
        if (idx == null_type_idx or idx >= self.nodes.items.len) return false;
        return self.nodes.items[idx].nominal;
    }

    /// Create a nominal (branded) alias of a base type. The resulting type is
    /// structurally identical but incompatible with other types via nominal flag.
    /// Returns the new nominal type index.
    pub fn addNominalAlias(self: *TypePool, allocator: std.mem.Allocator, base: TypeIndex) TypeIndex {
        const base_node = if (base < self.nodes.items.len) self.nodes.items[base] else return null_type_idx;
        return self.addNode(allocator, .{
            .tag = base_node.tag,
            .data = base_node.data,
            .nominal = true,
        });
    }

    /// Unwrap a nominal type to its base primitive. For non-nominal types, returns unchanged.
    /// Nominal types are branded wrappers around primitives (string, number, boolean),
    /// so widening always recovers the correct base type.
    pub fn unwrapNominal(self: *const TypePool, idx: TypeIndex) TypeIndex {
        if (!self.isNominal(idx)) return idx;
        return self.widenLiteral(idx);
    }

    // -------------------------------------------------------------------
    // Compound type constructors
    // -------------------------------------------------------------------

    /// Create a record type with the given fields.
    pub fn addRecord(self: *TypePool, allocator: std.mem.Allocator, record_fields: []const RecordField) TypeIndex {
        const start: u16 = @intCast(self.fields.items.len);
        const count: u16 = @intCast(record_fields.len);
        self.fields.appendSlice(allocator, record_fields) catch return null_type_idx;
        return self.addNode(allocator, .{
            .tag = .t_record,
            .data = .{ .a = start, .b = count },
        });
    }

    /// Create a union type from members.
    pub fn addUnion(self: *TypePool, allocator: std.mem.Allocator, union_members: []const TypeIndex) TypeIndex {
        // Flatten single-member unions
        if (union_members.len == 1) return union_members[0];
        const start: u16 = @intCast(self.members.items.len);
        const count: u16 = @intCast(union_members.len);
        self.members.appendSlice(allocator, union_members) catch return null_type_idx;
        return self.addNode(allocator, .{
            .tag = .t_union,
            .data = .{ .a = start, .b = count },
        });
    }

    /// Create a function type.
    pub fn addFunction(self: *TypePool, allocator: std.mem.Allocator, func_params: []const FuncParam, ret: TypeIndex) TypeIndex {
        return self.addFunctionWithReturn(allocator, func_params, ret);
    }

    /// Create an array type.
    pub fn addArray(self: *TypePool, allocator: std.mem.Allocator, element: TypeIndex) TypeIndex {
        return self.addNode(allocator, .{
            .tag = .t_array,
            .data = .{ .a = element, .b = 0 },
        });
    }

    /// Create a tuple type.
    pub fn addTuple(self: *TypePool, allocator: std.mem.Allocator, elements: []const TypeIndex) TypeIndex {
        const start: u16 = @intCast(self.members.items.len);
        const count: u16 = @intCast(elements.len);
        self.members.appendSlice(allocator, elements) catch return null_type_idx;
        return self.addNode(allocator, .{
            .tag = .t_tuple,
            .data = .{ .a = start, .b = count },
        });
    }

    /// Create a nullable type (T | null | undefined).
    pub fn addNullable(self: *TypePool, allocator: std.mem.Allocator, inner: TypeIndex) TypeIndex {
        return self.addNode(allocator, .{
            .tag = .t_nullable,
            .data = .{ .a = inner, .b = 0 },
        });
    }

    /// Create a named type reference.
    pub fn addRef(self: *TypePool, allocator: std.mem.Allocator, name: []const u8) TypeIndex {
        const n = self.addName(allocator, name);
        return self.addNode(allocator, .{
            .tag = .t_ref,
            .data = .{ .a = n.start, .b = @as(u16, n.len) },
        });
    }

    /// Create a literal string type.
    pub fn addLiteralString(self: *TypePool, allocator: std.mem.Allocator, value: []const u8) TypeIndex {
        const n = self.addName(allocator, value);
        return self.addNode(allocator, .{
            .tag = .t_literal_string,
            .data = .{ .a = n.start, .b = @as(u16, n.len) },
        });
    }

    /// Create a literal number type.
    pub fn addLiteralNumber(self: *TypePool, allocator: std.mem.Allocator, value: i16) TypeIndex {
        return self.addNode(allocator, .{
            .tag = .t_literal_number,
            .data = .{ .a = @bitCast(value), .b = 0 },
        });
    }

    /// Create a literal boolean type.
    pub fn addLiteralBool(self: *TypePool, allocator: std.mem.Allocator, value: bool) TypeIndex {
        return self.addNode(allocator, .{
            .tag = .t_literal_bool,
            .data = .{ .a = if (value) 1 else 0, .b = 0 },
        });
    }

    /// Create a generic type parameter.
    /// Create a generic type parameter.
    /// Constraint is stored separately in TypeEnv (to avoid complicating TypeData).
    pub fn addGenericParam(self: *TypePool, allocator: std.mem.Allocator, name: []const u8) TypeIndex {
        const n = self.addName(allocator, name);
        return self.addNode(allocator, .{
            .tag = .t_generic_param,
            .data = .{ .a = n.start, .b = @as(u16, n.len) },
        });
    }

    /// Instantiate a type by substituting generic parameters with concrete types.
    /// substitutions maps generic param names (via name pool) to concrete TypeIndices.
    /// Depth limit prevents infinite recursion (no recursive types in this subset).
    pub fn instantiate(
        self: *TypePool,
        allocator: std.mem.Allocator,
        idx: TypeIndex,
        param_names: []const []const u8,
        param_types: []const TypeIndex,
        depth: u8,
    ) TypeIndex {
        if (idx == null_type_idx or depth > 8) return idx;
        const tag = self.getTag(idx) orelse return idx;

        switch (tag) {
            .t_generic_param, .t_ref => {
                const name = self.getRefName(idx);
                for (param_names, 0..) |pn, i| {
                    if (std.mem.eql(u8, name, pn) and i < param_types.len) {
                        return param_types[i];
                    }
                }
                return idx;
            },
            .t_record => {
                const fields = self.getRecordFields(idx);
                var new_fields: [32]RecordField = undefined;
                const count = @min(fields.len, 32);
                for (fields[0..count], 0..) |f, i| {
                    new_fields[i] = f;
                    new_fields[i].type_idx = self.instantiate(allocator, f.type_idx, param_names, param_types, depth + 1);
                }
                return self.addRecord(allocator, new_fields[0..count]);
            },
            .t_array => {
                const elem = self.getArrayElement(idx);
                const new_elem = self.instantiate(allocator, elem, param_names, param_types, depth + 1);
                if (new_elem == elem) return idx;
                return self.addArray(allocator, new_elem);
            },
            .t_union => {
                const members = self.getUnionMembers(idx);
                var new_members: [32]TypeIndex = undefined;
                const count = @min(members.len, 32);
                var changed = false;
                for (members[0..count], 0..) |m, i| {
                    new_members[i] = self.instantiate(allocator, m, param_names, param_types, depth + 1);
                    if (new_members[i] != m) changed = true;
                }
                if (!changed) return idx;
                return self.addUnion(allocator, new_members[0..count]);
            },
            .t_nullable => {
                const inner = self.getNullableInner(idx);
                const new_inner = self.instantiate(allocator, inner, param_names, param_types, depth + 1);
                if (new_inner == inner) return idx;
                return self.addNullable(allocator, new_inner);
            },
            else => return idx,
        }
    }

    /// Create a generic application (e.g., Array<number>).
    pub fn addGenericApp(self: *TypePool, allocator: std.mem.Allocator, base: TypeIndex, type_args: []const TypeIndex) TypeIndex {
        const start: u16 = @intCast(self.members.items.len);
        const count: u16 = @intCast(type_args.len);
        self.members.appendSlice(allocator, type_args) catch return null_type_idx;
        return self.addNode(allocator, .{
            .tag = .t_generic_app,
            .data = .{ .a = base, .b = (@as(u16, @intCast(start)) & 0xFF) | (count << 8) },
        });
    }

    // -------------------------------------------------------------------
    // Accessors for compound types
    // -------------------------------------------------------------------

    /// Get record fields.
    pub fn getRecordFields(self: *const TypePool, idx: TypeIndex) []const RecordField {
        const data = self.getData(idx) orelse return &.{};
        if (self.getTag(idx) != .t_record) return &.{};
        const start = data.a;
        const count = data.b;
        if (start + count > self.fields.items.len) return &.{};
        return self.fields.items[start .. start + count];
    }

    /// Get union members.
    pub fn getUnionMembers(self: *const TypePool, idx: TypeIndex) []const TypeIndex {
        const data = self.getData(idx) orelse return &.{};
        if (self.getTag(idx) != .t_union) return &.{};
        const start = data.a;
        const count = data.b;
        if (start + count > self.members.items.len) return &.{};
        return self.members.items[start .. start + count];
    }

    /// Look up a record field by name. Returns the field if found.
    pub fn lookupRecordField(self: *const TypePool, idx: TypeIndex, field_name: []const u8) ?RecordField {
        const fields = self.getRecordFields(idx);
        for (fields) |field| {
            if (std.mem.eql(u8, self.getName(field.name_start, field.name_len), field_name)) {
                return field;
            }
        }
        return null;
    }

    /// Create a new record type with all fields marked readonly (Readonly<T> utility).
    pub fn makeReadonly(self: *TypePool, allocator: std.mem.Allocator, idx: TypeIndex) TypeIndex {
        const fields = self.getRecordFields(idx);
        if (fields.len == 0) return idx;
        var ro_fields: [32]RecordField = undefined;
        const count = @min(fields.len, 32);
        for (fields[0..count], 0..) |field, i| {
            ro_fields[i] = field;
            ro_fields[i].readonly = true;
        }
        return self.addRecord(allocator, ro_fields[0..count]);
    }

    /// Get the string value of a literal string type.
    pub fn getLiteralStringValue(self: *const TypePool, idx: TypeIndex) ?[]const u8 {
        if (self.getTag(idx) != .t_literal_string) return null;
        const data = self.getData(idx) orelse return null;
        return self.getName(data.a, @intCast(data.b));
    }

    /// Create a template literal type from parts.
    /// TypeData: a = parts_start, b = parts_count.
    pub fn addTemplateLiteral(self: *TypePool, allocator: std.mem.Allocator, parts: []const TemplatePart) TypeIndex {
        const start: u16 = @intCast(self.template_parts.items.len);
        const count: u16 = @intCast(parts.len);
        self.template_parts.appendSlice(allocator, parts) catch return null_type_idx;
        return self.addNode(allocator, .{
            .tag = .t_template_literal,
            .data = .{ .a = start, .b = count },
        });
    }

    /// Get the parts of a template literal type.
    pub fn getTemplateParts(self: *const TypePool, idx: TypeIndex) []const TemplatePart {
        if (self.getTag(idx) != .t_template_literal) return &.{};
        const data = self.getData(idx) orelse return &.{};
        const start = data.a;
        const count = data.b;
        if (start + count > self.template_parts.items.len) return &.{};
        return self.template_parts.items[start .. start + count];
    }

    /// Check if a literal string matches a template literal type pattern.
    /// E.g., "/api/users" matches `/api/${string}`.
    pub fn matchesTemplateLiteral(self: *const TypePool, literal_idx: TypeIndex, template_idx: TypeIndex) bool {
        const literal_val = self.getLiteralStringValue(literal_idx) orelse return false;
        const parts = self.getTemplateParts(template_idx);
        if (parts.len == 0) return false;

        var pos: usize = 0;
        for (parts, 0..) |part, i| {
            if (part.kind == .literal) {
                const segment = self.getName(part.name_start, part.name_len);
                if (pos + segment.len > literal_val.len) return false;
                if (!std.mem.eql(u8, literal_val[pos .. pos + segment.len], segment)) return false;
                pos += segment.len;
            } else {
                // Type slot (e.g., ${string}): matches any characters
                // If this is the last part, consume the rest
                if (i + 1 >= parts.len) {
                    pos = literal_val.len;
                } else {
                    // Find the next literal segment
                    const next = parts[i + 1];
                    if (next.kind == .literal) {
                        const next_seg = self.getName(next.name_start, next.name_len);
                        // Find the next literal segment in the remaining string
                        if (std.mem.indexOf(u8, literal_val[pos..], next_seg)) |found| {
                            pos += found;
                        } else {
                            return false;
                        }
                    }
                }
            }
        }
        return pos == literal_val.len;
    }

    /// Find a union member whose discriminant field matches a literal string value.
    /// For a union like { kind: "ok", value: string } | { kind: "err", error: string },
    /// calling findUnionMemberByDiscriminant(union_idx, "kind", "ok") returns the first member.
    pub fn findUnionMemberByDiscriminant(
        self: *const TypePool,
        union_idx: TypeIndex,
        field_name: []const u8,
        literal_value: []const u8,
    ) ?TypeIndex {
        const members = self.getUnionMembers(union_idx);
        if (members.len == 0) return null;
        for (members) |member| {
            const fields = self.getRecordFields(member);
            for (fields) |field| {
                const name = self.getName(field.name_start, field.name_len);
                if (!std.mem.eql(u8, name, field_name)) continue;
                const val = self.getLiteralStringValue(field.type_idx) orelse continue;
                if (std.mem.eql(u8, val, literal_value)) return member;
            }
        }
        return null;
    }

    /// Return a new union with the specified member excluded.
    /// If only one member remains, returns that member directly.
    pub fn excludeUnionMember(
        self: *TypePool,
        allocator: std.mem.Allocator,
        union_idx: TypeIndex,
        exclude: TypeIndex,
    ) TypeIndex {
        const members = self.getUnionMembers(union_idx);
        if (members.len == 0) return union_idx;
        var remaining: [16]TypeIndex = undefined;
        var count: usize = 0;
        for (members) |member| {
            if (member != exclude and count < 16) {
                remaining[count] = member;
                count += 1;
            }
        }
        if (count == 0) return null_type_idx;
        if (count == 1) return remaining[0];
        return self.addUnion(allocator, remaining[0..count]);
    }

    /// Get function params and return type.
    /// Layout: data.a = param_start (into params array), data.b = return type index.
    /// Param count is stored as a members entry at index = node index.
    pub fn getFunctionInfo(self: *const TypePool, idx: TypeIndex) struct { params: []const FuncParam, ret: TypeIndex } {
        const data = self.getData(idx) orelse return .{ .params = &.{}, .ret = null_type_idx };
        if (self.getTag(idx) != .t_function) return .{ .params = &.{}, .ret = null_type_idx };
        const param_start = data.a;
        const ret = data.b;
        // Param count encoded as first member entry for this function
        const param_count_raw = if (idx < self.members.items.len) self.members.items[idx] else 0;
        const param_count: u16 = @intCast(@min(param_count_raw, self.params.items.len -| param_start));
        if (param_start + param_count > self.params.items.len) return .{ .params = &.{}, .ret = ret };
        return .{
            .params = self.params.items[param_start .. param_start + param_count],
            .ret = ret,
        };
    }

    /// Create a function type with params and return type.
    pub fn addFunctionWithReturn(self: *TypePool, allocator: std.mem.Allocator, func_params: []const FuncParam, ret: TypeIndex) TypeIndex {
        const param_start: u16 = @intCast(self.params.items.len);
        self.params.appendSlice(allocator, func_params) catch return null_type_idx;
        const node_idx = self.addNode(allocator, .{
            .tag = .t_function,
            .data = .{ .a = param_start, .b = ret },
        });
        // Store param count in members at the node's index for retrieval
        // Pad members array if needed to match node index
        while (self.members.items.len < node_idx) {
            self.members.append(allocator, 0) catch break;
        }
        self.members.append(allocator, @intCast(func_params.len)) catch {};
        return node_idx;
    }

    /// Get the array element type.
    pub fn getArrayElement(self: *const TypePool, idx: TypeIndex) TypeIndex {
        const data = self.getData(idx) orelse return null_type_idx;
        if (self.getTag(idx) != .t_array) return null_type_idx;
        return data.a;
    }

    /// Get the nullable inner type.
    pub fn getNullableInner(self: *const TypePool, idx: TypeIndex) TypeIndex {
        const data = self.getData(idx) orelse return null_type_idx;
        if (self.getTag(idx) != .t_nullable) return null_type_idx;
        return data.a;
    }

    /// Get reference name.
    pub fn getRefName(self: *const TypePool, idx: TypeIndex) []const u8 {
        const data = self.getData(idx) orelse return "";
        const tag = self.getTag(idx) orelse return "";
        if (tag != .t_ref and tag != .t_generic_param) return "";
        return self.getName(data.a, @intCast(data.b));
    }

    /// Get generic application base and type arguments.
    pub fn getGenericAppInfo(self: *const TypePool, idx: TypeIndex) struct { base: TypeIndex, args: []const TypeIndex } {
        const data = self.getData(idx) orelse return .{ .base = null_type_idx, .args = &.{} };
        if (self.getTag(idx) != .t_generic_app) return .{ .base = null_type_idx, .args = &.{} };
        const start: u16 = data.b & 0xFF;
        const count: u16 = data.b >> 8;
        if (start + count > self.members.items.len) return .{ .base = data.a, .args = &.{} };
        return .{ .base = data.a, .args = self.members.items[start .. start + count] };
    }

    // -------------------------------------------------------------------
    // Structural subtyping
    // -------------------------------------------------------------------

    /// Check if `source` type is assignable to `target` type.
    /// Implements structural subtyping: source is assignable if it has at least
    /// all the fields/members of target with compatible types.
    pub fn isAssignableTo(self: *const TypePool, source: TypeIndex, target: TypeIndex) bool {
        if (source == target) return true;
        // Unknown target accepts anything; null_type_idx means inference
        // produced no result, so skip rather than reject.
        if (target == null_type_idx or target == self.idx_unknown) return true;
        if (source == null_type_idx) return true;

        const src_tag = self.getTag(source) orelse return false;
        const tgt_tag = self.getTag(target) orelse return false;

        // never is assignable to everything (bottom type)
        if (src_tag == .t_never) return true;
        // nothing is assignable to never
        if (tgt_tag == .t_never) return false;

        // Nominal types: only equal indices match, unless the source is
        // a record (object literal) which should be structurally checked
        // against the nominal target's fields.
        if (self.isNominal(target) and source != target) {
            if (src_tag == .t_record and tgt_tag == .t_record) {
                return self.isRecordAssignable(source, target);
            }
            return false;
        }

        // Same primitive tags
        if (src_tag == tgt_tag) {
            return switch (src_tag) {
                .t_boolean, .t_number, .t_string, .t_null, .t_undefined, .t_void, .t_unknown_type => true,
                .t_record => self.isRecordAssignable(source, target),
                .t_array => self.isAssignableTo(self.getArrayElement(source), self.getArrayElement(target)),
                .t_function => self.isFunctionAssignable(source, target),
                .t_union => self.isUnionAssignableToUnion(source, target),
                .t_nullable => self.isAssignableTo(self.getNullableInner(source), self.getNullableInner(target)),
                .t_literal_string, .t_literal_number, .t_literal_bool => self.literalEquals(source, target),
                .t_ref => std.mem.eql(u8, self.getRefName(source), self.getRefName(target)),
                else => false,
            };
        }

        // Literal types are assignable to their base types
        if (src_tag == .t_literal_string and tgt_tag == .t_string) return true;
        if (src_tag == .t_literal_number and tgt_tag == .t_number) return true;
        if (src_tag == .t_literal_bool and tgt_tag == .t_boolean) return true;

        // Literal string assignable to template literal type if it matches the pattern
        if (src_tag == .t_literal_string and tgt_tag == .t_template_literal) {
            return self.matchesTemplateLiteral(source, target);
        }

        // null/undefined are assignable to nullable types
        if (tgt_tag == .t_nullable) {
            if (src_tag == .t_null or src_tag == .t_undefined) return true;
            return self.isAssignableTo(source, self.getNullableInner(target));
        }

        // Source is nullable, target is not - not assignable (need narrowing)
        if (src_tag == .t_nullable and tgt_tag != .t_nullable and tgt_tag != .t_union) return false;

        // void is assignable to undefined
        if (src_tag == .t_void and tgt_tag == .t_undefined) return true;
        if (src_tag == .t_undefined and tgt_tag == .t_void) return true;

        // Union target: source must be assignable to at least one member
        if (tgt_tag == .t_union) {
            for (self.getUnionMembers(target)) |member| {
                if (self.isAssignableTo(source, member)) return true;
            }
            return false;
        }

        // Union source: every member must be assignable to target
        if (src_tag == .t_union) {
            for (self.getUnionMembers(source)) |member| {
                if (!self.isAssignableTo(member, target)) return false;
            }
            return true;
        }

        // Unresolved refs are effectively unknown: the TypePool cannot
        // resolve ref names to their definitions (that requires the TypeEnv).
        // Accept records against refs rather than rejecting valid code.
        if (src_tag == .t_record and tgt_tag == .t_ref) return true;

        return false;
    }

    fn isRecordAssignable(self: *const TypePool, source: TypeIndex, target: TypeIndex) bool {
        const tgt_fields = self.getRecordFields(target);
        const src_fields = self.getRecordFields(source);

        // Every required field in target must exist in source with compatible type
        for (tgt_fields) |tgt_f| {
            const tgt_name = self.getName(tgt_f.name_start, tgt_f.name_len);
            var found = false;
            for (src_fields) |src_f| {
                const src_name = self.getName(src_f.name_start, src_f.name_len);
                if (std.mem.eql(u8, src_name, tgt_name)) {
                    if (!self.isAssignableTo(src_f.type_idx, tgt_f.type_idx)) return false;
                    found = true;
                    break;
                }
            }
            if (!found and !tgt_f.optional) return false;
        }
        return true;
    }

    fn isFunctionAssignable(self: *const TypePool, source: TypeIndex, target: TypeIndex) bool {
        const src_info = self.getFunctionInfo(source);
        const tgt_info = self.getFunctionInfo(target);

        // Source can have fewer params (excess target params become optional in the call)
        if (src_info.params.len > tgt_info.params.len) return false;

        // Check parameter types (contravariant)
        for (src_info.params, 0..) |src_p, i| {
            if (i >= tgt_info.params.len) break;
            // Contravariant: target param must be assignable to source param
            if (!self.isAssignableTo(tgt_info.params[i].type_idx, src_p.type_idx)) return false;
        }

        // Return type (covariant) - only check if both have known return types
        if (src_info.ret != null_type_idx and tgt_info.ret != null_type_idx) {
            if (!self.isAssignableTo(src_info.ret, tgt_info.ret)) return false;
        }

        return true;
    }

    fn isUnionAssignableToUnion(self: *const TypePool, source: TypeIndex, target: TypeIndex) bool {
        // Every member of source must be assignable to at least one member of target
        for (self.getUnionMembers(source)) |src_member| {
            var assignable = false;
            for (self.getUnionMembers(target)) |tgt_member| {
                if (self.isAssignableTo(src_member, tgt_member)) {
                    assignable = true;
                    break;
                }
            }
            if (!assignable) return false;
        }
        return true;
    }

    fn literalEquals(self: *const TypePool, a: TypeIndex, b: TypeIndex) bool {
        const a_tag = self.getTag(a) orelse return false;
        const b_tag = self.getTag(b) orelse return false;
        if (a_tag != b_tag) return false;

        const a_data = self.getData(a) orelse return false;
        const b_data = self.getData(b) orelse return false;
        return switch (a_tag) {
            .t_literal_string => std.mem.eql(u8, self.getName(a_data.a, @truncate(a_data.b)), self.getName(b_data.a, @truncate(b_data.b))),
            .t_literal_number, .t_literal_bool => a_data.a == b_data.a,
            else => a_data.a == b_data.a and a_data.b == b_data.b,
        };
    }

    // -------------------------------------------------------------------
    // Display
    // -------------------------------------------------------------------

    /// Format a type for diagnostic messages.
    pub fn formatType(self: *const TypePool, idx: TypeIndex, buf: []u8) []const u8 {
        if (idx == null_type_idx) return "unknown";
        var writer: std.Io.Writer = .fixed(buf);
        self.writeType(idx, &writer) catch return "?";
        return writer.buffer[0..writer.end];
    }

    fn writeType(self: *const TypePool, idx: TypeIndex, writer: *std.Io.Writer) !void {
        const tag = self.getTag(idx) orelse {
            try writer.writeAll("unknown");
            return;
        };
        switch (tag) {
            .t_boolean => try writer.writeAll("boolean"),
            .t_number => try writer.writeAll("number"),
            .t_string => try writer.writeAll("string"),
            .t_null => try writer.writeAll("null"),
            .t_undefined => try writer.writeAll("undefined"),
            .t_void => try writer.writeAll("void"),
            .t_never => try writer.writeAll("never"),
            .t_unknown_type => try writer.writeAll("unknown"),
            .t_record => {
                try writer.writeAll("{ ");
                for (self.getRecordFields(idx), 0..) |field, i| {
                    if (i > 0) try writer.writeAll("; ");
                    try writer.writeAll(self.getName(field.name_start, field.name_len));
                    if (field.optional) try writer.writeByte('?');
                    try writer.writeAll(": ");
                    try self.writeType(field.type_idx, writer);
                }
                try writer.writeAll(" }");
            },
            .t_array => {
                try self.writeType(self.getArrayElement(idx), writer);
                try writer.writeAll("[]");
            },
            .t_function => {
                const info = self.getFunctionInfo(idx);
                try writer.writeByte('(');
                for (info.params, 0..) |p, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.writeAll(self.getName(p.name_start, p.name_len));
                    try writer.writeAll(": ");
                    try self.writeType(p.type_idx, writer);
                }
                try writer.writeAll(") => ");
                if (info.ret != null_type_idx) {
                    try self.writeType(info.ret, writer);
                } else {
                    try writer.writeAll("void");
                }
            },
            .t_union => {
                for (self.getUnionMembers(idx), 0..) |member, i| {
                    if (i > 0) try writer.writeAll(" | ");
                    try self.writeType(member, writer);
                }
            },
            .t_nullable => {
                try self.writeType(self.getNullableInner(idx), writer);
                try writer.writeAll(" | null");
            },
            .t_ref, .t_generic_param => {
                try writer.writeAll(self.getRefName(idx));
            },
            .t_literal_string => {
                const data = self.getData(idx) orelse return;
                try writer.writeByte('"');
                try writer.writeAll(self.getName(data.a, @intCast(data.b)));
                try writer.writeByte('"');
            },
            .t_literal_number => {
                const data = self.getData(idx) orelse return;
                const val: i16 = @bitCast(data.a);
                try writer.print("{d}", .{val});
            },
            .t_literal_bool => {
                const data = self.getData(idx) orelse return;
                try writer.writeAll(if (data.a == 1) "true" else "false");
            },
            .t_tuple => {
                const data = self.getData(idx) orelse return;
                try writer.writeByte('[');
                const start = data.a;
                const count = data.b;
                var i: u16 = 0;
                while (i < count) : (i += 1) {
                    if (i > 0) try writer.writeAll(", ");
                    if (start + i < self.members.items.len) {
                        try self.writeType(self.members.items[start + i], writer);
                    }
                }
                try writer.writeByte(']');
            },
            .t_template_literal => {
                try writer.writeByte('`');
                for (self.getTemplateParts(idx)) |part| {
                    if (part.kind == .literal) {
                        try writer.writeAll(self.getName(part.name_start, part.name_len));
                    } else {
                        try writer.writeAll("${");
                        try self.writeType(part.type_idx, writer);
                        try writer.writeByte('}');
                    }
                }
                try writer.writeByte('`');
            },
            .t_generic_app => {
                const data = self.getData(idx) orelse return;
                try self.writeType(data.a, writer);
                try writer.writeByte('<');
                const start: u16 = data.b & 0xFF;
                const count: u16 = data.b >> 8;
                var i: u16 = 0;
                while (i < count) : (i += 1) {
                    if (i > 0) try writer.writeAll(", ");
                    if (start + i < self.members.items.len) {
                        try self.writeType(self.members.items[start + i], writer);
                    }
                }
                try writer.writeByte('>');
            },
        }
    }
};

// ---------------------------------------------------------------------------
// Type Expression Parser
// ---------------------------------------------------------------------------

/// Parse a type expression string into the TypePool.
/// Returns the TypeIndex of the parsed type, or null_type_idx on failure.
pub fn parseTypeExpr(pool: *TypePool, allocator: std.mem.Allocator, source: []const u8) TypeIndex {
    var parser = TypeExprParser{
        .pool = pool,
        .allocator = allocator,
        .source = source,
        .pos = 0,
    };
    return parser.parseUnion();
}

const TypeExprParser = struct {
    pool: *TypePool,
    allocator: std.mem.Allocator,
    source: []const u8,
    pos: usize,

    fn parseUnion(self: *TypeExprParser) TypeIndex {
        const first = self.parsePrimary();
        if (first == null_type_idx) return null_type_idx;

        self.skipWs();
        if (!self.match('|')) return first;

        // Collect union members
        var members_buf: [32]TypeIndex = undefined;
        members_buf[0] = first;
        var count: usize = 1;

        while (true) {
            self.skipWs();
            const member = self.parsePrimary();
            if (member == null_type_idx) break;
            if (count < members_buf.len) {
                members_buf[count] = member;
                count += 1;
            }
            self.skipWs();
            if (!self.match('|')) break;
        }

        // Check for optional shorthand: T | undefined
        if (count == 2) {
            const tag0 = self.pool.getTag(members_buf[0]);
            const tag1 = self.pool.getTag(members_buf[1]);
            if (tag1 == .t_undefined) {
                return self.pool.addNullable(self.allocator, members_buf[0]);
            }
            if (tag0 == .t_undefined) {
                return self.pool.addNullable(self.allocator, members_buf[1]);
            }
        }

        return self.pool.addUnion(self.allocator, members_buf[0..count]);
    }

    fn parsePrimary(self: *TypeExprParser) TypeIndex {
        self.skipWs();
        if (self.pos >= self.source.len) return null_type_idx;

        const c = self.source[self.pos];

        // Object type: { ... }
        if (c == '{') return self.parseRecord();

        // Tuple type: [ ... ]
        if (c == '[') return self.parseTuple();

        // Parenthesized type or function type: ( ... )
        if (c == '(') return self.parseFunctionOrParen();

        // String literal type: "..." or '...'
        if (c == '"' or c == '\'') return self.parseStringLiteral();

        // Template literal type: `...${T}...`
        if (c == '`') return self.parseTemplateLiteralType();

        // Number literal (including negative)
        if (std.ascii.isDigit(c) or (c == '-' and self.pos + 1 < self.source.len and std.ascii.isDigit(self.source[self.pos + 1]))) {
            return self.parseNumberLiteral();
        }

        // Identifier-based types
        if (isIdentStart(c)) {
            const ident = self.scanIdent();
            return self.resolveIdentType(ident);
        }

        return null_type_idx;
    }

    fn parseRecord(self: *TypeExprParser) TypeIndex {
        if (!self.match('{')) return null_type_idx;
        self.skipWs();

        var fields_buf: [32]RecordField = undefined;
        var count: usize = 0;

        while (self.pos < self.source.len and self.source[self.pos] != '}') {
            self.skipWs();
            if (self.pos >= self.source.len or self.source[self.pos] == '}') break;

            // Check for readonly modifier
            var is_readonly = false;
            const saved_pos = self.pos;
            const first_ident = self.scanIdent();
            if (first_ident.len == 0) break;
            if (std.mem.eql(u8, first_ident, "readonly")) {
                self.skipWs();
                // Peek: if next char could start an ident, this was a modifier
                if (self.pos < self.source.len and isIdentStart(self.source[self.pos])) {
                    is_readonly = true;
                } else {
                    // "readonly" is the field name itself
                    self.pos = saved_pos + first_ident.len;
                }
            }

            // Parse field name (or use first_ident if not readonly)
            const name_str = if (is_readonly) self.scanIdent() else first_ident;
            if (name_str.len == 0) break;

            self.skipWs();

            // Optional marker
            var optional = false;
            if (self.match('?')) {
                optional = true;
            }

            // Expect colon
            self.skipWs();
            if (!self.match(':')) break;
            self.skipWs();

            // Parse field type (stop at ; or } or ,)
            const field_type = self.parseUnion();

            if (count < fields_buf.len) {
                const n = self.pool.addName(self.allocator, name_str);
                fields_buf[count] = .{
                    .name_start = n.start,
                    .name_len = n.len,
                    .type_idx = field_type,
                    .optional = optional,
                    .readonly = is_readonly,
                };
                count += 1;
            }

            self.skipWs();
            // Skip separator (; or ,)
            if (self.pos < self.source.len and (self.source[self.pos] == ';' or self.source[self.pos] == ',')) {
                self.pos += 1;
            }
        }
        _ = self.match('}');
        return self.pool.addRecord(self.allocator, fields_buf[0..count]);
    }

    fn parseTuple(self: *TypeExprParser) TypeIndex {
        if (!self.match('[')) return null_type_idx;
        self.skipWs();

        var elements_buf: [16]TypeIndex = undefined;
        var count: usize = 0;

        while (self.pos < self.source.len and self.source[self.pos] != ']') {
            self.skipWs();
            const elem = self.parseUnion();
            if (elem == null_type_idx) break;
            if (count < elements_buf.len) {
                elements_buf[count] = elem;
                count += 1;
            }
            self.skipWs();
            _ = self.match(',');
        }
        _ = self.match(']');
        return self.pool.addTuple(self.allocator, elements_buf[0..count]);
    }

    fn parseFunctionOrParen(self: *TypeExprParser) TypeIndex {
        // Save position to backtrack if this is just a paren group
        const saved = self.pos;
        if (!self.match('(')) return null_type_idx;
        self.skipWs();

        // Try to parse as function params
        var params_buf: [16]FuncParam = undefined;
        var param_count: usize = 0;
        var is_function = false;

        while (self.pos < self.source.len and self.source[self.pos] != ')') {
            self.skipWs();
            const param_name = self.scanIdent();
            if (param_name.len == 0) break;

            self.skipWs();
            var optional = false;
            if (self.match('?')) {
                optional = true;
                self.skipWs();
            }

            if (!self.match(':')) {
                // Not a function type - might be parenthesized
                self.pos = saved + 1; // after '('
                const inner = self.parseUnion();
                self.skipWs();
                _ = self.match(')');
                return inner;
            }
            self.skipWs();
            is_function = true;

            const param_type = self.parseUnion();
            if (param_count < params_buf.len) {
                const n = self.pool.addName(self.allocator, param_name);
                params_buf[param_count] = .{
                    .name_start = n.start,
                    .name_len = n.len,
                    .type_idx = param_type,
                    .optional = optional,
                };
                param_count += 1;
            }
            self.skipWs();
            _ = self.match(',');
        }
        _ = self.match(')');
        self.skipWs();

        if (!is_function and param_count == 0) {
            // Empty parens: () => ... is a zero-param function
            // Check for =>
            if (self.matchStr("=>")) {
                self.skipWs();
                const ret = self.parseUnion();
                return self.pool.addFunctionWithReturn(self.allocator, &.{}, ret);
            }
            // Just ()
            return null_type_idx;
        }

        // Check for => return type
        if (self.matchStr("=>")) {
            self.skipWs();
            const ret = self.parseUnion();
            return self.pool.addFunctionWithReturn(self.allocator, params_buf[0..param_count], ret);
        }

        // No arrow - this was function params without return type
        if (is_function) {
            return self.pool.addFunction(self.allocator, params_buf[0..param_count], null_type_idx);
        }

        return null_type_idx;
    }

    fn parseStringLiteral(self: *TypeExprParser) TypeIndex {
        const quote = self.source[self.pos];
        self.pos += 1;
        const start = self.pos;
        while (self.pos < self.source.len and self.source[self.pos] != quote) {
            if (self.source[self.pos] == '\\') self.pos += 1;
            self.pos += 1;
        }
        const end = self.pos;
        _ = self.match(quote);
        return self.pool.addLiteralString(self.allocator, self.source[start..end]);
    }

    fn parseNumberLiteral(self: *TypeExprParser) TypeIndex {
        const start = self.pos;
        if (self.source[self.pos] == '-') self.pos += 1;
        while (self.pos < self.source.len and std.ascii.isDigit(self.source[self.pos])) {
            self.pos += 1;
        }
        const num_str = self.source[start..self.pos];
        const value = std.fmt.parseInt(i16, num_str, 10) catch 0;
        return self.pool.addLiteralNumber(self.allocator, value);
    }

    fn parseTemplateLiteralType(self: *TypeExprParser) TypeIndex {
        if (!self.match('`')) return null_type_idx;

        var parts_buf: [16]TemplatePart = undefined;
        var count: usize = 0;

        while (self.pos < self.source.len and self.source[self.pos] != '`') {
            if (count >= parts_buf.len) break;

            if (self.source[self.pos] == '$' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '{') {
                // Type slot: ${T}
                self.pos += 2; // skip ${
                self.skipWs();
                const slot_type = self.parseUnion();
                self.skipWs();
                _ = self.match('}');
                parts_buf[count] = .{ .kind = .type_slot, .type_idx = slot_type };
                count += 1;
            } else {
                // Literal segment: read until ` or ${
                const seg_start = self.pos;
                while (self.pos < self.source.len and self.source[self.pos] != '`') {
                    if (self.source[self.pos] == '$' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '{') break;
                    self.pos += 1;
                }
                if (self.pos > seg_start) {
                    const n = self.pool.addName(self.allocator, self.source[seg_start..self.pos]);
                    parts_buf[count] = .{ .kind = .literal, .name_start = n.start, .name_len = n.len };
                    count += 1;
                }
            }
        }
        _ = self.match('`');
        if (count == 0) return null_type_idx;
        return self.pool.addTemplateLiteral(self.allocator, parts_buf[0..count]);
    }

    fn resolveIdentType(self: *TypeExprParser, ident: []const u8) TypeIndex {
        // Check primitives
        if (std.mem.eql(u8, ident, "boolean") or std.mem.eql(u8, ident, "bool")) return self.pool.idx_boolean;
        if (std.mem.eql(u8, ident, "number")) return self.pool.idx_number;
        if (std.mem.eql(u8, ident, "string")) return self.pool.idx_string;
        // 'null' type not supported - treat as unknown (parser rejects null literals)
        if (std.mem.eql(u8, ident, "null")) return self.pool.idx_unknown;
        if (std.mem.eql(u8, ident, "undefined")) return self.pool.idx_undefined;
        if (std.mem.eql(u8, ident, "void")) return self.pool.idx_void;
        if (std.mem.eql(u8, ident, "never")) return self.pool.idx_never;
        if (std.mem.eql(u8, ident, "unknown")) return self.pool.idx_unknown;
        if (std.mem.eql(u8, ident, "true")) return self.pool.addLiteralBool(self.allocator, true);
        if (std.mem.eql(u8, ident, "false")) return self.pool.addLiteralBool(self.allocator, false);

        // Check for array suffix: T[]
        self.skipWs();
        if (self.pos + 1 < self.source.len and self.source[self.pos] == '[' and self.source[self.pos + 1] == ']') {
            self.pos += 2;
            const ref = self.pool.addRef(self.allocator, ident);
            return self.pool.addArray(self.allocator, ref);
        }

        // Check for generic application: Foo<T>
        if (self.pos < self.source.len and self.source[self.pos] == '<') {
            return self.parseGenericApp(ident);
        }

        // Named type reference
        // Also check for array suffix after the ref: `number[]`
        const ref = self.pool.addRef(self.allocator, ident);

        // Post-fix array brackets on primitive-resolved types
        if (self.pos + 1 < self.source.len and self.source[self.pos] == '[' and self.source[self.pos + 1] == ']') {
            self.pos += 2;
            // For primitives that resolved to an index, wrap in array
            if (std.mem.eql(u8, ident, "number") or std.mem.eql(u8, ident, "string") or std.mem.eql(u8, ident, "boolean")) {
                const prim = if (std.mem.eql(u8, ident, "number")) self.pool.idx_number else if (std.mem.eql(u8, ident, "string")) self.pool.idx_string else self.pool.idx_boolean;
                return self.pool.addArray(self.allocator, prim);
            }
            return self.pool.addArray(self.allocator, ref);
        }

        return ref;
    }

    fn parseGenericApp(self: *TypeExprParser, base_name: []const u8) TypeIndex {
        if (!self.match('<')) return null_type_idx;

        // Check if base is a known generic: Array<T> -> T[]
        const is_array = std.mem.eql(u8, base_name, "Array");

        var args_buf: [8]TypeIndex = undefined;
        var count: usize = 0;

        while (self.pos < self.source.len and self.source[self.pos] != '>') {
            self.skipWs();
            const arg = self.parseUnion();
            if (arg == null_type_idx) break;
            if (count < args_buf.len) {
                args_buf[count] = arg;
                count += 1;
            }
            self.skipWs();
            _ = self.match(',');
        }
        _ = self.match('>');

        // Array<T> -> T[]
        if (is_array and count == 1) {
            return self.pool.addArray(self.allocator, args_buf[0]);
        }

        // Readonly<T> -> record with all fields readonly
        if (std.mem.eql(u8, base_name, "Readonly") and count == 1) {
            return self.pool.makeReadonly(self.allocator, args_buf[0]);
        }

        const base = self.pool.addRef(self.allocator, base_name);
        return self.pool.addGenericApp(self.allocator, base, args_buf[0..count]);
    }

    // Helpers

    fn skipWs(self: *TypeExprParser) void {
        while (self.pos < self.source.len and (self.source[self.pos] == ' ' or self.source[self.pos] == '\t' or self.source[self.pos] == '\n' or self.source[self.pos] == '\r')) {
            self.pos += 1;
        }
    }

    fn match(self: *TypeExprParser, expected: u8) bool {
        if (self.pos < self.source.len and self.source[self.pos] == expected) {
            self.pos += 1;
            return true;
        }
        return false;
    }

    fn matchStr(self: *TypeExprParser, expected: []const u8) bool {
        if (self.pos + expected.len <= self.source.len and
            std.mem.eql(u8, self.source[self.pos .. self.pos + expected.len], expected))
        {
            self.pos += expected.len;
            return true;
        }
        return false;
    }

    fn scanIdent(self: *TypeExprParser) []const u8 {
        const start = self.pos;
        if (self.pos >= self.source.len or !isIdentStart(self.source[self.pos])) return "";
        while (self.pos < self.source.len and isIdentContinue(self.source[self.pos])) {
            self.pos += 1;
        }
        return self.source[start..self.pos];
    }
};

fn isIdentStart(c: u8) bool {
    return std.ascii.isAlphabetic(c) or c == '_' or c == '$';
}

fn isIdentContinue(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_' or c == '$';
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "TypePool primitives" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    try std.testing.expect(pool.idx_boolean != null_type_idx);
    try std.testing.expect(pool.idx_number != null_type_idx);
    try std.testing.expect(pool.idx_string != null_type_idx);
    try std.testing.expectEqual(TypeTag.t_boolean, pool.getTag(pool.idx_boolean).?);
    try std.testing.expectEqual(TypeTag.t_number, pool.getTag(pool.idx_number).?);
}

test "TypePool record" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const n1 = pool.addName(allocator, "x");
    const n2 = pool.addName(allocator, "y");
    const rec = pool.addRecord(allocator, &.{
        .{ .name_start = n1.start, .name_len = n1.len, .type_idx = pool.idx_number, .optional = false },
        .{ .name_start = n2.start, .name_len = n2.len, .type_idx = pool.idx_string, .optional = true },
    });

    try std.testing.expectEqual(TypeTag.t_record, pool.getTag(rec).?);
    const fields = pool.getRecordFields(rec);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    try std.testing.expectEqualStrings("x", pool.getName(fields[0].name_start, fields[0].name_len));
    try std.testing.expect(!fields[0].optional);
    try std.testing.expect(fields[1].optional);
}

test "TypePool union" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const u = pool.addUnion(allocator, &.{ pool.idx_string, pool.idx_number });
    try std.testing.expectEqual(TypeTag.t_union, pool.getTag(u).?);
    const members = pool.getUnionMembers(u);
    try std.testing.expectEqual(@as(usize, 2), members.len);
}

test "TypePool single member union flattens" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const u = pool.addUnion(allocator, &.{pool.idx_string});
    try std.testing.expectEqual(pool.idx_string, u);
}

test "isAssignableTo basics" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    // Same type
    try std.testing.expect(pool.isAssignableTo(pool.idx_number, pool.idx_number));
    // Different primitive
    try std.testing.expect(!pool.isAssignableTo(pool.idx_number, pool.idx_string));
    // never assignable to anything
    try std.testing.expect(pool.isAssignableTo(pool.idx_never, pool.idx_string));
    // unknown target accepts anything
    try std.testing.expect(pool.isAssignableTo(pool.idx_number, pool.idx_unknown));
}

test "isAssignableTo literal to base" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const lit_get = pool.addLiteralString(allocator, "GET");
    try std.testing.expect(pool.isAssignableTo(lit_get, pool.idx_string));
    try std.testing.expect(!pool.isAssignableTo(lit_get, pool.idx_number));

    const lit_200 = pool.addLiteralNumber(allocator, 200);
    try std.testing.expect(pool.isAssignableTo(lit_200, pool.idx_number));
    try std.testing.expect(!pool.isAssignableTo(lit_200, pool.idx_string));
}

test "isAssignableTo record structural" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const nx = pool.addName(allocator, "x");
    const ny = pool.addName(allocator, "y");

    // source: { x: number, y: string }
    const src = pool.addRecord(allocator, &.{
        .{ .name_start = nx.start, .name_len = nx.len, .type_idx = pool.idx_number, .optional = false },
        .{ .name_start = ny.start, .name_len = ny.len, .type_idx = pool.idx_string, .optional = false },
    });

    // target: { x: number } (subset - should be assignable)
    const nx2 = pool.addName(allocator, "x");
    const tgt = pool.addRecord(allocator, &.{
        .{ .name_start = nx2.start, .name_len = nx2.len, .type_idx = pool.idx_number, .optional = false },
    });

    try std.testing.expect(pool.isAssignableTo(src, tgt));
    try std.testing.expect(!pool.isAssignableTo(tgt, src)); // missing required field y
}

test "isAssignableTo nullable" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const nullable_str = pool.addNullable(allocator, pool.idx_string);
    // string is assignable to string | null
    try std.testing.expect(pool.isAssignableTo(pool.idx_string, nullable_str));
    // null is assignable to string | null
    try std.testing.expect(pool.isAssignableTo(pool.idx_null, nullable_str));
    // string | null is NOT assignable to string (might be null)
    try std.testing.expect(!pool.isAssignableTo(nullable_str, pool.idx_string));
}

test "isAssignableTo union" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const str_or_num = pool.addUnion(allocator, &.{ pool.idx_string, pool.idx_number });
    // string is assignable to string | number
    try std.testing.expect(pool.isAssignableTo(pool.idx_string, str_or_num));
    // boolean is NOT assignable to string | number
    try std.testing.expect(!pool.isAssignableTo(pool.idx_boolean, str_or_num));
}

test "parseTypeExpr primitives" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    try std.testing.expectEqual(pool.idx_number, parseTypeExpr(&pool, allocator, "number"));
    try std.testing.expectEqual(pool.idx_string, parseTypeExpr(&pool, allocator, "string"));
    try std.testing.expectEqual(pool.idx_boolean, parseTypeExpr(&pool, allocator, "boolean"));
    try std.testing.expectEqual(pool.idx_void, parseTypeExpr(&pool, allocator, "void"));
    // 'null' type resolves to unknown (not supported)
    try std.testing.expectEqual(pool.idx_unknown, parseTypeExpr(&pool, allocator, "null"));
}

test "parseTypeExpr union" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = parseTypeExpr(&pool, allocator, "string | number");
    try std.testing.expectEqual(TypeTag.t_union, pool.getTag(idx).?);
    const members = pool.getUnionMembers(idx);
    try std.testing.expectEqual(@as(usize, 2), members.len);
}

test "parseTypeExpr optional" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = parseTypeExpr(&pool, allocator, "string | undefined");
    try std.testing.expectEqual(TypeTag.t_nullable, pool.getTag(idx).?);
    try std.testing.expectEqual(pool.idx_string, pool.getNullableInner(idx));
}

test "parseTypeExpr record" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = parseTypeExpr(&pool, allocator, "{ ok: boolean; value: string }");
    try std.testing.expectEqual(TypeTag.t_record, pool.getTag(idx).?);
    const fields = pool.getRecordFields(idx);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    try std.testing.expectEqualStrings("ok", pool.getName(fields[0].name_start, fields[0].name_len));
    try std.testing.expectEqualStrings("value", pool.getName(fields[1].name_start, fields[1].name_len));
}

test "parseTypeExpr function" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = parseTypeExpr(&pool, allocator, "(key: string) => boolean");
    try std.testing.expectEqual(TypeTag.t_function, pool.getTag(idx).?);
}

test "parseTypeExpr array" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = parseTypeExpr(&pool, allocator, "Array<number>");
    try std.testing.expectEqual(TypeTag.t_array, pool.getTag(idx).?);
    try std.testing.expectEqual(pool.idx_number, pool.getArrayElement(idx));
}

test "parseTypeExpr string literal" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = parseTypeExpr(&pool, allocator, "\"GET\"");
    try std.testing.expectEqual(TypeTag.t_literal_string, pool.getTag(idx).?);
}

test "parseTypeExpr named ref" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = parseTypeExpr(&pool, allocator, "Response");
    try std.testing.expectEqual(TypeTag.t_ref, pool.getTag(idx).?);
    try std.testing.expectEqualStrings("Response", pool.getRefName(idx));
}

test "formatType" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var buf: [256]u8 = undefined;
    try std.testing.expectEqualStrings("number", pool.formatType(pool.idx_number, &buf));
    try std.testing.expectEqualStrings("boolean", pool.formatType(pool.idx_boolean, &buf));
    try std.testing.expectEqualStrings("unknown", pool.formatType(null_type_idx, &buf));
}

test "TypePool instantiate generic" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    // type Result<T> = { ok: boolean; value: T }
    const t_param = pool.addGenericParam(allocator, "T");
    const ok_name = pool.addName(allocator, "ok");
    const val_name = pool.addName(allocator, "value");
    const result_type = pool.addRecord(allocator, &.{
        .{ .name_start = ok_name.start, .name_len = ok_name.len, .type_idx = pool.idx_boolean, .optional = false },
        .{ .name_start = val_name.start, .name_len = val_name.len, .type_idx = t_param, .optional = false },
    });

    // Instantiate Result<string>
    const instantiated = pool.instantiate(
        allocator,
        result_type,
        &.{"T"},
        &.{pool.idx_string},
        0,
    );

    try std.testing.expectEqual(TypeTag.t_record, pool.getTag(instantiated).?);
    const fields = pool.getRecordFields(instantiated);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    // "ok" field should still be boolean
    try std.testing.expectEqual(pool.idx_boolean, fields[0].type_idx);
    // "value" field should now be string (was T)
    try std.testing.expectEqual(pool.idx_string, fields[1].type_idx);
}

test "findUnionMemberByDiscriminant matches correct member" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    // Build { kind: "ok", value: string }
    const kind_ok = pool.addLiteralString(allocator, "ok");
    const ok_member = pool.addRecord(allocator, &.{
        .{ .name_start = blk: {
            const n = pool.addName(allocator, "kind");
            break :blk n.start;
        }, .name_len = 4, .type_idx = kind_ok, .optional = false },
        .{ .name_start = blk: {
            const n = pool.addName(allocator, "value");
            break :blk n.start;
        }, .name_len = 5, .type_idx = pool.idx_string, .optional = false },
    });

    // Build { kind: "err", error: string }
    const kind_err = pool.addLiteralString(allocator, "err");
    const err_member = pool.addRecord(allocator, &.{
        .{ .name_start = blk: {
            const n = pool.addName(allocator, "kind");
            break :blk n.start;
        }, .name_len = 4, .type_idx = kind_err, .optional = false },
        .{ .name_start = blk: {
            const n = pool.addName(allocator, "error");
            break :blk n.start;
        }, .name_len = 5, .type_idx = pool.idx_string, .optional = false },
    });

    // Build union
    const union_type = pool.addUnion(allocator, &.{ ok_member, err_member });

    // Find by discriminant
    try std.testing.expectEqual(ok_member, pool.findUnionMemberByDiscriminant(union_type, "kind", "ok").?);
    try std.testing.expectEqual(err_member, pool.findUnionMemberByDiscriminant(union_type, "kind", "err").?);
    try std.testing.expect(pool.findUnionMemberByDiscriminant(union_type, "kind", "unknown") == null);
    try std.testing.expect(pool.findUnionMemberByDiscriminant(union_type, "type", "ok") == null);

    // Exclude member
    const excluded = pool.excludeUnionMember(allocator, union_type, ok_member);
    try std.testing.expectEqual(err_member, excluded);
}

test "parseTypeExpr parses readonly record fields" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = parseTypeExpr(&pool, allocator, "{ readonly port: number; host: string }");
    try std.testing.expectEqual(TypeTag.t_record, pool.getTag(idx).?);
    const fields = pool.getRecordFields(idx);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    // port should be readonly
    try std.testing.expectEqualStrings("port", pool.getName(fields[0].name_start, fields[0].name_len));
    try std.testing.expect(fields[0].readonly);
    try std.testing.expectEqual(pool.idx_number, fields[0].type_idx);
    // host should not be readonly
    try std.testing.expectEqualStrings("host", pool.getName(fields[1].name_start, fields[1].name_len));
    try std.testing.expect(!fields[1].readonly);
    try std.testing.expectEqual(pool.idx_string, fields[1].type_idx);
}

test "makeReadonly marks all fields readonly" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = parseTypeExpr(&pool, allocator, "{ port: number; host: string }");
    const ro = pool.makeReadonly(allocator, idx);
    const fields = pool.getRecordFields(ro);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    try std.testing.expect(fields[0].readonly);
    try std.testing.expect(fields[1].readonly);
}

test "parseTypeExpr template literal type" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = parseTypeExpr(&pool, allocator, "`/api/${string}`");
    try std.testing.expectEqual(TypeTag.t_template_literal, pool.getTag(idx).?);
    const parts = pool.getTemplateParts(idx);
    try std.testing.expectEqual(@as(usize, 2), parts.len);
    try std.testing.expectEqual(.literal, parts[0].kind);
    try std.testing.expectEqualStrings("/api/", pool.getName(parts[0].name_start, parts[0].name_len));
    try std.testing.expectEqual(.type_slot, parts[1].kind);
    try std.testing.expectEqual(pool.idx_string, parts[1].type_idx);
}

test "matchesTemplateLiteral matches correctly" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const template = parseTypeExpr(&pool, allocator, "`/api/${string}`");
    const good = pool.addLiteralString(allocator, "/api/users");
    const bad = pool.addLiteralString(allocator, "/other");

    try std.testing.expect(pool.matchesTemplateLiteral(good, template));
    try std.testing.expect(!pool.matchesTemplateLiteral(bad, template));
}

test "template literal type assignability" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const template = parseTypeExpr(&pool, allocator, "`/api/${string}`");
    const good = pool.addLiteralString(allocator, "/api/users");
    const bad = pool.addLiteralString(allocator, "/other");

    try std.testing.expect(pool.isAssignableTo(good, template));
    try std.testing.expect(!pool.isAssignableTo(bad, template));
}
