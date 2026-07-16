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

/// Sentinel for semantic absence: no annotation, unresolved inference, or an
/// invalid type expression. Operational failures are recorded separately on
/// `TypePool` and must be checked with `ensureHealthy` before consuming types.
pub const null_type_idx: TypeIndex = std.math.maxInt(TypeIndex);

/// A poisoned pool cannot safely support type or proof decisions. The first
/// failure is sticky because compound constructors may already have appended
/// partial backing storage by the time the failure becomes observable.
pub const TypePoolError = error{
    TypePoolCapacityExceeded,
} || std.mem.Allocator.Error;

pub const NameRef = struct {
    start: u16,
    len: u8,
};

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
    t_intersection, // T & U & V

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
    /// Operational failure is separate from `null_type_idx`, which remains a
    /// semantic absence sentinel. Once poisoned, the pool must be discarded.
    failure: ?TypePoolError = null,

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

    /// Reject any type/proof result produced after the pool failed to allocate
    /// or outgrew its compact representation.
    pub fn ensureHealthy(self: *const TypePool) TypePoolError!void {
        if (self.failure) |failure| return failure;
    }

    fn recordFailure(self: *TypePool, failure: TypePoolError) void {
        if (self.failure == null) self.failure = failure;
    }

    fn isPoisoned(self: *const TypePool) bool {
        return self.failure != null;
    }

    fn failIndex(self: *TypePool, failure: TypePoolError) TypeIndex {
        self.recordFailure(failure);
        return null_type_idx;
    }

    fn failName(self: *TypePool, failure: TypePoolError) NameRef {
        self.recordFailure(failure);
        return .{ .start = 0, .len = 0 };
    }

    // -------------------------------------------------------------------
    // Node creation
    // -------------------------------------------------------------------

    fn addNode(self: *TypePool, allocator: std.mem.Allocator, node: TypeNode) TypeIndex {
        if (self.isPoisoned()) return null_type_idx;
        // TypeIndex is u16 and null_type_idx == maxInt(u16). Fail closed before
        // the cast can either truncate (silent wrong-node aliasing in ReleaseFast)
        // or produce an index that collides with null_type_idx. This guards every
        // producer below, since they all route through addNode.
        if (self.nodes.items.len >= std.math.maxInt(u16)) {
            return self.failIndex(error.TypePoolCapacityExceeded);
        }
        const idx: TypeIndex = @intCast(self.nodes.items.len);
        self.nodes.append(allocator, node) catch return self.failIndex(error.OutOfMemory);
        return idx;
    }

    fn fitsU16(value: usize) bool {
        return value <= std.math.maxInt(u16);
    }

    fn fitsU16Range(start: usize, count: usize) bool {
        return fitsU16(start) and fitsU16(count) and count <= std.math.maxInt(usize) - start;
    }

    fn fitsPackedFunction(start: usize, count: usize) bool {
        return start <= 0x0FFF and count <= 15;
    }

    /// Store a name string and return (start, len).
    pub fn addName(self: *TypePool, allocator: std.mem.Allocator, name: []const u8) NameRef {
        if (self.isPoisoned()) return .{ .start = 0, .len = 0 };
        // The names arena offset is stored as u16; once it exceeds u16 range a
        // further @intCast would truncate (mis-identifying a field/param/ref name
        // -> unsound compares) or panic. Fail closed with an empty (unknown) name.
        if (self.names.items.len > std.math.maxInt(u16)) {
            return self.failName(error.TypePoolCapacityExceeded);
        }
        const start: u16 = @intCast(self.names.items.len);
        const len: u8 = @intCast(@min(name.len, 255));
        self.names.appendSlice(allocator, name[0..len]) catch return self.failName(error.OutOfMemory);
        return .{ .start = start, .len = len };
    }

    /// Get a name string by (start, len).
    pub fn getName(self: *const TypePool, start: u16, len: u8) []const u8 {
        // Widen before adding: `start` (u16) + `len` (u8) peer-resolves to u16
        // and would overflow-panic for a name stored near the 64KB mark before
        // the bounds guard could run. The guard must fail closed, not crash.
        const end: usize = @as(usize, start) + @as(usize, len);
        if (end > self.names.items.len) return "";
        return self.names.items[start..end];
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
        if (self.isPoisoned()) return null_type_idx;
        const start = self.fields.items.len;
        if (!fitsU16Range(start, record_fields.len)) {
            return self.failIndex(error.TypePoolCapacityExceeded);
        }
        self.fields.appendSlice(allocator, record_fields) catch return self.failIndex(error.OutOfMemory);
        return self.addNode(allocator, .{
            .tag = .t_record,
            .data = .{ .a = @intCast(start), .b = @intCast(record_fields.len) },
        });
    }

    const UnionMemberScan = struct {
        deduped: [16]TypeIndex = undefined,
        deduped_count: usize = 0,
        flattened_count: usize = 0,
        dedup_overflow: bool = false,
    };

    fn scanUnionMembers(
        self: *const TypePool,
        union_members: []const TypeIndex,
        scan: *UnionMemberScan,
    ) bool {
        for (union_members) |member| {
            if (self.getTag(member) == .t_union) {
                if (!self.scanUnionMembers(self.getUnionMembers(member), scan)) return false;
                continue;
            }

            scan.flattened_count = std.math.add(usize, scan.flattened_count, 1) catch return false;
            if (scan.dedup_overflow) continue;
            if (std.mem.findScalar(TypeIndex, scan.deduped[0..scan.deduped_count], member) != null) continue;
            if (scan.deduped_count == scan.deduped.len) {
                scan.dedup_overflow = true;
                continue;
            }
            scan.deduped[scan.deduped_count] = member;
            scan.deduped_count += 1;
        }
        return true;
    }

    fn appendFlattenedUnionMembersAssumeCapacity(self: *TypePool, union_members: []const TypeIndex) void {
        for (union_members) |member| {
            if (self.getTag(member) == .t_union) {
                self.appendFlattenedUnionMembersAssumeCapacity(self.getUnionMembers(member));
            } else {
                self.members.appendAssumeCapacity(member);
            }
        }
    }

    /// Create a union type from members. A sole member is returned unchanged.
    /// With multiple inputs, nested unions are flattened. Exact-index duplicates
    /// collapse while the sixteen-member distinct scratch buffer suffices; on
    /// overflow, the raw flattened sequence is retained losslessly.
    pub fn addUnion(self: *TypePool, allocator: std.mem.Allocator, union_members: []const TypeIndex) TypeIndex {
        if (self.isPoisoned()) return null_type_idx;
        if (union_members.len == 1) return union_members[0];

        var scan: UnionMemberScan = .{};
        if (!self.scanUnionMembers(union_members, &scan)) {
            return self.failIndex(error.TypePoolCapacityExceeded);
        }
        if (!scan.dedup_overflow) {
            if (scan.deduped_count == 1) return scan.deduped[0];

            const start = self.members.items.len;
            if (!fitsU16Range(start, scan.deduped_count)) {
                return self.failIndex(error.TypePoolCapacityExceeded);
            }
            self.members.appendSlice(allocator, scan.deduped[0..scan.deduped_count]) catch return self.failIndex(error.OutOfMemory);
            return self.addNode(allocator, .{
                .tag = .t_union,
                .data = .{ .a = @intCast(start), .b = @intCast(scan.deduped_count) },
            });
        }

        // More than sixteen distinct members cannot be deduplicated in the
        // bounded scratch buffer. Fall back to the raw flattened sequence so
        // no source-union member is silently dropped: every member must remain
        // assignable to the target for isAssignableTo to accept the union.
        const start = self.members.items.len;
        if (!fitsU16Range(start, scan.flattened_count)) {
            return self.failIndex(error.TypePoolCapacityExceeded);
        }
        self.members.ensureUnusedCapacity(allocator, scan.flattened_count) catch return self.failIndex(error.OutOfMemory);
        self.appendFlattenedUnionMembersAssumeCapacity(union_members);
        return self.addNode(allocator, .{
            .tag = .t_union,
            .data = .{ .a = @intCast(start), .b = @intCast(scan.flattened_count) },
        });
    }

    /// Create an intersection type from members. Single-member intersections
    /// collapse to that member; consecutive duplicate TypeIndexes are dropped.
    /// Empty input returns null_type_idx.
    pub fn addIntersection(self: *TypePool, allocator: std.mem.Allocator, intersection_members: []const TypeIndex) TypeIndex {
        if (self.isPoisoned()) return null_type_idx;
        if (intersection_members.len == 0) return null_type_idx;

        // When the input has more distinct members than the dedup buffer can
        // hold, skip dedup and append the raw non-null members so no obligation
        // is silently dropped. A dropped target-intersection member is a dropped
        // constraint -> unsound accept in isAssignableTo.
        if (intersection_members.len > 16) {
            var raw_count: usize = 0;
            for (intersection_members) |m| {
                if (m == null_type_idx) continue;
                raw_count += 1;
            }
            if (raw_count == 0) return null_type_idx;
            if (raw_count == 1) {
                for (intersection_members) |m| {
                    if (m != null_type_idx) return m;
                }
                return null_type_idx;
            }

            const start_raw = self.members.items.len;
            if (!fitsU16Range(start_raw, raw_count)) {
                return self.failIndex(error.TypePoolCapacityExceeded);
            }
            for (intersection_members) |m| {
                if (m == null_type_idx) continue;
                self.members.append(allocator, m) catch return self.failIndex(error.OutOfMemory);
            }
            return self.addNode(allocator, .{
                .tag = .t_intersection,
                .data = .{ .a = @intCast(start_raw), .b = @intCast(raw_count) },
            });
        }

        var deduped: [16]TypeIndex = undefined;
        var count: usize = 0;
        for (intersection_members) |m| {
            if (m == null_type_idx) continue;
            var seen = false;
            for (deduped[0..count]) |existing| {
                if (existing == m) {
                    seen = true;
                    break;
                }
            }
            if (!seen and count < deduped.len) {
                deduped[count] = m;
                count += 1;
            }
        }
        if (count == 0) return null_type_idx;
        if (count == 1) return deduped[0];

        const start = self.members.items.len;
        if (!fitsU16Range(start, count)) {
            return self.failIndex(error.TypePoolCapacityExceeded);
        }
        self.members.appendSlice(allocator, deduped[0..count]) catch return self.failIndex(error.OutOfMemory);
        return self.addNode(allocator, .{
            .tag = .t_intersection,
            .data = .{ .a = @intCast(start), .b = @intCast(count) },
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
        if (self.isPoisoned()) return null_type_idx;
        const start = self.members.items.len;
        if (!fitsU16Range(start, elements.len)) {
            return self.failIndex(error.TypePoolCapacityExceeded);
        }
        self.members.appendSlice(allocator, elements) catch return self.failIndex(error.OutOfMemory);
        return self.addNode(allocator, .{
            .tag = .t_tuple,
            .data = .{ .a = @intCast(start), .b = @intCast(elements.len) },
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
        if (self.isPoisoned()) return null_type_idx;
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
                // Copy fields before the loop: getRecordFields returns a slice
                // into the pool's shared fields list, which a nested instantiate
                // (addRecord -> appendSlice) may reallocate, dangling the slice
                // mid-loop (UAF / garbage TypeIndex).
                const live = self.getRecordFields(idx);
                const new_fields = allocator.alloc(RecordField, live.len) catch {
                    return self.failIndex(error.OutOfMemory);
                };
                defer allocator.free(new_fields);
                @memcpy(new_fields, live);
                for (0..new_fields.len) |i| {
                    new_fields[i].type_idx = self.instantiate(allocator, new_fields[i].type_idx, param_names, param_types, depth + 1);
                }
                return self.addRecord(allocator, new_fields);
            },
            .t_array => {
                const elem = self.getArrayElement(idx);
                const new_elem = self.instantiate(allocator, elem, param_names, param_types, depth + 1);
                if (new_elem == elem) return idx;
                return self.addArray(allocator, new_elem);
            },
            .t_union => {
                // Copy members before the loop (shared members list may realloc).
                const live = self.getUnionMembers(idx);
                const new_members = allocator.alloc(TypeIndex, live.len) catch {
                    return self.failIndex(error.OutOfMemory);
                };
                defer allocator.free(new_members);
                @memcpy(new_members, live);
                var changed = false;
                for (new_members) |*m| {
                    const old = m.*;
                    m.* = self.instantiate(allocator, old, param_names, param_types, depth + 1);
                    if (m.* != old) changed = true;
                }
                if (!changed) return idx;
                return self.addUnion(allocator, new_members);
            },
            .t_intersection => {
                // Copy members before the loop (shared members list may realloc).
                const live = self.getIntersectionMembers(idx);
                const new_members = allocator.alloc(TypeIndex, live.len) catch {
                    return self.failIndex(error.OutOfMemory);
                };
                defer allocator.free(new_members);
                @memcpy(new_members, live);
                var changed = false;
                for (new_members) |*m| {
                    const old = m.*;
                    m.* = self.instantiate(allocator, old, param_names, param_types, depth + 1);
                    if (m.* != old) changed = true;
                }
                if (!changed) return idx;
                return self.addIntersection(allocator, new_members);
            },
            .t_nullable => {
                const inner = self.getNullableInner(idx);
                const new_inner = self.instantiate(allocator, inner, param_names, param_types, depth + 1);
                if (new_inner == inner) return idx;
                return self.addNullable(allocator, new_inner);
            },
            .t_generic_app => {
                // Copy args before the loop (shared members list may realloc on
                // a nested instantiate -> addGenericApp/addArray).
                const info = self.getGenericAppInfo(idx);
                const new_args = allocator.alloc(TypeIndex, info.args.len) catch {
                    return self.failIndex(error.OutOfMemory);
                };
                defer allocator.free(new_args);
                @memcpy(new_args, info.args);
                var changed = false;
                for (new_args) |*arg| {
                    const old = arg.*;
                    arg.* = self.instantiate(allocator, old, param_names, param_types, depth + 1);
                    if (arg.* != old) changed = true;
                }
                if (!changed) return idx;
                return self.addGenericApp(allocator, info.base, new_args);
            },
            else => return idx,
        }
    }

    /// Create a generic application (e.g., Array<number>).
    pub fn addGenericApp(self: *TypePool, allocator: std.mem.Allocator, base: TypeIndex, type_args: []const TypeIndex) TypeIndex {
        if (self.isPoisoned()) return null_type_idx;
        // Layout: members[start] = count, members[start+1..start+1+count] = type args.
        // Storing count inline avoids the 8-bit truncation of the previous (start&0xFF)|(count<<8) scheme.
        const start = self.members.items.len;
        if (!fitsU16(start) or !fitsU16(type_args.len)) {
            return self.failIndex(error.TypePoolCapacityExceeded);
        }
        self.members.append(allocator, @intCast(type_args.len)) catch return self.failIndex(error.OutOfMemory);
        self.members.appendSlice(allocator, type_args) catch return self.failIndex(error.OutOfMemory);
        return self.addNode(allocator, .{
            .tag = .t_generic_app,
            .data = .{ .a = base, .b = @intCast(start) },
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
        const end: usize = @as(usize, start) + @as(usize, count);
        if (end > self.fields.items.len) return &.{};
        return self.fields.items[start..end];
    }

    /// Get union members.
    pub fn getUnionMembers(self: *const TypePool, idx: TypeIndex) []const TypeIndex {
        const data = self.getData(idx) orelse return &.{};
        if (self.getTag(idx) != .t_union) return &.{};
        const start = data.a;
        const count = data.b;
        const end: usize = @as(usize, start) + @as(usize, count);
        if (end > self.members.items.len) return &.{};
        return self.members.items[start..end];
    }

    /// Get intersection members.
    pub fn getIntersectionMembers(self: *const TypePool, idx: TypeIndex) []const TypeIndex {
        const data = self.getData(idx) orelse return &.{};
        if (self.getTag(idx) != .t_intersection) return &.{};
        const start = data.a;
        const count = data.b;
        const end: usize = @as(usize, start) + @as(usize, count);
        if (end > self.members.items.len) return &.{};
        return self.members.items[start..end];
    }

    /// Get tuple elements.
    pub fn getTupleElements(self: *const TypePool, idx: TypeIndex) []const TypeIndex {
        const data = self.getData(idx) orelse return &.{};
        if (self.getTag(idx) != .t_tuple) return &.{};
        const start = data.a;
        const count = data.b;
        const end: usize = @as(usize, start) + @as(usize, count);
        if (end > self.members.items.len) return &.{};
        return self.members.items[start..end];
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
        if (self.isPoisoned()) return null_type_idx;
        const fields = self.getRecordFields(idx);
        if (fields.len == 0) return idx;
        const ro_fields = allocator.alloc(RecordField, fields.len) catch {
            return self.failIndex(error.OutOfMemory);
        };
        defer allocator.free(ro_fields);
        @memcpy(ro_fields, fields);
        for (ro_fields) |*field| {
            field.readonly = true;
        }
        return self.addRecord(allocator, ro_fields);
    }

    /// Create a new record type with all fields optional (Partial<T> utility).
    pub fn makePartial(self: *TypePool, allocator: std.mem.Allocator, idx: TypeIndex) TypeIndex {
        if (self.isPoisoned()) return null_type_idx;
        const fields = self.getRecordFields(idx);
        if (fields.len == 0) return idx;
        const out = allocator.alloc(RecordField, fields.len) catch {
            return self.failIndex(error.OutOfMemory);
        };
        defer allocator.free(out);
        @memcpy(out, fields);
        for (out) |*field| {
            field.optional = true;
        }
        return self.addRecord(allocator, out);
    }

    /// Create a new record type with all fields required (Required<T> utility).
    pub fn makeRequired(self: *TypePool, allocator: std.mem.Allocator, idx: TypeIndex) TypeIndex {
        if (self.isPoisoned()) return null_type_idx;
        const fields = self.getRecordFields(idx);
        if (fields.len == 0) return idx;
        const out = allocator.alloc(RecordField, fields.len) catch {
            return self.failIndex(error.OutOfMemory);
        };
        defer allocator.free(out);
        @memcpy(out, fields);
        for (out) |*field| {
            field.optional = false;
        }
        return self.addRecord(allocator, out);
    }

    /// Create a new record type with only the named fields (Pick<T, Keys>).
    /// `keys_idx` is a string literal or a union of string literals. A
    /// non-literal key argument or non-record source returns the source
    /// unchanged.
    pub fn pickFields(self: *TypePool, allocator: std.mem.Allocator, record_idx: TypeIndex, keys_idx: TypeIndex) TypeIndex {
        if (self.isPoisoned()) return null_type_idx;
        const fields = self.getRecordFields(record_idx);
        if (fields.len == 0) return record_idx;

        var out: std.ArrayListUnmanaged(RecordField) = .empty;
        defer out.deinit(allocator);

        var saw_literal_key = false;
        for (fields) |field| {
            const name = self.getName(field.name_start, field.name_len);
            if (self.stringKeySetContains(keys_idx, name, &saw_literal_key)) {
                out.append(allocator, field) catch return self.failIndex(error.OutOfMemory);
            }
        }

        if (!saw_literal_key) return record_idx;
        return self.addRecord(allocator, out.items);
    }

    /// Create a new record type without the named fields (Omit<T, Keys>).
    /// `keys_idx` is a string literal or a union of string literals. A
    /// non-literal key argument or non-record source returns the source
    /// unchanged.
    pub fn omitFields(self: *TypePool, allocator: std.mem.Allocator, record_idx: TypeIndex, keys_idx: TypeIndex) TypeIndex {
        if (self.isPoisoned()) return null_type_idx;
        const fields = self.getRecordFields(record_idx);
        if (fields.len == 0) return record_idx;

        var out: std.ArrayListUnmanaged(RecordField) = .empty;
        defer out.deinit(allocator);

        var saw_literal_key = false;
        for (fields) |field| {
            const name = self.getName(field.name_start, field.name_len);
            if (!self.stringKeySetContains(keys_idx, name, &saw_literal_key)) {
                out.append(allocator, field) catch return self.failIndex(error.OutOfMemory);
            }
        }

        if (!saw_literal_key) return record_idx;
        return self.addRecord(allocator, out.items);
    }

    /// Check whether a Pick/Omit key argument contains a field name. Literal
    /// keys may be provided as one string literal or a union of string literals.
    /// Non-literal members are ignored but do not cap the number of literal keys.
    fn stringKeySetContains(self: *const TypePool, keys_idx: TypeIndex, field_name: []const u8, saw_literal_key: *bool) bool {
        const tag = self.getTag(keys_idx) orelse return false;
        if (tag == .t_literal_string) {
            if (self.getLiteralStringValue(keys_idx)) |v| {
                saw_literal_key.* = true;
                return std.mem.eql(u8, field_name, v);
            }
            return false;
        }
        if (tag == .t_union) {
            for (self.getUnionMembers(keys_idx)) |m| {
                if (self.stringKeySetContains(m, field_name, saw_literal_key)) return true;
            }
        }
        return false;
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
        if (self.isPoisoned()) return null_type_idx;
        const start = self.template_parts.items.len;
        if (!fitsU16Range(start, parts.len)) {
            return self.failIndex(error.TypePoolCapacityExceeded);
        }
        self.template_parts.appendSlice(allocator, parts) catch return self.failIndex(error.OutOfMemory);
        return self.addNode(allocator, .{
            .tag = .t_template_literal,
            .data = .{ .a = @intCast(start), .b = @intCast(parts.len) },
        });
    }

    /// Get the parts of a template literal type.
    pub fn getTemplateParts(self: *const TypePool, idx: TypeIndex) []const TemplatePart {
        if (self.getTag(idx) != .t_template_literal) return &.{};
        const data = self.getData(idx) orelse return &.{};
        const start = data.a;
        const count = data.b;
        const end: usize = @as(usize, start) + @as(usize, count);
        if (end > self.template_parts.items.len) return &.{};
        return self.template_parts.items[start..end];
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
        if (self.isPoisoned()) return null_type_idx;
        const members = self.getUnionMembers(union_idx);
        if (members.len == 0) return union_idx;
        const remaining = allocator.alloc(TypeIndex, members.len) catch {
            return self.failIndex(error.OutOfMemory);
        };
        defer allocator.free(remaining);
        var count: usize = 0;
        for (members) |member| {
            if (member != exclude) {
                remaining[count] = member;
                count += 1;
            }
        }
        if (count == 0) return null_type_idx;
        if (count == 1) return remaining[0];
        return self.addUnion(allocator, remaining[0..count]);
    }

    /// Get function params and return type.
    /// Layout: data.a = (param_start & 0x0FFF) | (count << 12), data.b = return type index.
    pub fn getFunctionInfo(self: *const TypePool, idx: TypeIndex) struct { params: []const FuncParam, ret: TypeIndex } {
        const data = self.getData(idx) orelse return .{ .params = &.{}, .ret = null_type_idx };
        if (self.getTag(idx) != .t_function) return .{ .params = &.{}, .ret = null_type_idx };
        const param_start: u16 = data.a & 0x0FFF;
        const param_count: u16 = data.a >> 12;
        const ret = data.b;
        if (param_start + param_count > self.params.items.len) return .{ .params = &.{}, .ret = ret };
        return .{
            .params = self.params.items[param_start .. param_start + param_count],
            .ret = ret,
        };
    }

    /// Create a function type with params and return type.
    pub fn addFunctionWithReturn(self: *TypePool, allocator: std.mem.Allocator, func_params: []const FuncParam, ret: TypeIndex) TypeIndex {
        if (self.isPoisoned()) return null_type_idx;
        // param_start (12 bits) and count (4 bits) are packed into data.a. If the
        // shared params list has grown past 0x0FFF, a clamped param_start would
        // mis-slice into another function's params, and count > 15 cannot be
        // represented. Fail closed rather than creating a callable-looking
        // function with silently dropped parameters.
        const param_start = self.params.items.len;
        if (!fitsPackedFunction(param_start, func_params.len)) {
            return self.failIndex(error.TypePoolCapacityExceeded);
        }
        self.params.appendSlice(allocator, func_params) catch return self.failIndex(error.OutOfMemory);
        return self.addNode(allocator, .{
            .tag = .t_function,
            .data = .{
                .a = @as(u16, @intCast(param_start)) | (@as(u16, @intCast(func_params.len)) << 12),
                .b = ret,
            },
        });
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
        const start = data.b;
        if (start >= self.members.items.len) return .{ .base = data.a, .args = &.{} };
        const count: u16 = self.members.items[start];
        const args_start: usize = @as(usize, start) + 1;
        const args_end: usize = args_start + @as(usize, count);
        if (args_end > self.members.items.len) return .{ .base = data.a, .args = &.{} };
        return .{ .base = data.a, .args = self.members.items[args_start..args_end] };
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
                .t_intersection => self.isIntersectionAssignableToIntersection(source, target),
                .t_nullable => self.isAssignableTo(self.getNullableInner(source), self.getNullableInner(target)),
                .t_literal_string, .t_literal_number, .t_literal_bool => self.literalEquals(source, target),
                .t_ref => std.mem.eql(u8, self.getRefName(source), self.getRefName(target)),
                .t_tuple => blk: {
                    const src_elems = self.getTupleElements(source);
                    const tgt_elems = self.getTupleElements(target);
                    if (src_elems.len != tgt_elems.len) break :blk false;
                    for (src_elems, tgt_elems) |s, t| {
                        if (!self.isAssignableTo(s, t)) break :blk false;
                    }
                    break :blk true;
                },
                .t_generic_app => blk: {
                    // Two unresolved generic applications are assignable when they
                    // share a base ref name and their type arguments are pairwise
                    // assignable. Avoids spuriously rejecting `Foo<number>` vs an
                    // identically-structured `Foo<number>` from another node.
                    const src_info = self.getGenericAppInfo(source);
                    const tgt_info = self.getGenericAppInfo(target);
                    if (!std.mem.eql(u8, self.getRefName(src_info.base), self.getRefName(tgt_info.base))) break :blk false;
                    if (src_info.args.len != tgt_info.args.len) break :blk false;
                    for (src_info.args, tgt_info.args) |s, t| {
                        if (!self.isAssignableTo(s, t)) break :blk false;
                    }
                    break :blk true;
                },
                else => false,
            };
        }

        // Literal types are assignable to their base types
        if (src_tag == .t_literal_string and tgt_tag == .t_string) return true;
        if (src_tag == .t_literal_number and tgt_tag == .t_number) return true;
        if (src_tag == .t_literal_bool and tgt_tag == .t_boolean) return true;

        // Heterogeneous array literals infer as tuples. They are assignable
        // to T[] when every element is assignable to T.
        if (src_tag == .t_tuple and tgt_tag == .t_array) {
            const target_element = self.getArrayElement(target);
            for (self.getTupleElements(source)) |element| {
                if (!self.isAssignableTo(element, target_element)) return false;
            }
            return true;
        }

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

        // Intersection target: source must be assignable to every member
        if (tgt_tag == .t_intersection) {
            for (self.getIntersectionMembers(target)) |member| {
                if (!self.isAssignableTo(source, member)) return false;
            }
            return true;
        }

        // Intersection source: any single member may satisfy the target. For
        // structural records, the intersection's members combine into one
        // object shape, so fields may be satisfied across multiple members.
        if (src_tag == .t_intersection) {
            if (tgt_tag == .t_record) {
                return self.isIntersectionAssignableToRecord(source, target);
            }
            for (self.getIntersectionMembers(source)) |member| {
                if (self.isAssignableTo(member, target)) return true;
            }
            return false;
        }

        // Unresolved refs are effectively unknown: the TypePool cannot
        // resolve ref names to their definitions (that requires the TypeEnv),
        // and a generic type parameter (`T` in `first<T>(xs: T[]): T`) never
        // resolves there at all. A ref that survives to this point matched
        // neither the nominal nor the same-name rule above, so defer on
        // either side rather than rejecting valid code.
        if (src_tag == .t_ref or src_tag == .t_generic_param) return true;
        if (tgt_tag == .t_ref or tgt_tag == .t_generic_param) return true;

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
                    // An optional source field cannot satisfy a required target
                    // field: the source value may legally omit it, so `{ x?: T }`
                    // is not assignable to `{ x: T }`. Only the target's
                    // optionality was checked before, which silently accepted
                    // this unsound case (e.g. Partial<T> assigned to T).
                    if (src_f.optional and !tgt_f.optional) return false;
                    if (!self.isAssignableTo(src_f.type_idx, tgt_f.type_idx)) return false;
                    found = true;
                    break;
                }
            }
            if (!found and !tgt_f.optional) return false;
        }
        return true;
    }

    fn isIntersectionAssignableToRecord(self: *const TypePool, source: TypeIndex, target: TypeIndex) bool {
        const tgt_fields = self.getRecordFields(target);
        const src_members = self.getIntersectionMembers(source);

        for (tgt_fields) |tgt_f| {
            const tgt_name = self.getName(tgt_f.name_start, tgt_f.name_len);
            var found = false;
            for (src_members) |member| {
                if (self.findAssignableFieldInType(member, tgt_name, tgt_f.type_idx, tgt_f.optional)) {
                    found = true;
                    break;
                }
            }
            if (!found and !tgt_f.optional) return false;
        }
        return true;
    }

    fn findAssignableFieldInType(
        self: *const TypePool,
        source: TypeIndex,
        target_name: []const u8,
        target_type: TypeIndex,
        target_optional: bool,
    ) bool {
        const tag = self.getTag(source) orelse return false;
        if (tag == .t_intersection) {
            for (self.getIntersectionMembers(source)) |member| {
                if (self.findAssignableFieldInType(member, target_name, target_type, target_optional)) return true;
            }
            return false;
        }
        if (tag != .t_record) return false;
        for (self.getRecordFields(source)) |src_f| {
            const src_name = self.getName(src_f.name_start, src_f.name_len);
            if (std.mem.eql(u8, src_name, target_name)) {
                // An optional source field does not satisfy a required target field.
                if (src_f.optional and !target_optional) return false;
                return self.isAssignableTo(src_f.type_idx, target_type);
            }
        }
        return false;
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

    fn isIntersectionAssignableToIntersection(self: *const TypePool, source: TypeIndex, target: TypeIndex) bool {
        // Source A & B is assignable to target X & Y iff for every target member X,
        // at least one source member is assignable to X. This treats the source
        // intersection as a structural witness that satisfies all target obligations.
        for (self.getIntersectionMembers(target)) |tgt_member| {
            var satisfied = false;
            for (self.getIntersectionMembers(source)) |src_member| {
                if (self.isAssignableTo(src_member, tgt_member)) {
                    satisfied = true;
                    break;
                }
            }
            if (!satisfied) return false;
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
            .t_intersection => {
                for (self.getIntersectionMembers(idx), 0..) |member, i| {
                    if (i > 0) try writer.writeAll(" & ");
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
                const info = self.getGenericAppInfo(idx);
                try self.writeType(info.base, writer);
                try writer.writeByte('<');
                for (info.args, 0..) |arg, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try self.writeType(arg, writer);
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
    if (pool.isPoisoned()) return null_type_idx;
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
        self.skipWs();
        // Tolerate a leading `|` for the multi-line union layout:
        //     type T =
        //         | "a"
        //         | "b";
        _ = self.match('|');
        self.skipWs();

        const first = self.parseIntersection();
        if (first == null_type_idx) return null_type_idx;

        self.skipWs();
        if (!self.match('|')) return first;

        var members: std.ArrayListUnmanaged(TypeIndex) = .empty;
        defer members.deinit(self.allocator);
        members.append(self.allocator, first) catch return self.pool.failIndex(error.OutOfMemory);

        while (true) {
            self.skipWs();
            const member = self.parseIntersection();
            if (member == null_type_idx) break;
            members.append(self.allocator, member) catch return self.pool.failIndex(error.OutOfMemory);
            self.skipWs();
            if (!self.match('|')) break;
        }

        // Check for optional shorthand: T | undefined
        if (members.items.len == 2) {
            const tag0 = self.pool.getTag(members.items[0]);
            const tag1 = self.pool.getTag(members.items[1]);
            if (tag1 == .t_undefined) {
                return self.pool.addNullable(self.allocator, members.items[0]);
            }
            if (tag0 == .t_undefined) {
                return self.pool.addNullable(self.allocator, members.items[1]);
            }
        }

        return self.pool.addUnion(self.allocator, members.items);
    }

    /// Parse a TypeScript intersection: `A & B & C`. Binds tighter than `|`,
    /// looser than primary types, so `A | B & C` parses as `A | (B & C)`.
    /// A leading `|` or `&` is permitted before the first member to support
    /// the multi-line union/intersection style:
    ///     type T =
    ///         | "a"
    ///         | "b";
    fn parseIntersection(self: *TypeExprParser) TypeIndex {
        self.skipWs();
        // Tolerate a leading `&` for multi-line intersection layouts.
        _ = self.match('&');
        self.skipWs();

        const first = self.parsePrimary();
        if (first == null_type_idx) return null_type_idx;

        self.skipWs();
        if (!self.match('&')) return first;

        var members: std.ArrayListUnmanaged(TypeIndex) = .empty;
        defer members.deinit(self.allocator);
        members.append(self.allocator, first) catch return self.pool.failIndex(error.OutOfMemory);

        while (true) {
            self.skipWs();
            const member = self.parsePrimary();
            if (member == null_type_idx) break;
            members.append(self.allocator, member) catch return self.pool.failIndex(error.OutOfMemory);
            self.skipWs();
            if (!self.match('&')) break;
        }

        return self.pool.addIntersection(self.allocator, members.items);
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

        var fields: std.ArrayListUnmanaged(RecordField) = .empty;
        defer fields.deinit(self.allocator);

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

            const n = self.pool.addName(self.allocator, name_str);
            fields.append(self.allocator, .{
                .name_start = n.start,
                .name_len = n.len,
                .type_idx = field_type,
                .optional = optional,
                .readonly = is_readonly,
            }) catch return self.pool.failIndex(error.OutOfMemory);

            self.skipWs();
            // Skip separator (; or ,)
            if (self.pos < self.source.len and (self.source[self.pos] == ';' or self.source[self.pos] == ',')) {
                self.pos += 1;
            }
        }
        _ = self.match('}');
        return self.pool.addRecord(self.allocator, fields.items);
    }

    fn parseTuple(self: *TypeExprParser) TypeIndex {
        if (!self.match('[')) return null_type_idx;
        self.skipWs();

        var elements: std.ArrayListUnmanaged(TypeIndex) = .empty;
        defer elements.deinit(self.allocator);

        while (self.pos < self.source.len and self.source[self.pos] != ']') {
            self.skipWs();
            const elem = self.parseUnion();
            if (elem == null_type_idx) break;
            elements.append(self.allocator, elem) catch return self.pool.failIndex(error.OutOfMemory);
            self.skipWs();
            _ = self.match(',');
        }
        _ = self.match(']');
        return self.pool.addTuple(self.allocator, elements.items);
    }

    fn parseFunctionOrParen(self: *TypeExprParser) TypeIndex {
        // Save position to backtrack if this is just a paren group
        const saved = self.pos;
        if (!self.match('(')) return null_type_idx;
        self.skipWs();

        // Try to parse as function params
        var params: std.ArrayListUnmanaged(FuncParam) = .empty;
        defer params.deinit(self.allocator);
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
            const n = self.pool.addName(self.allocator, param_name);
            params.append(self.allocator, .{
                .name_start = n.start,
                .name_len = n.len,
                .type_idx = param_type,
                .optional = optional,
            }) catch return self.pool.failIndex(error.OutOfMemory);
            self.skipWs();
            _ = self.match(',');
        }
        _ = self.match(')');
        self.skipWs();

        if (!is_function and params.items.len == 0) {
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
            return self.pool.addFunctionWithReturn(self.allocator, params.items, ret);
        }

        // No arrow - this was function params without return type
        if (is_function) {
            return self.pool.addFunction(self.allocator, params.items, null_type_idx);
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
        // Literal-number types store only 16 bits. A literal outside i16 range
        // would clamp to literal 0, conflating distinct out-of-range literals
        // (e.g. `100000` and `200000` both compare equal). Fall back to the base
        // `number` type instead: it loses literal narrowing for large literals
        // but never reports two different literals as the same type.
        const value = std.fmt.parseInt(i16, num_str, 10) catch return self.pool.idx_number;
        return self.pool.addLiteralNumber(self.allocator, value);
    }

    fn parseTemplateLiteralType(self: *TypeExprParser) TypeIndex {
        if (!self.match('`')) return null_type_idx;

        var parts: std.ArrayListUnmanaged(TemplatePart) = .empty;
        defer parts.deinit(self.allocator);

        while (self.pos < self.source.len and self.source[self.pos] != '`') {
            if (self.source[self.pos] == '$' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '{') {
                // Type slot: ${T}
                self.pos += 2; // skip ${
                self.skipWs();
                const slot_type = self.parseUnion();
                self.skipWs();
                _ = self.match('}');
                parts.append(self.allocator, .{ .kind = .type_slot, .type_idx = slot_type }) catch return self.pool.failIndex(error.OutOfMemory);
            } else {
                // Literal segment: read until ` or ${
                const seg_start = self.pos;
                while (self.pos < self.source.len and self.source[self.pos] != '`') {
                    if (self.source[self.pos] == '$' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '{') break;
                    self.pos += 1;
                }
                if (self.pos > seg_start) {
                    const n = self.pool.addName(self.allocator, self.source[seg_start..self.pos]);
                    parts.append(self.allocator, .{ .kind = .literal, .name_start = n.start, .name_len = n.len }) catch return self.pool.failIndex(error.OutOfMemory);
                }
            }
        }
        _ = self.match('`');
        if (parts.items.len == 0) return null_type_idx;
        return self.pool.addTemplateLiteral(self.allocator, parts.items);
    }

    /// Consume any trailing `[]` array suffixes (`number[]`, `T[][]`) and wrap
    /// `base` in an array type for each one.
    fn maybeArrayWrap(self: *TypeExprParser, base: TypeIndex) TypeIndex {
        var result = base;
        while (true) {
            self.skipWs();
            if (self.pos + 1 < self.source.len and self.source[self.pos] == '[' and self.source[self.pos + 1] == ']') {
                self.pos += 2;
                result = self.pool.addArray(self.allocator, result);
            } else {
                break;
            }
        }
        return result;
    }

    fn resolveIdentType(self: *TypeExprParser, ident: []const u8) TypeIndex {
        // Check primitives. Each resolves to a primitive index; the trailing
        // `[]` suffix (if any) is applied uniformly via maybeArrayWrap so that
        // `number[]`, `string[]`, etc. produce an array type rather than dropping
        // the suffix.
        if (std.mem.eql(u8, ident, "boolean") or std.mem.eql(u8, ident, "bool")) return self.maybeArrayWrap(self.pool.idx_boolean);
        if (std.mem.eql(u8, ident, "number")) return self.maybeArrayWrap(self.pool.idx_number);
        if (std.mem.eql(u8, ident, "string")) return self.maybeArrayWrap(self.pool.idx_string);
        // 'null' type not supported - treat as unknown (parser rejects null literals)
        if (std.mem.eql(u8, ident, "null")) return self.maybeArrayWrap(self.pool.idx_unknown);
        if (std.mem.eql(u8, ident, "undefined")) return self.maybeArrayWrap(self.pool.idx_undefined);
        if (std.mem.eql(u8, ident, "void")) return self.maybeArrayWrap(self.pool.idx_void);
        if (std.mem.eql(u8, ident, "never")) return self.maybeArrayWrap(self.pool.idx_never);
        if (std.mem.eql(u8, ident, "unknown")) return self.maybeArrayWrap(self.pool.idx_unknown);
        if (std.mem.eql(u8, ident, "true")) return self.maybeArrayWrap(self.pool.addLiteralBool(self.allocator, true));
        if (std.mem.eql(u8, ident, "false")) return self.maybeArrayWrap(self.pool.addLiteralBool(self.allocator, false));

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

        var args: std.ArrayListUnmanaged(TypeIndex) = .empty;
        defer args.deinit(self.allocator);

        while (self.pos < self.source.len and self.source[self.pos] != '>') {
            self.skipWs();
            const arg = self.parseUnion();
            if (arg == null_type_idx) break;
            args.append(self.allocator, arg) catch return self.pool.failIndex(error.OutOfMemory);
            self.skipWs();
            _ = self.match(',');
        }
        _ = self.match('>');

        // Array<T> -> T[]
        if (is_array and args.items.len == 1) {
            return self.pool.addArray(self.allocator, args.items[0]);
        }

        // Readonly<{...}> on an INLINE record -> mark all fields readonly here
        // (no alias resolution needed). Readonly<NamedAlias> must instead build a
        // generic-app so TypeEnv.tryInstantiateGenericApp resolves the alias to
        // its record before applying makeReadonly -- applying makeReadonly to a
        // bare t_ref here is a no-op that silently drops the readonly markers.
        if (std.mem.eql(u8, base_name, "Readonly") and args.items.len == 1 and
            self.pool.getTag(args.items[0]) == .t_record)
        {
            return self.pool.makeReadonly(self.allocator, args.items[0]);
        }

        const base = self.pool.addRef(self.allocator, base_name);
        return self.pool.addGenericApp(self.allocator, base, args.items);
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

test "TypePool records primitive initialization allocation failure" {
    const allocator = std.testing.allocator;
    var failing = std.testing.FailingAllocator.init(allocator, .{ .fail_index = 0 });
    var pool = TypePool.init(failing.allocator());
    defer pool.deinit(failing.allocator());

    try std.testing.expectError(error.OutOfMemory, pool.ensureHealthy());
}

test "TypePool records constructor allocation failure separately from missing type" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var failing = std.testing.FailingAllocator.init(allocator, .{ .fail_index = 0 });
    const idx = pool.addRecord(failing.allocator(), &.{.{
        .name_start = 0,
        .name_len = 0,
        .type_idx = pool.idx_string,
        .optional = false,
    }});

    try std.testing.expectEqual(null_type_idx, idx);
    try std.testing.expectError(error.OutOfMemory, pool.ensureHealthy());
}

test "TypePool records type parser scratch allocation failure" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var failing = std.testing.FailingAllocator.init(allocator, .{ .fail_index = 0 });
    const idx = parseTypeExpr(&pool, failing.allocator(), "string | number");

    try std.testing.expectEqual(null_type_idx, idx);
    try std.testing.expectError(error.OutOfMemory, pool.ensureHealthy());
}

test "TypePool records name storage allocation failure" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var failing = std.testing.FailingAllocator.init(allocator, .{ .fail_index = 0 });
    const name = pool.addName(failing.allocator(), "field");

    try std.testing.expectEqual(@as(u8, 0), name.len);
    try std.testing.expectError(error.OutOfMemory, pool.ensureHealthy());
}

test "TypePool semantic absence does not poison the pool" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    try std.testing.expectEqual(null_type_idx, parseTypeExpr(&pool, allocator, ""));
    try pool.ensureHealthy();
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

test "addUnion preserves a sole union member" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const inner = pool.addUnion(allocator, &.{ pool.idx_string, pool.idx_number });
    const outer = pool.addUnion(allocator, &.{inner});

    try std.testing.expectEqual(inner, outer);
}

test "addUnion flattens nested unions" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const string_or_number = pool.addUnion(allocator, &.{ pool.idx_string, pool.idx_number });
    const nested = pool.addUnion(allocator, &.{ string_or_number, pool.idx_boolean });
    const direct = pool.addUnion(allocator, &.{ pool.idx_string, pool.idx_number, pool.idx_boolean });

    try std.testing.expectEqualSlices(TypeIndex, pool.getUnionMembers(direct), pool.getUnionMembers(nested));
    try std.testing.expect(pool.isAssignableTo(nested, direct));
    try std.testing.expect(pool.isAssignableTo(direct, nested));
}

test "addUnion collapses duplicate members" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = pool.addUnion(allocator, &.{ pool.idx_string, pool.idx_string });
    try std.testing.expectEqual(pool.idx_string, idx);
}

test "addUnion overflow fallback keeps every flattened member" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var distinct: [17]TypeIndex = undefined;
    for (&distinct, 0..) |*member, i| {
        member.* = pool.addLiteralNumber(allocator, @intCast(i));
    }

    const first_sixteen = pool.addUnion(allocator, distinct[0..16]);
    const wide = pool.addUnion(allocator, &.{ first_sixteen, distinct[16] });
    try std.testing.expectEqual(TypeTag.t_union, pool.getTag(wide).?);
    try std.testing.expectEqualSlices(TypeIndex, distinct[0..], pool.getUnionMembers(wide));
}

test "addUnion applies capacity guard after flattening" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var distinct: [17]TypeIndex = undefined;
    for (&distinct, 0..) |*member, i| {
        member.* = pool.addLiteralNumber(allocator, @intCast(i));
    }

    const max_member_count = std.math.maxInt(u16);
    const wide_members = try allocator.alloc(TypeIndex, max_member_count);
    defer allocator.free(wide_members);
    for (wide_members, 0..) |*member, i| {
        member.* = distinct[i % distinct.len];
    }

    const wide = pool.addUnion(allocator, wide_members);
    try std.testing.expectEqual(@as(usize, max_member_count), pool.getUnionMembers(wide).len);

    const overflow = pool.addUnion(allocator, &.{ wide, pool.idx_boolean });
    try std.testing.expectEqual(null_type_idx, overflow);
    try std.testing.expectError(error.TypePoolCapacityExceeded, pool.ensureHealthy());
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

test "isAssignableTo tuple literal to array target" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const text = pool.addName(allocator, "text");
    const done = pool.addName(allocator, "done");
    const todo = pool.addRecord(allocator, &.{
        .{ .name_start = text.start, .name_len = text.len, .type_idx = pool.idx_string, .optional = false },
        .{ .name_start = done.start, .name_len = done.len, .type_idx = pool.idx_boolean, .optional = false },
    });

    const text_a = pool.addName(allocator, "text");
    const done_a = pool.addName(allocator, "done");
    const item_a = pool.addRecord(allocator, &.{
        .{ .name_start = text_a.start, .name_len = text_a.len, .type_idx = pool.addLiteralString(allocator, "Learn Zig"), .optional = false },
        .{ .name_start = done_a.start, .name_len = done_a.len, .type_idx = pool.addLiteralBool(allocator, true), .optional = false },
    });

    const text_b = pool.addName(allocator, "text");
    const done_b = pool.addName(allocator, "done");
    const item_b = pool.addRecord(allocator, &.{
        .{ .name_start = text_b.start, .name_len = text_b.len, .type_idx = pool.addLiteralString(allocator, "Deploy"), .optional = false },
        .{ .name_start = done_b.start, .name_len = done_b.len, .type_idx = pool.addLiteralBool(allocator, false), .optional = false },
    });

    const tuple = pool.addTuple(allocator, &.{ item_a, item_b });
    const array = pool.addArray(allocator, todo);
    try std.testing.expect(pool.isAssignableTo(tuple, array));
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

test "makePartial marks all fields optional" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = parseTypeExpr(&pool, allocator, "{ id: number; name: string }");
    const partial = pool.makePartial(allocator, idx);
    const fields = pool.getRecordFields(partial);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    try std.testing.expect(fields[0].optional);
    try std.testing.expect(fields[1].optional);
}

test "makeRequired clears optional on all fields" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = parseTypeExpr(&pool, allocator, "{ id?: number; name?: string }");
    const required = pool.makeRequired(allocator, idx);
    const fields = pool.getRecordFields(required);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    try std.testing.expect(!fields[0].optional);
    try std.testing.expect(!fields[1].optional);
}

test "pickFields keeps only the named fields" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const rec = parseTypeExpr(&pool, allocator, "{ id: number; name: string; age: number }");
    const keys = parseTypeExpr(&pool, allocator, "\"id\" | \"name\"");
    const picked = pool.pickFields(allocator, rec, keys);
    const fields = pool.getRecordFields(picked);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    try std.testing.expect(pool.lookupRecordField(picked, "id") != null);
    try std.testing.expect(pool.lookupRecordField(picked, "name") != null);
    try std.testing.expect(pool.lookupRecordField(picked, "age") == null);
}

test "pickFields with a single string-literal key" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const rec = parseTypeExpr(&pool, allocator, "{ id: number; name: string }");
    const keys = parseTypeExpr(&pool, allocator, "\"id\"");
    const picked = pool.pickFields(allocator, rec, keys);
    const fields = pool.getRecordFields(picked);
    try std.testing.expectEqual(@as(usize, 1), fields.len);
    try std.testing.expect(pool.lookupRecordField(picked, "id") != null);
    try std.testing.expect(pool.lookupRecordField(picked, "name") == null);
}

test "omitFields drops the named fields" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const rec = parseTypeExpr(&pool, allocator, "{ id: number; name: string; age: number }");
    const keys = parseTypeExpr(&pool, allocator, "\"age\"");
    const omitted = pool.omitFields(allocator, rec, keys);
    const fields = pool.getRecordFields(omitted);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    try std.testing.expect(pool.lookupRecordField(omitted, "id") != null);
    try std.testing.expect(pool.lookupRecordField(omitted, "name") != null);
    try std.testing.expect(pool.lookupRecordField(omitted, "age") == null);
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

test "addIntersection flattens single member" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = pool.addIntersection(allocator, &.{pool.idx_string});
    try std.testing.expectEqual(pool.idx_string, idx);
}

test "addIntersection drops duplicate members" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    // string & string & number canonicalises to string & number
    const idx = pool.addIntersection(allocator, &.{ pool.idx_string, pool.idx_string, pool.idx_number });
    try std.testing.expectEqual(TypeTag.t_intersection, pool.getTag(idx).?);
    const members = pool.getIntersectionMembers(idx);
    try std.testing.expectEqual(@as(usize, 2), members.len);
    try std.testing.expectEqual(pool.idx_string, members[0]);
    try std.testing.expectEqual(pool.idx_number, members[1]);
}

test "parseTypeExpr intersection two members" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = parseTypeExpr(&pool, allocator, "string & number");
    try std.testing.expectEqual(TypeTag.t_intersection, pool.getTag(idx).?);
    const members = pool.getIntersectionMembers(idx);
    try std.testing.expectEqual(@as(usize, 2), members.len);
    try std.testing.expectEqual(pool.idx_string, members[0]);
    try std.testing.expectEqual(pool.idx_number, members[1]);
}

test "parseTypeExpr intersection three members" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const idx = parseTypeExpr(&pool, allocator, "string & number & boolean");
    try std.testing.expectEqual(TypeTag.t_intersection, pool.getTag(idx).?);
    try std.testing.expectEqual(@as(usize, 3), pool.getIntersectionMembers(idx).len);
}

test "parseTypeExpr intersection binds tighter than union" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    // `string | number & boolean` parses as `string | (number & boolean)`
    const idx = parseTypeExpr(&pool, allocator, "string | number & boolean");
    try std.testing.expectEqual(TypeTag.t_union, pool.getTag(idx).?);
    const members = pool.getUnionMembers(idx);
    try std.testing.expectEqual(@as(usize, 2), members.len);
    try std.testing.expectEqual(pool.idx_string, members[0]);
    try std.testing.expectEqual(TypeTag.t_intersection, pool.getTag(members[1]).?);
}

test "parseTypeExpr leading-pipe union" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    // Multi-line union style: leading `|` is permitted before the first member.
    const idx = parseTypeExpr(&pool, allocator, "| \"a\" | \"b\" | \"c\"");
    try std.testing.expectEqual(TypeTag.t_union, pool.getTag(idx).?);
    try std.testing.expectEqual(@as(usize, 3), pool.getUnionMembers(idx).len);
}

test "parseTypeExpr leading-amp intersection" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    // Multi-line intersection style: leading `&` is permitted before the first member.
    const idx = parseTypeExpr(&pool, allocator, "& string & number");
    try std.testing.expectEqual(TypeTag.t_intersection, pool.getTag(idx).?);
    try std.testing.expectEqual(@as(usize, 2), pool.getIntersectionMembers(idx).len);
}

test "instantiate substitutes generic params inside intersection" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const t_param = pool.addGenericParam(allocator, "T");
    const tagged_brand = pool.addRecord(allocator, &.{
        .{
            .name_start = pool.addName(allocator, "__tag").start,
            .name_len = pool.addName(allocator, "__tag").len,
            .type_idx = pool.addLiteralString(allocator, "x"),
            .optional = false,
        },
    });
    // body: T & { __tag: "x" }
    const body = pool.addIntersection(allocator, &.{ t_param, tagged_brand });
    const param_names: [1][]const u8 = .{"T"};
    const param_types: [1]TypeIndex = .{pool.idx_string};

    const instantiated = pool.instantiate(allocator, body, &param_names, &param_types, 0);
    try std.testing.expectEqual(TypeTag.t_intersection, pool.getTag(instantiated).?);
    const members = pool.getIntersectionMembers(instantiated);
    try std.testing.expectEqual(@as(usize, 2), members.len);
    try std.testing.expectEqual(pool.idx_string, members[0]);
    try std.testing.expectEqual(TypeTag.t_record, pool.getTag(members[1]).?);
}

test "isAssignableTo target intersection requires every member" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    // target: { a: number } & { b: string }
    const a_name = pool.addName(allocator, "a");
    const left = pool.addRecord(allocator, &.{
        .{ .name_start = a_name.start, .name_len = a_name.len, .type_idx = pool.idx_number, .optional = false },
    });
    const b_name = pool.addName(allocator, "b");
    const right = pool.addRecord(allocator, &.{
        .{ .name_start = b_name.start, .name_len = b_name.len, .type_idx = pool.idx_string, .optional = false },
    });
    const target = pool.addIntersection(allocator, &.{ left, right });

    // source has both fields - assignable
    const both = pool.addRecord(allocator, &.{
        .{ .name_start = a_name.start, .name_len = a_name.len, .type_idx = pool.idx_number, .optional = false },
        .{ .name_start = b_name.start, .name_len = b_name.len, .type_idx = pool.idx_string, .optional = false },
    });
    try std.testing.expect(pool.isAssignableTo(both, target));

    // source has only one - not assignable
    try std.testing.expect(!pool.isAssignableTo(left, target));
}

test "isAssignableTo source intersection any member assignable" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    // source: string & number; target: string. At least one member assignable.
    const source = pool.addIntersection(allocator, &.{ pool.idx_string, pool.idx_number });
    try std.testing.expect(pool.isAssignableTo(source, pool.idx_string));
    try std.testing.expect(pool.isAssignableTo(source, pool.idx_number));
    // Target not in source - not assignable.
    try std.testing.expect(!pool.isAssignableTo(source, pool.idx_boolean));
}

test "isAssignableTo source intersection combines record fields" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const a_name = pool.addName(allocator, "a");
    const left = pool.addRecord(allocator, &.{
        .{ .name_start = a_name.start, .name_len = a_name.len, .type_idx = pool.idx_number, .optional = false },
    });
    const b_name = pool.addName(allocator, "b");
    const right = pool.addRecord(allocator, &.{
        .{ .name_start = b_name.start, .name_len = b_name.len, .type_idx = pool.idx_string, .optional = false },
    });
    const source = pool.addIntersection(allocator, &.{ left, right });
    const target = pool.addRecord(allocator, &.{
        .{ .name_start = a_name.start, .name_len = a_name.len, .type_idx = pool.idx_number, .optional = false },
        .{ .name_start = b_name.start, .name_len = b_name.len, .type_idx = pool.idx_string, .optional = false },
    });

    try std.testing.expect(pool.isAssignableTo(source, target));

    const wrong_target = pool.addRecord(allocator, &.{
        .{ .name_start = a_name.start, .name_len = a_name.len, .type_idx = pool.idx_string, .optional = false },
        .{ .name_start = b_name.start, .name_len = b_name.len, .type_idx = pool.idx_string, .optional = false },
    });
    try std.testing.expect(!pool.isAssignableTo(source, wrong_target));
}

test "isAssignableTo defers on unresolved generic param targets" {
    // Call-site checking of a generic helper (`first<T>(xs: T[]): T`)
    // reaches isAssignableTo with `T` still unresolved; the pool must defer
    // rather than reject every generic call.
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const t_param = pool.addGenericParam(allocator, "T");
    const t_array_of_param = pool.addArray(allocator, t_param);
    const strings = pool.addArray(allocator, pool.idx_string);

    // string[] -> T[]
    try std.testing.expect(pool.isAssignableTo(strings, t_array_of_param));
    // tuple of string literals -> T[]
    const lit = parseTypeExpr(&pool, allocator, "\"alice\"");
    const tuple = pool.addTuple(allocator, &.{ lit, lit });
    try std.testing.expect(pool.isAssignableTo(tuple, t_array_of_param));
    // T -> string (return-position: `const head: string = first<string>(xs)`)
    try std.testing.expect(pool.isAssignableTo(t_param, pool.idx_string));
    // Unresolved named ref -> number
    const ref = parseTypeExpr(&pool, allocator, "SomeAlias");
    try std.testing.expect(pool.isAssignableTo(ref, pool.idx_number));
    // Concrete mismatches still reject
    try std.testing.expect(!pool.isAssignableTo(pool.idx_number, pool.idx_string));
}

test "addRecord fails closed when field start cannot fit" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    try pool.fields.resize(allocator, @as(usize, std.math.maxInt(u16)) + 1);

    const idx = pool.addRecord(allocator, &.{.{
        .name_start = 0,
        .name_len = 0,
        .type_idx = pool.idx_string,
        .optional = false,
    }});
    try std.testing.expectEqual(null_type_idx, idx);
    try std.testing.expectError(error.TypePoolCapacityExceeded, pool.ensureHealthy());
}

test "addFunctionWithReturn fails closed when params cannot be packed" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var params: [16]FuncParam = undefined;
    for (&params, 0..) |*param, i| {
        var name_buf: [8]u8 = undefined;
        const name = try std.fmt.bufPrint(&name_buf, "p{d}", .{i});
        const n = pool.addName(allocator, name);
        param.* = .{
            .name_start = n.start,
            .name_len = n.len,
            .type_idx = pool.idx_string,
            .optional = false,
        };
    }

    const idx = pool.addFunctionWithReturn(allocator, params[0..], pool.idx_void);
    try std.testing.expectEqual(null_type_idx, idx);
    try std.testing.expectError(error.TypePoolCapacityExceeded, pool.ensureHealthy());
}

test "instantiate preserves records wider than old stack buffers" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const t_param = pool.addGenericParam(allocator, "T");
    var fields: [33]RecordField = undefined;
    for (&fields, 0..) |*field, i| {
        var name_buf: [8]u8 = undefined;
        const name = try std.fmt.bufPrint(&name_buf, "f{d}", .{i});
        const n = pool.addName(allocator, name);
        field.* = .{
            .name_start = n.start,
            .name_len = n.len,
            .type_idx = t_param,
            .optional = false,
        };
    }

    const record = pool.addRecord(allocator, fields[0..]);
    const instantiated = pool.instantiate(allocator, record, &.{"T"}, &.{pool.idx_string}, 0);
    const out = pool.getRecordFields(instantiated);
    try std.testing.expectEqual(@as(usize, fields.len), out.len);
    for (out) |field| {
        try std.testing.expectEqual(pool.idx_string, field.type_idx);
    }
}

test "instantiate preserves generic app args wider than old stack buffers" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    const t_param = pool.addGenericParam(allocator, "T");
    const base = pool.addRef(allocator, "Box");
    var args: [17]TypeIndex = [_]TypeIndex{t_param} ** 17;

    const app = pool.addGenericApp(allocator, base, args[0..]);
    const instantiated = pool.instantiate(allocator, app, &.{"T"}, &.{pool.idx_string}, 0);
    const info = pool.getGenericAppInfo(instantiated);
    try std.testing.expectEqual(@as(usize, args.len), info.args.len);
    for (info.args) |arg| {
        try std.testing.expectEqual(pool.idx_string, arg);
    }
}

test "parseTypeExpr keeps unions wider than thirty two members" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var source_buf: [1024]u8 = undefined;
    var writer: std.Io.Writer = .fixed(&source_buf);
    for (0..40) |i| {
        if (i > 0) try writer.writeAll(" | ");
        try writer.print("\"k{d}\"", .{i});
    }

    const idx = parseTypeExpr(&pool, allocator, writer.buffer[0..writer.end]);
    try std.testing.expectEqual(TypeTag.t_union, pool.getTag(idx).?);
    try std.testing.expectEqual(@as(usize, 40), pool.getUnionMembers(idx).len);
}

test "parseTypeExpr keeps record fields wider than thirty two fields" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var source_buf: [2048]u8 = undefined;
    var writer: std.Io.Writer = .fixed(&source_buf);
    try writer.writeByte('{');
    for (0..40) |i| {
        try writer.print(" f{d}: string;", .{i});
    }
    try writer.writeAll(" }");

    const idx = parseTypeExpr(&pool, allocator, writer.buffer[0..writer.end]);
    try std.testing.expectEqual(TypeTag.t_record, pool.getTag(idx).?);
    try std.testing.expectEqual(@as(usize, 40), pool.getRecordFields(idx).len);
}

test "parseTypeExpr keeps generic app args wider than eight args" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var source_buf: [512]u8 = undefined;
    var writer: std.Io.Writer = .fixed(&source_buf);
    try writer.writeAll("Box<");
    for (0..12) |i| {
        if (i > 0) try writer.writeAll(", ");
        try writer.writeAll("string");
    }
    try writer.writeByte('>');

    const idx = parseTypeExpr(&pool, allocator, writer.buffer[0..writer.end]);
    try std.testing.expectEqual(TypeTag.t_generic_app, pool.getTag(idx).?);
    try std.testing.expectEqual(@as(usize, 12), pool.getGenericAppInfo(idx).args.len);
}

test "parseTypeExpr keeps template literal parts wider than sixteen parts" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var source_buf: [1024]u8 = undefined;
    var writer: std.Io.Writer = .fixed(&source_buf);
    try writer.writeByte('`');
    for (0..18) |i| {
        try writer.print("p{d}${{string}}", .{i});
    }
    try writer.writeByte('`');

    const idx = parseTypeExpr(&pool, allocator, writer.buffer[0..writer.end]);
    try std.testing.expectEqual(TypeTag.t_template_literal, pool.getTag(idx).?);
    try std.testing.expectEqual(@as(usize, 36), pool.getTemplateParts(idx).len);
}
