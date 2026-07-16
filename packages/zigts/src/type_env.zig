//! Type Environment: resolves type names to TypePool indices.
//!
//! Populated from TypeMap entries extracted by the stripper. Provides:
//! - Type alias resolution (type Foo = ...)
//! - Interface resolution (interface Bar { ... })
//! - Variable type annotations (const x: Type = ...)
//! - Function signatures (params + return types)
//! - Generic scope tracking
//!
//! The TypeEnv is the bridge between raw type text (from the stripper's TypeMap)
//! and the structured types in the TypePool.

const std = @import("std");
const type_pool_mod = @import("type_pool.zig");
const type_map_mod = @import("type_map.zig");

const TypePool = type_pool_mod.TypePool;
const TypeIndex = type_pool_mod.TypeIndex;
const null_type_idx = type_pool_mod.null_type_idx;
const parseTypeExpr = type_pool_mod.parseTypeExpr;
const TypeMap = type_map_mod.TypeMap;
const TypeMapKind = type_map_mod.TypeMapKind;
const TypeMapEntry = type_map_mod.TypeMapEntry;

/// The field name used to mark a record as the body of an instantiated
/// `Spec<...>` generic alias. The verifier looks for records bearing this
/// field when extracting declared spec sets from a handler return type.
/// Sized to be unmistakable for any user-authored field.
pub const spec_marker_field = "__zigttp_spec__";

/// Built-in object-deriving utility types. Recognized by name in
/// tryInstantiateGenericApp; the transforms themselves live in TypePool.
const UtilityKind = enum { pick, omit, partial, required, readonly };

fn utilityKind(name: []const u8) ?UtilityKind {
    if (std.mem.eql(u8, name, "Pick")) return .pick;
    if (std.mem.eql(u8, name, "Omit")) return .omit;
    if (std.mem.eql(u8, name, "Partial")) return .partial;
    if (std.mem.eql(u8, name, "Required")) return .required;
    if (std.mem.eql(u8, name, "Readonly")) return .readonly;
    return null;
}

/// The field name marking the body of an instantiated `Effects<...>` generic
/// alias. Distinct from `spec_marker_field` so a return type carrying both
/// (`Effects<Response, "env"> & Spec<"deterministic">`) keeps capability
/// names and proof-property names in separate extraction passes.
pub const effect_marker_field = "__zigttp_effect__";

// ---------------------------------------------------------------------------
// Generic scope
// ---------------------------------------------------------------------------

pub const GenericScope = struct {
    /// Type parameter names mapped to their TypeIndex in the pool.
    params: [8]struct { name: [32]u8, name_len: u8, idx: TypeIndex } = undefined,
    count: u8 = 0,

    pub fn addParam(self: *GenericScope, name: []const u8, idx: TypeIndex) void {
        if (self.count >= 8) return;
        const len: u8 = @intCast(@min(name.len, 32));
        @memcpy(self.params[self.count].name[0..len], name[0..len]);
        self.params[self.count].name_len = len;
        self.params[self.count].idx = idx;
        self.count += 1;
    }

    pub fn resolve(self: *const GenericScope, name: []const u8) ?TypeIndex {
        for (0..self.count) |i| {
            if (self.params[i].name_len == name.len and
                std.mem.eql(u8, self.params[i].name[0..self.params[i].name_len], name))
            {
                return self.params[i].idx;
            }
        }
        return null;
    }
};

// ---------------------------------------------------------------------------
// Function signature (params + return type)
// ---------------------------------------------------------------------------

pub const FunctionSig = struct {
    param_types: [16]TypeIndex = undefined,
    param_count: u8 = 0,
    required_param_count: ?u8 = null,
    return_type: TypeIndex = null_type_idx,
};

// ---------------------------------------------------------------------------
// Generic type alias (type Result<T> = ...)
// ---------------------------------------------------------------------------

pub const GenericAlias = struct {
    param_names: [8][]const u8 = undefined,
    param_count: u8 = 0,
    body: TypeIndex = null_type_idx,
};

// ---------------------------------------------------------------------------
// Type Environment
// ---------------------------------------------------------------------------

pub const TypeEnv = struct {
    const NamedVarAnnotation = struct {
        name: []const u8,
        ordinal: u32,
        type_idx: TypeIndex,
    };

    pool: *TypePool,
    allocator: std.mem.Allocator,

    /// Type namespace: type alias name -> resolved TypeIndex
    type_aliases: std.StringHashMapUnmanaged(TypeIndex),
    /// Generic type aliases: name -> uninstantiated body + param names
    generic_aliases: std.StringHashMapUnmanaged(GenericAlias),
    /// Interface namespace: interface name -> resolved TypeIndex
    interfaces: std.StringHashMapUnmanaged(TypeIndex),
    /// Variable types: packed(context_line, context_col) -> TypeIndex
    var_types: std.AutoHashMapUnmanaged(u32, TypeIndex),
    /// Function signatures: packed(context_line, context_col) -> FunctionSig
    fn_signatures: std.AutoHashMapUnmanaged(u32, FunctionSig),
    /// Variable name -> declared type (for name-based lookup)
    var_types_by_name: std.StringHashMapUnmanaged(TypeIndex),
    /// All variable annotations keyed by semantic (name, occurrence), in source order.
    var_annotations: std.ArrayListUnmanaged(NamedVarAnnotation),
    /// Scoped binding identity: packed(scope_id, name_atom) -> declared type.
    var_types_by_binding: std.AutoHashMapUnmanaged(u32, TypeIndex),
    /// Function name -> signature (for name-based lookup)
    fn_sigs_by_name: std.StringHashMapUnmanaged(FunctionSig),
    /// Generic scope stack
    generic_scopes: std.ArrayListUnmanaged(GenericScope),

    /// Stable storage for interned names used as hash-map keys.
    /// Keys must not move after insertion, so each name owns its own allocation.
    name_storage: std.ArrayListUnmanaged([]const u8),

    pub fn init(allocator: std.mem.Allocator, pool: *TypePool) TypeEnv {
        var env: TypeEnv = .{
            .pool = pool,
            .allocator = allocator,
            .type_aliases = .empty,
            .generic_aliases = .empty,
            .interfaces = .empty,
            .var_types = .empty,
            .fn_signatures = .empty,
            .var_types_by_name = .empty,
            .var_annotations = .empty,
            .var_types_by_binding = .empty,
            .fn_sigs_by_name = .empty,
            .generic_scopes = .empty,
            .name_storage = .empty,
        };
        env.registerBuiltins();
        return env;
    }

    /// Register built-in type aliases (`Spec<S>` and friends) so user code
    /// can write `import type { Spec } from "zigttp:types"` and have the
    /// type checker resolve it without a real source file. Idempotent;
    /// user-declared aliases of the same name will overwrite the built-in
    /// (last-write-wins matches the existing populateFromTypeMap pattern).
    pub fn registerBuiltins(self: *TypeEnv) void {
        self.registerSpecBuiltin();
        // Proof<T, S> and Effects<T, S> share the capsule shape
        // `T & { <marker>: S }`; they differ only in the marker field.
        self.registerCapsuleAlias("Proof", spec_marker_field);
        self.registerCapsuleAlias("Effects", effect_marker_field);
    }

    /// Spec<S>: phantom marker carrying the declared spec name set as S.
    /// Registered as a generic alias whose body is a record whose only
    /// field is named `__zigttp_spec__` with type S. After instantiation
    /// with a literal-string union, the field type carries the spec
    /// names. The verifier walks return-type intersections to extract
    /// them via extractSpecMembers.
    fn registerSpecBuiltin(self: *TypeEnv) void {
        self.pushGenericScope();
        const s_param = self.addGenericParam("S");
        const field_name = self.pool.addName(self.allocator, spec_marker_field);
        const body = self.pool.addRecord(self.allocator, &.{
            .{
                .name_start = field_name.start,
                .name_len = field_name.len,
                .type_idx = s_param,
                .optional = false,
            },
        });
        self.popGenericScope();

        if (body == null_type_idx) return;

        const owned_name = self.internName("Spec");
        if (owned_name.len == 0) return;
        const owned_param = self.internName("S");

        var alias = GenericAlias{};
        alias.param_names[0] = owned_param;
        alias.param_count = 1;
        alias.body = body;
        self.generic_aliases.put(self.allocator, owned_name, alias) catch {};
    }

    /// Register a two-param capsule alias `Name<T, S>` whose body is
    /// `T & { <marker>: S }` - the shape shared by `Proof<T, S>` (marker
    /// `__zigttp_spec__`) and `Effects<T, S>` (marker `__zigttp_effect__`).
    /// After instantiation the underlying return type `T` survives for type
    /// checking while the phantom marker record carries `S`. Distinct marker
    /// fields let a return type carry both capsules and have each extraction
    /// (`extractSpecMembers` / `extractEffectMembers`) recover only its own.
    fn registerCapsuleAlias(self: *TypeEnv, alias_name: []const u8, marker_field: []const u8) void {
        self.pushGenericScope();
        const t_param = self.addGenericParam("T");
        const s_param = self.addGenericParam("S");
        const field_name = self.pool.addName(self.allocator, marker_field);
        const marker = self.pool.addRecord(self.allocator, &.{
            .{
                .name_start = field_name.start,
                .name_len = field_name.len,
                .type_idx = s_param,
                .optional = false,
            },
        });
        const body = self.pool.addIntersection(self.allocator, &.{ t_param, marker });
        self.popGenericScope();

        if (body == null_type_idx) return;

        const owned_name = self.internName(alias_name);
        if (owned_name.len == 0) return;
        const owned_t = self.internName("T");
        const owned_s = self.internName("S");

        var alias = GenericAlias{};
        alias.param_names[0] = owned_t;
        alias.param_names[1] = owned_s;
        alias.param_count = 2;
        alias.body = body;
        self.generic_aliases.put(self.allocator, owned_name, alias) catch {};
    }

    pub fn deinit(self: *TypeEnv) void {
        self.type_aliases.deinit(self.allocator);
        self.generic_aliases.deinit(self.allocator);
        self.interfaces.deinit(self.allocator);
        self.var_types.deinit(self.allocator);
        self.fn_signatures.deinit(self.allocator);
        self.var_types_by_name.deinit(self.allocator);
        self.var_annotations.deinit(self.allocator);
        self.var_types_by_binding.deinit(self.allocator);
        self.fn_sigs_by_name.deinit(self.allocator);
        self.generic_scopes.deinit(self.allocator);
        for (self.name_storage.items) |name| {
            self.allocator.free(name);
        }
        self.name_storage.deinit(self.allocator);
    }

    // -------------------------------------------------------------------
    // Population from TypeMap
    // -------------------------------------------------------------------

    /// Populate the environment from a TypeMap (extracted by the stripper).
    /// Processes entries in order: type aliases first, then interfaces,
    /// then variable/param/return annotations.
    pub fn populateFromTypeMap(self: *TypeEnv, tm: *const TypeMap) void {
        // First pass: type aliases and interfaces (defines the type namespace)
        // Collect generic_params entries keyed by (name_start, name_end) for alias lookup.
        var generic_params_map: std.AutoHashMapUnmanaged(u64, TypeMapEntry) = .empty;
        defer generic_params_map.deinit(self.allocator);
        for (tm.entries.items) |entry| {
            if (entry.kind == .generic_params and entry.name_start != 0) {
                const key = (@as(u64, entry.name_start) << 32) | entry.name_end;
                generic_params_map.put(self.allocator, key, entry) catch {};
            }
        }

        for (tm.entries.items) |entry| {
            switch (entry.kind) {
                .type_alias => self.processTypeAlias(tm, entry, &generic_params_map),
                .interface_decl => self.processInterface(tm, entry),
                .distinct_type => self.processDistinctType(tm, entry),
                else => {},
            }
        }

        // Second pass: variable and function annotations
        // Group param and return annotations by context line to build function sigs
        var fn_params_by_line: std.AutoHashMapUnmanaged(u32, FunctionSig) = .empty;
        defer fn_params_by_line.deinit(self.allocator);
        var fn_names_by_line: std.AutoHashMapUnmanaged(u32, []const u8) = .empty;
        defer fn_names_by_line.deinit(self.allocator);

        for (tm.entries.items) |entry| {
            switch (entry.kind) {
                .var_annotation => self.processVarAnnotation(tm, entry),
                .param_annotation => {
                    const gop = fn_params_by_line.getOrPut(self.allocator, entry.context_line) catch continue;
                    if (!gop.found_existing) {
                        gop.value_ptr.* = .{};
                    }
                    const type_text = tm.getTypeText(entry);
                    const type_idx = self.resolveType(type_text);
                    if (gop.value_ptr.param_count < 16) {
                        gop.value_ptr.param_types[gop.value_ptr.param_count] = type_idx;
                        gop.value_ptr.param_count += 1;
                    }
                },
                .return_annotation => {
                    const type_text = tm.getTypeText(entry);
                    const type_idx = self.resolveType(type_text);
                    const gop = fn_params_by_line.getOrPut(self.allocator, entry.context_line) catch continue;
                    if (!gop.found_existing) {
                        gop.value_ptr.* = .{};
                    }
                    gop.value_ptr.return_type = type_idx;
                    if (tm.getNameText(entry)) |name| {
                        const owned_name = self.internName(name);
                        fn_names_by_line.put(self.allocator, entry.context_line, owned_name) catch {};
                    }
                },
                else => {},
            }
        }

        // Merge function signatures
        var iter = fn_params_by_line.iterator();
        while (iter.next()) |kv| {
            self.fn_signatures.put(self.allocator, kv.key_ptr.*, kv.value_ptr.*) catch {};
            if (fn_names_by_line.get(kv.key_ptr.*)) |name| {
                self.fn_sigs_by_name.put(self.allocator, name, kv.value_ptr.*) catch {};
            }
        }
    }

    fn processTypeAlias(
        self: *TypeEnv,
        tm: *const TypeMap,
        entry: TypeMapEntry,
        generic_params_map: *const std.AutoHashMapUnmanaged(u64, TypeMapEntry),
    ) void {
        const name = tm.getNameText(entry) orelse return;
        const type_text = tm.getTypeText(entry);
        if (type_text.len == 0) return;

        // Check for matching generic_params entry (same name range).
        const gp_key = (@as(u64, entry.name_start) << 32) | entry.name_end;
        if (generic_params_map.get(gp_key)) |gp_entry| {
            const params_text = tm.getTypeText(gp_entry);
            if (params_text.len > 0) {
                // Parse comma-separated param names and resolve body in generic scope.
                var alias = GenericAlias{};
                self.pushGenericScope();

                var it = std.mem.splitScalar(u8, params_text, ',');
                while (it.next()) |raw_param| {
                    const param_name = std.mem.trim(u8, raw_param, " \t\n\r");
                    if (param_name.len == 0) continue;
                    if (alias.param_count >= 8) break;
                    _ = self.addGenericParam(param_name);
                    alias.param_names[alias.param_count] = self.internName(param_name);
                    alias.param_count += 1;
                }

                alias.body = self.resolveType(type_text);
                self.popGenericScope();

                const owned_name = self.internName(name);
                self.generic_aliases.put(self.allocator, owned_name, alias) catch {};
                return;
            }
        }

        // Non-generic alias: simple name -> type mapping.
        const type_idx = self.resolveType(type_text);
        const owned_name = self.internName(name);
        self.type_aliases.put(self.allocator, owned_name, type_idx) catch {};
    }

    fn processDistinctType(self: *TypeEnv, tm: *const TypeMap, entry: TypeMapEntry) void {
        const name = tm.getNameText(entry) orelse return;
        const type_text = tm.getTypeText(entry);
        if (type_text.len == 0) return;

        // Resolve the base type, then create a nominal alias.
        const base_idx = self.resolveType(type_text);
        if (base_idx == null_type_idx) return;
        const nominal_idx = self.pool.addNominalAlias(self.allocator, base_idx);
        if (nominal_idx == null_type_idx) return;

        const owned_name = self.internName(name);
        self.type_aliases.put(self.allocator, owned_name, nominal_idx) catch {};
    }

    fn processInterface(self: *TypeEnv, tm: *const TypeMap, entry: TypeMapEntry) void {
        const name = tm.getNameText(entry) orelse return;
        const type_text = tm.getTypeText(entry);
        if (type_text.len == 0) return;

        const type_idx = self.resolveType(type_text);

        // Check if all members are function-typed -> mark as nominal (capability interface)
        if (type_idx != null_type_idx and self.pool.getTag(type_idx) == .t_record) {
            const fields = self.pool.getRecordFields(type_idx);
            var all_functions = fields.len > 0;
            for (fields) |field| {
                const ftag = self.pool.getTag(field.type_idx);
                if (ftag != .t_function and ftag != null) {
                    all_functions = false;
                    break;
                }
            }
            if (all_functions and type_idx < self.pool.nodes.items.len) {
                self.pool.nodes.items[type_idx].nominal = true;
            }
        }

        const owned_name = self.internName(name);
        self.interfaces.put(self.allocator, owned_name, type_idx) catch {};
    }

    fn processVarAnnotation(self: *TypeEnv, tm: *const TypeMap, entry: TypeMapEntry) void {
        const type_text = tm.getTypeText(entry);
        if (type_text.len == 0) return;

        const type_idx = self.resolveType(type_text);
        const key = packLocationKey(entry.context_line, entry.context_col);
        self.var_types.put(self.allocator, key, type_idx) catch {};

        // Also store by name for name-based lookup
        if (tm.getNameText(entry)) |name| {
            const owned_name = self.internName(name);
            self.var_types_by_name.put(self.allocator, owned_name, type_idx) catch {};
            if (entry.name_ordinal) |ordinal| {
                self.var_annotations.append(self.allocator, .{
                    .name = owned_name,
                    .ordinal = ordinal,
                    .type_idx = type_idx,
                }) catch {};
            }
        }
    }

    // -------------------------------------------------------------------
    // Type resolution
    // -------------------------------------------------------------------

    /// Resolve a type expression string to a TypeIndex.
    /// Checks type aliases and interfaces before falling back to the parser.
    /// For generic applications (e.g. Result<string>), instantiates the generic
    /// alias body by substituting type parameters with the provided arguments.
    pub fn resolveType(self: *TypeEnv, type_text: []const u8) TypeIndex {
        // Trim whitespace
        const trimmed = std.mem.trim(u8, type_text, " \t\n\r");
        if (trimmed.len == 0) return null_type_idx;

        // Check type aliases
        if (self.type_aliases.get(trimmed)) |idx| return idx;
        // Check interfaces
        if (self.interfaces.get(trimmed)) |idx| return idx;
        // Check generic scope stack (innermost first)
        if (self.generic_scopes.items.len > 0) {
            var i = self.generic_scopes.items.len;
            while (i > 0) {
                i -= 1;
                if (self.generic_scopes.items[i].resolve(trimmed)) |idx| return idx;
            }
        }
        // Fall back to type expression parser
        const parsed = parseTypeExpr(self.pool, self.allocator, trimmed);

        // If the parser produced a generic application (e.g. Result<string>),
        // try to instantiate it against a known generic alias.
        return self.tryInstantiateGenericApp(parsed);
    }

    /// If idx is a t_generic_app whose base resolves to a generic alias,
    /// instantiate the alias body with the provided type arguments.
    /// Recurses through intersection and union members so a generic
    /// application nested inside `Response & Spec<...>` or
    /// `Result<T> | Foo` is instantiated as well.
    fn tryInstantiateGenericApp(self: *TypeEnv, idx: TypeIndex) TypeIndex {
        if (idx == null_type_idx) return idx;
        const tag = self.pool.getTag(idx) orelse return idx;

        switch (tag) {
            .t_generic_app => {
                const info = self.pool.getGenericAppInfo(idx);
                if (info.base == null_type_idx) return idx;
                const base_name = self.pool.getRefName(info.base);
                if (base_name.len == 0) return idx;

                // Built-in utility types Pick/Omit/Partial/Required. Resolved
                // here (not at parse time like Readonly<T>) because resolving
                // a named source type to its record needs the alias/interface
                // maps, which only exist on TypeEnv.
                if (utilityKind(base_name)) |kind| {
                    const want_args: usize = switch (kind) {
                        .partial, .required, .readonly => 1,
                        .pick, .omit => 2,
                    };
                    if (info.args.len < want_args) return idx;
                    // Copy args before any transform: generic-app args live in
                    // the pool's shared members list, which a transform's
                    // addRecord/addUnion may reallocate.
                    var raw_args: [8]TypeIndex = undefined;
                    const uargc = @min(info.args.len, raw_args.len);
                    @memcpy(raw_args[0..uargc], info.args[0..uargc]);
                    const src = self.resolveRefToRecord(self.tryInstantiateGenericApp(raw_args[0]));
                    if (src == null_type_idx) return idx;
                    // Resolve the keys argument through the alias map too, so
                    // `type K = "id" | "name"; Pick<User, K>` sees the literal
                    // union instead of an unresolved t_ref (which collectStringKeys
                    // reads as zero keys, silently returning the full source - an
                    // unsound over-accept).
                    const keys = if (kind == .pick or kind == .omit)
                        self.resolveRef(self.tryInstantiateGenericApp(raw_args[1]))
                    else
                        null_type_idx;
                    return switch (kind) {
                        .partial => self.pool.makePartial(self.allocator, src),
                        .required => self.pool.makeRequired(self.allocator, src),
                        .readonly => self.pool.makeReadonly(self.allocator, src),
                        .pick => self.pool.pickFields(self.allocator, src, keys),
                        .omit => self.pool.omitFields(self.allocator, src, keys),
                    };
                }

                const alias = self.generic_aliases.get(base_name) orelse return idx;
                if (info.args.len != alias.param_count) return idx;
                // Copy args before instantiating: info.args is a slice into the
                // pool's shared members list, and a nested instantiation's
                // addRecord/addUnion may reallocate it, dangling the slice
                // mid-loop. Instantiate nested generic-app arguments first, so a
                // composed type like `Proof<Effects<string, "env">, "pure">`
                // carries both the proof marker and the effect marker.
                var inst_args: [8]TypeIndex = undefined;
                const argc = @min(info.args.len, inst_args.len);
                @memcpy(inst_args[0..argc], info.args[0..argc]);
                for (0..argc) |i| {
                    inst_args[i] = self.tryInstantiateGenericApp(inst_args[i]);
                }
                return self.pool.instantiate(
                    self.allocator,
                    alias.body,
                    alias.param_names[0..alias.param_count],
                    inst_args[0..argc],
                    0,
                );
            },
            .t_intersection => {
                // Copy members before the loop: getIntersectionMembers returns a
                // slice into the pool's shared members list, which a nested
                // instantiation may reallocate (UAF / garbage TypeIndex).
                const live = self.pool.getIntersectionMembers(idx);
                var members: [16]TypeIndex = undefined;
                const count = @min(live.len, 16);
                @memcpy(members[0..count], live[0..count]);
                var new_members: [16]TypeIndex = undefined;
                var changed = false;
                for (members[0..count], 0..) |m, i| {
                    new_members[i] = self.tryInstantiateGenericApp(m);
                    if (new_members[i] != m) changed = true;
                }
                if (!changed) return idx;
                return self.pool.addIntersection(self.allocator, new_members[0..count]);
            },
            .t_union => {
                // Copy members before the loop (see t_intersection above).
                const live = self.pool.getUnionMembers(idx);
                var members: [32]TypeIndex = undefined;
                const count = @min(live.len, 32);
                @memcpy(members[0..count], live[0..count]);
                var new_members: [32]TypeIndex = undefined;
                var changed = false;
                for (members[0..count], 0..) |m, i| {
                    new_members[i] = self.tryInstantiateGenericApp(m);
                    if (new_members[i] != m) changed = true;
                }
                if (!changed) return idx;
                return self.pool.addUnion(self.allocator, new_members[0..count]);
            },
            else => return idx,
        }
    }

    /// Follow a t_ref through the alias then interface map to its underlying
    /// type, chasing chains (`type A = B; type B = {...}` stores A as t_ref(B))
    /// up to a small depth with a self-reference guard. Non-ref types and
    /// unresolved refs pass through unchanged.
    fn resolveRef(self: *const TypeEnv, idx: TypeIndex) TypeIndex {
        var cur = idx;
        var depth: u8 = 0;
        while (depth < 8) : (depth += 1) {
            const tag = self.pool.getTag(cur) orelse return cur;
            if (tag != .t_ref) return cur;
            const name = self.pool.getRefName(cur);
            if (name.len == 0) return cur;
            const next = self.type_aliases.get(name) orelse self.interfaces.get(name) orelse return cur;
            if (next == cur) return cur;
            cur = next;
        }
        return cur;
    }

    /// Resolve a type to a record: a record passes through, a named ref resolves
    /// via resolveRef (following alias chains), otherwise null_type_idx. Used by
    /// the utility-type path to find the source object's fields.
    fn resolveRefToRecord(self: *const TypeEnv, idx: TypeIndex) TypeIndex {
        if (idx == null_type_idx) return null_type_idx;
        const resolved = self.resolveRef(idx);
        const tag = self.pool.getTag(resolved) orelse return null_type_idx;
        return if (tag == .t_record) resolved else null_type_idx;
    }

    /// Look up a variable's declared type by name.
    pub fn getVarTypeByName(self: *const TypeEnv, name: []const u8) ?TypeIndex {
        return self.var_types_by_name.get(name);
    }

    /// Resolve one declaration annotation by semantic name occurrence.
    pub fn getVarTypeByNameOrdinal(self: *const TypeEnv, name: []const u8, ordinal: u32) ?TypeIndex {
        for (self.var_annotations.items) |annotation| {
            if (annotation.ordinal == ordinal and std.mem.eql(u8, annotation.name, name)) {
                return annotation.type_idx;
            }
        }
        return null;
    }

    pub fn varAnnotationCount(self: *const TypeEnv) usize {
        return self.var_annotations.items.len;
    }

    pub fn bindVarType(self: *TypeEnv, scope_id: u16, name_atom: u16, type_idx: TypeIndex) !void {
        try self.var_types_by_binding.put(self.allocator, packBindingNameKey(scope_id, name_atom), type_idx);
    }

    pub fn getVarTypeByBinding(self: *const TypeEnv, scope_id: u16, name_atom: u16) ?TypeIndex {
        return self.var_types_by_binding.get(packBindingNameKey(scope_id, name_atom));
    }

    /// Look up a function signature by name.
    pub fn getFnSigByName(self: *const TypeEnv, name: []const u8) ?FunctionSig {
        return self.fn_sigs_by_name.get(name);
    }

    /// Look up a type alias by name.
    pub fn getTypeAlias(self: *const TypeEnv, name: []const u8) ?TypeIndex {
        return self.type_aliases.get(name);
    }

    /// Look up an interface by name.
    pub fn getInterface(self: *const TypeEnv, name: []const u8) ?TypeIndex {
        return self.interfaces.get(name);
    }

    /// Look up a variable type by source location.
    pub fn getVarTypeByLoc(self: *const TypeEnv, line: u32, col: u32) ?TypeIndex {
        return self.var_types.get(packLocationKey(line, col));
    }

    /// Look up a function signature by source location.
    pub fn getFnSigByLoc(self: *const TypeEnv, line: u32) ?FunctionSig {
        return self.fn_signatures.get(line);
    }

    /// Walk a TypeIndex (typically a function return-type annotation),
    /// following intersections and resolving alias references, and append
    /// every declared spec name string found inside a `Spec<...>` marker
    /// to `out`. Strings live as long as the type pool's name storage;
    /// the caller must not free them. Safe to call when no spec marker is
    /// present (the slice stays empty).
    pub fn extractSpecMembers(
        self: *const TypeEnv,
        idx: TypeIndex,
        out: *std.ArrayListUnmanaged([]const u8),
    ) void {
        self.collectMarkedMembers(idx, out, spec_marker_field, 0);
    }

    /// Like `extractSpecMembers`, but recovers the capability name strings
    /// inside an `Effects<...>` marker. A return type may carry both an
    /// `Effects<...>` and a `Spec<...>` / `Proof<...>` marker; each extraction
    /// keys on its own field name and ignores the other.
    pub fn extractEffectMembers(
        self: *const TypeEnv,
        idx: TypeIndex,
        out: *std.ArrayListUnmanaged([]const u8),
    ) void {
        self.collectMarkedMembers(idx, out, effect_marker_field, 0);
    }

    /// Strip phantom proof-marker members (the capsule records behind
    /// `Spec<...>`, `Proof<...>`, and `Effects<...>`) from a declared type,
    /// returning the value type a returned expression must actually satisfy.
    /// Markers are compile-time obligations discharged by the verifier and
    /// the contract extractor, never by the runtime value, so `return s`
    /// with `s: string` satisfies a declared `Effects<string, "env">`.
    /// Non-intersection types pass through unchanged.
    pub fn stripProofMarkers(self: *TypeEnv, idx: TypeIndex) TypeIndex {
        if (idx == null_type_idx) return idx;
        const tag = self.pool.getTag(idx) orelse return idx;
        if (tag != .t_intersection) return idx;

        var kept: [8]TypeIndex = undefined;
        var kept_count: usize = 0;
        for (self.pool.getIntersectionMembers(idx)) |member| {
            if (self.isProofMarkerRecord(member)) continue;
            // Recurse into nested intersections so composed markers like
            // Proof<Effects<string, "env">, "pure"> are fully stripped.
            const stripped_member = self.stripProofMarkers(member);
            if (stripped_member == null_type_idx) continue;
            // Wider intersections than the buffer: keep the declared type
            // unchanged rather than silently dropping members.
            if (kept_count >= kept.len) return idx;
            kept[kept_count] = stripped_member;
            kept_count += 1;
        }
        if (kept_count == 0) return idx;
        if (kept_count == 1) return kept[0];
        return self.pool.addIntersection(self.allocator, kept[0..kept_count]);
    }

    /// True when `idx` is (or aliases to) a single-field record whose only
    /// field is one of the phantom proof-marker fields.
    fn isProofMarkerRecord(self: *const TypeEnv, idx0: TypeIndex) bool {
        var idx = idx0;
        var depth: u8 = 0;
        while (depth < 8) : (depth += 1) {
            const tag = self.pool.getTag(idx) orelse return false;
            switch (tag) {
                .t_ref => {
                    const name = self.pool.getRefName(idx);
                    idx = self.type_aliases.get(name) orelse return false;
                },
                .t_record => {
                    const fields = self.pool.getRecordFields(idx);
                    if (fields.len != 1) return false;
                    const fname = self.pool.getName(fields[0].name_start, fields[0].name_len);
                    return std.mem.eql(u8, fname, spec_marker_field) or
                        std.mem.eql(u8, fname, effect_marker_field);
                },
                else => return false,
            }
        }
        return false;
    }

    fn collectMarkedMembers(
        self: *const TypeEnv,
        idx: TypeIndex,
        out: *std.ArrayListUnmanaged([]const u8),
        marker: []const u8,
        depth: u8,
    ) void {
        if (idx == null_type_idx or depth > 8) return;
        const tag = self.pool.getTag(idx) orelse return;

        switch (tag) {
            .t_intersection => {
                for (self.pool.getIntersectionMembers(idx)) |member| {
                    self.collectMarkedMembers(member, out, marker, depth + 1);
                }
            },
            .t_ref => {
                const name = self.pool.getRefName(idx);
                if (self.type_aliases.get(name)) |resolved| {
                    self.collectMarkedMembers(resolved, out, marker, depth + 1);
                }
            },
            .t_nullable => {
                // A nullable-wrapped marker (`Effects<...> | undefined`) still
                // carries the obligation; descend into the inner type so the
                // capability/spec set is recovered.
                self.collectMarkedMembers(self.pool.getNullableInner(idx), out, marker, depth + 1);
            },
            .t_record => {
                for (self.pool.getRecordFields(idx)) |field| {
                    const fname = self.pool.getName(field.name_start, field.name_len);
                    if (std.mem.eql(u8, fname, marker)) {
                        self.collectLiteralUnionStrings(field.type_idx, out);
                    }
                }
            },
            else => {},
        }
    }

    fn collectLiteralUnionStrings(
        self: *const TypeEnv,
        idx: TypeIndex,
        out: *std.ArrayListUnmanaged([]const u8),
    ) void {
        const tag = self.pool.getTag(idx) orelse return;
        switch (tag) {
            .t_union => {
                for (self.pool.getUnionMembers(idx)) |member| {
                    self.collectLiteralUnionStrings(member, out);
                }
            },
            .t_ref => {
                // An aliased capability set (`type Caps = "clock" | "crypto";`
                // used as `Effects<Response, Caps>`) reaches here as a t_ref.
                // Resolve through the alias/interface chain and recurse so the
                // literal names are recovered; without this the budget extracts
                // zero effects and the capability proof silently fails open.
                const resolved = self.resolveRef(idx);
                if (resolved != idx) self.collectLiteralUnionStrings(resolved, out);
            },
            .t_literal_string => {
                if (self.pool.getLiteralStringValue(idx)) |val| {
                    out.append(self.allocator, val) catch {};
                }
            },
            else => {},
        }
    }

    // -------------------------------------------------------------------
    // Generic scope management
    // -------------------------------------------------------------------

    pub fn pushGenericScope(self: *TypeEnv) void {
        self.generic_scopes.append(self.allocator, .{}) catch {};
    }

    pub fn popGenericScope(self: *TypeEnv) void {
        if (self.generic_scopes.items.len > 0) {
            _ = self.generic_scopes.pop();
        }
    }

    pub fn addGenericParam(self: *TypeEnv, name: []const u8) TypeIndex {
        const idx = self.pool.addGenericParam(self.allocator, name);
        if (self.generic_scopes.items.len > 0) {
            self.generic_scopes.items[self.generic_scopes.items.len - 1].addParam(name, idx);
        }
        return idx;
    }

    // -------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------

    pub fn internName(self: *TypeEnv, name: []const u8) []const u8 {
        const owned = self.allocator.dupe(u8, name) catch return "";
        self.name_storage.append(self.allocator, owned) catch {
            self.allocator.free(owned);
            return "";
        };
        return owned;
    }
};

fn packLocationKey(line: u32, col: u32) u32 {
    // Pack line (20 bits) + col (12 bits) into u32
    return (line << 12) | (col & 0xFFF);
}

fn packBindingNameKey(scope_id: u16, name_atom: u16) u32 {
    return (@as(u32, scope_id) << 16) | name_atom;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "TypeEnv basic type alias resolution" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    // Simulate: type Config = { port: number }
    const source = "type Config = { port: number };";
    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    try tm.addEntry(allocator, .{
        .kind = .type_alias,
        .source_start = 14, // "{ port: number }"
        .source_end = 30,
        .context_line = 1,
        .context_col = 1,
        .name_start = 5, // "Config"
        .name_end = 11,
    });

    env.populateFromTypeMap(&tm);

    const config_idx = env.getTypeAlias("Config");
    try std.testing.expect(config_idx != null);
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_record, pool.getTag(config_idx.?).?);
}

test "TypeEnv variable annotation" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const source = "const port: number = 8080;";
    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    try tm.addEntry(allocator, .{
        .kind = .var_annotation,
        .source_start = 12, // "number"
        .source_end = 18,
        .context_line = 1,
        .context_col = 1,
        .name_start = 6, // "port"
        .name_end = 10,
    });

    env.populateFromTypeMap(&tm);

    const port_type = env.getVarTypeByName("port");
    try std.testing.expect(port_type != null);
    try std.testing.expectEqual(pool.idx_number, port_type.?);
}

test "TypeEnv resolves type aliases in annotations" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    // First: define a type alias
    const source = "type Status = number;const code: Status = 200;";
    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    try tm.addEntry(allocator, .{
        .kind = .type_alias,
        .source_start = 14, // "number"
        .source_end = 20,
        .context_line = 1,
        .context_col = 1,
        .name_start = 5, // "Status"
        .name_end = 11,
    });

    try tm.addEntry(allocator, .{
        .kind = .var_annotation,
        .source_start = 33, // "Status"
        .source_end = 39,
        .context_line = 1,
        .context_col = 22,
        .name_start = 27, // "code"
        .name_end = 31,
    });

    env.populateFromTypeMap(&tm);

    // "Status" should resolve to number
    const status_idx = env.getTypeAlias("Status");
    try std.testing.expect(status_idx != null);
    try std.testing.expectEqual(pool.idx_number, status_idx.?);

    // "code" should also be number (via Status alias)
    const code_type = env.getVarTypeByName("code");
    try std.testing.expect(code_type != null);
    try std.testing.expectEqual(pool.idx_number, code_type.?);
}

test "TypeEnv function signature from params and return" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const source = "function add(a: number, b: number): number { }";
    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    // param a: number
    try tm.addEntry(allocator, .{
        .kind = .param_annotation,
        .source_start = 16, // "number"
        .source_end = 22,
        .context_line = 1,
        .context_col = 1,
        .name_start = 13, // "a"
        .name_end = 14,
    });
    // param b: number
    try tm.addEntry(allocator, .{
        .kind = .param_annotation,
        .source_start = 27, // "number"
        .source_end = 33,
        .context_line = 1,
        .context_col = 1,
        .name_start = 24, // "b"
        .name_end = 25,
    });
    // return: number
    try tm.addEntry(allocator, .{
        .kind = .return_annotation,
        .source_start = 36, // "number"
        .source_end = 42,
        .context_line = 1,
        .context_col = 1,
        .name_start = 0,
        .name_end = 0,
    });

    env.populateFromTypeMap(&tm);

    const sig = env.getFnSigByLoc(1);
    try std.testing.expect(sig != null);
    try std.testing.expectEqual(@as(u8, 2), sig.?.param_count);
    try std.testing.expectEqual(pool.idx_number, sig.?.param_types[0]);
    try std.testing.expectEqual(pool.idx_number, sig.?.param_types[1]);
    try std.testing.expectEqual(pool.idx_number, sig.?.return_type);
}

test "TypeEnv generic scope" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    env.pushGenericScope();
    const t_idx = env.addGenericParam("T");
    try std.testing.expect(t_idx != null_type_idx);

    // "T" should resolve to the generic param
    const resolved = env.resolveType("T");
    try std.testing.expectEqual(t_idx, resolved);

    env.popGenericScope();

    // After popping, "T" should NOT resolve to the generic param
    const resolved2 = env.resolveType("T");
    // It will be a ref type now, not the generic param
    try std.testing.expect(resolved2 != t_idx);
}

test "TypeEnv generic type alias Result<string>" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    // Simulate: type Result<T> = { ok: boolean; value: T; error: string }
    //           const auth: Result<object> = jwtVerify(token, secret);
    const source = "type Result<T> = { ok: boolean; value: T; error: string };const auth: Result<object> = jwtVerify();";
    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    // type_alias entry: body is "{ ok: boolean; value: T; error: string }"
    try tm.addEntry(allocator, .{
        .kind = .type_alias,
        .source_start = 17, // "{ ok: boolean; value: T; error: string }"
        .source_end = 57,
        .context_line = 1,
        .context_col = 1,
        .name_start = 5, // "Result"
        .name_end = 11,
    });

    // generic_params entry: "T" (same name range as the alias)
    try tm.addEntry(allocator, .{
        .kind = .generic_params,
        .source_start = 12, // "T"
        .source_end = 13,
        .context_line = 1,
        .context_col = 1,
        .name_start = 5, // "Result"
        .name_end = 11,
    });

    // var annotation: auth: Result<object>
    try tm.addEntry(allocator, .{
        .kind = .var_annotation,
        .source_start = 70, // "Result<object>"
        .source_end = 84,
        .context_line = 1,
        .context_col = 58,
        .name_start = 64, // "auth"
        .name_end = 68,
    });

    env.populateFromTypeMap(&tm);

    // "Result" should be in generic_aliases, not type_aliases
    try std.testing.expect(env.getTypeAlias("Result") == null);
    try std.testing.expect(env.generic_aliases.get("Result") != null);

    // auth should resolve to an instantiated record with value: object (ref)
    const auth_type = env.getVarTypeByName("auth");
    try std.testing.expect(auth_type != null);
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_record, pool.getTag(auth_type.?).?);

    const fields = pool.getRecordFields(auth_type.?);
    try std.testing.expectEqual(@as(usize, 3), fields.len);

    // ok: boolean
    try std.testing.expectEqual(pool.idx_boolean, fields[0].type_idx);
    // value: should be a ref("object"), not a generic param
    try std.testing.expect(pool.getTag(fields[1].type_idx) != .t_generic_param);
    // error: string
    try std.testing.expectEqual(pool.idx_string, fields[2].type_idx);
}

test "TypeEnv generic alias with multiple params" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    // Simulate: type Pair<A, B> = { first: A; second: B }
    //           const p: Pair<string, number> = ...
    const source = "type Pair<A, B> = { first: A; second: B };const p: Pair<string, number> = x;";
    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    try tm.addEntry(allocator, .{
        .kind = .type_alias,
        .source_start = 18, // "{ first: A; second: B }"
        .source_end = 41,
        .context_line = 1,
        .context_col = 1,
        .name_start = 5, // "Pair"
        .name_end = 9,
    });

    try tm.addEntry(allocator, .{
        .kind = .generic_params,
        .source_start = 10, // "A, B"
        .source_end = 14,
        .context_line = 1,
        .context_col = 1,
        .name_start = 5,
        .name_end = 9,
    });

    try tm.addEntry(allocator, .{
        .kind = .var_annotation,
        .source_start = 51, // "Pair<string, number>"
        .source_end = 71,
        .context_line = 1,
        .context_col = 42,
        .name_start = 48, // "p"
        .name_end = 49,
    });

    env.populateFromTypeMap(&tm);

    const p_type = env.getVarTypeByName("p");
    try std.testing.expect(p_type != null);
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_record, pool.getTag(p_type.?).?);

    const fields = pool.getRecordFields(p_type.?);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    // first: string
    try std.testing.expectEqual(pool.idx_string, fields[0].type_idx);
    // second: number
    try std.testing.expectEqual(pool.idx_number, fields[1].type_idx);
}

test "TypeEnv resolveType instantiates generic alias inline" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    // Manually register a generic alias: type Box<T> = { value: T }
    env.pushGenericScope();
    const t_param = env.addGenericParam("T");
    _ = t_param;
    const val_n = pool.addName(allocator, "value");
    const body = pool.addRecord(allocator, &.{
        .{ .name_start = val_n.start, .name_len = val_n.len, .type_idx = env.resolveType("T"), .optional = false },
    });
    env.popGenericScope();

    const owned = env.internName("Box");
    env.generic_aliases.put(allocator, owned, .{
        .param_names = .{ env.internName("T"), undefined, undefined, undefined, undefined, undefined, undefined, undefined },
        .param_count = 1,
        .body = body,
    }) catch {};

    // resolveType("Box<number>") should instantiate to { value: number }
    const resolved = env.resolveType("Box<number>");
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_record, pool.getTag(resolved).?);
    const fields = pool.getRecordFields(resolved);
    try std.testing.expectEqual(@as(usize, 1), fields.len);
    try std.testing.expectEqual(pool.idx_number, fields[0].type_idx);
}

test "TypeEnv intersection alias type AB = A & B" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    // Simulate: type AB = A & B;
    // Names A and B stay as t_ref inside the intersection (resolveType only
    // substitutes alias names when the entire annotation text matches; nested
    // identifiers in compound expressions remain refs - same behaviour as union).
    const source = "type AB = A & B;";
    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    try tm.addEntry(allocator, .{
        .kind = .type_alias,
        .source_start = 10, // "A & B"
        .source_end = 15,
        .context_line = 1,
        .context_col = 1,
        .name_start = 5, // "AB"
        .name_end = 7,
    });

    env.populateFromTypeMap(&tm);

    const ab_idx = env.getTypeAlias("AB");
    try std.testing.expect(ab_idx != null);
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_intersection, pool.getTag(ab_idx.?).?);
    const members = pool.getIntersectionMembers(ab_idx.?);
    try std.testing.expectEqual(@as(usize, 2), members.len);
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_ref, pool.getTag(members[0]).?);
    try std.testing.expectEqualStrings("A", pool.getRefName(members[0]));
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_ref, pool.getTag(members[1]).?);
    try std.testing.expectEqualStrings("B", pool.getRefName(members[1]));
}

test "TypeEnv return-type intersection X & Y" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const owned_x = env.internName("X");
    env.type_aliases.put(allocator, owned_x, pool.idx_string) catch {};
    const owned_y = env.internName("Y");
    env.type_aliases.put(allocator, owned_y, pool.idx_number) catch {};

    // function f(): X & Y { ... }
    const source = "function f(): X & Y { return null; }";
    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    try tm.addEntry(allocator, .{
        .kind = .return_annotation,
        .source_start = 14, // "X & Y"
        .source_end = 19,
        .context_line = 7,
        .context_col = 1,
        .name_start = 0,
        .name_end = 0,
    });

    env.populateFromTypeMap(&tm);

    const sig = env.getFnSigByLoc(7);
    try std.testing.expect(sig != null);
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_intersection, pool.getTag(sig.?.return_type).?);
}

test "resolveType Pick<User, keys> keeps only the named fields" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);
    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const user = parseTypeExpr(&pool, allocator, "{ id: number; name: string; age: number }");
    env.type_aliases.put(allocator, env.internName("User"), user) catch unreachable;

    const picked = env.resolveType("Pick<User, \"id\" | \"name\">");
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_record, pool.getTag(picked).?);
    try std.testing.expectEqual(@as(usize, 2), pool.getRecordFields(picked).len);
    try std.testing.expect(pool.lookupRecordField(picked, "id") != null);
    try std.testing.expect(pool.lookupRecordField(picked, "name") != null);
    try std.testing.expect(pool.lookupRecordField(picked, "age") == null);
}

test "resolveType Pick<User, Keys> resolves an aliased key union (not the full source)" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);
    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const user = parseTypeExpr(&pool, allocator, "{ id: number; name: string; age: number }");
    env.type_aliases.put(allocator, env.internName("User"), user) catch unreachable;
    // Keys is a named alias of a string-literal union. Previously the keys
    // argument was not alias-resolved, so collectStringKeys saw a t_ref, found
    // zero keys, and Pick silently returned the full User - an unsound
    // over-accept. It must now resolve to exactly {id, name}.
    const keys = parseTypeExpr(&pool, allocator, "\"id\" | \"name\"");
    env.type_aliases.put(allocator, env.internName("Keys"), keys) catch unreachable;

    const picked = env.resolveType("Pick<User, Keys>");
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_record, pool.getTag(picked).?);
    try std.testing.expectEqual(@as(usize, 2), pool.getRecordFields(picked).len);
    try std.testing.expect(pool.lookupRecordField(picked, "id") != null);
    try std.testing.expect(pool.lookupRecordField(picked, "name") != null);
    try std.testing.expect(pool.lookupRecordField(picked, "age") == null);
}

test "resolveType Required<A> follows a forward alias chain to the record" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);
    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    // `type A = B` (forward) stores A as a t_ref(B); B is the record. Previously
    // resolveRefToRecord returned the t_ref unchanged, so Required<A> no-op'd and
    // accepted any record. It must now chase the chain and clear `host?`.
    const b = parseTypeExpr(&pool, allocator, "{ host?: string }");
    env.type_aliases.put(allocator, env.internName("B"), b) catch unreachable;
    const a = parseTypeExpr(&pool, allocator, "B");
    env.type_aliases.put(allocator, env.internName("A"), a) catch unreachable;

    const required = env.resolveType("Required<A>");
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_record, pool.getTag(required).?);
    const fields = pool.getRecordFields(required);
    try std.testing.expectEqual(@as(usize, 1), fields.len);
    try std.testing.expect(!fields[0].optional);
}

test "resolveType Omit<User, key> drops the named field" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);
    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const user = parseTypeExpr(&pool, allocator, "{ id: number; name: string; age: number }");
    env.type_aliases.put(allocator, env.internName("User"), user) catch unreachable;

    const omitted = env.resolveType("Omit<User, \"age\">");
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_record, pool.getTag(omitted).?);
    try std.testing.expectEqual(@as(usize, 2), pool.getRecordFields(omitted).len);
    try std.testing.expect(pool.lookupRecordField(omitted, "id") != null);
    try std.testing.expect(pool.lookupRecordField(omitted, "age") == null);
}

test "resolveType Partial<User> makes fields optional and assignable from a subset" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);
    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const user = parseTypeExpr(&pool, allocator, "{ id: number; name: string }");
    env.type_aliases.put(allocator, env.internName("User"), user) catch unreachable;

    const partial = env.resolveType("Partial<User>");
    const fields = pool.getRecordFields(partial);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    try std.testing.expect(fields[0].optional);
    try std.testing.expect(fields[1].optional);

    // An object missing a field is assignable to Partial<User> but not to the
    // original required type - the transform flows into the assignability check.
    const subset = parseTypeExpr(&pool, allocator, "{ id: number }");
    try std.testing.expect(pool.isAssignableTo(subset, partial));
    try std.testing.expect(!pool.isAssignableTo(subset, user));
}

test "resolveType Required<Conf> clears optional and rejects a missing field" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);
    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const conf = parseTypeExpr(&pool, allocator, "{ host?: string; port?: number }");
    env.type_aliases.put(allocator, env.internName("Conf"), conf) catch unreachable;

    const req = env.resolveType("Required<Conf>");
    const fields = pool.getRecordFields(req);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    try std.testing.expect(!fields[0].optional);
    try std.testing.expect(!fields[1].optional);

    const subset = parseTypeExpr(&pool, allocator, "{ host: string }");
    try std.testing.expect(!pool.isAssignableTo(subset, req));
}

test "TypeEnv registers Spec<S> built-in alias on init" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    // Spec must live in generic_aliases (parametric), not type_aliases.
    try std.testing.expect(env.getTypeAlias("Spec") == null);
    try std.testing.expect(env.generic_aliases.get("Spec") != null);

    // Body must be a record with the magic field name carrying S.
    const alias = env.generic_aliases.get("Spec").?;
    try std.testing.expectEqual(@as(u8, 1), alias.param_count);
    try std.testing.expectEqualStrings("S", alias.param_names[0]);

    const body_tag = pool.getTag(alias.body) orelse return error.MissingBody;
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_record, body_tag);

    const fields = pool.getRecordFields(alias.body);
    try std.testing.expectEqual(@as(usize, 1), fields.len);
    try std.testing.expectEqualStrings(spec_marker_field, pool.getName(fields[0].name_start, fields[0].name_len));
}

test "TypeEnv resolveType Spec<\"a\" | \"b\"> instantiates marker" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const idx = env.resolveType("Spec<\"a\" | \"b\">");
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_record, pool.getTag(idx).?);

    const fields = pool.getRecordFields(idx);
    try std.testing.expectEqual(@as(usize, 1), fields.len);
    try std.testing.expectEqualStrings(spec_marker_field, pool.getName(fields[0].name_start, fields[0].name_len));

    // Field type is the literal-string union "a" | "b".
    const union_tag = pool.getTag(fields[0].type_idx) orelse return error.MissingBody;
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_union, union_tag);
    try std.testing.expectEqual(@as(usize, 2), pool.getUnionMembers(fields[0].type_idx).len);
}

test "extractSpecMembers walks intersection through alias to literal union" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    // Simulate:
    //     type Guardrails = Spec<"idempotent" | "deterministic">;
    //     function handler(): Response & Guardrails { ... }
    const source =
        "type Guardrails = Spec<\"idempotent\" | \"deterministic\">;" ++
        "function handler(): Response & Guardrails { return null; }";
    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    // Guardrails alias body: Spec<"idempotent" | "deterministic">
    try tm.addEntry(allocator, .{
        .kind = .type_alias,
        .source_start = 18,
        .source_end = 54,
        .context_line = 1,
        .context_col = 1,
        .name_start = 5,
        .name_end = 15,
    });
    // Return annotation: Response & Guardrails
    try tm.addEntry(allocator, .{
        .kind = .return_annotation,
        .source_start = 75,
        .source_end = 96,
        .context_line = 7,
        .context_col = 1,
        .name_start = 0,
        .name_end = 0,
    });

    env.populateFromTypeMap(&tm);

    const sig = env.getFnSigByLoc(7) orelse return error.MissingSig;

    var names: std.ArrayListUnmanaged([]const u8) = .empty;
    defer names.deinit(allocator);
    env.extractSpecMembers(sig.return_type, &names);

    try std.testing.expectEqual(@as(usize, 2), names.items.len);
    try std.testing.expectEqualStrings("idempotent", names.items[0]);
    try std.testing.expectEqualStrings("deterministic", names.items[1]);
}

test "extractSpecMembers handles inline Response & Spec<\"name\">" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    // function handler(): Response & Spec<"idempotent"> { ... }
    const source = "function handler(): Response & Spec<\"idempotent\"> { return null; }";
    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    try tm.addEntry(allocator, .{
        .kind = .return_annotation,
        .source_start = 20,
        .source_end = 49,
        .context_line = 3,
        .context_col = 1,
        .name_start = 0,
        .name_end = 0,
    });

    env.populateFromTypeMap(&tm);

    const sig = env.getFnSigByLoc(3) orelse return error.MissingSig;

    var names: std.ArrayListUnmanaged([]const u8) = .empty;
    defer names.deinit(allocator);
    env.extractSpecMembers(sig.return_type, &names);

    try std.testing.expectEqual(@as(usize, 1), names.items.len);
    try std.testing.expectEqualStrings("idempotent", names.items[0]);
}

test "extractSpecMembers returns empty when no Spec marker" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const source = "function handler(): Response { return null; }";
    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    try tm.addEntry(allocator, .{
        .kind = .return_annotation,
        .source_start = 20,
        .source_end = 28,
        .context_line = 5,
        .context_col = 1,
        .name_start = 0,
        .name_end = 0,
    });

    env.populateFromTypeMap(&tm);

    const sig = env.getFnSigByLoc(5) orelse return error.MissingSig;

    var names: std.ArrayListUnmanaged([]const u8) = .empty;
    defer names.deinit(allocator);
    env.extractSpecMembers(sig.return_type, &names);

    try std.testing.expectEqual(@as(usize, 0), names.items.len);
}

test "TypeEnv registers Proof<T, S> built-in alias on init" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    try std.testing.expect(env.getTypeAlias("Proof") == null);
    const alias = env.generic_aliases.get("Proof") orelse return error.MissingAlias;
    try std.testing.expectEqual(@as(u8, 2), alias.param_count);
    try std.testing.expectEqualStrings("T", alias.param_names[0]);
    try std.testing.expectEqualStrings("S", alias.param_names[1]);

    // Body is an intersection: the underlying T plus the spec marker record.
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_intersection, pool.getTag(alias.body).?);
}

test "resolveType Proof<T, S> carries capsule property members" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    // A capsule with two declared properties on an underlying string return.
    const idx = env.resolveType("Proof<string, \"total\" | \"pure\">");

    var names: std.ArrayListUnmanaged([]const u8) = .empty;
    defer names.deinit(allocator);
    env.extractSpecMembers(idx, &names);

    try std.testing.expectEqual(@as(usize, 2), names.items.len);
    try std.testing.expectEqualStrings("total", names.items[0]);
    try std.testing.expectEqualStrings("pure", names.items[1]);
}

test "extractSpecMembers handles inline return type Proof<T, \"name\">" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    // function load(): Proof<string, "read_only"> { ... }
    const source = "function load(): Proof<string, \"read_only\"> { return \"\"; }";
    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    try tm.addEntry(allocator, .{
        .kind = .return_annotation,
        .source_start = 17,
        .source_end = 43,
        .context_line = 4,
        .context_col = 1,
        .name_start = 0,
        .name_end = 0,
    });

    env.populateFromTypeMap(&tm);

    const sig = env.getFnSigByLoc(4) orelse return error.MissingSig;

    var names: std.ArrayListUnmanaged([]const u8) = .empty;
    defer names.deinit(allocator);
    env.extractSpecMembers(sig.return_type, &names);

    try std.testing.expectEqual(@as(usize, 1), names.items.len);
    try std.testing.expectEqualStrings("read_only", names.items[0]);
}

test "TypeEnv registers Effects<T, S> built-in alias on init" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const alias = env.generic_aliases.get("Effects") orelse return error.MissingAlias;
    try std.testing.expectEqual(@as(u8, 2), alias.param_count);
    try std.testing.expectEqualStrings("T", alias.param_names[0]);
    try std.testing.expectEqualStrings("S", alias.param_names[1]);
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_intersection, pool.getTag(alias.body).?);
}

test "resolveType Effects<T, S> carries capability members under its own marker" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const idx = env.resolveType("Effects<string, \"env\" | \"crypto\">");

    var caps: std.ArrayListUnmanaged([]const u8) = .empty;
    defer caps.deinit(allocator);
    env.extractEffectMembers(idx, &caps);
    try std.testing.expectEqual(@as(usize, 2), caps.items.len);
    try std.testing.expectEqualStrings("env", caps.items[0]);
    try std.testing.expectEqualStrings("crypto", caps.items[1]);

    // The effect marker is distinct from the spec marker: an Effects<...>
    // type carries no Spec/Proof members.
    var specs: std.ArrayListUnmanaged([]const u8) = .empty;
    defer specs.deinit(allocator);
    env.extractSpecMembers(idx, &specs);
    try std.testing.expectEqual(@as(usize, 0), specs.items.len);
}

test "Proof<Effects<...>, ...> composes: each marker extracted independently" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    const idx = env.resolveType("Proof<Effects<string, \"env\">, \"pure\">");

    var caps: std.ArrayListUnmanaged([]const u8) = .empty;
    defer caps.deinit(allocator);
    env.extractEffectMembers(idx, &caps);
    try std.testing.expectEqual(@as(usize, 1), caps.items.len);
    try std.testing.expectEqualStrings("env", caps.items[0]);

    var specs: std.ArrayListUnmanaged([]const u8) = .empty;
    defer specs.deinit(allocator);
    env.extractSpecMembers(idx, &specs);
    try std.testing.expectEqual(@as(usize, 1), specs.items.len);
    try std.testing.expectEqualStrings("pure", specs.items[0]);
}

test "TypeEnv leading-pipe union literal" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    // type Names = | "a" | "b" | "c"
    const source = "type Names = | \"a\" | \"b\" | \"c\";";
    var tm = TypeMap.init(source);
    defer tm.deinit(allocator);

    try tm.addEntry(allocator, .{
        .kind = .type_alias,
        .source_start = 13, // "| \"a\" | \"b\" | \"c\""
        .source_end = 30,
        .context_line = 1,
        .context_col = 1,
        .name_start = 5, // "Names"
        .name_end = 10,
    });

    env.populateFromTypeMap(&tm);

    const names_idx = env.getTypeAlias("Names");
    try std.testing.expect(names_idx != null);
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_union, pool.getTag(names_idx.?).?);
    try std.testing.expectEqual(@as(usize, 3), pool.getUnionMembers(names_idx.?).len);
}
