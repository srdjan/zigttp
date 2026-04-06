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
    /// Function name -> signature (for name-based lookup)
    fn_sigs_by_name: std.StringHashMapUnmanaged(FunctionSig),
    /// Generic scope stack
    generic_scopes: std.ArrayListUnmanaged(GenericScope),

    /// Stable storage for interned names used as hash-map keys.
    /// Keys must not move after insertion, so each name owns its own allocation.
    name_storage: std.ArrayListUnmanaged([]const u8),

    pub fn init(allocator: std.mem.Allocator, pool: *TypePool) TypeEnv {
        return .{
            .pool = pool,
            .allocator = allocator,
            .type_aliases = .empty,
            .generic_aliases = .empty,
            .interfaces = .empty,
            .var_types = .empty,
            .fn_signatures = .empty,
            .var_types_by_name = .empty,
            .fn_sigs_by_name = .empty,
            .generic_scopes = .empty,
            .name_storage = .empty,
        };
    }

    pub fn deinit(self: *TypeEnv) void {
        self.type_aliases.deinit(self.allocator);
        self.generic_aliases.deinit(self.allocator);
        self.interfaces.deinit(self.allocator);
        self.var_types.deinit(self.allocator);
        self.fn_signatures.deinit(self.allocator);
        self.var_types_by_name.deinit(self.allocator);
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
                else => {},
            }
        }

        // Second pass: variable and function annotations
        // Group param and return annotations by context line to build function sigs
        var fn_params_by_line: std.AutoHashMapUnmanaged(u32, FunctionSig) = .empty;
        defer fn_params_by_line.deinit(self.allocator);

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
                },
                else => {},
            }
        }

        // Merge function signatures
        var iter = fn_params_by_line.iterator();
        while (iter.next()) |kv| {
            self.fn_signatures.put(self.allocator, kv.key_ptr.*, kv.value_ptr.*) catch {};
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
    fn tryInstantiateGenericApp(self: *TypeEnv, idx: TypeIndex) TypeIndex {
        if (idx == null_type_idx) return idx;
        if (self.pool.getTag(idx) != .t_generic_app) return idx;

        const info = self.pool.getGenericAppInfo(idx);
        if (info.base == null_type_idx) return idx;

        // The base should be a t_ref; get its name.
        const base_name = self.pool.getRefName(info.base);
        if (base_name.len == 0) return idx;

        const alias = self.generic_aliases.get(base_name) orelse return idx;
        if (info.args.len != alias.param_count) return idx;

        return self.pool.instantiate(
            self.allocator,
            alias.body,
            alias.param_names[0..alias.param_count],
            info.args,
            0,
        );
    }

    /// Look up a variable's declared type by name.
    pub fn getVarTypeByName(self: *const TypeEnv, name: []const u8) ?TypeIndex {
        return self.var_types_by_name.get(name);
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
