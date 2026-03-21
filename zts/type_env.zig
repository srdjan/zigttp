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
// Type Environment
// ---------------------------------------------------------------------------

pub const TypeEnv = struct {
    pool: *TypePool,
    allocator: std.mem.Allocator,

    /// Type namespace: type alias name -> resolved TypeIndex
    type_aliases: std.StringHashMapUnmanaged(TypeIndex),
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
        for (tm.entries.items) |entry| {
            switch (entry.kind) {
                .type_alias => self.processTypeAlias(tm, entry),
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

    fn processTypeAlias(self: *TypeEnv, tm: *const TypeMap, entry: TypeMapEntry) void {
        const name = tm.getNameText(entry) orelse return;
        const type_text = tm.getTypeText(entry);
        if (type_text.len == 0) return;

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
        return parseTypeExpr(self.pool, self.allocator, trimmed);
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
