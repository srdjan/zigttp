//! Virtual Module Type Declarations
//!
//! Defines full function signatures for all virtual module exports.
//! Used by the type checker to validate argument types and infer return types
//! for calls to zigttp:* module functions. Return types use T | undefined for
//! optional values (no null in user-facing API).
//!
//! Replaces the hardcoded module_return_types table in bool_checker when
//! the type checker is active.

const std = @import("std");
const type_pool_mod = @import("../type_pool.zig");
const type_env_mod = @import("../type_env.zig");

const TypePool = type_pool_mod.TypePool;
const TypeIndex = type_pool_mod.TypeIndex;
const null_type_idx = type_pool_mod.null_type_idx;
const TypeEnv = type_env_mod.TypeEnv;
const FuncParam = type_pool_mod.FuncParam;

/// Module function signature: module name, function name, param types, return type.
pub const ModuleFnSig = struct {
    module: []const u8,
    name: []const u8,
    param_types: []const TypeIndex,
    return_type: TypeIndex,
    is_result: bool = false,
};

/// Populate the TypeEnv with full type signatures for all virtual module exports.
/// Call this after TypePool is initialized with primitive types.
pub fn populateModuleTypes(env: *TypeEnv, pool: *TypePool, allocator: std.mem.Allocator) void {
    // Build result type: { ok: boolean; value: unknown; error: string }
    const ok_n = pool.addName(allocator, "ok");
    const val_n = pool.addName(allocator, "value");
    const err_n = pool.addName(allocator, "error");
    const result_type = pool.addRecord(allocator, &.{
        .{ .name_start = ok_n.start, .name_len = ok_n.len, .type_idx = pool.idx_boolean, .optional = false },
        .{ .name_start = val_n.start, .name_len = val_n.len, .type_idx = pool.idx_unknown, .optional = false },
        .{ .name_start = err_n.start, .name_len = err_n.len, .type_idx = pool.idx_string, .optional = false },
    });

    // Optional types (T | undefined)
    const optional_string = pool.addNullable(allocator, pool.idx_string);
    const optional_object = pool.addNullable(allocator, pool.addRef(allocator, "object"));

    // Store module function signatures in the env by name
    const sigs = [_]struct { name: []const u8, sig: type_env_mod.FunctionSig }{
        // zigttp:auth
        .{ .name = "verifyWebhookSignature", .sig = makeSig(&.{ pool.idx_string, pool.idx_string, pool.idx_string }, pool.idx_boolean) },
        .{ .name = "timingSafeEqual", .sig = makeSig(&.{ pool.idx_string, pool.idx_string }, pool.idx_boolean) },
        .{ .name = "jwtVerify", .sig = makeSig(&.{ pool.idx_string, pool.idx_string }, result_type) },
        .{ .name = "jwtSign", .sig = makeSig(&.{ pool.idx_string, pool.idx_string }, pool.idx_string) },
        .{ .name = "parseBearer", .sig = makeSig(&.{pool.idx_string}, optional_string) },
        // zigttp:crypto
        .{ .name = "sha256", .sig = makeSig(&.{pool.idx_string}, pool.idx_string) },
        .{ .name = "hmacSha256", .sig = makeSig(&.{ pool.idx_string, pool.idx_string }, pool.idx_string) },
        .{ .name = "base64Encode", .sig = makeSig(&.{pool.idx_string}, pool.idx_string) },
        .{ .name = "base64Decode", .sig = makeSig(&.{pool.idx_string}, pool.idx_string) },
        // zigttp:validate
        .{ .name = "schemaCompile", .sig = makeSig(&.{pool.idx_string}, pool.idx_boolean) },
        .{ .name = "validateJson", .sig = makeSig(&.{ pool.idx_string, pool.idx_string }, result_type) },
        .{ .name = "validateObject", .sig = makeSig(&.{ pool.idx_string, pool.idx_string }, result_type) },
        .{ .name = "coerceJson", .sig = makeSig(&.{ pool.idx_string, pool.idx_string }, result_type) },
        .{ .name = "schemaDrop", .sig = makeSig(&.{pool.idx_string}, pool.idx_boolean) },
        // zigttp:cache
        .{ .name = "cacheSet", .sig = makeSig(&.{ pool.idx_string, pool.idx_string }, pool.idx_boolean) },
        .{ .name = "cacheDelete", .sig = makeSig(&.{pool.idx_string}, pool.idx_boolean) },
        .{ .name = "cacheIncr", .sig = makeSig(&.{pool.idx_string}, pool.idx_number) },
        .{ .name = "cacheStats", .sig = makeSig(&.{}, pool.idx_string) },
        .{ .name = "cacheGet", .sig = makeSig(&.{pool.idx_string}, optional_string) },
        // zigttp:env
        .{ .name = "env", .sig = makeSig(&.{pool.idx_string}, optional_string) },
        // zigttp:router
        .{ .name = "routerMatch", .sig = makeSig(&.{ pool.idx_string, pool.idx_string, pool.idx_string }, optional_object) },
        // zigttp:io
        .{ .name = "parallel", .sig = makeSig(&.{}, pool.idx_string) }, // takes array of thunks
        .{ .name = "race", .sig = makeSig(&.{}, pool.idx_string) }, // takes array of thunks
        // zigttp:compose
        .{ .name = "guard", .sig = makeSig(&.{pool.idx_string}, pool.idx_string) }, // compile-time marker
        // zigttp:durable
        .{ .name = "run", .sig = makeSig(&.{ pool.idx_string, pool.idx_unknown }, pool.idx_unknown) },
        .{ .name = "step", .sig = makeSig(&.{ pool.idx_string, pool.idx_unknown }, pool.idx_unknown) },
    };

    for (sigs) |entry| {
        const owned = env.internName(entry.name);
        env.fn_sigs_by_name.put(allocator, owned, entry.sig) catch {};
    }
}

fn makeSig(param_types: []const TypeIndex, return_type: TypeIndex) type_env_mod.FunctionSig {
    var sig = type_env_mod.FunctionSig{};
    sig.return_type = return_type;
    const count: u8 = @intCast(@min(param_types.len, 16));
    sig.param_count = count;
    for (param_types[0..count], 0..) |pt, i| {
        sig.param_types[i] = pt;
    }
    return sig;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "populateModuleTypes registers signatures" {
    const allocator = std.testing.allocator;
    var pool = TypePool.init(allocator);
    defer pool.deinit(allocator);

    var env = TypeEnv.init(allocator, &pool);
    defer env.deinit();

    populateModuleTypes(&env, &pool, allocator);

    // sha256 should be registered: (string) => string
    const sha256_sig = env.getFnSigByName("sha256");
    try std.testing.expect(sha256_sig != null);
    try std.testing.expectEqual(@as(u8, 1), sha256_sig.?.param_count);
    try std.testing.expectEqual(pool.idx_string, sha256_sig.?.param_types[0]);
    try std.testing.expectEqual(pool.idx_string, sha256_sig.?.return_type);

    // env should be registered: (string) => string | null
    const env_sig = env.getFnSigByName("env");
    try std.testing.expect(env_sig != null);
    try std.testing.expectEqual(@as(u8, 1), env_sig.?.param_count);
    // Return type should be nullable
    const ret_tag = pool.getTag(env_sig.?.return_type);
    try std.testing.expectEqual(type_pool_mod.TypeTag.t_nullable, ret_tag.?);
}
