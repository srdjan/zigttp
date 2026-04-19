//! Virtual Module Binding Specification
//!
//! A single comptime struct that captures every fact about a virtual module
//! needed by all consumers: resolver, type checker, verifier, bool checker,
//! contract builder, state manager, and trace/replay system.
//!
//! Built-in modules declare `pub const binding: ModuleBinding` alongside
//! their existing `exports` array. Third-party modules depend on zigttp-sdk
//! and write functions using ModuleFn (opaque handle) instead of NativeFn.
//!
//! The ModuleHandle opaque type provides a capability-based sandbox:
//! third-party modules cannot dereference the handle or access Context
//! internals. All interaction goes through free functions in this file.

const std = @import("std");
const object = @import("object.zig");
const value = @import("value.zig");
const context = @import("context.zig");
const compat = @import("compat.zig");
const file_io = @import("file_io.zig");
const sqlite_runtime = @import("sqlite.zig");
const resolver = @import("modules/internal/resolver.zig");
const security_events = @import("security_events.zig");

// Re-export EffectClass from resolver for backward compatibility
pub const EffectClass = resolver.EffectClass;

// -------------------------------------------------------------------------
// Opaque handle for third-party module sandbox
// -------------------------------------------------------------------------

/// Opaque handle passed to third-party module functions.
/// Cannot be dereferenced - modules interact with the runtime exclusively
/// through the free functions below. Internally this is a *Context, but
/// that fact is hidden from module authors.
pub const ModuleHandle = opaque {};

/// Function signature for third-party (sandboxed) module functions.
/// Receives an opaque handle instead of raw *anyopaque.
pub const ModuleFn = *const fn (
    handle: *ModuleHandle,
    this: value.JSValue,
    args: []const value.JSValue,
) anyerror!value.JSValue;

const ActiveModuleContext = struct {
    specifier: []const u8,
    required_capabilities: []const ModuleCapability,
};

pub const ActiveModuleToken = struct {
    previous: ?ActiveModuleContext,
};

threadlocal var active_module_context: ?ActiveModuleContext = null;
threadlocal var sdk_prng: ?std.Random.DefaultPrng = null;

/// Generate a NativeFn wrapper around a ModuleFn.
/// The wrapper casts *anyopaque to *ModuleHandle (a no-op pointer cast)
/// so the module receives the opaque handle it expects.
pub fn wrapModuleFn(comptime user_fn: ModuleFn) object.NativeFn {
    return wrapModuleFnWithCapabilities(user_fn, "<sandboxed>", &.{});
}

pub fn wrapNativeFnWithCapabilities(
    comptime user_fn: object.NativeFn,
    comptime specifier: []const u8,
    comptime required_capabilities: []const ModuleCapability,
) object.NativeFn {
    return struct {
        fn call(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
            const token = pushActiveModuleContext(specifier, required_capabilities);
            defer popActiveModuleContext(token);
            return user_fn(ctx_ptr, this, args);
        }
    }.call;
}

pub fn wrapModuleFnWithCapabilities(
    comptime user_fn: ModuleFn,
    comptime specifier: []const u8,
    comptime required_capabilities: []const ModuleCapability,
) object.NativeFn {
    return struct {
        fn call(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
            const token = pushActiveModuleContext(specifier, required_capabilities);
            defer popActiveModuleContext(token);
            return user_fn(@ptrCast(ctx_ptr), this, args);
        }
    }.call;
}

// -------------------------------------------------------------------------
// ModuleHandle free functions (the sandbox API)
// -------------------------------------------------------------------------

/// Cast a ModuleHandle to the underlying Context. Internal use only -
/// this function is called by the SDK free functions below, never by
/// module authors directly.
fn handleToContext(handle: *ModuleHandle) *context.Context {
    return @ptrCast(@alignCast(handle));
}

fn activeContextHasCapability(capability: ModuleCapability) bool {
    const active = active_module_context orelse return false;
    for (active.required_capabilities) |candidate| {
        if (candidate == capability) return true;
    }
    return false;
}

pub fn hasCapability(handle: *ModuleHandle, capability: ModuleCapability) bool {
    _ = handle;
    return activeContextHasCapability(capability);
}

pub const ModuleCapabilityError = error{MissingModuleCapability};

pub fn requireCapability(_: *ModuleHandle, capability: ModuleCapability) ModuleCapabilityError!void {
    if (activeContextHasCapability(capability)) return;
    return error.MissingModuleCapability;
}

pub fn pushActiveModuleContext(
    specifier: []const u8,
    required_capabilities: []const ModuleCapability,
) ActiveModuleToken {
    const prev = active_module_context;
    active_module_context = .{
        .specifier = specifier,
        .required_capabilities = required_capabilities,
    };
    return .{ .previous = prev };
}

pub fn popActiveModuleContext(token: ActiveModuleToken) void {
    active_module_context = token.previous;
}

/// Create a new JS string value. Ownership transfers to the GC.
pub fn createString(handle: *ModuleHandle, data: []const u8) !value.JSValue {
    const ctx = handleToContext(handle);
    return ctx.createString(data);
}

/// Extract a borrowed string slice from a JSValue.
/// The returned slice is valid only during the current function call.
pub fn extractString(val: value.JSValue) ?[]const u8 {
    const str = val.toStringStruct() orelse return null;
    return str.asSlice();
}

pub fn extractInt(val: value.JSValue) ?i32 {
    return val.toInt();
}

pub fn extractFloat(val: value.JSValue) ?f64 {
    return val.toFloat();
}

/// Create a Result object: { ok: true, value: payload }
pub fn resultOk(handle: *ModuleHandle, payload: value.JSValue) !value.JSValue {
    const ctx = handleToContext(handle);
    const util = @import("modules/internal/util.zig");
    return util.createPlainResultOk(ctx, payload);
}

/// Create a Result object: { ok: false, error: message }
pub fn resultErr(handle: *ModuleHandle, message: []const u8) !value.JSValue {
    const ctx = handleToContext(handle);
    const util = @import("modules/internal/util.zig");
    return util.createPlainResultErr(ctx, message);
}

/// Create a Result object: { ok: false, error: payload }
pub fn resultErrValue(handle: *ModuleHandle, payload: value.JSValue) !value.JSValue {
    const ctx = handleToContext(handle);
    const util = @import("modules/internal/util.zig");
    return util.createPlainResultErrValue(ctx, payload);
}

/// Create a Result object: { ok: false, errors: payload }
pub fn resultErrs(handle: *ModuleHandle, payload: value.JSValue) !value.JSValue {
    const ctx = handleToContext(handle);
    const util = @import("modules/internal/util.zig");
    return util.createPlainResultErrs(ctx, payload);
}

/// Throw a JS error. Sets ctx.exception and returns exception_val.
pub fn throwError(handle: *ModuleHandle, name: []const u8, message: []const u8) value.JSValue {
    const ctx = handleToContext(handle);
    const util = @import("modules/internal/util.zig");
    return util.throwError(ctx, name, message);
}

/// Get typed module state from a slot. Returns null if not initialized.
pub fn getState(handle: *ModuleHandle, comptime T: type, slot: usize) ?*T {
    const ctx = handleToContext(handle);
    return ctx.getModuleState(T, slot);
}

/// Set module state in a slot with a cleanup callback.
pub fn setState(
    handle: *ModuleHandle,
    slot: usize,
    ptr: *anyopaque,
    deinit_fn: *const fn (*anyopaque, std.mem.Allocator) void,
) void {
    const ctx = handleToContext(handle);
    ctx.setModuleState(slot, ptr, deinit_fn);
}

/// Get the runtime allocator for persistent allocations.
pub fn getAllocator(handle: *ModuleHandle) std.mem.Allocator {
    const ctx = handleToContext(handle);
    return ctx.allocator;
}

pub const ActiveCapabilityError = ModuleCapabilityError || error{
    ClockUnavailable,
    StderrWriteFailed,
};

fn currentActiveModuleSpecifier() []const u8 {
    return if (active_module_context) |active| active.specifier else "<no-active-module>";
}

fn requireActiveCapability(capability: ModuleCapability) ActiveCapabilityError!void {
    if (activeContextHasCapability(capability)) return;
    return error.MissingModuleCapability;
}

fn panicCapabilityError(err: ActiveCapabilityError, capability: ModuleCapability) noreturn {
    const spec = currentActiveModuleSpecifier();
    switch (err) {
        error.MissingModuleCapability => std.debug.panic(
            "module '{s}' used undeclared capability '{s}'",
            .{ spec, @tagName(capability) },
        ),
        error.ClockUnavailable => std.debug.panic(
            "module '{s}' failed to access clock capability",
            .{spec},
        ),
        error.StderrWriteFailed => std.debug.panic(
            "module '{s}' failed to write to stderr capability",
            .{spec},
        ),
    }
}

pub fn nowMsForActiveModule() ActiveCapabilityError!i64 {
    try requireActiveCapability(.clock);
    return compat.realtimeNowMs() catch error.ClockUnavailable;
}

pub fn nowNsForActiveModule() ActiveCapabilityError!u64 {
    try requireActiveCapability(.clock);
    return compat.realtimeNowNs() catch error.ClockUnavailable;
}

pub fn clockNowMsChecked() i64 {
    return nowMsForActiveModule() catch |err| panicCapabilityError(err, .clock);
}

pub fn clockNowNsChecked() u64 {
    return nowNsForActiveModule() catch |err| panicCapabilityError(err, .clock);
}

pub fn clockNowSecsChecked() i64 {
    return @divTrunc(clockNowMsChecked(), 1000);
}

pub fn fillRandomForActiveModule(buf: []u8) ActiveCapabilityError!void {
    try requireActiveCapability(.random);
    if (buf.len == 0) return;

    if (sdk_prng == null) {
        const seed = std.hash.Wyhash.hash(0, currentActiveModuleSpecifier()) ^
            @as(u64, @bitCast(nowNsForActiveModule() catch 0));
        sdk_prng = std.Random.DefaultPrng.init(seed);
    }

    var rng = sdk_prng.?.random();
    rng.bytes(buf);
}

pub fn fillRandomChecked(buf: []u8) void {
    fillRandomForActiveModule(buf) catch |err| panicCapabilityError(err, .random);
}

pub fn writeStderrForActiveModule(buf: []const u8) ActiveCapabilityError!void {
    try requireActiveCapability(.stderr);
    if (buf.len == 0) return;

    const written = std.c.write(std.c.STDERR_FILENO, buf.ptr, buf.len);
    if (written != @as(isize, @intCast(buf.len))) return error.StderrWriteFailed;
}

pub fn writeStderrChecked(buf: []const u8) void {
    writeStderrForActiveModule(buf) catch |err| panicCapabilityError(err, .stderr);
}

pub fn runtimeCallbackCapabilityChecked() void {
    requireActiveCapability(.runtime_callback) catch |err| panicCapabilityError(err, .runtime_callback);
}

pub fn getRuntimeCallbackStateChecked(
    ctx: *context.Context,
    comptime T: type,
    slot: usize,
) ?*T {
    runtimeCallbackCapabilityChecked();
    return ctx.getModuleState(T, slot);
}

pub fn readFileChecked(
    allocator: std.mem.Allocator,
    path: []const u8,
    max_size: usize,
) ![]u8 {
    requireActiveCapability(.filesystem) catch |err| panicCapabilityError(err, .filesystem);
    return file_io.readFile(allocator, path, max_size);
}

pub fn readEnvForActiveModule(name_z: [:0]const u8) ActiveCapabilityError!?[]const u8 {
    try requireActiveCapability(.env);
    const result = std.c.getenv(name_z) orelse return null;
    return std.mem.sliceTo(result, 0);
}

pub fn readEnvChecked(name_z: [:0]const u8) ?[]const u8 {
    return readEnvForActiveModule(name_z) catch |err| panicCapabilityError(err, .env);
}

pub fn sqliteCapabilityChecked() void {
    requireActiveCapability(.sqlite) catch |err| panicCapabilityError(err, .sqlite);
}

pub fn getSqliteStateChecked(
    ctx: *context.Context,
    comptime T: type,
    slot: usize,
) ?*T {
    sqliteCapabilityChecked();
    return ctx.getModuleState(T, slot);
}

pub fn openSqliteDbChecked(
    allocator: std.mem.Allocator,
    path: []const u8,
) !sqlite_runtime.Db {
    sqliteCapabilityChecked();
    return sqlite_runtime.Db.openReadWriteCreate(allocator, path);
}

pub fn hmacSha256ForActiveModule(
    out: *[std.crypto.auth.hmac.sha2.HmacSha256.mac_length]u8,
    data: []const u8,
    key: []const u8,
) ActiveCapabilityError!void {
    try requireActiveCapability(.crypto);
    std.crypto.auth.hmac.sha2.HmacSha256.create(out, data, key);
}

pub fn sha256ForActiveModule(
    out: *[std.crypto.hash.sha2.Sha256.digest_length]u8,
    data: []const u8,
) ActiveCapabilityError!void {
    try requireActiveCapability(.crypto);
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(data);
    out.* = hasher.finalResult();
}

pub fn sha256Checked(
    out: *[std.crypto.hash.sha2.Sha256.digest_length]u8,
    data: []const u8,
) void {
    sha256ForActiveModule(out, data) catch |err| panicCapabilityError(err, .crypto);
}

pub fn hmacSha256Checked(
    out: *[std.crypto.auth.hmac.sha2.HmacSha256.mac_length]u8,
    data: []const u8,
    key: []const u8,
) void {
    hmacSha256ForActiveModule(out, data, key) catch |err| panicCapabilityError(err, .crypto);
}

fn emitPolicyDenial(kind: security_events.SecurityEventKind, name: []const u8) void {
    security_events.emitGlobal(security_events.SecurityEvent.init(
        kind,
        currentActiveModuleSpecifier(),
        name,
    ));
}

pub fn allowsCacheNamespaceForActiveModule(
    ctx: *context.Context,
    ns: []const u8,
) ActiveCapabilityError!bool {
    try requireActiveCapability(.policy_check);
    const allowed = ctx.capability_policy.allowsCacheNamespace(ns);
    if (!allowed) emitPolicyDenial(.policy_denied_cache, ns);
    return allowed;
}

pub fn allowsCacheNamespaceChecked(ctx: *context.Context, ns: []const u8) bool {
    return allowsCacheNamespaceForActiveModule(ctx, ns) catch |err| panicCapabilityError(err, .policy_check);
}

pub fn allowsEnvForActiveModule(
    ctx: *context.Context,
    name: []const u8,
) ActiveCapabilityError!bool {
    try requireActiveCapability(.policy_check);
    const allowed = ctx.capability_policy.allowsEnv(name);
    if (!allowed) emitPolicyDenial(.policy_denied_env, name);
    return allowed;
}

pub fn allowsEnvChecked(ctx: *context.Context, name: []const u8) bool {
    return allowsEnvForActiveModule(ctx, name) catch |err| panicCapabilityError(err, .policy_check);
}

pub fn allowsSqlQueryForActiveModule(
    ctx: *context.Context,
    name: []const u8,
) ActiveCapabilityError!bool {
    try requireActiveCapability(.policy_check);
    const allowed = ctx.capability_policy.allowsSqlQuery(name);
    if (!allowed) emitPolicyDenial(.policy_denied_sql, name);
    return allowed;
}

pub fn allowsSqlQueryChecked(ctx: *context.Context, name: []const u8) bool {
    return allowsSqlQueryForActiveModule(ctx, name) catch |err| panicCapabilityError(err, .policy_check);
}

pub export fn zigttpSdkHasCapability(handle: *ModuleHandle, capability_tag: u8) bool {
    if (capability_tag > @intFromEnum(ModuleCapability.websocket)) return false;
    const capability: ModuleCapability = @enumFromInt(capability_tag);
    return hasCapability(handle, capability);
}

pub export fn zigttpSdkNowMs(handle: *ModuleHandle, out_ms: *i64) bool {
    _ = handle;
    out_ms.* = nowMsForActiveModule() catch return false;
    return true;
}

pub export fn zigttpSdkFillRandom(handle: *ModuleHandle, buf_ptr: [*]u8, len: usize) void {
    _ = handle;
    if (len == 0) return;
    fillRandomForActiveModule(buf_ptr[0..len]) catch {};
}

pub export fn zigttpSdkWriteStderr(handle: *ModuleHandle, buf_ptr: [*]const u8, len: usize) bool {
    _ = handle;
    if (len == 0) return true;
    writeStderrForActiveModule(buf_ptr[0..len]) catch return false;
    return true;
}

// -------------------------------------------------------------------------
// SDK bridge: handle-bound runtime operations
// -------------------------------------------------------------------------
//
// These exports let the zigttp-sdk package drive zigts runtime state
// through opaque ModuleHandle pointers. Each matches an `extern fn` on the
// SDK side. JSValue is transferred as u64 since both packages define it as
// packed struct(u64) and verify layout equivalence at comptime.

const util_mod = @import("modules/internal/util.zig");

pub export fn zigttpSdkExtractString(val_raw: u64, out_ptr: *[*]const u8, out_len: *usize) bool {
    const val: value.JSValue = .{ .raw = val_raw };
    const slice = util_mod.extractString(val) orelse return false;
    out_ptr.* = slice.ptr;
    out_len.* = slice.len;
    return true;
}

pub export fn zigttpSdkCreateString(handle: *ModuleHandle, ptr: [*]const u8, len: usize, out: *u64) bool {
    const ctx = handleToContext(handle);
    const result = ctx.createString(ptr[0..len]) catch return false;
    out.* = result.raw;
    return true;
}

pub export fn zigttpSdkCreateObject(handle: *ModuleHandle, out: *u64) bool {
    const ctx = handleToContext(handle);
    const obj = ctx.createObject(ctx.object_prototype) catch return false;
    out.* = obj.toValue().raw;
    return true;
}

pub export fn zigttpSdkObjectSet(
    handle: *ModuleHandle,
    obj_raw: u64,
    key_ptr: [*]const u8,
    key_len: usize,
    val_raw: u64,
) bool {
    const ctx = handleToContext(handle);
    const obj_val: value.JSValue = .{ .raw = obj_raw };
    if (!obj_val.isObject()) return false;
    const obj = obj_val.toPtr(object.JSObject);
    const atom = ctx.atoms.intern(key_ptr[0..key_len]) catch return false;
    ctx.setPropertyChecked(obj, atom, .{ .raw = val_raw }) catch return false;
    return true;
}

pub export fn zigttpSdkObjectGet(
    handle: *ModuleHandle,
    obj_raw: u64,
    key_ptr: [*]const u8,
    key_len: usize,
    out: *u64,
) bool {
    const ctx = handleToContext(handle);
    const obj_val: value.JSValue = .{ .raw = obj_raw };
    if (!obj_val.isObject()) return false;
    const obj = obj_val.toPtr(object.JSObject);
    const atom = ctx.atoms.intern(key_ptr[0..key_len]) catch return false;
    const pool = ctx.hidden_class_pool orelse return false;
    const val = obj.getProperty(pool, atom) orelse return false;
    out.* = val.raw;
    return true;
}

pub export fn zigttpSdkThrowError(
    handle: *ModuleHandle,
    name_ptr: [*]const u8,
    name_len: usize,
    msg_ptr: [*]const u8,
    msg_len: usize,
) u64 {
    const ctx = handleToContext(handle);
    const exc = util_mod.throwError(ctx, name_ptr[0..name_len], msg_ptr[0..msg_len]);
    return exc.raw;
}

pub export fn zigttpSdkResultOk(handle: *ModuleHandle, payload_raw: u64, out: *u64) bool {
    const ctx = handleToContext(handle);
    const result = util_mod.createPlainResultOk(ctx, .{ .raw = payload_raw }) catch return false;
    out.* = result.raw;
    return true;
}

pub export fn zigttpSdkResultErr(
    handle: *ModuleHandle,
    msg_ptr: [*]const u8,
    msg_len: usize,
    out: *u64,
) bool {
    const ctx = handleToContext(handle);
    const result = util_mod.createPlainResultErr(ctx, msg_ptr[0..msg_len]) catch return false;
    out.* = result.raw;
    return true;
}

pub export fn zigttpSdkResultErrValue(handle: *ModuleHandle, payload_raw: u64, out: *u64) bool {
    const ctx = handleToContext(handle);
    out.* = util_mod.createPlainResultErrValue(ctx, .{ .raw = payload_raw }).raw;
    return true;
}

pub export fn zigttpSdkResultErrs(handle: *ModuleHandle, payload_raw: u64, out: *u64) bool {
    const ctx = handleToContext(handle);
    const result = util_mod.createPlainResultErrs(ctx, .{ .raw = payload_raw }) catch return false;
    out.* = result.raw;
    return true;
}

pub export fn zigttpSdkGetAllocator(handle: *ModuleHandle) *anyopaque {
    const ctx = handleToContext(handle);
    return @constCast(&ctx.allocator);
}

pub export fn zigttpSdkSha256(
    handle: *ModuleHandle,
    data_ptr: [*]const u8,
    data_len: usize,
    out: [*]u8,
) bool {
    _ = handle;
    sha256ForActiveModule(@ptrCast(out[0..32]), data_ptr[0..data_len]) catch return false;
    return true;
}

pub export fn zigttpSdkHmacSha256(
    handle: *ModuleHandle,
    data_ptr: [*]const u8,
    data_len: usize,
    key_ptr: [*]const u8,
    key_len: usize,
    out: [*]u8,
) bool {
    _ = handle;
    hmacSha256ForActiveModule(@ptrCast(out[0..32]), data_ptr[0..data_len], key_ptr[0..key_len]) catch return false;
    return true;
}


// -------------------------------------------------------------------------
// Return type classification
// -------------------------------------------------------------------------

/// Unified return type for verification, type checking, and bool checking.
/// Each consumer maps this to its own internal representation.
pub const ReturnKind = enum {
    /// Plain value types - no caller-side check required
    boolean,
    number,
    string,
    object,
    undefined,
    unknown,

    /// Optional types - verifier requires narrowing before use
    optional_string,
    optional_object,

    /// Result type ({ok, value, error}) - verifier requires .ok check
    result,
};

// -------------------------------------------------------------------------
// Failure severity classification
// -------------------------------------------------------------------------

/// How the fault coverage checker treats a 2xx response on this function's
/// failure path. Derived from the function's domain semantics:
///   - critical: security/validation boundary - 2xx on failure is suspicious
///   - expected: cache miss or missing config - 2xx is normal (graceful degradation)
///   - upstream: external service failure - 2xx may be intentional (fallback)
///   - none: function cannot fail (returns plain value)
pub const FailureSeverity = enum {
    /// Auth or validation failure - 2xx on failure path is a warning.
    critical,
    /// Cache miss, missing env, route not matched - 2xx is fine.
    expected,
    /// External service failure (fetchSync) - 2xx is informational.
    upstream,
    /// Function always succeeds (returns plain value, not Result/optional).
    none,
};

// -------------------------------------------------------------------------
// Module implementation capability declarations
// -------------------------------------------------------------------------

/// Capabilities consumed by a virtual module's Zig implementation.
/// These are governance metadata for the module internals; they do not affect
/// handler-level effect classification or RuntimePolicy derivation.
pub const ModuleCapability = enum {
    env,
    clock,
    random,
    crypto,
    stderr,
    runtime_callback,
    sqlite,
    filesystem,
    network,
    policy_check,
    websocket,
};

pub const capability_count: usize = @typeInfo(ModuleCapability).@"enum".fields.len;

/// SHA-256 over a canonical capability list. Tag names are hashed in the
/// order given, newline-separated, so equal sets produce equal digests.
pub fn capabilityHash(caps: []const ModuleCapability) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    for (caps) |c| {
        hasher.update(@tagName(c));
        hasher.update("\n");
    }
    var out: [32]u8 = undefined;
    hasher.final(&out);
    return out;
}

// -------------------------------------------------------------------------
// Data provenance labels
// -------------------------------------------------------------------------

/// Classification of data sensitivity for compile-time information flow analysis.
/// The flow checker tracks these labels through the handler's data flow graph,
/// proving that sensitive data never reaches unauthorized sinks.
pub const DataLabel = enum(u3) {
    secret, // env vars with sensitive names (PASSWORD, KEY, TOKEN, SECRET, PRIVATE)
    credential, // auth tokens, JWT payloads, bearer tokens
    user_input, // request body, headers, query params, path params
    config, // non-secret env configuration
    internal, // cache values, internal state
    external, // data from fetchSync responses
    validated, // data that passed through validation
};

/// Bitset of data provenance labels. Propagates through operations via merge (OR).
/// Compact enough (1 byte) to store per-node in the flow checker's label map.
pub const LabelSet = packed struct(u8) {
    secret: bool = false,
    credential: bool = false,
    user_input: bool = false,
    config: bool = false,
    internal: bool = false,
    external: bool = false,
    validated: bool = false,
    _pad: u1 = 0,

    pub const empty: LabelSet = .{};

    /// Bitwise OR: union of two label sets.
    pub fn merge(a: LabelSet, b: LabelSet) LabelSet {
        const ai: u8 = @bitCast(a);
        const bi: u8 = @bitCast(b);
        return @bitCast(ai | bi);
    }

    /// Check if a specific label is present.
    pub fn has(self: LabelSet, label: DataLabel) bool {
        const bit: u3 = @intFromEnum(label);
        const mask: u8 = @as(u8, 1) << bit;
        const raw: u8 = @bitCast(self);
        return (raw & mask) != 0;
    }

    /// Check if any label in the mask is present.
    pub fn hasAny(self: LabelSet, mask: LabelSet) bool {
        const si: u8 = @bitCast(self);
        const mi: u8 = @bitCast(mask);
        return (si & mi) != 0;
    }

    /// True if no labels are set.
    pub fn isEmpty(self: LabelSet) bool {
        const raw: u8 = @bitCast(self);
        return (raw & 0x7F) == 0; // ignore pad bit
    }

    /// Create a LabelSet from a single label.
    pub fn fromLabel(label: DataLabel) LabelSet {
        const bit: u3 = @intFromEnum(label);
        const mask: u8 = @as(u8, 1) << bit;
        return @bitCast(mask);
    }
};

// -------------------------------------------------------------------------
// Contract extraction rules
// -------------------------------------------------------------------------

/// Category for extracted literals in the contract JSON.
pub const ContractCategory = enum {
    env,
    cache_namespace,
    sql_registration,
    scope_name,
    durable_key,
    durable_step,
    durable_signal,
    durable_producer_key,
    schema_compile,
    request_schema,
    route_pattern,
    cookie_name,
    cors_origin,
    rate_limit_key,
    service_call,
    fetch_host,
};

/// Transform applied to extracted literal before storing.
pub const ContractTransform = enum {
    /// Extract hostname from URL string.
    extract_host,
    /// No transform, use raw literal.
    identity,
};

/// Declarative rule for extracting a literal from a function argument.
pub const ContractExtraction = struct {
    /// Which argument position holds the literal (0-indexed).
    arg_position: u8 = 0,
    /// What category this literal belongs to in the contract.
    category: ContractCategory,
    /// Optional transform applied to the raw string.
    transform: ?ContractTransform = null,
    /// When true, the call sets a boolean flag rather than extracting a literal.
    flag_only: bool = false,
};

/// Boolean flags set on the contract when a function is imported or called.
pub const ContractFlags = struct {
    sets_scope_used: bool = false,
    sets_durable_used: bool = false,
    sets_durable_timers: bool = false,
    sets_bearer_auth: bool = false,
    sets_jwt_auth: bool = false,
};

// -------------------------------------------------------------------------
// Algebraic laws
// -------------------------------------------------------------------------

/// Tag identifying which algebraic law applies to a function.
/// Laws are the mechanism behind proven-equivalent deploys: the canonicalizer
/// walks `BehaviorPath.io_sequence` and applies rewrites justified by these
/// equations. Every variant here must be *unconditionally sound*, i.e. the
/// rewrite is valid regardless of surrounding context. Conditional laws
/// (commutation, reordering) are explicitly deferred until side-condition
/// machinery exists.
pub const LawKind = enum {
    pure,
    idempotent_call,
    inverse_of,
    absorbing,
};

/// An algebraic equation attached to a `FunctionBinding`.
///
/// - `.pure`: `f(args)` is a function of its arguments only. Two adjacent
///   calls with structurally-equal arguments collapse to one.
/// - `.idempotent_call`: `f(args); f(args)` has the same observable effect
///   as `f(args)`. The only law allowed on write-effect functions.
/// - `.inverse_of`: `g(f(x)) == x` where `g` is the named function.
///   The name is resolved against the full registry in `validateBindings`;
///   a dangling reference is a compile error.
/// - `.absorbing`: when an argument matches `AbsorbingPattern.argument_shape`,
///   the call is known to produce the fixed `residue` and can be folded.
pub const Law = union(LawKind) {
    pure: void,
    idempotent_call: void,
    inverse_of: []const u8,
    absorbing: AbsorbingPattern,
};

/// Describes a recognizable argument shape and the residue the function
/// produces when that shape appears. Used for dead-branch pruning during
/// path canonicalization (e.g. `jwtVerify("")` is always `.err`).
pub const AbsorbingPattern = struct {
    /// Which argument position the pattern matches on (0-indexed).
    arg_position: u8 = 0,
    /// The shape the argument must have for the residue to apply.
    argument_shape: ArgumentShape,
    /// The fixed residue produced when the shape matches.
    residue: Residue,

    pub const ArgumentShape = enum {
        empty_string_literal,
        undefined_literal,
    };

    pub const Residue = enum {
        result_err,
        returns_undefined,
        returns_false,
    };
};

// -------------------------------------------------------------------------
// Function binding
// -------------------------------------------------------------------------

/// Complete metadata for a single exported function.
pub const FunctionBinding = struct {
    /// Function name as visible from JS import.
    name: []const u8,

    /// Native implementation (for built-in modules using raw Context access).
    /// Exactly one of func or module_func must be set.
    func: ?object.NativeFn = null,

    /// Sandboxed implementation (for third-party modules using ModuleHandle).
    /// The registration system wraps this into a NativeFn automatically.
    module_func: ?ModuleFn = null,

    /// Argument count (for JS runtime).
    arg_count: u8,

    /// Effect classification for handler property derivation.
    effect: EffectClass = .read,

    /// Return type classification. Drives verifier, bool checker, and type checker.
    returns: ReturnKind = .unknown,

    /// Type signature for parameter types (mapped to TypeIndex during type init).
    param_types: []const ReturnKind = &.{},

    /// Whether trace/replay/durable should wrap this function.
    /// false for setup-only functions (schemaCompile, schemaDrop, sql register).
    traceable: bool = true,

    /// Contract extraction rules for this function's arguments.
    contract_extractions: []const ContractExtraction = &.{},

    /// Flags set on the contract when this function is called.
    contract_flags: ContractFlags = .{},

    /// Data provenance labels for this function's return value.
    /// Used by the flow checker to track sensitive data through the handler.
    return_labels: LabelSet = .{},

    /// Failure severity classification for fault coverage analysis.
    /// Determines how the fault coverage checker treats a 2xx response
    /// on this function's failure path.
    failure_severity: FailureSeverity = .none,

    /// Algebraic laws this function satisfies. Consumed by the behavior-path
    /// canonicalizer (see `behavior_canonical.zig`) to justify rewrites for
    /// proven-equivalent deploys. Every law must be unconditionally sound;
    /// `validateBindings` rejects laws that contradict the declared effect.
    laws: []const Law = &.{},

    /// Get the NativeFn for this binding, wrapping ModuleFn if needed.
    pub fn getNativeFn(comptime self: FunctionBinding) object.NativeFn {
        if (self.func) |f| return f;
        if (self.module_func) |mf| return wrapModuleFn(mf);
        @compileError("FunctionBinding must set either func or module_func");
    }

    /// Convert to a legacy ModuleExport for backward compatibility.
    pub fn toModuleExport(comptime self: FunctionBinding) resolver.ModuleExport {
        return .{
            .name = self.name,
            .func = self.getNativeFn(),
            .arg_count = self.arg_count,
            .effect = self.effect,
        };
    }
};

// -------------------------------------------------------------------------
// Module binding
// -------------------------------------------------------------------------

/// Complete declaration for a virtual module.
/// One per module - the single source of truth for all consumers.
pub const ModuleBinding = struct {
    /// Module specifier as used in JS imports: "zigttp:crypto", "zigttp:redis".
    specifier: []const u8,

    /// Short name for trace/replay JSON keys and enum references.
    name: []const u8,

    /// All exported functions with full metadata.
    exports: []const FunctionBinding,

    /// Runtime capabilities consumed by the module's Zig implementation.
    required_capabilities: []const ModuleCapability = &.{},

    /// Whether this module needs per-runtime state.
    stateful: bool = false,

    /// State initialization callback (called during runtime init).
    state_init: ?*const fn (*anyopaque, std.mem.Allocator) anyerror!void = null,

    /// State cleanup callback (called during context deinit).
    state_deinit: ?*const fn (*anyopaque, std.mem.Allocator) void = null,

    /// Contract section name for contract.json output.
    /// When non-null, the module gets its own top-level section.
    contract_section: ?[]const u8 = null,

    /// Whether the contract should feed into RuntimePolicy sandboxing.
    sandboxable: bool = false,

    /// Whether this module is compile-time only (skip trace/replay/durable).
    comptime_only: bool = false,

    /// Whether this module manages its own I/O wrapping (skip trace/replay/durable).
    /// Used by modules like durable that implement their own write-ahead logging.
    self_managed_io: bool = false,

    /// Generate a legacy exports array from this binding.
    pub fn toModuleExports(comptime self: ModuleBinding) [self.exports.len]resolver.ModuleExport {
        var result: [self.exports.len]resolver.ModuleExport = undefined;
        for (self.exports, 0..) |exp, i| {
            result[i] = exp.toModuleExport();
        }
        return result;
    }
};

// -------------------------------------------------------------------------
// Registry validation
// -------------------------------------------------------------------------

/// Validate a set of module bindings at compile time.
/// Produces clear compile errors for:
///   - duplicate specifiers
///   - duplicate function names across modules
///   - state lifecycle inconsistency (stateful without init/deinit)
///   - specifier format (must start with "zigttp:" or "zigttp-ext:")
///   - function bindings missing both func and module_func
pub fn validateBindings(comptime bindings: []const ModuleBinding) void {
    @setEvalBranchQuota(10000);
    // Check specifier format and state consistency per module
    for (bindings) |b| {
        const builtin_prefix = std.mem.startsWith(u8, b.specifier, "zigttp:");
        const extension_prefix = std.mem.startsWith(u8, b.specifier, "zigttp-ext:");
        if (!builtin_prefix and !extension_prefix) {
            @compileError("module specifier must start with 'zigttp:' or 'zigttp-ext:': " ++ b.specifier);
        }
        // state_init and state_deinit must be set together or not at all
        if (b.state_init != null and b.state_deinit == null) {
            @compileError("module has state_init but missing state_deinit: " ++ b.specifier);
        }
        if (b.state_init == null and b.state_deinit != null) {
            @compileError("module has state_deinit but missing state_init: " ++ b.specifier);
        }
        if (findDuplicateRequiredCapability(b.required_capabilities)) |capability| {
            @compileError("duplicate required capability '" ++ @tagName(capability) ++ "' in " ++ b.specifier);
        }
        for (b.exports) |f| {
            if (f.func == null and f.module_func == null) {
                @compileError("function binding missing both func and module_func: " ++ f.name);
            }
            if (f.laws.len > 0) {
                if (b.comptime_only) {
                    @compileError("laws are not allowed on comptime_only module '" ++ b.specifier ++ "' function '" ++ f.name ++ "'");
                }
                if (b.self_managed_io) {
                    @compileError("laws are not allowed on self_managed_io module '" ++ b.specifier ++ "' function '" ++ f.name ++ "'");
                }
                if (findDuplicateLawKind(f.laws)) |kind| {
                    @compileError("duplicate law '" ++ @tagName(kind) ++ "' on " ++ b.specifier ++ "." ++ f.name);
                }
                for (f.laws) |law| {
                    if (f.effect == .write and law != .idempotent_call) {
                        @compileError("only '.idempotent_call' may be declared on write-effect function " ++ b.specifier ++ "." ++ f.name ++ " (got '." ++ @tagName(std.meta.activeTag(law)) ++ "')");
                    }
                }
            }
        }
    }
    // Check unique specifiers
    for (bindings, 0..) |a, i| {
        for (bindings[i + 1 ..]) |b| {
            if (std.mem.eql(u8, a.specifier, b.specifier)) {
                @compileError("duplicate module specifier: " ++ a.specifier);
            }
        }
    }
    // Check unique function names across all modules
    for (bindings, 0..) |a, ai| {
        for (a.exports, 0..) |af, afi| {
            // Check within same module (later exports)
            for (a.exports[afi + 1 ..]) |af2| {
                if (std.mem.eql(u8, af.name, af2.name)) {
                    @compileError("duplicate function name within " ++ a.specifier ++ ": " ++ af.name);
                }
            }
            // Check across later modules
            for (bindings[ai + 1 ..]) |b| {
                for (b.exports) |bf| {
                    if (std.mem.eql(u8, af.name, bf.name)) {
                        @compileError("duplicate function name '" ++ af.name ++ "' in " ++ a.specifier ++ " and " ++ b.specifier);
                    }
                }
            }
        }
    }
    // Resolve .inverse_of references. The target name must refer to an
    // existing function somewhere in the registry, and the target must
    // declare the symmetric inverse so `g(f(x)) = x` and `f(g(x)) = x`
    // are both justified by paired declarations.
    for (bindings) |b| {
        for (b.exports) |f| {
            for (f.laws) |law| {
                switch (law) {
                    .inverse_of => |target_name| {
                        const target = findFunctionInRegistry(bindings, target_name) orelse {
                            @compileError("inverse_of target '" ++ target_name ++ "' not found for " ++ b.specifier ++ "." ++ f.name);
                        };
                        if (!hasInverseLawPointingTo(target.laws, f.name)) {
                            @compileError("inverse_of must be declared symmetrically: " ++
                                b.specifier ++ "." ++ f.name ++ " declares inverse_of=" ++
                                target_name ++ " but " ++ target_name ++ " does not declare inverse_of=" ++ f.name);
                        }
                    },
                    else => {},
                }
            }
        }
    }
}

fn findDuplicateRequiredCapability(comptime capabilities: []const ModuleCapability) ?ModuleCapability {
    for (capabilities, 0..) |capability, i| {
        for (capabilities[i + 1 ..]) |other| {
            if (capability == other) return capability;
        }
    }
    return null;
}

/// Return the first duplicated law *kind* within a single function's laws
/// list. Duplicates are always a spec error: laws are unconditionally sound
/// and idempotent, so declaring `[pure, pure]` or `[inverse_of "a", inverse_of "b"]`
/// signals confusion, not intent.
pub fn findDuplicateLawKind(comptime laws: []const Law) ?LawKind {
    for (laws, 0..) |law, i| {
        const kind = std.meta.activeTag(law);
        for (laws[i + 1 ..]) |other| {
            if (std.meta.activeTag(other) == kind) return kind;
        }
    }
    return null;
}

/// Find a function binding by name across an entire registry. Used to
/// resolve `.inverse_of` targets at comptime.
pub fn findFunctionInRegistry(
    comptime bindings: []const ModuleBinding,
    comptime name: []const u8,
) ?FunctionBinding {
    for (bindings) |b| {
        for (b.exports) |f| {
            if (std.mem.eql(u8, f.name, name)) return f;
        }
    }
    return null;
}

/// Check whether a laws list contains an `.inverse_of` pointing at `name`.
pub fn hasInverseLawPointingTo(
    comptime laws: []const Law,
    comptime name: []const u8,
) bool {
    for (laws) |law| {
        switch (law) {
            .inverse_of => |target| {
                if (std.mem.eql(u8, target, name)) return true;
            },
            else => {},
        }
    }
    return false;
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

test "FunctionBinding toModuleExport preserves fields" {
    const fb = FunctionBinding{
        .name = "testFn",
        .func = struct {
            fn f(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
                return value.JSValue.undefined_val;
            }
        }.f,
        .arg_count = 2,
        .effect = .write,
        .returns = .boolean,
    };

    const me = comptime fb.toModuleExport();
    try std.testing.expectEqualStrings("testFn", me.name);
    try std.testing.expectEqual(@as(u8, 2), me.arg_count);
    try std.testing.expectEqual(EffectClass.write, me.effect);
}

test "ModuleBinding toModuleExports generates correct array" {
    const binding = ModuleBinding{
        .specifier = "zigttp:test",
        .name = "test",
        .exports = &.{
            .{ .name = "fn1", .func = struct {
                fn f(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
                    return value.JSValue.undefined_val;
                }
            }.f, .arg_count = 1 },
            .{ .name = "fn2", .func = struct {
                fn f(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
                    return value.JSValue.false_val;
                }
            }.f, .arg_count = 0, .effect = .write },
        },
    };

    const exports = comptime binding.toModuleExports();
    try std.testing.expectEqual(@as(usize, 2), exports.len);
    try std.testing.expectEqualStrings("fn1", exports[0].name);
    try std.testing.expectEqualStrings("fn2", exports[1].name);
    try std.testing.expectEqual(EffectClass.read, exports[0].effect);
    try std.testing.expectEqual(EffectClass.write, exports[1].effect);
}

test "wrapModuleFn generates valid NativeFn" {
    const module_fn: ModuleFn = struct {
        fn f(_: *ModuleHandle, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
            return value.JSValue.true_val;
        }
    }.f;

    const native_fn = comptime wrapModuleFn(module_fn);
    // Verify the wrapper compiles and has the right type
    const ptr_info = @typeInfo(@TypeOf(native_fn));
    try std.testing.expect(ptr_info == .pointer);
}

test "FunctionBinding with module_func wraps correctly" {
    const fb = FunctionBinding{
        .name = "sandboxedFn",
        .module_func = struct {
            fn f(_: *ModuleHandle, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
                return value.JSValue.true_val;
            }
        }.f,
        .arg_count = 1,
    };

    const me = comptime fb.toModuleExport();
    try std.testing.expectEqualStrings("sandboxedFn", me.name);
    try std.testing.expectEqual(@typeInfo(object.NativeFn), @typeInfo(@TypeOf(me.func)));
}

test "LabelSet merge combines labels" {
    const a = LabelSet{ .secret = true };
    const b = LabelSet{ .credential = true };
    const merged = LabelSet.merge(a, b);
    try std.testing.expect(merged.secret);
    try std.testing.expect(merged.credential);
    try std.testing.expect(!merged.user_input);
}

test "LabelSet has checks specific label" {
    const labels = LabelSet{ .secret = true, .user_input = true };
    try std.testing.expect(labels.has(.secret));
    try std.testing.expect(labels.has(.user_input));
    try std.testing.expect(!labels.has(.credential));
    try std.testing.expect(!labels.has(.config));
}

test "LabelSet hasAny checks mask intersection" {
    const labels = LabelSet{ .config = true, .internal = true };
    const sensitive = LabelSet{ .secret = true, .credential = true };
    const config_mask = LabelSet{ .config = true };
    try std.testing.expect(!labels.hasAny(sensitive));
    try std.testing.expect(labels.hasAny(config_mask));
}

test "LabelSet isEmpty" {
    try std.testing.expect(LabelSet.empty.isEmpty());
    try std.testing.expect(!(LabelSet{ .secret = true }).isEmpty());
}

test "LabelSet fromLabel" {
    const label = LabelSet.fromLabel(.credential);
    try std.testing.expect(label.credential);
    try std.testing.expect(!label.secret);
}

test "FunctionBinding return_labels defaults to empty" {
    const fb = FunctionBinding{
        .name = "test",
        .func = struct {
            fn f(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
                return value.JSValue.undefined_val;
            }
        }.f,
        .arg_count = 0,
    };
    try std.testing.expect(fb.return_labels.isEmpty());
}

test "FunctionBinding failure_severity defaults to none" {
    const fb = FunctionBinding{
        .name = "test",
        .func = struct {
            fn f(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
                return value.JSValue.undefined_val;
            }
        }.f,
        .arg_count = 0,
    };
    try std.testing.expectEqual(FailureSeverity.none, fb.failure_severity);
}

test "ModuleBinding required_capabilities defaults to empty" {
    const binding = ModuleBinding{
        .specifier = "zigttp:test",
        .name = "test",
        .exports = &.{},
    };

    try std.testing.expectEqual(@as(usize, 0), binding.required_capabilities.len);
}

test "findDuplicateRequiredCapability returns duplicate capability" {
    const duplicate = comptime findDuplicateRequiredCapability(&.{ .clock, .crypto, .clock });
    try std.testing.expectEqual(ModuleCapability.clock, duplicate.?);
}

test "findDuplicateRequiredCapability ignores unique capabilities" {
    const duplicate = comptime findDuplicateRequiredCapability(&.{ .clock, .crypto, .policy_check });
    try std.testing.expect(duplicate == null);
}

test "requireCapability respects active module context" {
    const handle: *ModuleHandle = @ptrFromInt(0x1);

    const token = pushActiveModuleContext("zigttp-ext:test", &.{ .clock, .stderr });
    defer popActiveModuleContext(token);

    try requireCapability(handle, .clock);
    try std.testing.expect(hasCapability(handle, .stderr));
    try std.testing.expectError(ModuleCapabilityError.MissingModuleCapability, requireCapability(handle, .random));
}

test "active module helpers enforce declared capabilities" {
    const token = pushActiveModuleContext("zigttp:test", &.{ .clock, .random, .stderr, .crypto });
    defer popActiveModuleContext(token);

    const now_ms = try nowMsForActiveModule();
    try std.testing.expect(now_ms >= 0);

    var random_bytes: [8]u8 = undefined;
    try fillRandomForActiveModule(&random_bytes);

    try writeStderrForActiveModule("");

    var digest: [std.crypto.hash.sha2.Sha256.digest_length]u8 = undefined;
    try sha256ForActiveModule(&digest, "data");

    var mac: [std.crypto.auth.hmac.sha2.HmacSha256.mac_length]u8 = undefined;
    try hmacSha256ForActiveModule(&mac, "data", "key");
}

test "active module helpers reject missing capabilities" {
    const token = pushActiveModuleContext("zigttp:test", &.{.clock});
    defer popActiveModuleContext(token);

    var bytes: [8]u8 = undefined;
    try std.testing.expectError(ModuleCapabilityError.MissingModuleCapability, fillRandomForActiveModule(&bytes));
}

test "FunctionBinding laws defaults to empty" {
    const fb = FunctionBinding{
        .name = "test",
        .func = struct {
            fn f(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
                return value.JSValue.undefined_val;
            }
        }.f,
        .arg_count = 0,
    };
    try std.testing.expectEqual(@as(usize, 0), fb.laws.len);
}

test "findDuplicateLawKind detects repeats" {
    const laws_dup = [_]Law{ .pure, .pure };
    try std.testing.expectEqual(LawKind.pure, findDuplicateLawKind(&laws_dup).?);

    const laws_ok = [_]Law{ .pure, .idempotent_call };
    try std.testing.expect(findDuplicateLawKind(&laws_ok) == null);

    const laws_mixed = [_]Law{
        .{ .inverse_of = "decode" },
        .{ .inverse_of = "other" },
    };
    try std.testing.expectEqual(LawKind.inverse_of, findDuplicateLawKind(&laws_mixed).?);
}

test "findDuplicateLawKind returns null for empty list" {
    const laws = [_]Law{};
    try std.testing.expect(findDuplicateLawKind(&laws) == null);
}

test "hasInverseLawPointingTo matches by target name" {
    const laws = [_]Law{
        .pure,
        .{ .inverse_of = "base64Decode" },
    };
    try std.testing.expect(hasInverseLawPointingTo(&laws, "base64Decode"));
    try std.testing.expect(!hasInverseLawPointingTo(&laws, "base64Encode"));
    try std.testing.expect(!hasInverseLawPointingTo(&laws, ""));
}

test "findFunctionInRegistry locates binding across modules" {
    const dummy = struct {
        fn f(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
            return value.JSValue.undefined_val;
        }
    }.f;

    const bindings = [_]ModuleBinding{
        .{
            .specifier = "zigttp:a",
            .name = "a",
            .exports = &.{.{ .name = "alpha", .func = dummy, .arg_count = 0 }},
        },
        .{
            .specifier = "zigttp:b",
            .name = "b",
            .exports = &.{.{ .name = "beta", .func = dummy, .arg_count = 0 }},
        },
    };

    try std.testing.expect(findFunctionInRegistry(&bindings, "alpha") != null);
    try std.testing.expect(findFunctionInRegistry(&bindings, "beta") != null);
    try std.testing.expect(findFunctionInRegistry(&bindings, "gamma") == null);
}

test "AbsorbingPattern fields set correctly" {
    const pat = AbsorbingPattern{
        .arg_position = 1,
        .argument_shape = .empty_string_literal,
        .residue = .result_err,
    };
    try std.testing.expectEqual(@as(u8, 1), pat.arg_position);
    try std.testing.expectEqual(AbsorbingPattern.ArgumentShape.empty_string_literal, pat.argument_shape);
    try std.testing.expectEqual(AbsorbingPattern.Residue.result_err, pat.residue);
}

test "Law union carries inverse_of payload" {
    const law: Law = .{ .inverse_of = "decode" };
    switch (law) {
        .inverse_of => |target| try std.testing.expectEqualStrings("decode", target),
        else => try std.testing.expect(false),
    }
}

test "validateBindings accepts paired inverse_of laws" {
    const dummy = struct {
        fn f(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
            return value.JSValue.undefined_val;
        }
    }.f;

    comptime {
        const bindings = [_]ModuleBinding{.{
            .specifier = "zigttp:codec",
            .name = "codec",
            .exports = &.{
                .{
                    .name = "encode",
                    .func = dummy,
                    .arg_count = 1,
                    .effect = .none,
                    .laws = &.{ .pure, .{ .inverse_of = "decode" } },
                },
                .{
                    .name = "decode",
                    .func = dummy,
                    .arg_count = 1,
                    .effect = .none,
                    .laws = &.{ .pure, .{ .inverse_of = "encode" } },
                },
            },
        }};
        validateBindings(&bindings);
    }
}

test "validateBindings accepts idempotent_call on write-effect function" {
    const dummy = struct {
        fn f(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
            return value.JSValue.undefined_val;
        }
    }.f;

    comptime {
        const bindings = [_]ModuleBinding{.{
            .specifier = "zigttp:kv",
            .name = "kv",
            .exports = &.{
                .{
                    .name = "kvSet",
                    .func = dummy,
                    .arg_count = 2,
                    .effect = .write,
                    .laws = &.{.idempotent_call},
                },
            },
        }};
        validateBindings(&bindings);
    }
}

test "wrapNativeFnWithCapabilities activates context for built-in native fns" {
    const wrapped = comptime wrapNativeFnWithCapabilities(
        struct {
            fn f(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
                _ = clockNowMsChecked();
                return value.JSValue.true_val;
            }
        }.f,
        "zigttp:test",
        &.{.clock},
    );

    const result = try wrapped(@ptrFromInt(0x1), value.JSValue.undefined_val, &.{});
    try std.testing.expect(result.isTrue());
}
