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
const resolver = @import("modules/resolver.zig");

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
    const util = @import("modules/util.zig");
    return util.createPlainResultOk(ctx, payload);
}

/// Create a Result object: { ok: false, error: message }
pub fn resultErr(handle: *ModuleHandle, message: []const u8) !value.JSValue {
    const ctx = handleToContext(handle);
    const util = @import("modules/util.zig");
    return util.createPlainResultErr(ctx, message);
}

/// Create a Result object: { ok: false, error: payload }
pub fn resultErrValue(handle: *ModuleHandle, payload: value.JSValue) !value.JSValue {
    const ctx = handleToContext(handle);
    const util = @import("modules/util.zig");
    return util.createPlainResultErrValue(ctx, payload);
}

/// Create a Result object: { ok: false, errors: payload }
pub fn resultErrs(handle: *ModuleHandle, payload: value.JSValue) !value.JSValue {
    const ctx = handleToContext(handle);
    const util = @import("modules/util.zig");
    return util.createPlainResultErrs(ctx, payload);
}

/// Throw a JS error. Sets ctx.exception and returns exception_val.
pub fn throwError(handle: *ModuleHandle, name: []const u8, message: []const u8) value.JSValue {
    const ctx = handleToContext(handle);
    const util = @import("modules/util.zig");
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

pub fn allowsCacheNamespaceForActiveModule(
    ctx: *context.Context,
    ns: []const u8,
) ActiveCapabilityError!bool {
    try requireActiveCapability(.policy_check);
    return ctx.capability_policy.allowsCacheNamespace(ns);
}

pub fn allowsCacheNamespaceChecked(ctx: *context.Context, ns: []const u8) bool {
    return allowsCacheNamespaceForActiveModule(ctx, ns) catch |err| panicCapabilityError(err, .policy_check);
}

pub fn allowsEnvForActiveModule(
    ctx: *context.Context,
    name: []const u8,
) ActiveCapabilityError!bool {
    try requireActiveCapability(.policy_check);
    return ctx.capability_policy.allowsEnv(name);
}

pub fn allowsEnvChecked(ctx: *context.Context, name: []const u8) bool {
    return allowsEnvForActiveModule(ctx, name) catch |err| panicCapabilityError(err, .policy_check);
}

pub fn allowsSqlQueryForActiveModule(
    ctx: *context.Context,
    name: []const u8,
) ActiveCapabilityError!bool {
    try requireActiveCapability(.policy_check);
    return ctx.capability_policy.allowsSqlQuery(name);
}

pub fn allowsSqlQueryChecked(ctx: *context.Context, name: []const u8) bool {
    return allowsSqlQueryForActiveModule(ctx, name) catch |err| panicCapabilityError(err, .policy_check);
}

pub export fn zigttpSdkHasCapability(handle: *ModuleHandle, capability_tag: u8) bool {
    if (capability_tag > @intFromEnum(ModuleCapability.policy_check)) return false;
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
};

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
    sets_durable_used: bool = false,
    sets_durable_timers: bool = false,
    sets_bearer_auth: bool = false,
    sets_jwt_auth: bool = false,
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
}

fn findDuplicateRequiredCapability(comptime capabilities: []const ModuleCapability) ?ModuleCapability {
    for (capabilities, 0..) |capability, i| {
        for (capabilities[i + 1 ..]) |other| {
            if (capability == other) return capability;
        }
    }
    return null;
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
