const std = @import("std");
const sdk = @import("zigttp-sdk");

var test_allocator = std.testing.allocator;
var module_states = [_]?*anyopaque{null} ** 128;

const sqlite_error = "sqlite unavailable in sdk test shim";

// Capability denial bitmask consulted by `zigttpSdkHasCapability`. Default
// is 0 (all capabilities allowed) so existing tests written against the
// permissive shim keep passing unchanged. Tests that need to exercise a
// rejection path call `denyCapability` and either pair it with `defer
// allowCapability(...)` or `defer allowAllCapabilities()` to reset.
//
// 64 bits is sufficient for the current ModuleCapability enum (well under
// 32 variants). The mask is process-global, not thread-local, because
// Zig's default test runner executes tests serially on one thread.
var denied_capabilities_mask: u64 = 0;

pub fn denyCapability(capability: sdk.ModuleCapability) void {
    const tag: u6 = @intCast(@intFromEnum(capability));
    denied_capabilities_mask |= (@as(u64, 1) << tag);
}

pub fn allowCapability(capability: sdk.ModuleCapability) void {
    const tag: u6 = @intCast(@intFromEnum(capability));
    denied_capabilities_mask &= ~(@as(u64, 1) << tag);
}

pub fn allowAllCapabilities() void {
    denied_capabilities_mask = 0;
}

pub export fn zigttpSdkHasCapability(_: *sdk.ModuleHandle, capability_tag: u8) bool {
    // Tags outside the mask's range fall back to "allowed" — matches the
    // original shim's behavior and avoids spuriously failing tests that
    // probe capability values the enum does not yet define.
    if (capability_tag >= 64) return true;
    const bit: u6 = @intCast(capability_tag);
    return (denied_capabilities_mask & (@as(u64, 1) << bit)) == 0;
}

pub export fn zigttpSdkNowMs(_: *sdk.ModuleHandle, out_ms: *i64) bool {
    out_ms.* = 0;
    return true;
}

pub export fn zigttpSdkFillRandom(_: *sdk.ModuleHandle, buf_ptr: [*]u8, len: usize) void {
    @memset(buf_ptr[0..len], 0);
}

pub export fn zigttpSdkWriteStderr(_: *sdk.ModuleHandle, _: [*]const u8, _: usize) bool {
    return true;
}

pub export fn zigttpSdkExtractString(_: sdk.JSValue, _: *[*]const u8, _: *usize) bool {
    return false;
}

pub export fn zigttpSdkCreateString(_: *sdk.ModuleHandle, _: [*]const u8, _: usize, out: *sdk.JSValue) bool {
    out.* = sdk.JSValue.undefined_val;
    return true;
}

pub export fn zigttpSdkCreateObject(_: *sdk.ModuleHandle, out: *sdk.JSValue) bool {
    out.* = sdk.JSValue.undefined_val;
    return true;
}

pub export fn zigttpSdkObjectSet(_: *sdk.ModuleHandle, _: sdk.JSValue, _: [*]const u8, _: usize, _: sdk.JSValue) bool {
    return true;
}

pub export fn zigttpSdkObjectGet(_: *sdk.ModuleHandle, _: sdk.JSValue, _: [*]const u8, _: usize, _: *sdk.JSValue) bool {
    return false;
}

pub export fn zigttpSdkThrowError(_: *sdk.ModuleHandle, _: [*]const u8, _: usize, _: [*]const u8, _: usize) sdk.JSValue {
    return sdk.JSValue.undefined_val;
}

pub export fn zigttpSdkResultOk(_: *sdk.ModuleHandle, payload: sdk.JSValue, out: *sdk.JSValue) bool {
    out.* = payload;
    return true;
}

pub export fn zigttpSdkResultErr(_: *sdk.ModuleHandle, _: [*]const u8, _: usize, out: *sdk.JSValue) bool {
    out.* = sdk.JSValue.undefined_val;
    return true;
}

pub export fn zigttpSdkResultErrValue(_: *sdk.ModuleHandle, payload: sdk.JSValue, out: *sdk.JSValue) bool {
    out.* = payload;
    return true;
}

pub export fn zigttpSdkResultErrs(_: *sdk.ModuleHandle, payload: sdk.JSValue, out: *sdk.JSValue) bool {
    out.* = payload;
    return true;
}

pub export fn zigttpSdkGetAllocator(_: *sdk.ModuleHandle) *const std.mem.Allocator {
    return &test_allocator;
}

pub export fn zigttpSdkSha256(data_ptr: [*]const u8, data_len: usize, out: [*]u8) bool {
    std.crypto.hash.sha2.Sha256.hash(data_ptr[0..data_len], out[0..32], .{});
    return true;
}

pub export fn zigttpSdkHmacSha256(_: [*]const u8, _: usize, _: [*]const u8, _: usize, out: [*]u8) bool {
    @memset(out[0..32], 0);
    return true;
}

pub export fn zigttpSdkParseJson(_: *sdk.ModuleHandle, _: [*]const u8, _: usize, out: *sdk.JSValue) bool {
    out.* = sdk.JSValue.undefined_val;
    return true;
}

pub export fn zigttpSdkGetModuleState(_: *sdk.ModuleHandle, slot: usize) ?*anyopaque {
    if (slot >= module_states.len) return null;
    return module_states[slot];
}

pub export fn zigttpSdkSetModuleState(_: *sdk.ModuleHandle, slot: usize, ptr: *anyopaque, _: sdk.StateDeinitFn) bool {
    if (slot >= module_states.len) return false;
    module_states[slot] = ptr;
    return true;
}

pub export fn zigttpSdkIsString(_: sdk.JSValue) bool {
    return false;
}

pub export fn zigttpSdkIsObject(_: sdk.JSValue) bool {
    return false;
}

pub export fn zigttpSdkIsArray(_: sdk.JSValue) bool {
    return false;
}

pub export fn zigttpSdkIsCallable(_: sdk.JSValue) bool {
    return false;
}

pub export fn zigttpSdkArrayLength(_: sdk.JSValue, out: *u32) bool {
    out.* = 0;
    return true;
}

pub export fn zigttpSdkArrayGet(_: *sdk.ModuleHandle, _: sdk.JSValue, _: u32, _: *sdk.JSValue) bool {
    return false;
}

pub export fn zigttpSdkArraySet(_: *sdk.ModuleHandle, _: sdk.JSValue, _: u32, _: sdk.JSValue) bool {
    return true;
}

pub export fn zigttpSdkCreateArray(_: *sdk.ModuleHandle, out: *sdk.JSValue) bool {
    out.* = sdk.JSValue.undefined_val;
    return true;
}

pub export fn zigttpSdkStringify(_: *sdk.ModuleHandle, val: sdk.JSValue, out: *sdk.JSValue) bool {
    out.* = val;
    return true;
}

pub export fn zigttpSdkObjectKeys(_: *sdk.ModuleHandle, _: sdk.JSValue, out: *sdk.JSValue) bool {
    out.* = sdk.JSValue.undefined_val;
    return true;
}

pub export fn zigttpSdkReadEnv(_: *sdk.ModuleHandle, _: [*]const u8, _: usize, _: *[*]const u8, _: *usize) bool {
    return false;
}

pub export fn zigttpSdkAllowsEnv(_: *sdk.ModuleHandle, _: [*]const u8, _: usize) bool {
    return true;
}

pub export fn zigttpSdkAllowsCacheNamespace(_: *sdk.ModuleHandle, _: [*]const u8, _: usize) bool {
    return true;
}

pub export fn zigttpSdkReadFile(_: *sdk.ModuleHandle, _: [*]const u8, _: usize, _: usize, _: *[*]u8, _: *usize) bool {
    return false;
}

pub export fn zigttpSdkAllowsSqlQuery(_: *sdk.ModuleHandle, _: [*]const u8, _: usize) bool {
    return true;
}

pub export fn zigttpSdkAllowsSqlWrite(_: *sdk.ModuleHandle, _: [*]const u8, _: usize) bool {
    return true;
}

pub export fn zigttpSdkArrayPush(_: *sdk.ModuleHandle, _: sdk.JSValue, _: sdk.JSValue) bool {
    return true;
}

pub export fn zigttpSdkSqliteOpen(_: *sdk.ModuleHandle, _: [*]const u8, _: usize, _: **sdk.SqliteDb) bool {
    return false;
}

pub export fn zigttpSdkSqliteClose(_: *sdk.SqliteDb) void {}

pub export fn zigttpSdkSqliteChanges(_: *sdk.SqliteDb) i32 {
    return 0;
}

pub export fn zigttpSdkSqliteLastInsertRowId(_: *sdk.SqliteDb) i64 {
    return 0;
}

pub export fn zigttpSdkSqliteErrmsg(_: *sdk.SqliteDb, out_ptr: *[*]const u8, out_len: *usize) void {
    out_ptr.* = sqlite_error.ptr;
    out_len.* = sqlite_error.len;
}

pub export fn zigttpSdkSqlitePrepare(_: *sdk.SqliteDb, _: [*]const u8, _: usize, _: **sdk.SqliteStmt) bool {
    return false;
}

pub export fn zigttpSdkSqliteFinalize(_: *sdk.SqliteStmt) void {}

pub export fn zigttpSdkSqliteStep(_: *sdk.SqliteStmt) i32 {
    return sdk.sqlite_done;
}

pub export fn zigttpSdkSqliteReadonly(_: *sdk.SqliteStmt) bool {
    return true;
}

pub export fn zigttpSdkSqliteStmtErrmsg(_: *sdk.SqliteStmt, out_ptr: *[*]const u8, out_len: *usize) void {
    out_ptr.* = sqlite_error.ptr;
    out_len.* = sqlite_error.len;
}

pub export fn zigttpSdkSqliteParamCount(_: *sdk.SqliteStmt) u32 {
    return 0;
}

pub export fn zigttpSdkSqliteParamName(_: *sdk.SqliteStmt, _: u32, _: *[*]const u8, _: *usize) bool {
    return false;
}

pub export fn zigttpSdkSqliteBindNull(_: *sdk.SqliteStmt, _: u32) bool {
    return true;
}

pub export fn zigttpSdkSqliteBindInt64(_: *sdk.SqliteStmt, _: u32, _: i64) bool {
    return true;
}

pub export fn zigttpSdkSqliteBindDouble(_: *sdk.SqliteStmt, _: u32, _: f64) bool {
    return true;
}

pub export fn zigttpSdkSqliteBindText(_: *sdk.SqliteStmt, _: u32, _: [*]const u8, _: usize) bool {
    return true;
}

pub export fn zigttpSdkSqliteColumnCount(_: *sdk.SqliteStmt) u32 {
    return 0;
}

pub export fn zigttpSdkSqliteColumnName(_: *sdk.SqliteStmt, _: u32, out_ptr: *[*]const u8, out_len: *usize) void {
    out_ptr.* = sqlite_error.ptr;
    out_len.* = 0;
}

pub export fn zigttpSdkSqliteColumnType(_: *sdk.SqliteStmt, _: u32) i32 {
    return sdk.sqlite_null;
}

pub export fn zigttpSdkSqliteColumnInt64(_: *sdk.SqliteStmt, _: u32) i64 {
    return 0;
}

pub export fn zigttpSdkSqliteColumnDouble(_: *sdk.SqliteStmt, _: u32) f64 {
    return 0;
}

pub export fn zigttpSdkSqliteColumnText(_: *sdk.SqliteStmt, _: u32, out_ptr: *[*]const u8, out_len: *usize) void {
    out_ptr.* = sqlite_error.ptr;
    out_len.* = 0;
}

// ---------------------------------------------------------------------------
// Tests for the capability-denial wiring. Before these helpers existed, the
// shim's `zigttpSdkHasCapability` always returned true, so no SDK or modules
// test could observe a denial — the entire negative path of
// `requireCapability` was structurally unreachable in CI. The test below
// pins the round-trip: deny via the shim's mask, call through
// `sdk.requireCapability`, observe the `MissingModuleCapability` error.
// ---------------------------------------------------------------------------

test "denyCapability is observable through sdk.requireCapability" {
    allowAllCapabilities();
    defer allowAllCapabilities();

    // ModuleHandle is opaque; the shim's hasCapability ignores it.
    const fake_handle: *sdk.ModuleHandle = @ptrFromInt(8);

    // Default state: every capability is allowed, so the call returns void.
    try sdk.requireCapability(fake_handle, .crypto);

    // After denying crypto, the same call must surface the documented error.
    denyCapability(.crypto);
    try std.testing.expectError(
        error.MissingModuleCapability,
        sdk.requireCapability(fake_handle, .crypto),
    );

    // Other capabilities remain unaffected; denials are bit-precise.
    try sdk.requireCapability(fake_handle, .clock);

    // Re-allowing crypto restores the default behavior.
    allowCapability(.crypto);
    try sdk.requireCapability(fake_handle, .crypto);
}

test "allowAllCapabilities clears every prior denial" {
    allowAllCapabilities();
    defer allowAllCapabilities();

    denyCapability(.crypto);
    denyCapability(.clock);
    denyCapability(.network);

    allowAllCapabilities();

    const fake_handle: *sdk.ModuleHandle = @ptrFromInt(8);
    try sdk.requireCapability(fake_handle, .crypto);
    try sdk.requireCapability(fake_handle, .clock);
    try sdk.requireCapability(fake_handle, .network);
}

test "denials are independent across capabilities" {
    allowAllCapabilities();
    defer allowAllCapabilities();

    denyCapability(.clock);

    const fake_handle: *sdk.ModuleHandle = @ptrFromInt(8);
    // Denied:
    try std.testing.expectError(
        error.MissingModuleCapability,
        sdk.requireCapability(fake_handle, .clock),
    );
    // All siblings still allowed:
    try sdk.requireCapability(fake_handle, .crypto);
    try sdk.requireCapability(fake_handle, .random);
    try sdk.requireCapability(fake_handle, .stderr);
}
