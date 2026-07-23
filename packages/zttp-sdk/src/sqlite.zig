const handles = @import("handle.zig");
const capability = @import("capability.zig");

pub const ModuleHandle = handles.ModuleHandle;

// Modules that declare the `.sqlite` capability can open, prepare, and
// execute statements through these helpers. SqliteDb and SqliteStmt are
// opaque handles; the runtime owns their lifetime until close/finalize.
pub const SqliteDb = opaque {};
pub const SqliteStmt = opaque {};

pub const sqlite_row: i32 = 100;
pub const sqlite_done: i32 = 101;

pub const sqlite_integer: i32 = 1;
pub const sqlite_float: i32 = 2;
pub const sqlite_text: i32 = 3;
pub const sqlite_blob: i32 = 4;
pub const sqlite_null: i32 = 5;

pub const SqliteError = capability.ModuleCapabilityError || error{
    SqliteOpenFailed,
    SqlitePrepareFailed,
    SqliteBindFailed,
};

extern fn zttpSdkAllowsSqlQuery(handle: *ModuleHandle, name_ptr: [*]const u8, name_len: usize) bool;
extern fn zttpSdkAllowsSqlWrite(handle: *ModuleHandle, name_ptr: [*]const u8, name_len: usize) bool;
extern fn zttpSdkSqliteOpen(handle: *ModuleHandle, path_ptr: [*]const u8, path_len: usize, out: **SqliteDb) bool;
extern fn zttpSdkSqliteClose(db: *SqliteDb) void;
extern fn zttpSdkSqliteChanges(db: *SqliteDb) i32;
extern fn zttpSdkSqliteLastInsertRowId(db: *SqliteDb) i64;
extern fn zttpSdkSqliteErrmsg(db: *SqliteDb, out_ptr: *[*]const u8, out_len: *usize) void;
extern fn zttpSdkSqlitePrepare(db: *SqliteDb, sql_ptr: [*]const u8, sql_len: usize, out: **SqliteStmt) bool;
extern fn zttpSdkSqliteFinalize(stmt: *SqliteStmt) void;
extern fn zttpSdkSqliteStep(stmt: *SqliteStmt) i32;
extern fn zttpSdkSqliteReadonly(stmt: *SqliteStmt) bool;
extern fn zttpSdkSqliteStmtErrmsg(stmt: *SqliteStmt, out_ptr: *[*]const u8, out_len: *usize) void;
extern fn zttpSdkSqliteParamCount(stmt: *SqliteStmt) u32;
extern fn zttpSdkSqliteParamName(stmt: *SqliteStmt, index: u32, out_ptr: *[*]const u8, out_len: *usize) bool;
extern fn zttpSdkSqliteBindNull(stmt: *SqliteStmt, index: u32) bool;
extern fn zttpSdkSqliteBindInt64(stmt: *SqliteStmt, index: u32, v: i64) bool;
extern fn zttpSdkSqliteBindDouble(stmt: *SqliteStmt, index: u32, v: f64) bool;
extern fn zttpSdkSqliteBindText(stmt: *SqliteStmt, index: u32, ptr: [*]const u8, len: usize) bool;
extern fn zttpSdkSqliteColumnCount(stmt: *SqliteStmt) u32;
extern fn zttpSdkSqliteColumnName(stmt: *SqliteStmt, index: u32, out_ptr: *[*]const u8, out_len: *usize) void;
extern fn zttpSdkSqliteColumnType(stmt: *SqliteStmt, index: u32) i32;
extern fn zttpSdkSqliteColumnInt64(stmt: *SqliteStmt, index: u32) i64;
extern fn zttpSdkSqliteColumnDouble(stmt: *SqliteStmt, index: u32) f64;
extern fn zttpSdkSqliteColumnText(stmt: *SqliteStmt, index: u32, out_ptr: *[*]const u8, out_len: *usize) void;

/// Ask the capability policy whether `sql(name)` is permitted.
pub fn allowsSqlQuery(handle: *ModuleHandle, name: []const u8) bool {
    return zttpSdkAllowsSqlQuery(handle, name.ptr, name.len);
}

/// Ask the capability policy whether a write SQL exec for `name` is
/// permitted (action `db.write`).
pub fn allowsSqlWrite(handle: *ModuleHandle, name: []const u8) bool {
    return zttpSdkAllowsSqlWrite(handle, name.ptr, name.len);
}

/// Open a SQLite database through the capability-check path. Requires the
/// `.sqlite` capability to be declared on the module binding and the canonical
/// database path to be preallowed by the runtime's SDK SQLite allowlist.
pub fn sqliteOpen(handle: *ModuleHandle, path: []const u8) SqliteError!*SqliteDb {
    try capability.requireCapability(handle, .sqlite);
    var out: *SqliteDb = undefined;
    if (!zttpSdkSqliteOpen(handle, path.ptr, path.len, &out)) return error.SqliteOpenFailed;
    return out;
}

pub fn sqliteClose(db: *SqliteDb) void {
    zttpSdkSqliteClose(db);
}

pub fn sqliteChanges(db: *SqliteDb) i32 {
    return zttpSdkSqliteChanges(db);
}

pub fn sqliteLastInsertRowId(db: *SqliteDb) i64 {
    return zttpSdkSqliteLastInsertRowId(db);
}

pub fn sqliteErrmsg(db: *SqliteDb) []const u8 {
    var ptr: [*]const u8 = undefined;
    var len: usize = 0;
    zttpSdkSqliteErrmsg(db, &ptr, &len);
    return ptr[0..len];
}

/// Returns a prepared statement or null on failure; on failure read the
/// error message via `sqliteErrmsg(db)`.
pub fn sqlitePrepare(db: *SqliteDb, sql: []const u8) ?*SqliteStmt {
    var out: *SqliteStmt = undefined;
    if (!zttpSdkSqlitePrepare(db, sql.ptr, sql.len, &out)) return null;
    return out;
}

pub fn sqliteFinalize(stmt: *SqliteStmt) void {
    zttpSdkSqliteFinalize(stmt);
}

pub fn sqliteStep(stmt: *SqliteStmt) i32 {
    return zttpSdkSqliteStep(stmt);
}

pub fn sqliteReadonly(stmt: *SqliteStmt) bool {
    return zttpSdkSqliteReadonly(stmt);
}

pub fn sqliteStmtErrmsg(stmt: *SqliteStmt) []const u8 {
    var ptr: [*]const u8 = undefined;
    var len: usize = 0;
    zttpSdkSqliteStmtErrmsg(stmt, &ptr, &len);
    return ptr[0..len];
}

pub fn sqliteParamCount(stmt: *SqliteStmt) u32 {
    return zttpSdkSqliteParamCount(stmt);
}

pub fn sqliteParamName(stmt: *SqliteStmt, index: u32) ?[]const u8 {
    var ptr: [*]const u8 = undefined;
    var len: usize = 0;
    if (!zttpSdkSqliteParamName(stmt, index, &ptr, &len)) return null;
    return ptr[0..len];
}

pub fn sqliteBindNull(stmt: *SqliteStmt, index: u32) SqliteError!void {
    if (!zttpSdkSqliteBindNull(stmt, index)) return error.SqliteBindFailed;
}

pub fn sqliteBindInt64(stmt: *SqliteStmt, index: u32, v: i64) SqliteError!void {
    if (!zttpSdkSqliteBindInt64(stmt, index, v)) return error.SqliteBindFailed;
}

pub fn sqliteBindDouble(stmt: *SqliteStmt, index: u32, v: f64) SqliteError!void {
    if (!zttpSdkSqliteBindDouble(stmt, index, v)) return error.SqliteBindFailed;
}

pub fn sqliteBindText(stmt: *SqliteStmt, index: u32, text: []const u8) SqliteError!void {
    if (!zttpSdkSqliteBindText(stmt, index, text.ptr, text.len)) return error.SqliteBindFailed;
}

pub fn sqliteColumnCount(stmt: *SqliteStmt) u32 {
    return zttpSdkSqliteColumnCount(stmt);
}

pub fn sqliteColumnName(stmt: *SqliteStmt, index: u32) []const u8 {
    var ptr: [*]const u8 = undefined;
    var len: usize = 0;
    zttpSdkSqliteColumnName(stmt, index, &ptr, &len);
    return ptr[0..len];
}

pub fn sqliteColumnType(stmt: *SqliteStmt, index: u32) i32 {
    return zttpSdkSqliteColumnType(stmt, index);
}

pub fn sqliteColumnInt64(stmt: *SqliteStmt, index: u32) i64 {
    return zttpSdkSqliteColumnInt64(stmt, index);
}

pub fn sqliteColumnDouble(stmt: *SqliteStmt, index: u32) f64 {
    return zttpSdkSqliteColumnDouble(stmt, index);
}

pub fn sqliteColumnText(stmt: *SqliteStmt, index: u32) []const u8 {
    var ptr: [*]const u8 = undefined;
    var len: usize = 0;
    zttpSdkSqliteColumnText(stmt, index, &ptr, &len);
    return ptr[0..len];
}
