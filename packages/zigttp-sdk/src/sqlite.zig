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

extern fn zigttpSdkAllowsSqlQuery(handle: *ModuleHandle, name_ptr: [*]const u8, name_len: usize) bool;
extern fn zigttpSdkAllowsSqlWrite(handle: *ModuleHandle, name_ptr: [*]const u8, name_len: usize) bool;
extern fn zigttpSdkSqliteOpen(handle: *ModuleHandle, path_ptr: [*]const u8, path_len: usize, out: **SqliteDb) bool;
extern fn zigttpSdkSqliteClose(db: *SqliteDb) void;
extern fn zigttpSdkSqliteChanges(db: *SqliteDb) i32;
extern fn zigttpSdkSqliteLastInsertRowId(db: *SqliteDb) i64;
extern fn zigttpSdkSqliteErrmsg(db: *SqliteDb, out_ptr: *[*]const u8, out_len: *usize) void;
extern fn zigttpSdkSqlitePrepare(db: *SqliteDb, sql_ptr: [*]const u8, sql_len: usize, out: **SqliteStmt) bool;
extern fn zigttpSdkSqliteFinalize(stmt: *SqliteStmt) void;
extern fn zigttpSdkSqliteStep(stmt: *SqliteStmt) i32;
extern fn zigttpSdkSqliteReadonly(stmt: *SqliteStmt) bool;
extern fn zigttpSdkSqliteStmtErrmsg(stmt: *SqliteStmt, out_ptr: *[*]const u8, out_len: *usize) void;
extern fn zigttpSdkSqliteParamCount(stmt: *SqliteStmt) u32;
extern fn zigttpSdkSqliteParamName(stmt: *SqliteStmt, index: u32, out_ptr: *[*]const u8, out_len: *usize) bool;
extern fn zigttpSdkSqliteBindNull(stmt: *SqliteStmt, index: u32) bool;
extern fn zigttpSdkSqliteBindInt64(stmt: *SqliteStmt, index: u32, v: i64) bool;
extern fn zigttpSdkSqliteBindDouble(stmt: *SqliteStmt, index: u32, v: f64) bool;
extern fn zigttpSdkSqliteBindText(stmt: *SqliteStmt, index: u32, ptr: [*]const u8, len: usize) bool;
extern fn zigttpSdkSqliteColumnCount(stmt: *SqliteStmt) u32;
extern fn zigttpSdkSqliteColumnName(stmt: *SqliteStmt, index: u32, out_ptr: *[*]const u8, out_len: *usize) void;
extern fn zigttpSdkSqliteColumnType(stmt: *SqliteStmt, index: u32) i32;
extern fn zigttpSdkSqliteColumnInt64(stmt: *SqliteStmt, index: u32) i64;
extern fn zigttpSdkSqliteColumnDouble(stmt: *SqliteStmt, index: u32) f64;
extern fn zigttpSdkSqliteColumnText(stmt: *SqliteStmt, index: u32, out_ptr: *[*]const u8, out_len: *usize) void;

/// Ask the capability policy whether `sql(name)` is permitted.
pub fn allowsSqlQuery(handle: *ModuleHandle, name: []const u8) bool {
    return zigttpSdkAllowsSqlQuery(handle, name.ptr, name.len);
}

/// Ask the capability policy whether a write SQL exec for `name` is
/// permitted (action `db.write`).
pub fn allowsSqlWrite(handle: *ModuleHandle, name: []const u8) bool {
    return zigttpSdkAllowsSqlWrite(handle, name.ptr, name.len);
}

/// Open a SQLite database through the capability-check path. Requires the
/// `.sqlite` capability to be declared on the module binding and the canonical
/// database path to be preallowed by the runtime's SDK SQLite allowlist.
pub fn sqliteOpen(handle: *ModuleHandle, path: []const u8) SqliteError!*SqliteDb {
    try capability.requireCapability(handle, .sqlite);
    var out: *SqliteDb = undefined;
    if (!zigttpSdkSqliteOpen(handle, path.ptr, path.len, &out)) return error.SqliteOpenFailed;
    return out;
}

pub fn sqliteClose(db: *SqliteDb) void {
    zigttpSdkSqliteClose(db);
}

pub fn sqliteChanges(db: *SqliteDb) i32 {
    return zigttpSdkSqliteChanges(db);
}

pub fn sqliteLastInsertRowId(db: *SqliteDb) i64 {
    return zigttpSdkSqliteLastInsertRowId(db);
}

pub fn sqliteErrmsg(db: *SqliteDb) []const u8 {
    var ptr: [*]const u8 = undefined;
    var len: usize = 0;
    zigttpSdkSqliteErrmsg(db, &ptr, &len);
    return ptr[0..len];
}

/// Returns a prepared statement or null on failure; on failure read the
/// error message via `sqliteErrmsg(db)`.
pub fn sqlitePrepare(db: *SqliteDb, sql: []const u8) ?*SqliteStmt {
    var out: *SqliteStmt = undefined;
    if (!zigttpSdkSqlitePrepare(db, sql.ptr, sql.len, &out)) return null;
    return out;
}

pub fn sqliteFinalize(stmt: *SqliteStmt) void {
    zigttpSdkSqliteFinalize(stmt);
}

pub fn sqliteStep(stmt: *SqliteStmt) i32 {
    return zigttpSdkSqliteStep(stmt);
}

pub fn sqliteReadonly(stmt: *SqliteStmt) bool {
    return zigttpSdkSqliteReadonly(stmt);
}

pub fn sqliteStmtErrmsg(stmt: *SqliteStmt) []const u8 {
    var ptr: [*]const u8 = undefined;
    var len: usize = 0;
    zigttpSdkSqliteStmtErrmsg(stmt, &ptr, &len);
    return ptr[0..len];
}

pub fn sqliteParamCount(stmt: *SqliteStmt) u32 {
    return zigttpSdkSqliteParamCount(stmt);
}

pub fn sqliteParamName(stmt: *SqliteStmt, index: u32) ?[]const u8 {
    var ptr: [*]const u8 = undefined;
    var len: usize = 0;
    if (!zigttpSdkSqliteParamName(stmt, index, &ptr, &len)) return null;
    return ptr[0..len];
}

pub fn sqliteBindNull(stmt: *SqliteStmt, index: u32) SqliteError!void {
    if (!zigttpSdkSqliteBindNull(stmt, index)) return error.SqliteBindFailed;
}

pub fn sqliteBindInt64(stmt: *SqliteStmt, index: u32, v: i64) SqliteError!void {
    if (!zigttpSdkSqliteBindInt64(stmt, index, v)) return error.SqliteBindFailed;
}

pub fn sqliteBindDouble(stmt: *SqliteStmt, index: u32, v: f64) SqliteError!void {
    if (!zigttpSdkSqliteBindDouble(stmt, index, v)) return error.SqliteBindFailed;
}

pub fn sqliteBindText(stmt: *SqliteStmt, index: u32, text: []const u8) SqliteError!void {
    if (!zigttpSdkSqliteBindText(stmt, index, text.ptr, text.len)) return error.SqliteBindFailed;
}

pub fn sqliteColumnCount(stmt: *SqliteStmt) u32 {
    return zigttpSdkSqliteColumnCount(stmt);
}

pub fn sqliteColumnName(stmt: *SqliteStmt, index: u32) []const u8 {
    var ptr: [*]const u8 = undefined;
    var len: usize = 0;
    zigttpSdkSqliteColumnName(stmt, index, &ptr, &len);
    return ptr[0..len];
}

pub fn sqliteColumnType(stmt: *SqliteStmt, index: u32) i32 {
    return zigttpSdkSqliteColumnType(stmt, index);
}

pub fn sqliteColumnInt64(stmt: *SqliteStmt, index: u32) i64 {
    return zigttpSdkSqliteColumnInt64(stmt, index);
}

pub fn sqliteColumnDouble(stmt: *SqliteStmt, index: u32) f64 {
    return zigttpSdkSqliteColumnDouble(stmt, index);
}

pub fn sqliteColumnText(stmt: *SqliteStmt, index: u32) []const u8 {
    var ptr: [*]const u8 = undefined;
    var len: usize = 0;
    zigttpSdkSqliteColumnText(stmt, index, &ptr, &len);
    return ptr[0..len];
}
