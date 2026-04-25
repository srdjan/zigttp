//! zigttp:sql - registered SQL queries backed by SQLite.
//!
//! Exports:
//!   sql(name, statement) -> boolean
//!     Registers a named query. Redundant with an identical statement is
//!     idempotent; a different statement for the same name throws.
//!   sqlOne(name, params?) -> object | undefined
//!     Executes a SELECT and returns the first row (or undefined).
//!   sqlMany(name, params?) -> object[]
//!     Executes a SELECT and returns all rows.
//!   sqlExec(name, params?) -> { rowsAffected, lastInsertRowId? }
//!     Executes a write statement.

const std = @import("std");
const sdk = @import("zigttp-sdk");
const util = @import("../internal/util.zig");

pub const MODULE_STATE_SLOT: usize = 2; // module_slots.Slot.sql

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:sql",
    .name = "sql",
    .required_capabilities = &.{ .sqlite, .policy_check },
    .stateful = true,
    .contract_section = "sql",
    .sandboxable = true,
    .exports = &.{
        .{
            .name = "sql",
            .module_func = sqlRegisterImpl,
            .arg_count = 2,
            .returns = .boolean,
            .param_types = &.{ .string, .string },
            .traceable = false,
            .contract_extractions = &.{.{ .category = .sql_registration }},
        },
        .{
            .name = "sqlOne",
            .module_func = sqlOneImpl,
            .arg_count = 2,
            .returns = .optional_object,
            .param_types = &.{ .string, .object },
            .failure_severity = .expected,
            .return_labels = .{ .internal = true },
        },
        .{
            .name = "sqlMany",
            .module_func = sqlManyImpl,
            .arg_count = 2,
            .returns = .object,
            .param_types = &.{ .string, .object },
            .return_labels = .{ .internal = true },
        },
        .{
            .name = "sqlExec",
            .module_func = sqlExecImpl,
            .arg_count = 2,
            .effect = .write,
            .returns = .object,
            .param_types = &.{ .string, .object },
        },
    },
};

pub const SqlStore = struct {
    allocator: std.mem.Allocator,
    db_path: ?[]const u8 = null,
    db: ?*sdk.SqliteDb = null,
    queries: std.StringHashMap([]const u8),

    pub fn init(allocator: std.mem.Allocator, db_path: ?[]const u8) !SqlStore {
        return .{
            .allocator = allocator,
            .db_path = if (db_path) |path| try allocator.dupe(u8, path) else null,
            .db = null,
            .queries = std.StringHashMap([]const u8).init(allocator),
        };
    }

    pub fn configure(self: *SqlStore, db_path: ?[]const u8) !void {
        if (self.db) |db| {
            sdk.sqliteClose(db);
            self.db = null;
        }
        if (self.db_path) |existing| self.allocator.free(existing);
        self.db_path = if (db_path) |path| try self.allocator.dupe(u8, path) else null;
    }

    fn deinitSelf(self: *SqlStore) void {
        if (self.db) |db| sdk.sqliteClose(db);
        if (self.db_path) |path| self.allocator.free(path);

        var it = self.queries.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.queries.deinit();
    }

    pub fn sdkDeinit(ptr: *anyopaque) callconv(.c) void {
        const self: *SqlStore = @ptrCast(@alignCast(ptr));
        const allocator = self.allocator;
        self.deinitSelf();
        allocator.destroy(self);
    }

    fn registerQuery(self: *SqlStore, name: []const u8, statement: []const u8) !void {
        if (self.queries.get(name)) |existing| {
            if (std.mem.eql(u8, existing, statement)) return;
            return error.DuplicateQueryName;
        }
        try self.queries.put(
            try self.allocator.dupe(u8, name),
            try self.allocator.dupe(u8, statement),
        );
    }

    fn getQuery(self: *SqlStore, name: []const u8) ?[]const u8 {
        return self.queries.get(name);
    }

    fn ensureDb(self: *SqlStore, handle: *sdk.ModuleHandle) !*sdk.SqliteDb {
        if (self.db) |db| return db;
        const path = self.db_path orelse return error.MissingDatabasePath;
        const db = try sdk.sqliteOpen(handle, path);
        self.db = db;
        return db;
    }
};

fn getOrCreateStore(handle: *sdk.ModuleHandle) !*SqlStore {
    if (sdk.getModuleState(handle, SqlStore, MODULE_STATE_SLOT)) |store| return store;
    const allocator = sdk.getAllocator(handle);
    const store = try allocator.create(SqlStore);
    store.* = try SqlStore.init(allocator, null);
    try sdk.setModuleState(handle, MODULE_STATE_SLOT, @ptrCast(store), SqlStore.sdkDeinit);
    return store;
}

fn sqlRegisterImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return util.throwTypeError(handle, "sql() expects a query name and SQL statement");

    const name = sdk.extractString(args[0]) orelse return util.throwTypeError(handle, "sql() query name must be a string");
    const statement = sdk.extractString(args[1]) orelse return util.throwTypeError(handle, "sql() SQL statement must be a string");

    const store = getOrCreateStore(handle) catch return sdk.throwError(handle, "Error", "failed to initialize sql store");
    store.registerQuery(name, statement) catch |err| {
        return switch (err) {
            error.DuplicateQueryName => sdk.throwError(handle, "Error", "sql() query name already registered with a different statement"),
            else => sdk.throwError(handle, "Error", "failed to register sql query"),
        };
    };

    return sdk.JSValue.true_val;
}

fn sqlOneImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    return executeQuery(handle, args, .one);
}

fn sqlManyImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    return executeQuery(handle, args, .many);
}

fn sqlExecImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    return executeQuery(handle, args, .exec);
}

const ExecMode = enum { one, many, exec };

fn executeQuery(handle: *sdk.ModuleHandle, args: []const sdk.JSValue, mode: ExecMode) anyerror!sdk.JSValue {
    if (args.len == 0) return util.throwTypeError(handle, "sql query name must be provided");

    const name = sdk.extractString(args[0]) orelse return util.throwTypeError(handle, "sql query name must be a string");
    const allowed = switch (mode) {
        .one, .many => sdk.allowsSqlQuery(handle, name),
        .exec => sdk.allowsSqlWrite(handle, name),
    };
    if (!allowed) return util.throwCapabilityPolicyError(handle, "sql query", name);

    const store = getOrCreateStore(handle) catch return sdk.throwError(handle, "Error", "failed to initialize sql store");
    const statement = store.getQuery(name) orelse return sdk.throwError(handle, "Error", "SQL query not registered");
    const db = store.ensureDb(handle) catch |err| {
        return switch (err) {
            error.MissingDatabasePath => sdk.throwError(handle, "Error", "SQL query execution requires --sqlite <FILE>"),
            else => sdk.throwError(handle, "Error", "failed to open sqlite database"),
        };
    };

    const stmt = sdk.sqlitePrepare(db, statement) orelse return sdk.throwError(handle, "SqlError", sdk.sqliteErrmsg(db));
    defer sdk.sqliteFinalize(stmt);

    const readonly = sdk.sqliteReadonly(stmt);
    switch (mode) {
        .one, .many => if (!readonly) return sdk.throwError(handle, "Error", "sqlOne/sqlMany require a read-only SELECT statement"),
        .exec => if (readonly) return sdk.throwError(handle, "Error", "sqlExec requires a write statement"),
    }

    const params_val: sdk.JSValue = if (args.len >= 2) args[1] else sdk.JSValue.undefined_val;
    bindParams(handle, stmt, params_val) catch |err| {
        return switch (err) {
            error.ExpectedParamsObject => util.throwTypeError(handle, "SQL params must be an object"),
            error.UnsupportedParamType => util.throwTypeError(handle, "SQL params only support string, number, boolean, or undefined"),
            error.MissingParameter => sdk.throwError(handle, "SqlError", "missing SQL parameter"),
            error.ExtraParameter => sdk.throwError(handle, "SqlError", "unexpected SQL parameter"),
            error.PositionalParametersUnsupported => sdk.throwError(handle, "SqlError", "SQL statements must use named parameters"),
            else => sdk.throwError(handle, "SqlError", sdk.sqliteStmtErrmsg(stmt)),
        };
    };

    return switch (mode) {
        .one => executeOne(handle, stmt),
        .many => executeMany(handle, stmt),
        .exec => executeExec(handle, db, stmt),
    };
}

fn executeOne(handle: *sdk.ModuleHandle, stmt: *sdk.SqliteStmt) !sdk.JSValue {
    const rc = sdk.sqliteStep(stmt);
    if (rc == sdk.sqlite_done) return sdk.JSValue.undefined_val;
    if (rc != sdk.sqlite_row) return sdk.throwError(handle, "SqlError", sdk.sqliteStmtErrmsg(stmt));
    return buildRowObject(handle, stmt);
}

fn executeMany(handle: *sdk.ModuleHandle, stmt: *sdk.SqliteStmt) !sdk.JSValue {
    const result = try sdk.createArray(handle);
    while (true) {
        const rc = sdk.sqliteStep(stmt);
        if (rc == sdk.sqlite_done) break;
        if (rc != sdk.sqlite_row) return sdk.throwError(handle, "SqlError", sdk.sqliteStmtErrmsg(stmt));
        const row = try buildRowObject(handle, stmt);
        try sdk.arrayPush(handle, result, row);
    }
    return result;
}

fn executeExec(handle: *sdk.ModuleHandle, db: *sdk.SqliteDb, stmt: *sdk.SqliteStmt) !sdk.JSValue {
    const rc = sdk.sqliteStep(stmt);
    if (rc != sdk.sqlite_done) return sdk.throwError(handle, "SqlError", sdk.sqliteStmtErrmsg(stmt));

    const result = try sdk.createObject(handle);
    try sdk.objectSet(handle, result, "rowsAffected", sdk.JSValue.fromInt(sdk.sqliteChanges(db)));

    const insert_id = sdk.sqliteLastInsertRowId(db);
    if (insert_id != 0) {
        try sdk.objectSet(handle, result, "lastInsertRowId", sdk.numberFromF64(@floatFromInt(insert_id)));
    }
    return result;
}

fn bindParams(handle: *sdk.ModuleHandle, stmt: *sdk.SqliteStmt, params_val: sdk.JSValue) !void {
    const param_count = sdk.sqliteParamCount(stmt);
    if (param_count == 0) {
        if (!params_val.isUndefined()) {
            if (sdk.isObject(params_val)) {
                const keys = try sdk.objectKeys(handle, params_val);
                const keys_len = sdk.arrayLength(keys) orelse 0;
                if (keys_len > 0) return error.ExtraParameter;
            }
        }
        return;
    }

    if (params_val.isUndefined()) return error.MissingParameter;
    if (!sdk.isObject(params_val)) return error.ExpectedParamsObject;

    const keys = try sdk.objectKeys(handle, params_val);
    const keys_len = sdk.arrayLength(keys) orelse 0;

    var used = std.ArrayList(bool).empty;
    defer used.deinit(sdk.getAllocator(handle));
    try used.appendNTimes(sdk.getAllocator(handle), false, keys_len);

    var idx: u32 = 1;
    while (idx <= param_count) : (idx += 1) {
        const param_name = sdk.sqliteParamName(stmt, idx) orelse return error.PositionalParametersUnsupported;
        var matched = false;
        var k: u32 = 0;
        while (k < keys_len) : (k += 1) {
            const key_val = sdk.arrayGet(handle, keys, k) orelse continue;
            const key_str = sdk.extractString(key_val) orelse continue;
            if (!std.mem.eql(u8, key_str, param_name)) continue;
            const provided = sdk.objectGet(handle, params_val, key_str) orelse sdk.JSValue.undefined_val;
            try bindValue(stmt, idx, provided);
            used.items[k] = true;
            matched = true;
            break;
        }
        if (!matched) return error.MissingParameter;
    }

    for (used.items) |u| {
        if (!u) return error.ExtraParameter;
    }
}

fn bindValue(stmt: *sdk.SqliteStmt, index: u32, v: sdk.JSValue) !void {
    if (v.isUndefined() or v.isNull()) return sdk.sqliteBindNull(stmt, index);
    if (v.isTrue()) return sdk.sqliteBindInt64(stmt, index, 1);
    if (v.isFalse()) return sdk.sqliteBindInt64(stmt, index, 0);
    if (v.toInt()) |i| return sdk.sqliteBindInt64(stmt, index, i);
    if (v.toFloat()) |f| return sdk.sqliteBindDouble(stmt, index, f);
    if (sdk.extractString(v)) |s| return sdk.sqliteBindText(stmt, index, s);
    return error.UnsupportedParamType;
}

fn buildRowObject(handle: *sdk.ModuleHandle, stmt: *sdk.SqliteStmt) !sdk.JSValue {
    const row = try sdk.createObject(handle);
    const column_count = sdk.sqliteColumnCount(stmt);
    var i: u32 = 0;
    while (i < column_count) : (i += 1) {
        const cell = try buildColumnValue(handle, stmt, i);
        try sdk.objectSet(handle, row, sdk.sqliteColumnName(stmt, i), cell);
    }
    return row;
}

fn buildColumnValue(handle: *sdk.ModuleHandle, stmt: *sdk.SqliteStmt, index: u32) !sdk.JSValue {
    return switch (sdk.sqliteColumnType(stmt, index)) {
        sdk.sqlite_integer => sdk.numberFromF64(@floatFromInt(sdk.sqliteColumnInt64(stmt, index))),
        sdk.sqlite_float => sdk.JSValue.fromFloat(sdk.sqliteColumnDouble(stmt, index)),
        sdk.sqlite_text => try sdk.createString(handle, sdk.sqliteColumnText(stmt, index)),
        sdk.sqlite_null => sdk.JSValue.undefined_val,
        sdk.sqlite_blob => error.UnsupportedBlobColumn,
        else => sdk.JSValue.undefined_val,
    };
}
