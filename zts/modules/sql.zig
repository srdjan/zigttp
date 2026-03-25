//! zigttp:sql - registered SQL queries backed by SQLite
//!
//! Exports:
//!   sql(name: string, statement: string) -> boolean
//!   sqlOne(name: string, params?: object) -> object | undefined
//!   sqlMany(name: string, params?: object) -> object[]
//!   sqlExec(name: string, params?: object) -> { rowsAffected, lastInsertRowId? }

const std = @import("std");
const context = @import("../context.zig");
const value = @import("../value.zig");
const object = @import("../object.zig");
const resolver = @import("resolver.zig");
const util = @import("util.zig");
const sqlite = @import("../sqlite.zig");

const mb = @import("../module_binding.zig");

pub const MODULE_STATE_SLOT = @intFromEnum(@import("../module_slots.zig").Slot.sql);

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:sql",
    .name = "sql",
    .stateful = true,
    .contract_section = "sql",
    .sandboxable = true,
    .exports = &.{
        .{ .name = "sql", .func = sqlRegisterNative, .arg_count = 2,
           .returns = .boolean, .param_types = &.{ .string, .string },
           .traceable = false,
           .contract_extractions = &.{.{ .category = .sql_registration }} },
        .{ .name = "sqlOne", .func = sqlOneNative, .arg_count = 2,
           .returns = .optional_object, .param_types = &.{ .string, .object },
           .failure_severity = .expected,
           .return_labels = .{ .internal = true } },
        .{ .name = "sqlMany", .func = sqlManyNative, .arg_count = 2,
           .returns = .object, .param_types = &.{ .string, .object },
           .return_labels = .{ .internal = true } },
        .{ .name = "sqlExec", .func = sqlExecNative, .arg_count = 2,
           .effect = .write, .returns = .object, .param_types = &.{ .string, .object } },
    },
};

pub const exports = binding.toModuleExports();

pub const SqlStore = struct {
    allocator: std.mem.Allocator,
    db_path: ?[]const u8 = null,
    db: ?sqlite.Db = null,
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
        if (self.db) |*db| {
            db.close();
            self.db = null;
        }
        if (self.db_path) |existing| self.allocator.free(existing);
        self.db_path = if (db_path) |path| try self.allocator.dupe(u8, path) else null;
    }

    fn deinitSelf(self: *SqlStore) void {
        if (self.db) |*db| db.close();
        if (self.db_path) |path| self.allocator.free(path);

        var it = self.queries.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.queries.deinit();
    }

    pub fn deinitOpaque(ptr: *anyopaque, _: std.mem.Allocator) void {
        const self: *SqlStore = @ptrCast(@alignCast(ptr));
        self.deinitSelf();
        self.allocator.destroy(self);
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

    fn ensureDb(self: *SqlStore) !*sqlite.Db {
        if (self.db == null) {
            const path = self.db_path orelse return error.MissingDatabasePath;
            self.db = try sqlite.Db.openReadWriteCreate(self.allocator, path);
        }
        return &self.db.?;
    }
};

pub fn installStore(ctx: *context.Context, db_path: ?[]const u8) !void {
    if (ctx.getModuleState(SqlStore, MODULE_STATE_SLOT)) |store| {
        try store.configure(db_path);
        return;
    }

    const store = try ctx.allocator.create(SqlStore);
    store.* = try SqlStore.init(ctx.allocator, db_path);
    ctx.setModuleState(MODULE_STATE_SLOT, @ptrCast(store), &SqlStore.deinitOpaque);
}

fn getOrCreateStore(ctx: *context.Context) !*SqlStore {
    if (ctx.getModuleState(SqlStore, MODULE_STATE_SLOT)) |store| return store;

    const store = try ctx.allocator.create(SqlStore);
    store.* = try SqlStore.init(ctx.allocator, null);
    ctx.setModuleState(MODULE_STATE_SLOT, @ptrCast(store), &SqlStore.deinitOpaque);
    return store;
}

fn sqlRegisterNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    if (args.len < 2) return util.throwError(ctx, "TypeError", "sql() expects a query name and SQL statement");

    const name = util.extractString(args[0]) orelse return util.throwError(ctx, "TypeError", "sql() query name must be a string");
    const statement = util.extractString(args[1]) orelse return util.throwError(ctx, "TypeError", "sql() SQL statement must be a string");

    const store = getOrCreateStore(ctx) catch return util.throwError(ctx, "Error", "failed to initialize sql store");
    store.registerQuery(name, statement) catch |err| {
        return switch (err) {
            error.DuplicateQueryName => util.throwError(ctx, "Error", "sql() query name already registered with a different statement"),
            else => util.throwError(ctx, "Error", "failed to register sql query"),
        };
    };

    return value.JSValue.true_val;
}

fn sqlOneNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    return executeQuery(util.castContext(ctx_ptr), args, .one);
}

fn sqlManyNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    return executeQuery(util.castContext(ctx_ptr), args, .many);
}

fn sqlExecNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    return executeQuery(util.castContext(ctx_ptr), args, .exec);
}

const ExecMode = enum { one, many, exec };

const ProvidedParam = struct {
    name: []const u8,
    value: value.JSValue,
    used: bool = false,
};

fn executeQuery(ctx: *context.Context, args: []const value.JSValue, mode: ExecMode) value.JSValue {
    if (args.len == 0) return util.throwError(ctx, "TypeError", "sql query name must be provided");

    const name = util.extractString(args[0]) orelse return util.throwError(ctx, "TypeError", "sql query name must be a string");
    if (!ensureQueryAllowed(ctx, name)) return value.JSValue.exception_val;

    const store = getOrCreateStore(ctx) catch return util.throwError(ctx, "Error", "failed to initialize sql store");
    const statement = store.getQuery(name) orelse return util.throwError(ctx, "Error", "SQL query not registered");
    const db = store.ensureDb() catch |err| {
        return switch (err) {
            error.MissingDatabasePath => util.throwError(ctx, "Error", "SQL query execution requires --sqlite <FILE>"),
            else => util.throwError(ctx, "Error", "failed to open sqlite database"),
        };
    };

    var stmt = db.prepare(statement) catch return sqlError(ctx, db.errmsg());
    defer stmt.finalize();

    const readonly = stmt.readonly();
    switch (mode) {
        .one, .many => if (!readonly) return util.throwError(ctx, "Error", "sqlOne/sqlMany require a read-only SELECT statement"),
        .exec => if (readonly) return util.throwError(ctx, "Error", "sqlExec requires a write statement"),
    }

    bindParams(ctx, &stmt, if (args.len >= 2) args[1] else value.JSValue.undefined_val) catch |err| {
        return switch (err) {
            error.ExpectedParamsObject => util.throwError(ctx, "TypeError", "SQL params must be an object"),
            error.UnsupportedParamType => util.throwError(ctx, "TypeError", "SQL params only support string, number, boolean, or undefined"),
            error.MissingParameter => sqlError(ctx, "missing SQL parameter"),
            error.ExtraParameter => sqlError(ctx, "unexpected SQL parameter"),
            error.PositionalParametersUnsupported => sqlError(ctx, "SQL statements must use named parameters"),
            else => sqlError(ctx, stmt.errmsg()),
        };
    };

    return switch (mode) {
        .one => executeOne(ctx, &stmt),
        .many => executeMany(ctx, &stmt),
        .exec => executeExec(ctx, db.*, &stmt),
    };
}

fn executeOne(ctx: *context.Context, stmt: *sqlite.Stmt) value.JSValue {
    const rc = stmt.step();
    if (rc == sqlite.c.SQLITE_DONE) return value.JSValue.undefined_val;
    if (rc != sqlite.c.SQLITE_ROW) return sqlError(ctx, stmt.errmsg());

    return buildRowObject(ctx, stmt) catch return util.throwError(ctx, "Error", "failed to build SQL row");
}

fn executeMany(ctx: *context.Context, stmt: *sqlite.Stmt) value.JSValue {
    const result = ctx.createArray() catch return util.throwError(ctx, "Error", "failed to allocate SQL result array");

    while (true) {
        const rc = stmt.step();
        if (rc == sqlite.c.SQLITE_DONE) break;
        if (rc != sqlite.c.SQLITE_ROW) return sqlError(ctx, stmt.errmsg());

        const row = buildRowObject(ctx, stmt) catch return util.throwError(ctx, "Error", "failed to build SQL row");
        result.arrayPush(ctx.allocator, row) catch return util.throwError(ctx, "Error", "failed to append SQL row");
    }

    return result.toValue();
}

fn executeExec(ctx: *context.Context, db: sqlite.Db, stmt: *sqlite.Stmt) value.JSValue {
    const rc = stmt.step();
    if (rc != sqlite.c.SQLITE_DONE) return sqlError(ctx, stmt.errmsg());

    const result = ctx.createObject(null) catch return util.throwError(ctx, "Error", "failed to allocate SQL exec result");
    const pool = ctx.hidden_class_pool orelse return util.throwError(ctx, "Error", "missing hidden class pool");

    const rows_affected_atom = tryIntern(ctx, "rowsAffected") catch {
        return util.throwError(ctx, "Error", "failed to build SQL exec result");
    };
    result.setProperty(ctx.allocator, pool, rows_affected_atom, value.JSValue.fromInt(db.changes())) catch {
        return util.throwError(ctx, "Error", "failed to build SQL exec result");
    };

    const insert_id = db.lastInsertRowId();
    if (insert_id != 0) {
        const insert_val = if (insert_id >= std.math.minInt(i32) and insert_id <= std.math.maxInt(i32))
            value.JSValue.fromInt(@intCast(insert_id))
        else
            value.JSValue.fromFloat(@floatFromInt(insert_id));
        const insert_id_atom = tryIntern(ctx, "lastInsertRowId") catch {
            return util.throwError(ctx, "Error", "failed to build SQL exec result");
        };
        result.setProperty(ctx.allocator, pool, insert_id_atom, insert_val) catch {
            return util.throwError(ctx, "Error", "failed to build SQL exec result");
        };
    }

    return result.toValue();
}

fn bindParams(ctx: *context.Context, stmt: *sqlite.Stmt, params_val: value.JSValue) !void {
    const param_count = stmt.paramCount();
    if (param_count == 0) {
        if (!params_val.isUndefined()) {
            const provided = try collectParams(ctx, params_val);
            defer ctx.allocator.free(provided);
            if (provided.len > 0) return error.ExtraParameter;
        }
        return;
    }

    if (params_val.isUndefined()) return error.MissingParameter;
    if (!params_val.isObject()) return error.ExpectedParamsObject;

    const provided = try collectParams(ctx, params_val);
    defer ctx.allocator.free(provided);

    for (1..param_count + 1) |idx| {
        const param_name = stmt.paramName(idx) orelse return error.PositionalParametersUnsupported;
        var matched = false;
        for (provided) |*candidate| {
            if (!std.mem.eql(u8, candidate.name, param_name)) continue;
            try bindValue(stmt, idx, candidate.value);
            candidate.used = true;
            matched = true;
            break;
        }
        if (!matched) return error.MissingParameter;
    }

    for (provided) |candidate| {
        if (!candidate.used) return error.ExtraParameter;
    }
}

fn collectParams(ctx: *context.Context, params_val: value.JSValue) ![]ProvidedParam {
    if (params_val.isUndefined()) return try ctx.allocator.alloc(ProvidedParam, 0);
    if (!params_val.isObject()) return error.ExpectedParamsObject;

    const obj = params_val.toPtr(object.JSObject);
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    var params = std.ArrayList(ProvidedParam).empty;
    errdefer params.deinit(ctx.allocator);

    var iter = obj.propertyIterator(pool);
    while (iter.next()) |entry| {
        const name = ctx.atoms.getName(entry.atom) orelse continue;
        try params.append(ctx.allocator, .{
            .name = name,
            .value = entry.value,
            .used = false,
        });
    }

    return params.toOwnedSlice(ctx.allocator);
}

fn bindValue(stmt: *sqlite.Stmt, index: usize, param_val: value.JSValue) !void {
    if (param_val.isUndefined()) return stmt.bindNull(index);
    if (param_val.isTrue()) return stmt.bindInt64(index, 1);
    if (param_val.isFalse()) return stmt.bindInt64(index, 0);
    if (param_val.isInt()) return stmt.bindInt64(index, param_val.getInt());
    if (param_val.isNumber()) return stmt.bindDouble(index, param_val.getFloat64());
    if (util.extractString(param_val)) |str| return stmt.bindText(index, str);
    return error.UnsupportedParamType;
}

fn buildRowObject(ctx: *context.Context, stmt: *sqlite.Stmt) !value.JSValue {
    const row = try ctx.createObject(null);
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    const column_count = stmt.columnCount();
    for (0..column_count) |idx| {
        const atom = try ctx.atoms.intern(stmt.columnName(idx));
        const cell = try buildColumnValue(ctx, stmt, idx);
        try row.setProperty(ctx.allocator, pool, atom, cell);
    }

    return row.toValue();
}

fn buildColumnValue(ctx: *context.Context, stmt: *sqlite.Stmt, column_idx: usize) !value.JSValue {
    return switch (stmt.columnType(column_idx)) {
        sqlite.c.SQLITE_INTEGER => blk: {
            const int_val = stmt.columnInt64(column_idx);
            if (int_val >= std.math.minInt(i32) and int_val <= std.math.maxInt(i32)) {
                break :blk value.JSValue.fromInt(@intCast(int_val));
            }
            break :blk value.JSValue.fromFloat(@floatFromInt(int_val));
        },
        sqlite.c.SQLITE_FLOAT => value.JSValue.fromFloat(stmt.columnDouble(column_idx)),
        sqlite.c.SQLITE_TEXT => try ctx.createString(stmt.columnText(column_idx)),
        sqlite.c.SQLITE_NULL => value.JSValue.undefined_val,
        sqlite.c.SQLITE_BLOB => return error.UnsupportedBlobColumn,
        else => value.JSValue.undefined_val,
    };
}

fn ensureQueryAllowed(ctx: *context.Context, name: []const u8) bool {
    if (ctx.capability_policy.allowsSqlQuery(name)) return true;
    _ = util.throwCapabilityPolicyError(ctx, "sql query", name);
    return false;
}

fn sqlError(ctx: *context.Context, message: []const u8) value.JSValue {
    return util.throwError(ctx, "SqlError", message);
}

fn tryIntern(ctx: *context.Context, name: []const u8) !object.Atom {
    return object.lookupPredefinedAtom(name) orelse try ctx.atoms.intern(name);
}

test "sql module register and execute queries" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const db_path = try tmp.dir.realPathAlloc(std.testing.allocator, ".", 1024);
    defer std.testing.allocator.free(db_path);

    const sqlite_path = try std.fs.path.join(std.testing.allocator, &.{ db_path, "test.sqlite" });
    defer std.testing.allocator.free(sqlite_path);

    var db = try sqlite.Db.openReadWriteCreate(std.testing.allocator, sqlite_path);
    defer db.close();
    try db.exec(std.testing.allocator, "CREATE TABLE todos(id TEXT PRIMARY KEY, text TEXT, done INTEGER NOT NULL DEFAULT 0);");

    var gc_state = try @import("../gc.zig").GC.init(std.testing.allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();
    var heap_state = @import("../heap.zig").Heap.init(std.testing.allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    const ctx = try context.Context.init(std.testing.allocator, &gc_state, .{});
    defer ctx.deinit();
    try @import("../builtins/root.zig").initBuiltins(ctx);
    try installStore(ctx, sqlite_path);

    const register_args = [_]value.JSValue{
        try ctx.createString("insertTodo"),
        try ctx.createString("INSERT INTO todos(id, text, done) VALUES (:id, :text, :done)"),
    };
    _ = try sqlRegisterNative(@ptrCast(ctx), value.JSValue.undefined_val, &register_args);

    const read_args = [_]value.JSValue{
        try ctx.createString("getTodo"),
        try ctx.createString("SELECT id, text, done FROM todos WHERE id = :id"),
    };
    _ = try sqlRegisterNative(@ptrCast(ctx), value.JSValue.undefined_val, &read_args);

    const params = try ctx.createObject(null);
    try ctx.setPropertyChecked(params, try ctx.atoms.intern("id"), try ctx.createString("todo-1"));
    try ctx.setPropertyChecked(params, try ctx.atoms.intern("text"), try ctx.createString("write tests"));
    try ctx.setPropertyChecked(params, try ctx.atoms.intern("done"), value.JSValue.fromInt(0));

    const exec_args = [_]value.JSValue{
        try ctx.createString("insertTodo"),
        params.toValue(),
    };
    const exec_result = try sqlExecNative(@ptrCast(ctx), value.JSValue.undefined_val, &exec_args);
    try std.testing.expect(exec_result.isObject());

    const read_params = try ctx.createObject(null);
    try ctx.setPropertyChecked(read_params, try ctx.atoms.intern("id"), try ctx.createString("todo-1"));
    const one_args = [_]value.JSValue{
        try ctx.createString("getTodo"),
        read_params.toValue(),
    };
    const row_val = try sqlOneNative(@ptrCast(ctx), value.JSValue.undefined_val, &one_args);
    try std.testing.expect(row_val.isObject());

    const row = row_val.toPtr(object.JSObject);
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const text_val = row.getProperty(pool, try ctx.atoms.intern("text")) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqualStrings("write tests", util.extractString(text_val).?);
}
