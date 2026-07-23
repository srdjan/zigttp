const std = @import("std");

pub const c = @cImport({
    @cInclude("sqlite3.h");
});

pub const Error = error{
    SqliteOpenFailed,
    SqliteExecFailed,
    SqlitePrepareFailed,
    SqliteBindFailed,
};

pub const Db = struct {
    handle: *c.sqlite3,

    pub fn openReadWriteCreate(allocator: std.mem.Allocator, path: []const u8) !Db {
        return openPath(allocator, path, c.SQLITE_OPEN_READWRITE | c.SQLITE_OPEN_CREATE | c.SQLITE_OPEN_URI);
    }

    pub fn openReadOnly(allocator: std.mem.Allocator, path: []const u8) !Db {
        return openPath(allocator, path, c.SQLITE_OPEN_READONLY | c.SQLITE_OPEN_URI);
    }

    pub fn openInMemory() !Db {
        var handle: ?*c.sqlite3 = null;
        const rc = c.sqlite3_open_v2(":memory:", &handle, c.SQLITE_OPEN_READWRITE | c.SQLITE_OPEN_CREATE | c.SQLITE_OPEN_MEMORY, null);
        if (rc != c.SQLITE_OK or handle == null) {
            if (handle) |db| _ = c.sqlite3_close(db);
            return error.SqliteOpenFailed;
        }
        return .{ .handle = handle.? };
    }

    pub fn close(self: *Db) void {
        _ = c.sqlite3_close(self.handle);
    }

    pub fn exec(self: *Db, allocator: std.mem.Allocator, sql: []const u8) !void {
        const sql_z = try allocator.dupeZ(u8, sql);
        defer allocator.free(sql_z);

        var err_msg: [*c]u8 = null;
        const rc = c.sqlite3_exec(self.handle, sql_z.ptr, null, null, &err_msg);
        defer if (err_msg != null) c.sqlite3_free(err_msg);
        if (rc != c.SQLITE_OK) return error.SqliteExecFailed;
    }

    pub fn prepare(self: *Db, sql: []const u8) !Stmt {
        var stmt: ?*c.sqlite3_stmt = null;
        const rc = c.sqlite3_prepare_v2(self.handle, sql.ptr, @intCast(sql.len), &stmt, null);
        if (rc != c.SQLITE_OK or stmt == null) return error.SqlitePrepareFailed;
        return .{
            .db = self.handle,
            .handle = stmt.?,
        };
    }

    pub fn changes(self: Db) i32 {
        return c.sqlite3_changes(self.handle);
    }

    pub fn lastInsertRowId(self: Db) i64 {
        return c.sqlite3_last_insert_rowid(self.handle);
    }

    pub fn errmsg(self: Db) []const u8 {
        return std.mem.span(c.sqlite3_errmsg(self.handle));
    }
};

pub const Stmt = struct {
    db: *c.sqlite3,
    handle: *c.sqlite3_stmt,

    pub fn finalize(self: *Stmt) void {
        _ = c.sqlite3_finalize(self.handle);
    }

    pub fn step(self: *Stmt) c_int {
        return c.sqlite3_step(self.handle);
    }

    pub fn reset(self: *Stmt) void {
        _ = c.sqlite3_reset(self.handle);
        _ = c.sqlite3_clear_bindings(self.handle);
    }

    pub fn paramCount(self: Stmt) usize {
        return @intCast(c.sqlite3_bind_parameter_count(self.handle));
    }

    pub fn paramName(self: Stmt, index: usize) ?[]const u8 {
        const raw = c.sqlite3_bind_parameter_name(self.handle, @intCast(index));
        return normalizeParamName(raw);
    }

    pub fn readonly(self: Stmt) bool {
        return c.sqlite3_stmt_readonly(self.handle) != 0;
    }

    pub fn columnCount(self: Stmt) usize {
        return @intCast(c.sqlite3_column_count(self.handle));
    }

    pub fn columnName(self: Stmt, index: usize) []const u8 {
        const raw = c.sqlite3_column_name(self.handle, @intCast(index));
        return if (raw) |ptr| std.mem.span(ptr) else "";
    }

    pub fn columnType(self: Stmt, index: usize) c_int {
        return c.sqlite3_column_type(self.handle, @intCast(index));
    }

    pub fn columnInt64(self: Stmt, index: usize) i64 {
        return c.sqlite3_column_int64(self.handle, @intCast(index));
    }

    pub fn columnDouble(self: Stmt, index: usize) f64 {
        return c.sqlite3_column_double(self.handle, @intCast(index));
    }

    pub fn columnText(self: Stmt, index: usize) []const u8 {
        const ptr = c.sqlite3_column_text(self.handle, @intCast(index)) orelse return "";
        const len: usize = @intCast(c.sqlite3_column_bytes(self.handle, @intCast(index)));
        return ptr[0..len];
    }

    pub fn bindNull(self: *Stmt, index: usize) !void {
        if (c.sqlite3_bind_null(self.handle, @intCast(index)) != c.SQLITE_OK) return error.SqliteBindFailed;
    }

    pub fn bindInt64(self: *Stmt, index: usize, value: i64) !void {
        if (c.sqlite3_bind_int64(self.handle, @intCast(index), value) != c.SQLITE_OK) return error.SqliteBindFailed;
    }

    pub fn bindDouble(self: *Stmt, index: usize, value: f64) !void {
        if (c.sqlite3_bind_double(self.handle, @intCast(index), value) != c.SQLITE_OK) return error.SqliteBindFailed;
    }

    pub fn bindText(self: *Stmt, index: usize, value: []const u8) !void {
        if (c.sqlite3_bind_text(
            self.handle,
            @intCast(index),
            value.ptr,
            @intCast(value.len),
            null,
        ) != c.SQLITE_OK) return error.SqliteBindFailed;
    }

    pub fn errmsg(self: Stmt) []const u8 {
        return std.mem.span(c.sqlite3_errmsg(self.db));
    }
};

pub fn normalizeParamName(raw: ?[*:0]const u8) ?[]const u8 {
    const text = if (raw) |ptr| std.mem.span(ptr) else return null;
    if (text.len == 0) return null;
    return switch (text[0]) {
        ':', '@', '$' => if (text.len > 1) text[1..] else null,
        else => text,
    };
}

fn openPath(allocator: std.mem.Allocator, path: []const u8, flags: c_int) !Db {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    var handle: ?*c.sqlite3 = null;
    const rc = c.sqlite3_open_v2(path_z.ptr, &handle, flags, null);
    if (rc != c.SQLITE_OK or handle == null) {
        if (handle) |db| _ = c.sqlite3_close(db);
        return error.SqliteOpenFailed;
    }
    return .{ .handle = handle.? };
}
