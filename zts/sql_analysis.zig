const std = @import("std");

pub const Operation = enum {
    select,
    insert,
    update,
    delete,

    pub fn toString(self: Operation) []const u8 {
        return switch (self) {
            .select => "select",
            .insert => "insert",
            .update => "update",
            .delete => "delete",
        };
    }
};

pub const StatementInfo = struct {
    operation: Operation,
    tables: std.ArrayList([]const u8),

    pub fn deinit(self: *StatementInfo, allocator: std.mem.Allocator) void {
        for (self.tables.items) |table| allocator.free(table);
        self.tables.deinit(allocator);
    }
};

pub fn analyzeStatement(allocator: std.mem.Allocator, sql: []const u8) !StatementInfo {
    var cursor: usize = 0;
    const first = nextToken(sql, &cursor) orelse return error.UnsupportedSqlStatement;

    if (tokenEq(first, "select")) {
        var info = StatementInfo{ .operation = .select, .tables = .empty };
        errdefer info.deinit(allocator);
        try collectTablesForSelect(allocator, sql, &cursor, &info.tables);
        if (info.tables.items.len == 0) return error.UnsupportedSqlStatement;
        return info;
    }

    if (tokenEq(first, "insert")) {
        var info = StatementInfo{ .operation = .insert, .tables = .empty };
        errdefer info.deinit(allocator);
        try collectTableAfterKeyword(allocator, sql, &cursor, &info.tables, "into");
        if (info.tables.items.len == 0) return error.UnsupportedSqlStatement;
        return info;
    }

    if (tokenEq(first, "update")) {
        var info = StatementInfo{ .operation = .update, .tables = .empty };
        errdefer info.deinit(allocator);
        try appendTable(allocator, &info.tables, nextTableToken(sql, &cursor) orelse return error.UnsupportedSqlStatement);
        return info;
    }

    if (tokenEq(first, "delete")) {
        var info = StatementInfo{ .operation = .delete, .tables = .empty };
        errdefer info.deinit(allocator);
        try collectTableAfterKeyword(allocator, sql, &cursor, &info.tables, "from");
        if (info.tables.items.len == 0) return error.UnsupportedSqlStatement;
        return info;
    }

    return error.UnsupportedSqlStatement;
}

fn collectTablesForSelect(
    allocator: std.mem.Allocator,
    sql: []const u8,
    cursor: *usize,
    tables: *std.ArrayList([]const u8),
) !void {
    while (nextToken(sql, cursor)) |tok| {
        if (tokenEq(tok, "from") or tokenEq(tok, "join")) {
            const table = nextTableToken(sql, cursor) orelse return error.UnsupportedSqlStatement;
            try appendTable(allocator, tables, table);
        }
    }
}

fn collectTableAfterKeyword(
    allocator: std.mem.Allocator,
    sql: []const u8,
    cursor: *usize,
    tables: *std.ArrayList([]const u8),
    keyword: []const u8,
) !void {
    while (nextToken(sql, cursor)) |tok| {
        if (tokenEq(tok, keyword)) {
            const table = nextTableToken(sql, cursor) orelse return error.UnsupportedSqlStatement;
            try appendTable(allocator, tables, table);
            return;
        }
    }
}

fn nextTableToken(sql: []const u8, cursor: *usize) ?[]const u8 {
    const tok = nextToken(sql, cursor) orelse return null;
    if (tok.len == 1 and tok[0] == '(') return null;
    return tok;
}

fn appendTable(allocator: std.mem.Allocator, tables: *std.ArrayList([]const u8), table: []const u8) !void {
    if (table.len == 0) return;
    for (tables.items) |existing| {
        if (std.ascii.eqlIgnoreCase(existing, table)) return;
    }
    try tables.append(allocator, try allocator.dupe(u8, table));
}

fn tokenEq(token: []const u8, keyword: []const u8) bool {
    return std.ascii.eqlIgnoreCase(token, keyword);
}

fn nextToken(sql: []const u8, cursor: *usize) ?[]const u8 {
    skipTrivia(sql, cursor);
    if (cursor.* >= sql.len) return null;

    const start = cursor.*;
    const ch = sql[cursor.*];
    if (isPunctuation(ch)) {
        cursor.* += 1;
        return sql[start..cursor.*];
    }

    while (cursor.* < sql.len) : (cursor.* += 1) {
        const c = sql[cursor.*];
        if (!isIdentifierChar(c)) break;
    }

    if (cursor.* == start) {
        cursor.* += 1;
        return sql[start..cursor.*];
    }
    return sql[start..cursor.*];
}

fn skipTrivia(sql: []const u8, cursor: *usize) void {
    while (cursor.* < sql.len) {
        switch (sql[cursor.*]) {
            ' ', '\t', '\n', '\r' => cursor.* += 1,
            '\'' => skipQuoted(sql, cursor, '\''),
            '"' => skipQuoted(sql, cursor, '"'),
            '`' => skipQuoted(sql, cursor, '`'),
            '[' => skipBracketIdent(sql, cursor),
            '-' => {
                if (cursor.* + 1 < sql.len and sql[cursor.* + 1] == '-') {
                    cursor.* += 2;
                    while (cursor.* < sql.len and sql[cursor.*] != '\n') : (cursor.* += 1) {}
                } else {
                    return;
                }
            },
            '/' => {
                if (cursor.* + 1 < sql.len and sql[cursor.* + 1] == '*') {
                    cursor.* += 2;
                    while (cursor.* + 1 < sql.len and !(sql[cursor.*] == '*' and sql[cursor.* + 1] == '/')) : (cursor.* += 1) {}
                    if (cursor.* + 1 < sql.len) cursor.* += 2;
                } else {
                    return;
                }
            },
            else => return,
        }
    }
}

fn skipQuoted(sql: []const u8, cursor: *usize, quote: u8) void {
    cursor.* += 1;
    while (cursor.* < sql.len) : (cursor.* += 1) {
        if (sql[cursor.*] == quote) {
            cursor.* += 1;
            break;
        }
        if (sql[cursor.*] == '\\' and cursor.* + 1 < sql.len) {
            cursor.* += 1;
        }
    }
}

fn skipBracketIdent(sql: []const u8, cursor: *usize) void {
    cursor.* += 1;
    while (cursor.* < sql.len) : (cursor.* += 1) {
        if (sql[cursor.*] == ']') {
            cursor.* += 1;
            break;
        }
    }
}

fn isPunctuation(ch: u8) bool {
    return ch == '(' or ch == ')' or ch == ',' or ch == ';';
}

fn isIdentifierChar(ch: u8) bool {
    return std.ascii.isAlphanumeric(ch) or ch == '_' or ch == '.';
}

test "analyze select with join" {
    var info = try analyzeStatement(std.testing.allocator, "SELECT * FROM users JOIN accounts ON accounts.user_id = users.id");
    defer info.deinit(std.testing.allocator);

    try std.testing.expectEqual(Operation.select, info.operation);
    try std.testing.expectEqual(@as(usize, 2), info.tables.items.len);
    try std.testing.expectEqualStrings("users", info.tables.items[0]);
    try std.testing.expectEqualStrings("accounts", info.tables.items[1]);
}

test "analyze insert/update/delete" {
    var insert_info = try analyzeStatement(std.testing.allocator, "INSERT INTO todos(id, text) VALUES (:id, :text)");
    defer insert_info.deinit(std.testing.allocator);
    try std.testing.expectEqual(Operation.insert, insert_info.operation);
    try std.testing.expectEqualStrings("todos", insert_info.tables.items[0]);

    var update_info = try analyzeStatement(std.testing.allocator, "UPDATE todos SET done = 1 WHERE id = :id");
    defer update_info.deinit(std.testing.allocator);
    try std.testing.expectEqual(Operation.update, update_info.operation);
    try std.testing.expectEqualStrings("todos", update_info.tables.items[0]);

    var delete_info = try analyzeStatement(std.testing.allocator, "DELETE FROM todos WHERE id = :id");
    defer delete_info.deinit(std.testing.allocator);
    try std.testing.expectEqual(Operation.delete, delete_info.operation);
    try std.testing.expectEqualStrings("todos", delete_info.tables.items[0]);
}
