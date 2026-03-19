//! Handler Trace Recording and Replay
//!
//! Records every I/O boundary during handler execution as JSONL,
//! enabling exact reproduction of any request offline.
//!
//! Since virtual modules are the ONLY I/O boundary in zigttp's restricted
//! JS subset, recording their inputs/outputs captures ALL external state
//! a handler depends on. This makes handlers deterministic pure functions
//! of (Request, VirtualModuleResponses).

const std = @import("std");
const context = @import("context.zig");
const value = @import("value.zig");
const object = @import("object.zig");
const string = @import("string.zig");

/// Module state slot for trace recorder (slot 7, after io at 6).
pub const TRACE_STATE_SLOT = 7;

/// Mutex wrapper compatible with Zig 0.16's Io.Mutex.
pub const TraceMutex = struct {
    inner: std.Io.Mutex = .init,

    pub fn lock(self: *TraceMutex) void {
        self.inner.lockUncancelable(std.Options.debug_io);
    }

    pub fn unlock(self: *TraceMutex) void {
        self.inner.unlock(std.Options.debug_io);
    }
};

/// Per-request trace recorder.
/// Accumulates JSONL lines in a buffer during handler execution,
/// then flushes atomically to the shared output file.
pub const TraceRecorder = struct {
    /// Allocator for buffer operations.
    allocator: std.mem.Allocator,
    /// Buffer accumulating JSONL lines for the current request.
    buf: std.ArrayList(u8),
    /// Shared output file descriptor (owned by the Runtime, not the recorder).
    output_fd: std.c.fd_t,
    /// Mutex protecting concurrent writes to the output file.
    output_mutex: *TraceMutex,
    /// Whether recording is active (allows disabling mid-request on error).
    active: bool,
    /// Sequence counter for I/O calls within this request.
    io_seq: u32,

    pub fn init(allocator: std.mem.Allocator, output_fd: std.c.fd_t, output_mutex: *TraceMutex) TraceRecorder {
        return .{
            .allocator = allocator,
            .buf = .empty,
            .output_fd = output_fd,
            .output_mutex = output_mutex,
            .active = true,
            .io_seq = 0,
        };
    }

    pub fn deinit(self: *TraceRecorder) void {
        self.buf.deinit(self.allocator);
    }

    pub fn deinitOpaque(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        _ = allocator;
        const self: *TraceRecorder = @ptrCast(@alignCast(ptr));
        self.deinit();
    }

    /// Record the incoming HTTP request from raw parts (used by zruntime).
    pub fn recordRequestRaw(
        self: *TraceRecorder,
        method: []const u8,
        url: []const u8,
        header_names: []const []const u8,
        header_values: []const []const u8,
        body: ?[]const u8,
    ) void {
        if (!self.active) return;
        self.io_seq = 0;

        self.append("{\"type\":\"request\",\"method\":\"") orelse return;
        self.appendEscaped(method) orelse return;
        self.append("\",\"url\":\"") orelse return;
        self.appendEscaped(url) orelse return;
        self.append("\",\"headers\":{") orelse return;

        const count = @min(header_names.len, header_values.len);
        for (0..count) |i| {
            if (i > 0) self.appendByte(',') orelse return;
            self.appendByte('"') orelse return;
            self.appendEscaped(header_names[i]) orelse return;
            self.append("\":\"") orelse return;
            self.appendEscaped(header_values[i]) orelse return;
            self.appendByte('"') orelse return;
        }

        self.append("},\"body\":") orelse return;
        if (body) |b| {
            self.appendByte('"') orelse return;
            self.appendEscaped(b) orelse return;
            self.appendByte('"') orelse return;
        } else {
            self.append("null") orelse return;
        }
        self.append("}\n") orelse return;
    }

    /// Record a virtual module I/O call.
    pub fn recordIO(
        self: *TraceRecorder,
        module_name: []const u8,
        fn_name: []const u8,
        ctx_ptr: *context.Context,
        args: []const value.JSValue,
        result: value.JSValue,
    ) void {
        if (!self.active) return;

        self.append("{\"type\":\"io\",\"seq\":") orelse return;
        self.appendInt(self.io_seq) orelse return;
        self.append(",\"module\":\"") orelse return;
        self.appendEscaped(module_name) orelse return;
        self.append("\",\"fn\":\"") orelse return;
        self.appendEscaped(fn_name) orelse return;
        self.append("\",\"args\":[") orelse return;

        for (args, 0..) |arg, i| {
            if (i > 0) self.appendByte(',') orelse return;
            self.appendJSValue(ctx_ptr, arg) orelse return;
        }

        self.append("],\"result\":") orelse return;
        self.appendJSValue(ctx_ptr, result) orelse return;
        self.append("}\n") orelse return;

        self.io_seq += 1;
    }

    /// Record the outgoing HTTP response.
    pub fn recordResponse(
        self: *TraceRecorder,
        status: u16,
        header_names: []const []const u8,
        header_values: []const []const u8,
        body: []const u8,
    ) void {
        if (!self.active) return;

        self.append("{\"type\":\"response\",\"status\":") orelse return;
        self.appendInt(status) orelse return;
        self.append(",\"headers\":{") orelse return;

        const count = @min(header_names.len, header_values.len);
        for (0..count) |i| {
            if (i > 0) self.appendByte(',') orelse return;
            self.appendByte('"') orelse return;
            self.appendEscaped(header_names[i]) orelse return;
            self.append("\":\"") orelse return;
            self.appendEscaped(header_values[i]) orelse return;
            self.appendByte('"') orelse return;
        }

        self.append("},\"body\":\"") orelse return;
        self.appendEscaped(body) orelse return;
        self.append("\"}\n") orelse return;
    }

    /// Record metadata at the end of the request.
    pub fn recordMeta(
        self: *TraceRecorder,
        duration_us: u64,
        handler_name: []const u8,
        pool_slot: u32,
    ) void {
        if (!self.active) return;

        self.append("{\"type\":\"meta\",\"duration_us\":") orelse return;
        self.appendFmt("{d}", .{duration_us}) orelse return;
        self.append(",\"handler\":\"") orelse return;
        self.appendEscaped(handler_name) orelse return;
        self.append("\",\"pool_slot\":") orelse return;
        self.appendInt(pool_slot) orelse return;
        self.append(",\"io_count\":") orelse return;
        self.appendInt(self.io_seq) orelse return;
        self.append("}\n") orelse return;
    }

    /// Flush accumulated trace lines to the output file.
    pub fn flush(self: *TraceRecorder) void {
        if (self.buf.items.len == 0) return;

        self.output_mutex.lock();
        defer self.output_mutex.unlock();

        var remaining = self.buf.items;
        while (remaining.len > 0) {
            const result = std.c.write(self.output_fd, remaining.ptr, remaining.len);
            if (result < 0) break;
            remaining = remaining[@intCast(result)..];
        }

        self.buf.clearRetainingCapacity();
    }

    /// Reset for next request (keep buffer capacity).
    pub fn reset(self: *TraceRecorder) void {
        self.buf.clearRetainingCapacity();
        self.active = true;
        self.io_seq = 0;
    }

    // -- Buffer helpers (return null on OOM to deactivate) --

    fn append(self: *TraceRecorder, data: []const u8) ?void {
        self.buf.appendSlice(self.allocator, data) catch {
            self.active = false;
            return null;
        };
    }

    fn appendByte(self: *TraceRecorder, byte: u8) ?void {
        self.buf.append(self.allocator, byte) catch {
            self.active = false;
            return null;
        };
    }

    fn appendInt(self: *TraceRecorder, val: anytype) ?void {
        self.appendFmt("{d}", .{val}) orelse return null;
    }

    fn appendFmt(self: *TraceRecorder, comptime fmt: []const u8, args: anytype) ?void {
        var tmp: [64]u8 = undefined;
        const slice = std.fmt.bufPrint(&tmp, fmt, args) catch {
            self.active = false;
            return null;
        };
        self.append(slice) orelse return null;
    }

    fn appendEscaped(self: *TraceRecorder, data: []const u8) ?void {
        for (data) |c| {
            switch (c) {
                '"' => self.append("\\\"") orelse return null,
                '\\' => self.append("\\\\") orelse return null,
                '\n' => self.append("\\n") orelse return null,
                '\r' => self.append("\\r") orelse return null,
                '\t' => self.append("\\t") orelse return null,
                else => {
                    if (c < 0x20) {
                        const hex = "0123456789abcdef";
                        self.append("\\u00") orelse return null;
                        self.appendByte(hex[c >> 4]) orelse return null;
                        self.appendByte(hex[c & 0xF]) orelse return null;
                    } else {
                        self.appendByte(c) orelse return null;
                    }
                },
            }
        }
    }

    fn appendJSValue(self: *TraceRecorder, ctx: *context.Context, val: value.JSValue) ?void {
        if (val.isNull() or val.isUndefined()) {
            self.append("null") orelse return null;
        } else if (val.isTrue()) {
            self.append("true") orelse return null;
        } else if (val.isFalse()) {
            self.append("false") orelse return null;
        } else if (val.isInt()) {
            self.appendInt(val.getInt()) orelse return null;
        } else if (val.isFloat64()) {
            const f = val.getFloat64();
            if (std.math.isNan(f) or std.math.isInf(f)) {
                self.append("null") orelse return null;
            } else {
                self.appendFmt("{d}", .{f}) orelse return null;
            }
        } else if (val.isAnyString()) {
            self.appendByte('"') orelse return null;
            const data = extractStringData(val) orelse "";
            self.appendEscaped(data) orelse return null;
            self.appendByte('"') orelse return null;
        } else if (val.isObject()) {
            const obj = object.JSObject.fromValue(val);
            if (obj.class_id == .array) {
                self.appendByte('[') orelse return null;
                const len = obj.getArrayLength();
                for (0..len) |i| {
                    if (i > 0) self.appendByte(',') orelse return null;
                    if (obj.getIndex(@intCast(i))) |elem| {
                        self.appendJSValue(ctx, elem) orelse return null;
                    } else {
                        self.append("null") orelse return null;
                    }
                }
                self.appendByte(']') orelse return null;
            } else {
                self.appendByte('{') orelse return null;
                var first = true;
                const pool = ctx.hidden_class_pool orelse {
                    self.appendByte('}') orelse return null;
                    return;
                };
                const class_idx = obj.hidden_class_idx;
                if (!class_idx.isNone()) {
                    const idx = class_idx.toInt();
                    if (idx < pool.count) {
                        const prop_count = pool.property_counts.items[idx];
                        if (prop_count > 0) {
                            const start = pool.properties_starts.items[idx];
                            if (start + prop_count <= pool.property_names.items.len and
                                start + prop_count <= pool.property_offsets.items.len)
                            {
                                for (0..prop_count) |pi| {
                                    const atom = pool.property_names.items[start + pi];
                                    const offset = pool.property_offsets.items[start + pi];
                                    const prop_val = obj.getSlot(offset);
                                    if (prop_val.isUndefined()) continue;
                                    if (!first) self.appendByte(',') orelse return null;
                                    first = false;
                                    self.appendByte('"') orelse return null;
                                    const name = atomToString(atom, ctx) orelse "?";
                                    self.appendEscaped(name) orelse return null;
                                    self.append("\":") orelse return null;
                                    self.appendJSValue(ctx, prop_val) orelse return null;
                                }
                            }
                        }
                    }
                }
                self.appendByte('}') orelse return null;
            }
        } else {
            self.append("null") orelse return null;
        }
    }
};

pub fn extractStringData(val: value.JSValue) ?[]const u8 {
    if (val.isString()) return val.toPtr(string.JSString).data();
    if (val.isStringSlice()) return val.toPtr(string.SliceString).data();
    if (val.isRope()) {
        const rope = val.toPtr(string.RopeNode);
        if (rope.asLeaf()) |leaf| return leaf.data();
    }
    return null;
}

fn atomToString(atom: object.Atom, ctx: *context.Context) ?[]const u8 {
    if (atom.isPredefined()) return atom.toPredefinedName();
    return ctx.atoms.getName(atom);
}

// ============================================================================
// Comptime NativeFn Wrapper Generator
// ============================================================================

/// Generate a tracing NativeFn wrapper at compile time.
pub fn makeTracingWrapper(
    comptime module_name: []const u8,
    comptime fn_name: []const u8,
    comptime original_fn: object.NativeFn,
) object.NativeFn {
    return struct {
        fn call(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
            const result = try original_fn(ctx_ptr, this, args);
            const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
            if (ctx.getModuleState(TraceRecorder, TRACE_STATE_SLOT)) |recorder| {
                recorder.recordIO(module_name, fn_name, ctx, args, result);
            }
            return result;
        }
    }.call;
}

// ============================================================================
// Replay Support - Trace Parsing
// ============================================================================

pub const TraceEntry = union(enum) {
    request: RequestTrace,
    io: IoEntry,
    response: ResponseTrace,
    meta: MetaTrace,
    unknown: void,
};

pub const RequestTrace = struct {
    method: []const u8,
    url: []const u8,
    headers_json: []const u8,
    body: ?[]const u8,
};

pub const IoEntry = struct {
    seq: u32,
    module: []const u8,
    func: []const u8,
    args_json: []const u8,
    result_json: []const u8,
};

pub const ResponseTrace = struct {
    status: u16,
    headers_json: []const u8,
    body: []const u8,
};

pub const MetaTrace = struct {
    duration_us: u64,
    handler: []const u8,
    pool_slot: u32,
    io_count: u32,
};

pub const RequestTraceGroup = struct {
    request: RequestTrace,
    io_calls: []const IoEntry,
    response: ?ResponseTrace,
    meta: ?MetaTrace,
};

/// Parse a JSONL trace file into grouped request traces.
pub fn parseTraceFile(allocator: std.mem.Allocator, source: []const u8) ![]RequestTraceGroup {
    var groups: std.ArrayList(RequestTraceGroup) = .empty;
    errdefer groups.deinit(allocator);

    var current_io: std.ArrayList(IoEntry) = .empty;
    defer current_io.deinit(allocator);

    var current_request: ?RequestTrace = null;
    var current_response: ?ResponseTrace = null;
    var current_meta: ?MetaTrace = null;

    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        const entry = parseTraceLine(line) catch continue;
        switch (entry) {
            .request => |req| {
                if (current_request != null) {
                    try groups.append(allocator, .{
                        .request = current_request.?,
                        .io_calls = try current_io.toOwnedSlice(allocator),
                        .response = current_response,
                        .meta = current_meta,
                    });
                    current_response = null;
                    current_meta = null;
                }
                current_request = req;
            },
            .io => |io| try current_io.append(allocator, io),
            .response => |resp| current_response = resp,
            .meta => |meta| {
                current_meta = meta;
                if (current_request != null) {
                    try groups.append(allocator, .{
                        .request = current_request.?,
                        .io_calls = try current_io.toOwnedSlice(allocator),
                        .response = current_response,
                        .meta = current_meta,
                    });
                    current_request = null;
                    current_response = null;
                    current_meta = null;
                }
            },
            .unknown => {},
        }
    }

    if (current_request != null) {
        try groups.append(allocator, .{
            .request = current_request.?,
            .io_calls = try current_io.toOwnedSlice(allocator),
            .response = current_response,
            .meta = current_meta,
        });
    }

    return try groups.toOwnedSlice(allocator);
}

fn parseTraceLine(line: []const u8) !TraceEntry {
    const type_str = findJsonStringValue(line, "\"type\"") orelse return .unknown;

    if (std.mem.eql(u8, type_str, "request")) {
        return .{ .request = .{
            .method = findJsonStringValue(line, "\"method\"") orelse "GET",
            .url = findJsonStringValue(line, "\"url\"") orelse "/",
            .headers_json = findJsonObjectValue(line, "\"headers\"") orelse "{}",
            .body = findJsonStringValue(line, "\"body\""),
        } };
    } else if (std.mem.eql(u8, type_str, "io")) {
        return .{ .io = .{
            .seq = @intCast(findJsonIntValue(line, "\"seq\"") orelse 0),
            .module = findJsonStringValue(line, "\"module\"") orelse "",
            .func = findJsonStringValue(line, "\"fn\"") orelse "",
            .args_json = findJsonArrayValue(line, "\"args\"") orelse "[]",
            .result_json = findJsonAnyValue(line, "\"result\"") orelse "null",
        } };
    } else if (std.mem.eql(u8, type_str, "response")) {
        return .{ .response = .{
            .status = @intCast(findJsonIntValue(line, "\"status\"") orelse 200),
            .headers_json = findJsonObjectValue(line, "\"headers\"") orelse "{}",
            .body = findJsonStringValue(line, "\"body\"") orelse "",
        } };
    } else if (std.mem.eql(u8, type_str, "meta")) {
        return .{ .meta = .{
            .duration_us = @intCast(findJsonIntValue(line, "\"duration_us\"") orelse 0),
            .handler = findJsonStringValue(line, "\"handler\"") orelse "",
            .pool_slot = @intCast(findJsonIntValue(line, "\"pool_slot\"") orelse 0),
            .io_count = @intCast(findJsonIntValue(line, "\"io_count\"") orelse 0),
        } };
    }
    return .unknown;
}

// ============================================================================
// Minimal JSON Field Extraction (zero-copy)
// ============================================================================

fn findJsonStringValue(json: []const u8, key: []const u8) ?[]const u8 {
    const key_pos = std.mem.indexOf(u8, json, key) orelse return null;
    var pos = key_pos + key.len;
    while (pos < json.len and (json[pos] == ':' or json[pos] == ' ')) : (pos += 1) {}
    if (pos >= json.len) return null;
    if (pos + 4 <= json.len and std.mem.eql(u8, json[pos .. pos + 4], "null")) return null;
    if (json[pos] != '"') return null;
    pos += 1;
    const start = pos;
    while (pos < json.len) : (pos += 1) {
        if (json[pos] == '\\') { pos += 1; continue; }
        if (json[pos] == '"') return json[start..pos];
    }
    return null;
}

fn findJsonIntValue(json: []const u8, key: []const u8) ?i64 {
    const key_pos = std.mem.indexOf(u8, json, key) orelse return null;
    var pos = key_pos + key.len;
    while (pos < json.len and (json[pos] == ':' or json[pos] == ' ')) : (pos += 1) {}
    if (pos >= json.len) return null;
    const start = pos;
    if (pos < json.len and json[pos] == '-') pos += 1;
    while (pos < json.len and json[pos] >= '0' and json[pos] <= '9') : (pos += 1) {}
    if (pos == start) return null;
    return std.fmt.parseInt(i64, json[start..pos], 10) catch null;
}

fn findJsonObjectValue(json: []const u8, key: []const u8) ?[]const u8 {
    const key_pos = std.mem.indexOf(u8, json, key) orelse return null;
    var pos = key_pos + key.len;
    while (pos < json.len and (json[pos] == ':' or json[pos] == ' ')) : (pos += 1) {}
    if (pos >= json.len or json[pos] != '{') return null;
    return findMatchingBrace(json, pos, '{', '}');
}

fn findJsonArrayValue(json: []const u8, key: []const u8) ?[]const u8 {
    const key_pos = std.mem.indexOf(u8, json, key) orelse return null;
    var pos = key_pos + key.len;
    while (pos < json.len and (json[pos] == ':' or json[pos] == ' ')) : (pos += 1) {}
    if (pos >= json.len or json[pos] != '[') return null;
    return findMatchingBrace(json, pos, '[', ']');
}

fn findJsonAnyValue(json: []const u8, key: []const u8) ?[]const u8 {
    const key_pos = std.mem.indexOf(u8, json, key) orelse return null;
    var pos = key_pos + key.len;
    while (pos < json.len and (json[pos] == ':' or json[pos] == ' ')) : (pos += 1) {}
    if (pos >= json.len) return null;
    return switch (json[pos]) {
        '"' => blk: {
            var p = pos + 1;
            while (p < json.len) : (p += 1) {
                if (json[p] == '\\') { p += 1; continue; }
                if (json[p] == '"') break :blk json[pos .. p + 1];
            }
            break :blk null;
        },
        '{' => findMatchingBrace(json, pos, '{', '}'),
        '[' => findMatchingBrace(json, pos, '[', ']'),
        else => blk: {
            const start = pos;
            while (pos < json.len and json[pos] != ',' and json[pos] != '}' and json[pos] != ']') : (pos += 1) {}
            break :blk json[start..pos];
        },
    };
}

fn findMatchingBrace(json: []const u8, start: usize, open: u8, close: u8) ?[]const u8 {
    var depth: u32 = 0;
    var pos = start;
    var in_string = false;
    while (pos < json.len) : (pos += 1) {
        if (in_string) {
            if (json[pos] == '\\') { pos += 1; continue; }
            if (json[pos] == '"') in_string = false;
            continue;
        }
        if (json[pos] == '"') { in_string = true; continue; }
        if (json[pos] == open) depth += 1 else if (json[pos] == close) {
            depth -= 1;
            if (depth == 0) return json[start .. pos + 1];
        }
    }
    return null;
}

// ============================================================================
// Replay Support - Stub Functions
// ============================================================================

/// Module state slot for replay state (slot 3, separate from trace at 7).
pub const REPLAY_STATE_SLOT = 3;

/// Per-request replay state. Holds the recorded I/O entries and a cursor
/// tracking which entry to return next.
pub const ReplayState = struct {
    io_calls: []const IoEntry,
    cursor: u32,
    divergences: u32,

    /// Consume the next I/O entry, verifying module and function name.
    /// Increments divergence count on mismatch or exhaustion.
    pub fn nextIO(self: *ReplayState, module_name: []const u8, fn_name: []const u8) ?IoEntry {
        if (self.cursor >= self.io_calls.len) {
            self.divergences += 1;
            return null;
        }
        const entry = self.io_calls[self.cursor];
        self.cursor += 1;
        if (!std.mem.eql(u8, entry.module, module_name) or !std.mem.eql(u8, entry.func, fn_name)) {
            self.divergences += 1;
        }
        return entry;
    }

    pub fn deinitOpaque(_: *anyopaque, _: std.mem.Allocator) void {
        // ReplayState does not own its io_calls (owned by trace parser).
    }
};

/// Generate a replay stub NativeFn at compile time.
/// The stub reads the next recorded I/O entry from the ReplayState in
/// module_state and converts the result_json back to a JSValue.
pub fn makeReplayStub(
    comptime module_name: []const u8,
    comptime fn_name: []const u8,
) object.NativeFn {
    return struct {
        fn call(ctx_ptr: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
            const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
            const state = ctx.getModuleState(ReplayState, REPLAY_STATE_SLOT) orelse {
                return value.JSValue.undefined_val;
            };
            const entry = state.nextIO(module_name, fn_name) orelse {
                return value.JSValue.undefined_val;
            };
            return jsonToJSValue(ctx, entry.result_json);
        }
    }.call;
}

// ============================================================================
// JSON to JSValue Parser (for replay)
// ============================================================================

const ParseResult = struct {
    val: value.JSValue,
    end: usize,
};

/// Parse a JSON string into a JSValue. Used by replay stubs to reconstruct
/// recorded return values from their JSONL representation.
pub fn jsonToJSValue(ctx: *context.Context, json: []const u8) value.JSValue {
    if (json.len == 0) return value.JSValue.undefined_val;
    const result = parseValue(ctx, json, 0);
    return result.val;
}

fn parseValue(ctx: *context.Context, json: []const u8, start: usize) ParseResult {
    const pos = skipWhitespace(json, start);
    if (pos >= json.len) return .{ .val = value.JSValue.undefined_val, .end = pos };

    return switch (json[pos]) {
        '"' => parseString(ctx, json, pos),
        '{' => parseObject(ctx, json, pos),
        '[' => parseArray(ctx, json, pos),
        't' => .{ .val = value.JSValue.true_val, .end = @min(pos + 4, json.len) },
        'f' => .{ .val = value.JSValue.false_val, .end = @min(pos + 5, json.len) },
        'n' => .{ .val = value.JSValue.undefined_val, .end = @min(pos + 4, json.len) },
        '-', '0'...'9' => parseNumber(ctx, json, pos),
        else => .{ .val = value.JSValue.undefined_val, .end = pos + 1 },
    };
}

fn parseString(ctx: *context.Context, json: []const u8, start: usize) ParseResult {
    // start points to opening quote
    var pos = start + 1;
    // Fast path: no escapes
    const content_start = pos;
    var has_escapes = false;
    while (pos < json.len) : (pos += 1) {
        if (json[pos] == '\\') {
            has_escapes = true;
            pos += 1;
            continue;
        }
        if (json[pos] == '"') {
            if (!has_escapes) {
                const str_data = json[content_start..pos];
                const js_str = ctx.createString(str_data) catch return .{ .val = value.JSValue.undefined_val, .end = pos + 1 };
                return .{ .val = js_str, .end = pos + 1 };
            }
            // Slow path: unescape
            const unescaped = unescapeJson(ctx.allocator, json[content_start..pos]) catch return .{ .val = value.JSValue.undefined_val, .end = pos + 1 };
            defer ctx.allocator.free(unescaped);
            const js_str = ctx.createString(unescaped) catch return .{ .val = value.JSValue.undefined_val, .end = pos + 1 };
            return .{ .val = js_str, .end = pos + 1 };
        }
    }
    return .{ .val = value.JSValue.undefined_val, .end = pos };
}

fn parseNumber(ctx: *context.Context, json: []const u8, start: usize) ParseResult {
    var pos = start;
    var is_float = false;
    if (pos < json.len and json[pos] == '-') pos += 1;
    while (pos < json.len and json[pos] >= '0' and json[pos] <= '9') : (pos += 1) {}
    if (pos < json.len and json[pos] == '.') {
        is_float = true;
        pos += 1;
        while (pos < json.len and json[pos] >= '0' and json[pos] <= '9') : (pos += 1) {}
    }
    if (pos < json.len and (json[pos] == 'e' or json[pos] == 'E')) {
        is_float = true;
        pos += 1;
        if (pos < json.len and (json[pos] == '+' or json[pos] == '-')) pos += 1;
        while (pos < json.len and json[pos] >= '0' and json[pos] <= '9') : (pos += 1) {}
    }

    const num_str = json[start..pos];
    if (!is_float) {
        // Try integer first (i32 range for NaN-boxing)
        if (std.fmt.parseInt(i32, num_str, 10)) |int_val| {
            return .{ .val = value.JSValue.fromInt(int_val), .end = pos };
        } else |_| {}
    }
    // Fall back to float
    const f = std.fmt.parseFloat(f64, num_str) catch {
        return .{ .val = value.JSValue.undefined_val, .end = pos };
    };
    return .{ .val = allocFloat(ctx, f), .end = pos };
}

fn parseArray(ctx: *context.Context, json: []const u8, start: usize) ParseResult {
    const pool = ctx.hidden_class_pool orelse return .{ .val = value.JSValue.undefined_val, .end = start + 1 };
    const arr = object.JSObject.create(ctx.allocator, ctx.root_class_idx, null, pool) catch {
        return .{ .val = value.JSValue.undefined_val, .end = start + 1 };
    };
    arr.class_id = .array;
    ctx.builtin_objects.append(ctx.allocator, arr) catch {};

    var pos = start + 1; // skip '['
    var first = true;
    while (pos < json.len) {
        pos = skipWhitespace(json, pos);
        if (pos >= json.len or json[pos] == ']') {
            pos += 1;
            break;
        }
        if (!first) {
            if (pos < json.len and json[pos] == ',') pos += 1;
            pos = skipWhitespace(json, pos);
        }
        first = false;
        if (pos >= json.len or json[pos] == ']') {
            pos += 1;
            break;
        }
        const elem = parseValue(ctx, json, pos);
        pos = elem.end;
        arr.arrayPush(ctx.allocator, elem.val) catch {};
    }
    return .{ .val = arr.toValue(), .end = pos };
}

fn parseObject(ctx: *context.Context, json: []const u8, start: usize) ParseResult {
    const pool = ctx.hidden_class_pool orelse return .{ .val = value.JSValue.undefined_val, .end = start + 1 };
    const obj = object.JSObject.create(ctx.allocator, ctx.root_class_idx, null, pool) catch {
        return .{ .val = value.JSValue.undefined_val, .end = start + 1 };
    };
    ctx.builtin_objects.append(ctx.allocator, obj) catch {};

    var pos = start + 1; // skip '{'
    var first = true;
    while (pos < json.len) {
        pos = skipWhitespace(json, pos);
        if (pos >= json.len or json[pos] == '}') {
            pos += 1;
            break;
        }
        if (!first) {
            if (pos < json.len and json[pos] == ',') pos += 1;
            pos = skipWhitespace(json, pos);
        }
        first = false;
        if (pos >= json.len or json[pos] == '}') {
            pos += 1;
            break;
        }
        // Parse key (must be a string)
        if (json[pos] != '"') {
            pos += 1;
            continue;
        }
        const key_result = parseRawString(json, pos);
        pos = key_result.end;
        const key_str = key_result.data;

        // Skip colon
        pos = skipWhitespace(json, pos);
        if (pos < json.len and json[pos] == ':') pos += 1;

        // Parse value
        const val_result = parseValue(ctx, json, pos);
        pos = val_result.end;

        // Set property on object
        const atom = ctx.atoms.intern(key_str) catch continue;
        obj.setProperty(ctx.allocator, pool, atom, val_result.val) catch {};
    }
    return .{ .val = obj.toValue(), .end = pos };
}

const RawStringResult = struct {
    data: []const u8,
    end: usize,
};

/// Extract the raw content of a JSON string (without unescaping).
/// Returns the slice between the quotes.
fn parseRawString(json: []const u8, start: usize) RawStringResult {
    var pos = start + 1; // skip opening quote
    const content_start = pos;
    while (pos < json.len) : (pos += 1) {
        if (json[pos] == '\\') {
            pos += 1;
            continue;
        }
        if (json[pos] == '"') {
            return .{ .data = json[content_start..pos], .end = pos + 1 };
        }
    }
    return .{ .data = json[content_start..pos], .end = pos };
}

fn skipWhitespace(json: []const u8, start: usize) usize {
    var pos = start;
    while (pos < json.len and (json[pos] == ' ' or json[pos] == '\t' or json[pos] == '\n' or json[pos] == '\r')) : (pos += 1) {}
    return pos;
}

/// Unescape a JSON string value (convert \" to ", \n to newline, etc.).
pub fn unescapeJson(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var result: std.ArrayList(u8) = .empty;
    errdefer result.deinit(allocator);
    var i: usize = 0;
    while (i < input.len) : (i += 1) {
        if (input[i] == '\\' and i + 1 < input.len) {
            i += 1;
            try result.append(allocator, switch (input[i]) {
                '"' => '"',
                '\\' => '\\',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '/' => '/',
                else => input[i],
            });
        } else {
            try result.append(allocator, input[i]);
        }
    }
    return try result.toOwnedSlice(allocator);
}

fn allocFloat(_: *context.Context, f: f64) value.JSValue {
    // NaN-boxing: all f64 values stored inline, no heap allocation.
    if (!std.math.isNan(f) and !std.math.isInf(f) and @floor(f) == f and f >= -2147483648 and f <= 2147483647) {
        return value.JSValue.fromInt(@intFromFloat(f));
    }
    return value.JSValue.fromFloat(f);
}

// ============================================================================
// Tests
// ============================================================================

test "findJsonStringValue" {
    const line = "{\"type\":\"request\",\"method\":\"GET\",\"url\":\"/api/test\"}";
    try std.testing.expectEqualStrings("request", findJsonStringValue(line, "\"type\"").?);
    try std.testing.expectEqualStrings("GET", findJsonStringValue(line, "\"method\"").?);
    try std.testing.expectEqualStrings("/api/test", findJsonStringValue(line, "\"url\"").?);
    try std.testing.expect(findJsonStringValue(line, "\"missing\"") == null);
}

test "findJsonIntValue" {
    const line = "{\"type\":\"response\",\"status\":200,\"seq\":5}";
    try std.testing.expectEqual(@as(i64, 200), findJsonIntValue(line, "\"status\"").?);
    try std.testing.expectEqual(@as(i64, 5), findJsonIntValue(line, "\"seq\"").?);
}

test "findJsonObjectValue" {
    const line = "{\"type\":\"request\",\"headers\":{\"host\":\"localhost\"},\"body\":null}";
    try std.testing.expectEqualStrings("{\"host\":\"localhost\"}", findJsonObjectValue(line, "\"headers\"").?);
}

test "findJsonArrayValue" {
    const line = "{\"type\":\"io\",\"args\":[\"key\",\"value\"],\"result\":true}";
    try std.testing.expectEqualStrings("[\"key\",\"value\"]", findJsonArrayValue(line, "\"args\"").?);
}

test "parseTraceLine request" {
    const line = "{\"type\":\"request\",\"method\":\"POST\",\"url\":\"/api/users\",\"headers\":{},\"body\":\"hello\"}";
    const entry = try parseTraceLine(line);
    switch (entry) {
        .request => |req| {
            try std.testing.expectEqualStrings("POST", req.method);
            try std.testing.expectEqualStrings("/api/users", req.url);
            try std.testing.expectEqualStrings("hello", req.body.?);
        },
        else => return error.UnexpectedTraceType,
    }
}

test "parseTraceLine io" {
    const line = "{\"type\":\"io\",\"seq\":0,\"module\":\"env\",\"fn\":\"env\",\"args\":[\"API_KEY\"],\"result\":\"sk-123\"}";
    const entry = try parseTraceLine(line);
    switch (entry) {
        .io => |io| {
            try std.testing.expectEqual(@as(u32, 0), io.seq);
            try std.testing.expectEqualStrings("env", io.module);
            try std.testing.expectEqualStrings("env", io.func);
        },
        else => return error.UnexpectedTraceType,
    }
}

test "parseTraceFile groups by request" {
    const source =
        "{\"type\":\"request\",\"method\":\"GET\",\"url\":\"/a\",\"headers\":{},\"body\":null}\n" ++
        "{\"type\":\"io\",\"seq\":0,\"module\":\"env\",\"fn\":\"env\",\"args\":[\"K\"],\"result\":\"V\"}\n" ++
        "{\"type\":\"response\",\"status\":200,\"headers\":{},\"body\":\"ok\"}\n" ++
        "{\"type\":\"meta\",\"duration_us\":100,\"handler\":\"h.ts\",\"pool_slot\":0,\"io_count\":1}\n" ++
        "{\"type\":\"request\",\"method\":\"POST\",\"url\":\"/b\",\"headers\":{},\"body\":\"data\"}\n" ++
        "{\"type\":\"response\",\"status\":201,\"headers\":{},\"body\":\"created\"}\n" ++
        "{\"type\":\"meta\",\"duration_us\":200,\"handler\":\"h.ts\",\"pool_slot\":1,\"io_count\":0}\n";

    const groups = try parseTraceFile(std.testing.allocator, source);
    defer {
        for (groups) |g| std.testing.allocator.free(g.io_calls);
        std.testing.allocator.free(groups);
    }

    try std.testing.expectEqual(@as(usize, 2), groups.len);
    try std.testing.expectEqualStrings("GET", groups[0].request.method);
    try std.testing.expectEqual(@as(usize, 1), groups[0].io_calls.len);
    try std.testing.expectEqual(@as(u16, 200), groups[0].response.?.status);
    try std.testing.expectEqualStrings("POST", groups[1].request.method);
    try std.testing.expectEqual(@as(usize, 0), groups[1].io_calls.len);
    try std.testing.expectEqual(@as(u16, 201), groups[1].response.?.status);
}

test "jsonToJSValue primitives" {
    const zts = @import("root.zig");
    var test_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer test_arena.deinit();
    const allocator = test_arena.allocator();
    const ctx = try zts.createContext(allocator, .{ .nursery_size = 4096 });
    defer zts.destroyContext(ctx);

    // null -> undefined
    try std.testing.expect(jsonToJSValue(ctx, "null").isUndefined());

    // boolean
    try std.testing.expect(jsonToJSValue(ctx, "true").isTrue());
    try std.testing.expect(jsonToJSValue(ctx, "false").isFalse());

    // integer
    const int_val = jsonToJSValue(ctx, "42");
    try std.testing.expect(int_val.isInt());
    try std.testing.expectEqual(@as(i32, 42), int_val.getInt());

    // negative integer
    const neg_val = jsonToJSValue(ctx, "-7");
    try std.testing.expect(neg_val.isInt());
    try std.testing.expectEqual(@as(i32, -7), neg_val.getInt());

    // float
    const float_val = jsonToJSValue(ctx, "3.14");
    try std.testing.expect(float_val.isFloat64());

    // string
    const str_val = jsonToJSValue(ctx, "\"hello\"");
    try std.testing.expect(str_val.isAnyString());
}

test "jsonToJSValue object" {
    const zts = @import("root.zig");
    var test_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer test_arena.deinit();
    const allocator = test_arena.allocator();
    const ctx = try zts.createContext(allocator, .{ .nursery_size = 4096 });
    defer zts.destroyContext(ctx);

    const obj_val = jsonToJSValue(ctx, "{\"ok\":true,\"value\":42}");
    try std.testing.expect(obj_val.isObject());
}

test "jsonToJSValue array" {
    const zts = @import("root.zig");
    var test_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer test_arena.deinit();
    const allocator = test_arena.allocator();
    const ctx = try zts.createContext(allocator, .{ .nursery_size = 4096 });
    defer zts.destroyContext(ctx);

    const arr_val = jsonToJSValue(ctx, "[1,2,3]");
    try std.testing.expect(arr_val.isObject());
    const arr_obj = object.JSObject.fromValue(arr_val);
    try std.testing.expectEqual(@as(u32, 3), arr_obj.getArrayLength());
}

test "ReplayState nextIO" {
    const io_calls = [_]IoEntry{
        .{ .seq = 0, .module = "env", .func = "env", .args_json = "[\"K\"]", .result_json = "\"V\"" },
        .{ .seq = 1, .module = "cache", .func = "cacheGet", .args_json = "[\"ns\",\"k\"]", .result_json = "null" },
    };
    var state = ReplayState{
        .io_calls = &io_calls,
        .cursor = 0,
        .divergences = 0,
    };

    // Correct call
    const e1 = state.nextIO("env", "env").?;
    try std.testing.expectEqualStrings("env", e1.module);
    try std.testing.expectEqual(@as(u32, 0), state.divergences);

    // Correct call
    const e2 = state.nextIO("cache", "cacheGet").?;
    try std.testing.expectEqualStrings("cache", e2.module);
    try std.testing.expectEqual(@as(u32, 0), state.divergences);

    // Exhausted
    try std.testing.expect(state.nextIO("env", "env") == null);
    try std.testing.expectEqual(@as(u32, 1), state.divergences);
}

test "ReplayState divergence on mismatch" {
    const io_calls = [_]IoEntry{
        .{ .seq = 0, .module = "env", .func = "env", .args_json = "[\"K\"]", .result_json = "\"V\"" },
    };
    var state = ReplayState{
        .io_calls = &io_calls,
        .cursor = 0,
        .divergences = 0,
    };

    // Wrong module name
    const entry = state.nextIO("cache", "env");
    try std.testing.expect(entry != null);
    try std.testing.expectEqual(@as(u32, 1), state.divergences);
}
