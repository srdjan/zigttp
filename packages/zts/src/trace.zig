//! Handler Trace Recording and Replay
//!
//! Records every I/O boundary during handler execution as JSONL,
//! enabling exact reproduction of any request offline.
//!
//! Since virtual modules are the ONLY I/O boundary in zttp's restricted
//! JS subset, recording their inputs/outputs captures ALL external state
//! a handler depends on. This makes handlers deterministic pure functions
//! of (Request, VirtualModuleResponses).

const std = @import("std");
const builtin = @import("builtin");
const context = @import("context.zig");
const value = @import("value.zig");
const object = @import("object.zig");
const string = @import("string.zig");
const util = @import("modules/internal/util.zig");

const module_slots = @import("module_slots.zig");

pub const TRACE_STATE_SLOT = @intFromEnum(module_slots.Slot.trace);

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

    const MAX_JSON_DEPTH = 32;

    fn appendJSValue(self: *TraceRecorder, ctx: *context.Context, val: value.JSValue) ?void {
        // One serializer shared with the durable oplog (appendJSValueBuf) and
        // replay arg-checking, so recording, oplog, and replay comparison all
        // produce byte-identical JSON. On failure, deactivate the recorder like
        // the other append helpers: a partial line must stop all further
        // recording, not let the next record concatenate onto it and corrupt
        // the capsule JSONL.
        return appendJSValueBuf(&self.buf, self.allocator, ctx, val, 0) orelse {
            self.active = false;
            return null;
        };
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
            const ctx = util.castContext(ctx_ptr);
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
    witness: void,
    durable_run: DurableRunTrace,
    request: RequestTrace,
    io: IoEntry,
    step_start: StepStartTrace,
    step_result: StepResultTrace,
    wait_timer: WaitTimerTrace,
    resume_timer: ResumeTimerTrace,
    wait_signal: WaitSignalTrace,
    resume_signal: ResumeSignalTrace,
    response: ResponseTrace,
    meta: MetaTrace,
    complete: void,
    unknown: void,
};

pub const DurableRunTrace = struct {
    key: []const u8,
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

pub const StepStartTrace = struct {
    name: []const u8,
};

pub const StepResultTrace = struct {
    name: []const u8,
    result_json: []const u8,
};

pub const WaitTimerTrace = struct {
    until_ms: i64,
    timeout_ms: ?i64 = null,
};

pub const ResumeTimerTrace = struct {
    fired_at_ms: i64,
};

pub const WaitSignalTrace = struct {
    name: []const u8,
    timeout_ms: ?i64 = null,
};

pub const ResumeSignalTrace = struct {
    name: []const u8,
    payload_json: []const u8,
};

pub const DurableEvent = union(enum) {
    io: IoEntry,
    step_start: StepStartTrace,
    step_result: StepResultTrace,
    wait_timer: WaitTimerTrace,
    resume_timer: ResumeTimerTrace,
    wait_signal: WaitSignalTrace,
    resume_signal: ResumeSignalTrace,
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
    errdefer {
        for (groups.items) |group| allocator.free(group.io_calls);
        groups.deinit(allocator);
    }

    var current_io: std.ArrayList(IoEntry) = .empty;
    defer current_io.deinit(allocator);

    var current_request: ?RequestTrace = null;
    var current_response: ?ResponseTrace = null;
    var current_meta: ?MetaTrace = null;
    var pending_witness = false;
    var current_witness = false;

    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (!try std.json.validate(allocator, line)) return error.InvalidTraceJson;
        const entry = try parseTraceLine(line);
        switch (entry) {
            .request => |req| {
                if (current_request != null) {
                    if (current_response == null and !current_witness) return error.InvalidTraceEntry;
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
                current_witness = pending_witness;
                pending_witness = false;
            },
            .io => |io| try current_io.append(allocator, io),
            .response => |resp| current_response = resp,
            .meta => |meta| {
                current_meta = meta;
                if (current_request != null) {
                    if (current_response == null and !current_witness) return error.InvalidTraceEntry;
                    try groups.append(allocator, .{
                        .request = current_request.?,
                        .io_calls = try current_io.toOwnedSlice(allocator),
                        .response = current_response,
                        .meta = current_meta,
                    });
                    current_request = null;
                    current_response = null;
                    current_meta = null;
                    current_witness = false;
                }
            },
            .witness => pending_witness = true,
            .durable_run, .step_start, .step_result, .wait_timer, .resume_timer, .wait_signal, .resume_signal, .complete => {},
            .unknown => return error.InvalidTraceEntry,
        }
    }

    if (current_request != null) {
        if (current_response == null and !current_witness) return error.InvalidTraceEntry;
        try groups.append(allocator, .{
            .request = current_request.?,
            .io_calls = try current_io.toOwnedSlice(allocator),
            .response = current_response,
            .meta = current_meta,
        });
    }

    return try groups.toOwnedSlice(allocator);
}

pub fn parseTraceLine(line: []const u8) !TraceEntry {
    const type_str = findJsonStringValue(line, "\"type\"") orelse return .unknown;

    if (std.mem.eql(u8, type_str, "witness")) {
        return .witness;
    } else if (std.mem.eql(u8, type_str, "durable_run")) {
        return .{ .durable_run = .{
            .key = findJsonStringValue(line, "\"key\"") orelse "",
        } };
    } else if (std.mem.eql(u8, type_str, "request")) {
        return .{ .request = .{
            .method = findJsonStringValue(line, "\"method\"") orelse "GET",
            .url = findJsonStringValue(line, "\"url\"") orelse "/",
            .headers_json = findJsonObjectValue(line, "\"headers\"") orelse "{}",
            .body = findJsonStringValue(line, "\"body\""),
        } };
    } else if (std.mem.eql(u8, type_str, "io")) {
        return .{ .io = .{
            .seq = std.math.cast(u32, findJsonIntValue(line, "\"seq\"") orelse 0) orelse 0,
            .module = findJsonStringValue(line, "\"module\"") orelse "",
            .func = findJsonStringValue(line, "\"fn\"") orelse "",
            .args_json = findJsonArrayValue(line, "\"args\"") orelse "[]",
            .result_json = findJsonAnyValue(line, "\"result\"") orelse "null",
        } };
    } else if (std.mem.eql(u8, type_str, "step_start")) {
        return .{ .step_start = .{
            .name = findJsonStringValue(line, "\"name\"") orelse "",
        } };
    } else if (std.mem.eql(u8, type_str, "step_result")) {
        return .{ .step_result = .{
            .name = findJsonStringValue(line, "\"name\"") orelse "",
            .result_json = findJsonAnyValue(line, "\"result\"") orelse "null",
        } };
    } else if (std.mem.eql(u8, type_str, "wait_timer")) {
        return .{ .wait_timer = .{
            .until_ms = findJsonIntValue(line, "\"until_ms\"") orelse 0,
            .timeout_ms = findJsonIntValue(line, "\"timeout_ms\""),
        } };
    } else if (std.mem.eql(u8, type_str, "resume_timer")) {
        return .{ .resume_timer = .{
            .fired_at_ms = findJsonIntValue(line, "\"fired_at_ms\"") orelse 0,
        } };
    } else if (std.mem.eql(u8, type_str, "wait_signal")) {
        return .{ .wait_signal = .{
            .name = findJsonStringValue(line, "\"name\"") orelse "",
            .timeout_ms = findJsonIntValue(line, "\"timeout_ms\""),
        } };
    } else if (std.mem.eql(u8, type_str, "resume_signal")) {
        return .{ .resume_signal = .{
            .name = findJsonStringValue(line, "\"name\"") orelse "",
            .payload_json = findJsonAnyValue(line, "\"payload\"") orelse "null",
        } };
    } else if (std.mem.eql(u8, type_str, "response")) {
        return .{ .response = .{
            .status = std.math.cast(u16, findJsonIntValue(line, "\"status\"") orelse 200) orelse 200,
            .headers_json = findJsonObjectValue(line, "\"headers\"") orelse "{}",
            .body = findJsonStringValue(line, "\"body\"") orelse "",
        } };
    } else if (std.mem.eql(u8, type_str, "meta")) {
        return .{ .meta = .{
            .duration_us = std.math.cast(u64, findJsonIntValue(line, "\"duration_us\"") orelse 0) orelse 0,
            .handler = findJsonStringValue(line, "\"handler\"") orelse "",
            .pool_slot = std.math.cast(u32, findJsonIntValue(line, "\"pool_slot\"") orelse 0) orelse 0,
            .io_count = std.math.cast(u32, findJsonIntValue(line, "\"io_count\"") orelse 0) orelse 0,
        } };
    } else if (std.mem.eql(u8, type_str, "complete")) {
        return .complete;
    }
    return .unknown;
}

// ============================================================================
// Minimal JSON Field Extraction (zero-copy)
// ============================================================================

/// Find an object KEY at the top level (depth 1) of a JSON object, ignoring
/// occurrences of the key token inside nested structures or string values. `key`
/// is the quoted token (e.g. `"\"result\""`). Returns the index just past the
/// matched key, or null. Replaces a naive std.mem.indexOf that matched the first
/// substring anywhere — which corrupted extraction when an earlier sibling value
/// (e.g. an io arg equal to "result", or a header literally named "body")
/// contained the key token, silently feeding wrong values into durable recovery
/// and replay verification.
fn findTopLevelKeyEnd(json: []const u8, key: []const u8) ?usize {
    if (key.len == 0) return null;
    var depth: u32 = 0;
    var pos: usize = 0;
    var in_string = false;
    while (pos < json.len) {
        const c = json[pos];
        if (in_string) {
            if (c == '\\') {
                pos += 2;
                continue;
            }
            if (c == '"') in_string = false;
            pos += 1;
            continue;
        }
        switch (c) {
            '{', '[' => {
                depth += 1;
                pos += 1;
            },
            '}', ']' => {
                if (depth > 0) depth -= 1;
                pos += 1;
            },
            '"' => {
                // A quoted token at depth 1 followed by ':' is the key we want.
                if (depth == 1 and pos + key.len <= json.len and
                    std.mem.eql(u8, json[pos .. pos + key.len], key))
                {
                    var after = pos + key.len;
                    while (after < json.len and (json[after] == ' ' or json[after] == '\t')) : (after += 1) {}
                    if (after < json.len and json[after] == ':') return pos + key.len;
                }
                // Otherwise it is a non-matching key or a string value: skip it.
                in_string = true;
                pos += 1;
            },
            else => pos += 1,
        }
    }
    return null;
}

pub fn findJsonStringValue(json: []const u8, key: []const u8) ?[]const u8 {
    var pos = findTopLevelKeyEnd(json, key) orelse return null;
    while (pos < json.len and (json[pos] == ':' or json[pos] == ' ')) : (pos += 1) {}
    if (pos >= json.len) return null;
    if (pos + 4 <= json.len and std.mem.eql(u8, json[pos .. pos + 4], "null")) return null;
    if (json[pos] != '"') return null;
    pos += 1;
    const start = pos;
    while (pos < json.len) : (pos += 1) {
        if (json[pos] == '\\') {
            pos += 1;
            continue;
        }
        if (json[pos] == '"') return json[start..pos];
    }
    return null;
}

pub fn findJsonIntValue(json: []const u8, key: []const u8) ?i64 {
    var pos = findTopLevelKeyEnd(json, key) orelse return null;
    while (pos < json.len and (json[pos] == ':' or json[pos] == ' ')) : (pos += 1) {}
    if (pos >= json.len) return null;
    const start = pos;
    if (pos < json.len and json[pos] == '-') pos += 1;
    while (pos < json.len and json[pos] >= '0' and json[pos] <= '9') : (pos += 1) {}
    if (pos == start) return null;
    return std.fmt.parseInt(i64, json[start..pos], 10) catch null;
}

pub fn findJsonObjectValue(json: []const u8, key: []const u8) ?[]const u8 {
    var pos = findTopLevelKeyEnd(json, key) orelse return null;
    while (pos < json.len and (json[pos] == ':' or json[pos] == ' ')) : (pos += 1) {}
    if (pos >= json.len or json[pos] != '{') return null;
    return findMatchingBrace(json, pos, '{', '}');
}

pub fn findJsonArrayValue(json: []const u8, key: []const u8) ?[]const u8 {
    var pos = findTopLevelKeyEnd(json, key) orelse return null;
    while (pos < json.len and (json[pos] == ':' or json[pos] == ' ')) : (pos += 1) {}
    if (pos >= json.len or json[pos] != '[') return null;
    return findMatchingBrace(json, pos, '[', ']');
}

pub fn findJsonAnyValue(json: []const u8, key: []const u8) ?[]const u8 {
    var pos = findTopLevelKeyEnd(json, key) orelse return null;
    while (pos < json.len and (json[pos] == ':' or json[pos] == ' ')) : (pos += 1) {}
    if (pos >= json.len) return null;
    return switch (json[pos]) {
        '"' => blk: {
            var p = pos + 1;
            while (p < json.len) : (p += 1) {
                if (json[p] == '\\') {
                    p += 1;
                    continue;
                }
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
            if (json[pos] == '\\') {
                pos += 1;
                continue;
            }
            if (json[pos] == '"') in_string = false;
            continue;
        }
        if (json[pos] == '"') {
            in_string = true;
            continue;
        }
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

pub const REPLAY_STATE_SLOT = @intFromEnum(module_slots.Slot.replay);

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

    /// Non-consuming check: does the head entry target this module/function?
    /// Pure-function fallback stubs use this to decide replay-vs-real without
    /// disturbing the cursor (or counting a divergence) when no matching entry
    /// was recorded.
    pub fn peekMatches(self: *const ReplayState, module_name: []const u8, fn_name: []const u8) bool {
        if (self.cursor >= self.io_calls.len) return false;
        const entry = self.io_calls[self.cursor];
        return std.mem.eql(u8, entry.module, module_name) and std.mem.eql(u8, entry.func, fn_name);
    }

    /// Consume the head entry after `peekMatches` confirmed a match. The caller
    /// must have just observed `peekMatches == true` (cursor in bounds, names
    /// equal), so this advances without re-checking bounds or names.
    pub fn consumeHead(self: *ReplayState) IoEntry {
        const entry = self.io_calls[self.cursor];
        self.cursor += 1;
        return entry;
    }

    /// Does any not-yet-consumed entry target this module/function? Used by a
    /// pure-function fallback to tell a genuinely-new call (no recording -> run
    /// real silently) from a recorded call that drifted out of order (-> flag a
    /// divergence so replay verification still fails closed on structural drift).
    pub fn hasEntryAhead(self: *const ReplayState, module_name: []const u8, fn_name: []const u8) bool {
        var i = self.cursor;
        while (i < self.io_calls.len) : (i += 1) {
            const e = self.io_calls[i];
            if (std.mem.eql(u8, e.module, module_name) and std.mem.eql(u8, e.func, fn_name)) return true;
        }
        return false;
    }

    /// Count of recorded I/O entries the replay never consumed. A faithful
    /// replay re-makes every recorded call (each consumes its head entry), so a
    /// non-zero leftover means the edited handler DROPPED a recorded effect
    /// (e.g. a trailing cacheSet/sqlExec). Replay verification must fail closed
    /// on this, otherwise a deleted side effect reads as "reproduced".
    pub fn unconsumedCount(self: *const ReplayState) u32 {
        if (self.cursor >= self.io_calls.len) return 0;
        return @intCast(self.io_calls.len - self.cursor);
    }

    pub fn deinitOpaque(_: *anyopaque, _: std.mem.Allocator) void {
        // ReplayState does not own its io_calls (owned by trace parser).
    }
};

/// Parse a recorded entry's result back to a JSValue and count a divergence if
/// a non-null/undefined recording failed to parse. Shared tail of both replay
/// stubs so the parse-failure heuristic has a single owner.
/// A recorded entry replayed against changed arguments is a behavior drift.
/// Returns true when the live args diverge from what was recorded. Skips when
/// the recording did not pin args ("[]" or empty), so zero-arg calls and
/// hand-written fixtures that omit args are not flagged. Both sides are produced
/// by the same serializer (serializeArgsJson <-> recorded args_json), so a match
/// is byte-exact by construction.
fn argsDrifted(recorded_args_json: []const u8, live_args_json: []const u8) bool {
    if (recorded_args_json.len == 0 or std.mem.eql(u8, recorded_args_json, "[]")) return false;
    return !std.mem.eql(u8, live_args_json, recorded_args_json);
}

fn replayRecordedEntry(state: *ReplayState, ctx: *context.Context, entry: IoEntry, args: []const value.JSValue) value.JSValue {
    // Count a divergence so replay verification fails closed on an argument
    // change at the matched path.
    if (serializeArgsJson(ctx.allocator, ctx, args)) |live| {
        defer ctx.allocator.free(live);
        if (argsDrifted(entry.args_json, live)) state.divergences += 1;
    }
    const result = jsonToJSValue(ctx, entry.result_json);
    if (result.isUndefined() and entry.result_json.len > 0 and
        !std.mem.eql(u8, entry.result_json, "null") and
        !std.mem.eql(u8, entry.result_json, "undefined"))
    {
        state.divergences += 1;
    }
    return result;
}

/// Generate a replay stub NativeFn at compile time.
/// The stub reads the next recorded I/O entry from the ReplayState in
/// module_state and converts the result_json back to a JSValue.
pub fn makeReplayStub(
    comptime module_name: []const u8,
    comptime fn_name: []const u8,
) object.NativeFn {
    return struct {
        fn call(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
            const ctx = util.castContext(ctx_ptr);
            const state = ctx.getModuleState(ReplayState, REPLAY_STATE_SLOT) orelse {
                return value.JSValue.undefined_val;
            };
            const entry = state.nextIO(module_name, fn_name) orelse {
                return value.JSValue.undefined_val;
            };
            return replayRecordedEntry(state, ctx, entry, args);
        }
    }.call;
}

/// Generate a replay stub that falls through to the real implementation when
/// no matching I/O entry sits at the cursor head. Installed only for pure
/// functions (those declaring `Law.pure`): re-running them is deterministic,
/// so a test or capsule that did not record the call still observes the real
/// result instead of `undefined`. A recorded entry, when present, is consumed
/// and replayed exactly as `makeReplayStub` would, so divergence accounting and
/// existing fixtures are unchanged.
pub fn makeReplayStubWithFallback(
    comptime module_name: []const u8,
    comptime fn_name: []const u8,
    comptime original_fn: object.NativeFn,
) object.NativeFn {
    return struct {
        fn call(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
            const ctx = util.castContext(ctx_ptr);
            const state = ctx.getModuleState(ReplayState, REPLAY_STATE_SLOT) orelse {
                return original_fn(ctx_ptr, this, args);
            };
            // A recorded entry at the head: consume and replay it (trusting the
            // peek, so no second scan).
            if (state.peekMatches(module_name, fn_name)) {
                return replayRecordedEntry(state, ctx, state.consumeHead(), args);
            }
            // No entry at the head. If one for this call exists further on, the
            // recorded order/count drifted - flag a divergence so replay
            // verification still fails closed. Either way run the real
            // (deterministic) implementation; a genuinely-new call (no recording)
            // stays silent.
            if (state.hasEntryAhead(module_name, fn_name)) state.divergences += 1;
            return original_fn(ctx_ptr, this, args);
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
    const result = parseValue(ctx, json, 0, 0);
    return result.val;
}

// Cap nesting depth so an adversarial/corrupted trace (replay or shared capsule
// files are third-party input) with deeply nested values cannot overflow the
// native stack. Mirrors the serializer's MAX_JSON_DEPTH discipline.
const MAX_PARSE_DEPTH: u32 = 128;

fn parseValue(ctx: *context.Context, json: []const u8, start: usize, depth: u32) ParseResult {
    const pos = skipWhitespace(json, start);
    if (pos >= json.len) return .{ .val = value.JSValue.undefined_val, .end = pos };
    if (depth >= MAX_PARSE_DEPTH) return .{ .val = value.JSValue.undefined_val, .end = pos };

    return switch (json[pos]) {
        '"' => parseString(ctx, json, pos),
        '{' => parseObject(ctx, json, pos, depth),
        '[' => parseArray(ctx, json, pos, depth),
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

fn parseArray(ctx: *context.Context, json: []const u8, start: usize, depth: u32) ParseResult {
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
        const elem_start = pos;
        const elem = parseValue(ctx, json, pos, depth + 1);
        pos = elem.end;
        arr.arrayPush(ctx.allocator, elem.val) catch {};
        // Guarantee forward progress. parseValue returns end == start when the
        // MAX_PARSE_DEPTH cap is hit (an adversarial/corrupted trace nested past
        // 128 levels); without this break the loop would re-read the same
        // character forever (100% CPU + unbounded arrayPush). parseObject is
        // immune because its non-string-key escape always advances.
        if (pos <= elem_start) break;
    }
    return .{ .val = arr.toValue(), .end = pos };
}

fn parseObject(ctx: *context.Context, json: []const u8, start: usize, depth: u32) ParseResult {
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
        const val_result = parseValue(ctx, json, pos, depth + 1);
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

/// Unescape a JSON string value (convert \" to ", \n to newline, \uXXXX to UTF-8, etc.).
/// Resolve JSON escapes in an optional request body before it reaches a handler.
/// Returns the decoded `slice` plus the allocation to free in `owned` (null when
/// the body was null or allocation failed and the input is aliased). Shared by
/// the test runner and replay runner so both decode a recorded body identically.
pub fn unescapeBody(allocator: std.mem.Allocator, body: ?[]const u8) struct { slice: ?[]const u8, owned: ?[]u8 } {
    const b = body orelse return .{ .slice = null, .owned = null };
    const out = unescapeJson(allocator, b) catch return .{ .slice = b, .owned = null };
    return .{ .slice = out, .owned = out };
}

pub fn unescapeJson(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var result: std.ArrayList(u8) = .empty;
    errdefer result.deinit(allocator);
    var i: usize = 0;
    while (i < input.len) : (i += 1) {
        if (input[i] == '\\' and i + 1 < input.len) {
            i += 1;
            switch (input[i]) {
                '"' => try result.append(allocator, '"'),
                '\\' => try result.append(allocator, '\\'),
                'n' => try result.append(allocator, '\n'),
                'r' => try result.append(allocator, '\r'),
                't' => try result.append(allocator, '\t'),
                '/' => try result.append(allocator, '/'),
                'u' => {
                    // \uXXXX unicode escape -> UTF-8
                    if (i + 4 < input.len) {
                        const hex = input[i + 1 .. i + 5];
                        const codepoint = std.fmt.parseInt(u21, hex, 16) catch {
                            try result.append(allocator, 'u');
                            continue;
                        };
                        var buf: [4]u8 = undefined;
                        const len = std.unicode.utf8Encode(codepoint, &buf) catch {
                            try result.append(allocator, 'u');
                            continue;
                        };
                        try result.appendSlice(allocator, buf[0..len]);
                        i += 4;
                    } else {
                        try result.append(allocator, 'u');
                    }
                },
                else => try result.append(allocator, input[i]),
            }
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
// Durable Execution - Write-Ahead Oplog with Hybrid Replay/Record
// ============================================================================

pub const DURABLE_STATE_SLOT = @intFromEnum(module_slots.Slot.durable);

pub const DurablePersistenceError = error{
    DurableSerializeFailed,
    DurableWriteFailed,
    DurableWriteZero,
    DurableFsyncFailed,
};

pub const DurableStepReplay = union(enum) {
    live: void,
    execute: void,
    cached: []const u8,
};

pub const DurableTimerWaitReplay = union(enum) {
    live: void,
    ready: void,
    pending: WaitTimerTrace,
};

pub const DurableSignalWaitReplay = union(enum) {
    live: void,
    delivered: []const u8,
    pending: WaitSignalTrace,
};

/// Per-request durable execution state.
/// Combines oplog reading (replay phase) with write-ahead persistence (live phase).
///
/// During the replay phase, I/O calls return results from the oplog.
/// When the oplog is exhausted, the state transitions to live mode:
/// each real I/O result is persisted to the oplog before returning.
pub const DurableState = struct {
    /// Pre-parsed oplog events from an incomplete durable run.
    oplog_events: []const DurableEvent,
    /// Cursor tracking which oplog entry to return next.
    cursor: u32,
    /// Allocator for serialization buffers.
    allocator: std.mem.Allocator,
    /// File descriptor for write-ahead persistence.
    oplog_fd: std.c.fd_t,
    /// Whether we have transitioned from replay to live execution.
    is_live: bool,
    /// Count of I/O calls persisted in live phase.
    live_io_count: u32,
    /// Serialization buffer (reused across persist calls).
    write_buf: std.ArrayList(u8),

    pub fn init(
        allocator: std.mem.Allocator,
        oplog_events: []const DurableEvent,
        oplog_fd: std.c.fd_t,
    ) DurableState {
        return .{
            .oplog_events = oplog_events,
            .cursor = 0,
            .allocator = allocator,
            .oplog_fd = oplog_fd,
            .is_live = oplog_events.len == 0,
            .live_io_count = 0,
            .write_buf = .empty,
        };
    }

    pub fn deinit(self: *DurableState) void {
        self.write_buf.deinit(self.allocator);
    }

    pub fn deinitOpaque(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        _ = allocator;
        const self: *DurableState = @ptrCast(@alignCast(ptr));
        self.deinit();
    }

    /// Try to consume the next oplog entry during replay phase.
    /// Returns the recorded entry if available; transitions to live mode
    /// when the oplog is exhausted.
    pub fn replayNext(self: *DurableState, module_name: []const u8, fn_name: []const u8) ?IoEntry {
        if (self.is_live) return null;
        if (self.cursor >= self.oplog_events.len) {
            self.is_live = true;
            return null;
        }
        const event = self.oplog_events[self.cursor];
        const entry = switch (event) {
            .io => |io| io,
            else => {
                self.is_live = true;
                return null;
            },
        };
        // Verify call sequence matches (divergence = unrecoverable)
        if (!std.mem.eql(u8, entry.module, module_name) or !std.mem.eql(u8, entry.func, fn_name)) {
            self.is_live = true;
            return null;
        }
        self.cursor += 1;
        return entry;
    }

    /// Enter a named durable step.
    /// If a completed step result exists in the oplog, returns it immediately
    /// and advances the cursor past the entire recorded step segment.
    /// If the step was started but not completed before the crash, returns
    /// `.execute` after consuming the start marker so the thunk can continue
    /// replaying nested I/O and finish live.
    pub fn beginStep(self: *DurableState, name: []const u8) DurableStepReplay {
        if (self.is_live) return .live;
        if (self.cursor >= self.oplog_events.len) {
            self.is_live = true;
            return .live;
        }

        const event = self.oplog_events[self.cursor];
        const start = switch (event) {
            .step_start => |s| s,
            else => {
                self.is_live = true;
                return .live;
            },
        };

        if (!std.mem.eql(u8, start.name, name)) {
            self.is_live = true;
            return .live;
        }

        self.cursor += 1; // consume step_start

        var scan = self.cursor;
        while (scan < self.oplog_events.len) : (scan += 1) {
            switch (self.oplog_events[scan]) {
                .step_result => |result| {
                    if (std.mem.eql(u8, result.name, name)) {
                        self.cursor = @intCast(scan + 1);
                        return .{ .cached = result.result_json };
                    }
                },
                else => {},
            }
        }

        return .execute;
    }

    pub fn beginTimerWait(self: *DurableState) DurableTimerWaitReplay {
        if (self.is_live) return .live;
        if (self.cursor >= self.oplog_events.len) {
            self.is_live = true;
            return .live;
        }

        const event = self.oplog_events[self.cursor];
        // The recorded deadline is authoritative: a relative sleep()
        // recomputes now+delay on every recovery pass, so the caller's
        // until_ms never equals the recorded value. Matching on it would
        // bail to live, persist a fresh wait_timer, and re-arm the deadline
        // forever; timer waits replay positionally, like steps.
        const wait = switch (event) {
            .wait_timer => |w| w,
            else => {
                self.is_live = true;
                return .live;
            },
        };

        self.cursor += 1;
        if (self.cursor < self.oplog_events.len) {
            switch (self.oplog_events[self.cursor]) {
                .resume_timer => {
                    self.cursor += 1;
                    return .ready;
                },
                else => {},
            }
        }

        self.is_live = true;
        return .{ .pending = wait };
    }

    pub fn beginSignalWait(self: *DurableState, name: []const u8) DurableSignalWaitReplay {
        if (self.is_live) return .live;
        if (self.cursor >= self.oplog_events.len) {
            self.is_live = true;
            return .live;
        }

        const event = self.oplog_events[self.cursor];
        const wait = switch (event) {
            .wait_signal => |w| w,
            else => {
                self.is_live = true;
                return .live;
            },
        };
        if (!std.mem.eql(u8, wait.name, name)) {
            self.is_live = true;
            return .live;
        }

        self.cursor += 1;
        if (self.cursor < self.oplog_events.len) {
            switch (self.oplog_events[self.cursor]) {
                .resume_signal => |signal_resume| {
                    if (std.mem.eql(u8, signal_resume.name, name)) {
                        self.cursor += 1;
                        return .{ .delivered = signal_resume.payload_json };
                    }
                },
                else => {},
            }
        }

        self.is_live = true;
        return .{ .pending = wait };
    }

    /// Persist an I/O call result to the oplog with write-ahead guarantee.
    /// Serializes as JSONL, writes, and fsyncs before returning.
    pub fn persistIO(
        self: *DurableState,
        module_name: []const u8,
        fn_name: []const u8,
        ctx: *context.Context,
        args: []const value.JSValue,
        result: value.JSValue,
    ) DurablePersistenceError!void {
        self.write_buf.clearRetainingCapacity();

        // Build JSONL line in write_buf using same format as TraceRecorder.recordIO
        const seq = self.cursor + self.live_io_count;
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "{\"type\":\"io\",\"seq\":"));
        try requireAppend(appendInt(&self.write_buf, self.allocator, seq));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, ",\"module\":\""));
        try requireAppend(appendEscapedSlice(&self.write_buf, self.allocator, module_name));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "\",\"fn\":\""));
        try requireAppend(appendEscapedSlice(&self.write_buf, self.allocator, fn_name));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "\",\"args\":["));

        for (args, 0..) |arg, i| {
            if (i > 0) try requireAppend(appendByte(&self.write_buf, self.allocator, ','));
            try requireAppend(appendJSValueBuf(&self.write_buf, self.allocator, ctx, arg, 0));
        }

        try requireAppend(appendSlice(&self.write_buf, self.allocator, "],\"result\":"));
        try requireAppend(appendJSValueBuf(&self.write_buf, self.allocator, ctx, result, 0));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "}\n"));

        // Write-ahead: write + fsync before returning to handler
        try self.persistBuffer();

        self.live_io_count += 1;
    }

    pub fn persistRunKey(self: *DurableState, key: []const u8) DurablePersistenceError!void {
        self.write_buf.clearRetainingCapacity();
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "{\"type\":\"durable_run\",\"key\":\""));
        try requireAppend(appendEscapedSlice(&self.write_buf, self.allocator, key));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "\"}\n"));
        try self.persistBuffer();
    }

    pub fn persistStepStart(self: *DurableState, name: []const u8) DurablePersistenceError!void {
        self.write_buf.clearRetainingCapacity();
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "{\"type\":\"step_start\",\"name\":\""));
        try requireAppend(appendEscapedSlice(&self.write_buf, self.allocator, name));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "\"}\n"));
        try self.persistBuffer();
    }

    pub fn persistStepResult(
        self: *DurableState,
        name: []const u8,
        ctx: *context.Context,
        result: value.JSValue,
    ) DurablePersistenceError!void {
        self.write_buf.clearRetainingCapacity();
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "{\"type\":\"step_result\",\"name\":\""));
        try requireAppend(appendEscapedSlice(&self.write_buf, self.allocator, name));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "\",\"result\":"));
        try requireAppend(appendJSValueBuf(&self.write_buf, self.allocator, ctx, result, 0));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "}\n"));
        try self.persistBuffer();
    }

    pub fn persistWaitTimer(self: *DurableState, until_ms: i64, timeout_ms: ?i64) DurablePersistenceError!void {
        self.write_buf.clearRetainingCapacity();
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "{\"type\":\"wait_timer\",\"until_ms\":"));
        try requireAppend(appendInt(&self.write_buf, self.allocator, until_ms));
        if (timeout_ms) |deadline| {
            try requireAppend(appendSlice(&self.write_buf, self.allocator, ",\"timeout_ms\":"));
            try requireAppend(appendInt(&self.write_buf, self.allocator, deadline));
        }
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "}\n"));
        try self.persistBuffer();
    }

    pub fn persistResumeTimer(self: *DurableState, fired_at_ms: i64) DurablePersistenceError!void {
        self.write_buf.clearRetainingCapacity();
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "{\"type\":\"resume_timer\",\"fired_at_ms\":"));
        try requireAppend(appendInt(&self.write_buf, self.allocator, fired_at_ms));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "}\n"));
        try self.persistBuffer();
    }

    pub fn persistWaitSignal(self: *DurableState, name: []const u8, timeout_ms: ?i64) DurablePersistenceError!void {
        self.write_buf.clearRetainingCapacity();
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "{\"type\":\"wait_signal\",\"name\":\""));
        try requireAppend(appendEscapedSlice(&self.write_buf, self.allocator, name));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "\""));
        if (timeout_ms) |deadline| {
            try requireAppend(appendSlice(&self.write_buf, self.allocator, ",\"timeout_ms\":"));
            try requireAppend(appendInt(&self.write_buf, self.allocator, deadline));
        }
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "}\n"));
        try self.persistBuffer();
    }

    pub fn persistResumeSignal(self: *DurableState, name: []const u8, payload_json: []const u8) DurablePersistenceError!void {
        self.write_buf.clearRetainingCapacity();
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "{\"type\":\"resume_signal\",\"name\":\""));
        try requireAppend(appendEscapedSlice(&self.write_buf, self.allocator, name));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "\",\"payload\":"));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, payload_json));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "}\n"));
        try self.persistBuffer();
    }

    /// Persist the response line to the oplog and fsync.
    pub fn persistResponse(
        self: *DurableState,
        status: u16,
        header_names: []const []const u8,
        header_values: []const []const u8,
        body: []const u8,
    ) DurablePersistenceError!void {
        self.write_buf.clearRetainingCapacity();

        try requireAppend(appendSlice(&self.write_buf, self.allocator, "{\"type\":\"response\",\"status\":"));
        try requireAppend(appendInt(&self.write_buf, self.allocator, status));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, ",\"headers\":{"));

        const count = @min(header_names.len, header_values.len);
        for (0..count) |i| {
            if (i > 0) try requireAppend(appendByte(&self.write_buf, self.allocator, ','));
            try requireAppend(appendByte(&self.write_buf, self.allocator, '"'));
            try requireAppend(appendEscapedSlice(&self.write_buf, self.allocator, header_names[i]));
            try requireAppend(appendSlice(&self.write_buf, self.allocator, "\":\""));
            try requireAppend(appendEscapedSlice(&self.write_buf, self.allocator, header_values[i]));
            try requireAppend(appendByte(&self.write_buf, self.allocator, '"'));
        }

        try requireAppend(appendSlice(&self.write_buf, self.allocator, "},\"body\":\""));
        try requireAppend(appendEscapedSlice(&self.write_buf, self.allocator, body));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "\"}\n"));

        try self.persistBuffer();
    }

    /// Persist the request line to the oplog (written at start of request).
    pub fn persistRequest(
        self: *DurableState,
        method: []const u8,
        url: []const u8,
        header_names: []const []const u8,
        header_values: []const []const u8,
        body: ?[]const u8,
    ) DurablePersistenceError!void {
        self.write_buf.clearRetainingCapacity();

        try requireAppend(appendSlice(&self.write_buf, self.allocator, "{\"type\":\"request\",\"method\":\""));
        try requireAppend(appendEscapedSlice(&self.write_buf, self.allocator, method));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "\",\"url\":\""));
        try requireAppend(appendEscapedSlice(&self.write_buf, self.allocator, url));
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "\",\"headers\":{"));

        const count = @min(header_names.len, header_values.len);
        for (0..count) |i| {
            if (i > 0) try requireAppend(appendByte(&self.write_buf, self.allocator, ','));
            try requireAppend(appendByte(&self.write_buf, self.allocator, '"'));
            try requireAppend(appendEscapedSlice(&self.write_buf, self.allocator, header_names[i]));
            try requireAppend(appendSlice(&self.write_buf, self.allocator, "\":\""));
            try requireAppend(appendEscapedSlice(&self.write_buf, self.allocator, header_values[i]));
            try requireAppend(appendByte(&self.write_buf, self.allocator, '"'));
        }

        try requireAppend(appendSlice(&self.write_buf, self.allocator, "},\"body\":"));
        if (body) |b| {
            try requireAppend(appendByte(&self.write_buf, self.allocator, '"'));
            try requireAppend(appendEscapedSlice(&self.write_buf, self.allocator, b));
            try requireAppend(appendByte(&self.write_buf, self.allocator, '"'));
        } else {
            try requireAppend(appendSlice(&self.write_buf, self.allocator, "null"));
        }
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "}\n"));

        try self.persistBuffer();
    }

    /// Mark the oplog as complete by writing a completion marker and fsyncing.
    pub fn markComplete(self: *DurableState) DurablePersistenceError!void {
        self.write_buf.clearRetainingCapacity();
        try requireAppend(appendSlice(&self.write_buf, self.allocator, "{\"type\":\"complete\"}\n"));
        try self.persistBuffer();
    }

    fn persistBuffer(self: *DurableState) DurablePersistenceError!void {
        try writeAllChecked(self.oplog_fd, self.write_buf.items);
        try fsyncFdChecked(self.oplog_fd);
    }
};

/// Generate a durable NativeFn wrapper at compile time.
/// Combines replay (from oplog) and recording (write-ahead) in a single wrapper.
/// During replay phase: returns recorded results from the oplog.
/// During live phase: executes the real function, persists result, then returns.
pub fn makeDurableWrapper(
    comptime module_name: []const u8,
    comptime fn_name: []const u8,
    comptime original_fn: object.NativeFn,
) object.NativeFn {
    return struct {
        fn call(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
            const ctx = util.castContext(ctx_ptr);
            const state = ctx.getModuleState(DurableState, DURABLE_STATE_SLOT) orelse {
                // No durable state: fall through to real execution
                return original_fn(ctx_ptr, this, args);
            };

            // Phase 1: Replay from oplog if entries remain
            if (state.replayNext(module_name, fn_name)) |entry| {
                return jsonToJSValue(ctx, entry.result_json);
            }

            // Phase 2: Live execution with write-ahead persistence
            const result = try original_fn(ctx_ptr, this, args);
            try state.persistIO(module_name, fn_name, ctx, args, result);
            return result;
        }
    }.call;
}

// -- DurableState buffer helpers (standalone, not methods on TraceRecorder) --

fn requireAppend(result: ?void) DurablePersistenceError!void {
    if (result == null) return error.DurableSerializeFailed;
}

pub fn throwDurablePersistenceError(ctx: *context.Context, _: anyerror) value.JSValue {
    return util.throwError(ctx, "DurablePersistenceError", "failed to persist durable oplog entry");
}

fn appendSlice(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, data: []const u8) ?void {
    buf.appendSlice(allocator, data) catch return null;
}

fn appendByte(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, byte: u8) ?void {
    buf.append(allocator, byte) catch return null;
}

fn appendInt(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, val: anytype) ?void {
    var tmp: [64]u8 = undefined;
    const slice = std.fmt.bufPrint(&tmp, "{d}", .{val}) catch return null;
    appendSlice(buf, allocator, slice) orelse return null;
}

fn appendEscapedSlice(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, data: []const u8) ?void {
    for (data) |c| {
        switch (c) {
            '"' => appendSlice(buf, allocator, "\\\"") orelse return null,
            '\\' => appendSlice(buf, allocator, "\\\\") orelse return null,
            '\n' => appendSlice(buf, allocator, "\\n") orelse return null,
            '\r' => appendSlice(buf, allocator, "\\r") orelse return null,
            '\t' => appendSlice(buf, allocator, "\\t") orelse return null,
            else => {
                if (c < 0x20) {
                    const hex = "0123456789abcdef";
                    appendSlice(buf, allocator, "\\u00") orelse return null;
                    appendByte(buf, allocator, hex[c >> 4]) orelse return null;
                    appendByte(buf, allocator, hex[c & 0xF]) orelse return null;
                } else {
                    appendByte(buf, allocator, c) orelse return null;
                }
            },
        }
    }
}

fn appendJSValueBuf(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, ctx: *context.Context, val: value.JSValue, depth: u32) ?void {
    if (depth >= TraceRecorder.MAX_JSON_DEPTH) {
        appendSlice(buf, allocator, "null") orelse return null;
        return;
    }
    if (val.isNull() or val.isUndefined()) {
        appendSlice(buf, allocator, "null") orelse return null;
    } else if (val.isTrue()) {
        appendSlice(buf, allocator, "true") orelse return null;
    } else if (val.isFalse()) {
        appendSlice(buf, allocator, "false") orelse return null;
    } else if (val.isInt()) {
        appendInt(buf, allocator, val.getInt()) orelse return null;
    } else if (val.isFloat64()) {
        const f = val.getFloat64();
        if (std.math.isNan(f) or std.math.isInf(f)) {
            appendSlice(buf, allocator, "null") orelse return null;
        } else {
            // f64 decimal rendering can need up to ~347 bytes (e.g. a
            // small-magnitude value like 1e-300); a 64-byte buffer overflowed
            // and aborted the record mid-line, corrupting the capsule. Size for
            // the worst case so any finite float serializes.
            var tmp: [512]u8 = undefined;
            const slice = std.fmt.bufPrint(&tmp, "{d}", .{f}) catch return null;
            appendSlice(buf, allocator, slice) orelse return null;
        }
    } else if (val.isAnyString()) {
        appendByte(buf, allocator, '"') orelse return null;
        const data = extractStringData(val) orelse "";
        appendEscapedSlice(buf, allocator, data) orelse return null;
        appendByte(buf, allocator, '"') orelse return null;
    } else if (val.isObject()) {
        const obj = object.JSObject.fromValue(val);
        if (obj.class_id == .array) {
            appendByte(buf, allocator, '[') orelse return null;
            const len = obj.getArrayLength();
            for (0..len) |i| {
                if (i > 0) appendByte(buf, allocator, ',') orelse return null;
                if (obj.getIndex(@intCast(i))) |elem| {
                    appendJSValueBuf(buf, allocator, ctx, elem, depth + 1) orelse return null;
                } else {
                    appendSlice(buf, allocator, "null") orelse return null;
                }
            }
            appendByte(buf, allocator, ']') orelse return null;
        } else {
            appendByte(buf, allocator, '{') orelse return null;
            var first = true;
            const pool = ctx.hidden_class_pool orelse {
                appendByte(buf, allocator, '}') orelse return null;
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
                                if (!first) appendByte(buf, allocator, ',') orelse return null;
                                first = false;
                                appendByte(buf, allocator, '"') orelse return null;
                                const name = atomToString(atom, ctx) orelse "?";
                                appendEscapedSlice(buf, allocator, name) orelse return null;
                                appendSlice(buf, allocator, "\":") orelse return null;
                                appendJSValueBuf(buf, allocator, ctx, prop_val, depth + 1) orelse return null;
                            }
                        }
                    }
                }
            }
            appendByte(buf, allocator, '}') orelse return null;
        }
    } else {
        appendSlice(buf, allocator, "null") orelse return null;
    }
}

/// Serialize a call's arguments to the recorded `args_json` shape
/// (`[arg0,arg1,...]`) using the shared value serializer, so the result can be
/// compared byte-for-byte against a recorded `IoEntry.args_json`. The caller
/// owns the returned slice; returns null on allocation failure.
fn serializeArgsJson(allocator: std.mem.Allocator, ctx: *context.Context, args: []const value.JSValue) ?[]u8 {
    var buf: std.ArrayList(u8) = .empty;
    // This function returns ?[]u8 (no error set), so an `errdefer` would never
    // fire - every failure path must free the partial buffer explicitly.
    appendByte(&buf, allocator, '[') orelse {
        buf.deinit(allocator);
        return null;
    };
    for (args, 0..) |arg, i| {
        if (i > 0) appendByte(&buf, allocator, ',') orelse {
            buf.deinit(allocator);
            return null;
        };
        appendJSValueBuf(&buf, allocator, ctx, arg, 0) orelse {
            buf.deinit(allocator);
            return null;
        };
    }
    appendByte(&buf, allocator, ']') orelse {
        buf.deinit(allocator);
        return null;
    };
    return buf.toOwnedSlice(allocator) catch {
        buf.deinit(allocator);
        return null;
    };
}

/// Current wall-clock time in milliseconds since Unix epoch.
/// Freestanding/wasm has no POSIX clock; the analyzer never executes handler
/// I/O, so the clock code is comptime-elided there and the call returns 0.
pub fn unixMillis() i64 {
    if (comptime builtin.target.os.tag != .freestanding) {
        var ts: std.posix.timespec = undefined;
        switch (std.posix.errno(std.posix.system.clock_gettime(.REALTIME, &ts))) {
            .SUCCESS => {
                const secs: i64 = @intCast(ts.sec);
                const nanos: i64 = @intCast(ts.nsec);
                return (secs * 1000) + @divTrunc(nanos, 1_000_000);
            },
            else => return 0,
        }
    }
    return 0;
}

/// Write all bytes to fd, retrying on partial writes.
pub fn writeAll(fd: std.c.fd_t, data: []const u8) void {
    var remaining = data;
    while (remaining.len > 0) {
        const result = std.c.write(fd, remaining.ptr, remaining.len);
        if (result < 0) {
            if (std.posix.errno(result) == .INTR) continue;
            break;
        }
        remaining = remaining[@intCast(result)..];
    }
}

/// Write all bytes to fd, failing closed on I/O errors.
pub fn writeAllChecked(fd: std.c.fd_t, data: []const u8) DurablePersistenceError!void {
    var remaining = data;
    while (remaining.len > 0) {
        const result = std.c.write(fd, remaining.ptr, remaining.len);
        if (result < 0) {
            if (std.posix.errno(result) == .INTR) continue;
            return error.DurableWriteFailed;
        }
        if (result == 0) return error.DurableWriteZero;
        remaining = remaining[@intCast(result)..];
    }
}

/// fsync a file descriptor. Best-effort - failure is not fatal.
fn fsyncFd(fd: std.c.fd_t) void {
    _ = std.c.fsync(fd);
}

fn fsyncFdChecked(fd: std.c.fd_t) DurablePersistenceError!void {
    const result = std.c.fsync(fd);
    if (result < 0) return error.DurableFsyncFailed;
}

/// Check if a trace file represents an incomplete (recoverable) oplog.
/// An oplog is incomplete if it has a request entry but no "complete" marker.
/// A crash can tear the final oplog line mid-append. Every durably written
/// line ends with a newline, so a trailing fragment without one is a
/// truncation point, not an event: the lenient field defaults in
/// parseTraceLine would otherwise fabricate an event from it (for example a
/// resume_timer with fired_at_ms=0, faking a fired sleep).
fn completeOplogLines(source: []const u8) []const u8 {
    if (source.len == 0 or source[source.len - 1] == '\n') return source;
    const last_nl = std.mem.lastIndexOfScalar(u8, source, '\n') orelse return source[0..0];
    return source[0 .. last_nl + 1];
}

pub fn isIncompleteOplog(raw_source: []const u8) bool {
    const source = completeOplogLines(raw_source);
    var has_request = false;
    var has_complete = false;
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        const type_str = findJsonStringValue(line, "\"type\"") orelse continue;
        if (std.mem.eql(u8, type_str, "request")) has_request = true;
        if (std.mem.eql(u8, type_str, "complete")) has_complete = true;
    }
    return has_request and !has_complete;
}

pub const DurableLog = struct {
    run_key: ?[]const u8,
    request: ?RequestTrace,
    events: []const DurableEvent,
    response: ?ResponseTrace,
    complete: bool,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *DurableLog) void {
        self.allocator.free(self.events);
    }
};

pub fn parseDurableOplog(allocator: std.mem.Allocator, raw_source: []const u8) !DurableLog {
    const source = completeOplogLines(raw_source);
    var events: std.ArrayList(DurableEvent) = .empty;
    errdefer events.deinit(allocator);
    var run_key: ?[]const u8 = null;
    var request: ?RequestTrace = null;
    var response: ?ResponseTrace = null;
    var complete = false;

    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        const entry = parseTraceLine(line) catch continue;
        switch (entry) {
            .durable_run => |run| run_key = run.key,
            .request => |req| request = req,
            .io => |io| try events.append(allocator, .{ .io = io }),
            .step_start => |start| try events.append(allocator, .{ .step_start = start }),
            .step_result => |result| try events.append(allocator, .{ .step_result = result }),
            .wait_timer => |wait| try events.append(allocator, .{ .wait_timer = wait }),
            .resume_timer => |timer_resume| try events.append(allocator, .{ .resume_timer = timer_resume }),
            .wait_signal => |wait| try events.append(allocator, .{ .wait_signal = wait }),
            .resume_signal => |signal_resume| try events.append(allocator, .{ .resume_signal = signal_resume }),
            .response => |resp| response = resp,
            .complete => complete = true,
            else => {},
        }
    }

    return .{
        .run_key = run_key,
        .request = request,
        .events = try events.toOwnedSlice(allocator),
        .response = response,
        .complete = complete,
        .allocator = allocator,
    };
}

/// Parse an incomplete oplog into its constituent parts for recovery.
/// Returns the request trace and any I/O entries that were persisted
/// before the crash.
pub const IncompleteOplog = struct {
    run_key: ?[]const u8,
    request: RequestTrace,
    events: []const DurableEvent,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *IncompleteOplog) void {
        self.allocator.free(self.events);
    }
};

pub fn parseIncompleteOplog(allocator: std.mem.Allocator, source: []const u8) !IncompleteOplog {
    var parsed = try parseDurableOplog(allocator, source);
    errdefer parsed.deinit();

    if (parsed.complete) return error.OplogAlreadyComplete;

    const request = parsed.request orelse return error.NoRequestInOplog;
    const events = parsed.events;
    parsed.events = &.{};
    parsed.deinit();

    return .{
        .run_key = parsed.run_key,
        .request = request,
        .events = events,
        .allocator = allocator,
    };
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

test "parseTraceLine durable step result" {
    const line = "{\"type\":\"step_result\",\"name\":\"charge\",\"result\":{\"ok\":true}}";
    const entry = try parseTraceLine(line);
    switch (entry) {
        .step_result => |result| {
            try std.testing.expectEqualStrings("charge", result.name);
            try std.testing.expectEqualStrings("{\"ok\":true}", result.result_json);
        },
        else => return error.UnexpectedTraceType,
    }
}

test "parseTraceLine durable wait timer timeout metadata" {
    const line = "{\"type\":\"wait_timer\",\"until_ms\":1000,\"timeout_ms\":900}";
    const entry = try parseTraceLine(line);
    switch (entry) {
        .wait_timer => |wait| {
            try std.testing.expectEqual(@as(i64, 1000), wait.until_ms);
            try std.testing.expectEqual(@as(?i64, 900), wait.timeout_ms);
        },
        else => return error.UnexpectedTraceType,
    }
}

test "parseTraceLine durable wait and resume signal" {
    const wait_line = "{\"type\":\"wait_signal\",\"name\":\"approval\"}";
    const wait_entry = try parseTraceLine(wait_line);
    switch (wait_entry) {
        .wait_signal => |wait| {
            try std.testing.expectEqualStrings("approval", wait.name);
            try std.testing.expectEqual(@as(?i64, null), wait.timeout_ms);
        },
        else => return error.UnexpectedTraceType,
    }

    const timed_wait_line = "{\"type\":\"wait_signal\",\"name\":\"approval\",\"timeout_ms\":1234}";
    const timed_wait_entry = try parseTraceLine(timed_wait_line);
    switch (timed_wait_entry) {
        .wait_signal => |wait| {
            try std.testing.expectEqualStrings("approval", wait.name);
            try std.testing.expectEqual(@as(?i64, 1234), wait.timeout_ms);
        },
        else => return error.UnexpectedTraceType,
    }

    const resume_line = "{\"type\":\"resume_signal\",\"name\":\"approval\",\"payload\":{\"ok\":true}}";
    const resume_entry = try parseTraceLine(resume_line);
    switch (resume_entry) {
        .resume_signal => |signal_resume| {
            try std.testing.expectEqualStrings("approval", signal_resume.name);
            try std.testing.expectEqualStrings("{\"ok\":true}", signal_resume.payload_json);
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

test "parseTraceFile rejects malformed recorded values" {
    const source =
        "{\"type\":\"request\",\"method\":\"GET\",\"url\":\"/\",\"headers\":{},\"body\":null}\n" ++
        "{\"type\":\"response\",\"status\":200,\"headers\":{},\"body\":\"ok\"}\n" ++
        "{\"type\":\"meta\",\"duration_us\":1,\"handler\":\"h.ts\",\"pool_slot\":0,\"io_count\":0}\n" ++
        "{\"type\":\"request\",\"method\":\"GET\",\"url\":\"/malformed\",\"headers\":{},\"body\":null}\n" ++
        "{\"type\":\"io\",\"seq\":0,\"module\":\"env\",\"fn\":\"env\",\"args\":[],\"result\":not-json}\n";

    try std.testing.expectError(
        error.InvalidTraceJson,
        parseTraceFile(std.testing.allocator, source),
    );
}

test "parseTraceFile rejects unknown trace entry types" {
    const source =
        \\{"type":"request","method":"GET","url":"/","headers":{},"body":null}
        \\{"type":"respones","status":200,"headers":{},"body":"ok"}
        \\{"type":"meta","duration_us":1,"handler":"handler.ts","pool_slot":0,"io_count":0}
    ;
    try std.testing.expectError(
        error.InvalidTraceEntry,
        parseTraceFile(std.testing.allocator, source),
    );
}

test "parseTraceFile rejects response-less traces without a witness tag" {
    const source =
        \\{"type":"request","method":"GET","url":"/","headers":{},"body":null}
        \\{"type":"meta","duration_us":1,"handler":"handler.ts","pool_slot":0,"io_count":0}
    ;
    try std.testing.expectError(
        error.InvalidTraceEntry,
        parseTraceFile(std.testing.allocator, source),
    );
}

test "parseTraceFile preserves optional absence and recorded undefined" {
    const source =
        "{\"type\":\"witness\",\"property\":\"optional-values\"}\n" ++
        "{\"type\":\"request\",\"method\":\"GET\",\"url\":\"/\",\"headers\":{},\"body\":null}\n" ++
        "{\"type\":\"io\",\"seq\":0,\"module\":\"env\",\"fn\":\"env\",\"args\":[]}\n" ++
        "{\"type\":\"io\",\"seq\":1,\"module\":\"env\",\"fn\":\"env\",\"args\":[],\"result\":null}\n";

    const groups = try parseTraceFile(std.testing.allocator, source);
    defer {
        for (groups) |group| std.testing.allocator.free(group.io_calls);
        std.testing.allocator.free(groups);
    }

    try std.testing.expectEqual(@as(usize, 1), groups.len);
    try std.testing.expectEqual(@as(usize, 2), groups[0].io_calls.len);
    try std.testing.expectEqualStrings("null", groups[0].io_calls[0].result_json);
    try std.testing.expectEqualStrings("null", groups[0].io_calls[1].result_json);

    const zts = @import("root.zig");
    var test_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer test_arena.deinit();
    const ctx = try zts.createContext(test_arena.allocator(), .{ .nursery_size = 4096 });
    defer zts.destroyContext(ctx);
    try std.testing.expect(jsonToJSValue(ctx, groups[0].io_calls[1].result_json).isUndefined());
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

test "ReplayState peekMatches does not consume or count divergence" {
    const io_calls = [_]IoEntry{
        .{ .seq = 0, .module = "validate", .func = "validateJson", .args_json = "[]", .result_json = "{\"ok\":true}" },
    };
    var state = ReplayState{
        .io_calls = &io_calls,
        .cursor = 0,
        .divergences = 0,
    };

    // Head matches: peek is true and leaves the cursor and divergence untouched.
    try std.testing.expect(state.peekMatches("validate", "validateJson"));
    try std.testing.expectEqual(@as(u32, 0), state.cursor);
    try std.testing.expectEqual(@as(u32, 0), state.divergences);

    // Head does not match this (pure) call: peek is false, still no consume,
    // so a fallback stub can run the real impl and leave the entry for the
    // genuinely-recorded call that follows.
    try std.testing.expect(!state.peekMatches("crypto", "sha256"));
    try std.testing.expectEqual(@as(u32, 0), state.cursor);
    try std.testing.expectEqual(@as(u32, 0), state.divergences);

    // The matching entry is still available to consume normally.
    try std.testing.expect(state.nextIO("validate", "validateJson") != null);
    try std.testing.expectEqual(@as(u32, 1), state.cursor);

    // Exhausted: peek is false without underflow.
    try std.testing.expect(!state.peekMatches("validate", "validateJson"));
}

test "ReplayState consumeHead and hasEntryAhead" {
    const io_calls = [_]IoEntry{
        .{ .seq = 0, .module = "http", .func = "fetchSync", .args_json = "[]", .result_json = "{}" },
        .{ .seq = 1, .module = "validate", .func = "validateJson", .args_json = "[]", .result_json = "{\"ok\":true}" },
    };
    var state = ReplayState{ .io_calls = &io_calls, .cursor = 0, .divergences = 0 };

    // A pure call whose recorded entry sits AHEAD of the head (the head is a
    // different, effectful call): hasEntryAhead is true -> the fallback flags a
    // divergence rather than silently absorbing the drift.
    try std.testing.expect(!state.peekMatches("validate", "validateJson"));
    try std.testing.expect(state.hasEntryAhead("validate", "validateJson"));

    // A genuinely-new pure call appears in no recording: no divergence.
    try std.testing.expect(!state.hasEntryAhead("crypto", "sha256"));

    // consumeHead trusts a prior peek: advances exactly one without touching
    // divergence, returning the head entry.
    try std.testing.expect(state.peekMatches("http", "fetchSync"));
    const head = state.consumeHead();
    try std.testing.expectEqualStrings("http", head.module);
    try std.testing.expectEqual(@as(u32, 1), state.cursor);
    try std.testing.expectEqual(@as(u32, 0), state.divergences);

    // Now validate is at the head and no longer "ahead".
    try std.testing.expect(state.peekMatches("validate", "validateJson"));
}

test "ReplayState unconsumedCount flags a dropped trailing effect" {
    const io_calls = [_]IoEntry{
        .{ .seq = 0, .module = "cache", .func = "cacheGet", .args_json = "[]", .result_json = "{}" },
        .{ .seq = 1, .module = "cache", .func = "cacheSet", .args_json = "[]", .result_json = "{}" },
    };
    var state = ReplayState{ .io_calls = &io_calls, .cursor = 0, .divergences = 0 };

    // Nothing consumed yet: both entries are leftover.
    try std.testing.expectEqual(@as(u32, 2), state.unconsumedCount());

    // Edited handler made only the cacheGet (dropped the trailing cacheSet):
    // one entry stays unconsumed, so replay must fail closed.
    _ = state.consumeHead();
    try std.testing.expectEqual(@as(u32, 1), state.unconsumedCount());

    // Faithful replay consumes every entry: no leftover.
    _ = state.consumeHead();
    try std.testing.expectEqual(@as(u32, 0), state.unconsumedCount());
}

test "argsDrifted flags changed args, skips unpinned recordings" {
    // Same args -> no drift.
    try std.testing.expect(!argsDrifted("[\"AAA\"]", "[\"AAA\"]"));
    // Changed arg -> drift (the matched-path divergence #5 detects).
    try std.testing.expect(argsDrifted("[\"AAA\"]", "[\"BBB\"]"));
    // Different arity -> drift.
    try std.testing.expect(argsDrifted("[\"a\",\"b\"]", "[\"a\"]"));
    // Recording did not pin args ("[]"/empty) -> never flagged, regardless of
    // live args (protects zero-arg calls and hand-written fixtures).
    try std.testing.expect(!argsDrifted("[]", "[\"a\"]"));
    try std.testing.expect(!argsDrifted("", "[\"a\"]"));
    // Zero-arg call against a zero-arg recording -> no drift.
    try std.testing.expect(!argsDrifted("[]", "[]"));
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

test "isIncompleteOplog" {
    // Incomplete: has request but no complete marker
    const incomplete =
        "{\"type\":\"request\",\"method\":\"GET\",\"url\":\"/a\",\"headers\":{},\"body\":null}\n" ++
        "{\"type\":\"io\",\"seq\":0,\"module\":\"env\",\"fn\":\"env\",\"args\":[\"K\"],\"result\":\"V\"}\n";
    try std.testing.expect(isIncompleteOplog(incomplete));

    // Complete: has request and complete marker
    const complete =
        "{\"type\":\"request\",\"method\":\"GET\",\"url\":\"/a\",\"headers\":{},\"body\":null}\n" ++
        "{\"type\":\"io\",\"seq\":0,\"module\":\"env\",\"fn\":\"env\",\"args\":[\"K\"],\"result\":\"V\"}\n" ++
        "{\"type\":\"response\",\"status\":200,\"headers\":{},\"body\":\"ok\"}\n" ++
        "{\"type\":\"complete\"}\n";
    try std.testing.expect(!isIncompleteOplog(complete));

    // Empty: no request
    try std.testing.expect(!isIncompleteOplog(""));

    // Only complete marker, no request
    try std.testing.expect(!isIncompleteOplog("{\"type\":\"complete\"}\n"));
}

test "parseIncompleteOplog" {
    const source =
        "{\"type\":\"durable_run\",\"key\":\"order:123\"}\n" ++
        "{\"type\":\"request\",\"method\":\"POST\",\"url\":\"/api/data\",\"headers\":{},\"body\":\"payload\"}\n" ++
        "{\"type\":\"io\",\"seq\":0,\"module\":\"env\",\"fn\":\"env\",\"args\":[\"KEY\"],\"result\":\"val\"}\n" ++
        "{\"type\":\"io\",\"seq\":1,\"module\":\"cache\",\"fn\":\"cacheGet\",\"args\":[\"ns\",\"k\"],\"result\":null}\n";

    var parsed = try parseIncompleteOplog(std.testing.allocator, source);
    defer parsed.deinit();

    try std.testing.expectEqualStrings("POST", parsed.request.method);
    try std.testing.expectEqualStrings("/api/data", parsed.request.url);
    try std.testing.expectEqualStrings("order:123", parsed.run_key.?);
    try std.testing.expectEqual(@as(usize, 2), parsed.events.len);
    switch (parsed.events[0]) {
        .io => |io| try std.testing.expectEqualStrings("env", io.module),
        else => return error.UnexpectedTraceType,
    }
    switch (parsed.events[1]) {
        .io => |io| try std.testing.expectEqualStrings("cache", io.module),
        else => return error.UnexpectedTraceType,
    }
}

test "DurableState replay then live" {
    const events = [_]DurableEvent{
        .{ .io = .{ .seq = 0, .module = "env", .func = "env", .args_json = "[\"K\"]", .result_json = "\"V\"" } },
    };
    var state = DurableState.init(
        std.testing.allocator,
        &events,
        -1, // dummy fd, not used in this test
    );
    defer state.deinit();

    // Initially in replay mode
    try std.testing.expect(!state.is_live);

    // First call: replays from oplog
    const entry = state.replayNext("env", "env");
    try std.testing.expect(entry != null);
    try std.testing.expectEqualStrings("\"V\"", entry.?.result_json);

    // Oplog exhausted: transitions to live mode
    const entry2 = state.replayNext("cache", "cacheGet");
    try std.testing.expect(entry2 == null);
    try std.testing.expect(state.is_live);
}

test "DurableState divergence triggers live mode" {
    const events = [_]DurableEvent{
        .{ .io = .{ .seq = 0, .module = "env", .func = "env", .args_json = "[\"K\"]", .result_json = "\"V\"" } },
    };
    var state = DurableState.init(
        std.testing.allocator,
        &events,
        -1,
    );
    defer state.deinit();

    // Call with wrong module: divergence transitions to live mode
    const entry = state.replayNext("cache", "cacheGet");
    try std.testing.expect(entry == null);
    try std.testing.expect(state.is_live);
}

test "DurableState empty oplog starts in live mode" {
    var state = DurableState.init(
        std.testing.allocator,
        &.{},
        -1,
    );
    defer state.deinit();

    try std.testing.expect(state.is_live);
    const entry = state.replayNext("env", "env");
    try std.testing.expect(entry == null);
}

test "writeAllChecked reports invalid durable fd" {
    try std.testing.expectError(error.DurableWriteFailed, writeAllChecked(@as(std.c.fd_t, -1), "x"));
}

test "DurableState persistence methods report invalid durable fd" {
    const gc_mod = @import("gc.zig");
    const heap_mod = @import("heap.zig");

    var gc_state = try gc_mod.GC.init(std.testing.allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var heap_state = heap_mod.Heap.init(std.testing.allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context.Context.init(std.testing.allocator, &gc_state, .{});
    defer ctx.deinit();

    var state = DurableState.init(
        std.testing.allocator,
        &.{},
        -1,
    );
    defer state.deinit();

    try std.testing.expectError(error.DurableWriteFailed, state.persistRunKey("order:bad-fd"));
    try std.testing.expectError(
        error.DurableWriteFailed,
        state.persistIO("builtin", "Math.random", ctx, &.{}, value.JSValue.fromInt(1)),
    );
    try std.testing.expectError(error.DurableWriteFailed, state.markComplete());
}

test "DurableState beginStep returns cached result" {
    const events = [_]DurableEvent{
        .{ .step_start = .{ .name = "charge" } },
        .{ .io = .{ .seq = 0, .module = "http", .func = "fetchSync", .args_json = "[\"https://payments.internal\"]", .result_json = "{\"ok\":true}" } },
        .{ .step_result = .{ .name = "charge", .result_json = "{\"status\":\"paid\"}" } },
    };
    var state = DurableState.init(std.testing.allocator, &events, -1);
    defer state.deinit();

    const replay = state.beginStep("charge");
    switch (replay) {
        .cached => |result_json| try std.testing.expectEqualStrings("{\"status\":\"paid\"}", result_json),
        else => return error.ExpectedCachedStep,
    }
    try std.testing.expectEqual(@as(u32, 3), state.cursor);
}

test "DurableState beginStep executes incomplete step" {
    const events = [_]DurableEvent{
        .{ .step_start = .{ .name = "charge" } },
        .{ .io = .{ .seq = 0, .module = "http", .func = "fetchSync", .args_json = "[\"https://payments.internal\"]", .result_json = "{\"ok\":true}" } },
    };
    var state = DurableState.init(std.testing.allocator, &events, -1);
    defer state.deinit();

    const replay = state.beginStep("charge");
    try std.testing.expectEqual(DurableStepReplay.execute, replay);
    try std.testing.expectEqual(@as(u32, 1), state.cursor);
}

test "DurableState beginTimerWait resumes from the recorded deadline" {
    // A relative sleep() recomputes now+delay on every recovery pass, so the
    // recomputed deadline never equals the recorded one. The recorded
    // deadline is authoritative; bailing to live re-arms a fresh timer and
    // the suspended sleep recedes forever.
    const events = [_]DurableEvent{
        .{ .wait_timer = .{ .until_ms = 1000 } },
    };
    var state = DurableState.init(std.testing.allocator, &events, -1);
    defer state.deinit();

    switch (state.beginTimerWait()) {
        .pending => |wait| try std.testing.expectEqual(@as(i64, 1000), wait.until_ms),
        else => return error.ExpectedPendingTimer,
    }
}

test "DurableState beginTimerWait ready after recorded resume" {
    const events = [_]DurableEvent{
        .{ .wait_timer = .{ .until_ms = 1000 } },
        .{ .resume_timer = .{ .fired_at_ms = 1200 } },
    };
    var state = DurableState.init(std.testing.allocator, &events, -1);
    defer state.deinit();

    switch (state.beginTimerWait()) {
        .ready => {},
        else => return error.ExpectedReadyTimer,
    }
    try std.testing.expect(!state.is_live);
}

test "parseDurableOplog treats a torn final line as truncation" {
    // A crash mid-append leaves a fragment without a terminating newline.
    // Lenient field defaults would fabricate an event from it (here a
    // resume_timer with fired_at_ms=0, which would fake a fired sleep).
    const source =
        "{\"type\":\"durable_run\",\"key\":\"order:1\"}\n" ++
        "{\"type\":\"wait_timer\",\"until_ms\":5000}\n" ++
        "{\"type\":\"resume_timer\",\"fired_at_";
    var parsed = try parseDurableOplog(std.testing.allocator, source);
    defer parsed.deinit();

    try std.testing.expectEqual(@as(usize, 1), parsed.events.len);
    switch (parsed.events[0]) {
        .wait_timer => |w| try std.testing.expectEqual(@as(i64, 5000), w.until_ms),
        else => return error.UnexpectedTraceType,
    }
}

test "isIncompleteOplog ignores a torn final line" {
    // A torn complete marker did not durably land; the run is incomplete.
    const torn =
        "{\"type\":\"request\",\"method\":\"GET\",\"url\":\"/a\",\"headers\":{},\"body\":null}\n" ++
        "{\"type\":\"complete\"}";
    try std.testing.expect(isIncompleteOplog(torn));
}
