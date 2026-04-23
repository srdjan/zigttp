//! Line-delimited JSON-RPC 2.0 adapter over stdio. Runs one long-lived
//! session and dispatches incoming requests to the same in-process surfaces
//! the REPL and TUI use (agent.runOneTurn, registry.invokeJson, catalogs).
//!
//! Protocol:
//!   - One JSON object per line on stdin; one response line per request on
//!     stdout. No batched calls, no positional params.
//!   - Requests:  {"jsonrpc":"2.0","id":<id>,"method":"...","params":{...}}
//!   - Responses: {"jsonrpc":"2.0","id":<id>,"result":...}
//!             or {"jsonrpc":"2.0","id":<id>,"error":{code,message}}
//!   - Turn events emit as notifications with method "event" and params
//!     matching session/events.zig envelope ({"v":1,"k":"user_text",...}).
//!
//! Methods (v1):
//!   - turn(params: {text: string})
//!   - compact()
//!   - session.info()
//!   - tools.list()          tools.invoke(params: {name, args_json})
//!   - skills.list()         skills.invoke(params: {name})
//!   - templates.list()      templates.expand(params: {name, args: [..]})
//!   - model.list()          model.set(params: {id})
//!   - shutdown()            returns ok, main loop exits on next read

const std = @import("std");

const agent = @import("agent.zig");
const app = @import("app.zig");
const loop = @import("loop.zig");
const registry_mod = @import("registry/registry.zig");
const transcript_mod = @import("transcript.zig");
const session_events = @import("session/events.zig");
const skills_catalog = @import("skills/catalog.zig");
const prompts_catalog = @import("prompts/catalog.zig");
const models_registry = @import("providers/models.zig");
const json_writer = @import("providers/anthropic/json_writer.zig");

const Registry = registry_mod.Registry;
const ExpertFlags = app.ExpertFlags;

/// JSON-RPC 2.0 error codes (https://www.jsonrpc.org/specification).
const PARSE_ERROR: i32 = -32700;
const INVALID_REQUEST: i32 = -32600;
const METHOD_NOT_FOUND: i32 = -32601;
const INVALID_PARAMS: i32 = -32602;
const INTERNAL_ERROR: i32 = -32603;

/// Maximum size of one line-delimited request. 4 MiB matches the upper bound
/// transcript entries can grow to during long turns; anything above this is
/// almost certainly a framing error.
const MAX_LINE_BYTES: usize = 4 * 1024 * 1024;

pub fn run(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    flags: ExpertFlags,
    policy: loop.ApprovalPolicy,
) !void {
    var session = try agent.initFromEnvWithSessionConfig(allocator, registry, .{
        .no_session = flags.no_session,
        .no_persist_tool_output = flags.no_persist_tool_output,
        .no_context_files = flags.no_context_files,
        .session_id = flags.session_id,
        .resume_latest = flags.resume_latest,
        .fork_session_id = flags.fork_session_id,
    });
    defer session.deinit(allocator);

    try runWithSession(allocator, &session, registry, policy, null, null);
}

/// Test-facing variant. Injects an optional stdin reader and stdout writer
/// so tests can drive the loop without touching real file descriptors.
pub fn runWithSession(
    allocator: std.mem.Allocator,
    session: *agent.AgentSession,
    registry: *const Registry,
    policy: loop.ApprovalPolicy,
    in_reader: ?*std.Io.Reader,
    out_writer: ?*std.Io.Writer,
) !void {
    const approval_fn = loop.resolveApprovalFn(policy, null);

    var line_buf: std.ArrayList(u8) = .empty;
    defer line_buf.deinit(allocator);

    while (true) {
        line_buf.clearRetainingCapacity();
        const ok = try readOneLine(allocator, in_reader, &line_buf);
        if (!ok) break;
        if (line_buf.items.len == 0) continue;

        const done = try dispatchLine(
            allocator,
            session,
            registry,
            approval_fn,
            line_buf.items,
            out_writer,
        );
        if (done) break;
    }
}

/// Reads one `\n`-terminated line into `out`. Returns false on EOF.
/// `out` never contains the trailing newline.
fn readOneLine(
    allocator: std.mem.Allocator,
    in: ?*std.Io.Reader,
    out: *std.ArrayList(u8),
) !bool {
    if (in) |reader| {
        while (true) {
            if (out.items.len > MAX_LINE_BYTES) return error.LineTooLong;
            const b = reader.takeByte() catch |err| switch (err) {
                error.EndOfStream => return out.items.len > 0,
                else => return err,
            };
            if (b == '\n') return true;
            try out.append(allocator, b);
        }
    }

    // Production path: one byte at a time from stdin. We accept the syscall
    // overhead for line-oriented correctness; RPC is not a throughput path.
    var byte: [1]u8 = undefined;
    while (true) {
        if (out.items.len > MAX_LINE_BYTES) return error.LineTooLong;
        const rc = std.c.read(std.c.STDIN_FILENO, &byte, 1);
        if (rc == 0) return out.items.len > 0;
        if (rc < 0) return error.ReadFailure;
        if (byte[0] == '\n') return true;
        try out.append(allocator, byte[0]);
    }
}

/// Parse one request line and dispatch. Returns true when the caller should
/// stop the loop (i.e. the `shutdown` method succeeded).
fn dispatchLine(
    allocator: std.mem.Allocator,
    session: *agent.AgentSession,
    registry: *const Registry,
    approval_fn: ?loop.ApprovalFn,
    line: []const u8,
    out: ?*std.Io.Writer,
) !bool {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, line, .{}) catch {
        try emitError(allocator, out, .{ .null = {} }, PARSE_ERROR, "parse error");
        return false;
    };
    defer parsed.deinit();

    if (parsed.value != .object) {
        try emitError(allocator, out, .{ .null = {} }, INVALID_REQUEST, "request must be a JSON object");
        return false;
    }
    const obj = parsed.value.object;

    const id: std.json.Value = obj.get("id") orelse .{ .null = {} };
    const method_val = obj.get("method") orelse {
        try emitError(allocator, out, id, INVALID_REQUEST, "missing method");
        return false;
    };
    if (method_val != .string) {
        try emitError(allocator, out, id, INVALID_REQUEST, "method must be a string");
        return false;
    }
    const method = method_val.string;
    const params: ?std.json.Value = obj.get("params");

    return dispatchMethod(allocator, session, registry, approval_fn, method, params, id, out);
}

fn dispatchMethod(
    allocator: std.mem.Allocator,
    session: *agent.AgentSession,
    registry: *const Registry,
    approval_fn: ?loop.ApprovalFn,
    method: []const u8,
    params: ?std.json.Value,
    id: std.json.Value,
    out: ?*std.Io.Writer,
) !bool {
    if (std.mem.eql(u8, method, "shutdown")) {
        try emitResultString(allocator, out, id, "ok");
        return true;
    }
    if (std.mem.eql(u8, method, "session.info")) {
        try handleSessionInfo(allocator, session, id, out);
        return false;
    }
    if (std.mem.eql(u8, method, "model.list")) {
        try handleModelList(allocator, id, out);
        return false;
    }
    if (std.mem.eql(u8, method, "model.set")) {
        try handleModelSet(allocator, session, params, id, out);
        return false;
    }
    if (std.mem.eql(u8, method, "skills.list")) {
        try handleSkillsList(allocator, id, out);
        return false;
    }
    if (std.mem.eql(u8, method, "templates.list")) {
        try handleTemplatesList(allocator, id, out);
        return false;
    }
    if (std.mem.eql(u8, method, "templates.expand")) {
        try handleTemplatesExpand(allocator, params, id, out);
        return false;
    }
    if (std.mem.eql(u8, method, "tools.list")) {
        try handleToolsList(allocator, registry, id, out);
        return false;
    }
    if (std.mem.eql(u8, method, "tools.invoke")) {
        try handleToolsInvoke(allocator, registry, params, id, out);
        return false;
    }
    if (std.mem.eql(u8, method, "compact")) {
        try handleCompact(allocator, session, id, out);
        return false;
    }
    if (std.mem.eql(u8, method, "turn")) {
        try handleTurn(allocator, session, registry, approval_fn, params, id, out);
        return false;
    }

    try emitErrorFmt(allocator, out, id, METHOD_NOT_FOUND, "method not found: {s}", .{method});
    return false;
}

// ---------------------------------------------------------------------------
// Params validation helpers
// ---------------------------------------------------------------------------

/// Checks that `params` is present and is a JSON object. On failure emits
/// the matching JSON-RPC error response and returns null; the caller just
/// `orelse return`s.
fn requireObjectParams(
    allocator: std.mem.Allocator,
    out: ?*std.Io.Writer,
    params: ?std.json.Value,
    id: std.json.Value,
) !?std.json.ObjectMap {
    const p = params orelse {
        try emitError(allocator, out, id, INVALID_PARAMS, "missing params");
        return null;
    };
    if (p != .object) {
        try emitError(allocator, out, id, INVALID_PARAMS, "params must be an object");
        return null;
    }
    return p.object;
}

/// Looks up `key` on `obj` and asserts it's a string. Emits `missing <key>`
/// or `<key> must be a string` and returns null on failure.
fn requireStringField(
    allocator: std.mem.Allocator,
    out: ?*std.Io.Writer,
    obj: std.json.ObjectMap,
    key: []const u8,
    id: std.json.Value,
) !?[]const u8 {
    const val = obj.get(key) orelse {
        try emitErrorFmt(allocator, out, id, INVALID_PARAMS, "missing {s}", .{key});
        return null;
    };
    if (val != .string) {
        try emitErrorFmt(allocator, out, id, INVALID_PARAMS, "{s} must be a string", .{key});
        return null;
    }
    return val.string;
}

// ---------------------------------------------------------------------------
// Method handlers
// ---------------------------------------------------------------------------

fn handleSessionInfo(
    allocator: std.mem.Allocator,
    session: *const agent.AgentSession,
    id: std.json.Value,
    out: ?*std.Io.Writer,
) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll("{\"session_id\":");
    if (session.session_id) |sid| try json_writer.writeString(w, sid) else try w.writeAll("null");
    try w.writeAll(",\"model\":");
    if (session.currentModel()) |m| try json_writer.writeString(w, m) else try w.writeAll("null");
    try w.writeAll(",\"transcript_len\":");
    try w.print("{d}", .{session.transcript.len()});
    try w.writeAll(",\"tokens\":{\"input\":");
    try w.print("{d}", .{session.token_totals.input_tokens});
    try w.writeAll(",\"output\":");
    try w.print("{d}", .{session.token_totals.output_tokens});
    try w.writeAll(",\"cache_read\":");
    try w.print("{d}", .{session.token_totals.cache_read_input_tokens});
    try w.writeAll(",\"cache_creation\":");
    try w.print("{d}", .{session.token_totals.cache_creation_input_tokens});
    try w.writeAll("}}");

    buf = aw.toArrayList();
    try emitResultRaw(allocator, out, id, buf.items);
}

fn handleModelList(
    allocator: std.mem.Allocator,
    id: std.json.Value,
    out: ?*std.Io.Writer,
) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeByte('[');
    inline for (models_registry.registry, 0..) |m, i| {
        if (i > 0) try w.writeByte(',');
        try w.writeAll("{\"id\":");
        try json_writer.writeString(w, m.id);
        try w.writeAll(",\"display_name\":");
        try json_writer.writeString(w, m.display_name);
        try w.print(",\"context_window\":{d},\"max_output_tokens\":{d}}}", .{ m.context_window, m.max_output_tokens });
    }
    try w.writeByte(']');

    buf = aw.toArrayList();
    try emitResultRaw(allocator, out, id, buf.items);
}

fn handleModelSet(
    allocator: std.mem.Allocator,
    session: *agent.AgentSession,
    params: ?std.json.Value,
    id: std.json.Value,
    out: ?*std.Io.Writer,
) !void {
    const obj = (try requireObjectParams(allocator, out, params, id)) orelse return;
    const model_id = (try requireStringField(allocator, out, obj, "id", id)) orelse return;
    const model = models_registry.findById(model_id) orelse {
        try emitErrorFmt(allocator, out, id, INVALID_PARAMS, "unknown model: {s}", .{model_id});
        return;
    };
    session.setModel(model.id);
    try emitResultString(allocator, out, id, model.id);
}

fn handleSkillsList(
    allocator: std.mem.Allocator,
    id: std.json.Value,
    out: ?*std.Io.Writer,
) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeByte('[');
    inline for (skills_catalog.catalog, 0..) |s, i| {
        if (i > 0) try w.writeByte(',');
        try w.writeAll("{\"name\":");
        try json_writer.writeString(w, s.name);
        try w.writeAll(",\"description\":");
        try json_writer.writeString(w, s.description);
        try w.writeByte('}');
    }
    try w.writeByte(']');

    buf = aw.toArrayList();
    try emitResultRaw(allocator, out, id, buf.items);
}

fn handleTemplatesList(
    allocator: std.mem.Allocator,
    id: std.json.Value,
    out: ?*std.Io.Writer,
) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeByte('[');
    inline for (prompts_catalog.catalog, 0..) |t, i| {
        if (i > 0) try w.writeByte(',');
        try w.writeAll("{\"name\":");
        try json_writer.writeString(w, t.name);
        try w.writeAll(",\"description\":");
        try json_writer.writeString(w, t.description);
        try w.writeByte('}');
    }
    try w.writeByte(']');

    buf = aw.toArrayList();
    try emitResultRaw(allocator, out, id, buf.items);
}

fn handleTemplatesExpand(
    allocator: std.mem.Allocator,
    params: ?std.json.Value,
    id: std.json.Value,
    out: ?*std.Io.Writer,
) !void {
    const obj = (try requireObjectParams(allocator, out, params, id)) orelse return;
    const tmpl_name = (try requireStringField(allocator, out, obj, "name", id)) orelse return;
    const tmpl = prompts_catalog.findByName(tmpl_name) orelse {
        try emitErrorFmt(allocator, out, id, INVALID_PARAMS, "unknown template: {s}", .{tmpl_name});
        return;
    };

    var args: std.ArrayList([]const u8) = .empty;
    defer args.deinit(allocator);
    if (obj.get("args")) |args_val| {
        if (args_val != .array) {
            try emitError(allocator, out, id, INVALID_PARAMS, "args must be an array of strings");
            return;
        }
        for (args_val.array.items) |v| {
            if (v != .string) {
                try emitError(allocator, out, id, INVALID_PARAMS, "args must be strings");
                return;
            }
            try args.append(allocator, v.string);
        }
    }

    const expanded = try prompts_catalog.expand(allocator, tmpl.body, args.items);
    defer allocator.free(expanded);
    try emitResultString(allocator, out, id, expanded);
}

fn handleToolsList(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    id: std.json.Value,
    out: ?*std.Io.Writer,
) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeByte('[');
    for (registry.entries.items, 0..) |tool, i| {
        if (i > 0) try w.writeByte(',');
        try w.writeAll("{\"name\":");
        try json_writer.writeString(w, tool.name);
        try w.writeAll(",\"label\":");
        try json_writer.writeString(w, tool.label);
        try w.writeAll(",\"description\":");
        try json_writer.writeString(w, tool.description);
        try w.writeByte('}');
    }
    try w.writeByte(']');

    buf = aw.toArrayList();
    try emitResultRaw(allocator, out, id, buf.items);
}

fn handleToolsInvoke(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    params: ?std.json.Value,
    id: std.json.Value,
    out: ?*std.Io.Writer,
) !void {
    const obj = (try requireObjectParams(allocator, out, params, id)) orelse return;
    const tool_name = (try requireStringField(allocator, out, obj, "name", id)) orelse return;
    // args_json is passed through to registry.invokeJson. Absent = {}.
    const args_json: []const u8 = blk: {
        if (obj.get("args_json")) |aj_val| {
            if (aj_val != .string) {
                try emitError(allocator, out, id, INVALID_PARAMS, "args_json must be a string");
                return;
            }
            break :blk aj_val.string;
        }
        break :blk "{}";
    };

    const args_owned = try allocator.dupe(u8, args_json);
    defer allocator.free(args_owned);

    var result = registry.invokeJson(allocator, tool_name, args_owned) catch |err| switch (err) {
        registry_mod.RegistryError.ToolNotFound => {
            try emitErrorFmt(allocator, out, id, INVALID_PARAMS, "unknown tool: {s}", .{tool_name});
            return;
        },
        else => {
            try emitErrorFmt(allocator, out, id, INTERNAL_ERROR, "tool failed: {s}", .{@errorName(err)});
            return;
        },
    };
    defer result.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;
    try w.writeAll("{\"ok\":");
    try w.writeAll(if (result.ok) "true" else "false");
    try w.writeAll(",\"llm_text\":");
    try json_writer.writeString(w, result.llm_text);
    try w.writeAll(",\"body\":");
    try json_writer.writeString(w, result.llm_text);
    if (result.ui_payload) |payload| {
        try w.writeAll(",\"ui_payload\":");
        try @import("ui_payload.zig").writeJson(w, payload);
    }
    try w.writeByte('}');

    buf = aw.toArrayList();
    try emitResultRaw(allocator, out, id, buf.items);
}

fn handleCompact(
    allocator: std.mem.Allocator,
    session: *agent.AgentSession,
    id: std.json.Value,
    out: ?*std.Io.Writer,
) !void {
    const msg = try agent.compact(allocator, session);
    defer allocator.free(msg);
    try emitResultString(allocator, out, id, msg);
}

fn handleTurn(
    allocator: std.mem.Allocator,
    session: *agent.AgentSession,
    registry: *const Registry,
    approval_fn: ?loop.ApprovalFn,
    params: ?std.json.Value,
    id: std.json.Value,
    out: ?*std.Io.Writer,
) !void {
    const obj = (try requireObjectParams(allocator, out, params, id)) orelse return;
    const text = (try requireStringField(allocator, out, obj, "text", id)) orelse return;

    const start_len = session.transcript.len();
    const rendered = agent.runOneTurn(allocator, session, registry, text, approval_fn) catch |err| {
        try emitErrorFmt(allocator, out, id, INTERNAL_ERROR, "turn failed: {s}", .{@errorName(err)});
        return;
    };
    defer allocator.free(rendered);

    // Stream each new transcript entry as an "event" notification, then
    // respond with a summary result the caller can use to key further ops.
    const tr = &session.transcript;
    var idx: usize = start_len;
    while (idx < tr.len()) : (idx += 1) {
        try emitEntryNotification(allocator, out, tr.at(idx));
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;
    try w.writeAll("{\"appended\":");
    try w.print("{d}", .{tr.len() - start_len});
    try w.writeAll(",\"rendered\":");
    try json_writer.writeString(w, rendered);
    try w.writeByte('}');

    buf = aw.toArrayList();
    try emitResultRaw(allocator, out, id, buf.items);
}

// ---------------------------------------------------------------------------
// Emit helpers
// ---------------------------------------------------------------------------

fn writeOut(out: ?*std.Io.Writer, bytes: []const u8) !void {
    if (out) |w| {
        try w.writeAll(bytes);
        return;
    }
    _ = std.c.write(std.c.STDOUT_FILENO, bytes.ptr, bytes.len);
}

fn writeId(w: *std.Io.Writer, id: std.json.Value) !void {
    switch (id) {
        .null => try w.writeAll("null"),
        .integer => |n| try w.print("{d}", .{n}),
        .float => |f| try w.print("{d}", .{f}),
        .string => |s| try json_writer.writeString(w, s),
        .bool => |b| try w.writeAll(if (b) "true" else "false"),
        else => try w.writeAll("null"),
    }
}

fn emitResultRaw(
    allocator: std.mem.Allocator,
    out: ?*std.Io.Writer,
    id: std.json.Value,
    result_json: []const u8,
) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try writeId(w, id);
    try w.writeAll(",\"result\":");
    try w.writeAll(result_json);
    try w.writeAll("}\n");

    buf = aw.toArrayList();
    try writeOut(out, buf.items);
}

fn emitResultString(
    allocator: std.mem.Allocator,
    out: ?*std.Io.Writer,
    id: std.json.Value,
    s: []const u8,
) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try json_writer.writeString(&aw.writer, s);
    buf = aw.toArrayList();
    try emitResultRaw(allocator, out, id, buf.items);
}

fn emitError(
    allocator: std.mem.Allocator,
    out: ?*std.Io.Writer,
    id: std.json.Value,
    code: i32,
    message: []const u8,
) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try writeId(w, id);
    try w.print(",\"error\":{{\"code\":{d},\"message\":", .{code});
    try json_writer.writeString(w, message);
    try w.writeAll("}}\n");

    buf = aw.toArrayList();
    try writeOut(out, buf.items);
}

fn emitErrorFmt(
    allocator: std.mem.Allocator,
    out: ?*std.Io.Writer,
    id: std.json.Value,
    code: i32,
    comptime fmt: []const u8,
    args: anytype,
) !void {
    const msg = try std.fmt.allocPrint(allocator, fmt, args);
    defer allocator.free(msg);
    try emitError(allocator, out, id, code, msg);
}

fn emitEntryNotification(
    allocator: std.mem.Allocator,
    out: ?*std.Io.Writer,
    entry: *const transcript_mod.OwnedEntry,
) !void {
    const record: ?session_events.EventRecord = switch (entry.*) {
        .user_text => |b| .{ .user_text = b },
        .model_text => |b| .{ .model_text = b },
        .proof_card => |b| .{ .proof_card = .{
            .llm_text = b.llm_text,
            .ui_payload = b.ui_payload,
        } },
        .diagnostic_box => |b| .{ .diagnostic_box = .{
            .llm_text = b.llm_text,
            .ui_payload = b.ui_payload,
        } },
        .system_note => |b| .{ .system_note = b },
        .tool_result => |tr| .{ .tool_result = .{
            .tool_use_id = tr.tool_use_id,
            .tool_name = tr.tool_name,
            .ok = tr.ok,
            .llm_text = tr.llm_text,
            .ui_payload = tr.ui_payload,
        } },
        .assistant_tool_use => null,
    };

    if (record) |r| {
        try emitNotification(allocator, out, r);
        return;
    }
    // Fan out one notification per tool call.
    const calls = entry.assistant_tool_use;
    for (calls) |c| {
        try emitNotification(allocator, out, .{ .tool_use = .{
            .id = c.id,
            .name = c.name,
            .args_json = c.args_json,
        } });
    }
}

fn emitNotification(
    allocator: std.mem.Allocator,
    out: ?*std.Io.Writer,
    record: session_events.EventRecord,
) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll("{\"jsonrpc\":\"2.0\",\"method\":\"event\",\"params\":");
    try session_events.writeEventLine(w, record);
    // writeEventLine appends '\n'; strip it by overwriting and adding the
    // closing brace + newline ourselves.
    buf = aw.toArrayList();
    // Drop the trailing '\n' written by writeEventLine so the wrapping
    // notification object stays on one line.
    if (buf.items.len > 0 and buf.items[buf.items.len - 1] == '\n') {
        _ = buf.pop();
    }
    try buf.appendSlice(allocator, "}\n");
    try writeOut(out, buf.items);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;
const turn = @import("turn.zig");
const meta_tool = @import("tools/zigts_expert_meta.zig");

const CannedClient = struct {
    reply: turn.AssistantReply,

    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) anyerror!loop.ModelCallResult {
        const self: *CannedClient = @ptrCast(@alignCast(ctx));
        _ = arena;
        _ = transcript;
        _ = extra_user_text;
        return .{ .reply = self.reply };
    }

    fn asModelClient(self: *CannedClient) loop.ModelClient {
        return .{ .context = self, .request_fn = requestFn };
    }
};

fn buildMiniRegistry(allocator: std.mem.Allocator) !Registry {
    var reg: Registry = .{};
    errdefer reg.deinit(allocator);
    try reg.register(allocator, meta_tool.tool);
    return reg;
}

fn driveWith(
    allocator: std.mem.Allocator,
    input: []const u8,
    out: *std.Io.Writer.Allocating,
) !void {
    var reg = try buildMiniRegistry(allocator);
    defer reg.deinit(allocator);

    var session = agent.AgentSession.initStub();
    defer session.deinit(allocator);

    var reader: std.Io.Reader = .fixed(input);
    try runWithSession(allocator, &session, &reg, .auto_reject, &reader, &out.writer);
}

test "rpc: shutdown returns ok and stops the loop" {
    const allocator = testing.allocator;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try driveWith(
        allocator,
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"shutdown\"}\n" ++
            "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"session.info\"}\n",
        &aw,
    );

    buf = aw.toArrayList();
    // First response must be for id=1 shutdown; the second request must be
    // ignored because the loop exits.
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"id\":1") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"result\":\"ok\"") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"id\":2") == null);
}

test "rpc: parse error yields id null and code -32700" {
    const allocator = testing.allocator;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try driveWith(
        allocator,
        "{not json\n" ++
            "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"shutdown\"}\n",
        &aw,
    );

    buf = aw.toArrayList();
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"code\":-32700") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"id\":null") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"result\":\"ok\"") != null);
}

test "rpc: unknown method returns -32601" {
    const allocator = testing.allocator;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try driveWith(
        allocator,
        "{\"jsonrpc\":\"2.0\",\"id\":\"x\",\"method\":\"no.such.thing\"}\n" ++
            "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"shutdown\"}\n",
        &aw,
    );

    buf = aw.toArrayList();
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"code\":-32601") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"id\":\"x\"") != null);
}

test "rpc: model.list enumerates registered models" {
    const allocator = testing.allocator;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try driveWith(
        allocator,
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"model.list\"}\n" ++
            "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"shutdown\"}\n",
        &aw,
    );

    buf = aw.toArrayList();
    try testing.expect(std.mem.indexOf(u8, buf.items, "claude-opus-4-6") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "claude-sonnet-4-6") != null);
}

test "rpc: skills.list and templates.list return catalog metadata" {
    const allocator = testing.allocator;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try driveWith(
        allocator,
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"skills.list\"}\n" ++
            "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"templates.list\"}\n" ++
            "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"shutdown\"}\n",
        &aw,
    );

    buf = aw.toArrayList();
    try testing.expect(std.mem.indexOf(u8, buf.items, "handler-scaffold") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"name\":\"explain\"") != null);
}

test "rpc: templates.expand substitutes positional args" {
    const allocator = testing.allocator;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try driveWith(
        allocator,
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"templates.expand\",\"params\":{\"name\":\"review\",\"args\":[\"handler.ts\"]}}\n" ++
            "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"shutdown\"}\n",
        &aw,
    );

    buf = aw.toArrayList();
    try testing.expect(std.mem.indexOf(u8, buf.items, "handler.ts") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "{{1}}") == null);
}

test "rpc: tools.list returns registered tool names" {
    const allocator = testing.allocator;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try driveWith(
        allocator,
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools.list\"}\n" ++
            "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"shutdown\"}\n",
        &aw,
    );

    buf = aw.toArrayList();
    try testing.expect(std.mem.indexOf(u8, buf.items, "zigts_expert_meta") != null);
}

test "rpc: session.info reports stub session fields" {
    const allocator = testing.allocator;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try driveWith(
        allocator,
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"session.info\"}\n" ++
            "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"shutdown\"}\n",
        &aw,
    );

    buf = aw.toArrayList();
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"session_id\":null") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"transcript_len\":0") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"input\":0") != null);
}

test "rpc: turn missing params returns INVALID_PARAMS" {
    const allocator = testing.allocator;
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();

    try driveWith(
        allocator,
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"turn\"}\n" ++
            "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"shutdown\"}\n",
        &aw,
    );

    const out = aw.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, out, "\"code\":-32602") != null);
    try testing.expect(std.mem.indexOf(u8, out, "\"message\":\"missing params\"") != null);
}

test "rpc: turn missing text field returns INVALID_PARAMS" {
    const allocator = testing.allocator;
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();

    try driveWith(
        allocator,
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"turn\",\"params\":{}}\n" ++
            "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"shutdown\"}\n",
        &aw,
    );

    const out = aw.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, out, "\"code\":-32602") != null);
    try testing.expect(std.mem.indexOf(u8, out, "missing text") != null);
}

test "rpc: turn with stub session emits event notifications and a final result" {
    const allocator = testing.allocator;
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();

    try driveWith(
        allocator,
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"turn\",\"params\":{\"text\":\"hello\"}}\n" ++
            "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"shutdown\"}\n",
        &aw,
    );

    const out = aw.writer.buffered();
    // user_text event notification for the prompt.
    try testing.expect(std.mem.indexOf(u8, out, "\"method\":\"event\"") != null);
    try testing.expect(std.mem.indexOf(u8, out, "\"k\":\"user_text\"") != null);
    try testing.expect(std.mem.indexOf(u8, out, "\"k\":\"model_text\"") != null);
    // Final result: two entries appended (user + stub reply).
    try testing.expect(std.mem.indexOf(u8, out, "\"id\":1,\"result\":") != null);
    try testing.expect(std.mem.indexOf(u8, out, "\"appended\":2") != null);
}

test "rpc: tools.invoke missing name returns INVALID_PARAMS" {
    const allocator = testing.allocator;
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();

    try driveWith(
        allocator,
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools.invoke\",\"params\":{}}\n" ++
            "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"shutdown\"}\n",
        &aw,
    );

    const out = aw.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, out, "\"code\":-32602") != null);
    try testing.expect(std.mem.indexOf(u8, out, "missing name") != null);
}

test "rpc: tools.invoke with unknown tool name surfaces as INVALID_PARAMS" {
    const allocator = testing.allocator;
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();

    try driveWith(
        allocator,
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools.invoke\",\"params\":{\"name\":\"no_such_tool\"}}\n" ++
            "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"shutdown\"}\n",
        &aw,
    );

    const out = aw.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, out, "\"code\":-32602") != null);
    try testing.expect(std.mem.indexOf(u8, out, "unknown tool: no_such_tool") != null);
}

test "rpc: tools.invoke with known tool returns {ok, body}" {
    const allocator = testing.allocator;
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();

    try driveWith(
        allocator,
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools.invoke\",\"params\":{\"name\":\"zigts_expert_meta\"}}\n" ++
            "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"shutdown\"}\n",
        &aw,
    );

    const out = aw.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, out, "\"id\":1,\"result\":{\"ok\":true") != null);
    try testing.expect(std.mem.indexOf(u8, out, "compiler_version") != null);
}
