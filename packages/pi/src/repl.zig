//! Line-buffered expert REPL driver.
//!
//! Natural language goes to the model by default. Deterministic slash commands,
//! safe raw `zigts ...` / `zig build ...` commands, and direct tool names are
//! routed locally.

const std = @import("std");
const registry_mod = @import("registry/registry.zig");
const agent = @import("agent.zig");
const commands = @import("commands.zig");
const loop = @import("loop.zig");
const app = @import("app.zig");
const skills_catalog = @import("skills/catalog.zig");
const prompts_catalog = @import("prompts/catalog.zig");
const models_registry = @import("providers/models.zig");
const theme_mod = @import("tui/theme.zig");

pub const Registry = registry_mod.Registry;
const ToolResult = registry_mod.ToolResult;
const SessionTreeNode = @import("registry/tool.zig").SessionTreeNode;
const ExpertFlags = app.ExpertFlags;

pub const DispatchOutcome = union(enum) {
    noop,
    quit,
    result: ToolResult,
    session_resume,
    session_new,
    session_compact,
    session_fork,
};

/// Surface-agnostic result of processing one submitted line. Both the
/// line-buffered REPL and the raw-mode TUI share this shape so the only
/// per-surface code is how `rendered` / `tool_result` bytes are written.
pub const SubmitOutcome = union(enum) {
    noop,
    quit,
    /// Owned rendered text from `agent.runOneTurn`. Caller frees.
    rendered: []u8,
    /// Dispatched tool result. Caller calls `.deinit(allocator)`.
    tool_result: ToolResult,
    session_resume,
    session_new,
    session_compact,
    session_fork,
};

/// Routes a single line through either the model (NL) or the dispatch
/// table (slash/explicit/raw tool name). Returns a surface-agnostic
/// outcome; the caller prints `rendered` / `tool_result.body` using its
/// own writer conventions (bare stdout for REPL, CRLF-translated for TUI).
pub fn processSubmit(
    allocator: std.mem.Allocator,
    session: *agent.AgentSession,
    registry: *const Registry,
    raw_line: []const u8,
    approval_fn: ?loop.ApprovalFn,
) !SubmitOutcome {
    const trimmed = std.mem.trim(u8, raw_line, " \t\r\n");
    if (trimmed.len == 0) return .noop;

    if (std.mem.startsWith(u8, trimmed, "/skill:")) {
        const skill_name = trimmed["/skill:".len..];
        if (skills_catalog.findByName(skill_name)) |skill| {
            const rendered = try agent.runOneTurn(allocator, session, registry, skill.body, approval_fn);
            return .{ .rendered = rendered };
        }
        const msg = try std.fmt.allocPrint(allocator, "unknown skill: {s}\nAvailable: run /skills to list\n", .{skill_name});
        return .{ .tool_result = .{ .ok = false, .body = msg } };
    }

    if (std.mem.eql(u8, trimmed, "/model")) {
        return .{ .tool_result = try renderModel(allocator, session.currentModel()) };
    }

    if (std.mem.eql(u8, trimmed, "/status")) {
        return .{ .tool_result = try renderStatus(allocator, session) };
    }

    if (std.mem.startsWith(u8, trimmed, "/model ")) {
        const model_id = std.mem.trim(u8, trimmed["/model ".len..], " \t");
        if (models_registry.findById(model_id)) |model| {
            session.setModel(model.id);
            const msg = try std.fmt.allocPrint(allocator, "Model switched to: {s}\n", .{model.display_name});
            return .{ .tool_result = .{ .ok = true, .body = msg } };
        }
        const msg = try std.fmt.allocPrint(allocator, "Unknown model: {s}\n", .{model_id});
        return .{ .tool_result = .{ .ok = false, .body = msg } };
    }

    if (std.mem.startsWith(u8, trimmed, "/settings theme")) {
        const rest_raw = trimmed["/settings theme".len..];
        const rest = std.mem.trim(u8, rest_raw, " \t");
        if (rest.len == 0) {
            return .{ .tool_result = try renderThemes(allocator, session.theme) };
        }
        if (theme_mod.findByName(rest)) |t| {
            session.theme = t;
            const msg = try std.fmt.allocPrint(allocator, "Theme switched to: {s}\n", .{t.display_name});
            return .{ .tool_result = .{ .ok = true, .body = msg } };
        }
        const msg = try std.fmt.allocPrint(allocator, "Unknown theme: {s}\nAvailable: run /settings theme to list\n", .{rest});
        return .{ .tool_result = .{ .ok = false, .body = msg } };
    }

    if (std.mem.startsWith(u8, trimmed, "/template:")) {
        const rest = trimmed["/template:".len..];
        var it = std.mem.tokenizeAny(u8, rest, " \t");
        const tmpl_name = it.next() orelse "";
        var tmpl_args: std.ArrayList([]const u8) = .empty;
        defer tmpl_args.deinit(allocator);
        while (it.next()) |arg| try tmpl_args.append(allocator, arg);

        if (prompts_catalog.findByName(tmpl_name)) |tmpl| {
            const expanded = try prompts_catalog.expand(allocator, tmpl.body, tmpl_args.items);
            defer allocator.free(expanded);
            const rendered = try agent.runOneTurn(allocator, session, registry, expanded, approval_fn);
            return .{ .rendered = rendered };
        }
        const msg = try std.fmt.allocPrint(allocator, "unknown template: {s}\nAvailable: run /templates to list\n", .{tmpl_name});
        return .{ .tool_result = .{ .ok = false, .body = msg } };
    }

    if (std.mem.eql(u8, trimmed, "/tree")) {
        return .{ .tool_result = try renderTree(allocator, if (session.session_id) |sid| sid else null) };
    }

    if (!shouldDispatchTool(registry, trimmed)) {
        const rendered = try agent.runOneTurn(allocator, session, registry, trimmed, approval_fn);
        return .{ .rendered = rendered };
    }

    const outcome = try dispatchLine(allocator, registry, raw_line);
    return switch (outcome) {
        .noop => .noop,
        .quit => .quit,
        .result => |r| .{ .tool_result = r },
        .session_resume => .session_resume,
        .session_new => .session_new,
        .session_compact => .session_compact,
        .session_fork => .session_fork,
    };
}

pub fn dispatchLine(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    line: []const u8,
) !DispatchOutcome {
    const trimmed = std.mem.trim(u8, line, " \t\r\n");
    if (trimmed.len == 0) return .noop;

    var tokens = try tokenizeLine(allocator, trimmed);
    defer tokens.deinit(allocator);
    const argv = tokens.items;
    if (argv.len == 0) return .noop;

    if (commands.isQuit(argv[0])) return .quit;
    if (commands.isHelp(argv[0])) return .{ .result = try renderHelp(allocator, registry) };
    if (commands.isSessionResume(argv[0])) return .session_resume;
    if (commands.isSessionNew(argv[0])) return .session_new;
    if (commands.isCompact(argv[0])) return .session_compact;
    if (commands.isSessionFork(argv[0])) return .session_fork;
    if (commands.isSessionTree(argv[0])) return .{ .result = try renderTree(allocator, null) };
    if (commands.isSettings(argv[0])) return .{ .result = try renderSettings(allocator) };
    if (commands.isHotkeys(argv[0])) return .{ .result = try renderHotkeys(allocator) };
    if (commands.isChangelog(argv[0])) return .{ .result = try renderChangelog(allocator) };
    if (std.mem.eql(u8, argv[0], "/skills")) return .{ .result = try renderSkills(allocator) };
    if (std.mem.eql(u8, argv[0], "/templates")) return .{ .result = try renderTemplates(allocator) };

    if (commands.lookup(argv)) |cmd| {
        return .{ .result = try invokeTool(allocator, registry, cmd.tool_name, cmd.args) };
    }

    if (registry.findByName(argv[0])) |tool| {
        _ = tool;
        return .{ .result = try invokeTool(allocator, registry, argv[0], argv[1..]) };
    }

    const msg = try std.fmt.allocPrint(allocator, "unknown tool or command: {s}\n", .{argv[0]});
    return .{ .result = .{ .ok = false, .body = msg } };
}

pub fn shouldDispatchTool(registry: *const Registry, line: []const u8) bool {
    const trimmed = std.mem.trim(u8, line, " \t\r\n");
    if (trimmed.len == 0) return true;

    var first_token_iter = std.mem.tokenizeAny(u8, trimmed, " \t\r\n");
    const first = first_token_iter.next() orelse return true;
    if (commands.isQuit(first) or commands.isHelp(first)) return true;
    if (commands.isSessionResume(first) or commands.isSessionNew(first)) return true;
    if (first.len > 0 and first[0] == '/') return true;
    if (std.mem.eql(u8, first, "zigts") or std.mem.eql(u8, first, "zig")) return true;
    return registry.findByName(first) != null;
}

fn invokeTool(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    name: []const u8,
    args: []const []const u8,
) !ToolResult {
    return registry.invoke(allocator, name, args) catch |err| switch (err) {
        registry_mod.RegistryError.ToolNotFound => registry_mod.ToolResult.errFmt(
            allocator,
            "unknown tool: {s}\n",
            .{name},
        ),
        else => registry_mod.ToolResult.errFmt(
            allocator,
            "tool {s} failed: {s}\n",
            .{ name, @errorName(err) },
        ),
    };
}

fn tokenizeLine(
    allocator: std.mem.Allocator,
    line: []const u8,
) !std.ArrayList([]const u8) {
    var tokens = std.ArrayList([]const u8).empty;
    errdefer tokens.deinit(allocator);
    var it = std.mem.tokenizeAny(u8, line, " \t\r\n");
    while (it.next()) |token| {
        try tokens.append(allocator, token);
    }
    return tokens;
}

fn renderHelp(allocator: std.mem.Allocator, registry: *const Registry) !ToolResult {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll("Natural language is sent to the expert backend by default.\n");
    try w.writeAll("Explicit local commands:\n");
    for (commands.command_table) |row| {
        if (row.slash) |slash| {
            if (row.takes_trailing_args) {
                try w.print("  {s} <args...>\n", .{slash});
            } else {
                try w.print("  {s}\n", .{slash});
            }
        }
    }
    for (commands.session_commands) |slash| {
        try w.print("  {s}\n", .{slash});
    }
    try w.writeAll("  zigts");
    for (commands.command_table) |row| {
        if (row.explicit) |name| {
            try w.print(" {s}", .{name});
        }
    }
    try w.writeAll(" ...\n");
    try w.writeAll("  zig build <step>  zig build test[-...]\n\n");
    try w.writeAll("Approval flags (pass on launch):\n");
    try w.writeAll("  --yes      auto-approve every verified edit\n");
    try w.writeAll("  --no-edit  auto-reject every verified edit\n");
    try w.writeAll("Session flags (pass on launch):\n");
    try w.writeAll("  --session-id <id>          resume or create a named session\n");
    try w.writeAll("  --resume / --continue      resume the newest session for this cwd\n");
    try w.writeAll("  --fork <session-id>        branch from an existing session\n");
    try w.writeAll("  --no-session               disable session persistence for this run\n");
    try w.writeAll("  --no-persist-tool-output   omit tool output bodies from persisted session\n");
    try w.writeAll("Non-interactive flags (pass on launch):\n");
    try w.writeAll("  --print <prompt>           run a single turn and exit\n");
    try w.writeAll("  --mode json                with --print, emit NDJSON events to stdout\n\n");
    try w.writeAll("Registered tools:\n");
    for (registry.list()) |entry| {
        try w.print("  {s: <36}  {s}\n", .{ entry.name, entry.description });
    }
    try w.writeAll("\nMeta commands: help (:h), quit (:q)\n");
    try w.writeAll("Info commands: /model  /status  /settings  /hotkeys  /changelog\n");
    try w.writeAll("Session:       /compact  /resume  /continue  /new  /fork  /tree\n");
    try w.writeAll("Skills:        /skills  /skill:<name>\n");
    try w.writeAll("Templates:     /templates  /template:<name> [args...]\n");

    buf = aw.toArrayList();
    return .{ .ok = true, .body = try buf.toOwnedSlice(allocator) };
}

const request_mod = @import("providers/anthropic/request.zig");
const session_paths = @import("session/paths.zig");

fn renderModel(allocator: std.mem.Allocator, active: ?[]const u8) !ToolResult {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;
    const current = active orelse request_mod.default_model;
    try w.print("Active model: {s}\n\nAvailable models:\n", .{current});
    for (models_registry.registry) |m| {
        try w.print("  {s: <36}  {s}\n", .{ m.id, m.display_name });
    }
    try w.writeAll("\nSwitch with: /model <model-id>\n");
    buf = aw.toArrayList();
    return .{ .ok = true, .body = try buf.toOwnedSlice(allocator) };
}

fn renderSettings(allocator: std.mem.Allocator) !ToolResult {
    const msg = try std.fmt.allocPrint(
        allocator,
        "Settings (compile-time defaults):\n" ++
            "  model:           {s}\n" ++
            "  max_tokens:      {d}\n" ++
            "  max_attempts:    3\n" ++
            "  roundtrips/turn: 8\n" ++
            "  tool_calls/turn: 16\n" ++
            "  batch_size:      8\n" ++
            "Theme: run /settings theme to list or switch.\n",
        .{ request_mod.default_model, request_mod.default_max_tokens },
    );
    return .{ .ok = true, .body = msg };
}

fn renderStatus(allocator: std.mem.Allocator, session: *const agent.AgentSession) !ToolResult {
    const session_id = if (session.session_id) |sid| sid else "ephemeral";
    const model = session.currentModel() orelse "stub";
    const tok = session.token_totals;
    const descriptor = session.backendDescriptor();
    const msg = try std.fmt.allocPrint(
        allocator,
        "Expert status:\n" ++
            "  provider:      {s}\n" ++
            "  auth:          {s}\n" ++
            "  model:         {s}\n" ++
            "  session:       {s}\n" ++
            "  persistence:   {s}\n" ++
            "  tokens.in:     {d}\n" ++
            "  tokens.cache_r:{d}\n" ++
            "  tokens.cache_w:{d}\n" ++
            "  tokens.out:    {d}\n",
        .{
            descriptor.provider_label,
            descriptor.auth_label,
            model,
            session_id,
            if (session.session_dir != null) "enabled" else "disabled",
            tok.input_tokens,
            tok.cache_read_input_tokens,
            tok.cache_creation_input_tokens,
            tok.output_tokens,
        },
    );
    return .{ .ok = true, .body = msg };
}

fn renderThemes(allocator: std.mem.Allocator, current: *const theme_mod.Theme) !ToolResult {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;
    try w.writeAll("Available themes (invoke with /settings theme <name>):\n");
    for (theme_mod.registry) |t| {
        const marker = if (t == current) "*" else " ";
        try w.print("  {s} {s: <16}  {s}\n", .{ marker, t.name, t.display_name });
    }
    buf = aw.toArrayList();
    return .{ .ok = true, .body = try buf.toOwnedSlice(allocator) };
}

fn renderHotkeys(allocator: std.mem.Allocator) !ToolResult {
    const msg = try allocator.dupe(
        u8,
        "Keyboard shortcuts (TUI mode):\n" ++
            "  Enter          submit current line\n" ++
            "  Ctrl-C         interrupt / cancel\n" ++
            "  Ctrl-D         quit (EOF)\n" ++
            "  Left/Right     move cursor\n" ++
            "  Home/End       jump to start/end of line\n" ++
            "  Delete         delete under cursor\n" ++
            "  Up/Down        history navigation\n",
    );
    return .{ .ok = true, .body = msg };
}

fn renderSkills(allocator: std.mem.Allocator) !ToolResult {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;
    try w.writeAll("Available skills (invoke with /skill:<name>):\n");
    for (skills_catalog.catalog) |skill| {
        try w.print("  {s: <20}  {s}\n", .{ skill.name, skill.description });
    }
    buf = aw.toArrayList();
    return .{ .ok = true, .body = try buf.toOwnedSlice(allocator) };
}

fn renderTemplates(allocator: std.mem.Allocator) !ToolResult {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;
    try w.writeAll("Available templates (invoke with /template:<name> [args...]):\n");
    for (prompts_catalog.catalog) |tmpl| {
        try w.print("  {s: <16}  {s}\n", .{ tmpl.name, tmpl.description });
    }
    buf = aw.toArrayList();
    return .{ .ok = true, .body = try buf.toOwnedSlice(allocator) };
}

fn renderChangelog(allocator: std.mem.Allocator) !ToolResult {
    const msg = try allocator.dupe(
        u8,
        "zigts expert - changelog\n" ++
            "  Full token accounting (input, cache_read, cache_write, output)\n" ++
            "  Post-apply auto-verify (verify_paths + diff review after each edit)\n" ++
            "  Session compaction (/compact)\n" ++
            "  Session branching: /fork, /tree, --fork, --continue\n" ++
            "  Session commands: /resume, /continue, /new, /compact, /fork, /tree\n" ++
            "  Skills catalog (/skill:<name>)\n" ++
            "  Informational commands: /model, /status, /settings, /hotkeys, /changelog\n",
    );
    return .{ .ok = true, .body = msg };
}

fn renderTree(allocator: std.mem.Allocator, current_session_id: ?[]const u8) !ToolResult {
    const root = session_paths.sessionRoot(allocator) catch |err| {
        const msg = try std.fmt.allocPrint(allocator, "error reading session root: {s}\n", .{@errorName(err)});
        return .{ .ok = false, .body = msg };
    };
    defer allocator.free(root);

    const hash = session_paths.cwdHashFull(allocator) catch |err| {
        const msg = try std.fmt.allocPrint(allocator, "error computing cwd hash: {s}\n", .{@errorName(err)});
        return .{ .ok = false, .body = msg };
    };

    const entries = session_paths.listSessions(allocator, root, hash[0..]) catch |err| {
        const msg = try std.fmt.allocPrint(allocator, "error listing sessions: {s}\n", .{@errorName(err)});
        return .{ .ok = false, .body = msg };
    };
    defer {
        for (entries) |*e| e.deinit(allocator);
        allocator.free(entries);
    }

    if (entries.len == 0) {
        return .{ .ok = true, .body = try allocator.dupe(u8, "No sessions found for this workspace.\n") };
    }

    const nodes = try buildSessionTreeNodes(allocator, entries, current_session_id);
    defer freeSessionTreeNodes(allocator, nodes);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;
    try w.print("Sessions for this workspace ({d} total):\n", .{entries.len});
    try writeSessionTree(w, nodes);
    try w.writeAll("\nUse /fork to branch the current session.\n");
    try w.writeAll("Use /resume or /continue to reload the newest session.\n");
    buf = aw.toArrayList();
    return try ToolResult.withSessionTree(allocator, true, buf.items, nodes);
}

fn buildSessionTreeNodes(
    allocator: std.mem.Allocator,
    entries: []const session_paths.SessionEntry,
    current_session_id: ?[]const u8,
) ![]SessionTreeNode {
    var by_id: std.StringHashMapUnmanaged(usize) = .empty;
    defer by_id.deinit(allocator);
    try by_id.ensureUnusedCapacity(allocator, @intCast(entries.len));
    for (entries, 0..) |entry, i| {
        by_id.putAssumeCapacity(entry.session_id, i);
    }

    const child_lists = try allocator.alloc(std.ArrayListUnmanaged(usize), entries.len);
    defer {
        for (child_lists) |*children| children.deinit(allocator);
        allocator.free(child_lists);
    }
    for (child_lists) |*children| children.* = .empty;

    const orphan_roots = try allocator.alloc(bool, entries.len);
    defer allocator.free(orphan_roots);
    @memset(orphan_roots, false);

    var roots: std.ArrayList(usize) = .empty;
    defer roots.deinit(allocator);

    for (entries, 0..) |entry, i| {
        if (entry.parent_id) |parent_id| {
            if (by_id.get(parent_id)) |parent_index| {
                try child_lists[parent_index].append(allocator, i);
            } else {
                orphan_roots[i] = true;
                try roots.append(allocator, i);
            }
        } else {
            try roots.append(allocator, i);
        }
    }

    var flat: std.ArrayList(SessionTreeNode) = .empty;
    defer flat.deinit(allocator);

    for (roots.items) |root_index| {
        try appendSessionTreePreorder(
            allocator,
            &flat,
            entries,
            child_lists,
            orphan_roots,
            root_index,
            0,
            current_session_id,
        );
    }

    return try flat.toOwnedSlice(allocator);
}

fn appendSessionTreePreorder(
    allocator: std.mem.Allocator,
    flat: *std.ArrayList(SessionTreeNode),
    entries: []const session_paths.SessionEntry,
    child_lists: []const std.ArrayListUnmanaged(usize),
    orphan_roots: []const bool,
    index: usize,
    depth: usize,
    current_session_id: ?[]const u8,
) !void {
    const entry = entries[index];
    try flat.append(allocator, .{
        .session_id = try allocator.dupe(u8, entry.session_id),
        .parent_id = if (entry.parent_id) |parent_id|
            try allocator.dupe(u8, parent_id)
        else
            null,
        .created_at_unix_ms = entry.created_at_unix_ms,
        .depth = depth,
        .is_current = if (current_session_id) |sid| std.mem.eql(u8, sid, entry.session_id) else false,
        .is_orphan_root = orphan_roots[index],
    });

    for (child_lists[index].items) |child_index| {
        try appendSessionTreePreorder(
            allocator,
            flat,
            entries,
            child_lists,
            orphan_roots,
            child_index,
            depth + 1,
            current_session_id,
        );
    }
}

fn freeSessionTreeNodes(allocator: std.mem.Allocator, nodes: []SessionTreeNode) void {
    for (nodes) |*node| node.deinit(allocator);
    allocator.free(nodes);
}

fn writeSessionTree(writer: *std.Io.Writer, nodes: []const SessionTreeNode) !void {
    for (nodes) |node| {
        var depth: usize = 0;
        while (depth < node.depth) : (depth += 1) {
            try writer.writeAll("  ");
        }
        try writer.writeAll(if (node.is_current) "* " else "- ");
        try writer.writeAll(node.session_id);
        try writer.print("  created={d}", .{@divTrunc(node.created_at_unix_ms, 1000)});
        if (node.is_orphan_root and node.parent_id != null) {
            try writer.writeAll("  orphaned-from=");
            try writer.writeAll(node.parent_id.?);
        }
        try writer.writeByte('\n');
    }
}

pub fn baseSessionConfig(flags: ExpertFlags, overrides: agent.SessionConfig) agent.SessionConfig {
    var cfg = overrides;
    cfg.no_session = flags.no_session;
    cfg.no_persist_tool_output = flags.no_persist_tool_output;
    cfg.no_context_files = flags.no_context_files;
    return cfg;
}

pub fn run(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    flags: ExpertFlags,
    policy: loop.ApprovalPolicy,
) !void {
    const approval_fn = selectApprovalFn(policy);
    const is_tty = std.c.isatty(std.c.STDIN_FILENO) != 0;
    if (is_tty) {
        const banner = "zigts expert — NL by default, 'help' for commands, or 'quit'\n";
        _ = std.c.write(std.c.STDOUT_FILENO, banner.ptr, banner.len);
    }

    var session = try agent.initFromEnvWithSessionConfig(allocator, registry, .{
        .no_session = flags.no_session,
        .no_persist_tool_output = flags.no_persist_tool_output,
        .no_context_files = flags.no_context_files,
        .session_id = flags.session_id,
        .resume_latest = flags.resume_latest,
        .fork_session_id = flags.fork_session_id,
    });
    defer session.deinit(allocator);

    var line_buf: [64 * 1024]u8 = undefined;
    while (true) {
        if (is_tty) {
            const prompt = "expert> ";
            _ = std.c.write(std.c.STDOUT_FILENO, prompt.ptr, prompt.len);
        }

        const maybe_line = try readLine(&line_buf);
        const line = maybe_line orelse break;

        var outcome = processSubmit(allocator, &session, registry, line, approval_fn) catch |err| {
            var msg_buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&msg_buf, "error: {s}\n", .{@errorName(err)}) catch "error\n";
            _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
            continue;
        };

        switch (outcome) {
            .noop => {},
            .quit => break,
            .rendered => |rendered| {
                defer allocator.free(rendered);
                _ = std.c.write(std.c.STDOUT_FILENO, rendered.ptr, rendered.len);
                if (is_tty) {
                    const tok = session.token_totals;
                    var token_buf: [128]u8 = undefined;
                    const token_line = std.fmt.bufPrint(
                        &token_buf,
                        "[tokens: in={d} cache_r={d} cache_w={d} out={d}]\n",
                        .{
                            tok.input_tokens,
                            tok.cache_read_input_tokens,
                            tok.cache_creation_input_tokens,
                            tok.output_tokens,
                        },
                    ) catch "";
                    _ = std.c.write(std.c.STDOUT_FILENO, token_line.ptr, token_line.len);
                }
            },
            .tool_result => |*result| {
                defer result.deinit(allocator);
                if (result.body.len > 0) {
                    _ = std.c.write(std.c.STDOUT_FILENO, result.body.ptr, result.body.len);
                    if (result.body[result.body.len - 1] != '\n') {
                        _ = std.c.write(std.c.STDOUT_FILENO, "\n", 1);
                    }
                }
            },
            .session_resume => try agent.rebuildSession(allocator, &session, registry, baseSessionConfig(flags, .{ .resume_latest = true })),
            .session_new => try agent.rebuildSession(allocator, &session, registry, baseSessionConfig(flags, .{})),
            .session_compact => {
                const msg = try agent.compact(allocator, &session);
                defer allocator.free(msg);
                _ = std.c.write(std.c.STDOUT_FILENO, msg.ptr, msg.len);
            },
            .session_fork => {
                const msg = try agent.fork(allocator, &session);
                defer allocator.free(msg);
                _ = std.c.write(std.c.STDOUT_FILENO, msg.ptr, msg.len);
            },
        }
    }
}

fn approveEdit(file: []const u8) !bool {
    var prompt_buf: [512]u8 = undefined;
    const prompt = std.fmt.bufPrint(&prompt_buf, "Apply verified edit to {s}? [y/N] ", .{file}) catch "Apply verified edit? [y/N] ";
    _ = std.c.write(std.c.STDOUT_FILENO, prompt.ptr, prompt.len);

    var line_buf: [256]u8 = undefined;
    const maybe_line = try readLine(&line_buf);
    const line = maybe_line orelse return false;
    const trimmed = std.mem.trim(u8, line, " \t\r\n");
    if (trimmed.len == 0) return false;
    return trimmed[0] == 'y' or trimmed[0] == 'Y';
}

fn readLine(buf: []u8) !?[]const u8 {
    var len: usize = 0;
    while (len < buf.len) {
        var byte: [1]u8 = undefined;
        const n = std.posix.read(std.posix.STDIN_FILENO, &byte) catch |err| switch (err) {
            error.WouldBlock => continue,
            else => return err,
        };
        if (n == 0) {
            if (len == 0) return null;
            return buf[0..len];
        }
        if (byte[0] == '\n') return buf[0..len];
        buf[len] = byte[0];
        len += 1;
    }
    return buf[0..len];
}

fn selectApprovalFn(policy: loop.ApprovalPolicy) loop.ApprovalFn {
    return loop.resolveApprovalFn(policy, approveEdit);
}

const testing = std.testing;
const meta_tool_mod = @import("tools/zigts_expert_meta.zig");
const check_tool_mod = @import("tools/zigts_check.zig");
const test_tool_mod = @import("tools/zig_test_step.zig");

fn buildMiniRegistry(allocator: std.mem.Allocator) !Registry {
    var reg: Registry = .{};
    errdefer reg.deinit(allocator);
    try reg.register(allocator, meta_tool_mod.tool);
    try reg.register(allocator, check_tool_mod.tool);
    try reg.register(allocator, test_tool_mod.tool);
    return reg;
}

fn expectResult(outcome: *DispatchOutcome, allocator: std.mem.Allocator, needle: []const u8, want_ok: bool) !void {
    switch (outcome.*) {
        .result => |*r| {
            defer r.deinit(allocator);
            try testing.expectEqual(want_ok, r.ok);
            try testing.expect(std.mem.indexOf(u8, r.body, needle) != null);
        },
        else => return error.TestFailed,
    }
}

test "help renders local command guidance" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var outcome = try dispatchLine(testing.allocator, &reg, "help");
    switch (outcome) {
        .result => |*r| {
            defer r.deinit(testing.allocator);
            try testing.expect(r.ok);
            try testing.expect(std.mem.indexOf(u8, r.body, "/verify") != null);
            try testing.expect(std.mem.indexOf(u8, r.body, "/status") != null);
            try testing.expect(std.mem.indexOf(u8, r.body, commands.session_commands[0]) != null);
            try testing.expect(std.mem.indexOf(u8, r.body, commands.session_commands[1]) != null);
        },
        else => return error.TestFailed,
    }
}

test "slash command routes locally" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var outcome = try dispatchLine(testing.allocator, &reg, "/meta");
    try expectResult(&outcome, testing.allocator, "\"compiler_version\"", true);
}

test "explicit zigts command routes locally" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var outcome = try dispatchLine(testing.allocator, &reg, "zigts meta");
    try expectResult(&outcome, testing.allocator, "\"policy_version\"", true);
}

test "raw tool name still dispatches directly" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var outcome = try dispatchLine(testing.allocator, &reg, "zigts_expert_meta");
    try expectResult(&outcome, testing.allocator, "\"compiler_version\"", true);
}

test "shouldDispatchTool is false for plain natural language" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    try testing.expect(!shouldDispatchTool(&reg, "add a GET route and then run tests"));
    try testing.expect(shouldDispatchTool(&reg, "/test"));
    try testing.expect(shouldDispatchTool(&reg, "zig build test-zigts"));
}

test "selectApprovalFn: auto_approve resolves to loop.autoApprove" {
    try testing.expect(selectApprovalFn(.auto_approve) == loop.autoApprove);
}

test "selectApprovalFn: auto_reject resolves to loop.autoReject" {
    try testing.expect(selectApprovalFn(.auto_reject) == loop.autoReject);
}

test "selectApprovalFn: ask resolves to the interactive approveEdit" {
    try testing.expect(selectApprovalFn(.ask) == approveEdit);
}

test "processSubmit: /settings theme lists themes and marks the current one" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);

    var outcome = try processSubmit(testing.allocator, &session, &reg, "/settings theme", null);
    switch (outcome) {
        .tool_result => |*r| {
            defer r.deinit(testing.allocator);
            try testing.expect(r.ok);
            try testing.expect(std.mem.indexOf(u8, r.body, "default") != null);
            try testing.expect(std.mem.indexOf(u8, r.body, "solarized-dark") != null);
            // The current theme gets a '*' marker.
            try testing.expect(std.mem.indexOf(u8, r.body, "* default") != null);
        },
        else => return error.TestFailed,
    }
}

test "processSubmit: /settings theme <name> swaps the session theme" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);

    try testing.expect(session.theme == &theme_mod.default);

    var outcome = try processSubmit(testing.allocator, &session, &reg, "/settings theme solarized-dark", null);
    switch (outcome) {
        .tool_result => |*r| {
            defer r.deinit(testing.allocator);
            try testing.expect(r.ok);
            try testing.expect(std.mem.indexOf(u8, r.body, "Solarized Dark") != null);
        },
        else => return error.TestFailed,
    }
    try testing.expect(session.theme == &theme_mod.solarized_dark);
}

test "processSubmit: /settings theme <unknown> returns an error result" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);

    var outcome = try processSubmit(testing.allocator, &session, &reg, "/settings theme neon-pink", null);
    switch (outcome) {
        .tool_result => |*r| {
            defer r.deinit(testing.allocator);
            try testing.expect(!r.ok);
            try testing.expect(std.mem.indexOf(u8, r.body, "Unknown theme") != null);
        },
        else => return error.TestFailed,
    }
    // Theme should not have changed.
    try testing.expect(session.theme == &theme_mod.default);
}

test "processSubmit: /status reports provider auth and persistence state" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);

    var outcome = try processSubmit(testing.allocator, &session, &reg, "/status", null);
    switch (outcome) {
        .tool_result => |*r| {
            defer r.deinit(testing.allocator);
            try testing.expect(r.ok);
            try testing.expect(std.mem.indexOf(u8, r.body, "provider:      stub") != null);
            try testing.expect(std.mem.indexOf(u8, r.body, "auth:          stub") != null);
            try testing.expect(std.mem.indexOf(u8, r.body, "persistence:   disabled") != null);
        },
        else => return error.TestFailed,
    }
}

test "renderTree builds nested payload nodes from parent ids" {
    const allocator = testing.allocator;
    var entries = [_]session_paths.SessionEntry{
        .{
            .session_id = try allocator.dupe(u8, "child"),
            .dir_path = try allocator.dupe(u8, "/tmp/child"),
            .created_at_unix_ms = 300,
            .parent_id = try allocator.dupe(u8, "root"),
        },
        .{
            .session_id = try allocator.dupe(u8, "root"),
            .dir_path = try allocator.dupe(u8, "/tmp/root"),
            .created_at_unix_ms = 200,
            .parent_id = null,
        },
        .{
            .session_id = try allocator.dupe(u8, "orphan"),
            .dir_path = try allocator.dupe(u8, "/tmp/orphan"),
            .created_at_unix_ms = 100,
            .parent_id = try allocator.dupe(u8, "missing"),
        },
    };
    defer {
        for (&entries) |*entry| entry.deinit(allocator);
    }

    const nodes = try buildSessionTreeNodes(allocator, &entries, "child");
    defer freeSessionTreeNodes(allocator, nodes);

    try testing.expectEqual(@as(usize, 3), nodes.len);
    try testing.expectEqualStrings("root", nodes[0].session_id);
    try testing.expectEqual(@as(usize, 0), nodes[0].depth);
    try testing.expectEqualStrings("child", nodes[1].session_id);
    try testing.expectEqual(@as(usize, 1), nodes[1].depth);
    try testing.expect(nodes[1].is_current);
    try testing.expectEqualStrings("orphan", nodes[2].session_id);
    try testing.expect(nodes[2].is_orphan_root);
}
