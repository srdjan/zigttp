//! Line-buffered expert REPL driver.
//!
//! Natural language goes to the model by default. Deterministic slash commands,
//! safe raw `zigts ...` / `zig build ...` commands, and direct tool names are
//! routed locally.

const std = @import("std");
const zigts = @import("zigts");
const registry_mod = @import("registry/registry.zig");
const agent = @import("agent.zig");
const commands = @import("commands.zig");
const loop = @import("loop.zig");
const app = @import("app.zig");
const ledger = @import("ledger.zig");
const skills_catalog = @import("skills/catalog.zig");
const prompts_catalog = @import("prompts/catalog.zig");
const models_registry = @import("providers/models.zig");

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

/// Surface-agnostic result of processing one submitted line.
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
/// outcome; the caller prints `rendered` / `tool_result.llm_text`.
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
        return .{ .tool_result = .{ .ok = false, .llm_text = msg } };
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
            return .{ .tool_result = .{ .ok = true, .llm_text = msg } };
        }
        const msg = try std.fmt.allocPrint(allocator, "Unknown model: {s}\n", .{model_id});
        return .{ .tool_result = .{ .ok = false, .llm_text = msg } };
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
        return .{ .tool_result = .{ .ok = false, .llm_text = msg } };
    }

    if (std.mem.eql(u8, trimmed, "/tree")) {
        return .{ .tool_result = try renderTree(allocator, if (session.session_id) |sid| sid else null) };
    }

    if (commands.isViewLedger(trimmed)) {
        return .{ .tool_result = try renderLedgerGuidance(allocator) };
    }

    if (commands.isViewChat(trimmed)) {
        return .{ .tool_result = try renderChatGuidance(allocator) };
    }

    if (std.mem.startsWith(u8, trimmed, "/ledger export ")) {
        const out_path = std.mem.trim(u8, trimmed["/ledger export ".len..], " \t");
        if (out_path.len == 0) {
            return .{ .tool_result = .{ .ok = false, .llm_text = try allocator.dupe(u8, "usage: /ledger export <path>\n") } };
        }
        const sid = session.session_id orelse {
            return .{ .tool_result = .{ .ok = false, .llm_text = try allocator.dupe(u8, "ledger export requires a persisted session\n") } };
        };
        try ledger.exportSessionLedger(allocator, sid, out_path);
        const msg = try std.fmt.allocPrint(allocator, "ledger exported to {s}\n", .{out_path});
        return .{ .tool_result = .{ .ok = true, .llm_text = msg } };
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
    if (commands.isHelp(argv[0])) {
        const show_tools = argv.len > 1 and
            (std.mem.eql(u8, argv[1], "--tools") or std.mem.eql(u8, argv[1], "tools"));
        return .{ .result = try renderHelp(allocator, registry, show_tools) };
    }
    if (commands.isSessionResume(argv[0])) return .session_resume;
    if (commands.isSessionNew(argv[0])) return .session_new;
    if (commands.isCompact(argv[0])) return .session_compact;
    if (commands.isSessionFork(argv[0])) return .session_fork;
    if (commands.isSessionTree(argv[0])) return .{ .result = try renderTree(allocator, null) };
    if (commands.isViewLedger(argv[0])) return .{ .result = try renderLedgerGuidance(allocator) };
    if (commands.isViewChat(argv[0])) return .{ .result = try renderChatGuidance(allocator) };
    if (commands.isSettings(argv[0])) return .{ .result = try renderSettings(allocator) };
    if (commands.isHotkeys(argv[0])) return .{ .result = try renderHotkeys(allocator) };
    if (commands.isChangelog(argv[0])) return .{ .result = try renderChangelog(allocator) };
    if (commands.isStudio(argv[0])) {
        const handler = if (argv.len > 1) argv[1] else "";
        return .{ .result = try renderStudio(allocator, handler) };
    }
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
    return .{ .result = .{ .ok = false, .llm_text = msg } };
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

fn renderHelp(allocator: std.mem.Allocator, registry: *const Registry, show_tools: bool) !ToolResult {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    // Lead with how to drive the loop, not with a wall of internal tool names.
    // A new user who types `help` needs the two things they actually do: state a
    // goal in plain English, and approve the verified edit.
    try w.writeAll(
        \\How to use zigttp expert:
        \\  1. Type what you want in plain English, e.g. "add a GET /health route to handler.ts".
        \\  2. The expert drafts an edit; the analyzer verifies it and rejects any draft that fails.
        \\  3. You review the change and approve with y/N before anything is written to disk.
        \\
        \\
    );

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
    for (commands.view_commands) |slash| {
        try w.print("  {s}\n", .{slash});
    }
    for (commands.ledger_commands) |slash| {
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
    try w.writeAll("  --mode json                with --print, emit NDJSON events to stdout\n");

    // The ~32 registered tool names are agent-facing dispatch names a user
    // almost never types (NL is the default), so keep them behind `help --tools`
    // instead of burying the how-to above under them.
    if (show_tools) {
        try w.writeAll("\nRegistered tools (you rarely call these directly):\n");
        for (registry.list()) |entry| {
            try w.print("  {s: <36}  {s}\n", .{ entry.name, entry.description });
        }
    } else {
        try w.print(
            "\n{d} analyzer and agent tools back the expert; you rarely call them directly. Run `help --tools` to list them.\n",
            .{registry.list().len},
        );
    }

    try w.writeAll("\nMeta commands: help (:h), quit (:q)\n");
    try w.writeAll("Info commands: /model  /status  /settings  /hotkeys  /changelog\n");
    try w.writeAll("Session:       /compact  /resume  /continue  /new  /fork  /tree\n");
    try w.writeAll("Views:         /ledger  /chat  /ledger export <path>\n");
    try w.writeAll("Studio:        /studio <handler.ts>   show browser proof workbench command\n");
    try w.writeAll("Route Forge:   /feature route file=<handler.ts> method=<VERB> path=</path>\n");
    try w.writeAll("               /forge route file=<handler.ts> method=<VERB> path=</path>\n");
    try w.writeAll("Spec Forge:    /forge spec file=<handler.ts> specs=<spec[,spec...]>\n");
    try w.writeAll("Specs:         /specs <handler.ts>   show declared Spec<...> obligations\n");
    try w.writeAll("Skills:        /skills  /skill:<name>\n");
    try w.writeAll("Templates:     /templates  /template:<name> [args...]\n");

    buf = aw.toArrayList();
    return ToolResult.withPlainText(allocator, true, buf.items);
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
    return ToolResult.withPlainText(allocator, true, buf.items);
}

fn renderStudio(allocator: std.mem.Allocator, handler_path: []const u8) !ToolResult {
    const path = if (handler_path.len == 0) "<handler.ts>" else handler_path;
    const msg = try std.fmt.allocPrint(
        allocator,
        "Browser proof workbench:\n  zigttp studio {s}\n\nOpen the studio page on the running server (default http://localhost:8080/_zigttp/studio).\n\nStudio runs the handler with --watch --prove, shows release readiness, declared specs, witnesses, generated tests, and next actions.\n",
        .{path},
    );
    defer allocator.free(msg);
    return ToolResult.withPlainText(allocator, handler_path.len != 0, msg);
}

fn renderSettings(allocator: std.mem.Allocator) !ToolResult {
    // Source every figure from the actual defaults so this display cannot drift.
    const defaults = loop.RunOptions{};
    const msg = try std.fmt.allocPrint(
        allocator,
        "Settings (compile-time defaults):\n" ++
            "  model:           {s}\n" ++
            "  max_tokens:      {d}\n" ++
            "  max_attempts:    {d}\n" ++
            "  roundtrips/turn: {d}\n" ++
            "  tool_calls/turn: {d}\n" ++
            "  batch_size:      {d}\n",
        .{
            request_mod.default_model,
            request_mod.default_max_tokens,
            loop.interactive_max_attempts,
            defaults.max_model_roundtrips_per_turn,
            defaults.max_tool_calls_per_turn,
            defaults.max_tool_batch_size,
        },
    );
    defer allocator.free(msg);
    return ToolResult.withPlainText(allocator, true, msg);
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
    defer allocator.free(msg);
    return ToolResult.withPlainText(allocator, true, msg);
}

fn renderHotkeys(allocator: std.mem.Allocator) !ToolResult {
    const msg = try allocator.dupe(
        u8,
        "Keyboard shortcuts (CLI REPL):\n" ++
            "  Enter          submit current line\n" ++
            "  Ctrl-C         interrupt / cancel\n" ++
            "  Ctrl-D         quit (EOF)\n",
    );
    defer allocator.free(msg);
    return ToolResult.withPlainText(allocator, true, msg);
}

fn renderLedgerGuidance(allocator: std.mem.Allocator) !ToolResult {
    const msg = try allocator.dupe(
        u8,
        "Session ledger is available from the CLI:\n" ++
            "  /ledger export <path>\n" ++
            "  zigttp ledger export --session <id> --out <path>\n" ++
            "  zigttp ledger replay --input <path> --onto <git-ref>\n",
    );
    defer allocator.free(msg);
    return ToolResult.withPlainText(allocator, true, msg);
}

fn renderChatGuidance(allocator: std.mem.Allocator) !ToolResult {
    const msg = try allocator.dupe(u8, "Already in the CLI REPL chat. Type help for local commands.\n");
    defer allocator.free(msg);
    return ToolResult.withPlainText(allocator, true, msg);
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
    return ToolResult.withPlainText(allocator, true, buf.items);
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
    return ToolResult.withPlainText(allocator, true, buf.items);
}

fn renderChangelog(allocator: std.mem.Allocator) !ToolResult {
    const msg = try allocator.dupe(
        u8,
        "zigttp expert - changelog\n" ++
            "  Full token accounting (input, cache_read, cache_write, output)\n" ++
            "  Post-apply auto-verify (verify_paths + diff review after each edit)\n" ++
            "  Session compaction (/compact)\n" ++
            "  Session branching: /fork, /tree, --fork, --continue\n" ++
            "  Session commands: /resume, /continue, /new, /compact, /fork, /tree\n" ++
            "  Proof ledger mode: /ledger, /chat, /ledger export, zigttp ledger replay/export\n" ++
            "  Route Forge: /feature previews route plans, /forge proves route candidates\n" ++
            "  Spec Forge: /forge spec annotates and proves handler Spec<...> intent\n" ++
            "  Author-declared specs: /specs reads Spec<...> obligations + discharge state\n" ++
            "  Skills catalog (/skill:<name>)\n" ++
            "  Informational commands: /model, /status, /settings, /hotkeys, /changelog\n",
    );
    defer allocator.free(msg);
    return ToolResult.withPlainText(allocator, true, msg);
}

fn renderTree(allocator: std.mem.Allocator, current_session_id: ?[]const u8) !ToolResult {
    const root = session_paths.sessionRoot(allocator) catch |err| {
        const msg = try std.fmt.allocPrint(allocator, "error reading session root: {s}\n", .{@errorName(err)});
        return .{ .ok = false, .llm_text = msg };
    };
    defer allocator.free(root);

    const hash = session_paths.cwdHashFull(allocator) catch |err| {
        const msg = try std.fmt.allocPrint(allocator, "error computing cwd hash: {s}\n", .{@errorName(err)});
        return .{ .ok = false, .llm_text = msg };
    };

    const entries = session_paths.listSessions(allocator, root, hash[0..]) catch |err| {
        const msg = try std.fmt.allocPrint(allocator, "error listing sessions: {s}\n", .{@errorName(err)});
        return .{ .ok = false, .llm_text = msg };
    };
    defer {
        for (entries) |*e| e.deinit(allocator);
        allocator.free(entries);
    }

    if (entries.len == 0) {
        return .{ .ok = true, .llm_text = try allocator.dupe(u8, "No sessions found for this workspace.\n") };
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
    if (is_tty) writeBanner(allocator);

    var session = try agent.initFromEnvWithSessionConfig(allocator, registry, .{
        .no_session = flags.no_session,
        .no_persist_tool_output = flags.no_persist_tool_output,
        .no_context_files = flags.no_context_files,
        .session_id = flags.session_id,
        .resume_latest = flags.resume_latest,
        .fork_session_id = flags.fork_session_id,
    });
    defer session.deinit(allocator);

    // Confirm a restore the user asked for (otherwise resume is silent), and
    // surface any policy-drift note that would otherwise reach only the model.
    if (is_tty and (flags.resume_latest or flags.fork_session_id != null)) {
        writeResumeDisclosure(&session, flags.fork_session_id != null);
    }

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
                if (result.llm_text.len > 0) {
                    _ = std.c.write(std.c.STDOUT_FILENO, result.llm_text.ptr, result.llm_text.len);
                    if (result.llm_text[result.llm_text.len - 1] != '\n') {
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

/// First-run orientation for the interactive expert. Leads with the value
/// proposition (compiler-verified edits), a copyable starter prompt, and how to
/// drive the loop, so a new user is never met with a bare prompt. TTY-only.
const expert_banner =
    "zigttp expert: I propose compiler-verified edits to your handler. Every draft is\n" ++
    "checked by the analyzer and rejected if it fails, so you only approve edits that pass.\n" ++
    "\n" ++
    "Try: add a GET /health route to handler.ts\n" ++
    "Type a goal in plain English, 'help' for commands, or 'quit' to exit.\n";

const expert_no_workspace_hint =
    "\nNo handler found here. Run 'zigttp init <name>' to scaffold a project first.\n";

fn writeBanner(allocator: std.mem.Allocator) void {
    _ = std.c.write(std.c.STDOUT_FILENO, expert_banner.ptr, expert_banner.len);
    if (!workspaceHasHandler(allocator)) {
        _ = std.c.write(std.c.STDOUT_FILENO, expert_no_workspace_hint.ptr, expert_no_workspace_hint.len);
    }
}

/// Best-effort detection of an editable handler in the current directory so the
/// banner can nudge a user who launched expert outside a project.
fn workspaceHasHandler(allocator: std.mem.Allocator) bool {
    const candidates = [_][]const u8{
        "zigttp.json", "src/handler.ts", "handler.ts", "src/handler.tsx", "handler.tsx",
    };
    for (candidates) |path| {
        if (zigts.file_io.fileExists(allocator, path)) return true;
    }
    return false;
}

/// One-line confirmation that a resumed/forked session was restored, plus an
/// echo of any policy-drift note so the user (not just the model) sees it.
fn writeResumeDisclosure(session: *const agent.AgentSession, forked: bool) void {
    var buf: [256]u8 = undefined;
    const id = session.session_id orelse "(ephemeral)";
    const verb = if (forked) "forked" else "resumed";
    const line = std.fmt.bufPrint(
        &buf,
        "{s} session {s}: {d} entries restored\n",
        .{ verb, id, session.transcript.len() },
    ) catch "session restored\n";
    _ = std.c.write(std.c.STDOUT_FILENO, line.ptr, line.len);

    if (agent.policyDriftNote(session)) |note| {
        _ = std.c.write(std.c.STDOUT_FILENO, note.ptr, note.len);
    }
}

fn approveEdit(preview: loop.ApprovalPreview) !bool {
    // Show what is being approved before asking: change size and the proven
    // properties the edit establishes (the differentiator), with `d` to dump the
    // full proposed file. Approving a verified change sight-unseen defeats the
    // human-in-the-loop gate, so the preview comes before the prompt.
    var hdr_buf: [512]u8 = undefined;
    const header = std.fmt.bufPrint(
        &hdr_buf,
        "\nVerified edit to {s}  ({d} -> {d} lines)\n",
        .{ preview.file, lineCount(preview.before orelse ""), lineCount(preview.after) },
    ) catch "\nVerified edit\n";
    _ = std.c.write(std.c.STDOUT_FILENO, header.ptr, header.len);
    if (preview.properties) |props| writeProvenProperties(props);

    while (true) {
        const prompt = "Apply this edit? [y/N, d=show proposed file] ";
        _ = std.c.write(std.c.STDOUT_FILENO, prompt.ptr, prompt.len);

        var line_buf: [256]u8 = undefined;
        const maybe_line = try readLine(&line_buf);
        const line = maybe_line orelse return false;
        const trimmed = std.mem.trim(u8, line, " \t\r\n");
        if (trimmed.len == 0) return false;
        switch (trimmed[0]) {
            'y', 'Y' => return true,
            'd', 'D' => {
                _ = std.c.write(std.c.STDOUT_FILENO, "\n", 1);
                _ = std.c.write(std.c.STDOUT_FILENO, preview.after.ptr, preview.after.len);
                if (preview.after.len == 0 or preview.after[preview.after.len - 1] != '\n') {
                    _ = std.c.write(std.c.STDOUT_FILENO, "\n", 1);
                }
                continue;
            },
            else => return false,
        }
    }
}

/// Rough logical line count for the approval change-size hint.
fn lineCount(text: []const u8) usize {
    if (text.len == 0) return 0;
    var n: usize = 1;
    for (text) |c| {
        if (c == '\n') n += 1;
    }
    // A trailing newline terminates the last line rather than starting a new one.
    if (text[text.len - 1] == '\n') n -= 1;
    return n;
}

/// Write the held (true) properties of the verified edit as a single line, e.g.
/// "  proven: deterministic, read_only, input_validated". This is the trust
/// signal that distinguishes an approved edit from a blind write.
fn writeProvenProperties(props: anytype) void {
    var buf: [1024]u8 = undefined;
    var w = std.Io.Writer.fixed(&buf);
    w.writeAll("  proven: ") catch {};
    var first = true;
    inline for (@typeInfo(@TypeOf(props)).@"struct".fields) |field| {
        if (field.type == bool and @field(props, field.name)) {
            if (!first) w.writeAll(", ") catch {};
            w.writeAll(field.name) catch {};
            first = false;
        }
    }
    if (first) w.writeAll("(no properties established)") catch {};
    w.writeAll("\n") catch {};
    const out = w.buffered();
    _ = std.c.write(std.c.STDOUT_FILENO, out.ptr, out.len);
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
    return loop.resolveApprovalFn(policy, loop.ApprovalFn.fromFn(approveEdit));
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
            try testing.expect(std.mem.indexOf(u8, r.llm_text, needle) != null);
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
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "/verify") != null);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "/status") != null);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, commands.session_commands[0]) != null);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, commands.session_commands[1]) != null);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, commands.view_commands[0]) != null);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, commands.ledger_commands[0]) != null);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "/feature route") != null);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "/forge route") != null);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "/specs <handler.ts>") != null);
            // Leads with how-to guidance and gates the tool dump behind --tools.
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "How to use zigttp expert") != null);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "help --tools") != null);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "Registered tools") == null);
        },
        else => return error.TestFailed,
    }
}

test "help --tools lists the registered tool names" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var outcome = try dispatchLine(testing.allocator, &reg, "help --tools");
    switch (outcome) {
        .result => |*r| {
            defer r.deinit(testing.allocator);
            try testing.expect(r.ok);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "Registered tools") != null);
            // Still leads with the how-to block.
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "How to use zigttp expert") != null);
        },
        else => return error.TestFailed,
    }
}

test "hotkeys and changelog mention Route Forge apply flow" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var hotkeys = try dispatchLine(testing.allocator, &reg, "/hotkeys");
    try expectResult(&hotkeys, testing.allocator, "CLI REPL", true);

    var changelog = try dispatchLine(testing.allocator, &reg, "/changelog");
    try expectResult(&changelog, testing.allocator, "Route Forge", true);
}

test "slash view commands route locally" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var ledger_outcome = try dispatchLine(testing.allocator, &reg, "/ledger");
    try expectResult(&ledger_outcome, testing.allocator, "zigttp ledger export --session", true);

    var chat_outcome = try dispatchLine(testing.allocator, &reg, "/chat");
    try expectResult(&chat_outcome, testing.allocator, "CLI REPL chat", true);
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
    switch (selectApprovalFn(.auto_approve)) {
        .bare => |func| try testing.expect(func == loop.autoApprove),
        else => return error.TestFailed,
    }
}

test "selectApprovalFn: auto_reject resolves to loop.autoReject" {
    switch (selectApprovalFn(.auto_reject)) {
        .bare => |func| try testing.expect(func == loop.autoReject),
        else => return error.TestFailed,
    }
}

test "selectApprovalFn: ask resolves to the interactive approveEdit" {
    switch (selectApprovalFn(.ask)) {
        .bare => |func| try testing.expect(func == approveEdit),
        else => return error.TestFailed,
    }
}

test "processSubmit: /settings reports compile-time defaults without themes" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);

    var outcome = try processSubmit(testing.allocator, &session, &reg, "/settings", null);
    switch (outcome) {
        .tool_result => |*r| {
            defer r.deinit(testing.allocator);
            try testing.expect(r.ok);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "max_tokens") != null);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "Theme") == null);
        },
        else => return error.TestFailed,
    }
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
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "provider:      stub") != null);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "auth:          stub") != null);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "persistence:   disabled") != null);
        },
        else => return error.TestFailed,
    }
}

test "processSubmit: /ledger and /chat switch views" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);

    var ledger_outcome = try processSubmit(testing.allocator, &session, &reg, "/ledger", null);
    switch (ledger_outcome) {
        .tool_result => |*r| {
            defer r.deinit(testing.allocator);
            try testing.expect(r.ok);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "zigttp ledger export --session") != null);
        },
        else => return error.TestFailed,
    }

    var chat_outcome = try processSubmit(testing.allocator, &session, &reg, "/chat", null);
    switch (chat_outcome) {
        .tool_result => |*r| {
            defer r.deinit(testing.allocator);
            try testing.expect(r.ok);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "CLI REPL chat") != null);
        },
        else => return error.TestFailed,
    }
}

test "processSubmit: /ledger export requires a persisted session" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);

    var outcome = try processSubmit(testing.allocator, &session, &reg, "/ledger export /tmp/proof.ndjson", null);
    switch (outcome) {
        .tool_result => |*r| {
            defer r.deinit(testing.allocator);
            try testing.expect(!r.ok);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, "persisted session") != null);
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

test "buildSessionTreeNodes: empty entries returns empty" {
    const nodes = try buildSessionTreeNodes(testing.allocator, &.{}, null);
    defer freeSessionTreeNodes(testing.allocator, nodes);
    try testing.expectEqual(@as(usize, 0), nodes.len);
}

test "buildSessionTreeNodes: multiple roots emit both" {
    const allocator = testing.allocator;
    var entries = [_]session_paths.SessionEntry{
        .{
            .session_id = try allocator.dupe(u8, "a"),
            .dir_path = try allocator.dupe(u8, "/tmp/a"),
            .created_at_unix_ms = 100,
            .parent_id = null,
        },
        .{
            .session_id = try allocator.dupe(u8, "b"),
            .dir_path = try allocator.dupe(u8, "/tmp/b"),
            .created_at_unix_ms = 200,
            .parent_id = null,
        },
    };
    defer for (&entries) |*e| e.deinit(allocator);

    const nodes = try buildSessionTreeNodes(allocator, &entries, null);
    defer freeSessionTreeNodes(allocator, nodes);

    try testing.expectEqual(@as(usize, 2), nodes.len);
    try testing.expectEqual(@as(usize, 0), nodes[0].depth);
    try testing.expectEqual(@as(usize, 0), nodes[1].depth);
    try testing.expect(!nodes[0].is_current);
    try testing.expect(!nodes[1].is_current);
    try testing.expect(!nodes[0].is_orphan_root);
}

test "buildSessionTreeNodes: grandchild gets depth 2" {
    const allocator = testing.allocator;
    var entries = [_]session_paths.SessionEntry{
        .{
            .session_id = try allocator.dupe(u8, "root"),
            .dir_path = try allocator.dupe(u8, "/tmp/root"),
            .created_at_unix_ms = 100,
            .parent_id = null,
        },
        .{
            .session_id = try allocator.dupe(u8, "child"),
            .dir_path = try allocator.dupe(u8, "/tmp/child"),
            .created_at_unix_ms = 200,
            .parent_id = try allocator.dupe(u8, "root"),
        },
        .{
            .session_id = try allocator.dupe(u8, "grand"),
            .dir_path = try allocator.dupe(u8, "/tmp/grand"),
            .created_at_unix_ms = 300,
            .parent_id = try allocator.dupe(u8, "child"),
        },
    };
    defer for (&entries) |*e| e.deinit(allocator);

    const nodes = try buildSessionTreeNodes(allocator, &entries, null);
    defer freeSessionTreeNodes(allocator, nodes);

    try testing.expectEqual(@as(usize, 3), nodes.len);
    try testing.expectEqualStrings("root", nodes[0].session_id);
    try testing.expectEqual(@as(usize, 0), nodes[0].depth);
    try testing.expectEqualStrings("child", nodes[1].session_id);
    try testing.expectEqual(@as(usize, 1), nodes[1].depth);
    try testing.expectEqualStrings("grand", nodes[2].session_id);
    try testing.expectEqual(@as(usize, 2), nodes[2].depth);
}

test "writeSessionTree: indents per depth, marks current with '*'" {
    const allocator = testing.allocator;
    const nodes = [_]SessionTreeNode{
        .{
            .session_id = try allocator.dupe(u8, "root"),
            .parent_id = null,
            .created_at_unix_ms = 1_000,
            .depth = 0,
            .is_current = false,
            .is_orphan_root = false,
        },
        .{
            .session_id = try allocator.dupe(u8, "child"),
            .parent_id = try allocator.dupe(u8, "root"),
            .created_at_unix_ms = 2_000,
            .depth = 1,
            .is_current = true,
            .is_orphan_root = false,
        },
    };
    defer {
        var mutable = nodes;
        for (&mutable) |*n| n.deinit(allocator);
    }

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    try writeSessionTree(&aw.writer, &nodes);

    const out = aw.writer.buffered();
    // Root: no indent, dash marker, id.
    try testing.expect(std.mem.startsWith(u8, out, "- root"));
    // Current child: indented two spaces, star marker, id.
    try testing.expect(std.mem.indexOf(u8, out, "\n  * child") != null);
}

test "writeSessionTree: orphan root gets orphaned-from annotation" {
    const allocator = testing.allocator;
    const nodes = [_]SessionTreeNode{
        .{
            .session_id = try allocator.dupe(u8, "stray"),
            .parent_id = try allocator.dupe(u8, "missing"),
            .created_at_unix_ms = 1_000,
            .depth = 0,
            .is_current = false,
            .is_orphan_root = true,
        },
    };
    defer {
        var mutable = nodes;
        for (&mutable) |*n| n.deinit(allocator);
    }

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    try writeSessionTree(&aw.writer, &nodes);

    const out = aw.writer.buffered();
    try testing.expect(std.mem.indexOf(u8, out, "orphaned-from=missing") != null);
}

test "writeSessionTree: empty nodes writes nothing" {
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    try writeSessionTree(&aw.writer, &.{});
    try testing.expectEqual(@as(usize, 0), aw.writer.end);
}

test "renderStatus: stub session shows 'stub' provider + 'ephemeral' session" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);

    var result = try renderStatus(testing.allocator, &session);
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "provider:      stub") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "auth:          stub") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "session:       ephemeral") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "persistence:   disabled") != null);
}

test "renderStatus: enumerates every token total" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    session.token_totals = .{
        .input_tokens = 11,
        .output_tokens = 22,
        .cache_read_input_tokens = 33,
        .cache_creation_input_tokens = 44,
    };

    var result = try renderStatus(testing.allocator, &session);
    defer result.deinit(testing.allocator);

    try testing.expect(std.mem.indexOf(u8, result.llm_text, "tokens.in:     11") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "tokens.out:    22") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "tokens.cache_r:33") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "tokens.cache_w:44") != null);
}

test "renderStudio returns workbench command and URL" {
    var result = try renderStudio(testing.allocator, "examples/handler/handler.ts");
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "zigttp studio examples/handler/handler.ts") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "http://localhost:8080/_zigttp/studio") != null);
}
