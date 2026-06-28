//! Line-buffered expert REPL driver.
//!
//! Natural language goes to the model by default. Deterministic slash commands,
//! safe raw `zigts ...` / `zig build ...` commands, and direct tool names are
//! routed locally.

const std = @import("std");
const builtin = @import("builtin");
const zigts = @import("zigts");
const registry_mod = @import("registry/registry.zig");
const transcript_mod = @import("transcript.zig");
const agent = @import("agent.zig");
const commands = @import("commands.zig");
const loop = @import("loop.zig");
const app = @import("app.zig");
const ledger = @import("ledger.zig");
const session_events = @import("session/events.zig");
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
            var ticker: WorkingTicker = .{};
            ticker.start();
            defer ticker.finish();
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
            var ticker: WorkingTicker = .{};
            ticker.start();
            defer ticker.finish();
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
        return .{ .tool_result = try renderLedgerGuidance(allocator, session.metrics.summary()) };
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
        var ticker: WorkingTicker = .{};
        ticker.start();
        defer ticker.finish();
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
    if (commands.isViewLedger(argv[0])) return .{ .result = try renderLedgerGuidance(allocator, .{}) };
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
        \\  1. Type what you want in plain English, e.g. "add a GET /health route to src/handler.ts".
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
            "  Ctrl-C         stop the session\n" ++
            "  Ctrl-D         quit (EOF)\n",
    );
    defer allocator.free(msg);
    return ToolResult.withPlainText(allocator, true, msg);
}

fn renderLedgerGuidance(allocator: std.mem.Allocator, summary: session_events.SessionSummary) !ToolResult {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    if (summary.turn_count > 0) {
        try w.writeAll("This session so far:\n");
        try w.print("  turns:             {d}\n", .{summary.turn_count});
        try w.print("  model round-trips: {d}\n", .{summary.total_roundtrips});
        try w.print("  verified edits:    {d}\n", .{summary.verified_patch_count});
        if (summary.reached_proof) {
            try w.print("  round-trips to first green proof: {d}\n", .{summary.round_trips_to_first_green});
            try w.print("  proven-path ratio: {d}/{d} guarantees ({d:.0}%)\n", .{
                summary.proven_properties,
                summary.tracked_properties,
                summary.provenPathRatio() * 100,
            });
        } else {
            try w.writeAll("  no verified edit applied yet\n");
        }
        try w.writeAll("\n");
    }

    try w.writeAll(
        "Session ledger is available from the CLI:\n" ++
            "  /ledger export <path>\n" ++
            "  zigttp ledger export --session <id> --out <path>\n" ++
            "  zigttp ledger replay --input <path> --onto <git-ref>\n",
    );

    buf = aw.toArrayList();
    return ToolResult.withPlainText(allocator, true, buf.items);
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
    /// Explicit policy from `--yes` / `--no-edit`. Null means the user did not
    /// pass a policy flag; the session's stored policy (from a prior `--yes` run,
    /// persisted via meta.json) is used when available, falling back to `.ask`.
    explicit_policy: ?loop.ApprovalPolicy,
) !void {
    const is_tty = std.c.isatty(std.c.STDIN_FILENO) != 0;
    if (is_tty) writeBanner(allocator);

    // Encode the explicit policy as a string tag so it can be written to
    // meta.json and restored on --resume without events.zig importing loop.zig.
    const policy_tag: ?[]const u8 = if (explicit_policy) |p| @tagName(p) else null;

    var session = try agent.initFromEnvWithSessionConfig(allocator, registry, .{
        .no_session = flags.no_session,
        .no_persist_tool_output = flags.no_persist_tool_output,
        .no_context_files = flags.no_context_files,
        .session_id = flags.session_id,
        .resume_latest = flags.resume_latest,
        .fork_session_id = flags.fork_session_id,
        .model = flags.model,
        .approval_policy_tag = policy_tag,
    });
    defer session.deinit(allocator);

    // Effective policy: explicit flag wins; fall back to the stored policy from
    // the resumed session's meta.json; finally default to .ask.
    const policy: loop.ApprovalPolicy = explicit_policy orelse
        session.stored_approval_policy orelse
        .ask;
    const approval_fn = selectApprovalFn(policy);

    // Confirm a restore the user asked for (otherwise resume is silent), and
    // surface any policy-drift note that would otherwise reach only the model.
    if (is_tty and (flags.resume_latest or flags.fork_session_id != null)) {
        writeResumeDisclosure(&session, flags.fork_session_id != null);
    }

    // Live streaming of transcript entries during a turn, for interactive TTY
    // sessions only (piped output keeps the original post-turn render so its
    // ordering is unchanged).
    var stream_ctx = InteractiveStreamCtx{ .allocator = allocator };

    var line_buf: [64 * 1024]u8 = undefined;
    while (true) {
        if (is_tty) {
            const prompt = "expert> ";
            _ = std.c.write(std.c.STDOUT_FILENO, prompt.ptr, prompt.len);
        }

        const maybe_line = try readLine(&line_buf);
        const line = maybe_line orelse break;

        // Stream entries live during interactive turns. Cleared right after so
        // session rebuilds (resume/new/fork) don't replay the restored history.
        stream_ctx.streamed = false;
        if (is_tty) {
            session.transcript.observer = .{ .context = &stream_ctx, .on_append = InteractiveStreamCtx.onAppend };
        }
        var outcome = processSubmit(allocator, &session, registry, line, approval_fn) catch |err| {
            session.transcript.observer = null;
            loop.writeTurnErrorToStderr(err);
            continue;
        };
        session.transcript.observer = null;

        switch (outcome) {
            .noop => {},
            .quit => break,
            .rendered => |rendered| {
                defer allocator.free(rendered);
                // When the entries were streamed live, the last one is already
                // on screen; printing `rendered` again would duplicate it.
                if (!stream_ctx.streamed) {
                    _ = std.c.write(std.c.STDOUT_FILENO, rendered.ptr, rendered.len);
                }
                if (is_tty) {
                    const tok = session.token_totals;
                    var token_buf: [160]u8 = undefined;
                    const token_line = std.fmt.bufPrint(
                        &token_buf,
                        "[session tokens (cumulative): in={d} cache_r={d} cache_w={d} out={d}]\n",
                        .{
                            tok.input_tokens,
                            tok.cache_read_input_tokens,
                            tok.cache_creation_input_tokens,
                            tok.output_tokens,
                        },
                    ) catch "";
                    _ = std.c.write(std.c.STDOUT_FILENO, token_line.ptr, token_line.len);
                    maybeAutoCompact(allocator, &session);
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

    // Session ended (quit or EOF): write the per-session metrics row.
    session.writeSessionSummary(allocator);
}

/// First-run orientation for the interactive expert. Leads with the value
/// proposition (compiler-verified edits), a copyable starter prompt, and how to
/// drive the loop, so a new user is never met with a bare prompt. TTY-only.
const expert_banner =
    "zigttp expert: I propose compiler-verified edits to your handler. Every draft is\n" ++
    "checked by the analyzer and rejected if it fails, so you only approve edits that pass.\n" ++
    "\n" ++
    "Each turn calls your model provider and consumes API credits.\n" ++
    "\n" ++
    "Try: add a GET /health route to src/handler.ts\n" ++
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

/// Streams each transcript entry to stdout the moment it is appended during a
/// turn, so the user watches the compiler-in-the-loop work (the model's prose,
/// each tool call, each veto retry) as it happens instead of staring at a frozen
/// prompt until the whole turn returns. Reuses the same per-entry renderer as
/// the post-turn path, so the live output matches what was shown before. Marks
/// `streamed` so the caller skips the duplicate post-turn print.
const InteractiveStreamCtx = struct {
    allocator: std.mem.Allocator,
    streamed: bool = false,

    fn onAppend(context: *anyopaque, entry: *const transcript_mod.OwnedEntry) void {
        const self: *InteractiveStreamCtx = @ptrCast(@alignCast(context));
        // The user just typed their prompt; no need to echo it back.
        switch (entry.*) {
            .user_text => return,
            else => {},
        }
        const rendered = transcript_mod.renderRichEntryToOwnedTty(self.allocator, entry) catch return;
        defer self.allocator.free(rendered);
        if (!builtin.is_test) {
            // Clear the elapsed-time tick before the first entry prints so they
            // never collide on the same line.
            stopWorkingTicker();
            _ = std.c.write(std.c.STDOUT_FILENO, rendered.ptr, rendered.len);
        }
        self.streamed = true;
    }
};

/// Immediate feedback for the gap between submitting a prompt and the first
/// streamed entry arriving, so the terminal is never silently frozen. The
/// provider client reads the whole response body before returning, so without
/// this the terminal would look frozen for the entire first turn. A background
/// thread rewrites a single `working... Ns` line each second until the first
/// entry streams (or the turn returns). TTY-only, never in --print/json/test.
const WorkingTicker = struct {
    thread: ?std.Thread = null,
    stop: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),

    /// File-scope handle so the live-stream observer can stop the ticker the
    /// instant the first entry arrives, before it prints over the elapsed line.
    var active: ?*WorkingTicker = null;

    fn start(self: *WorkingTicker) void {
        if (builtin.is_test) return;
        if (std.c.isatty(std.c.STDOUT_FILENO) == 0) return;
        self.stop.store(false, .seq_cst);
        active = self;
        self.thread = std.Thread.spawn(.{}, tick, .{self}) catch {
            // Without the thread, fall back to a static one-shot notice so the
            // user still sees that work started.
            active = null;
            const msg = "working...\n";
            _ = std.c.write(std.c.STDOUT_FILENO, msg.ptr, msg.len);
            return;
        };
    }

    /// Idempotent: safe to call from both the observer and the post-turn path.
    fn finish(self: *WorkingTicker) void {
        active = null;
        if (self.thread) |t| {
            self.stop.store(true, .seq_cst);
            t.join();
            self.thread = null;
            // Clear the elapsed-time line so the entry text starts clean.
            const clear = "\r\x1b[K";
            _ = std.c.write(std.c.STDOUT_FILENO, clear.ptr, clear.len);
        }
    }

    fn tick(self: *WorkingTicker) void {
        var seconds: u64 = 0;
        while (!self.stop.load(.seq_cst)) {
            var buf: [64]u8 = undefined;
            const line = std.fmt.bufPrint(&buf, "\rworking... {d}s", .{seconds}) catch "\rworking...";
            _ = std.c.write(std.c.STDOUT_FILENO, line.ptr, line.len);
            // Sleep in short slices so stop is observed within ~50ms of finish().
            var slept: u64 = 0;
            while (slept < std.time.ns_per_s) : (slept += 50 * std.time.ns_per_ms) {
                if (self.stop.load(.seq_cst)) return;
                var req = std.c.timespec{ .sec = 0, .nsec = 50 * std.time.ns_per_ms };
                _ = std.c.nanosleep(&req, null);
            }
            seconds += 1;
        }
    }
};

/// Stops the active ticker (if any) so the first streamed entry prints over a
/// cleared line instead of colliding with the elapsed-time tick. No-op when no
/// ticker is running (e.g. dispatched tool commands).
fn stopWorkingTicker() void {
    if (WorkingTicker.active) |t| t.finish();
}

/// Surface a one-line notice when the shared auto-compaction guard fired, so the
/// user knows earlier turns were summarized. The decision and the compaction
/// itself live in `agent.maybeAutoCompact`; this only prints.
fn maybeAutoCompact(allocator: std.mem.Allocator, session: *agent.AgentSession) void {
    const compacted = agent.maybeAutoCompact(allocator, session) catch return;
    if (!compacted) return;
    const notice = "[auto-compacted: the conversation neared the model's context window; earlier turns were summarized into one note]\n";
    _ = std.c.write(std.c.STDOUT_FILENO, notice.ptr, notice.len);
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
    if (preview.rewrite_trace.len > 0) writeRewriteTrace(preview.rewrite_trace);
    // Show what changed inline so review+approve is one keystroke; `d` still
    // dumps the full proposed file for cases the compact diff cannot convey.
    writeEditDiff(preview.before, preview.after);

    while (true) {
        const prompt = "Apply this edit? [y/N, d=show file] ";
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
                writeMaybeTruncated(preview.after);
                continue;
            },
            else => return false,
        }
    }
}

/// Lines above this threshold trigger head/tail truncation in the `d` diff preview.
const diff_truncate_lines: usize = 100;

/// Write `content` to stdout. When the content exceeds `diff_truncate_lines` lines,
/// show the first 50 and last 20 lines separated by a truncation notice.
fn writeMaybeTruncated(content: []const u8) void {
    const n = lineCount(content);
    if (n <= diff_truncate_lines) {
        _ = std.c.write(std.c.STDOUT_FILENO, content.ptr, content.len);
        if (content.len == 0 or content[content.len - 1] != '\n') {
            _ = std.c.write(std.c.STDOUT_FILENO, "\n", 1);
        }
        return;
    }
    // Split into head (first 50 lines) and tail (last 20 lines).
    const head_lines: usize = 50;
    const tail_lines: usize = 20;
    const omitted = n - head_lines - tail_lines;
    // Find byte offset after `head_lines` newlines.
    var head_end: usize = 0;
    var found: usize = 0;
    while (head_end < content.len) : (head_end += 1) {
        if (content[head_end] == '\n') {
            found += 1;
            if (found >= head_lines) {
                head_end += 1; // include the newline
                break;
            }
        }
    }
    // Find byte offset before the last `tail_lines` newlines.
    var tail_start: usize = content.len;
    var found_tail: usize = 0;
    var i: usize = content.len;
    while (i > 0) {
        i -= 1;
        if (content[i] == '\n') {
            found_tail += 1;
            if (found_tail > tail_lines) {
                tail_start = i + 1;
                break;
            }
        }
    }
    _ = std.c.write(std.c.STDOUT_FILENO, content.ptr, head_end);
    var trunc_buf: [128]u8 = undefined;
    const trunc_msg = std.fmt.bufPrint(
        &trunc_buf,
        "... [{d} lines truncated] ...\n",
        .{omitted},
    ) catch "... [lines truncated] ...\n";
    _ = std.c.write(std.c.STDOUT_FILENO, trunc_msg.ptr, trunc_msg.len);
    if (tail_start < content.len) {
        _ = std.c.write(std.c.STDOUT_FILENO, content[tail_start..].ptr, content.len - tail_start);
        if (content[content.len - 1] != '\n') {
            _ = std.c.write(std.c.STDOUT_FILENO, "\n", 1);
        }
    }
}

/// Lines of unchanged context shown around each change in the approval diff.
const diff_context_lines: usize = 3;

/// Byte offset just past the line that starts at `i` (the index after its '\n',
/// or `s.len` for a final line that has no terminator).
fn lineEndIncl(s: []const u8, i: usize) usize {
    const nl = std.mem.indexOfScalarPos(u8, s, i, '\n') orelse return s.len;
    return nl + 1;
}

/// Byte offset where the line ending at `end` (exclusive) begins.
fn lineStart(s: []const u8, end: usize) usize {
    if (end == 0) return 0;
    var k = end - 1;
    while (k > 0 and s[k - 1] != '\n') k -= 1;
    return k;
}

/// Byte offset in `s` just past its first `k` lines (or `s.len` if fewer).
fn offsetAfterLines(s: []const u8, k: usize) usize {
    var i: usize = 0;
    var c: usize = 0;
    while (c < k and i < s.len) : (c += 1) i = lineEndIncl(s, i);
    return i;
}

/// Split `s` into line slices, each including its trailing '\n' (except a final
/// unterminated line). Returns the count; never writes past `out`.
fn splitLines(s: []const u8, out: [][]const u8) usize {
    var n: usize = 0;
    var i: usize = 0;
    while (i < s.len and n < out.len) {
        const e = lineEndIncl(s, i);
        out[n] = s[i..e];
        n += 1;
        i = e;
    }
    return n;
}

/// Write one diff row: `marker` ("- ", "+ ", "  ") then the line with its EOL
/// stripped (so CRLF does not leak a '\r' that would garble the marker), then a
/// newline. A source line with no trailing newline (the file's last line) gets a
/// git-style "no newline" note so a trailing-newline-only change is legible.
fn writeDiffRow(w: *std.Io.Writer, marker: []const u8, line_incl_term: []const u8) !void {
    var content = line_incl_term;
    var terminated = false;
    if (content.len > 0 and content[content.len - 1] == '\n') {
        content = content[0 .. content.len - 1];
        terminated = true;
    }
    if (content.len > 0 and content[content.len - 1] == '\r') content = content[0 .. content.len - 1];
    try w.writeAll(marker);
    try w.writeAll(content);
    try w.writeByte('\n');
    if (!terminated) try w.writeAll("  \\ No newline at end of file\n");
}

/// Write each line of `region` (a run of unchanged lines) as context rows.
fn writeContextLines(w: *std.Io.Writer, region: []const u8) !void {
    var i: usize = 0;
    while (i < region.len) {
        const e = lineEndIncl(region, i);
        try writeDiffRow(w, "  ", region[i..e]);
        i = e;
    }
}

/// Emit the LCS-aligned diff of two line arrays: shared lines as context, lines
/// only in `bl` as removed, lines only in `al` as added. Both arrays are bounded
/// by the caller to at most `diff_truncate_lines` lines.
fn writeLcsDiff(w: *std.Io.Writer, bl: []const []const u8, al: []const []const u8) !void {
    // dp[i][j] = LCS length of bl[i..] and al[j..], filled from the bottom-right.
    var dp: [diff_truncate_lines + 2][diff_truncate_lines + 2]u16 = undefined;
    var i: usize = bl.len + 1;
    while (i > 0) {
        i -= 1;
        var j: usize = al.len + 1;
        while (j > 0) {
            j -= 1;
            if (i == bl.len or j == al.len) {
                dp[i][j] = 0;
            } else if (std.mem.eql(u8, bl[i], al[j])) {
                dp[i][j] = dp[i + 1][j + 1] + 1;
            } else {
                dp[i][j] = @max(dp[i + 1][j], dp[i][j + 1]);
            }
        }
    }
    var x: usize = 0;
    var y: usize = 0;
    while (x < bl.len and y < al.len) {
        if (std.mem.eql(u8, bl[x], al[y])) {
            try writeDiffRow(w, "  ", bl[x]);
            x += 1;
            y += 1;
        } else if (dp[x + 1][y] >= dp[x][y + 1]) {
            try writeDiffRow(w, "- ", bl[x]);
            x += 1;
        } else {
            try writeDiffRow(w, "+ ", al[y]);
            y += 1;
        }
    }
    while (x < bl.len) : (x += 1) try writeDiffRow(w, "- ", bl[x]);
    while (y < al.len) : (y += 1) try writeDiffRow(w, "+ ", al[y]);
}

/// Render a compact line diff of `before` -> `after` for the approval preview.
/// Trims the common leading/trailing lines, then runs an LCS over the changed
/// middle so multiple edits in one file render as separate hunks (unchanged lines
/// between them stay context, not removed+added). A middle larger than
/// `diff_truncate_lines` falls back to a one-line "press d" summary; `before == null`
/// (a new file) diffs against empty so the whole file shows as additions.
fn renderEditDiff(w: *std.Io.Writer, before: ?[]const u8, after: []const u8) !void {
    const b = before orelse "";
    if (std.mem.eql(u8, b, after)) {
        try w.writeAll("  (no changes)\n");
        return;
    }

    // Trim common leading lines.
    var lb: usize = 0;
    var la: usize = 0;
    while (lb < b.len and la < after.len) {
        const eb = lineEndIncl(b, lb);
        const ea = lineEndIncl(after, la);
        if (!std.mem.eql(u8, b[lb..eb], after[la..ea])) break;
        lb = eb;
        la = ea;
    }
    // Trim common trailing lines without crossing the leading region.
    var tb: usize = b.len;
    var ta: usize = after.len;
    while (tb > lb and ta > la) {
        const sb = lineStart(b, tb);
        const sa = lineStart(after, ta);
        if (sb < lb or sa < la) break;
        if (!std.mem.eql(u8, b[sb..tb], after[sa..ta])) break;
        tb = sb;
        ta = sa;
    }

    const mid_b = b[lb..tb];
    const mid_a = after[la..ta];

    // Bound the LCS table: a change spanning more lines than the cap defers to the
    // full proposed file behind `d` (counted on the changed span, not the file).
    if (lineCount(mid_b) > diff_truncate_lines or lineCount(mid_a) > diff_truncate_lines) {
        var buf: [128]u8 = undefined;
        const msg = std.fmt.bufPrint(
            &buf,
            "  large change spanning {d}/{d} lines. Press d to show the proposed file.\n",
            .{ lineCount(mid_b), lineCount(mid_a) },
        ) catch "  large change. Press d to show the proposed file.\n";
        try w.writeAll(msg);
        return;
    }

    // Leading context: the last `diff_context_lines` shared lines before the change.
    const lead = b[0..lb];
    const lead_skip = if (lineCount(lead) > diff_context_lines) lineCount(lead) - diff_context_lines else 0;
    try writeContextLines(w, lead[offsetAfterLines(lead, lead_skip)..]);

    var b_lines: [diff_truncate_lines + 2][]const u8 = undefined;
    var a_lines: [diff_truncate_lines + 2][]const u8 = undefined;
    const nb = splitLines(mid_b, &b_lines);
    const na = splitLines(mid_a, &a_lines);
    try writeLcsDiff(w, b_lines[0..nb], a_lines[0..na]);

    // Trailing context: the first `diff_context_lines` shared lines after the change.
    const trail = b[tb..];
    try writeContextLines(w, trail[0..offsetAfterLines(trail, diff_context_lines)]);
}

/// Render the inline approval diff to stdout. Buffers into a fixed scratch so a
/// pathological case (very long lines overflowing the buffer) cleanly falls back
/// to the full proposed file, which `d` also serves.
fn writeEditDiff(before: ?[]const u8, after: []const u8) void {
    var buf: [16 * 1024]u8 = undefined;
    var w = std.Io.Writer.fixed(&buf);
    renderEditDiff(&w, before, after) catch return writeMaybeTruncated(after);
    const out = w.buffered();
    _ = std.c.write(std.c.STDOUT_FILENO, out.ptr, out.len);
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

/// Write canonical-normalization rewrite names applied during salvage, e.g.
/// "  normalized: ternary-redundancy-elimination (line 12), bool-compare-redundancy".
fn writeRewriteTrace(trace: []const []u8) void {
    var buf: [1024]u8 = undefined;
    var w = std.Io.Writer.fixed(&buf);
    w.writeAll("  normalized: ") catch {};
    for (trace, 0..) |name, i| {
        if (i > 0) w.writeAll(", ") catch {};
        w.writeAll(name) catch {};
    }
    w.writeByte('\n') catch {};
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

test "EXP-5: hotkeys help no longer claims Ctrl-C interrupts a blocked turn" {
    var result = try renderHotkeys(testing.allocator);
    defer result.deinit(testing.allocator);
    // No SIGINT handler exists, so the old "interrupt / cancel" text was a lie.
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "interrupt") == null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "Ctrl-C         stop the session") != null);
}

test "EXP-8: interactive banner warns that each turn spends API credits" {
    try testing.expect(std.mem.indexOf(u8, expert_banner, "consumes API credits") != null);
}

test "EXP-8: post-turn token line is labelled cumulative for the whole session" {
    // The line accumulates across turns; the label must say so. Asserted against
    // the same literal the interactive loop formats so a relabel can't silently
    // drift back to the unlabelled per-turn-looking form.
    var buf: [160]u8 = undefined;
    const line = std.fmt.bufPrint(
        &buf,
        "[session tokens (cumulative): in={d} cache_r={d} cache_w={d} out={d}]\n",
        .{ @as(u64, 1), @as(u64, 2), @as(u64, 3), @as(u64, 4) },
    ) catch unreachable;
    try testing.expect(std.mem.indexOf(u8, line, "cumulative") != null);
}

test "InteractiveStreamCtx skips user_text and marks streamed for model entries" {
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var ctx = InteractiveStreamCtx{ .allocator = testing.allocator };

    // The user's own prompt is not echoed back, so streamed stays false.
    try tr.append(testing.allocator, .{ .user_text = "hi" });
    InteractiveStreamCtx.onAppend(&ctx, &tr.entries.items[tr.entries.items.len - 1]);
    try testing.expect(!ctx.streamed);

    // A model entry is streamed live and marks streamed so the caller skips the
    // duplicate post-turn print.
    try tr.append(testing.allocator, .{ .model_text = "inspecting the handler" });
    InteractiveStreamCtx.onAppend(&ctx, &tr.entries.items[tr.entries.items.len - 1]);
    try testing.expect(ctx.streamed);
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

fn renderDiffToBuf(buf: []u8, before: ?[]const u8, after: []const u8) []const u8 {
    var w = std.Io.Writer.fixed(buf);
    renderEditDiff(&w, before, after) catch unreachable;
    return w.buffered();
}

test "renderEditDiff: middle insertion shows the added line with context" {
    var buf: [256]u8 = undefined;
    try testing.expectEqualStrings("  a\n+ b\n  c\n", renderDiffToBuf(&buf, "a\nc\n", "a\nb\nc\n"));
}

test "renderEditDiff: pure deletion shows the removed line with context" {
    var buf: [256]u8 = undefined;
    try testing.expectEqualStrings("  a\n- b\n  c\n", renderDiffToBuf(&buf, "a\nb\nc\n", "a\nc\n"));
}

test "renderEditDiff: two separate edits keep the unchanged middle as context (not removed+added)" {
    var buf: [256]u8 = undefined;
    // Only lines 2 and 5 change; c and d must render as context, never as -/+.
    try testing.expectEqualStrings(
        "  a\n- b\n+ X\n  c\n  d\n- e\n+ Y\n",
        renderDiffToBuf(&buf, "a\nb\nc\nd\ne\n", "a\nX\nc\nd\nY\n"),
    );
}

test "renderEditDiff: CRLF line endings do not leak a carriage return" {
    var buf: [256]u8 = undefined;
    const out = renderDiffToBuf(&buf, "x\r\ny\r\n", "x\r\nz\r\n");
    try testing.expectEqualStrings("  x\n- y\n+ z\n", out);
    try testing.expect(std.mem.indexOfScalar(u8, out, '\r') == null);
}

test "renderEditDiff: a trailing-newline-only change is legible via the no-newline note" {
    var buf: [256]u8 = undefined;
    try testing.expectEqualStrings(
        "  a\n  b\n- c\n  \\ No newline at end of file\n+ c\n",
        renderDiffToBuf(&buf, "a\nb\nc", "a\nb\nc\n"),
    );
}

test "renderEditDiff: identical content reports no changes" {
    var buf: [256]u8 = undefined;
    try testing.expectEqualStrings("  (no changes)\n", renderDiffToBuf(&buf, "a\nb\n", "a\nb\n"));
}

test "renderEditDiff: a change spanning more than the cap falls back to the file view" {
    var bbuf: [1024]u8 = undefined;
    var abuf: [1024]u8 = undefined;
    var bw = std.Io.Writer.fixed(&bbuf);
    var aw = std.Io.Writer.fixed(&abuf);
    // 105 lines, first and last differ -> no common prefix/suffix, middle > cap.
    aw.writeAll("b\n") catch unreachable;
    var n: usize = 0;
    while (n < 105) : (n += 1) {
        bw.writeAll("a\n") catch unreachable;
        if (n > 0 and n < 104) aw.writeAll("a\n") catch unreachable;
    }
    aw.writeAll("b\n") catch unreachable;
    var obuf: [8192]u8 = undefined;
    const out = renderDiffToBuf(&obuf, bw.buffered(), aw.buffered());
    try testing.expect(std.mem.indexOf(u8, out, "large change") != null);
}

test "renderLedgerGuidance shows live session metrics when turns exist" {
    var res = try renderLedgerGuidance(testing.allocator, .{
        .turn_count = 3,
        .total_roundtrips = 7,
        .verified_patch_count = 1,
        .reached_proof = true,
        .round_trips_to_first_green = 5,
        .proven_properties = 12,
        .tracked_properties = 16,
    });
    defer res.deinit(testing.allocator);
    try testing.expect(std.mem.indexOf(u8, res.llm_text, "This session so far:") != null);
    try testing.expect(std.mem.indexOf(u8, res.llm_text, "round-trips to first green proof: 5") != null);
    try testing.expect(std.mem.indexOf(u8, res.llm_text, "12/16 guarantees") != null);
    // CLI guidance is still appended.
    try testing.expect(std.mem.indexOf(u8, res.llm_text, "zigttp ledger export") != null);
}

test "renderLedgerGuidance omits the metrics block for a fresh session" {
    var res = try renderLedgerGuidance(testing.allocator, .{});
    defer res.deinit(testing.allocator);
    try testing.expect(std.mem.indexOf(u8, res.llm_text, "This session so far:") == null);
    try testing.expect(std.mem.indexOf(u8, res.llm_text, "zigttp ledger export") != null);
}
