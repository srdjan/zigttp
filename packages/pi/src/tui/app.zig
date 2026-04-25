//! Full-screen raw-mode TUI for the expert workflow. The transcript is the
//! durable source of truth; local slash-command results live only in `AppState`
//! so the TUI can surface them without changing replay semantics.

const std = @import("std");
const zigts = @import("zigts");
const term = @import("term.zig");
const ansi = @import("ansi.zig");
const line_editor = @import("line_editor.zig");
const repl = @import("../repl.zig");
const agent = @import("../agent.zig");
const loop = @import("../loop.zig");
const app = @import("../app.zig");
const autoloop = @import("../autoloop.zig");
const transcript_mod = @import("../transcript.zig");
const ui_payload_mod = @import("../ui_payload.zig");
const witness_replay = @import("../witness_replay.zig");
const session_events = @import("../session/events.zig");
const session_state = @import("../session_state.zig");
const property_goals = @import("../property_goals.zig");
const registry_tool = @import("../registry/tool.zig");
const tools_common = @import("../tools/common.zig");

const LineEditor = line_editor.LineEditor;
const KeyEvent = line_editor.KeyEvent;
const KeyKind = line_editor.KeyKind;
const Registry = repl.Registry;
const ExpertFlags = app.ExpertFlags;
const UiPayload = ui_payload_mod.UiPayload;
const ToolResult = registry_tool.ToolResult;

const prompt_label = "expert> ";
const alternate_screen_enter = "\x1b[?1049h";
const alternate_screen_exit = "\x1b[?1049l";
const clear_screen = "\x1b[2J\x1b[H";
const hide_cursor = "\x1b[?25l";
const show_cursor = "\x1b[?25h";

pub const FocusMode = enum {
    composer,
    feed,
    inspector,
};

pub const ViewMode = enum {
    ledger,
    chat,
};

const StatusNotice = union(enum) {
    none,
    structural_property,
    invalid_property,
    /// `g` was just pressed on a goal-driveable chip; the autoloop is in
    /// flight. The slice is the property name and is owned by the comptime
    /// PropertiesSnapshot field set, so no allocation is required.
    goal_driving: []const u8,
    /// `g` was just pressed on a witness in the witnesses tab; the
    /// autoloop is converging on that specific counterexample. Both
    /// fields borrow from `pending_witness_focus` and the comptime
    /// property table, so the variant itself owns nothing.
    witness_driving: WitnessDriving,
    /// The autoloop returned. Carries the verdict so the user can see
    /// whether the chip went green or which budget tripped.
    goal_completed: GoalCompleted,
    /// The autoloop dispatch itself failed (workspace lookup, tool error,
    /// allocation). Distinct from a non-`achieved` verdict.
    goal_dispatch_error: []const u8,
    /// `r` was pressed on a witness but no replay implementation is
    /// registered. Surfaced inline in the status row so the user knows
    /// the keystroke was recognised but cannot run.
    replay_unavailable,
    /// Witness replay finished. The TUI uses this to surface `replayed
    /// PASS/FIXED <key>` in the status row alongside the inline verdict
    /// rendered in the witness pane.
    replay_done: ReplayDone,
    /// Witness replay errored before the engine could finish (file
    /// missing, parse error, tool unwired). The error name surfaces in
    /// the status row.
    replay_error: []const u8,
    /// `m` minted the selected witness as a regression test case.
    /// `short_key` borrows from the witness body in the transcript.
    /// The full path lands as a system_note for durability; this
    /// notice just acknowledges the action in the status row.
    mint_done: []const u8,
    /// `m` was pressed but minting could not proceed: missing verdict,
    /// witness selection out of range, or filesystem failure.
    mint_error: []const u8,
};

const ReplayDone = struct {
    reproduced: bool,
    /// First 12 hex chars of the witness key. Borrowed from the witness
    /// body; the verdict record itself owns the full key.
    short_key: []const u8,
};

const WitnessDriving = struct {
    property: []const u8,
    short_key: []const u8,
};

const GoalCompleted = struct {
    name: []const u8,
    verdict: autoloop.AutoloopVerdict,
};

const LayoutMode = enum {
    split,
    stacked,
};

pub const FeedItemKind = enum {
    user_text,
    model_text,
    tool_use,
    tool_result,
    proof_card,
    diagnostic_box,
    verified_patch,
    system_note,
    local_result,
};

const FeedSource = union(enum) {
    transcript: usize,
    local_result: usize,
};

pub const FeedItem = struct {
    source: FeedSource,
    kind: FeedItemKind,
};

/// Pending dispatch carrier for `g` pressed on a witness. Owns its key
/// copy so the keystroke handler does not have to keep the source
/// witness body alive while the autoloop runs.
pub const PendingWitnessFocus = struct {
    key: []u8,
    /// Property tag (e.g. `no_secret_leakage`). Borrowed from the
    /// `boolPropertyNameAt` table; free `key` only.
    property: []const u8,

    pub fn deinit(self: *PendingWitnessFocus, allocator: std.mem.Allocator) void {
        allocator.free(self.key);
        self.* = .{ .key = &.{}, .property = &.{} };
    }
};

/// Owned record of a witness replay verdict held in AppState. Pairs the
/// stable witness key with the runtime's `Verdict` so the renderer can
/// confirm the verdict still belongs to the currently-selected witness
/// after the patch or selection changes.
pub const WitnessVerdictRecord = struct {
    /// Stable witness key the verdict applies to. Owned.
    key: []u8,
    verdict: witness_replay.Verdict,
    /// Cached classification of `verdict` against the original witness
    /// body (was the violation reproduced?). The renderer surfaces this
    /// as PASS / FIXED so the user does not have to interpret status
    /// codes themselves.
    reproduced: bool,

    pub fn deinit(self: *WitnessVerdictRecord, allocator: std.mem.Allocator) void {
        allocator.free(self.key);
        self.verdict.deinit(allocator);
        self.* = .{ .key = &.{}, .verdict = .{ .ran = false, .actual_status = 0, .actual_body = &.{}, .error_text = null }, .reproduced = false };
    }
};

const LedgerTab = enum {
    /// Proof-first summary: property delta, violations before/after, witness
    /// status, chain metadata. The default tab - callers trust the badges
    /// and expand the diff only when a change needs line-level inspection.
    delta,
    diff,
    properties,
    violations,
    prove,
    system,
    citations,
    /// Counterexample bodies (request + IO stub script) for each witness
    /// the patch closed or introduced. Step 1 of the Witness Theater.
    witnesses,
};

pub const ComposerState = struct {
    line: []const u8 = "",
    cursor: usize = 0,
};

pub const InspectorState = struct {
    scroll_offset: usize = 0,
};

const ApprovalChoice = enum {
    reject,
    approve,
};

const ApprovalModal = struct {
    file: []const u8,
    choice: ApprovalChoice = .reject,
};

/// One optional modal overlay. Extend to a tagged union if a second
/// kind ever appears; for now the approval prompt is the only modal.
pub const ModalState = ?ApprovalModal;

const LocalResult = struct {
    title: []u8,
    ok: bool,
    llm_text: []u8,
    ui_payload: ?UiPayload = null,

    fn initFromToolResult(
        allocator: std.mem.Allocator,
        title: []const u8,
        result: *const ToolResult,
    ) !LocalResult {
        return .{
            .title = try allocator.dupe(u8, title),
            .ok = result.ok,
            .llm_text = try allocator.dupe(u8, result.llm_text),
            .ui_payload = if (result.ui_payload) |payload|
                try payload.clone(allocator)
            else
                null,
        };
    }

    fn initPlainText(
        allocator: std.mem.Allocator,
        title: []const u8,
        ok: bool,
        body: []const u8,
    ) !LocalResult {
        return .{
            .title = try allocator.dupe(u8, title),
            .ok = ok,
            .llm_text = try allocator.dupe(u8, body),
            .ui_payload = .{ .plain_text = try allocator.dupe(u8, body) },
        };
    }

    fn deinit(self: *LocalResult, allocator: std.mem.Allocator) void {
        allocator.free(self.title);
        allocator.free(self.llm_text);
        if (self.ui_payload) |*payload| payload.deinit(allocator);
        self.* = .{
            .title = &.{},
            .ok = false,
            .llm_text = &.{},
            .ui_payload = null,
        };
    }
};

pub const AppState = struct {
    feed_items: std.ArrayListUnmanaged(FeedItem) = .empty,
    ledger_items: std.ArrayListUnmanaged(usize) = .empty,
    local_results: std.ArrayListUnmanaged(LocalResult) = .empty,
    selected_feed_index: usize = 0,
    selected_ledger_index: usize = 0,
    feed_scroll: usize = 0,
    ledger_scroll: usize = 0,
    selected_property_index: usize = 0,
    focus_mode: FocusMode = .composer,
    view_mode: ViewMode = .ledger,
    ledger_tab: LedgerTab = .delta,
    diff_expanded: bool = false,
    composer: ComposerState = .{},
    inspector: InspectorState = .{},
    modal: ModalState = null,
    status_notice: StatusNotice = .none,
    observed_transcript_len: usize = 0,
    layout_mode: LayoutMode = .split,
    /// True while a property-goal autoloop is running on the main thread.
    /// Single-flight guard: a second `g` press is dropped until the run
    /// returns. Step 3 (async) replaces this with a worker-pipe wakeup.
    autoloop_in_flight: bool = false,
    /// Cursor in the witnesses tab. Indexes (defeated ++ new) for the
    /// currently selected ledger patch. Reset to 0 whenever the patch
    /// selection changes.
    selected_witness_index: usize = 0,
    /// Verdict from the most recent `r` (replay) on the selected witness.
    /// Owned. Cleared when the patch selection or witness selection
    /// changes, or when the user explicitly clears it. The TUI renders
    /// it inline below the selected witness in the witnesses tab.
    witness_verdict: ?WitnessVerdictRecord = null,
    /// Pending witness-scoped goal drive: stable key copy stashed by the
    /// `g` keystroke handler in the witnesses tab, consumed by
    /// `driveSelectedGoal` and freed there. Non-null only between the
    /// keystroke and the autoloop dispatch (a few milliseconds at most).
    pending_witness_focus: ?PendingWitnessFocus = null,

    pub fn deinit(self: *AppState, allocator: std.mem.Allocator) void {
        self.feed_items.deinit(allocator);
        self.ledger_items.deinit(allocator);
        self.clearLocalResults(allocator);
        self.clearWitnessVerdict(allocator);
        self.clearPendingWitnessFocus(allocator);
    }

    pub fn clearWitnessVerdict(self: *AppState, allocator: std.mem.Allocator) void {
        if (self.witness_verdict) |*record| {
            record.deinit(allocator);
            self.witness_verdict = null;
        }
    }

    pub fn clearPendingWitnessFocus(self: *AppState, allocator: std.mem.Allocator) void {
        if (self.pending_witness_focus) |*focus| {
            focus.deinit(allocator);
            self.pending_witness_focus = null;
        }
    }

    pub fn clearLocalResults(self: *AppState, allocator: std.mem.Allocator) void {
        for (self.local_results.items) |*result| result.deinit(allocator);
        self.local_results.clearAndFree(allocator);
    }

    pub fn sync(
        self: *AppState,
        allocator: std.mem.Allocator,
        session: *const agent.AgentSession,
        editor: *const LineEditor,
    ) !void {
        self.composer = .{
            .line = editor.line(),
            .cursor = editor.cursor(),
        };
        if (self.observed_transcript_len > session.transcript.len()) {
            self.clearLocalResults(allocator);
            self.clearWitnessVerdict(allocator);
            try self.rebuildTranscriptFeed(allocator, session, editor);
            return;
        }

        const follow_tail = self.shouldFollowTail();
        while (self.observed_transcript_len < session.transcript.len()) : (self.observed_transcript_len += 1) {
            const entry = session.transcript.at(self.observed_transcript_len);
            try self.feed_items.append(allocator, .{
                .source = .{ .transcript = self.observed_transcript_len },
                .kind = kindForTranscriptEntry(entry),
            });
            switch (entry.*) {
                .verified_patch => try self.ledger_items.append(allocator, self.observed_transcript_len),
                else => {},
            }
        }
        if (follow_tail and self.feed_items.items.len > 0) {
            self.selected_feed_index = self.feed_items.items.len - 1;
        }
        self.clampSelection();
        self.clampLedgerSelection();
        self.clampPropertySelection();
    }

    pub fn resetToTranscript(
        self: *AppState,
        allocator: std.mem.Allocator,
        session: *const agent.AgentSession,
        editor: *const LineEditor,
    ) !void {
        try self.rebuildTranscriptFeed(allocator, session, editor);
    }

    fn rebuildTranscriptFeed(
        self: *AppState,
        allocator: std.mem.Allocator,
        session: *const agent.AgentSession,
        editor: *const LineEditor,
    ) !void {
        self.feed_items.clearRetainingCapacity();
        self.observed_transcript_len = 0;
        self.feed_scroll = 0;
        self.ledger_scroll = 0;
        self.inspector.scroll_offset = 0;
        self.selected_feed_index = 0;
        self.selected_ledger_index = 0;
        self.selected_property_index = 0;
        self.diff_expanded = false;
        self.ledger_items.clearRetainingCapacity();
        self.composer = .{
            .line = editor.line(),
            .cursor = editor.cursor(),
        };
        self.status_notice = .none;
        while (self.observed_transcript_len < session.transcript.len()) : (self.observed_transcript_len += 1) {
            const entry = session.transcript.at(self.observed_transcript_len);
            try self.feed_items.append(allocator, .{
                .source = .{ .transcript = self.observed_transcript_len },
                .kind = kindForTranscriptEntry(entry),
            });
            switch (entry.*) {
                .verified_patch => try self.ledger_items.append(allocator, self.observed_transcript_len),
                else => {},
            }
        }
        if (self.feed_items.items.len > 0) {
            self.selected_feed_index = self.feed_items.items.len - 1;
        }
        if (self.ledger_items.items.len > 0) {
            self.selected_ledger_index = self.ledger_items.items.len - 1;
        }
        self.clampSelection();
        self.clampLedgerSelection();
        self.clampPropertySelection();
    }

    pub fn appendLocalToolResult(
        self: *AppState,
        allocator: std.mem.Allocator,
        title: []const u8,
        result: *const ToolResult,
    ) !void {
        try self.local_results.append(allocator, try LocalResult.initFromToolResult(allocator, title, result));
        try self.feed_items.append(allocator, .{
            .source = .{ .local_result = self.local_results.items.len - 1 },
            .kind = .local_result,
        });
        self.selected_feed_index = self.feed_items.items.len - 1;
        self.focus_mode = .inspector;
        self.inspector.scroll_offset = 0;
    }

    pub fn appendLocalText(
        self: *AppState,
        allocator: std.mem.Allocator,
        title: []const u8,
        ok: bool,
        body: []const u8,
    ) !void {
        try self.local_results.append(allocator, try LocalResult.initPlainText(allocator, title, ok, body));
        try self.feed_items.append(allocator, .{
            .source = .{ .local_result = self.local_results.items.len - 1 },
            .kind = .local_result,
        });
        self.selected_feed_index = self.feed_items.items.len - 1;
        self.focus_mode = .inspector;
        self.inspector.scroll_offset = 0;
    }

    fn shouldFollowTail(self: *const AppState) bool {
        return self.feed_items.items.len == 0 or self.selected_feed_index + 1 >= self.feed_items.items.len;
    }

    fn clampSelection(self: *AppState) void {
        if (self.feed_items.items.len == 0) {
            self.selected_feed_index = 0;
            self.feed_scroll = 0;
            return;
        }
        if (self.selected_feed_index >= self.feed_items.items.len) {
            self.selected_feed_index = self.feed_items.items.len - 1;
        }
    }

    fn selectedFeedItem(self: *const AppState) ?FeedItem {
        if (self.feed_items.items.len == 0) return null;
        return self.feed_items.items[self.selected_feed_index];
    }

    fn selectedLedgerIndex(self: *const AppState) ?usize {
        if (self.ledger_items.items.len == 0) return null;
        return self.ledger_items.items[self.selected_ledger_index];
    }

    fn clampLedgerSelection(self: *AppState) void {
        if (self.ledger_items.items.len == 0) {
            self.selected_ledger_index = 0;
            self.ledger_scroll = 0;
            return;
        }
        if (self.selected_ledger_index >= self.ledger_items.items.len) {
            self.selected_ledger_index = self.ledger_items.items.len - 1;
        }
    }

    fn clampPropertySelection(self: *AppState) void {
        if (property_goals.bool_property_count == 0) {
            self.selected_property_index = 0;
            return;
        }
        if (self.selected_property_index >= property_goals.bool_property_count) {
            self.selected_property_index = property_goals.bool_property_count - 1;
        }
    }

    fn clearStatusNotice(self: *AppState) void {
        self.status_notice = .none;
    }
};

const TerminalSize = struct {
    width: usize,
    height: usize,
};

const Layout = struct {
    mode: LayoutMode,
    body_height: usize,
    feed_width: usize,
    inspector_width: usize,
    feed_height: usize,
    inspector_height: usize,

    fn totalBodyRows(self: Layout) usize {
        return switch (self.mode) {
            .split => self.body_height,
            .stacked => self.feed_height + self.inspector_height,
        };
    }
};

const OwnedLines = struct {
    storage: []u8,
    lines: []const []const u8,

    fn deinit(self: *OwnedLines, allocator: std.mem.Allocator) void {
        allocator.free(self.lines);
        allocator.free(self.storage);
        self.* = .{ .storage = &.{}, .lines = &.{} };
    }
};

const ComposerView = struct {
    visible: []const u8,
    cursor_col: usize,
};

const ModalAction = union(enum) {
    none,
    redraw,
    decided: bool,
};

const TuiRuntime = struct {
    allocator: std.mem.Allocator,
    session: *agent.AgentSession,
    editor: *LineEditor,
    state: *AppState,

    fn syncState(self: *TuiRuntime) !void {
        try self.state.sync(self.allocator, self.session, self.editor);
    }

    fn redrawFrame(self: *TuiRuntime) !void {
        try self.syncState();
        try redraw(self.allocator, self.session, self.state);
    }
};

pub fn run(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    flags: ExpertFlags,
    policy: loop.ApprovalPolicy,
) !void {
    var raw = try term.RawMode.enter();
    defer raw.exit();

    writeAll(alternate_screen_enter);
    defer {
        writeAll(show_cursor);
        writeAll(ansi.reset);
        writeAll(alternate_screen_exit);
    }

    var editor: LineEditor = .{};
    defer editor.deinit(allocator);

    var session = try agent.initFromEnvWithSessionConfig(allocator, registry, .{
        .no_session = flags.no_session,
        .no_persist_tool_output = flags.no_persist_tool_output,
        .no_context_files = flags.no_context_files,
        .session_id = flags.session_id,
        .resume_latest = flags.resume_latest,
        .fork_session_id = flags.fork_session_id,
    });
    defer session.deinit(allocator);

    var state: AppState = .{};
    defer state.deinit(allocator);

    var runtime: TuiRuntime = .{
        .allocator = allocator,
        .session = &session,
        .editor = &editor,
        .state = &state,
    };
    const approval_fn = selectApprovalFn(policy, &runtime);

    try runtime.redrawFrame();

    var input_buf: [256]u8 = undefined;
    while (true) {
        const n = std.posix.read(std.posix.STDIN_FILENO, &input_buf) catch |err| switch (err) {
            error.WouldBlock => continue,
            else => return err,
        };
        if (n == 0) break;

        var i: usize = 0;
        while (i < n) {
            const event, const consumed = parseKeyEvent(input_buf[0..n], i);
            i += consumed;

            if (state.modal != null) continue;

            switch (state.focus_mode) {
                .composer => {
                    if (try handleComposerEvent(&runtime, registry, flags, approval_fn, event)) return;
                },
                .feed, .inspector => {
                    switch (handlePaneEvent(runtime.allocator, &state, &session, event)) {
                        .no_op => {},
                        .redraw => try runtime.redrawFrame(),
                        .quit => return,
                        .approve => {
                            try approveSelectedPatch(&runtime);
                            try runtime.redrawFrame();
                        },
                        .drive_goal => {
                            // Paint the in-flight indicator before the
                            // (currently sync-blocking) autoloop runs so
                            // the user sees the chip-driving status notice
                            // and not a stale frame for the duration.
                            try runtime.redrawFrame();
                            try driveSelectedGoal(&runtime, registry);
                            try runtime.redrawFrame();
                        },
                        .replay_witness => {
                            try runtime.redrawFrame();
                            try replaySelectedWitness(&runtime);
                            try runtime.redrawFrame();
                        },
                        .mint_witness => {
                            try mintSelectedWitness(&runtime);
                            try runtime.redrawFrame();
                        },
                    }
                },
            }
        }
    }
}

fn handleComposerEvent(
    runtime: *TuiRuntime,
    registry: *const Registry,
    flags: ExpertFlags,
    approval_fn: loop.ApprovalFn,
    event: KeyEvent,
) !bool {
    switch (event.kind) {
        .tab => {
            runtime.state.focus_mode = nextFocus(runtime.state.focus_mode);
            try runtime.redrawFrame();
            return false;
        },
        .shift_tab => {
            runtime.state.focus_mode = prevFocus(runtime.state.focus_mode);
            try runtime.redrawFrame();
            return false;
        },
        .esc => {
            runtime.state.focus_mode = .composer;
            try runtime.redrawFrame();
            return false;
        },
        else => {},
    }

    const action = try runtime.editor.handle(runtime.allocator, event);
    switch (action) {
        .none => return false,
        .redraw => {
            try runtime.redrawFrame();
            return false;
        },
        .cancel => return true,
        .submit => {
            const submitted = runtime.editor.line();
            if (try handleSubmit(runtime, registry, flags, approval_fn, submitted)) return true;
            runtime.editor.clear(runtime.allocator);
            try runtime.redrawFrame();
            return false;
        },
    }
}

pub const PaneOutcome = enum {
    /// Event was not recognised by this pane; caller should not redraw.
    no_op,
    /// Event consumed; caller redraws.
    redraw,
    /// User asked to quit.
    quit,
    /// User pressed `A` on a selected ledger patch whose witness state is
    /// clean. Caller resolves by emitting an approval system_note.
    approve,
    /// User pressed `g` on a selected goal-driveable property chip. The
    /// caller redraws to surface the in-flight indicator, then invokes
    /// `driveSelectedGoal` so the autoloop runs against the live session
    /// transcript. The chip's file binds to the currently-selected ledger
    /// patch.
    drive_goal,
    /// User pressed `r` on a selected witness in the witnesses tab. The
    /// caller resolves by invoking the registered witness replay impl
    /// against the patch's handler file and storing the verdict in
    /// `state.witness_verdict` for the next redraw.
    replay_witness,
    /// User pressed `m` on a selected witness with a recent verdict.
    /// The caller serialises the witness as a JSONL test case
    /// (request + io stubs + an `expect` clause derived from the
    /// verdict's actual status) and appends it to the handler's
    /// regression-tests file.
    mint_witness,
};

fn handlePaneEvent(
    allocator: std.mem.Allocator,
    state: *AppState,
    session: *const agent.AgentSession,
    event: KeyEvent,
) PaneOutcome {
    switch (event.kind) {
        .tab => {
            state.clearStatusNotice();
            if (state.view_mode == .ledger) {
                state.ledger_tab = nextLedgerTab(state.ledger_tab);
                state.clampPropertySelection();
                state.focus_mode = .inspector;
            } else {
                state.focus_mode = nextFocus(state.focus_mode);
            }
            return .redraw;
        },
        .shift_tab => {
            state.clearStatusNotice();
            if (state.view_mode == .ledger) {
                state.ledger_tab = prevLedgerTab(state.ledger_tab);
                state.clampPropertySelection();
                state.focus_mode = .inspector;
            } else {
                state.focus_mode = prevFocus(state.focus_mode);
            }
            return .redraw;
        },
        .esc => {
            state.focus_mode = .composer;
            return .redraw;
        },
        .left => {
            if (state.view_mode == .ledger and state.ledger_tab == .properties) {
                if (state.selected_property_index == 0) return .no_op;
                state.clearStatusNotice();
                state.selected_property_index -= 1;
                return .redraw;
            }
            return .no_op;
        },
        .right => {
            if (state.view_mode == .ledger and state.ledger_tab == .properties) {
                if (state.selected_property_index + 1 >= property_goals.bool_property_count) return .no_op;
                state.clearStatusNotice();
                state.selected_property_index += 1;
                return .redraw;
            }
            return .no_op;
        },
        .up => {
            state.clearStatusNotice();
            if (state.view_mode == .ledger) {
                if (state.ledger_tab == .witnesses and state.focus_mode == .inspector) {
                    moveWitnessSelectionUp(allocator, state);
                } else {
                    moveLedgerSelectionUp(allocator, state);
                }
            } else {
                moveSelectionUp(state);
            }
            return .redraw;
        },
        .down => {
            state.clearStatusNotice();
            if (state.view_mode == .ledger) {
                if (state.ledger_tab == .witnesses and state.focus_mode == .inspector) {
                    moveWitnessSelectionDown(allocator, state, session);
                } else {
                    moveLedgerSelectionDown(allocator, state);
                }
            } else {
                moveSelectionDown(state);
            }
            return .redraw;
        },
        .enter => {
            if (state.view_mode == .ledger and
                (state.ledger_tab == .diff or state.ledger_tab == .delta))
            {
                state.diff_expanded = !state.diff_expanded;
            } else if (state.focus_mode == .feed) {
                state.focus_mode = .inspector;
            }
            return .redraw;
        },
        .char => switch (event.byte) {
            'c', 'C' => {
                state.view_mode = .chat;
                return .redraw;
            },
            'l', 'L' => {
                state.clearStatusNotice();
                state.view_mode = .ledger;
                return .redraw;
            },
            'w', 'W' => {
                state.clearStatusNotice();
                state.view_mode = .ledger;
                state.ledger_tab = .witnesses;
                state.focus_mode = .inspector;
                return .redraw;
            },
            'g', 'G' => {
                if (state.view_mode != .ledger) return .no_op;
                if (state.autoloop_in_flight) return .no_op;
                if (state.ledger_tab == .witnesses) {
                    return dispatchWitnessGoal(allocator, state, session);
                }
                if (state.ledger_tab != .properties) return .no_op;
                const name = property_goals.boolPropertyNameAt(state.selected_property_index) orelse {
                    state.status_notice = .invalid_property;
                    return .redraw;
                };
                switch (property_goals.classify(name)) {
                    .goal_driveable => {
                        // The chip's file binds to the currently-selected
                        // ledger patch. No selection means no anchor and the
                        // dispatch is meaningless.
                        const idx = state.selectedLedgerIndex() orelse {
                            state.status_notice = .invalid_property;
                            return .redraw;
                        };
                        if (session_state.patchPayload(session.transcript.at(idx)) == null) {
                            state.status_notice = .invalid_property;
                            return .redraw;
                        }
                        state.autoloop_in_flight = true;
                        state.status_notice = .{ .goal_driving = name };
                        return .drive_goal;
                    },
                    .structural => {
                        state.status_notice = .structural_property;
                        return .redraw;
                    },
                    .unknown => {
                        state.status_notice = .invalid_property;
                        return .redraw;
                    },
                }
            },
            'r', 'R' => {
                if (state.view_mode != .ledger or state.ledger_tab != .witnesses) return .no_op;
                const total = witnessCountForSelectedPatch(state, session);
                if (total == 0) return .no_op;
                if (state.selected_witness_index >= total) return .no_op;
                if (!witness_replay.isConfigured()) {
                    state.status_notice = .{ .replay_unavailable = {} };
                    return .redraw;
                }
                state.clearWitnessVerdict(allocator);
                return .replay_witness;
            },
            'm', 'M' => {
                if (state.view_mode != .ledger or state.ledger_tab != .witnesses) return .no_op;
                if (state.witness_verdict == null) {
                    state.status_notice = .{ .mint_error = "no recent verdict; press r first" };
                    return .redraw;
                }
                const total = witnessCountForSelectedPatch(state, session);
                if (total == 0 or state.selected_witness_index >= total) {
                    state.status_notice = .{ .mint_error = "no witness selected" };
                    return .redraw;
                }
                return .mint_witness;
            },
            'A' => {
                if (state.view_mode == .ledger and approvalGateClear(state, session)) {
                    return .approve;
                }
                return .redraw;
            },
            else => return .no_op,
        },
        .ctrl_c, .eof => return .quit,
        else => return .no_op,
    }
}

/// Approval is only offered on a selected patch with no new witnesses and
/// a successful post-apply check. The gate is the load-bearing UX claim of
/// the Proof Delta Card: a single keystroke is safe only because witness
/// state is clean.
fn approveSelectedPatch(runtime: *TuiRuntime) !void {
    const allocator = runtime.allocator;
    const transcript_index = runtime.state.selectedLedgerIndex() orelse return;
    const patch = session_state.patchPayload(runtime.session.transcript.at(transcript_index)) orelse return;

    const note = if (patch.patch_hash) |hash| blk: {
        const hex = std.fmt.bytesToHex(hash, .lower);
        break :blk try std.fmt.allocPrint(
            allocator,
            "approved {s} for {s}",
            .{ hex[0..12], patch.file },
        );
    } else try std.fmt.allocPrint(allocator, "approved {s}", .{patch.file});
    var note_owned_by_transcript = false;
    errdefer if (!note_owned_by_transcript) allocator.free(note);

    try runtime.session.transcript.entries.append(allocator, .{ .system_note = note });
    note_owned_by_transcript = true;

    if (runtime.session.events_path) |path| {
        try session_events.appendEvent(allocator, path, .{ .system_note = note });
    }
}

fn approvalGateClear(state: *const AppState, session: *const agent.AgentSession) bool {
    const transcript_index = state.selectedLedgerIndex() orelse return false;
    const patch = session_state.patchPayload(session.transcript.at(transcript_index)) orelse return false;
    return patch.post_apply_ok and patch.witnesses_new.len == 0;
}

/// Replay the witness at `state.selected_witness_index` against the
/// patch's handler file using the registered witness_replay impl. The
/// verdict lands in `state.witness_verdict` (owned), and a one-line
/// PASS/FIXED summary surfaces in the status row. Errors during replay
/// (missing impl, file not found, parse failure) flow into
/// `status_notice = .replay_error` rather than aborting the TUI.
fn replaySelectedWitness(runtime: *TuiRuntime) !void {
    const allocator = runtime.allocator;
    const state = runtime.state;

    const ledger_idx = state.selectedLedgerIndex() orelse {
        state.status_notice = .{ .replay_error = "no patch selected" };
        return;
    };
    const patch = session_state.patchPayload(runtime.session.transcript.at(ledger_idx)) orelse {
        state.status_notice = .{ .replay_error = "selected entry is not a patch" };
        return;
    };

    const witness = selectedWitness(patch, state.selected_witness_index) orelse {
        state.status_notice = .{ .replay_error = "witness selection out of range" };
        return;
    };

    const workspace_root = tools_common.workspaceRoot(allocator) catch {
        state.status_notice = .{ .replay_error = "workspace root unresolved" };
        return;
    };
    defer allocator.free(workspace_root);
    const handler_path = tools_common.resolveInsideWorkspace(
        allocator,
        workspace_root,
        patch.file,
    ) catch {
        state.status_notice = .{ .replay_error = "handler path not in workspace" };
        return;
    };
    defer allocator.free(handler_path);

    var verdict = witness_replay.replay(allocator, handler_path, witness) catch |err| {
        state.status_notice = .{ .replay_error = @errorName(err) };
        return;
    };
    errdefer verdict.deinit(allocator);

    const reproduced = verdict.reproducedViolation(witness);
    const key_copy = try allocator.dupe(u8, witness.key);
    errdefer allocator.free(key_copy);

    state.clearWitnessVerdict(allocator);
    state.witness_verdict = .{
        .key = key_copy,
        .verdict = verdict,
        .reproduced = reproduced,
    };
    state.status_notice = .{ .replay_done = .{
        .reproduced = reproduced,
        .short_key = shortKey(state.witness_verdict.?.key),
    } };
}

/// Map a flat witness index (defeated ++ new) to the underlying body.
fn selectedWitness(
    patch: ui_payload_mod.VerifiedPatchPayload,
    index: usize,
) ?ui_payload_mod.WitnessBody {
    if (index < patch.witnesses_defeated.len) return patch.witnesses_defeated[index];
    const offset = index - patch.witnesses_defeated.len;
    if (offset < patch.witnesses_new.len) return patch.witnesses_new[offset];
    return null;
}

/// Append the selected witness to the handler's regression tests file
/// as a JSONL test case. The current `state.witness_verdict` becomes the
/// `expect` clause - the test asserts the post-fix response shape, so a
/// future regression that re-introduces the leak fails the test.
///
/// File layout: `<handler-dir>/witness-regressions.jsonl`. Append-only;
/// duplicates from repeated minting are tolerated because the test
/// runner runs each case independently and the JSONL parser is
/// per-line.
fn mintSelectedWitness(runtime: *TuiRuntime) !void {
    const allocator = runtime.allocator;
    const state = runtime.state;

    const ledger_idx = state.selectedLedgerIndex() orelse {
        state.status_notice = .{ .mint_error = "no patch selected" };
        return;
    };
    const patch = session_state.patchPayload(runtime.session.transcript.at(ledger_idx)) orelse {
        state.status_notice = .{ .mint_error = "selected entry is not a patch" };
        return;
    };
    const witness = selectedWitness(patch, state.selected_witness_index) orelse {
        state.status_notice = .{ .mint_error = "witness selection out of range" };
        return;
    };
    const verdict_record = state.witness_verdict orelse {
        state.status_notice = .{ .mint_error = "no recent verdict; press r first" };
        return;
    };
    if (!std.mem.eql(u8, verdict_record.key, witness.key)) {
        state.status_notice = .{ .mint_error = "verdict does not match selected witness" };
        return;
    }
    if (!verdict_record.verdict.ran) {
        state.status_notice = .{ .mint_error = "verdict errored; replay before minting" };
        return;
    }

    const workspace_root = tools_common.workspaceRoot(allocator) catch {
        state.status_notice = .{ .mint_error = "workspace root unresolved" };
        return;
    };
    defer allocator.free(workspace_root);
    const handler_path = tools_common.resolveInsideWorkspace(allocator, workspace_root, patch.file) catch {
        state.status_notice = .{ .mint_error = "handler path not in workspace" };
        return;
    };
    defer allocator.free(handler_path);

    const tests_path = mintTestsPathFor(allocator, handler_path) catch {
        state.status_notice = .{ .mint_error = "tests path build failed" };
        return;
    };
    defer allocator.free(tests_path);

    appendWitnessRegression(
        allocator,
        tests_path,
        witness,
        verdict_record.verdict.actual_status,
    ) catch |err| {
        state.status_notice = .{ .mint_error = @errorName(err) };
        return;
    };

    const short = shortKey(witness.key);
    const note = try std.fmt.allocPrint(
        allocator,
        "minted regression test: {s} -> {s} (status {d})",
        .{ short, tests_path, verdict_record.verdict.actual_status },
    );
    var note_owned_by_transcript = false;
    errdefer if (!note_owned_by_transcript) allocator.free(note);

    try runtime.session.transcript.entries.append(allocator, .{ .system_note = note });
    note_owned_by_transcript = true;

    if (runtime.session.events_path) |path| {
        try session_events.appendEvent(allocator, path, .{ .system_note = note });
    }

    state.status_notice = .{ .mint_done = short };
}

/// Build the per-handler regression tests path. Sits next to the
/// handler so `zig build -Dhandler=... -Dtest-file=...` lines up
/// without extra plumbing. `handler_path` arrives from
/// `tools_common.resolveInsideWorkspace` and is always an absolute path
/// inside the workspace root, so `dirname` cannot be null at this
/// callsite; the error path treats a null dirname as a bug rather than
/// a recoverable condition.
fn mintTestsPathFor(allocator: std.mem.Allocator, handler_path: []const u8) ![]u8 {
    const dir = std.fs.path.dirname(handler_path) orelse return error.HandlerPathHasNoDirectory;
    return std.fs.path.join(allocator, &.{ dir, "witness-regressions.jsonl" });
}

fn appendWitnessRegression(
    allocator: std.mem.Allocator,
    tests_path: []const u8,
    witness: ui_payload_mod.WitnessBody,
    expect_status: u16,
) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll("{\"type\":\"test\",\"name\":\"witness-");
    try w.writeAll(shortKey(witness.key));
    try w.writeAll("-");
    try w.writeAll(witness.property);
    try w.writeAll("\"}\n");
    try witness_replay.writeWitnessJsonl(w, witness);
    try w.print("{{\"type\":\"expect\",\"status\":{d}}}\n", .{expect_status});

    buf = aw.toArrayList();

    // Read existing content (if any), concatenate, write back.
    // The runtime's writeFile overwrites; there is no append helper,
    // and the regression file is small enough that read+write is fine.
    const existing = zigts.file_io.readFile(allocator, tests_path, 16 * 1024 * 1024) catch |err| switch (err) {
        error.FileNotFound => try allocator.alloc(u8, 0),
        else => return err,
    };
    defer allocator.free(existing);

    var combined: std.ArrayList(u8) = .empty;
    defer combined.deinit(allocator);
    try combined.appendSlice(allocator, existing);
    if (existing.len > 0 and existing[existing.len - 1] != '\n') {
        try combined.append(allocator, '\n');
    }
    try combined.appendSlice(allocator, buf.items);

    try zigts.file_io.writeFile(allocator, tests_path, combined.items);
}

/// Validate the selected witness, stash a focus key in AppState, and
/// hand off via `.drive_goal`. The autoloop dispatcher
/// (`driveSelectedGoal`) reads `pending_witness_focus` to decide
/// whether the run targets a property chip or a specific witness.
/// Returns `.redraw` with a status notice on any failure (no patch
/// selected, witness out of range, property not goal-driveable).
fn dispatchWitnessGoal(
    allocator: std.mem.Allocator,
    state: *AppState,
    session: *const agent.AgentSession,
) PaneOutcome {
    const ledger_idx = state.selectedLedgerIndex() orelse {
        state.status_notice = .invalid_property;
        return .redraw;
    };
    const patch = session_state.patchPayload(session.transcript.at(ledger_idx)) orelse {
        state.status_notice = .invalid_property;
        return .redraw;
    };
    const witness = selectedWitness(patch, state.selected_witness_index) orelse {
        state.status_notice = .invalid_property;
        return .redraw;
    };
    const property_tag = property_goals.parseDriveableGoal(witness.property) orelse {
        state.status_notice = .structural_property;
        return .redraw;
    };
    const property_name = property_tag.asString();

    const key_copy = allocator.dupe(u8, witness.key) catch {
        state.status_notice = .invalid_property;
        return .redraw;
    };
    state.clearPendingWitnessFocus(allocator);
    state.pending_witness_focus = .{ .key = key_copy, .property = property_name };
    state.autoloop_in_flight = true;
    state.status_notice = .{ .witness_driving = .{
        .property = property_name,
        .short_key = shortKey(key_copy),
    } };
    return .drive_goal;
}

/// Run the property-goal autoloop against the live session transcript for
/// the chip or witness the user pressed `g` on. The dispatcher reads
/// `pending_witness_focus` to choose between two anchors:
///
///   * Witness-focused: the loop converges on a single counterexample
///     by stable key, regardless of unrelated witnesses still live for
///     the same property tag.
///   * Chip-focused (default): the loop converges on the entire
///     property class for the chip the user selected on the properties
///     tab.
///
/// In both cases the file is the currently-selected ledger patch's
/// file; `handlePaneEvent` has already validated the anchor and flipped
/// `autoloop_in_flight` true, so this helper owns the dispatch and the
/// post-run status update, and always clears `autoloop_in_flight` so a
/// failure can never wedge the rail.
///
/// Sync-blocking by design at step 1: the UI freezes for the run. Step 3
/// moves `autoloop.drive` to a worker thread and replaces the freeze with
/// a self-pipe wakeup. The status notice is the same in either world.
fn driveSelectedGoal(runtime: *TuiRuntime, registry: *const Registry) !void {
    const allocator = runtime.allocator;
    defer runtime.state.autoloop_in_flight = false;
    defer runtime.state.clearPendingWitnessFocus(allocator);

    const idx = runtime.state.selectedLedgerIndex() orelse {
        runtime.state.status_notice = .invalid_property;
        return;
    };
    const patch = session_state.patchPayload(runtime.session.transcript.at(idx)) orelse {
        runtime.state.status_notice = .invalid_property;
        return;
    };

    const name: []const u8 = if (runtime.state.pending_witness_focus) |focus|
        focus.property
    else blk: {
        const chip_name = property_goals.boolPropertyNameAt(runtime.state.selected_property_index) orelse {
            runtime.state.status_notice = .invalid_property;
            return;
        };
        if (!property_goals.isGoalDriveable(chip_name)) {
            runtime.state.status_notice = .invalid_property;
            return;
        }
        break :blk chip_name;
    };

    const focus_key: ?[]const u8 = if (runtime.state.pending_witness_focus) |f| f.key else null;

    const workspace_root = tools_common.workspaceRoot(allocator) catch {
        runtime.state.status_notice = .{ .goal_dispatch_error = name };
        return;
    };
    defer allocator.free(workspace_root);

    const goals = [_][]const u8{name};
    const outcome = autoloop.drive(allocator, registry, &runtime.session.transcript, .{
        .workspace_root = workspace_root,
        .file = patch.file,
        .goals = &goals,
        .events_path = runtime.session.events_path,
        .focus_witness_key = focus_key,
    }) catch {
        runtime.state.status_notice = .{ .goal_dispatch_error = name };
        return;
    };

    runtime.state.status_notice = .{ .goal_completed = .{ .name = name, .verdict = outcome.verdict } };
}

fn handleSubmit(
    runtime: *TuiRuntime,
    registry: *const Registry,
    flags: ExpertFlags,
    approval_fn: loop.ApprovalFn,
    submitted: []const u8,
) !bool {
    var outcome = repl.processSubmit(runtime.allocator, runtime.session, registry, submitted, approval_fn) catch |err| {
        var msg_buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&msg_buf, "error: {s}", .{@errorName(err)}) catch "error";
        try runtime.state.appendLocalText(runtime.allocator, submitted, false, msg);
        return false;
    };

    switch (outcome) {
        .noop => return false,
        .quit => return true,
        .rendered => |rendered| {
            defer runtime.allocator.free(rendered);
            runtime.state.focus_mode = .composer;
            return false;
        },
        .tool_result => |*result| {
            defer result.deinit(runtime.allocator);
            try runtime.state.appendLocalToolResult(runtime.allocator, submitted, result);
            return false;
        },
        .session_resume => {
            try agent.rebuildSession(
                runtime.allocator,
                runtime.session,
                registry,
                repl.baseSessionConfig(flags, .{ .resume_latest = true }),
            );
            runtime.state.clearLocalResults(runtime.allocator);
            try runtime.state.resetToTranscript(runtime.allocator, runtime.session, runtime.editor);
            if (runtime.session.session_id) |sid| {
                const msg = try std.fmt.allocPrint(runtime.allocator, "Resumed session {s}.", .{sid});
                defer runtime.allocator.free(msg);
                try runtime.state.appendLocalText(runtime.allocator, "/resume", true, msg);
            }
            return false;
        },
        .session_new => {
            try agent.rebuildSession(
                runtime.allocator,
                runtime.session,
                registry,
                repl.baseSessionConfig(flags, .{}),
            );
            runtime.state.clearLocalResults(runtime.allocator);
            try runtime.state.resetToTranscript(runtime.allocator, runtime.session, runtime.editor);
            if (runtime.session.session_id) |sid| {
                const msg = try std.fmt.allocPrint(runtime.allocator, "Started new session {s}.", .{sid});
                defer runtime.allocator.free(msg);
                try runtime.state.appendLocalText(runtime.allocator, "/new", true, msg);
            }
            return false;
        },
        .session_compact => {
            const msg = try agent.compact(runtime.allocator, runtime.session);
            defer runtime.allocator.free(msg);
            runtime.state.clearLocalResults(runtime.allocator);
            try runtime.state.resetToTranscript(runtime.allocator, runtime.session, runtime.editor);
            try runtime.state.appendLocalText(runtime.allocator, "/compact", true, msg);
            return false;
        },
        .session_fork => {
            const msg = try agent.fork(runtime.allocator, runtime.session);
            defer runtime.allocator.free(msg);
            try runtime.state.appendLocalText(runtime.allocator, "/fork", true, msg);
            return false;
        },
        .view_ledger => {
            runtime.state.view_mode = .ledger;
            runtime.state.focus_mode = .inspector;
            return false;
        },
        .view_chat => {
            runtime.state.view_mode = .chat;
            runtime.state.focus_mode = .inspector;
            return false;
        },
    }
}

fn selectApprovalFn(policy: loop.ApprovalPolicy, runtime: *TuiRuntime) loop.ApprovalFn {
    return switch (policy) {
        .auto_approve => loop.resolveApprovalFn(policy, null),
        .auto_reject => loop.resolveApprovalFn(policy, null),
        .ask => loop.resolveApprovalFn(policy, loop.ApprovalFn.withContext(runtime, approveEditWithContext)),
    };
}

fn approveEditWithContext(ctx: *anyopaque, file: []const u8) anyerror!bool {
    const runtime: *TuiRuntime = @ptrCast(@alignCast(ctx));
    runtime.state.modal = .{ .file = file };
    defer runtime.state.modal = null;
    try runtime.redrawFrame();

    var input_buf: [128]u8 = undefined;
    while (true) {
        const n = std.posix.read(std.posix.STDIN_FILENO, &input_buf) catch |err| switch (err) {
            error.WouldBlock => continue,
            else => return err,
        };
        if (n == 0) {
            runtime.state.modal = null;
            try runtime.redrawFrame();
            return false;
        }

        var i: usize = 0;
        while (i < n) {
            const event, const consumed = parseKeyEvent(input_buf[0..n], i);
            i += consumed;

            switch (handleModalEvent(runtime.state, event)) {
                .none => {},
                .redraw => try runtime.redrawFrame(),
                .decided => |decision| {
                    runtime.state.modal = null;
                    try runtime.redrawFrame();
                    return decision;
                },
            }
        }
    }
}

fn handleModalEvent(state: *AppState, event: KeyEvent) ModalAction {
    const modal = if (state.modal) |*m| m else return .none;

    return switch (event.kind) {
        .left, .shift_tab => blk: {
            modal.choice = .reject;
            break :blk .redraw;
        },
        .right, .tab => blk: {
            modal.choice = .approve;
            break :blk .redraw;
        },
        .enter => .{ .decided = modal.choice == .approve },
        .esc, .ctrl_c, .eof => .{ .decided = false },
        .char => switch (event.byte) {
            'y', 'Y' => .{ .decided = true },
            'n', 'N' => .{ .decided = false },
            else => .none,
        },
        else => .none,
    };
}

fn redraw(
    allocator: std.mem.Allocator,
    session: *const agent.AgentSession,
    state: *AppState,
) !void {
    const size = terminalSize();
    const layout = computeLayout(size);
    state.layout_mode = layout.mode;
    switch (state.view_mode) {
        .chat => clampFeedViewport(state, visiblePrimaryRows(layout)),
        .ledger => clampLedgerViewport(state, visiblePrimaryRows(layout)),
    }

    var inspector = try buildInspectorLines(allocator, state, session);
    defer inspector.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll(ansi.sync_begin);
    try w.writeAll(hide_cursor);
    try w.writeAll(clear_screen);

    try renderStatusRow(w, session, state, size.width);
    try writeCrlf(w);

    switch (layout.mode) {
        .split => try renderSplitBody(w, state, session, layout, inspector.lines),
        .stacked => try renderStackedBody(w, state, session, layout, inspector.lines),
    }

    try renderComposer(w, session, state, size.width);

    if (state.modal) |modal| {
        try renderModalOverlay(w, size, modal);
    } else if (state.focus_mode == .composer) {
        const row, const col = composerCursorPosition(layout, size.width, state.composer);
        try moveCursor(w, row, col);
        try w.writeAll(show_cursor);
    }

    try w.writeAll(ansi.sync_end);
    buf = aw.toArrayList();
    writeAll(buf.items);
}

fn renderStatusRow(
    w: *std.Io.Writer,
    session: *const agent.AgentSession,
    state: *const AppState,
    width: usize,
) !void {
    const descriptor = session.backendDescriptor();
    const model = session.currentModel() orelse "stub";
    const session_id = session.session_id orelse "ephemeral";
    const tok = session.token_totals;

    var notice_buf: [256]u8 = undefined;
    const notice = statusNoticeText(state.status_notice, &notice_buf);
    const base_args = .{
        session_id,
        descriptor.provider_label,
        descriptor.auth_label,
        model,
        tok.input_tokens,
        tok.output_tokens,
        tok.cache_read_input_tokens,
        tok.cache_creation_input_tokens,
        viewLabel(state.view_mode),
        focusLabel(state.focus_mode),
    };
    const base_fmt = "session {s} | provider {s}/{s} | model {s} | in {d} out {d} | cache {d}/{d} | view {s} | focus {s}";
    var line_buf: [1536]u8 = undefined;
    const line = if (notice.len == 0)
        std.fmt.bufPrint(&line_buf, base_fmt, base_args) catch "status"
    else
        std.fmt.bufPrint(&line_buf, base_fmt ++ " | {s}", base_args ++ .{notice}) catch "status";
    try writeFitted(w, line, width);
}

fn statusNoticeText(notice: StatusNotice, buf: *[256]u8) []const u8 {
    return switch (notice) {
        .none => "",
        .structural_property => "structural property - not goal-driveable; edit the source to change it.",
        .invalid_property => "invalid property selection",
        .goal_driving => |name| std.fmt.bufPrint(
            buf,
            "driving goal {s}... (autoloop in flight)",
            .{name},
        ) catch "driving goal",
        .witness_driving => |d| std.fmt.bufPrint(
            buf,
            "driving witness {s} on {s}... (autoloop in flight)",
            .{ d.short_key, d.property },
        ) catch "driving witness",
        .goal_completed => |c| std.fmt.bufPrint(
            buf,
            "autoloop {s} on goal {s}",
            .{ @tagName(c.verdict), c.name },
        ) catch "autoloop completed",
        .goal_dispatch_error => |name| std.fmt.bufPrint(
            buf,
            "autoloop dispatch failed for goal {s}",
            .{name},
        ) catch "autoloop dispatch failed",
        .replay_unavailable => "witness replay unavailable in this build",
        .replay_done => |d| std.fmt.bufPrint(
            buf,
            "replay {s} {s}",
            .{ if (d.reproduced) "PASS" else "FIXED", d.short_key },
        ) catch "replay finished",
        .replay_error => |text| std.fmt.bufPrint(
            buf,
            "replay error: {s}",
            .{text},
        ) catch "replay error",
        .mint_done => |key| std.fmt.bufPrint(
            buf,
            "minted regression test for {s}",
            .{key},
        ) catch "minted regression test",
        .mint_error => |text| std.fmt.bufPrint(
            buf,
            "mint error: {s}",
            .{text},
        ) catch "mint error",
    };
}

fn renderSplitBody(
    w: *std.Io.Writer,
    state: *const AppState,
    session: *const agent.AgentSession,
    layout: Layout,
    inspector_lines: []const []const u8,
) !void {
    var row: usize = 0;
    while (row < layout.body_height) : (row += 1) {
        try writePrimaryRow(w, state, session, row, layout.feed_width, visiblePrimaryRows(layout));
        try w.writeAll(" | ");
        try writeInspectorRow(w, state, row, inspector_lines, layout.inspector_width, state.focus_mode == .inspector);
        if (row + 1 < layout.body_height) try writeCrlf(w);
    }
    try writeCrlf(w);
}

fn renderStackedBody(
    w: *std.Io.Writer,
    state: *const AppState,
    session: *const agent.AgentSession,
    layout: Layout,
    inspector_lines: []const []const u8,
) !void {
    var row: usize = 0;
    while (row < layout.feed_height) : (row += 1) {
        try writePrimaryRow(w, state, session, row, layout.feed_width, visiblePrimaryRows(layout));
        try writeCrlf(w);
    }
    row = 0;
    while (row < layout.inspector_height) : (row += 1) {
        try writeInspectorRow(w, state, row, inspector_lines, layout.inspector_width, state.focus_mode == .inspector);
        if (row + 1 < layout.inspector_height) try writeCrlf(w);
    }
    try writeCrlf(w);
}

fn renderComposer(
    w: *std.Io.Writer,
    session: *const agent.AgentSession,
    state: *const AppState,
    width: usize,
) !void {
    _ = session;
    var header_buf: [256]u8 = undefined;
    const header = std.fmt.bufPrint(
        &header_buf,
        "composer {s} | Tab next pane | Shift-Tab previous | Esc composer",
        .{if (state.focus_mode == .composer) "*" else ""},
    ) catch "composer";
    try writeFitted(w, header, width);
    try writeCrlf(w);

    const available = width -| prompt_label.len;
    const composer_view = visibleComposer(state.composer.line, state.composer.cursor, available);

    if (width <= prompt_label.len) {
        try writeFitted(w, prompt_label, width);
    } else {
        try w.writeAll(prompt_label);
        try w.writeAll(composer_view.visible);
        try writeSpaces(w, available - composer_view.visible.len);
    }
    try writeCrlf(w);

    const hints = switch (state.view_mode) {
        .chat => switch (state.focus_mode) {
            .composer => "Enter submits. Up/Down keep line history. Left/Right edit within the composer. /ledger switches views.",
            .feed => "Up/Down move the transcript selection. Enter opens the inspector. Esc returns to the composer. l opens the ledger.",
            .inspector => "Up/Down still move the transcript selection. Esc returns to the composer. l opens the ledger.",
        },
        .ledger => switch (state.focus_mode) {
            .composer => "Enter submits. /chat switches back to transcript view.",
            .feed, .inspector => "Up/Down move the patch rail. Tab cycles detail tabs. Left/Right select property chips. g drives the selected goal green. A approves when witness state is clean. c opens chat.",
        },
    };
    try writeFitted(w, hints, width);
}

fn renderModalOverlay(w: *std.Io.Writer, size: TerminalSize, approval: ApprovalModal) !void {
    const width = @min(size.width -| 4, @as(usize, 64));
    const left_col = if (size.width > width) (size.width - width) / 2 + 1 else 1;
    const top_row = if (size.height > 7) (size.height - 5) / 2 + 1 else 1;

    var title_buf: [256]u8 = undefined;
    const title = std.fmt.bufPrint(&title_buf, "Apply verified edit to {s}?", .{approval.file}) catch "Apply verified edit?";

    const reject_style = if (approval.choice == .reject) "7" else "";
    const approve_style = if (approval.choice == .approve) "7" else "";

    try writeModalLine(w, top_row, left_col, width, "+--------------------------------------------------------------+");
    try writeModalLine(w, top_row + 1, left_col, width, "| approval required                                            |");
    try writeModalLine(w, top_row + 2, left_col, width, title);
    try moveCursor(w, top_row + 3, left_col);
    try writeFitted(w, "| ", 2);
    try ansi.styled(w, reject_style, " reject ");
    try w.writeAll("   ");
    try ansi.styled(w, approve_style, " approve ");
    const used = 2 + @as(usize, 8) + 3 + @as(usize, 9);
    try writeSpaces(w, width -| used);
    try writeCrlf(w);
    try writeModalLine(w, top_row + 4, left_col, width, "y/n or Enter. Tab switches choice. Esc rejects.");
}

fn writeModalLine(
    w: *std.Io.Writer,
    row: usize,
    col: usize,
    width: usize,
    text: []const u8,
) !void {
    try moveCursor(w, row, col);
    try ansi.sgr(w, "1");
    try writeFitted(w, text, width);
    try w.writeAll(ansi.reset);
}

fn writePrimaryRow(
    w: *std.Io.Writer,
    state: *const AppState,
    session: *const agent.AgentSession,
    row: usize,
    width: usize,
    visible_items: usize,
) !void {
    switch (state.view_mode) {
        .chat => try writeFeedRow(w, state, session, row, width, visible_items),
        .ledger => try writeLedgerRow(w, state, session, row, width, visible_items),
    }
}

fn writeFeedRow(
    w: *std.Io.Writer,
    state: *const AppState,
    session: *const agent.AgentSession,
    row: usize,
    width: usize,
    visible_items: usize,
) !void {
    if (row == 0) {
        var header_buf: [256]u8 = undefined;
        const header = std.fmt.bufPrint(
            &header_buf,
            "feed {s} | items {d} | selected {d}/{d}",
            .{
                if (state.focus_mode == .feed) "*" else "",
                state.feed_items.items.len,
                if (state.feed_items.items.len == 0) 0 else state.selected_feed_index + 1,
                state.feed_items.items.len,
            },
        ) catch "feed";
        try writeFitted(w, header, width);
        return;
    }

    const item_index = state.feed_scroll + (row - 1);
    if (item_index >= state.feed_items.items.len or visible_items == 0) {
        try writeFitted(w, "", width);
        return;
    }

    var line_buf: [1024]u8 = undefined;
    const line = buildFeedLine(&line_buf, state.feed_items.items[item_index], state, session);
    if (item_index == state.selected_feed_index) {
        try ansi.sgr(w, "7");
        try writeFitted(w, line, width);
        try w.writeAll(ansi.reset);
    } else {
        try writeFitted(w, line, width);
    }
}

fn writeLedgerRow(
    w: *std.Io.Writer,
    state: *const AppState,
    session: *const agent.AgentSession,
    row: usize,
    width: usize,
    visible_items: usize,
) !void {
    if (row == 0) {
        var header_buf: [256]u8 = undefined;
        const header = std.fmt.bufPrint(
            &header_buf,
            "ledger {s} | patches {d} | selected {d}/{d}",
            .{
                if (state.focus_mode != .composer) "*" else "",
                state.ledger_items.items.len,
                if (state.ledger_items.items.len == 0) 0 else state.selected_ledger_index + 1,
                state.ledger_items.items.len,
            },
        ) catch "ledger";
        try writeFitted(w, header, width);
        return;
    }

    const item_index = state.ledger_scroll + (row - 1);
    if (item_index >= state.ledger_items.items.len or visible_items == 0) {
        try writeFitted(w, "", width);
        return;
    }

    var line_buf: [1024]u8 = undefined;
    const line = buildLedgerLine(&line_buf, state, session, item_index);
    if (item_index == state.selected_ledger_index) {
        try ansi.sgr(w, "7");
        try writeFitted(w, line, width);
        try w.writeAll(ansi.reset);
    } else {
        try writeFitted(w, line, width);
    }
}

fn buildLedgerLine(
    out: *[1024]u8,
    state: *const AppState,
    session: *const agent.AgentSession,
    ledger_index: usize,
) []const u8 {
    var fw = std.Io.Writer.fixed(out);
    const w = &fw;
    const transcript_index = state.ledger_items.items[ledger_index];
    const entry = session.transcript.at(transcript_index);
    switch (entry.*) {
        .verified_patch => |message| {
            if (message.ui_payload) |payload| switch (payload) {
                .verified_patch => |patch| {
                    w.print("{s} | {s} | new {d}", .{
                        patch.file,
                        if (patch.prove) |prove| prove.classification else "unclassified",
                        patch.stats.new,
                    }) catch {};
                    if (!patch.post_apply_ok) {
                        w.writeAll(" | post-apply warn") catch {};
                    }
                },
                else => w.writeAll(firstLine(message.llm_text)) catch {},
            } else {
                w.writeAll(firstLine(message.llm_text)) catch {};
            }
        },
        else => w.writeAll("(not a patch)") catch {},
    }
    return fw.buffered();
}

fn writeInspectorRow(
    w: *std.Io.Writer,
    state: *const AppState,
    row: usize,
    inspector_lines: []const []const u8,
    width: usize,
    focused: bool,
) !void {
    if (row == 0) {
        var header_buf: [256]u8 = undefined;
        const header = std.fmt.bufPrint(
            &header_buf,
            "{s} {s}",
            .{
                switch (state.view_mode) {
                    .chat => "inspector",
                    .ledger => ledgerTabLabel(state.ledger_tab),
                },
                if (focused) "*" else "",
            },
        ) catch "inspector";
        try writeFitted(w, header, width);
        return;
    }

    const line_index = row - 1;
    if (line_index >= inspector_lines.len) {
        try writeFitted(w, "", width);
        return;
    }
    try writeFitted(w, inspector_lines[line_index], width);
}

fn buildFeedLine(
    out: *[1024]u8,
    item: FeedItem,
    state: *const AppState,
    session: *const agent.AgentSession,
) []const u8 {
    var fw = std.Io.Writer.fixed(out);
    const w = &fw;
    switch (item.source) {
        .transcript => |index| {
            const entry = session.transcript.at(index);
            switch (entry.*) {
                .user_text => |body| {
                    w.writeAll("user: ") catch {};
                    w.writeAll(firstLine(body)) catch {};
                },
                .model_text => |body| {
                    w.writeAll("model: ") catch {};
                    w.writeAll(firstLine(body)) catch {};
                },
                .assistant_tool_use => |calls| {
                    if (calls.len == 0) {
                        w.writeAll("tools: (empty)") catch {};
                    } else {
                        w.writeAll("tools: ") catch {};
                        for (calls, 0..) |call, i| {
                            if (i > 0) w.writeAll(", ") catch {};
                            w.writeAll(call.name) catch {};
                        }
                    }
                },
                .tool_result => |result| {
                    w.print("{s} {s}: {s}", .{
                        if (result.ok) "ok" else "err",
                        result.tool_name,
                        summaryText(result.llm_text, result.ui_payload),
                    }) catch {};
                },
                .proof_card => |message| {
                    w.writeAll("proof: ") catch {};
                    w.writeAll(summaryText(message.llm_text, message.ui_payload)) catch {};
                },
                .diagnostic_box => |message| {
                    w.writeAll("diag: ") catch {};
                    w.writeAll(summaryText(message.llm_text, message.ui_payload)) catch {};
                },
                .verified_patch => |message| {
                    w.writeAll("patch: ") catch {};
                    w.writeAll(summaryText(message.llm_text, message.ui_payload)) catch {};
                },
                .system_note => |body| {
                    w.writeAll("note: ") catch {};
                    w.writeAll(firstLine(body)) catch {};
                },
            }
        },
        .local_result => |index| {
            const result = &state.local_results.items[index];
            w.print("{s}: {s}", .{ result.title, summaryText(result.llm_text, result.ui_payload) }) catch {};
        },
    }
    return fw.buffered();
}

fn buildInspectorLines(
    allocator: std.mem.Allocator,
    state: *const AppState,
    session: *const agent.AgentSession,
) !OwnedLines {
    const text = try buildInspectorText(allocator, state, session);
    errdefer allocator.free(text);

    var lines = std.ArrayList([]const u8).empty;
    errdefer lines.deinit(allocator);

    var start: usize = 0;
    var i: usize = 0;
    while (i < text.len) : (i += 1) {
        if (text[i] == '\n') {
            const end = if (i > start and text[i - 1] == '\r') i - 1 else i;
            try lines.append(allocator, text[start..end]);
            start = i + 1;
        }
    }
    if (start < text.len) {
        try lines.append(allocator, text[start..]);
    }

    return .{
        .storage = text,
        .lines = try lines.toOwnedSlice(allocator),
    };
}

fn buildInspectorText(
    allocator: std.mem.Allocator,
    state: *const AppState,
    session: *const agent.AgentSession,
) ![]u8 {
    if (state.view_mode == .ledger) {
        return buildLedgerText(allocator, state, session);
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    if (state.selectedFeedItem()) |item| {
        switch (item.source) {
            .transcript => |index| {
                const entry = session.transcript.at(index);
                try writeTranscriptInspector(w, entry);
            },
            .local_result => |index| {
                try writeLocalInspector(w, &state.local_results.items[index]);
            },
        }
    } else {
        try w.writeAll("No feed items yet.\n\nType a request or run /status, /model, or /tree.");
    }

    buf = aw.toArrayList();
    return try buf.toOwnedSlice(allocator);
}

fn buildLedgerText(
    allocator: std.mem.Allocator,
    state: *const AppState,
    session: *const agent.AgentSession,
) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    const transcript_index = state.selectedLedgerIndex() orelse {
        try w.writeAll("No verified patches yet.\n\nApproved edits will populate this rail.\nRun /chat to inspect the live transcript.");
        buf = aw.toArrayList();
        return try buf.toOwnedSlice(allocator);
    };

    const entry = session.transcript.at(transcript_index);
    switch (entry.*) {
        .verified_patch => |message| {
            if (message.ui_payload) |payload| switch (payload) {
                .verified_patch => |patch| try writeLedgerPatchPanel(
                    w,
                    patch,
                    state.ledger_tab,
                    state.diff_expanded,
                    state.selected_property_index,
                    .{
                        .selected_index = state.selected_witness_index,
                        .verdict = state.witness_verdict,
                    },
                ),
                else => try writeTextBlock(w, message.llm_text),
            } else {
                try writeTextBlock(w, message.llm_text);
            }
        },
        else => try w.writeAll("Selected ledger entry is unavailable."),
    }

    buf = aw.toArrayList();
    return try buf.toOwnedSlice(allocator);
}

fn writeTranscriptInspector(w: *std.Io.Writer, entry: *const transcript_mod.OwnedEntry) !void {
    switch (entry.*) {
        .user_text => |body| try writeTextBlock(w, body),
        .model_text => |body| try writeTextBlock(w, body),
        .system_note => |body| try writeTextBlock(w, body),
        .assistant_tool_use => |calls| {
            try w.writeAll("tool calls\n\n");
            for (calls) |call| {
                try w.print("id: {s}\nname: {s}\nargs: {s}\n\n", .{ call.id, call.name, call.args_json });
            }
        },
        .tool_result => |result| {
            try w.print("tool result: {s}\nstatus: {s}\n\n", .{
                result.tool_name,
                if (result.ok) "ok" else "error",
            });
            try writePayloadOrText(w, result.llm_text, result.ui_payload);
        },
        .proof_card => |message| {
            try w.writeAll("proof\n\n");
            try writePayloadOrText(w, message.llm_text, message.ui_payload);
        },
        .diagnostic_box => |message| {
            try w.writeAll("diagnostics\n\n");
            try writePayloadOrText(w, message.llm_text, message.ui_payload);
        },
        .verified_patch => |message| {
            try w.writeAll("verified patch\n\n");
            try writePayloadOrText(w, message.llm_text, message.ui_payload);
        },
    }
}

fn writeLocalInspector(w: *std.Io.Writer, result: *const LocalResult) !void {
    try w.print("command: {s}\nstatus: {s}\n\n", .{
        result.title,
        if (result.ok) "ok" else "error",
    });
    try writePayloadOrText(w, result.llm_text, result.ui_payload);
}

fn writePayloadOrText(
    w: *std.Io.Writer,
    llm_text: []const u8,
    payload: ?UiPayload,
) !void {
    if (payload) |value| {
        switch (value) {
            .plain_text => |text| try writeTextBlock(w, text),
            .diagnostics => |diag| try writeDiagnosticsPayload(w, diag),
            .proof_card => |proof| try writeProofCardPayload(w, proof),
            .command_outcome => |outcome| try writeCommandOutcomePayload(w, outcome),
            .repair_candidate => |candidate| try writeRepairCandidatePayload(w, candidate),
            .session_tree => |tree| try writeSessionTreePayload(w, tree),
            .verified_patch => |patch| try writeVerifiedPatchPayload(w, patch),
        }
        return;
    }
    try writeTextBlock(w, llm_text);
}

fn writeDiagnosticsPayload(
    w: *std.Io.Writer,
    payload: ui_payload_mod.DiagnosticsPayload,
) !void {
    try w.writeAll(payload.summary);
    try w.writeAll("\n\n");
    for (payload.items, 0..) |item, i| {
        try w.print(
            "{d}. {s} {s} {s}:{d}:{d}\n   {s}",
            .{ i + 1, item.severity, item.code, item.path, item.line, item.column, item.message },
        );
        if (item.introduced_by_patch) |flag| {
            try w.print("\n   introduced_by_patch: {s}", .{if (flag) "true" else "false"});
        }
        try w.writeAll("\n\n");
    }
}

fn writeProofCardPayload(
    w: *std.Io.Writer,
    payload: ui_payload_mod.ProofCardPayload,
) !void {
    try w.print("title: {s}\nsummary: {s}\nstats: total={d} new={d}", .{
        payload.title,
        payload.summary,
        payload.stats.total,
        payload.stats.new,
    });
    if (payload.stats.preexisting) |count| {
        try w.print(" preexisting={d}", .{count});
    }
    try w.writeAll("\n");
    if (payload.highlights.len > 0) {
        try w.writeAll("\nhighlights:\n");
        for (payload.highlights) |highlight| {
            try w.print("- {s}\n", .{highlight});
        }
    }
}

fn writeRepairCandidatePayload(
    w: *std.Io.Writer,
    payload: ui_payload_mod.RepairCandidatePayload,
) !void {
    try w.print(
        "path: {s}\nplan_id: {s}\nintent: {s}\nverification_ok: {s}\nstats: total={d} new={d}",
        .{
            payload.path,
            payload.plan_id,
            payload.intent_kind,
            if (payload.verification_ok) "true" else "false",
            payload.stats.total,
            payload.stats.new,
        },
    );
    if (payload.stats.preexisting) |count| {
        try w.print(" preexisting={d}", .{count});
    }
    try w.print("\nsummary: {s}\n\nproposed_content:\n", .{payload.verification_summary});
    try writeTextBlock(w, payload.proposed_content);
}

fn writeVerifiedPatchPayload(
    w: *std.Io.Writer,
    payload: ui_payload_mod.VerifiedPatchPayload,
) !void {
    try w.print("file: {s}\npolicy_hash: {s}\nstats: total={d} new={d}", .{
        payload.file,
        payload.policy_hash,
        payload.stats.total,
        payload.stats.new,
    });
    if (payload.stats.preexisting) |count| {
        try w.print(" preexisting={d}", .{count});
    }
    try w.print("\npost_apply_ok: {s}\n", .{if (payload.post_apply_ok) "true" else "false"});
    if (payload.post_apply_summary) |note| {
        try w.print("post_apply: {s}\n", .{note});
    }
    if (payload.after_properties) |p| {
        try w.writeAll("\nproperties:\n");
        inline for (@typeInfo(ui_payload_mod.PropertiesSnapshot).@"struct".fields) |field| {
            switch (@typeInfo(field.type)) {
                .bool => try w.print("- {s}: {s}\n", .{
                    field.name,
                    if (@field(p, field.name)) "true" else "false",
                }),
                .optional => {
                    if (@field(p, field.name)) |number| {
                        try w.print("- {s}: {d}\n", .{ field.name, number });
                    } else {
                        try w.print("- {s}: null\n", .{field.name});
                    }
                },
                else => @compileError("unsupported PropertiesSnapshot field type"),
            }
        }
    }
}

fn writeLedgerPatchPanel(
    w: *std.Io.Writer,
    payload: ui_payload_mod.VerifiedPatchPayload,
    tab: LedgerTab,
    diff_expanded: bool,
    selected_property_index: usize,
    witness_view: WitnessView,
) !void {
    try w.print(
        "file: {s}\napplied_at_unix_ms: {d}\npolicy_hash: {s}\nstats: total={d} new={d} preexisting={d}\npost_apply_ok: {s}\n",
        .{
            payload.file,
            payload.applied_at_unix_ms,
            payload.policy_hash,
            payload.stats.total,
            payload.stats.new,
            payload.stats.preexisting orelse 0,
            if (payload.post_apply_ok) "true" else "false",
        },
    );
    if (payload.patch_hash) |hash| {
        try w.writeAll("patch_hash: ");
        try writeShortHash(w, hash);
        try w.writeByte('\n');
    }
    if (payload.parent_hash) |hash| {
        try w.writeAll("parent_hash: ");
        try writeShortHash(w, hash);
        try w.writeByte('\n');
    }
    if (payload.post_apply_summary) |summary| {
        try w.print("post_apply_summary: {s}\n", .{summary});
    }
    try w.writeAll("\n");

    switch (tab) {
        .delta => try writeProofDeltaCard(w, payload, diff_expanded),
        .diff => {
            try w.writeAll("Diff\n\n");
            if (payload.hunks.len == 0) {
                try w.writeAll("No content delta recorded.");
                return;
            }
            if (diff_expanded) {
                try writeTextBlock(w, payload.unified_diff);
            } else {
                for (payload.hunks, 0..) |hunk, i| {
                    try w.print(
                        "{d}. @@ -{d},{d} +{d},{d} @@\n",
                        .{ i + 1, hunk.old_start, hunk.old_count, hunk.new_start, hunk.new_count },
                    );
                }
                try w.writeAll("\nPress Enter to expand the full diff.");
            }
        },
        .properties => {
            try w.writeAll("Properties\n\nbefore:\n");
            try writePropertiesSnapshot(w, payload.before_properties, null);
            try w.writeAll("\nafter:\n");
            try writePropertiesSnapshot(w, payload.after_properties, selected_property_index);
        },
        .violations => {
            try w.writeAll("Violations\n\n");
            if (payload.violations.len == 0) {
                try w.writeAll("No violations recorded.");
                return;
            }
            for (payload.violations, 0..) |violation, i| {
                try w.print(
                    "{d}. {s} {s} line={d} col={d} introduced={s}\n   {s}\n   key={s}\n\n",
                    .{
                        i + 1,
                        violation.severity,
                        violation.code,
                        violation.line,
                        violation.column,
                        if (violation.introduced_by_patch) "true" else "false",
                        violation.message,
                        violation.stable_key,
                    },
                );
            }
        },
        .prove => {
            try w.writeAll("Prove\n\n");
            if (payload.prove) |prove| {
                try w.print(
                    "classification: {s}\nproof_level: {s}\nrecommendation: {s}\n",
                    .{ prove.classification, prove.proof_level, prove.recommendation },
                );
                if (prove.counterexample) |counterexample| {
                    try w.print("counterexample: {s}\n", .{counterexample});
                }
                if (prove.laws_used.len > 0) {
                    try w.writeAll("laws_used:\n");
                    for (prove.laws_used) |law| try w.print("- {s}\n", .{law});
                }
            } else {
                try w.writeAll("No contract-pair proof was available for this patch.");
            }
        },
        .system => {
            try w.writeAll("System\n\n");
            if (payload.system) |system| {
                try w.print(
                    "system_path: {s}\nproof_level: {s}\nall_links_resolved: {s}\nall_responses_covered: {s}\npayload_compatible: {s}\ninjection_safe: {s}\nno_secret_leakage: {s}\nno_credential_leakage: {s}\nretry_safe: {s}\nfault_covered: {s}\nstate_isolated: {s}\ndynamic_links: {d}\n",
                    .{
                        system.system_path,
                        system.proof_level,
                        if (system.all_links_resolved) "true" else "false",
                        if (system.all_responses_covered) "true" else "false",
                        if (system.payload_compatible) "true" else "false",
                        if (system.injection_safe) "true" else "false",
                        if (system.no_secret_leakage) "true" else "false",
                        if (system.no_credential_leakage) "true" else "false",
                        if (system.retry_safe) "true" else "false",
                        if (system.fault_covered) "true" else "false",
                        if (system.state_isolated) "true" else "false",
                        system.dynamic_links,
                    },
                );
                if (system.max_system_io_depth) |depth| {
                    try w.print("max_system_io_depth: {d}\n", .{depth});
                }
                if (system.warnings.len > 0) {
                    try w.writeAll("warnings:\n");
                    for (system.warnings) |warning| try w.print("- {s}\n", .{warning});
                }
            } else {
                try w.writeAll("No system proof was available for this patch.");
            }
        },
        .citations => {
            try w.writeAll("Citations\n\n");
            if (payload.rule_citations.len == 0) {
                try w.writeAll("No cited rule codes were captured for this turn.");
                return;
            }
            for (payload.rule_citations) |citation| {
                try w.print("- {s}\n", .{citation});
            }
        },
        .witnesses => try writeWitnessesTab(w, payload, witness_view),
    }
}

const WitnessView = struct {
    selected_index: usize,
    verdict: ?WitnessVerdictRecord,
};

/// Render the counterexample witness diff carried on this patch.
/// Each witness shows the synthesised request, the IO stub script that
/// pins virtual-module return values, the property tag it violates, and
/// the origin/sink source spans. Defeated witnesses are the ones the
/// patch closed; new witnesses are the ones it introduced. The selected
/// witness is marked with `>` and, when a replay verdict is available,
/// the verdict (PASS/FIXED + actual response) renders inline below it.
fn writeWitnessesTab(
    w: *std.Io.Writer,
    payload: ui_payload_mod.VerifiedPatchPayload,
    view: WitnessView,
) !void {
    try w.writeAll("Witnesses (r=replay selected, up/down=select)\n\n");
    try writeWitnessSection(w, "defeated by this patch", payload.witnesses_defeated, 0, view);
    try w.writeByte('\n');
    try writeWitnessSection(
        w,
        "introduced by this patch",
        payload.witnesses_new,
        payload.witnesses_defeated.len,
        view,
    );
}

fn writeWitnessSection(
    w: *std.Io.Writer,
    title: []const u8,
    bodies: []const ui_payload_mod.WitnessBody,
    flat_offset: usize,
    view: WitnessView,
) !void {
    try w.print("{s} ({d})\n", .{ title, bodies.len });
    if (bodies.len == 0) {
        try w.writeAll("  (none)\n");
        return;
    }
    for (bodies, 0..) |body, i| {
        const selected = (flat_offset + i) == view.selected_index;
        try writeWitnessBody(w, i + 1, body, selected);
        if (selected) try writeVerdictInline(w, view.verdict, body);
    }
}

fn writeWitnessBody(
    w: *std.Io.Writer,
    index: usize,
    body: ui_payload_mod.WitnessBody,
    selected: bool,
) !void {
    const marker: u8 = if (selected) '>' else ' ';
    try w.print("  {c} {d}. [{s}]\n", .{ marker, index, body.property });
    try w.print("     {s} {s}", .{ body.request_method, body.request_url });
    if (body.request_has_auth) try w.writeAll("  (auth)");
    if (body.request_body) |b| try w.print("  body={s}", .{firstLine(b)});
    try w.writeByte('\n');
    try w.print(
        "     origin {d}:{d} -> sink {d}:{d}\n",
        .{ body.origin_line, body.origin_column, body.sink_line, body.sink_column },
    );
    if (body.summary.len > 0) {
        try w.print("     summary: {s}\n", .{body.summary});
    }
    try w.print("     key: {s}\n", .{shortKey(body.key)});
    if (body.io_stubs.len == 0) {
        try w.writeAll("     io_stubs: (none)\n");
    } else {
        try w.writeAll("     io_stubs:\n");
        for (body.io_stubs) |stub| {
            try w.print(
                "       {d}. {s}.{s}() -> {s}\n",
                .{ stub.seq, stub.module, stub.func, stub.result_json },
            );
        }
    }
    try w.writeByte('\n');
}

fn writeVerdictInline(
    w: *std.Io.Writer,
    verdict_opt: ?WitnessVerdictRecord,
    body: ui_payload_mod.WitnessBody,
) !void {
    const record = verdict_opt orelse return;
    if (!std.mem.eql(u8, record.key, body.key)) return;

    try w.writeAll("     replay: ");
    if (!record.verdict.ran) {
        try w.writeAll("ERROR");
        if (record.verdict.error_text) |t| try w.print(" ({s})", .{t});
        try w.writeByte('\n');
        return;
    }
    if (record.reproduced) {
        try w.writeAll("PASS - violation reproduced");
    } else {
        try w.writeAll("FIXED - violation no longer reproduces");
    }
    try w.print(" (status {d})\n", .{record.verdict.actual_status});
    if (record.verdict.actual_body.len > 0) {
        try w.writeAll("     actual: ");
        try writeBodyExcerpt(w, record.verdict.actual_body, 160);
        try w.writeByte('\n');
    }
}

fn writeBodyExcerpt(w: *std.Io.Writer, body: []const u8, max_len: usize) !void {
    const limit = @min(body.len, max_len);
    var i: usize = 0;
    while (i < limit) : (i += 1) {
        const c = body[i];
        if (c == '\n') {
            try w.writeAll("\\n");
        } else if (c < 0x20) {
            try w.writeByte('?');
        } else {
            try w.writeByte(c);
        }
    }
    if (body.len > max_len) try w.writeAll("...");
}

fn shortKey(key: []const u8) []const u8 {
    if (key.len <= 12) return key;
    return key[0..12];
}

/// Proof-first summary renderer. Property delta badges, violations before -> after,
/// witness lifecycle counts, chain metadata, collapsed diff pointer. The badges are
/// the review primitive here: trust the delta, press `d` (or Tab + Enter on .diff)
/// only when a change needs line-level inspection.
fn writeProofDeltaCard(
    w: *std.Io.Writer,
    payload: ui_payload_mod.VerifiedPatchPayload,
    diff_expanded: bool,
) !void {
    try w.writeAll("Proof Delta\n\n");

    if (payload.patch_hash) |hash| {
        try w.writeAll("patch  ");
        try writeShortHash(w, hash);
        if (payload.parent_hash) |parent| {
            try w.writeAll("  <- ");
            try writeShortHash(w, parent);
        }
        try w.writeByte('\n');
    }

    if (payload.goal_context.len > 0) {
        try w.writeAll("goals  ");
        for (payload.goal_context, 0..) |goal, i| {
            if (i > 0) try w.writeAll(", ");
            try w.writeAll(goal);
        }
        try w.writeByte('\n');
    }

    try w.writeByte('\n');
    try w.writeAll("properties\n");
    try writePropertyDelta(w, payload.before_properties, payload.after_properties);

    try w.writeByte('\n');
    try w.print("violations  {d} total", .{payload.stats.total});
    if (payload.stats.preexisting) |pre| {
        try w.print("  ({d} new, {d} preexisting)", .{ payload.stats.new, pre });
    } else {
        try w.print("  ({d} new)", .{payload.stats.new});
    }
    try w.writeByte('\n');

    try w.print("witnesses   {d} defeated", .{payload.witnesses_defeated.len});
    if (payload.witnesses_new.len > 0) {
        try w.print(", ", .{});
        try ansi.sgr(w, "31");
        try w.print("{d} new", .{payload.witnesses_new.len});
        try w.writeAll(ansi.reset);
    }
    try w.writeByte('\n');

    if (payload.post_apply_summary) |summary| {
        try w.writeByte('\n');
        try w.print("note  {s}\n", .{summary});
    }

    try w.writeByte('\n');
    if (payload.hunks.len == 0) {
        try w.writeAll("diff  (no content delta)\n");
    } else if (diff_expanded) {
        try w.writeAll("diff\n");
        try writeTextBlock(w, payload.unified_diff);
    } else {
        try w.print("diff  {d} hunk{s} (Enter to expand)\n", .{
            payload.hunks.len,
            if (payload.hunks.len == 1) "" else "s",
        });
    }
}

/// Emit SGR-coloured property-change badges: green `+name` for promotions
/// (false -> true), red `-name` for demotions. Unchanged fields are
/// suppressed so the eye lands on what this patch actually did.
fn writePropertyDelta(
    w: *std.Io.Writer,
    before_opt: ?ui_payload_mod.PropertiesSnapshot,
    after_opt: ?ui_payload_mod.PropertiesSnapshot,
) !void {
    const after = after_opt orelse {
        try w.writeAll("  (not computed)\n");
        return;
    };

    const Visitor = struct {
        w: *std.Io.Writer,
        wrote: bool = false,

        pub fn visit(self: *@This(), change: ui_payload_mod.PropertiesSnapshot.Change) !void {
            // Leading "  " for the first badge acts as the indent; subsequent
            // "  " acts as the separator. Both look the same on the wire.
            try self.w.writeAll("  ");
            switch (change.kind) {
                .promoted => try ansi.sgr(self.w, "32"),
                .demoted => try ansi.sgr(self.w, "31"),
            }
            const prefix: u8 = switch (change.kind) {
                .promoted => '+',
                .demoted => '-',
            };
            try self.w.print("{c}{s}", .{ prefix, change.name });
            try self.w.writeAll(ansi.reset);
            self.wrote = true;
        }
    };

    var visitor: Visitor = .{ .w = w };
    try ui_payload_mod.PropertiesSnapshot.forEachChange(before_opt, after, *Visitor, &visitor);

    if (!visitor.wrote) try w.writeAll("  (no property changes)");
    try w.writeByte('\n');
}

fn writeShortHash(w: *std.Io.Writer, hash: [32]u8) !void {
    const hex = std.fmt.bytesToHex(hash, .lower);
    try w.writeAll(hex[0..12]);
}

fn writePropertiesSnapshot(
    w: *std.Io.Writer,
    snapshot: ?ui_payload_mod.PropertiesSnapshot,
    cursor: ?usize,
) !void {
    const value = snapshot orelse {
        try w.writeAll("(none)\n");
        return;
    };

    var bool_index: usize = 0;
    inline for (@typeInfo(ui_payload_mod.PropertiesSnapshot).@"struct".fields) |field| {
        switch (@typeInfo(field.type)) {
            .bool => {
                const truthy = if (@field(value, field.name)) "true" else "false";
                if (cursor) |selected_index| {
                    const marker: u8 = if (bool_index == selected_index) '>' else ' ';
                    const lane = property_goals.classify(field.name).label();
                    try w.print("{c} [{s}] {s} ({s})\n", .{ marker, truthy, field.name, lane });
                } else {
                    try w.print("- {s}: {s}\n", .{ field.name, truthy });
                }
                bool_index += 1;
            },
            .optional => {
                if (cursor == null) {
                    if (@field(value, field.name)) |number| {
                        try w.print("- {s}: {d}\n", .{ field.name, number });
                    } else {
                        try w.print("- {s}: null\n", .{field.name});
                    }
                } else if (comptime std.mem.eql(u8, field.name, "max_io_depth")) {
                    if (@field(value, field.name)) |number| {
                        try w.print("  {s}: {d} (metric)\n", .{ field.name, number });
                    } else {
                        try w.print("  {s}: null (metric)\n", .{field.name});
                    }
                }
            },
            else => @compileError("unsupported PropertiesSnapshot field type"),
        }
    }
}

fn writeCommandOutcomePayload(
    w: *std.Io.Writer,
    payload: ui_payload_mod.CommandOutcomePayload,
) !void {
    try w.print("title: {s}\ncommand: {s}\nexit_code: ", .{
        payload.title,
        payload.command,
    });
    if (payload.exit_code) |code| {
        try w.print("{d}\n", .{code});
    } else {
        try w.writeAll("none\n");
    }
    try w.writeAll("\nstdout:\n");
    if (payload.stdout.len == 0) {
        try w.writeAll("(empty)\n");
    } else {
        try writeTextBlock(w, payload.stdout);
    }
    try w.writeAll("\nstderr:\n");
    if (payload.stderr.len == 0) {
        try w.writeAll("(empty)\n");
    } else {
        try writeTextBlock(w, payload.stderr);
    }
}

fn writeSessionTreePayload(
    w: *std.Io.Writer,
    payload: ui_payload_mod.SessionTreePayload,
) !void {
    if (payload.nodes.len == 0) {
        try w.writeAll("No sessions.");
        return;
    }
    for (payload.nodes) |node| {
        var depth: usize = 0;
        while (depth < node.depth) : (depth += 1) {
            try w.writeAll("  ");
        }
        try w.writeAll(if (node.is_current) "* " else "- ");
        try w.writeAll(node.session_id);
        try w.print("  created={d}", .{@divTrunc(node.created_at_unix_ms, 1000)});
        if (node.is_orphan_root and node.parent_id != null) {
            try w.print("  orphaned-from={s}", .{node.parent_id.?});
        }
        try w.writeAll("\n");
    }
}

fn writeTextBlock(w: *std.Io.Writer, text: []const u8) !void {
    try w.writeAll(text);
    if (text.len == 0 or text[text.len - 1] != '\n') try w.writeAll("\n");
}

fn terminalSize() TerminalSize {
    var winsize: std.posix.winsize = .{
        .row = 0,
        .col = 0,
        .xpixel = 0,
        .ypixel = 0,
    };
    if (std.c.ioctl(std.c.STDOUT_FILENO, std.c.T.IOCGWINSZ, &winsize) == 0 and winsize.col > 0 and winsize.row > 0) {
        return .{
            .width = @intCast(winsize.col),
            .height = @intCast(winsize.row),
        };
    }
    return .{ .width = 80, .height = 25 };
}

fn computeLayout(size: TerminalSize) Layout {
    const composer_height: usize = 3;
    const status_height: usize = 1;
    const body_height = @max(@as(usize, 1), size.height -| (status_height + composer_height));

    if (size.width >= 100 and body_height >= 8) {
        const inspector_width = @min(@max(@as(usize, 32), size.width / 3), size.width -| 24);
        return .{
            .mode = .split,
            .body_height = body_height,
            .feed_width = size.width -| (inspector_width + 3),
            .inspector_width = inspector_width,
            .feed_height = body_height,
            .inspector_height = body_height,
        };
    }

    if (body_height <= 2) {
        return .{
            .mode = .stacked,
            .body_height = body_height,
            .feed_width = size.width,
            .inspector_width = size.width,
            .feed_height = body_height,
            .inspector_height = 0,
        };
    }

    if (body_height <= 2) {
        return .{
            .mode = .stacked,
            .body_height = body_height,
            .feed_width = size.width,
            .inspector_width = size.width,
            .feed_height = body_height,
            .inspector_height = 0,
        };
    }

    var inspector_height = @max(@as(usize, 3), body_height / 3);
    if (inspector_height >= body_height) inspector_height = body_height -| 1;
    const feed_height = @max(@as(usize, 1), body_height -| inspector_height);
    return .{
        .mode = .stacked,
        .body_height = body_height,
        .feed_width = size.width,
        .inspector_width = size.width,
        .feed_height = feed_height,
        .inspector_height = @max(@as(usize, 1), body_height -| feed_height),
    };
}

fn visiblePrimaryRows(layout: Layout) usize {
    return switch (layout.mode) {
        .split => layout.body_height -| 1,
        .stacked => layout.feed_height -| 1,
    };
}

fn clampFeedViewport(state: *AppState, visible_rows: usize) void {
    state.clampSelection();
    if (state.feed_items.items.len == 0 or visible_rows == 0) {
        state.feed_scroll = 0;
        return;
    }
    if (state.selected_feed_index < state.feed_scroll) {
        state.feed_scroll = state.selected_feed_index;
    } else if (state.selected_feed_index >= state.feed_scroll + visible_rows) {
        state.feed_scroll = state.selected_feed_index + 1 - visible_rows;
    }
    const max_scroll = state.feed_items.items.len -| visible_rows;
    if (state.feed_scroll > max_scroll) state.feed_scroll = max_scroll;
}

fn clampLedgerViewport(state: *AppState, visible_rows: usize) void {
    state.clampLedgerSelection();
    if (state.ledger_items.items.len == 0 or visible_rows == 0) {
        state.ledger_scroll = 0;
        return;
    }
    if (state.selected_ledger_index < state.ledger_scroll) {
        state.ledger_scroll = state.selected_ledger_index;
    } else if (state.selected_ledger_index >= state.ledger_scroll + visible_rows) {
        state.ledger_scroll = state.selected_ledger_index + 1 - visible_rows;
    }
    const max_scroll = state.ledger_items.items.len -| visible_rows;
    if (state.ledger_scroll > max_scroll) state.ledger_scroll = max_scroll;
}

fn composerCursorPosition(layout: Layout, width: usize, composer: ComposerState) struct { usize, usize } {
    const available = width -| prompt_label.len;
    const view = visibleComposer(composer.line, composer.cursor, available);
    const row = 1 + layout.totalBodyRows() + 2;
    const col = @min(width, prompt_label.len + view.cursor_col + 1);
    return .{ row, if (col == 0) 1 else col };
}

fn visibleComposer(line: []const u8, cursor: usize, max_width: usize) ComposerView {
    if (max_width == 0) return .{ .visible = "", .cursor_col = 0 };
    if (line.len <= max_width) return .{
        .visible = line,
        .cursor_col = @min(cursor, line.len),
    };

    const safe_cursor = @min(cursor, line.len);
    var start: usize = 0;
    if (safe_cursor >= max_width) {
        start = safe_cursor + 1 - max_width;
    }
    if (start + max_width > line.len) {
        start = line.len - max_width;
    }
    return .{
        .visible = line[start .. start + max_width],
        .cursor_col = safe_cursor - start,
    };
}

fn nextFocus(current: FocusMode) FocusMode {
    return switch (current) {
        .composer => .feed,
        .feed => .inspector,
        .inspector => .composer,
    };
}

fn prevFocus(current: FocusMode) FocusMode {
    return switch (current) {
        .composer => .inspector,
        .feed => .composer,
        .inspector => .feed,
    };
}

fn nextLedgerTab(current: LedgerTab) LedgerTab {
    return switch (current) {
        .delta => .diff,
        .diff => .properties,
        .properties => .violations,
        .violations => .prove,
        .prove => .system,
        .system => .citations,
        .citations => .witnesses,
        .witnesses => .delta,
    };
}

fn prevLedgerTab(current: LedgerTab) LedgerTab {
    return switch (current) {
        .delta => .witnesses,
        .diff => .delta,
        .properties => .diff,
        .violations => .properties,
        .prove => .violations,
        .system => .prove,
        .citations => .system,
        .witnesses => .citations,
    };
}

fn moveSelectionUp(state: *AppState) void {
    if (state.feed_items.items.len == 0 or state.selected_feed_index == 0) return;
    state.selected_feed_index -= 1;
    state.inspector.scroll_offset = 0;
}

fn moveSelectionDown(state: *AppState) void {
    if (state.feed_items.items.len == 0 or state.selected_feed_index + 1 >= state.feed_items.items.len) return;
    state.selected_feed_index += 1;
    state.inspector.scroll_offset = 0;
}

fn moveLedgerSelectionUp(allocator: std.mem.Allocator, state: *AppState) void {
    if (state.ledger_items.items.len == 0 or state.selected_ledger_index == 0) return;
    state.selected_ledger_index -= 1;
    state.inspector.scroll_offset = 0;
    state.diff_expanded = false;
    state.selected_witness_index = 0;
    state.clearWitnessVerdict(allocator);
}

fn moveLedgerSelectionDown(allocator: std.mem.Allocator, state: *AppState) void {
    if (state.ledger_items.items.len == 0 or state.selected_ledger_index + 1 >= state.ledger_items.items.len) return;
    state.selected_ledger_index += 1;
    state.inspector.scroll_offset = 0;
    state.diff_expanded = false;
    state.selected_witness_index = 0;
    state.clearWitnessVerdict(allocator);
}

fn moveWitnessSelectionUp(allocator: std.mem.Allocator, state: *AppState) void {
    if (state.selected_witness_index == 0) return;
    state.selected_witness_index -= 1;
    state.clearWitnessVerdict(allocator);
}

fn moveWitnessSelectionDown(
    allocator: std.mem.Allocator,
    state: *AppState,
    session: *const agent.AgentSession,
) void {
    const total = witnessCountForSelectedPatch(state, session);
    if (state.selected_witness_index + 1 >= total) return;
    state.selected_witness_index += 1;
    state.clearWitnessVerdict(allocator);
}

fn witnessCountForSelectedPatch(
    state: *const AppState,
    session: *const agent.AgentSession,
) usize {
    const idx = state.selectedLedgerIndex() orelse return 0;
    const payload = session_state.patchPayload(session.transcript.at(idx)) orelse return 0;
    return payload.witnesses_defeated.len + payload.witnesses_new.len;
}

fn kindForTranscriptEntry(entry: *const transcript_mod.OwnedEntry) FeedItemKind {
    return switch (entry.*) {
        .user_text => .user_text,
        .model_text => .model_text,
        .assistant_tool_use => .tool_use,
        .tool_result => .tool_result,
        .proof_card => .proof_card,
        .diagnostic_box => .diagnostic_box,
        .verified_patch => .verified_patch,
        .system_note => .system_note,
    };
}

fn summaryText(llm_text: []const u8, payload: ?UiPayload) []const u8 {
    if (payload) |value| {
        switch (value) {
            .diagnostics => |diag| return firstLine(diag.summary),
            .proof_card => |proof| return if (proof.summary.len > 0) firstLine(proof.summary) else firstLine(proof.title),
            .command_outcome => |outcome| return if (outcome.title.len > 0) firstLine(outcome.title) else firstLine(outcome.command),
            .repair_candidate => |candidate| return firstLine(candidate.plan_id),
            .verified_patch => |patch| return firstLine(patch.file),
            .plain_text => |text| return firstLine(text),
            .session_tree => {},
        }
    }
    return firstLine(llm_text);
}

fn firstLine(text: []const u8) []const u8 {
    const end = std.mem.indexOfScalar(u8, text, '\n') orelse text.len;
    const trimmed_end = if (end > 0 and text[end - 1] == '\r') end - 1 else end;
    return text[0..trimmed_end];
}

fn focusLabel(focus: FocusMode) []const u8 {
    return switch (focus) {
        .composer => "composer",
        .feed => "feed",
        .inspector => "inspector",
    };
}

fn viewLabel(view: ViewMode) []const u8 {
    return switch (view) {
        .ledger => "ledger",
        .chat => "chat",
    };
}

fn ledgerTabLabel(tab: LedgerTab) []const u8 {
    return switch (tab) {
        .delta => "proof delta",
        .diff => "detail Diff",
        .properties => "detail Properties",
        .violations => "detail Violations",
        .prove => "detail Prove",
        .system => "detail System",
        .citations => "detail Citations",
        .witnesses => "detail Witnesses",
    };
}

fn parseKeyEvent(bytes: []const u8, start: usize) struct { KeyEvent, usize } {
    const b = bytes[start];
    if (b != 0x1b) return .{ classifySingleByte(b), 1 };
    if (start + 1 >= bytes.len) return .{ .{ .kind = .esc }, 1 };

    const second = bytes[start + 1];
    if (second == '[') {
        if (start + 2 >= bytes.len) return .{ .{ .kind = .ignore }, 2 };
        const third = bytes[start + 2];
        return switch (third) {
            'A' => .{ .{ .kind = .up }, 3 },
            'B' => .{ .{ .kind = .down }, 3 },
            'C' => .{ .{ .kind = .right }, 3 },
            'D' => .{ .{ .kind = .left }, 3 },
            'H' => .{ .{ .kind = .home }, 3 },
            'F' => .{ .{ .kind = .end }, 3 },
            'Z' => .{ .{ .kind = .shift_tab }, 3 },
            '1', '7', '4', '8', '3' => parseTildeTerminated(bytes, start, third),
            else => .{ .{ .kind = .ignore }, 3 },
        };
    }

    if (second == 'O') {
        if (start + 2 >= bytes.len) return .{ .{ .kind = .ignore }, 2 };
        return switch (bytes[start + 2]) {
            'H' => .{ .{ .kind = .home }, 3 },
            'F' => .{ .{ .kind = .end }, 3 },
            else => .{ .{ .kind = .ignore }, 3 },
        };
    }

    return .{ .{ .kind = .esc }, 1 };
}

fn parseTildeTerminated(bytes: []const u8, start: usize, code: u8) struct { KeyEvent, usize } {
    if (start + 3 >= bytes.len or bytes[start + 3] != '~') {
        return .{ .{ .kind = .ignore }, 3 };
    }
    const kind: KeyKind = switch (code) {
        '1', '7' => .home,
        '4', '8' => .end,
        '3' => .delete,
        else => .ignore,
    };
    return .{ .{ .kind = kind }, 4 };
}

fn classifySingleByte(b: u8) KeyEvent {
    return switch (b) {
        3 => .{ .kind = .ctrl_c },
        4 => .{ .kind = .eof },
        9 => .{ .kind = .tab },
        13, 10 => .{ .kind = .enter },
        127, 8 => .{ .kind = .backspace },
        32...126, 128...255 => .{ .kind = .char, .byte = b },
        else => .{ .kind = .ignore },
    };
}

fn writeFitted(w: *std.Io.Writer, text: []const u8, width: usize) !void {
    if (width == 0) return;
    if (text.len <= width) {
        try w.writeAll(text);
        try writeSpaces(w, width - text.len);
        return;
    }
    if (width <= 3) {
        try w.writeAll(text[0..width]);
        return;
    }
    try w.writeAll(text[0 .. width - 3]);
    try w.writeAll("...");
}

fn writeSpaces(w: *std.Io.Writer, count: usize) !void {
    var remaining = count;
    while (remaining > 0) : (remaining -= 1) {
        try w.writeByte(' ');
    }
}

fn writeCrlf(w: *std.Io.Writer) !void {
    try w.writeAll("\r\n");
}

fn moveCursor(w: *std.Io.Writer, row: usize, col: usize) !void {
    try w.print("\x1b[{d};{d}H", .{ row, col });
}

fn writeAll(bytes: []const u8) void {
    if (bytes.len == 0) return;
    _ = std.c.write(std.c.STDOUT_FILENO, bytes.ptr, bytes.len);
}

const testing = std.testing;
const IsolatedTmp = @import("../test_support/tmp.zig").IsolatedTmp;

fn appendTestVerifiedPatch(
    allocator: std.mem.Allocator,
    session: *agent.AgentSession,
) !void {
    const hunks = try allocator.alloc(ui_payload_mod.DiffHunk, 1);
    hunks[0] = .{ .old_start = 1, .old_count = 1, .new_start = 1, .new_count = 1 };

    const violations = try allocator.alloc(ui_payload_mod.ViolationDeltaItem, 1);
    violations[0] = try ui_payload_mod.ViolationDeltaItem.init(
        allocator,
        "stable-1",
        "ZTS204",
        "error",
        "serviceCall target must be validated",
        3,
        9,
        true,
    );

    const laws_used = try allocator.alloc([]u8, 1);
    laws_used[0] = try allocator.dupe(u8, "subtyping");

    const warnings = try allocator.alloc([]u8, 1);
    warnings[0] = try allocator.dupe(u8, "dynamic edge retained");

    const citations = try allocator.alloc([]u8, 1);
    citations[0] = try allocator.dupe(u8, "ZTS204");

    try session.transcript.entries.append(allocator, .{ .verified_patch = .{
        .llm_text = try allocator.dupe(u8, "verified: handler.ts (additive, total=1 new=0)"),
        .ui_payload = .{ .verified_patch = .{
            .file = try allocator.dupe(u8, "handler.ts"),
            .policy_hash = try allocator.dupe(u8, "a" ** 64),
            .applied_at_unix_ms = 42,
            .stats = .{ .total = 1, .new = 0, .preexisting = 1 },
            .before = try allocator.dupe(u8, "function handler(req: Request): Response { return Response.json({ ok: true }); }"),
            .after = try allocator.dupe(u8, "function handler(req: Request): Response { return Response.json({ ok: true, region: \"iad\" }); }"),
            .unified_diff = try allocator.dupe(u8, "@@ -1,1 +1,1 @@\n-function handler(req: Request): Response { return Response.json({ ok: true }); }\n+function handler(req: Request): Response { return Response.json({ ok: true, region: \"iad\" }); }\n"),
            .hunks = hunks,
            .violations = violations,
            .before_properties = .{
                .pure = true,
                .read_only = true,
                .stateless = true,
                .retry_safe = true,
                .deterministic = true,
                .has_egress = false,
                .no_secret_leakage = true,
                .no_credential_leakage = true,
                .input_validated = true,
                .pii_contained = true,
                .idempotent = true,
                .max_io_depth = 0,
                .injection_safe = true,
                .state_isolated = true,
                .fault_covered = true,
                .result_safe = true,
                .optional_safe = true,
            },
            .after_properties = .{
                .pure = false,
                .read_only = true,
                .stateless = true,
                .retry_safe = true,
                .deterministic = true,
                .has_egress = false,
                .no_secret_leakage = true,
                .no_credential_leakage = true,
                .input_validated = true,
                .pii_contained = true,
                .idempotent = true,
                .max_io_depth = 1,
                .injection_safe = true,
                .state_isolated = true,
                .fault_covered = true,
                .result_safe = true,
                .optional_safe = true,
            },
            .prove = .{
                .classification = try allocator.dupe(u8, "additive"),
                .proof_level = try allocator.dupe(u8, "complete"),
                .recommendation = try allocator.dupe(u8, "safe to accept"),
                .counterexample = try allocator.dupe(u8, "none"),
                .laws_used = laws_used,
            },
            .system = .{
                .system_path = try allocator.dupe(u8, "system.json"),
                .proof_level = try allocator.dupe(u8, "partial"),
                .all_links_resolved = true,
                .all_responses_covered = true,
                .payload_compatible = true,
                .injection_safe = true,
                .no_secret_leakage = true,
                .no_credential_leakage = true,
                .retry_safe = true,
                .fault_covered = true,
                .state_isolated = true,
                .max_system_io_depth = 2,
                .dynamic_links = 1,
                .warnings = warnings,
            },
            .rule_citations = citations,
            .post_apply_ok = true,
            .post_apply_summary = try allocator.dupe(u8, "post-apply verification succeeded"),
        } },
    } });
}

test "classifySingleByte maps control bytes plus tab" {
    try testing.expectEqual(KeyKind.ctrl_c, classifySingleByte(3).kind);
    try testing.expectEqual(KeyKind.eof, classifySingleByte(4).kind);
    try testing.expectEqual(KeyKind.tab, classifySingleByte(9).kind);
    try testing.expectEqual(KeyKind.enter, classifySingleByte(13).kind);
    try testing.expectEqual(KeyKind.backspace, classifySingleByte(127).kind);
}

test "parseKeyEvent decodes escape, shift-tab, arrows, and delete" {
    const esc_event, const esc_len = parseKeyEvent("\x1b", 0);
    try testing.expectEqual(KeyKind.esc, esc_event.kind);
    try testing.expectEqual(@as(usize, 1), esc_len);

    const shift_tab_event, const shift_tab_len = parseKeyEvent("\x1b[Z", 0);
    try testing.expectEqual(KeyKind.shift_tab, shift_tab_event.kind);
    try testing.expectEqual(@as(usize, 3), shift_tab_len);

    const down_event, const down_len = parseKeyEvent("\x1b[B", 0);
    try testing.expectEqual(KeyKind.down, down_event.kind);
    try testing.expectEqual(@as(usize, 3), down_len);

    const delete_event, const delete_len = parseKeyEvent("\x1b[3~", 0);
    try testing.expectEqual(KeyKind.delete, delete_event.kind);
    try testing.expectEqual(@as(usize, 4), delete_len);
}

test "layout stacks inspector on narrow terminals" {
    const layout = computeLayout(.{ .width = 72, .height = 24 });
    try testing.expectEqual(LayoutMode.stacked, layout.mode);
    try testing.expectEqual(@as(usize, 72), layout.feed_width);
    try testing.expect(layout.inspector_height > 0);
}

test "Tab and Esc move focus through panes" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    var state: AppState = .{};
    defer state.deinit(testing.allocator);

    try testing.expectEqual(FocusMode.composer, state.focus_mode);
    state.focus_mode = nextFocus(state.focus_mode);
    try testing.expectEqual(FocusMode.feed, state.focus_mode);
    state.focus_mode = nextFocus(state.focus_mode);
    try testing.expectEqual(FocusMode.inspector, state.focus_mode);
    _ = handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .esc });
    try testing.expectEqual(FocusMode.composer, state.focus_mode);
}

test "feed selection drives inspector content" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try session.transcript.append(testing.allocator, .{ .user_text = "add a GET route" });
    try session.transcript.append(testing.allocator, .{ .proof_card = .{
        .llm_text = "verification passed",
        .ui_payload = .{ .proof_card = .{
            .title = @constCast("verification"),
            .summary = @constCast("all checks passed"),
            .stats = .{ .total = 4, .new = 0, .preexisting = 0 },
            .highlights = @constCast(&[_][]u8{}),
        } },
    } });

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    state.view_mode = .chat;
    try state.sync(testing.allocator, &session, &editor);
    state.selected_feed_index = 1;

    const text = try buildInspectorText(testing.allocator, &state, &session);
    defer testing.allocator.free(text);

    try testing.expect(std.mem.indexOf(u8, text, "all checks passed") != null);
    try testing.expect(std.mem.indexOf(u8, text, "total=4") != null);
}

test "ledger view shows empty state when no verified patches exist" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);

    const text = try buildInspectorText(testing.allocator, &state, &session);
    defer testing.allocator.free(text);

    try testing.expect(std.mem.indexOf(u8, text, "No verified patches yet.") != null);
    try testing.expect(std.mem.indexOf(u8, text, "Run /chat") != null);
}

test "ledger view renders verified patch tabs" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);

    try testing.expectEqual(@as(usize, 1), state.ledger_items.items.len);

    state.ledger_tab = .diff;
    const diff_text = try buildInspectorText(testing.allocator, &state, &session);
    defer testing.allocator.free(diff_text);
    try testing.expect(std.mem.indexOf(u8, diff_text, "Diff") != null);
    try testing.expect(std.mem.indexOf(u8, diff_text, "@@ -1,1 +1,1 @@") != null);

    state.ledger_tab = .prove;
    const prove_text = try buildInspectorText(testing.allocator, &state, &session);
    defer testing.allocator.free(prove_text);
    try testing.expect(std.mem.indexOf(u8, prove_text, "classification: additive") != null);
    try testing.expect(std.mem.indexOf(u8, prove_text, "laws_used:") != null);

    state.ledger_tab = .system;
    const system_text = try buildInspectorText(testing.allocator, &state, &session);
    defer testing.allocator.free(system_text);
    try testing.expect(std.mem.indexOf(u8, system_text, "system_path: system.json") != null);
    try testing.expect(std.mem.indexOf(u8, system_text, "dynamic_links: 1") != null);

    state.ledger_tab = .citations;
    const citations_text = try buildInspectorText(testing.allocator, &state, &session);
    defer testing.allocator.free(citations_text);
    try testing.expect(std.mem.indexOf(u8, citations_text, "ZTS204") != null);
}

test "ledger proof-delta tab is the default and renders promoted/demoted badges" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);

    try testing.expectEqual(LedgerTab.delta, state.ledger_tab);

    const text = try buildInspectorText(testing.allocator, &state, &session);
    defer testing.allocator.free(text);

    try testing.expect(std.mem.indexOf(u8, text, "Proof Delta") != null);
    // Fixture demotes `pure` (true -> false) and max_io_depth is optional so it
    // doesn't surface; no bool field is promoted in the fixture. Expect the
    // demotion badge to render.
    try testing.expect(std.mem.indexOf(u8, text, "-pure") != null);
    try testing.expect(std.mem.indexOf(u8, text, "violations") != null);
    try testing.expect(std.mem.indexOf(u8, text, "witnesses") != null);
    try testing.expect(std.mem.indexOf(u8, text, "diff") != null);
    try testing.expect(std.mem.indexOf(u8, text, "Enter to expand") != null);
}

test "ledger properties tab marks goal-driveable and structural properties" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);
    state.ledger_tab = .properties;
    state.selected_property_index = property_goals.boolPropertyIndexOf("pure").?;

    const text = try buildInspectorText(testing.allocator, &state, &session);
    defer testing.allocator.free(text);

    try testing.expect(std.mem.indexOf(u8, text, "> [false] pure (struct)") != null);
    try testing.expect(std.mem.indexOf(u8, text, "[true] no_secret_leakage (goal)") != null);
    try testing.expect(std.mem.indexOf(u8, text, "max_io_depth: 1 (metric)") != null);
}

test "ledger properties tab left and right move the property cursor" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);
    state.ledger_tab = .properties;
    state.selected_property_index = 0;

    try testing.expectEqual(PaneOutcome.redraw, handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .char, .byte = 'g' }));
    switch (state.status_notice) {
        .none => return error.TestFailed,
        else => {},
    }

    try testing.expectEqual(PaneOutcome.redraw, handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .right }));
    try testing.expectEqual(@as(usize, 1), state.selected_property_index);
    switch (state.status_notice) {
        .none => {},
        else => return error.TestFailed,
    }

    try testing.expectEqual(PaneOutcome.redraw, handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .left }));
    try testing.expectEqual(@as(usize, 0), state.selected_property_index);

    try testing.expectEqual(PaneOutcome.no_op, handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .left }));
    try testing.expectEqual(@as(usize, 0), state.selected_property_index);

    state.selected_property_index = property_goals.bool_property_count - 1;
    try testing.expectEqual(PaneOutcome.no_op, handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .right }));
    try testing.expectEqual(property_goals.bool_property_count - 1, state.selected_property_index);
}

test "g on structural property shows honest boundary status" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);
    state.ledger_tab = .properties;
    state.selected_property_index = property_goals.boolPropertyIndexOf("pure").?;

    try testing.expectEqual(PaneOutcome.redraw, handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .char, .byte = 'g' }));
    switch (state.status_notice) {
        .structural_property => {},
        else => return error.TestFailed,
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);
    try renderStatusRow(&aw.writer, &session, &state, 240);
    buf = aw.toArrayList();
    try testing.expect(std.mem.indexOf(
        u8,
        buf.items,
        "structural property - not goal-driveable; edit the source to change it.",
    ) != null);
}

test "g on flow property dispatches the autoloop and surfaces in-flight state" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);
    state.ledger_tab = .properties;
    state.selected_property_index = property_goals.boolPropertyIndexOf("no_secret_leakage").?;

    try testing.expectEqual(PaneOutcome.drive_goal, handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .char, .byte = 'g' }));
    try testing.expect(state.autoloop_in_flight);
    switch (state.status_notice) {
        .goal_driving => |name| try testing.expectEqualStrings("no_secret_leakage", name),
        else => return error.TestFailed,
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);
    try renderStatusRow(&aw.writer, &session, &state, 240);
    buf = aw.toArrayList();
    try testing.expect(std.mem.indexOf(u8, buf.items, "driving goal no_secret_leakage") != null);
}

test "g while autoloop is in flight is dropped" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);
    state.ledger_tab = .properties;
    state.selected_property_index = property_goals.boolPropertyIndexOf("no_secret_leakage").?;
    state.autoloop_in_flight = true;

    // Single-flight guard: a second `g` while a drive is already running
    // is silently ignored. The dispatch outcome must not fire twice or
    // race the in-flight worker.
    try testing.expectEqual(PaneOutcome.no_op, handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .char, .byte = 'g' }));
}

test "ledger pane hotkeys switch tabs and views" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);

    try testing.expectEqual(LedgerTab.delta, state.ledger_tab);
    try testing.expectEqual(ViewMode.ledger, state.view_mode);

    _ = handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .tab });
    try testing.expectEqual(LedgerTab.diff, state.ledger_tab);
    try testing.expectEqual(FocusMode.inspector, state.focus_mode);

    _ = handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .shift_tab });
    try testing.expectEqual(LedgerTab.delta, state.ledger_tab);

    _ = handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .enter });
    try testing.expect(state.diff_expanded);

    _ = handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .char, .byte = 'c' });
    try testing.expectEqual(ViewMode.chat, state.view_mode);

    _ = handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .char, .byte = 'l' });
    try testing.expectEqual(ViewMode.ledger, state.view_mode);
}

test "ledger witnesses tab renders defeated and new counterexample bodies" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);
    try injectTestWitnesses(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);
    state.ledger_tab = .witnesses;

    const text = try buildInspectorText(testing.allocator, &state, &session);
    defer testing.allocator.free(text);

    try testing.expect(std.mem.indexOf(u8, text, "Witnesses") != null);
    try testing.expect(std.mem.indexOf(u8, text, "defeated by this patch (1)") != null);
    try testing.expect(std.mem.indexOf(u8, text, "[no_secret_leakage]") != null);
    try testing.expect(std.mem.indexOf(u8, text, "GET /") != null);
    try testing.expect(std.mem.indexOf(u8, text, "origin 5:9 -> sink 7:12") != null);
    try testing.expect(std.mem.indexOf(u8, text, "summary: DB_KEY in response") != null);
    try testing.expect(std.mem.indexOf(u8, text, "key: aaaaaaaaaaaa") != null);
    try testing.expect(std.mem.indexOf(u8, text, "env.env() -> \"sentinel\"") != null);
    try testing.expect(std.mem.indexOf(u8, text, "introduced by this patch (1)") != null);
    try testing.expect(std.mem.indexOf(u8, text, "[injection_safe]") != null);
    try testing.expect(std.mem.indexOf(u8, text, "POST /api/items  (auth)") != null);
}

test "lowercase w jumps to the witnesses tab and focuses the inspector" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);
    state.view_mode = .chat;
    state.ledger_tab = .delta;
    state.focus_mode = .composer;

    try testing.expectEqual(PaneOutcome.redraw, handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .char, .byte = 'w' }));
    try testing.expectEqual(ViewMode.ledger, state.view_mode);
    try testing.expectEqual(LedgerTab.witnesses, state.ledger_tab);
    try testing.expectEqual(FocusMode.inspector, state.focus_mode);
}

test "ledger tab cycler reaches witnesses after citations and wraps to delta" {
    try testing.expectEqual(LedgerTab.witnesses, nextLedgerTab(.citations));
    try testing.expectEqual(LedgerTab.delta, nextLedgerTab(.witnesses));
    try testing.expectEqual(LedgerTab.witnesses, prevLedgerTab(.delta));
}

test "g on a witness with goal-driveable property stashes a focus and dispatches drive_goal" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);
    try injectTestWitnesses(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);
    state.ledger_tab = .witnesses;
    state.selected_witness_index = 0; // defeated witness: no_secret_leakage

    const outcome = handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .char, .byte = 'g' });
    try testing.expectEqual(PaneOutcome.drive_goal, outcome);
    try testing.expect(state.autoloop_in_flight);
    try testing.expect(state.pending_witness_focus != null);
    try testing.expectEqualStrings("a" ** 64, state.pending_witness_focus.?.key);
    try testing.expectEqualStrings("no_secret_leakage", state.pending_witness_focus.?.property);
    switch (state.status_notice) {
        .witness_driving => |d| {
            try testing.expectEqualStrings("no_secret_leakage", d.property);
            try testing.expectEqualStrings("a" ** 12, d.short_key);
        },
        else => return error.TestFailed,
    }
}

test "m without a recent verdict surfaces an error and does not dispatch" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);
    try injectTestWitnesses(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);
    state.ledger_tab = .witnesses;
    state.selected_witness_index = 0;

    const outcome = handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .char, .byte = 'm' });
    try testing.expectEqual(PaneOutcome.redraw, outcome);
    switch (state.status_notice) {
        .mint_error => |text| try testing.expect(std.mem.indexOf(u8, text, "press r first") != null),
        else => return error.TestFailed,
    }
}

test "m with a matching verdict returns mint_witness" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);
    try injectTestWitnesses(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);
    state.ledger_tab = .witnesses;
    state.selected_witness_index = 0;

    // Plant a verdict whose key matches the first defeated witness.
    const key_copy = try testing.allocator.dupe(u8, "a" ** 64);
    state.witness_verdict = .{
        .key = key_copy,
        .verdict = .{
            .ran = true,
            .actual_status = 401,
            .actual_body = try testing.allocator.dupe(u8, "Unauthorized"),
            .error_text = null,
        },
        .reproduced = false,
    };

    const outcome = handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .char, .byte = 'm' });
    try testing.expectEqual(PaneOutcome.mint_witness, outcome);
}

test "appendWitnessRegression writes a parseable test block" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator, "witness-mint");
    defer tmp.cleanup(allocator);
    const tests_path = try tmp.childPath(allocator, "witness-regressions.jsonl");
    defer allocator.free(tests_path);

    const stubs = [_]ui_payload_mod.WitnessStub{
        .{
            .seq = 0,
            .module = @constCast("env"),
            .func = @constCast("env"),
            .result_json = @constCast("\"sentinel\""),
        },
    };
    const body = ui_payload_mod.WitnessBody{
        .key = @constCast("d" ** 64),
        .property = @constCast("no_secret_leakage"),
        .summary = @constCast(""),
        .origin_line = 5,
        .origin_column = 9,
        .sink_line = 7,
        .sink_column = 12,
        .request_method = @constCast("GET"),
        .request_url = @constCast("/"),
        .request_has_auth = false,
        .request_body = null,
        .io_stubs = @constCast(&stubs),
    };

    try appendWitnessRegression(allocator, tests_path, body, 401);
    const written = try zigts.file_io.readFile(allocator, tests_path, 1024 * 1024);
    defer allocator.free(written);

    try testing.expect(std.mem.indexOf(u8, written, "\"type\":\"test\"") != null);
    try testing.expect(std.mem.indexOf(u8, written, "witness-dddddddddddd-no_secret_leakage") != null);
    try testing.expect(std.mem.indexOf(u8, written, "\"type\":\"request\"") != null);
    try testing.expect(std.mem.indexOf(u8, written, "\"method\":\"GET\"") != null);
    try testing.expect(std.mem.indexOf(u8, written, "\"type\":\"io\"") != null);
    try testing.expect(std.mem.indexOf(u8, written, "\"fn\":\"env\"") != null);
    try testing.expect(std.mem.indexOf(u8, written, "\"type\":\"expect\"") != null);
    try testing.expect(std.mem.indexOf(u8, written, "\"status\":401") != null);

    // Append a second mint to the same file - both cases must coexist.
    try appendWitnessRegression(allocator, tests_path, body, 200);
    const both = try zigts.file_io.readFile(allocator, tests_path, 1024 * 1024);
    defer allocator.free(both);
    try testing.expect(std.mem.indexOf(u8, both, "\"status\":401") != null);
    try testing.expect(std.mem.indexOf(u8, both, "\"status\":200") != null);
}

test "g on a witness while the autoloop is in flight is dropped" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);
    try injectTestWitnesses(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);
    state.ledger_tab = .witnesses;
    state.selected_witness_index = 0;
    state.autoloop_in_flight = true;

    const outcome = handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .char, .byte = 'g' });
    try testing.expectEqual(PaneOutcome.no_op, outcome);
    try testing.expect(state.pending_witness_focus == null);
}

/// Replace the latest verified_patch entry's witness arrays with concrete
/// bodies. Used by witness-pane render tests; constructed inline rather
/// than baked into `appendTestVerifiedPatch` so the existing fixture stays
/// minimal for tests that do not exercise witnesses.
fn injectTestWitnesses(
    allocator: std.mem.Allocator,
    session: *agent.AgentSession,
) !void {
    const last_index = session.transcript.entries.items.len - 1;
    const entry = &session.transcript.entries.items[last_index];
    switch (entry.*) {
        .verified_patch => |*vp| {
            const payload_ptr = if (vp.ui_payload) |*p| p else return error.TestFailed;
            switch (payload_ptr.*) {
                .verified_patch => |*patch| {
                    ui_payload_mod.freeWitnessBodySlice(allocator, patch.witnesses_defeated);
                    ui_payload_mod.freeWitnessBodySlice(allocator, patch.witnesses_new);

                    const defeated = try allocator.alloc(ui_payload_mod.WitnessBody, 1);
                    defeated[0] = try buildTestWitness(
                        allocator,
                        "a" ** 64,
                        "no_secret_leakage",
                        "DB_KEY in response",
                        .{ 5, 9 },
                        .{ 7, 12 },
                        "GET",
                        "/",
                        false,
                        null,
                        "env",
                        "env",
                        "\"sentinel\"",
                    );

                    const new_w = try allocator.alloc(ui_payload_mod.WitnessBody, 1);
                    new_w[0] = try buildTestWitness(
                        allocator,
                        "b" ** 64,
                        "injection_safe",
                        "unvalidated body reaches sql",
                        .{ 11, 3 },
                        .{ 14, 7 },
                        "POST",
                        "/api/items",
                        true,
                        "{\"name\":\"x\"}",
                        "sql",
                        "sql",
                        "{}",
                    );

                    patch.witnesses_defeated = defeated;
                    patch.witnesses_new = new_w;
                },
                else => return error.TestFailed,
            }
        },
        else => return error.TestFailed,
    }
}

fn buildTestWitness(
    allocator: std.mem.Allocator,
    key: []const u8,
    property: []const u8,
    summary: []const u8,
    origin: [2]u32,
    sink: [2]u32,
    method: []const u8,
    url: []const u8,
    has_auth: bool,
    body: ?[]const u8,
    stub_module: []const u8,
    stub_func: []const u8,
    stub_result: []const u8,
) !ui_payload_mod.WitnessBody {
    const stubs = try allocator.alloc(ui_payload_mod.WitnessStub, 1);
    stubs[0] = .{
        .seq = 0,
        .module = try allocator.dupe(u8, stub_module),
        .func = try allocator.dupe(u8, stub_func),
        .result_json = try allocator.dupe(u8, stub_result),
    };
    return .{
        .key = try allocator.dupe(u8, key),
        .property = try allocator.dupe(u8, property),
        .summary = try allocator.dupe(u8, summary),
        .origin_line = origin[0],
        .origin_column = origin[1],
        .sink_line = sink[0],
        .sink_column = sink[1],
        .request_method = try allocator.dupe(u8, method),
        .request_url = try allocator.dupe(u8, url),
        .request_has_auth = has_auth,
        .request_body = if (body) |b| try allocator.dupe(u8, b) else null,
        .io_stubs = stubs,
    };
}

test "uppercase A on a clean ledger patch returns .approve" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);

    const outcome = handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .char, .byte = 'A' });
    try testing.expectEqual(PaneOutcome.approve, outcome);
}

test "uppercase A in chat view does not trigger approval" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try appendTestVerifiedPatch(testing.allocator, &session);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);
    state.view_mode = .chat;

    const outcome = handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .char, .byte = 'A' });
    try testing.expectEqual(PaneOutcome.redraw, outcome);
}

test "uppercase A with no selected patch does not trigger approval" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);

    const outcome = handlePaneEvent(testing.allocator, &state, &session, .{ .kind = .char, .byte = 'A' });
    try testing.expectEqual(PaneOutcome.redraw, outcome);
}

test "approval modal accepts and rejects with keyboard actions" {
    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    state.modal = .{ .file = "src/handler.ts" };

    try testing.expectEqual(ModalAction.redraw, handleModalEvent(&state, .{ .kind = .tab }));
    if (state.modal) |approval| {
        try testing.expectEqual(ApprovalChoice.approve, approval.choice);
    } else return error.TestFailed;

    switch (handleModalEvent(&state, .{ .kind = .enter })) {
        .decided => |decision| try testing.expect(decision),
        else => return error.TestFailed,
    }

    state.modal = .{ .file = "src/handler.ts" };
    switch (handleModalEvent(&state, .{ .kind = .esc })) {
        .decided => |decision| try testing.expect(!decision),
        else => return error.TestFailed,
    }
}

test "resetToTranscript rebuilds feed from transcript entries" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try session.transcript.append(testing.allocator, .{ .user_text = "hello" });
    try session.transcript.append(testing.allocator, .{ .model_text = "world" });

    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    var state: AppState = .{};
    defer state.deinit(testing.allocator);

    try state.appendLocalText(testing.allocator, "/status", true, "stub");
    state.clearLocalResults(testing.allocator);
    try state.resetToTranscript(testing.allocator, &session, &editor);

    try testing.expectEqual(@as(usize, 2), state.feed_items.items.len);
    try testing.expectEqual(@as(usize, 2), state.observed_transcript_len);
    try testing.expectEqual(FeedItemKind.model_text, state.feed_items.items[1].kind);
}

test "summaryText prefers structured payload summaries" {
    const summary = summaryText(
        "fallback",
        .{ .diagnostics = .{
            .summary = @constCast("2 diagnostics"),
            .items = @constCast(&[_]ui_payload_mod.DiagnosticItem{}),
        } },
    );
    try testing.expectEqualStrings("2 diagnostics", summary);
}

test "transcript tool result summary line uses llm text when no payload exists" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);
    try session.transcript.append(testing.allocator, .{ .tool_result = .{
        .tool_use_id = "toolu_1",
        .tool_name = "zigts_expert_meta",
        .ok = true,
        .llm_text = "{\"ok\":true}",
    } });

    var state: AppState = .{};
    defer state.deinit(testing.allocator);
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);
    try state.sync(testing.allocator, &session, &editor);

    var buf: [1024]u8 = undefined;
    const line = buildFeedLine(&buf, state.feed_items.items[0], &state, &session);
    try testing.expect(std.mem.indexOf(u8, line, "zigts_expert_meta") != null);
    try testing.expect(std.mem.indexOf(u8, line, "{\"ok\":true}") != null);
}
