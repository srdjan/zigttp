//! AppState (transcript view + ledger + composer + modal) and every
//! value type it owns. Render and handler code live in app.zig.

const std = @import("std");

const agent = @import("../agent.zig");
const autoloop = @import("../autoloop.zig");
const transcript_mod = @import("../transcript.zig");
const ui_payload_mod = @import("../ui_payload.zig");
const witness_replay = @import("../witness_replay.zig");
const property_goals = @import("../property_goals.zig");
const registry_tool = @import("../registry/tool.zig");
const line_editor = @import("line_editor.zig");
const layout_mod = @import("layout.zig");

const LineEditor = line_editor.LineEditor;
const UiPayload = ui_payload_mod.UiPayload;
const ToolResult = registry_tool.ToolResult;
const LayoutMode = layout_mod.LayoutMode;

pub const FocusMode = enum {
    composer,
    feed,
    inspector,
};

pub const ViewMode = enum {
    ledger,
    chat,
};

pub const StatusNotice = union(enum) {
    none,
    structural_property,
    invalid_property,
    goal_driving: []const u8,
    witness_driving: WitnessDriving,
    goal_completed: GoalCompleted,
    goal_dispatch_error: []const u8,
    replay_unavailable,
    replay_done: ReplayDone,
    replay_error: []const u8,
    mint_done: []const u8,
    mint_error: []const u8,
    cancelling,
    goal_cancelled: []const u8,
    replay_cancelled,
};

pub const ReplayDone = struct {
    reproduced: bool,
    /// First 12 hex chars of the witness key. Borrowed from the witness
    /// body; the verdict record itself owns the full key.
    short_key: []const u8,
};

pub const WitnessDriving = struct {
    property: []const u8,
    short_key: []const u8,
};

pub const GoalCompleted = struct {
    name: []const u8,
    verdict: autoloop.AutoloopVerdict,
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

pub const FeedSource = union(enum) {
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
    /// Property tag borrowed from the `boolPropertyNameAt` table; free `key` only.
    property: []const u8,

    pub fn deinit(self: *PendingWitnessFocus, allocator: std.mem.Allocator) void {
        allocator.free(self.key);
        self.* = .{ .key = &.{}, .property = &.{} };
    }
};

/// Owned record of a witness replay verdict held in AppState.
pub const WitnessVerdictRecord = struct {
    key: []u8,
    verdict: witness_replay.Verdict,
    reproduced: bool,

    pub fn deinit(self: *WitnessVerdictRecord, allocator: std.mem.Allocator) void {
        allocator.free(self.key);
        self.verdict.deinit(allocator);
        self.* = .{ .key = &.{}, .verdict = .{ .ran = false, .actual_status = 0, .actual_body = &.{}, .error_text = null }, .reproduced = false };
    }
};

pub const LedgerTab = enum {
    delta,
    diff,
    properties,
    violations,
    prove,
    system,
    citations,
    witnesses,
};

pub const ComposerState = struct {
    line: []const u8 = "",
    cursor: usize = 0,
};

pub const InspectorState = struct {
    scroll_offset: usize = 0,
};

pub const ApprovalChoice = enum {
    reject,
    approve,
};

pub const ApprovalModal = struct {
    file: []const u8,
    choice: ApprovalChoice = .reject,
};

/// Optional modal overlay. Extend to a tagged union if a second kind
/// appears; for now the approval prompt is the only modal.
pub const ModalState = ?ApprovalModal;

pub const LocalResult = struct {
    title: []u8,
    ok: bool,
    llm_text: []u8,
    ui_payload: ?UiPayload = null,

    pub fn initFromToolResult(
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

    pub fn initPlainText(
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

    pub fn deinit(self: *LocalResult, allocator: std.mem.Allocator) void {
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
    /// True while a worker thread holds the transcript.
    autoloop_in_flight: bool = false,
    selected_witness_index: usize = 0,
    witness_verdict: ?WitnessVerdictRecord = null,
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
        if (self.autoloop_in_flight) return;
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

    pub fn shouldFollowTail(self: *const AppState) bool {
        return self.feed_items.items.len == 0 or self.selected_feed_index + 1 >= self.feed_items.items.len;
    }

    pub fn clampSelection(self: *AppState) void {
        if (self.feed_items.items.len == 0) {
            self.selected_feed_index = 0;
            self.feed_scroll = 0;
            return;
        }
        if (self.selected_feed_index >= self.feed_items.items.len) {
            self.selected_feed_index = self.feed_items.items.len - 1;
        }
    }

    pub fn selectedFeedItem(self: *const AppState) ?FeedItem {
        if (self.feed_items.items.len == 0) return null;
        return self.feed_items.items[self.selected_feed_index];
    }

    pub fn selectedLedgerIndex(self: *const AppState) ?usize {
        if (self.ledger_items.items.len == 0) return null;
        return self.ledger_items.items[self.selected_ledger_index];
    }

    pub fn clampLedgerSelection(self: *AppState) void {
        if (self.ledger_items.items.len == 0) {
            self.selected_ledger_index = 0;
            self.ledger_scroll = 0;
            return;
        }
        if (self.selected_ledger_index >= self.ledger_items.items.len) {
            self.selected_ledger_index = self.ledger_items.items.len - 1;
        }
    }

    pub fn clampPropertySelection(self: *AppState) void {
        if (property_goals.bool_property_count == 0) {
            self.selected_property_index = 0;
            return;
        }
        if (self.selected_property_index >= property_goals.bool_property_count) {
            self.selected_property_index = property_goals.bool_property_count - 1;
        }
    }

    pub fn clearStatusNotice(self: *AppState) void {
        self.status_notice = .none;
    }
};

pub fn kindForTranscriptEntry(entry: *const transcript_mod.OwnedEntry) FeedItemKind {
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
