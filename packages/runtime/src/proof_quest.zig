const std = @import("std");
const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const shared = @import("cli_shared.zig");
const compat = zigts.compat;

const fixture = zigts_cli.proof_quest_fixture;

pub const Config = struct {
    enabled: bool = false,
    explicit: bool = false,
};

const Stage = enum {
    idle,
    baseline,
    preview_break,
    waiting_broken_proof,
    broken,
    preview_repair,
    waiting_repaired_proof,
    complete,
};

const PendingAction = enum {
    break_proof,
    repair_proof,
};

pub const Snapshot = struct {
    enabled: bool,
    explicit: bool,
    stage: []const u8,
    title: []const u8,
    message: []const u8,
    available_actions: []const []const u8,
    complete: bool,
};

const no_actions = [_][]const u8{};
const baseline_actions = [_][]const u8{ "b", "s" };
const preview_break_actions = [_][]const u8{ "y", "s" };
const broken_actions = [_][]const u8{ "r", "s" };
const preview_repair_actions = [_][]const u8{ "y", "s" };
const complete_actions = [_][]const u8{ "zigttp check", "zigttp build", "zigttp deploy", "zigttp proofs badge" };

pub const State = struct {
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    config: Config,
    mu: compat.Mutex = .{},
    stage: Stage = .idle,
    pending: ?PendingAction = null,
    announced: Stage = .idle,

    pub fn init(allocator: std.mem.Allocator, handler_path: []const u8, config: Config) State {
        return .{
            .allocator = allocator,
            .handler_path = handler_path,
            .config = config,
        };
    }

    pub fn maybeStart(self: *State) void {
        if (!self.config.enabled) return;
        if (!self.config.explicit and markerExists(self.allocator)) return;

        const source = zigts.file_io.readFile(self.allocator, self.handler_path, 1024 * 1024) catch {
            if (self.config.explicit) printQuest("could not read {s}; quest unavailable.\n", .{self.handler_path});
            return;
        };
        defer self.allocator.free(source);

        const kind = fixture.classifySource(source);
        self.mu.lock();
        defer self.mu.unlock();

        switch (kind) {
            .starter => {
                self.stage = .baseline;
                self.pending = null;
                self.announced = .idle;
                self.printStageLocked(.baseline);
            },
            .broken => {
                self.stage = .broken;
                self.pending = null;
                self.announced = .idle;
                self.printStageLocked(.broken);
            },
            .other => {
                if (self.config.explicit) {
                    printQuest(
                        "this handler is not the scaffolded starter, so the quest will not edit it.\n" ++
                            "manual break: add `Date.now()` inside the `/` response, then save.\n" ++
                            "manual repair: remove that nondeterministic value and save again.\n",
                        .{},
                    );
                }
            },
        }
    }

    pub fn snapshot(self: *State) Snapshot {
        if (!self.config.enabled) {
            return .{
                .enabled = false,
                .explicit = self.config.explicit,
                .stage = "disabled",
                .title = "Proof Passport",
                .message = "Proof Passport is disabled for this dev session.",
                .available_actions = &no_actions,
                .complete = false,
            };
        }

        self.mu.lock();
        defer self.mu.unlock();
        return snapshotFor(self.stage, self.config.explicit);
    }

    /// Returns true when the key belonged to the quest, even if the action was
    /// refused. The caller should not also treat the byte as a HUD lens key.
    pub fn handleKey(self: *State, byte: u8) bool {
        if (!self.config.enabled) return false;
        switch (byte) {
            'b', 'B' => {
                self.preview(.break_proof);
                return true;
            },
            'r', 'R' => {
                self.preview(.repair_proof);
                return true;
            },
            'y', 'Y' => {
                self.confirmPending();
                return true;
            },
            's', 'S' => {
                self.skip();
                return true;
            },
            else => return false,
        }
    }

    pub fn afterProofRendered(self: *State) void {
        if (!self.config.enabled) return;

        self.mu.lock();
        defer self.mu.unlock();

        switch (self.stage) {
            .waiting_broken_proof => {
                self.stage = .broken;
                self.pending = null;
                self.printStageLocked(.broken);
            },
            .waiting_repaired_proof => {
                self.stage = .complete;
                self.pending = null;
                touchMarker(self.allocator);
                self.printStageLocked(.complete);
            },
            else => {},
        }
    }

    pub fn afterProofFailed(self: *State) void {
        if (!self.config.enabled) return;

        self.mu.lock();
        defer self.mu.unlock();

        if (self.stage == .waiting_broken_proof) {
            self.stage = .broken;
            self.pending = null;
            self.printStageLocked(.broken);
        }
    }

    fn preview(self: *State, action: PendingAction) void {
        self.mu.lock();
        defer self.mu.unlock();

        switch (action) {
            .break_proof => {
                if (self.stage != .baseline and self.stage != .preview_break) {
                    printQuest("press `r` to preview the repair, or `s` to finish the quest.\n", .{});
                    return;
                }
                self.stage = .preview_break;
                self.pending = .break_proof;
                printQuest(
                    "preview: break the deterministic proof. Press `y` to apply, `s` to skip.\n{s}\n",
                    .{fixture.break_diff},
                );
            },
            .repair_proof => {
                if (self.stage == .waiting_broken_proof) {
                    printQuest("wait for the HUD to show `-deterministic`, then press `r`.\n", .{});
                    return;
                }
                if (self.stage != .broken and self.stage != .preview_repair) {
                    printQuest("nothing to repair yet. Press `b` to preview the proof-breaking edit first.\n", .{});
                    return;
                }
                self.stage = .preview_repair;
                self.pending = .repair_proof;
                printQuest(
                    "preview: repair deterministic by removing the nondeterministic value. Press `y` to apply.\n{s}\n",
                    .{fixture.repair_diff},
                );
            },
        }
    }

    fn confirmPending(self: *State) void {
        self.mu.lock();
        const pending = self.pending orelse {
            self.mu.unlock();
            printQuest("nothing previewed. Press `b` to preview the first quest edit.\n", .{});
            return;
        };
        self.mu.unlock();

        const source = zigts.file_io.readFile(self.allocator, self.handler_path, 1024 * 1024) catch |err| {
            printQuest("could not read {s}: {}.\n", .{ self.handler_path, err });
            return;
        };
        defer self.allocator.free(source);

        const next_source, const next_stage = switch (pending) {
            .break_proof => blk: {
                if (fixture.classifySource(source) != .starter) {
                    printQuest("refused: handler no longer matches the scaffolded starter. No files changed.\n", .{});
                    return;
                }
                break :blk .{ fixture.broken_source, Stage.waiting_broken_proof };
            },
            .repair_proof => blk: {
                if (fixture.classifySource(source) != .broken) {
                    printQuest("refused: handler is not at the quest's broken state. No files changed.\n", .{});
                    return;
                }
                break :blk .{ fixture.repaired_source, Stage.waiting_repaired_proof };
            },
        };

        zigts.file_io.writeFile(self.allocator, self.handler_path, next_source) catch |err| {
            printQuest("could not write {s}: {}.\n", .{ self.handler_path, err });
            return;
        };

        self.mu.lock();
        defer self.mu.unlock();
        self.stage = next_stage;
        self.pending = null;
        self.announced = .idle;
        switch (pending) {
            .break_proof => printQuest("applied unsafe edit. Waiting for the HUD to show `-deterministic`.\n", .{}),
            .repair_proof => printQuest("applied repair. Waiting for the HUD to turn green again.\n", .{}),
        }
    }

    fn skip(self: *State) void {
        self.mu.lock();
        defer self.mu.unlock();
        self.stage = .complete;
        self.pending = null;
        touchMarker(self.allocator);
        printQuest("skipped. Continue editing; the HUD will keep proving every save.\n", .{});
    }

    fn printStageLocked(self: *State, stage: Stage) void {
        if (self.announced == stage) return;
        self.announced = stage;
        switch (stage) {
            .baseline => printQuest(
                "Proof Quest: press `b` to preview a tiny edit that breaks `deterministic`; `s` skips.\n",
                .{},
            ),
            .broken => printQuest(
                "Proof Quest: the proof is red. Press `r` to preview the repair, then `y` to apply it.\n",
                .{},
            ),
            .complete => printQuest(
                "complete. Next: `zigttp check`, `zigttp build`, `zigttp deploy`, then `zigttp proofs badge`.\n",
                .{},
            ),
            else => {},
        }
    }
};

fn snapshotFor(stage: Stage, explicit: bool) Snapshot {
    return switch (stage) {
        .idle => .{
            .enabled = true,
            .explicit = explicit,
            .stage = "idle",
            .title = "Proof Passport",
            .message = "Waiting for the scaffolded starter handler.",
            .available_actions = &no_actions,
            .complete = false,
        },
        .baseline => .{
            .enabled = true,
            .explicit = explicit,
            .stage = "baseline",
            .title = "Proof Passport",
            .message = "Break one declared proof on purpose: press b to preview a deterministic regression.",
            .available_actions = &baseline_actions,
            .complete = false,
        },
        .preview_break => .{
            .enabled = true,
            .explicit = explicit,
            .stage = "preview_break",
            .title = "Proof Passport",
            .message = "Preview is staged. Press y to apply the unsafe edit and watch deterministic flip red.",
            .available_actions = &preview_break_actions,
            .complete = false,
        },
        .waiting_broken_proof => .{
            .enabled = true,
            .explicit = explicit,
            .stage = "waiting_broken_proof",
            .title = "Proof Passport",
            .message = "Unsafe edit applied. Waiting for the proof HUD to show -deterministic.",
            .available_actions = &no_actions,
            .complete = false,
        },
        .broken => .{
            .enabled = true,
            .explicit = explicit,
            .stage = "broken",
            .title = "Proof Passport",
            .message = "The proof is red. Press r to preview the repair, then y to restore the guardrail.",
            .available_actions = &broken_actions,
            .complete = false,
        },
        .preview_repair => .{
            .enabled = true,
            .explicit = explicit,
            .stage = "preview_repair",
            .title = "Proof Passport",
            .message = "Repair preview is staged. Press y to apply it and watch the proof return green.",
            .available_actions = &preview_repair_actions,
            .complete = false,
        },
        .waiting_repaired_proof => .{
            .enabled = true,
            .explicit = explicit,
            .stage = "waiting_repaired_proof",
            .title = "Proof Passport",
            .message = "Repair applied. Waiting for the proof HUD to turn green again.",
            .available_actions = &no_actions,
            .complete = false,
        },
        .complete => .{
            .enabled = true,
            .explicit = explicit,
            .stage = "complete",
            .title = "Proof Passport",
            .message = "Passport complete. Build, deploy locally, then export the proof badge.",
            .available_actions = &complete_actions,
            .complete = true,
        },
    };
}

pub fn markerPath() []const u8 {
    return ".zigttp/tour-shown";
}

fn markerExists(allocator: std.mem.Allocator) bool {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    std.Io.Dir.access(std.Io.Dir.cwd(), io_backend.io(), markerPath(), .{}) catch return false;
    return true;
}

fn touchMarker(allocator: std.mem.Allocator) void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, ".zigttp") catch return;
    var file = std.Io.Dir.createFile(std.Io.Dir.cwd(), io, markerPath(), .{}) catch return;
    file.close(io);
}

fn printQuest(comptime fmt: []const u8, args: anytype) void {
    const c = shared.palette(shared.stderrIsTty());
    std.debug.print("{s}[quest]{s} " ++ fmt, .{ c.cyan, c.reset } ++ args);
}

test "marker path stays compatible with the original first-run tour" {
    try std.testing.expectEqualStrings(".zigttp/tour-shown", markerPath());
}

test "snapshot exposes stage-specific passport actions" {
    const baseline = snapshotFor(.baseline, false);
    try std.testing.expect(baseline.enabled);
    try std.testing.expectEqualStrings("baseline", baseline.stage);
    try std.testing.expectEqualStrings("b", baseline.available_actions[0]);
    try std.testing.expect(!baseline.complete);

    const complete = snapshotFor(.complete, true);
    try std.testing.expect(complete.explicit);
    try std.testing.expect(complete.complete);
    try std.testing.expectEqualStrings("complete", complete.stage);
    try std.testing.expectEqualStrings("zigttp deploy", complete.available_actions[2]);
}
