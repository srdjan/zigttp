//! Job and outcome data types for the TUI worker thread. Pure data, no
//! TuiRuntime/AppState references. WorkerCtx (which carries the
//! TuiRuntime pointer) stays in app.zig.

const autoloop = @import("../autoloop.zig");
const witness_replay = @import("../witness_replay.zig");
const ui_payload_mod = @import("../ui_payload.zig");

pub const DriveJob = struct {
    workspace_root: []u8,
    file: []u8,
    name: []const u8,
    focus_key: ?[]u8,
    events_path: ?[]u8,
};

pub const ReplayJob = struct {
    workspace_root: []u8,
    handler_path: []u8,
    witness: ui_payload_mod.WitnessBody,
};

pub const DriveSuccess = struct {
    name: []const u8,
    verdict: autoloop.AutoloopVerdict,
};

pub const DriveOutcome = union(enum) {
    success: DriveSuccess,
    dispatch_error: []const u8,
    cancelled: []const u8,
};

pub const ReplaySuccess = struct {
    verdict: witness_replay.Verdict,
    key: []u8,
    reproduced: bool,
};

pub const ReplayOutcome = union(enum) {
    success: ReplaySuccess,
    err: []const u8,
    cancelled,
};

pub const WorkerJob = union(enum) {
    drive: DriveJob,
    replay: ReplayJob,
};

pub const WorkerResult = union(enum) {
    drive: DriveOutcome,
    replay: ReplayOutcome,
};
