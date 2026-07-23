//! First-run tour rendering and durable dismissal marker.
//!
//! Extracted from dev_cli.zig. The tour text is rendered once per project,
//! the first time `zttp dev` is invoked; the marker file at
//! `.zttp/tour-shown` records dismissal and is checked on every
//! subsequent run. Skipped when stderr is not a TTY (CI, redirected logs)
//! or when the user passes `--no-tour`.

const std = @import("std");
const shared = @import("cli_shared.zig");

/// Path of the marker file that records "first-run tour was shown."
/// Once this file exists, `zttp dev` skips the tour on subsequent runs.
pub fn tourMarkerPath() []const u8 {
    return ".zttp/tour-shown";
}

/// First-run tour copy. One screen, no prompts, dismissed by writing the
/// marker file the moment we render. Designed to land just before the HUD
/// from `serve --watch --prove` starts streaming, so the author reads what
/// the four properties mean and then sees them flip live.
pub const tour_text =
    \\
    \\  zttp dev   proof-aware live reload
    \\  ----------------------------------------------------------------------
    \\  every save is recompiled, then proven. the hud frame below shows your
    \\  handler's proof surface in real time. four properties to watch:
    \\
    \\    pure              no virtual-module calls
    \\    read_only         no state mutations
    \\    deterministic     no Date.now() / Math.random()
    \\    injection_safe    user input never reaches sensitive sinks
    \\
    \\  the starter declares `Spec<"deterministic" | "no_secret_leakage">` on
    \\  its return type. that is the author-declared proof obligation the
    \\  compiler discharges on every save.
    \\
    \\  try it: drop a `Date.now()` into the handler body and watch
    \\  -deterministic light up. revert it and watch +deterministic come back.
    \\  the proof card streams below.
    \\
    \\  press `tab` in the hud to rotate the proof lens:
    \\    Properties   the proven `[+]` / `[-]` pills (default)
    \\    Trade        each proof paired with what the substrate gave up
    \\    Handover     a copy-pasteable proof certificate for ai agents
    \\
    \\  this is also why ai coding agents work well with zttp: every
    \\  restriction is a guarantee the agent can rely on while it refactors.
    \\
    \\
;

pub fn tourMarkerExistsAt(allocator: std.mem.Allocator, base_dir: []const u8) bool {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    const path = std.fs.path.join(allocator, &.{ base_dir, tourMarkerPath() }) catch return false;
    defer allocator.free(path);
    std.Io.Dir.access(std.Io.Dir.cwd(), io, path, .{}) catch return false;
    return true;
}

pub fn touchTourMarkerAt(allocator: std.mem.Allocator, base_dir: []const u8) void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    const state_dir = std.fs.path.join(allocator, &.{ base_dir, ".zttp" }) catch return;
    defer allocator.free(state_dir);
    std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, state_dir) catch return;
    const path = std.fs.path.join(allocator, &.{ base_dir, tourMarkerPath() }) catch return;
    defer allocator.free(path);
    var file = std.Io.Dir.createFile(std.Io.Dir.cwd(), io, path, .{}) catch return;
    file.close(io);
}

pub fn tourMarkerExists(allocator: std.mem.Allocator) bool {
    return tourMarkerExistsAt(allocator, ".");
}

pub fn touchTourMarker(allocator: std.mem.Allocator) void {
    touchTourMarkerAt(allocator, ".");
}

/// True when the user opted out of the first-run tour and quest. `--no-quest`
/// is the documented name; `--no-tour` is kept as an alias. Defined here so the
/// alias set lives in one place for both the tour gate and the dev quest gate.
pub fn skipRequested(argv: []const []const u8) bool {
    return shared.hasFlag(argv, "--no-tour") or shared.hasFlag(argv, "--no-quest");
}

/// Render the first-run tour exactly once, the first time `zttp dev` is
/// invoked in a project. Dismissal is durable (marker file). Skipped when
/// stderr is not a TTY (CI, redirected logs) or `--no-quest`/`--no-tour` passed.
pub fn maybeShowFirstRunTour(allocator: std.mem.Allocator, argv: []const []const u8) void {
    if (skipRequested(argv)) return;
    if (!shared.stderrIsTty()) return;
    if (tourMarkerExists(allocator)) return;
    _ = std.c.write(std.c.STDERR_FILENO, tour_text.ptr, tour_text.len);
    touchTourMarker(allocator);
}
