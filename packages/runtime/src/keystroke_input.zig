//! Minimal POSIX keystroke listener for the dev-loop HUD.
//!
//! Puts stdin in cbreak mode, spawns a detached worker that reads single
//! bytes, and installs a SIGINT/SIGTERM handler that restores termios
//! before the default exit. Returns null on non-POSIX or non-TTY. The
//! worker thread is detached because a blocking read would otherwise
//! hang shutdown; one listener per process by design.

const std = @import("std");
const builtin = @import("builtin");

const is_posix = builtin.os.tag != .windows;

pub const KeyCallback = *const fn (ctx: *anyopaque, byte: u8) void;

/// Opaque to callers; fields are internal and may change.
pub const Handle = struct {
    allocator: std.mem.Allocator,
    saved_termios: if (is_posix) std.posix.termios else void,
    installed: bool,
};

const ListenerArgs = struct {
    ctx: *anyopaque,
    on_key: KeyCallback,
};

var g_active_handle: ?*Handle = null;
var signal_handlers_installed: bool = false;

/// Install the keystroke listener. Returns null on non-POSIX, non-TTY,
/// or termios failure. Caller invokes `uninstall` on graceful shutdown.
pub fn install(
    allocator: std.mem.Allocator,
    ctx: *anyopaque,
    on_key: KeyCallback,
) ?*Handle {
    if (!is_posix) return null;
    if (std.posix.system.isatty(std.posix.STDIN_FILENO) == 0) return null;
    if (std.posix.system.isatty(std.posix.STDERR_FILENO) == 0) return null;

    const handle = allocator.create(Handle) catch return null;
    handle.* = .{
        .allocator = allocator,
        .saved_termios = undefined,
        .installed = false,
    };

    const saved = std.posix.tcgetattr(std.posix.STDIN_FILENO) catch {
        allocator.destroy(handle);
        return null;
    };
    handle.saved_termios = saved;

    var raw = saved;
    raw.lflag.ICANON = false;
    raw.lflag.ECHO = false;
    raw.cc[@intFromEnum(std.posix.V.MIN)] = 1;
    raw.cc[@intFromEnum(std.posix.V.TIME)] = 0;

    std.posix.tcsetattr(std.posix.STDIN_FILENO, .FLUSH, raw) catch {
        allocator.destroy(handle);
        return null;
    };
    handle.installed = true;

    g_active_handle = handle;
    installSignalHandlers();

    // ListenerArgs intentionally leaks: the detached thread runs until
    // process exit and the OS reclaims the small allocation.
    const args = allocator.create(ListenerArgs) catch {
        uninstall(handle);
        return null;
    };
    args.* = .{ .ctx = ctx, .on_key = on_key };

    const thread = std.Thread.spawn(.{}, listenLoop, .{args}) catch {
        allocator.destroy(args);
        uninstall(handle);
        return null;
    };
    thread.detach();

    return handle;
}

/// Restore the original terminal mode and free the handle. Idempotent.
pub fn uninstall(handle: *Handle) void {
    if (!is_posix) {
        handle.allocator.destroy(handle);
        return;
    }
    if (handle.installed) {
        std.posix.tcsetattr(std.posix.STDIN_FILENO, .FLUSH, handle.saved_termios) catch {};
        handle.installed = false;
    }
    if (g_active_handle == handle) g_active_handle = null;
    handle.allocator.destroy(handle);
}

fn listenLoop(args: *ListenerArgs) void {
    var buf: [1]u8 = undefined;
    while (true) {
        const n = std.posix.read(std.posix.STDIN_FILENO, buf[0..]) catch return;
        if (n == 0) return;
        args.on_key(args.ctx, buf[0]);
    }
}

fn installSignalHandlers() void {
    if (!is_posix) return;
    if (signal_handlers_installed) return;
    signal_handlers_installed = true;

    var act = std.posix.Sigaction{
        .handler = .{ .handler = restoreOnSignal },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    };
    std.posix.sigaction(std.posix.SIG.INT, &act, null);
    std.posix.sigaction(std.posix.SIG.TERM, &act, null);
}

fn restoreOnSignal(sig: std.posix.SIG) callconv(.c) void {
    if (g_active_handle) |h| {
        if (h.installed) {
            std.posix.tcsetattr(std.posix.STDIN_FILENO, .FLUSH, h.saved_termios) catch {};
            h.installed = false;
        }
    }
    var default_act = std.posix.Sigaction{
        .handler = .{ .handler = std.posix.SIG.DFL },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    };
    std.posix.sigaction(sig, &default_act, null);
    std.posix.raise(sig) catch {};
}
