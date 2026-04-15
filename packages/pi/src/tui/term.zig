//! Raw mode entry/exit for stdin. Ctrl-C is handled as byte 0x03 in the read
//! loop (ISIG is off), which sidesteps signal handlers and lets the normal
//! `defer exit()` restore the terminal on any exit path short of a panic.
//!
//! Panic recovery is a known phase-1 gap: if the process crashes while raw
//! mode is active, the user must run `reset` to recover their terminal.

const std = @import("std");

pub const Error = error{
    TcgetattrFailed,
    TcsetattrFailed,
};

pub const RawMode = struct {
    original: std.c.termios,
    fd: std.c.fd_t,

    pub fn enter() Error!RawMode {
        const fd = std.c.STDIN_FILENO;
        var original: std.c.termios = undefined;
        if (std.c.tcgetattr(fd, &original) != 0) return Error.TcgetattrFailed;

        var raw = original;
        raw.iflag.BRKINT = false;
        raw.iflag.ICRNL = false;
        raw.iflag.INPCK = false;
        raw.iflag.ISTRIP = false;
        raw.iflag.IXON = false;
        raw.oflag.OPOST = false;
        raw.lflag.ECHO = false;
        raw.lflag.ICANON = false;
        raw.lflag.IEXTEN = false;
        raw.lflag.ISIG = false;

        if (std.c.tcsetattr(fd, std.c.TCSA.FLUSH, &raw) != 0) return Error.TcsetattrFailed;

        return .{ .original = original, .fd = fd };
    }

    pub fn exit(self: *RawMode) void {
        _ = std.c.tcsetattr(self.fd, std.c.TCSA.FLUSH, &self.original);
    }
};
