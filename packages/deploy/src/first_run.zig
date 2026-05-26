const builtin = @import("builtin");
const std = @import("std");
const auth = @import("auth.zig");
const control_plane = @import("control_plane.zig");
const io_util = @import("io_util.zig");
const printer_mod = @import("printer.zig");

const Printer = printer_mod.Printer;

pub const LoginMode = enum {
    auto,
    prompt,
    token_stdin,
    device,
};

pub fn ensureSignedIn(allocator: std.mem.Allocator, printer: Printer) !auth.Credentials {
    if (auth.load(allocator)) |creds| {
        return creds;
    } else |err| switch (err) {
        error.NotSignedIn => return try login(allocator, printer, .auto),
        else => return err,
    }
}

pub fn login(allocator: std.mem.Allocator, printer: Printer, mode: LoginMode) !auth.Credentials {
    return switch (mode) {
        .auto => loginAuto(allocator, printer),
        .prompt => loginPrompt(allocator, printer),
        .token_stdin => loginFromStdin(allocator, printer),
        .device => deviceLogin(allocator, printer),
    };
}

fn loginAuto(allocator: std.mem.Allocator, printer: Printer) !auth.Credentials {
    if (!stdinIsTty()) return deviceLogin(allocator, printer);

    const token = try promptForToken(allocator, printer);
    defer allocator.free(token);

    if (token.len == 0) return deviceLogin(allocator, printer);

    return saveValidatedToken(allocator, printer, token) catch |err| switch (err) {
        error.InvalidToken => {
            printer.warn("Token rejected. Falling back to browser login.");
            return deviceLogin(allocator, printer);
        },
        else => return err,
    };
}

fn loginPrompt(allocator: std.mem.Allocator, printer: Printer) !auth.Credentials {
    const token = try promptForToken(allocator, printer);
    defer allocator.free(token);

    if (token.len == 0) return deviceLogin(allocator, printer);
    return saveValidatedToken(allocator, printer, token);
}

fn loginFromStdin(allocator: std.mem.Allocator, printer: Printer) !auth.Credentials {
    const token = try readTokenFromStdin(allocator);
    defer allocator.free(token);

    if (token.len == 0) return error.EmptyToken;
    return saveValidatedToken(allocator, printer, token);
}

fn saveValidatedToken(allocator: std.mem.Allocator, printer: Printer, token: []const u8) !auth.Credentials {
    var identity = try control_plane.validateToken(allocator, token);
    defer identity.deinit(allocator);

    const owned_token = try allocator.dupe(u8, token);
    errdefer allocator.free(owned_token);
    const owned_email: ?[]u8 = if (identity.email) |value|
        try allocator.dupe(u8, value)
    else
        null;
    errdefer if (owned_email) |value| allocator.free(value);

    try auth.save(allocator, .{ .token = owned_token, .email = owned_email });
    try printSignedInMessage(allocator, printer, owned_email);
    return .{ .token = owned_token, .email = owned_email };
}

fn deviceLogin(allocator: std.mem.Allocator, printer: Printer) !auth.Credentials {
    var io_backend = io_util.threadedIo(allocator);
    defer io_backend.deinit();
    const io = io_backend.io();

    var challenge = try control_plane.startLogin(allocator);
    defer challenge.deinit(allocator);

    const stderr_msg = try std.fmt.allocPrint(
        allocator,
        "You are not signed in.\nOpen {s}\nWaiting for confirmation...\n",
        .{challenge.verification_url},
    );
    defer allocator.free(stderr_msg);
    printer.write(stderr_msg);

    const interval_s = std.math.clamp(challenge.interval_seconds, 1, 60);
    const deadline_s = std.math.clamp(challenge.expires_in_seconds, 60, 3600);
    const interval_ms: u32 = interval_s * 1000;
    const deadline_ms: u32 = deadline_s * 1000;
    var elapsed_ms: u32 = 0;

    while (elapsed_ms < deadline_ms) {
        std.Io.sleep(io, .fromMilliseconds(interval_ms), .awake) catch {};
        elapsed_ms += interval_ms;
        const outcome = try control_plane.pollLogin(allocator, challenge.device_code);
        switch (outcome) {
            .pending => continue,
            .expired => return error.LoginExpired,
            .denied => return error.LoginDenied,
            .done => |result| {
                var r = result;
                defer r.deinit(allocator);
                try auth.save(allocator, .{
                    .token = r.token,
                    .email = r.email,
                });
                try printSignedInMessage(allocator, printer, r.email);
                return try auth.load(allocator);
            },
        }
    }
    return error.LoginExpired;
}

fn printSignedInMessage(allocator: std.mem.Allocator, printer: Printer, email: ?[]const u8) !void {
    const ok_msg = if (email) |value|
        try std.fmt.allocPrint(allocator, "Signed in as {s}.\n", .{value})
    else
        try allocator.dupe(u8, "Signed in.\n");
    defer allocator.free(ok_msg);
    printer.write(ok_msg);
}

fn stdinIsTty() bool {
    return switch (builtin.os.tag) {
        .windows => false,
        else => std.posix.system.isatty(std.posix.STDIN_FILENO) != 0,
    };
}

fn promptForToken(allocator: std.mem.Allocator, printer: Printer) ![]u8 {
    if (!stdinIsTty()) return error.TokenPromptUnavailable;

    printer.write("Paste Zigttp token (input hidden). Press Enter on an empty line to use browser login.\nToken: ");
    const token = try readSecretLine(allocator);
    printer.write("\n");
    return token;
}

fn readSecretLine(allocator: std.mem.Allocator) ![]u8 {
    if (builtin.os.tag == .windows) return error.TokenPromptUnavailable;

    const original = try std.posix.tcgetattr(std.posix.STDIN_FILENO);
    var hidden = original;
    hidden.lflag.ECHO = false;
    try std.posix.tcsetattr(std.posix.STDIN_FILENO, .FLUSH, hidden);
    defer std.posix.tcsetattr(std.posix.STDIN_FILENO, .FLUSH, original) catch {};

    return try readLineFromStdin(allocator);
}

fn readTokenFromStdin(allocator: std.mem.Allocator) ![]u8 {
    const raw = try readAllStdin(allocator);
    errdefer allocator.free(raw);
    return shrinkToTrimmed(allocator, raw);
}

fn readLineFromStdin(allocator: std.mem.Allocator) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(allocator);

    var byte: [1]u8 = undefined;
    while (true) {
        const n = std.posix.read(std.posix.STDIN_FILENO, &byte) catch |err| switch (err) {
            error.WouldBlock => continue,
            else => return err,
        };
        if (n == 0) break;
        if (byte[0] == '\n' or byte[0] == '\r') break;
        try buf.append(allocator, byte[0]);
    }

    const raw = try buf.toOwnedSlice(allocator);
    errdefer allocator.free(raw);
    return shrinkToTrimmed(allocator, raw);
}

fn shrinkToTrimmed(allocator: std.mem.Allocator, raw: []u8) ![]u8 {
    const trimmed = std.mem.trim(u8, raw, " \t\r\n");
    if (trimmed.len == raw.len) return raw;
    const start: usize = @intFromPtr(trimmed.ptr) - @intFromPtr(raw.ptr);
    if (start > 0) std.mem.copyForwards(u8, raw[0..trimmed.len], raw[start..][0..trimmed.len]);
    return allocator.realloc(raw, trimmed.len);
}

fn readAllStdin(allocator: std.mem.Allocator) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(allocator);

    var read_buf: [4096]u8 = undefined;
    while (true) {
        const n = std.posix.read(std.posix.STDIN_FILENO, &read_buf) catch |err| switch (err) {
            error.WouldBlock => continue,
            else => return err,
        };
        if (n == 0) break;
        try buf.appendSlice(allocator, read_buf[0..n]);
    }
    return buf.toOwnedSlice(allocator);
}
