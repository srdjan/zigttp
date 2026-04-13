const std = @import("std");
const auth = @import("auth.zig");
const control_plane = @import("control_plane.zig");
const io_util = @import("io_util.zig");
const printer_mod = @import("printer.zig");

const Printer = printer_mod.Printer;

pub fn ensureSignedIn(allocator: std.mem.Allocator, printer: Printer) !auth.Credentials {
    if (auth.load(allocator)) |creds| {
        return creds;
    } else |err| switch (err) {
        error.NotSignedIn => return try interactiveLogin(allocator, printer),
        else => return err,
    }
}

fn interactiveLogin(allocator: std.mem.Allocator, printer: Printer) !auth.Credentials {
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
                if (r.email) |email| {
                    const ok_msg = try std.fmt.allocPrint(allocator, "Signed in as {s}.\n", .{email});
                    defer allocator.free(ok_msg);
                    printer.write(ok_msg);
                }
                return try auth.load(allocator);
            },
        }
    }
    return error.LoginExpired;
}
