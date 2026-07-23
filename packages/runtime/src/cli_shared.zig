const std = @import("std");
const builtin = @import("builtin");
const project_config_mod = @import("project_config");
const zts = @import("zts");

/// True when stderr is attached to a real terminal. Used to gate ANSI escape
/// codes so piped output and CI logs stay clean.
pub fn stderrIsTty() bool {
    return switch (builtin.os.tag) {
        .windows => false,
        else => std.posix.system.isatty(std.posix.STDERR_FILENO) != 0,
    };
}

/// ANSI escape codes resolved at call time from a tty flag. Pass `false` for
/// non-TTY destinations and every field becomes an empty string, so the same
/// format strings produce clean output in both modes.
pub const Palette = struct {
    reset: []const u8,
    bold: []const u8,
    dim: []const u8,
    red: []const u8,
    green: []const u8,
    yellow: []const u8,
    cyan: []const u8,
};

pub fn palette(tty: bool) Palette {
    return if (tty) .{
        .reset = "\x1b[0m",
        .bold = "\x1b[1m",
        .dim = "\x1b[2m",
        .red = "\x1b[31m",
        .green = "\x1b[32m",
        .yellow = "\x1b[33m",
        .cyan = "\x1b[36m",
    } else .{
        .reset = "",
        .bold = "",
        .dim = "",
        .red = "",
        .green = "",
        .yellow = "",
        .cyan = "",
    };
}

/// Emit a phase-start line to stderr so a long-running local command shows
/// progress instead of going silent until it finishes. Dimmed on a TTY,
/// plain when piped. Stderr-only so `--json` stdout stays machine-pure;
/// mirrors the `deploy --cloud` step-line style for a consistent feel.
pub fn step(label: []const u8) void {
    const p = palette(stderrIsTty());
    std.debug.print("  {s}{s}{s}\n", .{ p.dim, label, p.reset });
}

pub fn collectArgs(allocator: std.mem.Allocator, args_vector: std.process.Args) ![]const []const u8 {
    var args_iter = std.process.Args.Iterator.init(args_vector);
    defer args_iter.deinit();

    var args = std.ArrayList([]const u8).empty;
    errdefer args.deinit(allocator);
    while (args_iter.next()) |arg| {
        try args.append(allocator, try allocator.dupe(u8, arg));
    }
    return args.toOwnedSlice(allocator);
}

pub fn threadedIo(allocator: std.mem.Allocator) std.Io.Threaded {
    return std.Io.Threaded.init(allocator, .{ .environ = .empty });
}

pub fn realCwd(allocator: std.mem.Allocator) ![]u8 {
    var io_backend = threadedIo(allocator);
    defer io_backend.deinit();
    const cwd_z = try std.Io.Dir.realPathFileAlloc(std.Io.Dir.cwd(), io_backend.io(), ".", allocator);
    defer allocator.free(cwd_z);
    return try allocator.dupe(u8, cwd_z);
}

pub fn nowUnixMs() i64 {
    var ts: std.posix.timespec = undefined;
    _ = std.c.clock_gettime(@enumFromInt(@intFromEnum(std.posix.CLOCK.REALTIME)), &ts);
    return @as(i64, ts.sec) * 1000 + @divTrunc(@as(i64, ts.nsec), 1_000_000);
}

pub fn takeArg(i: *usize, argv: []const []const u8, missing: anyerror) ![]const u8 {
    i.* += 1;
    if (i.* >= argv.len) return missing;
    return argv[i.*];
}

/// Strip TypeScript annotations from an inline `-e`/`--eval` snippet so it
/// matches the file path, where `.ts`/`.tsx` handlers are stripped before
/// parsing. Without this, `-e "const x: number = 5"` reaches the parser raw
/// and fails, while the identical code in a file loads fine.
///
/// Returns an owned buffer; the caller owns it (the serve path leaks the
/// handler source for the process lifetime). If the stripper rejects the
/// source (an unsupported construct, OOM), the original is duped through
/// unchanged so plain-JS `-e` keeps its existing behavior and the parser
/// produces the same diagnostic it always did.
pub fn stripInlineSource(allocator: std.mem.Allocator, source: []const u8) ![]const u8 {
    var strip_result = zts.strip(allocator, source, .{ .report_errors = false }) catch {
        return allocator.dupe(u8, source);
    };
    defer strip_result.deinit();
    return allocator.dupe(u8, strip_result.code);
}

pub fn printVersion() void {
    const version = "zttp " ++ zts.version.string ++ "\n";
    _ = std.c.write(std.c.STDOUT_FILENO, version.ptr, version.len);
}

pub fn writeStdoutLine(s: []const u8) void {
    _ = std.c.write(std.c.STDOUT_FILENO, s.ptr, s.len);
    _ = std.c.write(std.c.STDOUT_FILENO, "\n", 1);
}

/// Report that an opt-in feature was compiled out of this build and exit
/// non-zero. `name` is the command (e.g. "studio"), `flag` the build option
/// to rebuild with (e.g. "studio" for `zig build -Dstudio`).
pub fn featureCompiledOut(name: []const u8, flag: []const u8) noreturn {
    std.debug.print("zttp {s} was compiled out of this build. Rebuild with: zig build -D{s}\n", .{ name, flag });
    std.process.exit(1);
}

pub fn findPositionalPath(argv: []const []const u8) ?[]const u8 {
    var skip_next = false;
    for (argv) |arg| {
        if (skip_next) {
            skip_next = false;
            continue;
        }
        if (std.mem.eql(u8, arg, "-p") or
            std.mem.eql(u8, arg, "--port") or
            std.mem.eql(u8, arg, "-h") or
            std.mem.eql(u8, arg, "--host") or
            std.mem.eql(u8, arg, "-e") or
            std.mem.eql(u8, arg, "--eval") or
            std.mem.eql(u8, arg, "-m") or
            std.mem.eql(u8, arg, "--memory") or
            std.mem.eql(u8, arg, "-n") or
            std.mem.eql(u8, arg, "--pool") or
            std.mem.eql(u8, arg, "--static") or
            std.mem.eql(u8, arg, "--outbound-host") or
            std.mem.eql(u8, arg, "--outbound-timeout-ms") or
            std.mem.eql(u8, arg, "--outbound-max-response") or
            std.mem.eql(u8, arg, "--max-body-size") or
            std.mem.eql(u8, arg, "--security-log") or
            std.mem.eql(u8, arg, "--lifecycle") or
            std.mem.eql(u8, arg, "--sqlite") or
            std.mem.eql(u8, arg, "--trace") or
            std.mem.eql(u8, arg, "--incident-log") or
            std.mem.eql(u8, arg, "--replay") or
            std.mem.eql(u8, arg, "--test") or
            std.mem.eql(u8, arg, "--durable") or
            std.mem.eql(u8, arg, "--system"))
        {
            skip_next = true;
            continue;
        }
        if (std.mem.startsWith(u8, arg, "-")) continue;
        return arg;
    }
    return null;
}

pub fn hasFlag(argv: []const []const u8, name: []const u8) bool {
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, name)) return true;
    }
    return false;
}

fn looksLikeHandlerFile(path: []const u8) bool {
    return std.mem.endsWith(u8, path, ".js") or
        std.mem.endsWith(u8, path, ".jsx") or
        std.mem.endsWith(u8, path, ".ts") or
        std.mem.endsWith(u8, path, ".tsx");
}

pub fn parseSize(str: []const u8) !usize {
    var num_end: usize = 0;
    for (str, 0..) |c, i| {
        if (c >= '0' and c <= '9') {
            num_end = i + 1;
        } else {
            break;
        }
    }

    if (num_end == 0) return error.InvalidSize;

    const num = try std.fmt.parseInt(usize, str[0..num_end], 10);
    const suffix = str[num_end..];

    const multiplier: usize = if (suffix.len == 0)
        1
    else if (std.ascii.eqlIgnoreCase(suffix, "k") or std.ascii.eqlIgnoreCase(suffix, "kb"))
        1024
    else if (std.ascii.eqlIgnoreCase(suffix, "m") or std.ascii.eqlIgnoreCase(suffix, "mb"))
        1024 * 1024
    else if (std.ascii.eqlIgnoreCase(suffix, "g") or std.ascii.eqlIgnoreCase(suffix, "gb"))
        1024 * 1024 * 1024
    else
        return error.InvalidSizeSuffix;

    return std.math.mul(usize, num, multiplier) catch return error.InvalidSize;
}

pub const WatchSet = struct {
    paths: []const []const u8,

    pub fn deinit(self: *WatchSet, allocator: std.mem.Allocator) void {
        for (self.paths) |path| allocator.free(path);
        allocator.free(self.paths);
    }

    pub fn computeStamp(self: *const WatchSet, io: std.Io) !u64 {
        var hash = std.hash.Wyhash.init(0);
        for (self.paths) |path| {
            try foldPathIntoHash(io, &hash, path);
        }
        return hash.final();
    }
};

pub fn buildWatchSet(allocator: std.mem.Allocator, argv: []const []const u8) !WatchSet {
    var io_backend = threadedIo(allocator);
    defer io_backend.deinit();
    const io = io_backend.io();

    const explicit_path = findPositionalPath(argv);
    var project = try project_config_mod.discover(allocator, io, explicit_path);
    defer if (project) |*p| p.deinit(allocator);

    var paths = std.ArrayList([]const u8).empty;
    errdefer {
        for (paths.items) |path| allocator.free(path);
        paths.deinit(allocator);
    }

    if (project) |*cfg| {
        try paths.append(allocator, try allocator.dupe(u8, cfg.manifest_path));
        try paths.append(allocator, try std.fs.path.resolve(allocator, &.{ cfg.root_dir, "src" }));
        if (try cfg.resolvedStaticDir(allocator)) |static_dir| {
            try paths.append(allocator, static_dir);
        }
        if (try cfg.resolvedSystemPath(allocator)) |system_path| {
            try paths.append(allocator, system_path);
        }
    } else if (explicit_path) |path| {
        if (looksLikeHandlerFile(path)) {
            const abs = try std.fs.path.resolve(allocator, &.{path});
            const dir_name = std.fs.path.dirname(abs) orelse ".";
            defer allocator.free(abs);
            try paths.append(allocator, try allocator.dupe(u8, dir_name));
        }
    }

    if (paths.items.len == 0) {
        try paths.append(allocator, try std.fs.path.resolve(allocator, &.{"."}));
    }

    return .{ .paths = try paths.toOwnedSlice(allocator) };
}

pub fn foldPathIntoHash(io: std.Io, hash: *std.hash.Wyhash, path: []const u8) !void {
    const stat = std.Io.Dir.statFile(std.Io.Dir.cwd(), io, path, .{}) catch |err| switch (err) {
        error.FileNotFound, error.NotDir => return,
        else => return err,
    };

    hash.update(path);
    hash.update(std.mem.asBytes(&stat.mtime.nanoseconds));
    hash.update(std.mem.asBytes(&stat.size));

    if (stat.kind != .directory) return;

    var dir = try std.Io.Dir.openDir(std.Io.Dir.cwd(), io, path, .{ .iterate = true });
    defer dir.close(io);

    var walker = try dir.walk(std.heap.smp_allocator);
    defer walker.deinit();

    while (try walker.next(io)) |entry| {
        const entry_stat = try std.Io.Dir.statFile(entry.dir, io, entry.basename, .{});
        hash.update(entry.path);
        hash.update(std.mem.asBytes(&entry_stat.mtime.nanoseconds));
        hash.update(std.mem.asBytes(&entry_stat.size));
    }
}

test "parse size" {
    try std.testing.expectEqual(@as(usize, 1024), try parseSize("1k"));
    try std.testing.expectEqual(@as(usize, 1024), try parseSize("1K"));
    try std.testing.expectEqual(@as(usize, 1024), try parseSize("1kb"));
    try std.testing.expectEqual(@as(usize, 256 * 1024), try parseSize("256k"));
    try std.testing.expectEqual(@as(usize, 1024 * 1024), try parseSize("1m"));
    try std.testing.expectEqual(@as(usize, 1024 * 1024), try parseSize("1mb"));
    try std.testing.expectEqual(@as(usize, 100), try parseSize("100"));
}

test "stripInlineSource strips TS annotations ENG7" {
    const allocator = std.testing.allocator;

    // A type annotation that would be a parse error if fed raw to the parser
    // must be stripped, matching the file path.
    const ts_src = "function handler(req) { const x: number = 5; return Response.text(String(x)); }";
    const stripped = try stripInlineSource(allocator, ts_src);
    defer allocator.free(stripped);
    try std.testing.expect(std.mem.indexOf(u8, stripped, ": number") == null);
    try std.testing.expect(std.mem.indexOf(u8, stripped, "const x") != null);

    // Plain JS round-trips unchanged so `-e` of pure JS keeps its behavior.
    const js_src = "function handler(req) { return Response.json({ok:true}); }";
    const passthrough = try stripInlineSource(allocator, js_src);
    defer allocator.free(passthrough);
    try std.testing.expectEqualStrings(js_src, passthrough);
}
