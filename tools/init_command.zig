const std = @import("std");
const zigts = @import("zigts");
const writeFile = zigts.file_io.writeFile;

const skill_md = @embedFile("skills/zigts-expert/SKILL.md");
const virtual_modules_md = @embedFile("skills/zigts-expert/references/virtual-modules.md");
const testing_replay_md = @embedFile("skills/zigts-expert/references/testing-replay.md");
const jsx_patterns_md = @embedFile("skills/zigts-expert/references/jsx-patterns.md");
const pre_edit_hook = @embedFile("hooks/pre-edit-zts.sh");
const post_edit_hook = @embedFile("hooks/post-edit-zts.sh");
const session_start_hook = @embedFile("hooks/session-start.sh");

const SkillFile = struct {
    path: []const u8,
    data: []const u8,
    executable: bool = false,
};

const skill_files = [_]SkillFile{
    .{ .path = ".claude/skills/zigts-expert/SKILL.md", .data = skill_md },
    .{ .path = ".claude/skills/zigts-expert/references/virtual-modules.md", .data = virtual_modules_md },
    .{ .path = ".claude/skills/zigts-expert/references/testing-replay.md", .data = testing_replay_md },
    .{ .path = ".claude/skills/zigts-expert/references/jsx-patterns.md", .data = jsx_patterns_md },
    .{ .path = ".claude/hooks/pre-edit-zts.sh", .data = pre_edit_hook, .executable = true },
    .{ .path = ".claude/hooks/post-edit-zts.sh", .data = post_edit_hook, .executable = true },
    .{ .path = ".claude/hooks/session-start.sh", .data = session_start_hook, .executable = true },
};

pub fn runInit(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var force = false;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--force")) {
            force = true;
        } else if (std.mem.eql(u8, arg, "--help")) {
            printInitHelp();
            return;
        } else {
            std.debug.print("Unknown argument: {s}\n", .{arg});
            return error.InvalidArgument;
        }
    }

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, ".claude/skills/zigts-expert/references");
    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, ".claude/hooks");

    var written: usize = 0;
    var skipped: usize = 0;

    for (skill_files) |file| {
        if (!force and fileExists(allocator, file.path)) {
            std.debug.print("  skip {s} (exists, use --force to overwrite)\n", .{file.path});
            skipped += 1;
            continue;
        }
        writeFile(allocator, file.path, file.data) catch |err| {
            std.debug.print("  error writing {s}: {}\n", .{ file.path, err });
            return err;
        };
        if (file.executable) {
            makeExecutable(allocator, file.path);
        }
        std.debug.print("  wrote {s}\n", .{file.path});
        written += 1;
    }

    std.debug.print("\nInstalled zigts-expert ({d} files written, {d} skipped)\n", .{ written, skipped });
    if (written > 0) {
        std.debug.print("\nClaude Code will now use the compiler-in-the-loop workflow.\n", .{});
        std.debug.print("Hook scripts installed in .claude/hooks/\n", .{});
        std.debug.print("Run `zigts check --json handler.ts` to verify handlers.\n", .{});
    }
}

const fileExists = zigts.file_io.fileExists;

fn makeExecutable(allocator: std.mem.Allocator, path: []const u8) void {
    const path_z = allocator.dupeZ(u8, path) catch return;
    defer allocator.free(path_z);
    _ = std.c.chmod(path_z, 0o755);
}

fn printInitHelp() void {
    const help =
        \\zigts init - install Claude Code skill files
        \\
        \\Usage: zigts init [--force]
        \\
        \\Writes zigts-expert skill files into .claude/skills/zigts-expert/
        \\in the current directory. Claude Code picks these up automatically.
        \\
        \\Options:
        \\  --force   Overwrite existing files
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}
