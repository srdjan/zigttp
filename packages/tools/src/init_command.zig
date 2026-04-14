const std = @import("std");
const zigts = @import("zigts");
const writeFile = zigts.file_io.writeFile;
const fileExists = zigts.file_io.fileExists;

const zigts_skill_md = @embedFile("skills/zigts-expert/SKILL.md");
const zigts_virtual_modules_md = @embedFile("skills/zigts-expert/references/virtual-modules.md");
const zigts_testing_replay_md = @embedFile("skills/zigts-expert/references/testing-replay.md");
const zigts_jsx_patterns_md = @embedFile("skills/zigts-expert/references/jsx-patterns.md");

const vm_skill_md = @embedFile("skills/zigttp-virtual-module-author/SKILL.md");
const vm_capability_discipline_md = @embedFile("skills/zigttp-virtual-module-author/references/capability-discipline.md");
const vm_module_specs_md = @embedFile("skills/zigttp-virtual-module-author/references/module-specs.md");
const vm_law_review_md = @embedFile("skills/zigttp-virtual-module-author/references/law-review.md");

const pre_edit_hook = @embedFile("hooks/pre-edit-zts.sh");
const post_edit_hook = @embedFile("hooks/post-edit-zts.sh");
const pre_edit_zig_module_hook = @embedFile("hooks/pre-edit-zig-module.sh");
const post_edit_zig_module_hook = @embedFile("hooks/post-edit-zig-module.sh");
const session_start_hook = @embedFile("hooks/session-start.sh");
const agent_settings_json = @embedFile("settings/claude-settings.json");

const legacy_settings_json =
    \\{
    \\  "permissions": {
    \\    "allow": [
    \\      "Bash(zigts expert *)",
    \\      "Bash(zigts check *)",
    \\      "Bash(zigts edit-simulate *)",
    \\      "Bash(zigts describe-rule *)",
    \\      "Bash(zigts search *)",
    \\      "Bash(zigts init *)"
    \\    ],
    \\    "deny": [
    \\      "Bash(python3 *)",
    \\      "Bash(python *)",
    \\      "Edit(./.claude/hooks/**)",
    \\      "Write(./.claude/hooks/**)",
    \\      "Edit(./.claude/settings.json)",
    \\      "Write(./.claude/settings.json)",
    \\      "Edit(./packages/zigts/src/rule_registry.zig)",
    \\      "Write(./packages/zigts/src/rule_registry.zig)"
    \\    ]
    \\  },
    \\  "hooks": {
    \\    "PreToolUse": [
    \\      {
    \\        "matcher": "Edit|Write|MultiEdit",
    \\        "hooks": [
    \\          {
    \\            "type": "command",
    \\            "command": "bash .claude/hooks/pre-edit-zts.sh",
    \\            "timeout": 15
    \\          }
    \\        ]
    \\      }
    \\    ],
    \\    "PostToolUse": [
    \\      {
    \\        "matcher": "Edit|Write|MultiEdit",
    \\        "hooks": [
    \\          {
    \\            "type": "command",
    \\            "command": "bash .claude/hooks/post-edit-zts.sh",
    \\            "timeout": 30
    \\          }
    \\        ]
    \\      }
    \\    ]
    \\  }
    \\}
;

const InstallFile = struct {
    path: []const u8,
    data: []const u8,
    executable: bool = false,
};

const InstallProfile = enum {
    legacy,
    agent,
};

const legacy_files = [_]InstallFile{
    .{ .path = ".claude/skills/zigts-expert/SKILL.md", .data = zigts_skill_md },
    .{ .path = ".claude/skills/zigts-expert/references/virtual-modules.md", .data = zigts_virtual_modules_md },
    .{ .path = ".claude/skills/zigts-expert/references/testing-replay.md", .data = zigts_testing_replay_md },
    .{ .path = ".claude/skills/zigts-expert/references/jsx-patterns.md", .data = zigts_jsx_patterns_md },
    .{ .path = ".claude/hooks/pre-edit-zts.sh", .data = pre_edit_hook, .executable = true },
    .{ .path = ".claude/hooks/post-edit-zts.sh", .data = post_edit_hook, .executable = true },
    .{ .path = ".claude/hooks/session-start.sh", .data = session_start_hook, .executable = true },
    .{ .path = ".claude/settings.json", .data = legacy_settings_json },
};

const agent_files = [_]InstallFile{
    .{ .path = ".claude/skills/zigts-expert/SKILL.md", .data = zigts_skill_md },
    .{ .path = ".claude/skills/zigts-expert/references/virtual-modules.md", .data = zigts_virtual_modules_md },
    .{ .path = ".claude/skills/zigts-expert/references/testing-replay.md", .data = zigts_testing_replay_md },
    .{ .path = ".claude/skills/zigts-expert/references/jsx-patterns.md", .data = zigts_jsx_patterns_md },
    .{ .path = ".claude/skills/zigttp-virtual-module-author/SKILL.md", .data = vm_skill_md },
    .{ .path = ".claude/skills/zigttp-virtual-module-author/references/capability-discipline.md", .data = vm_capability_discipline_md },
    .{ .path = ".claude/skills/zigttp-virtual-module-author/references/module-specs.md", .data = vm_module_specs_md },
    .{ .path = ".claude/skills/zigttp-virtual-module-author/references/law-review.md", .data = vm_law_review_md },
    .{ .path = ".claude/hooks/pre-edit-zts.sh", .data = pre_edit_hook, .executable = true },
    .{ .path = ".claude/hooks/post-edit-zts.sh", .data = post_edit_hook, .executable = true },
    .{ .path = ".claude/hooks/pre-edit-zig-module.sh", .data = pre_edit_zig_module_hook, .executable = true },
    .{ .path = ".claude/hooks/post-edit-zig-module.sh", .data = post_edit_zig_module_hook, .executable = true },
    .{ .path = ".claude/hooks/session-start.sh", .data = session_start_hook, .executable = true },
    .{ .path = ".claude/settings.json", .data = agent_settings_json },
};

pub fn runInit(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    try runProfileInstall(allocator, .legacy, argv);
}

pub fn runAgentCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    if (argv.len == 0 or std.mem.eql(u8, argv[0], "--help") or std.mem.eql(u8, argv[0], "help")) {
        printAgentHelp();
        return;
    }

    if (std.mem.eql(u8, argv[0], "init")) {
        try runProfileInstall(allocator, .agent, argv[1..]);
        return;
    }

    std.debug.print("Unknown zigts agent subcommand: {s}\n", .{argv[0]});
    printAgentHelp();
    return error.InvalidArgument;
}

fn runProfileInstall(
    allocator: std.mem.Allocator,
    profile: InstallProfile,
    argv: []const []const u8,
) !void {
    var force = false;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--force")) {
            force = true;
        } else if (std.mem.eql(u8, arg, "--help")) {
            switch (profile) {
                .legacy => printInitHelp(),
                .agent => printAgentInitHelp(),
            }
            return;
        } else {
            std.debug.print("Unknown argument: {s}\n", .{arg});
            return error.InvalidArgument;
        }
    }

    const files = profileFiles(profile);

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    for (files) |file| {
        try ensureParentDir(allocator, io, file.path);
    }

    var written: usize = 0;
    var skipped: usize = 0;

    for (files) |file| {
        if (!force and fileExists(allocator, file.path)) {
            std.debug.print("  skip {s} (exists, use --force to overwrite)\n", .{file.path});
            skipped += 1;
            continue;
        }
        writeFile(allocator, file.path, file.data) catch |err| {
            std.debug.print("  error writing {s}: {}\n", .{ file.path, err });
            return err;
        };
        if (file.executable) makeExecutable(allocator, file.path);
        std.debug.print("  wrote {s}\n", .{file.path});
        written += 1;
    }

    switch (profile) {
        .legacy => {
            std.debug.print("\nInstalled zigts-expert ({d} files written, {d} skipped)\n", .{ written, skipped });
            if (written > 0) {
                std.debug.print("\nClaude Code will now use the compiler-in-the-loop handler workflow.\n", .{});
                std.debug.print("Run `zigts check --json handler.ts` to verify handlers.\n", .{});
            }
        },
        .agent => {
            std.debug.print("\nInstalled zigttp agent bundle ({d} files written, {d} skipped)\n", .{ written, skipped });
            if (written > 0) {
                std.debug.print("\nInstalled skills:\n", .{});
                std.debug.print("  - zigts-expert\n", .{});
                std.debug.print("  - zigttp-virtual-module-author\n", .{});
                std.debug.print("Run `zigttp agent init --force` after upgrading to refresh hooks and settings.\n", .{});
            }
        },
    }
}

fn profileFiles(profile: InstallProfile) []const InstallFile {
    return switch (profile) {
        .legacy => &legacy_files,
        .agent => &agent_files,
    };
}

fn ensureParentDir(allocator: std.mem.Allocator, io: std.Io, path: []const u8) !void {
    const parent = std.fs.path.dirname(path) orelse return;
    if (parent.len == 0) return;
    const duped = try allocator.dupe(u8, parent);
    defer allocator.free(duped);
    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, duped);
}

fn makeExecutable(allocator: std.mem.Allocator, path: []const u8) void {
    const path_z = allocator.dupeZ(u8, path) catch return;
    defer allocator.free(path_z);
    _ = std.c.chmod(path_z, 0o755);
}

fn printInitHelp() void {
    const help =
        \\zigts init - install handler authoring skill files
        \\
        \\Usage: zigts init [--force]
        \\
        \\Writes zigts-expert, handler hooks, and .claude/settings.json into
        \\the current directory. Claude Code picks these up automatically.
        \\
        \\Options:
        \\  --force   Overwrite existing files
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printAgentHelp() void {
    const help =
        \\zigts agent - install agent tooling bundles
        \\
        \\Usage:
        \\  zigts agent init [--force]
        \\
        \\Subcommands:
        \\  init   Install zigts-expert, zigttp-virtual-module-author, hooks, and settings
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printAgentInitHelp() void {
    const help =
        \\zigts agent init - install zigttp agent tooling
        \\
        \\Usage: zigts agent init [--force]
        \\
        \\Writes both local skill bundles, handler and virtual-module hooks,
        \\and .claude/settings.json into the current directory.
        \\
        \\Options:
        \\  --force   Overwrite existing files
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}
