const std = @import("std");
const zigts = @import("zigts");
const plan = @import("plan.zig");

pub const DeployConfig = struct {
    provider: plan.Provider,
    handler_path: []const u8,
    name: []const u8,
    region: ?[]const u8 = null,
    env_file: ?[]const u8 = null,
    arch: plan.Arch = .amd64,
    dry_run: bool = false,
    json: bool = false,
    confirm: bool = false,

    pub fn deinit(self: *DeployConfig, allocator: std.mem.Allocator) void {
        allocator.free(self.handler_path);
        allocator.free(self.name);
        if (self.region) |value| allocator.free(value);
        if (self.env_file) |value| allocator.free(value);
    }
};

pub fn parse(allocator: std.mem.Allocator, argv: []const []const u8) !DeployConfig {
    var provider: ?plan.Provider = null;
    var handler_path: ?[]const u8 = null;
    var name: ?[]const u8 = null;
    var region: ?[]const u8 = null;
    var env_file: ?[]const u8 = null;
    var arch: plan.Arch = .amd64;
    var dry_run = false;
    var json = false;
    var confirm = false;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--provider")) {
            i += 1;
            if (i >= argv.len) return error.MissingProvider;
            provider = plan.Provider.fromString(argv[i]) orelse return error.UnsupportedProvider;
        } else if (std.mem.eql(u8, arg, "--handler")) {
            i += 1;
            if (i >= argv.len) return error.MissingHandler;
            handler_path = argv[i];
        } else if (std.mem.eql(u8, arg, "--name")) {
            i += 1;
            if (i >= argv.len) return error.MissingName;
            name = argv[i];
        } else if (std.mem.eql(u8, arg, "--region")) {
            i += 1;
            if (i >= argv.len) return error.MissingRegion;
            region = argv[i];
        } else if (std.mem.eql(u8, arg, "--env-file")) {
            i += 1;
            if (i >= argv.len) return error.MissingEnvFile;
            env_file = argv[i];
        } else if (std.mem.eql(u8, arg, "--arch")) {
            i += 1;
            if (i >= argv.len) return error.MissingArch;
            arch = plan.Arch.fromString(argv[i]) orelse return error.UnsupportedArch;
        } else if (std.mem.eql(u8, arg, "--dry-run")) {
            dry_run = true;
        } else if (std.mem.eql(u8, arg, "--json")) {
            json = true;
        } else if (std.mem.eql(u8, arg, "--confirm")) {
            confirm = true;
        } else if (std.mem.eql(u8, arg, "--help")) {
            return error.HelpRequested;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            if (handler_path == null) {
                handler_path = arg;
            } else {
                return error.InvalidArgument;
            }
        } else {
            return error.UnknownOption;
        }
    }

    const parsed_provider = provider orelse return error.MissingProvider;
    const parsed_handler = handler_path orelse return error.MissingHandler;
    const parsed_name = name orelse return error.MissingName;

    return .{
        .provider = parsed_provider,
        .handler_path = try allocator.dupe(u8, parsed_handler),
        .name = try allocator.dupe(u8, parsed_name),
        .region = if (region) |value| try allocator.dupe(u8, value) else null,
        .env_file = if (env_file) |value| try allocator.dupe(u8, value) else null,
        .arch = arch,
        .dry_run = dry_run,
        .json = json,
        .confirm = confirm,
    };
}

pub fn loadEnvFile(allocator: std.mem.Allocator, path: ?[]const u8) ![]const plan.EnvVar {
    const env_path = path orelse return allocator.alloc(plan.EnvVar, 0);
    const bytes = try zigts.file_io.readFile(allocator, env_path, 1024 * 1024);
    defer allocator.free(bytes);

    var list = std.ArrayList(plan.EnvVar).empty;
    errdefer {
        for (list.items) |item| {
            allocator.free(item.key);
            allocator.free(item.value);
        }
        list.deinit(allocator);
    }

    var lines = std.mem.splitScalar(u8, bytes, '\n');
    while (lines.next()) |line_raw| {
        const line = std.mem.trim(u8, line_raw, " \t\r");
        if (line.len == 0 or line[0] == '#') continue;
        const eq_idx = std.mem.indexOfScalar(u8, line, '=') orelse return error.InvalidEnvFile;
        const key = std.mem.trim(u8, line[0..eq_idx], " \t");
        const value = std.mem.trim(u8, line[eq_idx + 1 ..], " \t");
        if (key.len == 0) return error.InvalidEnvFile;
        try list.append(allocator, .{
            .key = try allocator.dupe(u8, key),
            .value = try allocator.dupe(u8, value),
        });
    }

    return try list.toOwnedSlice(allocator);
}

test "parse deploy config supports positional handler" {
    var cfg = try parse(std.testing.allocator, &.{
        "--provider",
        "render",
        "--name",
        "demo",
        "src/handler.ts",
    });
    defer cfg.deinit(std.testing.allocator);

    try std.testing.expectEqual(plan.Provider.render, cfg.provider);
    try std.testing.expectEqualStrings("src/handler.ts", cfg.handler_path);
}

test "parse deploy config rejects unsupported provider" {
    try std.testing.expectError(error.UnsupportedProvider, parse(std.testing.allocator, &.{
        "--provider",
        "bogus",
        "--name",
        "demo",
        "src/handler.ts",
    }));
}

test "parse deploy config requires name" {
    try std.testing.expectError(error.MissingName, parse(std.testing.allocator, &.{
        "--provider",
        "render",
        "src/handler.ts",
    }));
}

test "parse deploy config parses arch flag" {
    var cfg = try parse(std.testing.allocator, &.{
        "--provider",
        "render",
        "--name",
        "demo",
        "--arch",
        "amd64",
        "src/handler.ts",
    });
    defer cfg.deinit(std.testing.allocator);

    try std.testing.expectEqual(plan.Arch.amd64, cfg.arch);
}

test "parse deploy config rejects unknown option" {
    try std.testing.expectError(error.UnknownOption, parse(std.testing.allocator, &.{
        "--provider",
        "render",
        "--name",
        "demo",
        "--registry",
        "ghcr.io/acme/demo",
        "src/handler.ts",
    }));
}

test "load env file parses key value pairs" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = "deploy.env",
        .data =
        \\API_KEY=secret
        \\PORT=3000
        ,
    });

    const cwd = try std.process.currentPathAlloc(std.testing.io, std.testing.allocator);
    defer std.testing.allocator.free(cwd);
    const path = try std.fs.path.resolve(std.testing.allocator, &.{ tmp.sub_path, "deploy.env" });
    defer std.testing.allocator.free(path);

    const env_vars = try loadEnvFile(std.testing.allocator, path);
    defer {
        for (env_vars) |item| {
            std.testing.allocator.free(item.key);
            std.testing.allocator.free(item.value);
        }
        std.testing.allocator.free(env_vars);
    }

    try std.testing.expectEqual(@as(usize, 2), env_vars.len);
    try std.testing.expectEqualStrings("API_KEY", env_vars[0].key);
    try std.testing.expectEqualStrings("3000", env_vars[1].value);
}
