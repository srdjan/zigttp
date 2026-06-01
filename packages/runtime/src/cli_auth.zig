//! `zigttp auth` — store provider API keys so `zigttp expert` (and any
//! handler that declares the env var in its contract) can find them without
//! the user exporting shell variables or sourcing a `.env`.
//!
//! Storage: `~/.zigttp/providers.json`, 0600. Sibling to `~/.zigttp/credentials`
//! used by the hosted-cloud deploy path; keeping the two files orthogonal
//! avoids accidentally coupling provider auth to deferred-cloud-auth state.
//!
//! The `injectStoredProvidersIntoEnv` helper is called by `dev_cli.zig`
//! before `dev`/`serve`/`expert` dispatch so the runtime sees the values
//! transparently. Shell-set variables always win; the file only fills gaps.

const std = @import("std");
const builtin = @import("builtin");

extern "c" fn setenv(name: [*:0]const u8, value: [*:0]const u8, overwrite: c_int) c_int;
extern "c" fn unsetenv(name: [*:0]const u8) c_int;

const rel_dir: []const u8 = ".zigttp";
const rel_file: []const u8 = ".zigttp/providers.json";

/// Providers we know how to store and inject. Add new entries here when a
/// new provider lands in `packages/pi/src/providers/`.
const Provider = struct {
    name: []const u8,
    env_var: [:0]const u8,
    label: []const u8,
};

const providers = [_]Provider{
    .{ .name = "anthropic", .env_var = "ANTHROPIC_API_KEY", .label = "claude" },
    .{ .name = "openai", .env_var = "OPENAI_API_KEY", .label = "openai" },
};

fn providerByLabel(label: []const u8) ?Provider {
    for (providers) |p| {
        if (std.mem.eql(u8, p.label, label)) return p;
    }
    return null;
}

pub fn authCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    if (argv.len == 0 or
        std.mem.eql(u8, argv[0], "--help") or
        std.mem.eql(u8, argv[0], "-h") or
        std.mem.eql(u8, argv[0], "help"))
    {
        printAuthHelp();
        return;
    }

    const sub = argv[0];
    if (std.mem.eql(u8, sub, "claude")) {
        try authProviderSet(allocator, providerByLabel("claude").?, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, sub, "openai")) {
        try authProviderSet(allocator, providerByLabel("openai").?, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, sub, "status")) {
        try authStatus(allocator);
        return;
    }
    if (std.mem.eql(u8, sub, "revoke")) {
        if (argv.len < 2) {
            std.debug.print("zigttp auth revoke requires a provider label, e.g. `zigttp auth revoke claude`.\n", .{});
            return error.InvalidArgument;
        }
        const provider = providerByLabel(argv[1]) orelse {
            std.debug.print("Unknown provider: {s}. Known: claude, openai.\n", .{argv[1]});
            return error.InvalidArgument;
        };
        try authRevoke(allocator, provider);
        return;
    }

    std.debug.print("Unknown auth subcommand: {s}\n\n", .{sub});
    printAuthHelp();
    return error.InvalidArgument;
}

pub fn printAuthHelp() void {
    const help =
        \\zigttp auth - store provider API keys for expert and handlers
        \\
        \\Usage:
        \\  zigttp auth claude              Prompt for an Anthropic API key and store it
        \\  zigttp auth openai              Prompt for an OpenAI API key and store it
        \\  zigttp auth status              Show which providers are configured
        \\  zigttp auth revoke <provider>   Remove a stored key (claude | openai)
        \\
        \\Storage: ~/.zigttp/providers.json (mode 0600).
        \\
        \\The runtime injects stored keys into ANTHROPIC_API_KEY / OPENAI_API_KEY
        \\at the start of `zigttp dev`, `zigttp serve`, and `zigttp expert`.
        \\Shell-set variables always win; the stored value only fills gaps.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn authProviderSet(
    allocator: std.mem.Allocator,
    provider: Provider,
    argv: []const []const u8,
) !void {
    _ = argv;
    if (!stdinIsTty()) {
        std.debug.print(
            "zigttp auth {s} needs an interactive terminal to prompt for the key.\n" ++
                "Run it from a real shell, or set {s} directly in your environment.\n",
            .{ provider.label, provider.env_var },
        );
        return error.NotATty;
    }

    std.debug.print(
        "Paste your {s} API key (input hidden), then press Enter.\nKey: ",
        .{provider.label},
    );

    const key = readSecretLine(allocator) catch |err| {
        std.debug.print("\nFailed to read key: {}\n", .{err});
        return err;
    };
    defer allocator.free(key);
    std.debug.print("\n", .{});

    if (key.len == 0) {
        std.debug.print("No key entered. Nothing saved.\n", .{});
        return error.EmptyKey;
    }
    if (!looksLikeApiKey(provider, key)) {
        std.debug.print(
            "That does not look like a {s} key (expected at least 20 characters{s}).\n" ++
                "Nothing saved. Re-run `zigttp auth {s}` and try again.\n",
            .{
                provider.label,
                if (std.mem.eql(u8, provider.name, "anthropic"))
                    @as([]const u8, ", usually starting with sk-ant-")
                else
                    @as([]const u8, ""),
                provider.label,
            },
        );
        return error.InvalidKey;
    }

    var store = try Store.load(allocator);
    defer store.deinit();

    try store.set(provider.name, key);
    try store.save();

    std.debug.print(
        "Saved {s} key for {s}.\n" ++
            "Verifying: run `zigttp expert` to confirm the backend authenticates.\n",
        .{ provider.label, provider.name },
    );
}

fn authStatus(allocator: std.mem.Allocator) !void {
    var store = try Store.load(allocator);
    defer store.deinit();

    std.debug.print("Provider keys:\n", .{});
    for (providers) |p| {
        const file_value = store.get(p.name);
        const env_value = readEnvVar(p.env_var);

        const source: []const u8 = if (env_value != null)
            "shell"
        else if (file_value != null)
            "file"
        else
            "none";

        const visible_value = env_value orelse file_value;

        if (visible_value) |v| {
            const masked = maskKey(v);
            std.debug.print(
                "  {s:<8} {s:<6} {s}\n",
                .{ p.label, source, masked },
            );
        } else {
            std.debug.print(
                "  {s:<8} {s:<6} (run `zigttp auth {s}`)\n",
                .{ p.label, source, p.label },
            );
        }
    }
    std.debug.print("\nFile: ~/.zigttp/providers.json\n", .{});
}

fn authRevoke(allocator: std.mem.Allocator, provider: Provider) !void {
    var store = try Store.load(allocator);
    defer store.deinit();

    if (!store.has(provider.name)) {
        std.debug.print("No stored {s} key. Nothing to revoke.\n", .{provider.label});
        return;
    }
    try store.remove(provider.name);
    try store.save();
    std.debug.print("Removed stored {s} key.\n", .{provider.label});
}

/// Read `~/.zigttp/providers.json` and set the env var for each known
/// provider, but only when the shell has not already exported a non-blank
/// value for it. Silently no-ops when the file is missing or unreadable;
/// the existing expert fail-fast path will produce a clear setup message
/// in that case. dev_cli.zig calls this before dispatching `dev`, `serve`,
/// or `expert`.
pub fn injectStoredProvidersIntoEnv(allocator: std.mem.Allocator) void {
    var store = Store.load(allocator) catch return;
    defer store.deinit();

    for (providers) |p| {
        const file_value = store.get(p.name) orelse continue;
        if (readEnvVar(p.env_var) != null) continue;

        const value_z = allocator.dupeZ(u8, file_value) catch continue;
        defer allocator.free(value_z);
        _ = setenv(p.env_var, value_z.ptr, 1);
    }
}

// ---------------------------------------------------------------------------
// Storage: ~/.zigttp/providers.json
// ---------------------------------------------------------------------------
//
// In-memory shape is a flat list of (provider_name, api_key) pairs. The
// JSON file format is `{ "<provider>": { "api_key": "..." } }`, kept
// hand-rolled instead of leaning on std.json.ObjectMap so the unmanaged
// ArrayHashMap API doesn't have to be threaded through every mutation.

const Entry = struct {
    name: []u8,
    api_key: []u8,
};

const Store = struct {
    allocator: std.mem.Allocator,
    entries: std.ArrayList(Entry),

    fn load(allocator: std.mem.Allocator) !Store {
        var store: Store = .{ .allocator = allocator, .entries = .empty };
        errdefer store.deinit();

        const bytes = readProvidersFile(allocator) catch |err| switch (err) {
            error.FileNotFound, error.HomeDirMissing => return store,
            else => return err,
        };
        defer allocator.free(bytes);

        var parsed = std.json.parseFromSlice(std.json.Value, allocator, bytes, .{}) catch {
            // A corrupt file should not poison `zigttp dev`. Treat it as empty
            // so injection silently no-ops; the user can re-run `auth claude`
            // to rewrite it.
            return store;
        };
        defer parsed.deinit();
        if (parsed.value != .object) return store;

        var it = parsed.value.object.iterator();
        while (it.next()) |kv| {
            if (kv.value_ptr.* != .object) continue;
            const inner = kv.value_ptr.object;
            const key_value = inner.get("api_key") orelse continue;
            if (key_value != .string) continue;
            const trimmed = std.mem.trim(u8, key_value.string, " \t\r\n");
            if (trimmed.len == 0) continue;

            try store.setOwned(kv.key_ptr.*, trimmed);
        }
        return store;
    }

    fn deinit(self: *Store) void {
        for (self.entries.items) |entry| {
            self.allocator.free(entry.name);
            self.allocator.free(entry.api_key);
        }
        self.entries.deinit(self.allocator);
    }

    fn has(self: *const Store, provider_name: []const u8) bool {
        return self.findIndex(provider_name) != null;
    }

    fn get(self: *const Store, provider_name: []const u8) ?[]const u8 {
        const idx = self.findIndex(provider_name) orelse return null;
        return self.entries.items[idx].api_key;
    }

    fn findIndex(self: *const Store, provider_name: []const u8) ?usize {
        for (self.entries.items, 0..) |entry, i| {
            if (std.mem.eql(u8, entry.name, provider_name)) return i;
        }
        return null;
    }

    fn set(self: *Store, provider_name: []const u8, api_key: []const u8) !void {
        try self.setOwned(provider_name, api_key);
    }

    fn setOwned(self: *Store, provider_name: []const u8, api_key: []const u8) !void {
        if (self.findIndex(provider_name)) |idx| {
            const new_key = try self.allocator.dupe(u8, api_key);
            self.allocator.free(self.entries.items[idx].api_key);
            self.entries.items[idx].api_key = new_key;
            return;
        }
        const name_copy = try self.allocator.dupe(u8, provider_name);
        errdefer self.allocator.free(name_copy);
        const key_copy = try self.allocator.dupe(u8, api_key);
        errdefer self.allocator.free(key_copy);
        try self.entries.append(self.allocator, .{ .name = name_copy, .api_key = key_copy });
    }

    fn remove(self: *Store, provider_name: []const u8) !void {
        const idx = self.findIndex(provider_name) orelse return;
        const removed = self.entries.orderedRemove(idx);
        self.allocator.free(removed.name);
        self.allocator.free(removed.api_key);
    }

    fn save(self: *Store) !void {
        var aw: std.Io.Writer.Allocating = .init(self.allocator);
        defer aw.deinit();
        var json: std.json.Stringify = .{ .writer = &aw.writer, .options = .{ .whitespace = .indent_2 } };
        try json.beginObject();
        for (self.entries.items) |entry| {
            try json.objectField(entry.name);
            try json.beginObject();
            try json.objectField("api_key");
            try json.write(entry.api_key);
            try json.endObject();
        }
        try json.endObject();
        try aw.writer.writeAll("\n");

        try writeProvidersFile(self.allocator, aw.writer.buffered());
    }
};

fn homeDirAlloc(allocator: std.mem.Allocator) ![]u8 {
    const raw = std.c.getenv("HOME") orelse return error.HomeDirMissing;
    return allocator.dupe(u8, std.mem.sliceTo(raw, 0));
}

fn providersFilePath(allocator: std.mem.Allocator) ![]u8 {
    const home = try homeDirAlloc(allocator);
    defer allocator.free(home);
    return std.fs.path.join(allocator, &.{ home, rel_file });
}

fn providersDirPath(allocator: std.mem.Allocator) ![]u8 {
    const home = try homeDirAlloc(allocator);
    defer allocator.free(home);
    return std.fs.path.join(allocator, &.{ home, rel_dir });
}

fn readProvidersFile(allocator: std.mem.Allocator) ![]u8 {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const path = try providersFilePath(allocator);
    defer allocator.free(path);

    var file = try std.Io.Dir.openFileAbsolute(io, path, .{});
    defer file.close(io);

    var buf: [4096]u8 = undefined;
    var reader = file.reader(io, &buf);
    return try reader.interface.allocRemaining(allocator, .unlimited);
}

fn writeProvidersFile(allocator: std.mem.Allocator, bytes: []const u8) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const dir_path = try providersDirPath(allocator);
    defer allocator.free(dir_path);
    std.Io.Dir.createDirAbsolute(io, dir_path, std.Io.File.Permissions.fromMode(0o700)) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    const file_path = try providersFilePath(allocator);
    defer allocator.free(file_path);

    var file = try std.Io.Dir.createFileAbsolute(io, file_path, .{
        .truncate = true,
        .permissions = std.Io.File.Permissions.fromMode(0o600),
    });
    defer file.close(io);

    var buf: [512]u8 = undefined;
    var writer = file.writer(io, &buf);
    try writer.interface.writeAll(bytes);
    try writer.interface.flush();
}

// ---------------------------------------------------------------------------
// Helpers: env, TTY input, masking
// ---------------------------------------------------------------------------

fn readEnvVar(name_z: [:0]const u8) ?[]const u8 {
    const raw = std.c.getenv(name_z) orelse return null;
    const value = std.mem.sliceTo(raw, 0);
    const trimmed = std.mem.trim(u8, value, " \t\r\n");
    if (trimmed.len == 0) return null;
    return trimmed;
}

fn stdinIsTty() bool {
    return switch (builtin.os.tag) {
        .windows => false,
        else => std.posix.system.isatty(std.posix.STDIN_FILENO) != 0,
    };
}

fn readSecretLine(allocator: std.mem.Allocator) ![]u8 {
    if (builtin.os.tag == .windows) return error.PromptUnsupported;

    const original = try std.posix.tcgetattr(std.posix.STDIN_FILENO);
    var hidden = original;
    hidden.lflag.ECHO = false;
    try std.posix.tcsetattr(std.posix.STDIN_FILENO, .FLUSH, hidden);
    defer std.posix.tcsetattr(std.posix.STDIN_FILENO, .FLUSH, original) catch {};

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
    const trimmed = std.mem.trim(u8, raw, " \t\r\n");
    if (trimmed.len == raw.len) return raw;
    const start: usize = @intFromPtr(trimmed.ptr) - @intFromPtr(raw.ptr);
    if (start > 0) std.mem.copyForwards(u8, raw[0..trimmed.len], raw[start..][0..trimmed.len]);
    return allocator.realloc(raw, trimmed.len);
}

fn looksLikeApiKey(provider: Provider, key: []const u8) bool {
    if (key.len < 20) return false;
    if (std.mem.eql(u8, provider.name, "anthropic")) {
        // Real Anthropic console keys all start with `sk-ant-`. Refusing
        // anything else catches the common paste-the-wrong-string mistake
        // without locking out a future format change: if Anthropic ever
        // ships a non-`sk-ant-` key, the user can set ANTHROPIC_API_KEY
        // directly in their shell and skip this CLI.
        return std.mem.startsWith(u8, key, "sk-ant-");
    }
    return true;
}

fn maskKey(key: []const u8) [32]u8 {
    var out: [32]u8 = undefined;
    @memset(&out, ' ');
    const visible_tail = 4;
    const prefix_len: usize = if (std.mem.startsWith(u8, key, "sk-ant-"))
        7
    else if (std.mem.startsWith(u8, key, "sk-"))
        3
    else
        0;

    var i: usize = 0;
    if (prefix_len > 0 and i + prefix_len < out.len) {
        @memcpy(out[i..][0..prefix_len], key[0..prefix_len]);
        i += prefix_len;
    }
    const tail_start: usize = if (key.len > visible_tail) key.len - visible_tail else 0;
    // Three dots between prefix and tail, then the last 4 chars.
    if (i + 3 < out.len) {
        out[i] = '.';
        out[i + 1] = '.';
        out[i + 2] = '.';
        i += 3;
    }
    const tail_len = key.len - tail_start;
    if (i + tail_len <= out.len) {
        @memcpy(out[i..][0..tail_len], key[tail_start..]);
        i += tail_len;
    }
    // Pad the remainder so the printer sees a fixed-width slice.
    while (i < out.len) : (i += 1) out[i] = ' ';
    return out;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "looksLikeApiKey: anthropic requires sk-ant- prefix and length" {
    const anth = providerByLabel("claude").?;
    try std.testing.expect(!looksLikeApiKey(anth, ""));
    try std.testing.expect(!looksLikeApiKey(anth, "sk-ant-short"));
    try std.testing.expect(!looksLikeApiKey(anth, "this-is-a-long-enough-string-but-no-prefix"));
    try std.testing.expect(looksLikeApiKey(anth, "sk-ant-aaaaaaaaaaaaaaaa"));
}

test "looksLikeApiKey: openai only checks length" {
    const oai = providerByLabel("openai").?;
    try std.testing.expect(!looksLikeApiKey(oai, "short"));
    try std.testing.expect(looksLikeApiKey(oai, "anything-longer-than-twenty-chars"));
}

test "maskKey: redacts the middle, keeps prefix + tail" {
    const masked = maskKey("sk-ant-1234567890abcd");
    const trimmed = std.mem.trim(u8, masked[0..], " ");
    try std.testing.expectEqualStrings("sk-ant-...abcd", trimmed);
}

test "Store: set/get/has/remove round-trip in memory" {
    const allocator = std.testing.allocator;
    var store: Store = .{ .allocator = allocator, .entries = .empty };
    defer store.deinit();

    try std.testing.expect(!store.has("anthropic"));
    try store.set("anthropic", "sk-ant-fixture");
    try std.testing.expect(store.has("anthropic"));
    try std.testing.expectEqualStrings("sk-ant-fixture", store.get("anthropic").?);

    try store.remove("anthropic");
    try std.testing.expect(!store.has("anthropic"));
}

test "Store: save then load roundtrip via tmp HOME" {
    const allocator = std.testing.allocator;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const home_z = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(home_z);

    const previous_home = std.c.getenv("HOME");
    _ = setenv("HOME", home_z.ptr, 1);
    defer if (previous_home) |p| {
        _ = setenv("HOME", p, 1);
    } else {
        _ = unsetenv("HOME");
    };

    {
        var store = try Store.load(allocator);
        defer store.deinit();
        try store.set("anthropic", "sk-ant-roundtrip-test-value");
        try store.save();
    }

    var loaded = try Store.load(allocator);
    defer loaded.deinit();
    try std.testing.expectEqualStrings("sk-ant-roundtrip-test-value", loaded.get("anthropic").?);
}
