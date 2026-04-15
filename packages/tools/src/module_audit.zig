const std = @import("std");
const zigts = @import("zigts");
const json_diag = @import("json_diagnostics.zig");
const expert_meta = @import("expert_meta.zig");
const writeJsonString = zigts.handler_contract.writeJsonString;

const file_io = zigts.file_io;
const builtin_modules = zigts.builtin_modules;

/// Write the v1 verify-modules envelope to `writer`. Single source of truth
/// for the shape documented in docs/zigts-expert-contract.md.
pub fn writeJsonEnvelope(
    writer: anytype,
    result: *const VerifyResult,
    policy_hash: [64]u8,
) !void {
    try writer.print(
        "{{\"ok\":{s},\"policy_version\":\"{s}\",\"policy_hash\":\"{s}\",\"checked_files\":[",
        .{
            if (result.hasErrors()) "false" else "true",
            expert_meta.policy_version,
            policy_hash,
        },
    );
    for (result.checked_files.items, 0..) |path, i| {
        if (i > 0) try writer.writeByte(',');
        try writeJsonString(writer, path);
    }
    try writer.writeAll("],\"violations\":[");
    for (result.diagnostics.items, 0..) |*diag, i| {
        if (i > 0) try writer.writeByte(',');
        try json_diag.writeDiagnosticJson(writer, &diag.diag);
    }
    try writer.writeAll("]}\n");
}

pub const OwnedDiagnostic = struct {
    diag: json_diag.JsonDiagnostic,
    owned_message: ?[]u8 = null,
    owned_suggestion: ?[]u8 = null,

    pub fn deinit(self: *OwnedDiagnostic, allocator: std.mem.Allocator) void {
        if (self.owned_message) |msg| allocator.free(msg);
        if (self.owned_suggestion) |msg| allocator.free(msg);
    }

    fn initOwned(
        allocator: std.mem.Allocator,
        code: []const u8,
        severity: []const u8,
        message: []const u8,
        file: []const u8,
        line: u32,
        column: u16,
        suggestion: ?[]const u8,
    ) !OwnedDiagnostic {
        const owned_message = try allocator.dupe(u8, message);
        const owned_suggestion = if (suggestion) |value| try allocator.dupe(u8, value) else null;
        return .{
            .diag = .{
                .code = code,
                .severity = severity,
                .message = owned_message,
                .file = file,
                .line = line,
                .column = column,
                .suggestion = owned_suggestion,
            },
            .owned_message = owned_message,
            .owned_suggestion = owned_suggestion,
        };
    }
};

pub const VerifyResult = struct {
    checked_files: std.ArrayList([]const u8) = .empty,
    diagnostics: std.ArrayList(OwnedDiagnostic) = .empty,

    pub fn deinit(self: *VerifyResult, allocator: std.mem.Allocator) void {
        self.checked_files.deinit(allocator);
        for (self.diagnostics.items) |*diag| diag.deinit(allocator);
        self.diagnostics.deinit(allocator);
    }

    pub fn hasErrors(self: *const VerifyResult) bool {
        for (self.diagnostics.items) |diag| {
            if (std.mem.eql(u8, diag.diag.severity, "error")) return true;
        }
        return false;
    }
};

pub const VerifyOptions = struct {
    strict: bool = false,
};

const CapabilityRule = struct {
    capability: []const u8,
    helpers: []const []const u8,
};

const ForbiddenPattern = struct {
    code: []const u8,
    pattern: []const u8,
    message: []const u8,
    suggestion: []const u8,
};

const capability_rules = [_]CapabilityRule{
    .{ .capability = "env", .helpers = &.{ "readEnvChecked", "readEnvForActiveModule" } },
    .{ .capability = "clock", .helpers = &.{ "clockNowMsChecked", "clockNowNsChecked", "clockNowSecsChecked", "nowMsForActiveModule", "nowNsForActiveModule" } },
    .{ .capability = "random", .helpers = &.{ "fillRandomChecked", "fillRandomForActiveModule" } },
    .{ .capability = "crypto", .helpers = &.{ "sha256Checked", "sha256ForActiveModule", "hmacSha256Checked", "hmacSha256ForActiveModule" } },
    .{ .capability = "stderr", .helpers = &.{ "writeStderrChecked", "writeStderrForActiveModule" } },
    .{ .capability = "runtime_callback", .helpers = &.{ "runtimeCallbackCapabilityChecked", "getRuntimeCallbackStateChecked" } },
    .{ .capability = "sqlite", .helpers = &.{ "sqliteCapabilityChecked", "getSqliteStateChecked", "openSqliteDbChecked" } },
    .{ .capability = "filesystem", .helpers = &.{"readFileChecked"} },
    .{ .capability = "policy_check", .helpers = &.{ "allowsEnvChecked", "allowsEnvForActiveModule", "allowsCacheNamespaceChecked", "allowsCacheNamespaceForActiveModule", "allowsSqlQueryChecked", "allowsSqlQueryForActiveModule" } },
};

const forbidden_patterns = [_]ForbiddenPattern{
    .{
        .code = "ZVM001",
        .pattern = "capability_policy.",
        .message = "virtual module accesses runtime capability_policy directly",
        .suggestion = "use checked policy helpers from packages/zigts/src/module_binding.zig",
    },
    .{
        .code = "ZVM001",
        .pattern = "std.c.getenv",
        .message = "virtual module reads environment variables directly",
        .suggestion = "use readEnvChecked instead of std.c.getenv",
    },
    .{
        .code = "ZVM001",
        .pattern = "file_io.readFile",
        .message = "virtual module reads files directly",
        .suggestion = "use readFileChecked instead of file_io.readFile",
    },
    .{
        .code = "ZVM001",
        .pattern = "Db.openReadWriteCreate",
        .message = "virtual module opens SQLite directly",
        .suggestion = "use openSqliteDbChecked instead of Db.openReadWriteCreate",
    },
    .{
        .code = "ZVM001",
        .pattern = "compat.realtimeNowMs",
        .message = "virtual module reads the clock directly",
        .suggestion = "use clockNowMsChecked instead of compat.realtimeNowMs",
    },
    .{
        .code = "ZVM001",
        .pattern = "compat.realtimeNowNs",
        .message = "virtual module reads the clock directly",
        .suggestion = "use clockNowNsChecked instead of compat.realtimeNowNs",
    },
    .{
        .code = "ZVM001",
        .pattern = "std.c.write(",
        .message = "virtual module writes to stderr directly",
        .suggestion = "use writeStderrChecked instead of std.c.write",
    },
    .{
        .code = "ZVM001",
        .pattern = "HmacSha256.create",
        .message = "virtual module performs HMAC directly",
        .suggestion = "use hmacSha256Checked instead of HmacSha256.create",
    },
    .{
        .code = "ZVM001",
        .pattern = "Sha256.init",
        .message = "virtual module hashes directly",
        .suggestion = "use sha256Checked instead of Sha256.init",
    },
};

pub fn verifyPaths(
    allocator: std.mem.Allocator,
    paths: []const []const u8,
    options: VerifyOptions,
) !VerifyResult {
    var result = VerifyResult{};
    errdefer result.deinit(allocator);

    for (paths) |path| {
        try result.checked_files.append(allocator, path);
        try auditPath(allocator, path, &result.diagnostics, options);
    }

    return result;
}

pub fn verifyBuiltins(allocator: std.mem.Allocator, options: VerifyOptions) !VerifyResult {
    var result = VerifyResult{};
    errdefer result.deinit(allocator);

    for (builtin_modules.governanceEntries()) |entry| {
        try result.checked_files.append(allocator, entry.module_path);
        try result.checked_files.append(allocator, entry.spec_path);
        try auditModuleAndSpec(allocator, entry.module_path, entry.spec_path, &result.diagnostics, options);
    }

    return result;
}

fn auditPath(
    allocator: std.mem.Allocator,
    path: []const u8,
    diagnostics: *std.ArrayList(OwnedDiagnostic),
    options: VerifyOptions,
) !void {
    if (findBuiltinEntryByModulePath(path)) |entry| {
        const spec_path = try resolveCompanionPath(allocator, path, entry.module_path, entry.spec_path);
        defer allocator.free(spec_path);
        try auditModuleAndSpec(allocator, path, spec_path, diagnostics, options);
        return;
    }

    if (findBuiltinEntryBySpecPath(path)) |entry| {
        const module_path = try resolveCompanionPath(allocator, path, entry.spec_path, entry.module_path);
        defer allocator.free(module_path);

        if (!file_io.fileExists(allocator, module_path)) {
            try appendDiagnostic(
                allocator,
                diagnostics,
                "ZVM006",
                "error",
                "module spec has no matching Zig module file",
                path,
                1,
                1,
                "create or restore the matching file under packages/zigts/src/modules/",
            );
            return;
        }

        try auditModuleContentWithSpecPath(allocator, module_path, path, diagnostics);
        return;
    }

    if (looksLikeBuiltinAdjacentPath(path)) {
        return;
    }

    try appendDiagnostic(
        allocator,
        diagnostics,
        "ZVM000",
        "error",
        "verify-modules only accepts built-in module Zig files or module spec JSON files",
        path,
        1,
        1,
        "pass a path under packages/zigts/src/modules/ or packages/zigts/module-specs/",
    );
}

fn auditModuleAndSpec(
    allocator: std.mem.Allocator,
    module_path: []const u8,
    spec_path: []const u8,
    diagnostics: *std.ArrayList(OwnedDiagnostic),
    options: VerifyOptions,
) !void {
    if (!file_io.fileExists(allocator, spec_path)) {
        try auditModuleContentWithSpecPath(allocator, module_path, null, diagnostics);
        try appendDiagnostic(
            allocator,
            diagnostics,
            "ZVM003",
            if (options.strict) "error" else "warning",
            "built-in module has no module spec artifact",
            module_path,
            1,
            1,
            "add packages/zigts/module-specs/<module>.json for this built-in module",
        );
        return;
    }

    try auditModuleContentWithSpecPath(allocator, module_path, spec_path, diagnostics);
}

fn auditModuleContentWithSpecPath(
    allocator: std.mem.Allocator,
    module_path: []const u8,
    spec_path: ?[]const u8,
    diagnostics: *std.ArrayList(OwnedDiagnostic),
) !void {
    const module_content = file_io.readFile(allocator, module_path, 512 * 1024) catch |err| {
        const message = try std.fmt.allocPrint(allocator, "failed to read module file: {s}", .{@errorName(err)});
        defer allocator.free(message);
        try appendDiagnostic(allocator, diagnostics, "ZVM000", "error", message, module_path, 1, 1, "ensure the file exists and is readable");
        return;
    };
    defer allocator.free(module_content);

    var binding_caps = try parseBindingCapabilities(allocator, module_content);
    defer binding_caps.deinit(allocator);

    const specifier = parseBindingSpecifier(module_content);

    try checkForbiddenPatterns(allocator, module_path, module_content, diagnostics);
    try checkCapabilityHelperDrift(allocator, module_path, module_content, binding_caps.items, diagnostics);

    if (spec_path) |path| {
        const spec_content = file_io.readFile(allocator, path, 256 * 1024) catch |err| {
            const message = try std.fmt.allocPrint(allocator, "failed to read module spec: {s}", .{@errorName(err)});
            defer allocator.free(message);
            try appendDiagnostic(allocator, diagnostics, "ZVM000", "error", message, path, 1, 1, "ensure the spec file exists and is readable");
            return;
        };
        defer allocator.free(spec_content);

        const spec_specifier = parseSpecSpecifier(spec_content);
        var spec_caps = try parseSpecCapabilities(allocator, spec_content);
        defer spec_caps.deinit(allocator);

        if (specifier == null or spec_specifier == null or !std.mem.eql(u8, specifier.?, spec_specifier.?)) {
            try appendDiagnostic(
                allocator,
                diagnostics,
                "ZVM004",
                "error",
                "module specifier differs between Zig binding and module spec",
                path,
                1,
                1,
                "keep `specifier` identical in the Zig binding and the module spec",
            );
        }

        if (!sameStringSet(binding_caps.items, spec_caps.items)) {
            try appendDiagnostic(
                allocator,
                diagnostics,
                "ZVM005",
                "error",
                "requiredCapabilities in the module spec drift from required_capabilities in the Zig binding",
                path,
                1,
                1,
                "update the module spec and binding so they declare the same capability set",
            );
        }
    }
}

fn checkForbiddenPatterns(
    allocator: std.mem.Allocator,
    path: []const u8,
    content: []const u8,
    diagnostics: *std.ArrayList(OwnedDiagnostic),
) !void {
    for (forbidden_patterns) |rule| {
        if (std.mem.indexOf(u8, content, rule.pattern)) |offset| {
            try appendDiagnostic(
                allocator,
                diagnostics,
                rule.code,
                "error",
                rule.message,
                path,
                lineNumber(content, offset),
                1,
                rule.suggestion,
            );
        }
    }
}

fn checkCapabilityHelperDrift(
    allocator: std.mem.Allocator,
    path: []const u8,
    content: []const u8,
    declared_capabilities: []const []const u8,
    diagnostics: *std.ArrayList(OwnedDiagnostic),
) !void {
    for (capability_rules) |rule| {
        var used = false;
        var offset: usize = 0;
        for (rule.helpers) |helper| {
            if (std.mem.indexOf(u8, content, helper)) |idx| {
                used = true;
                offset = idx;
                break;
            }
        }
        if (used and !containsString(declared_capabilities, rule.capability)) {
            const message = try std.fmt.allocPrint(
                allocator,
                "module uses `{s}` capability helpers but does not declare `.{s}` in required_capabilities",
                .{ rule.capability, rule.capability },
            );
            defer allocator.free(message);
            try appendDiagnostic(
                allocator,
                diagnostics,
                "ZVM002",
                "error",
                message,
                path,
                lineNumber(content, offset),
                1,
                "declare the capability in both the Zig binding and the module spec",
            );
        }
    }
}

fn appendDiagnostic(
    allocator: std.mem.Allocator,
    diagnostics: *std.ArrayList(OwnedDiagnostic),
    code: []const u8,
    severity: []const u8,
    message: []const u8,
    file: []const u8,
    line: u32,
    column: u16,
    suggestion: ?[]const u8,
) !void {
    try diagnostics.append(allocator, try OwnedDiagnostic.initOwned(
        allocator,
        code,
        severity,
        message,
        file,
        line,
        column,
        suggestion,
    ));
}

fn parseBindingSpecifier(content: []const u8) ?[]const u8 {
    return parseAssignedString(content, "specifier = ");
}

fn parseSpecSpecifier(content: []const u8) ?[]const u8 {
    return parseJsonStringField(content, "\"specifier\"");
}

fn parseBindingCapabilities(allocator: std.mem.Allocator, content: []const u8) !std.ArrayList([]const u8) {
    var caps: std.ArrayList([]const u8) = .empty;
    errdefer caps.deinit(allocator);

    const marker = "required_capabilities = &.{";
    const start = std.mem.indexOf(u8, content, marker) orelse return caps;
    const body_start = start + marker.len;
    const body_end_rel = std.mem.indexOfScalarPos(u8, content, body_start, '}') orelse return caps;
    const body = content[body_start..body_end_rel];

    var i: usize = 0;
    while (i < body.len) : (i += 1) {
        if (body[i] != '.') continue;
        const start_name = i + 1;
        var end_name = start_name;
        while (end_name < body.len and isIdentChar(body[end_name])) : (end_name += 1) {}
        if (end_name > start_name) {
            const cap = body[start_name..end_name];
            if (!containsString(caps.items, cap)) {
                try caps.append(allocator, cap);
            }
        }
        i = end_name;
    }

    return caps;
}

fn parseSpecCapabilities(allocator: std.mem.Allocator, content: []const u8) !std.ArrayList([]const u8) {
    var caps: std.ArrayList([]const u8) = .empty;
    errdefer caps.deinit(allocator);

    const marker = "\"requiredCapabilities\"";
    const field_idx = std.mem.indexOf(u8, content, marker) orelse return caps;
    const list_start = std.mem.indexOfScalarPos(u8, content, field_idx + marker.len, '[') orelse return caps;
    const list_end = std.mem.indexOfScalarPos(u8, content, list_start + 1, ']') orelse return caps;
    const body = content[list_start + 1 .. list_end];

    var i: usize = 0;
    while (i < body.len) : (i += 1) {
        if (body[i] != '"') continue;
        const start_name = i + 1;
        const end_name = std.mem.indexOfScalarPos(u8, body, start_name, '"') orelse break;
        const cap = body[start_name..end_name];
        if (!containsString(caps.items, cap)) {
            try caps.append(allocator, cap);
        }
        i = end_name;
    }

    return caps;
}

fn parseAssignedString(content: []const u8, marker: []const u8) ?[]const u8 {
    const idx = std.mem.indexOf(u8, content, marker) orelse return null;
    const first_quote = std.mem.indexOfScalarPos(u8, content, idx + marker.len, '"') orelse return null;
    const end_quote = std.mem.indexOfScalarPos(u8, content, first_quote + 1, '"') orelse return null;
    return content[first_quote + 1 .. end_quote];
}

fn parseJsonStringField(content: []const u8, field_name: []const u8) ?[]const u8 {
    const idx = std.mem.indexOf(u8, content, field_name) orelse return null;
    const colon_idx = std.mem.indexOfScalarPos(u8, content, idx + field_name.len, ':') orelse return null;
    const first_quote = std.mem.indexOfScalarPos(u8, content, colon_idx + 1, '"') orelse return null;
    const end_quote = std.mem.indexOfScalarPos(u8, content, first_quote + 1, '"') orelse return null;
    return content[first_quote + 1 .. end_quote];
}

fn looksLikeBuiltinAdjacentPath(path: []const u8) bool {
    const in_modules_dir = std.mem.indexOf(u8, path, "packages/zigts/src/modules/") != null and std.mem.endsWith(u8, path, ".zig");
    const in_specs_dir = std.mem.indexOf(u8, path, "packages/zigts/module-specs/") != null and std.mem.endsWith(u8, path, ".json");
    return in_modules_dir or in_specs_dir;
}

fn findBuiltinEntryByModulePath(path: []const u8) ?*const builtin_modules.BuiltinGovernanceEntry {
    for (builtin_modules.governanceEntries()) |*entry| {
        if (pathMatchesCanonical(path, entry.module_path)) return entry;
    }
    return null;
}

fn findBuiltinEntryBySpecPath(path: []const u8) ?*const builtin_modules.BuiltinGovernanceEntry {
    for (builtin_modules.governanceEntries()) |*entry| {
        if (pathMatchesCanonical(path, entry.spec_path)) return entry;
    }
    return null;
}

fn pathMatchesCanonical(path: []const u8, canonical_path: []const u8) bool {
    if (std.mem.eql(u8, path, canonical_path)) return true;
    if (!std.mem.endsWith(u8, path, canonical_path)) return false;
    const prefix_len = path.len - canonical_path.len;
    return prefix_len > 0 and path[prefix_len - 1] == '/';
}

fn resolveCompanionPath(
    allocator: std.mem.Allocator,
    input_path: []const u8,
    canonical_input: []const u8,
    canonical_target: []const u8,
) ![]u8 {
    if (std.mem.eql(u8, input_path, canonical_input)) {
        return allocator.dupe(u8, canonical_target);
    }

    const prefix_len = input_path.len - canonical_input.len;
    return std.fmt.allocPrint(allocator, "{s}{s}", .{
        input_path[0..prefix_len],
        canonical_target,
    });
}

fn sameStringSet(a: []const []const u8, b: []const []const u8) bool {
    if (a.len != b.len) return false;
    for (a) |item| {
        if (!containsString(b, item)) return false;
    }
    return true;
}

fn containsString(items: []const []const u8, needle: []const u8) bool {
    for (items) |item| {
        if (std.mem.eql(u8, item, needle)) return true;
    }
    return false;
}

fn lineNumber(content: []const u8, offset: usize) u32 {
    var line: u32 = 1;
    for (content[0..@min(offset, content.len)]) |byte| {
        if (byte == '\n') line += 1;
    }
    return line;
}

fn isIdentChar(byte: u8) bool {
    return std.ascii.isAlphanumeric(byte) or byte == '_';
}

test "parse binding capabilities from Zig binding" {
    const source =
        \\pub const binding = mb.ModuleBinding{
        \\    .required_capabilities = &.{ .env, .policy_check },
        \\};
    ;

    var caps = try parseBindingCapabilities(std.testing.allocator, source);
    defer caps.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 2), caps.items.len);
    try std.testing.expect(containsString(caps.items, "env"));
    try std.testing.expect(containsString(caps.items, "policy_check"));
}

test "verify module reports helper use without declared capability" {
    const allocator = std.testing.allocator;
    var diags: std.ArrayList(OwnedDiagnostic) = .empty;
    defer {
        for (diags.items) |*diag| diag.deinit(allocator);
        diags.deinit(allocator);
    }

    const source =
        \\pub const binding = mb.ModuleBinding{
        \\    .specifier = "zigttp:test",
        \\    .required_capabilities = &.{},
        \\};
        \\
        \\fn useClock() void {
        \\    _ = clockNowMsChecked();
        \\}
    ;

    var caps = try parseBindingCapabilities(allocator, source);
    defer caps.deinit(allocator);

    try checkCapabilityHelperDrift(allocator, "packages/zigts/src/modules/test.zig", source, caps.items, &diags);
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    try std.testing.expectEqualStrings("ZVM002", diags.items[0].diag.code);
}

test "registry-driven companion path mapping preserves rooted paths" {
    const allocator = std.testing.allocator;
    const module_path = "/tmp/work/packages/zigts/src/modules/http_mod.zig";
    const spec_path = try resolveCompanionPath(
        allocator,
        module_path,
        "packages/zigts/src/modules/http_mod.zig",
        "packages/zigts/module-specs/http-mod.json",
    );
    defer allocator.free(spec_path);
    try std.testing.expectEqualStrings("/tmp/work/packages/zigts/module-specs/http-mod.json", spec_path);
}

test "writeJsonEnvelope on empty VerifyResult emits the ok envelope" {
    var result: VerifyResult = .{};
    defer result.deinit(std.testing.allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &buf);

    const hash = zigts.rule_registry.policyHash();
    try writeJsonEnvelope(&aw.writer, &result, hash);

    buf = aw.toArrayList();
    const s = buf.items;

    try std.testing.expect(std.mem.indexOf(u8, s, "\"ok\":true") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"checked_files\":[]") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"violations\":[]") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"policy_version\":\"2026.04.2\"") != null);
    try std.testing.expectEqual(@as(u8, '\n'), s[s.len - 1]);
}

test "verifyPaths ignores internal helper files outside the public built-in set" {
    var result = try verifyPaths(
        std.testing.allocator,
        &.{"packages/zigts/src/modules/util.zig"},
        .{},
    );
    defer result.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 1), result.checked_files.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.diagnostics.items.len);
}

test "verifyBuiltins reports the authoritative public module and spec set" {
    var result = try verifyBuiltins(std.testing.allocator, .{});
    defer result.deinit(std.testing.allocator);

    try std.testing.expectEqual(builtin_modules.governanceEntries().len * 2, result.checked_files.items.len);
    try std.testing.expect(containsString(result.checked_files.items, "packages/zigts/src/modules/env.zig"));
    try std.testing.expect(containsString(result.checked_files.items, "packages/zigts/module-specs/env.json"));
}

test "pathMatchesCanonical requires a path-separator boundary" {
    try std.testing.expect(pathMatchesCanonical("packages/zigts/src/modules/env.zig", "packages/zigts/src/modules/env.zig"));
    try std.testing.expect(pathMatchesCanonical("/repo/packages/zigts/src/modules/env.zig", "packages/zigts/src/modules/env.zig"));
    try std.testing.expect(!pathMatchesCanonical("xpackages/zigts/src/modules/env.zig", "packages/zigts/src/modules/env.zig"));
}
