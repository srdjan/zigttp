//! Proof Flight Recorder capsule: a portable, per-handler evidence bundle.
//!
//! A capsule unifies what zttp already produces in scattered places -
//! contract facts, proven specs, recorded replay traces, pinned witnesses,
//! and (Slice 2) signed behavioral-equivalence receipts - into one directory
//! under `.zttp/capsules/<name>/` that can be committed, diffed, and
//! replayed. The `capsule.json` manifest is the index; data files
//! (traces, witnesses) sit beside it and are referenced by relative path.
//!
//! This module owns only the manifest format, its hashing, the redaction
//! helpers that keep secrets out of recorded data, and the on-disk layout.
//! Recording (`dev --record-proof`) and replay (`proof replay`) live in the
//! CLI and reuse `trace.zig` / `replay_runner.zig`; they call in here for the
//! manifest. Manifest parsing fails closed on a schema-version mismatch so a
//! capsule recorded under an incompatible format is never silently replayed.

const std = @import("std");
const json_utils = @import("zts").json_utils;
const Sha256 = std.crypto.hash.sha2.Sha256;

/// Bump when the manifest shape changes incompatibly. `parse` rejects any
/// capsule whose `schemaVersion` differs unless the caller opts into an
/// upgrade, so stale capsules fail closed rather than replay against a format
/// the reader does not understand.
pub const schema_version: u32 = 1;

/// Root directory for all capsules, relative to the project working dir.
pub const capsules_root = ".zttp/capsules";

pub const Route = struct {
    method: []const u8,
    path: []const u8,
};

/// The capsule index. String slices are borrowed: when produced by `parse`
/// they point into the returned `Loaded`'s arena and stay valid until its
/// `deinit`. When built by a caller for `writeJson`, the caller owns them.
pub const Manifest = struct {
    schema_version: u32 = schema_version,
    name: []const u8,
    handler_path: []const u8,
    /// Hex SHA-256 of the handler source bytes.
    handler_hash: []const u8,
    /// Hex SHA-256 of the contract JSON bytes.
    contract_hash: []const u8,
    zttp_version: []const u8,
    /// Hex policy-registry hash, ties the capsule to a rule set.
    policy_hash: []const u8,
    proven_specs: []const []const u8 = &.{},
    declared_specs: []const []const u8 = &.{},
    routes: []const Route = &.{},
    effects: []const []const u8 = &.{},
    /// Relative paths (within the capsule dir) of recorded trace JSONL files.
    trace_files: []const []const u8 = &.{},
    /// Keys of witnesses pinned into this capsule (see witness store).
    witness_keys: []const []const u8 = &.{},
    /// Human-readable notes on what redaction removed, for auditability.
    redaction_notes: []const []const u8 = &.{},
    /// Optional reference (URL or JWS) to a deploy attestation envelope.
    attestation_ref: ?[]const u8 = null,

    /// Serialize as camelCase JSON, matching the `.zttp/proofs.jsonl`
    /// convention. Deterministic field order so the bytes hash stably.
    pub fn writeJson(self: *const Manifest, writer: anytype) !void {
        try writer.writeAll("{");
        try writer.print("\"schemaVersion\":{d}", .{self.schema_version});
        try writeStringField(writer, "name", self.name);
        try writeStringField(writer, "handlerPath", self.handler_path);
        try writeStringField(writer, "handlerHash", self.handler_hash);
        try writeStringField(writer, "contractHash", self.contract_hash);
        try writeStringField(writer, "zttpVersion", self.zttp_version);
        try writeStringField(writer, "policyHash", self.policy_hash);

        try writer.writeAll(",\"provenSpecs\":");
        try writeStringArray(writer, self.proven_specs);
        try writer.writeAll(",\"declaredSpecs\":");
        try writeStringArray(writer, self.declared_specs);

        try writer.writeAll(",\"routes\":[");
        for (self.routes, 0..) |route, i| {
            if (i > 0) try writer.writeAll(",");
            try writer.writeAll("{\"method\":");
            try writeJsonString(writer, route.method);
            try writer.writeAll(",\"path\":");
            try writeJsonString(writer, route.path);
            try writer.writeAll("}");
        }
        try writer.writeAll("]");

        try writer.writeAll(",\"effects\":");
        try writeStringArray(writer, self.effects);
        try writer.writeAll(",\"traceFiles\":");
        try writeStringArray(writer, self.trace_files);
        try writer.writeAll(",\"witnessKeys\":");
        try writeStringArray(writer, self.witness_keys);
        try writer.writeAll(",\"redactionNotes\":");
        try writeStringArray(writer, self.redaction_notes);

        if (self.attestation_ref) |ref| {
            try writer.writeAll(",\"attestationRef\":");
            try writeJsonString(writer, ref);
        }

        try writer.writeAll("}");
    }

    /// Allocate the manifest JSON into a freshly owned buffer.
    pub fn toJsonAlloc(self: *const Manifest, allocator: std.mem.Allocator) ![]u8 {
        var aw: std.Io.Writer.Allocating = .init(allocator);
        defer aw.deinit();
        try self.writeJson(&aw.writer);
        return allocator.dupe(u8, aw.writer.buffered());
    }
};

fn writeStringField(writer: anytype, key: []const u8, value: []const u8) !void {
    try writer.print(",\"{s}\":", .{key});
    try writeJsonString(writer, value);
}

fn writeStringArray(writer: anytype, items: []const []const u8) !void {
    try writer.writeAll("[");
    for (items, 0..) |item, i| {
        if (i > 0) try writer.writeAll(",");
        try writeJsonString(writer, item);
    }
    try writer.writeAll("]");
}

const writeJsonString = json_utils.writeJsonString;

/// A parsed manifest plus the arena backing its string slices. Call `deinit`.
pub const Loaded = struct {
    arena: std.heap.ArenaAllocator,
    manifest: Manifest,

    pub fn deinit(self: *Loaded) void {
        self.arena.deinit();
    }
};

pub const ParseOptions = struct {
    /// When false (default) a `schemaVersion` other than `schema_version`
    /// rejects with `error.SchemaVersionMismatch` (fail closed). Set true to
    /// load an older/newer capsule deliberately during an upgrade.
    allow_version_mismatch: bool = false,
};

/// Parse a `capsule.json`. Unknown fields are ignored (forward compatible);
/// a version mismatch fails closed unless `allow_version_mismatch`.
/// Errors include `error.InvalidManifest`, `error.SchemaVersionMismatch`,
/// `error.MissingField`, plus JSON-parse and allocation errors.
pub fn parse(gpa: std.mem.Allocator, json: []const u8, opts: ParseOptions) !Loaded {
    var arena = std.heap.ArenaAllocator.init(gpa);
    errdefer arena.deinit();
    const a = arena.allocator();

    const root = try std.json.parseFromSliceLeaky(std.json.Value, a, json, .{});
    if (root != .object) return error.InvalidManifest;
    const obj = root.object;

    const sv: u32 = blk: {
        const v = obj.get("schemaVersion") orelse return error.MissingField;
        if (v != .integer or v.integer < 0) return error.InvalidManifest;
        break :blk @intCast(v.integer);
    };
    if (sv != schema_version and !opts.allow_version_mismatch) {
        return error.SchemaVersionMismatch;
    }

    const manifest: Manifest = .{
        .schema_version = sv,
        .name = try reqString(obj, "name"),
        .handler_path = try reqString(obj, "handlerPath"),
        .handler_hash = try reqString(obj, "handlerHash"),
        .contract_hash = try reqString(obj, "contractHash"),
        .zttp_version = try reqString(obj, "zttpVersion"),
        .policy_hash = try reqString(obj, "policyHash"),
        .proven_specs = try optStringArray(a, obj, "provenSpecs"),
        .declared_specs = try optStringArray(a, obj, "declaredSpecs"),
        .routes = try optRoutes(a, obj),
        .effects = try optStringArray(a, obj, "effects"),
        .trace_files = try optStringArray(a, obj, "traceFiles"),
        .witness_keys = try optStringArray(a, obj, "witnessKeys"),
        .redaction_notes = try optStringArray(a, obj, "redactionNotes"),
        .attestation_ref = optString(obj, "attestationRef"),
    };

    return .{ .arena = arena, .manifest = manifest };
}

fn reqString(obj: std.json.ObjectMap, key: []const u8) ![]const u8 {
    const v = obj.get(key) orelse return error.MissingField;
    if (v != .string) return error.InvalidManifest;
    return v.string;
}

fn optString(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const v = obj.get(key) orelse return null;
    if (v != .string) return null;
    return v.string;
}

fn optStringArray(
    a: std.mem.Allocator,
    obj: std.json.ObjectMap,
    key: []const u8,
) ![]const []const u8 {
    const v = obj.get(key) orelse return &.{};
    if (v != .array) return error.InvalidManifest;
    const arr = v.array.items;
    const out = try a.alloc([]const u8, arr.len);
    for (arr, 0..) |item, i| {
        if (item != .string) return error.InvalidManifest;
        out[i] = item.string;
    }
    return out;
}

fn optRoutes(a: std.mem.Allocator, obj: std.json.ObjectMap) ![]const Route {
    const v = obj.get("routes") orelse return &.{};
    if (v != .array) return error.InvalidManifest;
    const arr = v.array.items;
    const out = try a.alloc(Route, arr.len);
    for (arr, 0..) |item, i| {
        if (item != .object) return error.InvalidManifest;
        const ro = item.object;
        const method = ro.get("method") orelse return error.MissingField;
        const path = ro.get("path") orelse return error.MissingField;
        if (method != .string or path != .string) return error.InvalidManifest;
        out[i] = .{ .method = method.string, .path = path.string };
    }
    return out;
}

/// Hex SHA-256 of `bytes`, written into a 64-byte caller buffer.
pub fn hashHex(bytes: []const u8, out: *[64]u8) void {
    var digest: [Sha256.digest_length]u8 = undefined;
    Sha256.hash(bytes, &digest, .{});
    out.* = std.fmt.bytesToHex(digest, .lower);
}

// -- Path helpers -----------------------------------------------------------

/// Join the capsule directory path for `name` into a caller-owned buffer.
pub fn capsuleDirAlloc(allocator: std.mem.Allocator, name: []const u8) ![]u8 {
    return std.fs.path.join(allocator, &.{ capsules_root, name });
}

/// Join the `capsule.json` path for `name` into a caller-owned buffer.
pub fn manifestPathAlloc(allocator: std.mem.Allocator, name: []const u8) ![]u8 {
    return std.fs.path.join(allocator, &.{ capsules_root, name, "capsule.json" });
}

/// Create `.zttp/capsules/<name>/` if absent, one segment at a time. Uses
/// the POSIX `mkdir` idiom the rest of the runtime relies on (`std.fs.cwd()`
/// is unavailable in this build; filesystem access goes through `std.Io` or
/// raw syscalls). `.EXIST` is success — the directory is already there.
pub fn ensureCapsuleDir(allocator: std.mem.Allocator, name: []const u8) !void {
    try mkdirIfAbsent(allocator, ".zttp");
    try mkdirIfAbsent(allocator, capsules_root);
    const dir = try capsuleDirAlloc(allocator, name);
    defer allocator.free(dir);
    try mkdirIfAbsent(allocator, dir);
}

/// Create `path` if absent via POSIX `mkdir` (`std.fs.cwd()` is unavailable
/// in this build). `.EXIST` is success. Exposed so callers that lay out
/// capsule subdirectories (e.g. the recorder's `traces/`) share one idiom.
pub fn mkdirIfAbsent(allocator: std.mem.Allocator, path: []const u8) !void {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);
    switch (std.posix.errno(std.posix.system.mkdir(path_z, 0o755))) {
        .SUCCESS, .EXIST => {},
        else => return error.MakeDirFailed,
    }
}

// -- Redaction --------------------------------------------------------------

/// A secret value the recorder knows at capture time (an env var's value, a
/// derived credential). Redaction replaces every occurrence of `.value` in
/// recorded bytes with `[redacted:<name>]` so the capsule never stores it raw.
pub const SensitiveValue = struct {
    name: []const u8,
    value: []const u8,
};

/// Header keys whose values are stripped wholesale from recorded requests,
/// regardless of value (case-insensitive match).
pub const sensitive_header_keys = [_][]const u8{
    "authorization",
    "cookie",
    "set-cookie",
    "x-api-key",
    "proxy-authorization",
};

/// True when `key` names a header that must never be recorded raw.
pub fn isSensitiveHeader(key: []const u8) bool {
    for (sensitive_header_keys) |sensitive| {
        if (std.ascii.eqlIgnoreCase(key, sensitive)) return true;
    }
    return false;
}

/// Minimum length of a sensitive value worth redacting. Below this, a literal
/// match is too likely to be coincidental noise (e.g. a one-char env value),
/// and blanket replacement would corrupt unrelated bytes.
pub const min_redactable_len: usize = 4;

/// Replace every occurrence of each sensitive value in `input` with a named
/// marker. Returns a freshly owned buffer; `input` is left untouched.
pub fn redactAlloc(
    allocator: std.mem.Allocator,
    input: []const u8,
    values: []const SensitiveValue,
) ![]u8 {
    var current: []u8 = try allocator.dupe(u8, input);
    errdefer allocator.free(current);

    for (values) |sv| {
        if (sv.value.len < min_redactable_len) continue;
        if (std.mem.indexOf(u8, current, sv.value) == null) continue;

        const marker = try std.fmt.allocPrint(allocator, "[redacted:{s}]", .{sv.name});
        defer allocator.free(marker);

        const replaced = try replaceAlloc(allocator, current, sv.value, marker);
        allocator.free(current);
        current = replaced;
    }

    return current;
}

/// Allocate a copy of `haystack` with every `needle` replaced by `replacement`.
fn replaceAlloc(
    allocator: std.mem.Allocator,
    haystack: []const u8,
    needle: []const u8,
    replacement: []const u8,
) ![]u8 {
    const new_len = std.mem.replacementSize(u8, haystack, needle, replacement);
    const out = try allocator.alloc(u8, new_len);
    errdefer allocator.free(out);
    _ = std.mem.replace(u8, haystack, needle, replacement, out);
    return out;
}

// -- Tests ------------------------------------------------------------------

const testing = std.testing;

fn sampleManifest() Manifest {
    return .{
        .name = "checkout",
        .handler_path = "src/handler.ts",
        .handler_hash = "aa",
        .contract_hash = "bb",
        .zttp_version = "0.18.0",
        .policy_hash = "cc",
        .proven_specs = &.{ "pure", "injection_safe" },
        .declared_specs = &.{"injection_safe"},
        .routes = &.{.{ .method = "GET", .path = "/" }},
        .effects = &.{"fetch"},
        .trace_files = &.{"traces/001.jsonl"},
        .witness_keys = &.{"abc123"},
        .redaction_notes = &.{"stripped API_KEY value"},
        .attestation_ref = "https://example.com/.well-known/zttp-attest",
    };
}

test "manifest round-trips through JSON" {
    const original = sampleManifest();
    const json = try original.toJsonAlloc(testing.allocator);
    defer testing.allocator.free(json);

    var loaded = try parse(testing.allocator, json, .{});
    defer loaded.deinit();
    const m = loaded.manifest;

    try testing.expectEqual(schema_version, m.schema_version);
    try testing.expectEqualStrings("checkout", m.name);
    try testing.expectEqualStrings("src/handler.ts", m.handler_path);
    try testing.expectEqualStrings("0.18.0", m.zttp_version);
    try testing.expectEqual(@as(usize, 2), m.proven_specs.len);
    try testing.expectEqualStrings("injection_safe", m.proven_specs[1]);
    try testing.expectEqual(@as(usize, 1), m.routes.len);
    try testing.expectEqualStrings("GET", m.routes[0].method);
    try testing.expectEqualStrings("/", m.routes[0].path);
    try testing.expectEqualStrings("traces/001.jsonl", m.trace_files[0]);
    try testing.expect(m.attestation_ref != null);
    try testing.expectEqualStrings(
        "https://example.com/.well-known/zttp-attest",
        m.attestation_ref.?,
    );
}

test "optional fields default empty when absent" {
    const json =
        \\{"schemaVersion":1,"name":"x","handlerPath":"h.ts","handlerHash":"a",
        \\"contractHash":"b","zttpVersion":"v","policyHash":"c"}
    ;
    var loaded = try parse(testing.allocator, json, .{});
    defer loaded.deinit();
    try testing.expectEqual(@as(usize, 0), loaded.manifest.proven_specs.len);
    try testing.expectEqual(@as(usize, 0), loaded.manifest.routes.len);
    try testing.expect(loaded.manifest.attestation_ref == null);
}

test "version mismatch fails closed" {
    const json =
        \\{"schemaVersion":999,"name":"x","handlerPath":"h.ts","handlerHash":"a",
        \\"contractHash":"b","zttpVersion":"v","policyHash":"c"}
    ;
    try testing.expectError(error.SchemaVersionMismatch, parse(testing.allocator, json, .{}));

    var loaded = try parse(testing.allocator, json, .{ .allow_version_mismatch = true });
    defer loaded.deinit();
    try testing.expectEqual(@as(u32, 999), loaded.manifest.schema_version);
}

test "missing required field rejects" {
    const json =
        \\{"schemaVersion":1,"name":"x"}
    ;
    try testing.expectError(error.MissingField, parse(testing.allocator, json, .{}));
}

test "unknown fields are ignored for forward compatibility" {
    const json =
        \\{"schemaVersion":1,"name":"x","handlerPath":"h.ts","handlerHash":"a",
        \\"contractHash":"b","zttpVersion":"v","policyHash":"c","futureField":42}
    ;
    var loaded = try parse(testing.allocator, json, .{});
    defer loaded.deinit();
    try testing.expectEqualStrings("x", loaded.manifest.name);
}

test "hashHex is stable and lowercase" {
    var a: [64]u8 = undefined;
    var b: [64]u8 = undefined;
    hashHex("hello", &a);
    hashHex("hello", &b);
    try testing.expectEqualStrings(&a, &b);
    // Known SHA-256 of "hello".
    try testing.expectEqualStrings(
        "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824",
        &a,
    );
}

test "redaction replaces sensitive values with named markers" {
    const values = [_]SensitiveValue{
        .{ .name = "API_KEY", .value = "sk-secret-token" },
        .{ .name = "DB_PASS", .value = "hunter2pass" },
    };
    const input = "auth=sk-secret-token; retry sk-secret-token; pw=hunter2pass";
    const out = try redactAlloc(testing.allocator, input, &values);
    defer testing.allocator.free(out);

    try testing.expect(std.mem.indexOf(u8, out, "sk-secret-token") == null);
    try testing.expect(std.mem.indexOf(u8, out, "hunter2pass") == null);
    try testing.expectEqualStrings(
        "auth=[redacted:API_KEY]; retry [redacted:API_KEY]; pw=[redacted:DB_PASS]",
        out,
    );
}

test "redaction skips too-short values to avoid corruption" {
    const values = [_]SensitiveValue{.{ .name = "X", .value = "ab" }};
    const input = "a table about absolutes";
    const out = try redactAlloc(testing.allocator, input, &values);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings(input, out);
}

test "sensitive header detection is case-insensitive" {
    try testing.expect(isSensitiveHeader("Authorization"));
    try testing.expect(isSensitiveHeader("COOKIE"));
    try testing.expect(isSensitiveHeader("x-api-key"));
    try testing.expect(!isSensitiveHeader("content-type"));
}

test "ensureCapsuleDir creates the nested capsule directory" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const tmp_path_len = try tmp.dir.realPath(testing.io, &buf);
    const old_cwd = try std.process.currentPathAlloc(testing.io, testing.allocator);
    defer testing.allocator.free(old_cwd);
    try std.Io.Threaded.chdir(buf[0..tmp_path_len]);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try ensureCapsuleDir(testing.allocator, "checkout");
    // Idempotent: a second call on an existing tree is a no-op, not an error.
    try ensureCapsuleDir(testing.allocator, "checkout");

    const manifest_path = try manifestPathAlloc(testing.allocator, "checkout");
    defer testing.allocator.free(manifest_path);
    const m = sampleManifest();
    const json = try m.toJsonAlloc(testing.allocator);
    defer testing.allocator.free(json);
    // The directory exists, so writing the manifest into it succeeds.
    try @import("zts").file_io.writeFile(testing.allocator, manifest_path, json);
}
