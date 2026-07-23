const std = @import("std");
const zts = @import("zts");
const proof_enrichment = @import("proof_enrichment.zig");
const transcript_mod = @import("transcript.zig");
const ui_payload = @import("ui_payload.zig");
const session_events = @import("session/events.zig");
const session_paths = @import("session/paths.zig");
const reconstructor = @import("session/reconstructor.zig");
const json_writer = @import("providers/anthropic/json_writer.zig");
const tools_common = @import("tools/common.zig");

pub const export_schema_version: u32 = 1;

pub const ExportMeta = struct {
    workspace_hash: []u8,
    session_id: []u8,
    parent_session_id: ?[]u8,
    exported_at_unix_ms: i64,

    pub fn deinit(self: *ExportMeta, allocator: std.mem.Allocator) void {
        allocator.free(self.workspace_hash);
        allocator.free(self.session_id);
        if (self.parent_session_id) |parent| allocator.free(parent);
        self.* = .{
            .workspace_hash = &.{},
            .session_id = &.{},
            .parent_session_id = null,
            .exported_at_unix_ms = 0,
        };
    }
};

pub const LedgerPatch = struct {
    session_id: []u8,
    summary: []u8,
    payload: ui_payload.VerifiedPatchPayload,

    pub fn deinit(self: *LedgerPatch, allocator: std.mem.Allocator) void {
        allocator.free(self.session_id);
        allocator.free(self.summary);
        self.payload.deinit(allocator);
        self.* = .{
            .session_id = &.{},
            .summary = &.{},
            .payload = undefined,
        };
    }
};

pub const ExportBundle = struct {
    meta: ExportMeta,
    patches: []LedgerPatch,

    pub fn deinit(self: *ExportBundle, allocator: std.mem.Allocator) void {
        self.meta.deinit(allocator);
        for (self.patches) |*patch| patch.deinit(allocator);
        allocator.free(self.patches);
        self.* = .{
            .meta = undefined,
            .patches = &.{},
        };
    }
};

pub const ReplayKind = enum {
    success,
    policy_drift,
    violation_drift,
    prove_drift,
    system_drift,
    apply_failure,
};

pub const ReplayResult = struct {
    kind: ReplayKind,
    patch_index: usize,
    file: []u8,
    detail: []u8,

    pub fn deinit(self: *ReplayResult, allocator: std.mem.Allocator) void {
        allocator.free(self.file);
        allocator.free(self.detail);
        self.* = .{
            .kind = .success,
            .patch_index = 0,
            .file = &.{},
            .detail = &.{},
        };
    }
};

pub fn exportSessionLedger(
    allocator: std.mem.Allocator,
    session_id: []const u8,
    out_path: []const u8,
) !void {
    var bundle = try collectSessionLedger(allocator, session_id);
    defer bundle.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try writeMetaLine(w, bundle.meta);
    try w.writeByte('\n');
    for (bundle.patches) |patch| {
        try writePatchLine(w, patch);
        try w.writeByte('\n');
    }

    buf = aw.toArrayList();
    try zts.file_io.writeFile(allocator, out_path, buf.items);
}

pub fn collectSessionLedger(
    allocator: std.mem.Allocator,
    session_id: []const u8,
) !ExportBundle {
    const root = try session_paths.sessionRoot(allocator);
    defer allocator.free(root);
    const hash = try session_paths.cwdHashFull(allocator);

    const entries = try session_paths.listSessions(allocator, root, hash[0..]);
    defer {
        for (entries) |*entry| entry.deinit(allocator);
        allocator.free(entries);
    }

    var by_id: std.StringHashMapUnmanaged(usize) = .empty;
    defer by_id.deinit(allocator);
    try by_id.ensureUnusedCapacity(allocator, @intCast(entries.len));
    for (entries, 0..) |entry, i| {
        by_id.putAssumeCapacity(entry.session_id, i);
    }

    const target_index = by_id.get(session_id) orelse return error.SessionNotFound;
    var chain = std.ArrayList(usize).empty;
    defer chain.deinit(allocator);

    var cursor_index: ?usize = target_index;
    while (cursor_index) |index| {
        try chain.append(allocator, index);
        cursor_index = if (entries[index].parent_id) |parent_id|
            by_id.get(parent_id)
        else
            null;
    }
    std.mem.reverse(usize, chain.items);

    var patches: std.ArrayList(LedgerPatch) = .empty;
    defer patches.deinit(allocator);
    var seen: std.StringHashMapUnmanaged(void) = .empty;
    defer {
        var it = seen.iterator();
        while (it.next()) |entry| allocator.free(entry.key_ptr.*);
        seen.deinit(allocator);
    }

    for (chain.items) |index| {
        const events_path = try std.fs.path.join(allocator, &.{ entries[index].dir_path, "events.jsonl" });
        defer allocator.free(events_path);
        var transcript = reconstructor.reconstructTranscript(allocator, events_path, null) catch |err| switch (err) {
            error.FileNotFound => continue,
            else => return err,
        };
        defer transcript.deinit(allocator);

        for (transcript.entries.items) |*entry| {
            switch (entry.*) {
                .verified_patch => |message| {
                    const patch_payload = if (message.ui_payload) |payload|
                        switch (payload) {
                            .verified_patch => |value| value,
                            else => continue,
                        }
                    else
                        continue;

                    const fingerprint = try patchFingerprint(allocator, patch_payload);
                    defer allocator.free(fingerprint);
                    if (seen.contains(fingerprint)) continue;
                    try seen.put(allocator, try allocator.dupe(u8, fingerprint), {});

                    try patches.append(allocator, .{
                        .session_id = try allocator.dupe(u8, entries[index].session_id),
                        .summary = try allocator.dupe(u8, message.llm_text),
                        .payload = try patch_payload.clone(allocator),
                    });
                },
                else => {},
            }
        }
    }

    return .{
        .meta = .{
            .workspace_hash = try allocator.dupe(u8, hash[0..]),
            .session_id = try allocator.dupe(u8, entries[target_index].session_id),
            .parent_session_id = if (entries[target_index].parent_id) |parent|
                try allocator.dupe(u8, parent)
            else
                null,
            .exported_at_unix_ms = nowUnixMs(),
        },
        .patches = try patches.toOwnedSlice(allocator),
    };
}

pub fn readLedgerFile(
    allocator: std.mem.Allocator,
    input_path: []const u8,
) !ExportBundle {
    const raw = try zts.file_io.readFile(allocator, input_path, 64 * 1024 * 1024);
    defer allocator.free(raw);

    var lines = std.mem.splitScalar(u8, raw, '\n');
    const meta_line = lines.next() orelse return error.InvalidLedgerFile;
    if (meta_line.len == 0) return error.InvalidLedgerFile;

    var bundle = ExportBundle{
        .meta = try parseMetaLine(allocator, meta_line),
        .patches = try allocator.alloc(LedgerPatch, 0),
    };
    errdefer bundle.deinit(allocator);

    var patches = std.ArrayList(LedgerPatch).empty;
    defer patches.deinit(allocator);

    while (lines.next()) |line| {
        if (line.len == 0) continue;
        try patches.append(allocator, try parsePatchLine(allocator, line));
    }

    bundle.patches = try patches.toOwnedSlice(allocator);
    return bundle;
}

pub fn replayBundleInWorkspace(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
    bundle: *const ExportBundle,
) !ReplayResult {
    for (bundle.patches, 0..) |patch, index| {
        const absolute = try tools_common.resolveInsideWorkspace(allocator, workspace_root, patch.payload.file);
        defer allocator.free(absolute);

        const current_before = zts.file_io.readFile(allocator, absolute, 16 * 1024 * 1024) catch |err| switch (err) {
            error.FileNotFound => null,
            else => return err,
        };
        defer if (current_before) |before| allocator.free(before);

        if (!optionalStringEqual(current_before, patch.payload.before)) {
            return .{
                .kind = .apply_failure,
                .patch_index = index,
                .file = try allocator.dupe(u8, patch.payload.file),
                .detail = try allocator.dupe(u8, "base content does not match stored before snapshot"),
            };
        }

        try ensureParentDir(allocator, absolute);
        try zts.file_io.writeFile(allocator, absolute, patch.payload.after);

        var analysis = try proof_enrichment.analyzePatch(
            allocator,
            workspace_root,
            patch.payload.file,
            patch.payload.before,
            patch.payload.after,
            null,
        );
        defer analysis.deinit(allocator);

        const current_hash = zts.rule_registry.policyHash();
        if (!std.mem.eql(u8, &current_hash, patch.payload.policy_hash)) {
            return .{
                .kind = .policy_drift,
                .patch_index = index,
                .file = try allocator.dupe(u8, patch.payload.file),
                .detail = try std.fmt.allocPrint(allocator, "expected {s}, got {s}", .{ patch.payload.policy_hash, &current_hash }),
            };
        }

        if (!sameViolationState(analysis, patch.payload)) {
            return .{
                .kind = .violation_drift,
                .patch_index = index,
                .file = try allocator.dupe(u8, patch.payload.file),
                .detail = try allocator.dupe(u8, "violation summary or delta set changed"),
            };
        }

        if (!sameProveSummary(analysis.prove, patch.payload.prove)) {
            return .{
                .kind = .prove_drift,
                .patch_index = index,
                .file = try allocator.dupe(u8, patch.payload.file),
                .detail = try allocator.dupe(u8, "prove verdict changed"),
            };
        }

        if (!sameSystemSummary(analysis.system, patch.payload.system)) {
            return .{
                .kind = .system_drift,
                .patch_index = index,
                .file = try allocator.dupe(u8, patch.payload.file),
                .detail = try allocator.dupe(u8, "system proof summary changed"),
            };
        }
    }

    return .{
        .kind = .success,
        .patch_index = bundle.patches.len,
        .file = try allocator.dupe(u8, ""),
        .detail = try std.fmt.allocPrint(allocator, "replayed {d} patch(es)", .{bundle.patches.len}),
    };
}

pub fn replayLedgerOntoRef(
    allocator: std.mem.Allocator,
    input_path: []const u8,
    onto_ref: []const u8,
) !ReplayResult {
    var bundle = try readLedgerFile(allocator, input_path);
    defer bundle.deinit(allocator);

    const root = try tools_common.workspaceRoot(allocator);
    defer allocator.free(root);

    const worktree_dir = try std.fmt.allocPrint(allocator, "/tmp/zts-ledger-replay-{d}", .{nowUnixMs()});
    defer allocator.free(worktree_dir);

    const add_argv = [_][]const u8{ "git", "worktree", "add", "--detach", worktree_dir, onto_ref };
    var add_result = try tools_common.runCommand(allocator, root, &add_argv);
    defer add_result.deinit(allocator);
    if (!add_result.ok) return error.WorktreeAddFailed;
    defer {
        const remove_argv = [_][]const u8{ "git", "worktree", "remove", "--force", worktree_dir };
        const maybe_remove = tools_common.runCommand(allocator, root, &remove_argv) catch null;
        if (maybe_remove) |result| {
            var owned = result;
            defer owned.deinit(allocator);
        }
    }

    return replayBundleInWorkspace(allocator, worktree_dir, &bundle);
}

pub fn runWithArgs(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
) !void {
    if (argv.len == 0 or std.mem.eql(u8, argv[0], "--help") or std.mem.eql(u8, argv[0], "help")) {
        printHelp();
        return;
    }

    if (std.mem.eql(u8, argv[0], "export")) {
        var session_id: ?[]const u8 = null;
        var out_path: ?[]const u8 = null;
        var i: usize = 1;
        while (i < argv.len) : (i += 1) {
            if (std.mem.eql(u8, argv[i], "--session")) {
                i += 1;
                if (i >= argv.len) return error.MissingArgument;
                session_id = argv[i];
                continue;
            }
            if (std.mem.eql(u8, argv[i], "--out")) {
                i += 1;
                if (i >= argv.len) return error.MissingArgument;
                out_path = argv[i];
                continue;
            }
            return error.InvalidArgument;
        }
        try exportSessionLedger(allocator, session_id orelse return error.MissingArgument, out_path orelse return error.MissingArgument);
        return;
    }

    if (std.mem.eql(u8, argv[0], "replay")) {
        var input_path: ?[]const u8 = null;
        var onto_ref: ?[]const u8 = null;
        var i: usize = 1;
        while (i < argv.len) : (i += 1) {
            if (std.mem.eql(u8, argv[i], "--input")) {
                i += 1;
                if (i >= argv.len) return error.MissingArgument;
                input_path = argv[i];
                continue;
            }
            if (std.mem.eql(u8, argv[i], "--onto")) {
                i += 1;
                if (i >= argv.len) return error.MissingArgument;
                onto_ref = argv[i];
                continue;
            }
            return error.InvalidArgument;
        }
        var result = try replayLedgerOntoRef(allocator, input_path orelse return error.MissingArgument, onto_ref orelse return error.MissingArgument);
        defer result.deinit(allocator);
        printReplayResult(result);
        if (result.kind != .success) std.process.exit(1);
        return;
    }

    if (std.mem.eql(u8, argv[0], "stats")) {
        try runStats(allocator);
        return;
    }

    return error.UnknownCommand;
}

/// The three per-session signals `ledger stats` aggregates, read back from a
/// session's persisted `session_summary` row.
const SummaryStat = struct {
    reached_proof: bool,
    round_trips_to_first_green: u32,
    proven: u32,
    tracked: u32,
};

fn summaryU32(obj: std.json.ObjectMap, key: []const u8) u32 {
    if (obj.get(key)) |v| {
        if (v == .integer and v.integer >= 0) return @intCast(v.integer);
    }
    return 0;
}

/// Read the last `session_summary` row from a session's events.jsonl. Returns
/// null for a session that never wrote one (empty or in-progress). Best-effort:
/// an unreadable file or a malformed line is skipped, never fatal.
fn readLastSessionSummary(allocator: std.mem.Allocator, events_path: []const u8) !?SummaryStat {
    const bytes = zts.file_io.readFile(allocator, events_path, 16 * 1024 * 1024) catch return null;
    defer allocator.free(bytes);

    var found: ?SummaryStat = null;
    var it = std.mem.splitScalar(u8, bytes, '\n');
    while (it.next()) |line| {
        if (line.len == 0) continue;
        // Cheap pre-filter before the JSON parse.
        if (std.mem.indexOf(u8, line, "\"session_summary\"") == null) continue;
        var parsed = std.json.parseFromSlice(std.json.Value, allocator, line, .{}) catch continue;
        defer parsed.deinit();
        if (parsed.value != .object) continue;
        const root_obj = parsed.value.object;
        const k = root_obj.get("k") orelse continue;
        if (k != .string or !std.mem.eql(u8, k.string, "session_summary")) continue;
        const d = root_obj.get("d") orelse continue;
        if (d != .object) continue;
        const dobj = d.object;
        found = .{
            .reached_proof = if (dobj.get("reached_proof")) |v| (v == .bool and v.bool) else false,
            .round_trips_to_first_green = summaryU32(dobj, "round_trips_to_first_green"),
            .proven = summaryU32(dobj, "proven_properties"),
            .tracked = summaryU32(dobj, "tracked_properties"),
        };
    }
    return found;
}

fn medianU32(items: []u32) f64 {
    if (items.len == 0) return 0;
    std.mem.sort(u32, items, {}, std.sort.asc(u32));
    const mid = items.len / 2;
    if (items.len % 2 == 1) return @floatFromInt(items[mid]);
    return (@as(f64, @floatFromInt(items[mid - 1])) + @as(f64, @floatFromInt(items[mid]))) / 2.0;
}

fn medianF32(items: []f32) f64 {
    if (items.len == 0) return 0;
    std.mem.sort(f32, items, {}, std.sort.asc(f32));
    const mid = items.len / 2;
    if (items.len % 2 == 1) return items[mid];
    return (@as(f64, items[mid - 1]) + @as(f64, items[mid])) / 2.0;
}

/// Aggregate every session's `session_summary` for the current workspace into
/// the three STRATEGY.md metrics (expert success rate, median round-trips to
/// first green, median proven-path ratio) and print them. These were staked as
/// cross-session medians/rates but had no measurement home until now.
fn runStats(allocator: std.mem.Allocator) !void {
    const root = try session_paths.sessionRoot(allocator);
    defer allocator.free(root);
    const hash = try session_paths.cwdHashFull(allocator);
    const entries = try session_paths.listSessions(allocator, root, hash[0..]);
    defer {
        for (entries) |*e| e.deinit(allocator);
        allocator.free(entries);
    }

    var green: std.ArrayListUnmanaged(u32) = .empty;
    defer green.deinit(allocator);
    var ratios: std.ArrayListUnmanaged(f32) = .empty;
    defer ratios.deinit(allocator);
    var total: usize = 0;
    var proof_reached: usize = 0;

    for (entries) |e| {
        const events_path = try std.fs.path.join(allocator, &.{ e.dir_path, "events.jsonl" });
        defer allocator.free(events_path);
        const stat = (try readLastSessionSummary(allocator, events_path)) orelse continue;
        total += 1;
        if (stat.reached_proof) {
            proof_reached += 1;
            try green.append(allocator, stat.round_trips_to_first_green);
            const ratio: f32 = if (stat.tracked == 0)
                0
            else
                @as(f32, @floatFromInt(stat.proven)) / @as(f32, @floatFromInt(stat.tracked));
            try ratios.append(allocator, ratio);
        }
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;
    if (total == 0) {
        try w.writeAll("ledger stats: no sessions with a summary yet for this workspace.\n");
    } else {
        const success_pct = proof_reached * 100 / total;
        try w.print("ledger stats for this workspace ({d} session{s} with a summary)\n", .{ total, if (total == 1) "" else "s" });
        try w.print("  expert success rate:         {d}%  ({d}/{d} reached a verified proof)\n", .{ success_pct, proof_reached, total });
        if (proof_reached == 0) {
            try w.writeAll("  round-trips to first green:  n/a  (no proof sessions yet)\n");
            try w.writeAll("  proven-path ratio:           n/a  (no proof sessions yet)\n");
        } else {
            try w.print("  round-trips to first green:  median {d:.1}  (over {d} proof session{s})\n", .{ medianU32(green.items), proof_reached, if (proof_reached == 1) "" else "s" });
            try w.print("  proven-path ratio:           median {d:.3}  (over {d} proof session{s})\n", .{ medianF32(ratios.items), proof_reached, if (proof_reached == 1) "" else "s" });
        }
    }
    buf = aw.toArrayList();
    _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
}

fn writeMetaLine(writer: *std.Io.Writer, meta: ExportMeta) !void {
    try writer.writeAll("{\"kind\":\"meta\",\"schema_version\":");
    try writer.print("{d}", .{export_schema_version});
    try writer.writeAll(",\"workspace_hash\":");
    try json_writer.writeString(writer, meta.workspace_hash);
    try writer.writeAll(",\"session_id\":");
    try json_writer.writeString(writer, meta.session_id);
    try writer.writeAll(",\"parent_session_id\":");
    if (meta.parent_session_id) |parent| {
        try json_writer.writeString(writer, parent);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(",\"exported_at_unix_ms\":");
    try writer.print("{d}", .{meta.exported_at_unix_ms});
    try writer.writeByte('}');
}

fn writePatchLine(writer: *std.Io.Writer, patch: LedgerPatch) !void {
    try writer.writeAll("{\"kind\":\"verified_patch\",\"session_id\":");
    try json_writer.writeString(writer, patch.session_id);
    try writer.writeAll(",\"summary\":");
    try json_writer.writeString(writer, patch.summary);
    try writer.writeAll(",\"payload\":");
    try ui_payload.writeJson(writer, .{ .verified_patch = patch.payload });
    try writer.writeByte('}');
}

fn parseMetaLine(allocator: std.mem.Allocator, line: []const u8) !ExportMeta {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, line, .{}) catch return error.InvalidLedgerFile;
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidLedgerFile;
    const obj = parsed.value.object;
    if (!valueIsString(obj.get("kind"), "meta")) return error.InvalidLedgerFile;
    const version = obj.get("schema_version") orelse return error.InvalidLedgerFile;
    if (version != .integer or version.integer != export_schema_version) return error.InvalidLedgerFile;

    return .{
        .workspace_hash = try allocator.dupe(u8, getString(obj, "workspace_hash") orelse return error.InvalidLedgerFile),
        .session_id = try allocator.dupe(u8, getString(obj, "session_id") orelse return error.InvalidLedgerFile),
        .parent_session_id = if (try getOptionalString(obj, "parent_session_id")) |parent|
            try allocator.dupe(u8, parent)
        else
            null,
        .exported_at_unix_ms = getInteger(obj, "exported_at_unix_ms") orelse return error.InvalidLedgerFile,
    };
}

fn parsePatchLine(allocator: std.mem.Allocator, line: []const u8) !LedgerPatch {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, line, .{}) catch return error.InvalidLedgerFile;
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidLedgerFile;
    const obj = parsed.value.object;
    if (!valueIsString(obj.get("kind"), "verified_patch")) return error.InvalidLedgerFile;

    const payload_val = obj.get("payload") orelse return error.InvalidLedgerFile;
    var payload_union = try ui_payload.parse(allocator, payload_val);
    errdefer payload_union.deinit(allocator);

    return switch (payload_union) {
        .verified_patch => |payload| .{
            .session_id = try allocator.dupe(u8, getString(obj, "session_id") orelse return error.InvalidLedgerFile),
            .summary = try allocator.dupe(u8, getString(obj, "summary") orelse return error.InvalidLedgerFile),
            .payload = payload,
        },
        else => error.InvalidLedgerFile,
    };
}

fn patchFingerprint(
    allocator: std.mem.Allocator,
    patch: ui_payload.VerifiedPatchPayload,
) ![]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(patch.file);
    hasher.update("\x00");
    hasher.update(patch.policy_hash);
    hasher.update("\x00");
    if (patch.before) |before| hasher.update(before);
    hasher.update("\x00");
    hasher.update(patch.after);
    hasher.update("\x00");
    var time_buf: [32]u8 = undefined;
    const time_text = try std.fmt.bufPrint(&time_buf, "{d}", .{patch.applied_at_unix_ms});
    hasher.update(time_text);
    var digest: [std.crypto.hash.sha2.Sha256.digest_length]u8 = undefined;
    hasher.final(&digest);
    return std.fmt.allocPrint(allocator, "{s}", .{std.fmt.bytesToHex(digest, .lower)});
}

fn optionalStringEqual(a: ?[]const u8, b: ?[]const u8) bool {
    if (a == null and b == null) return true;
    if (a == null or b == null) return false;
    return std.mem.eql(u8, a.?, b.?);
}

fn ensureParentDir(allocator: std.mem.Allocator, absolute_path: []const u8) !void {
    if (std.fs.path.dirname(absolute_path)) |parent| {
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io_backend.io(), parent);
    }
}

fn sameViolationState(
    analysis: proof_enrichment.PatchAnalysis,
    payload: ui_payload.VerifiedPatchPayload,
) bool {
    if (analysis.stats.total != payload.stats.total) return false;
    if (analysis.stats.new != payload.stats.new) return false;
    if ((analysis.stats.preexisting orelse 0) != (payload.stats.preexisting orelse 0)) return false;
    if (analysis.violations.len != payload.violations.len) return false;
    for (analysis.violations, payload.violations) |left, right| {
        if (!std.mem.eql(u8, left.stable_key, right.stable_key)) return false;
        if (!std.mem.eql(u8, left.code, right.code)) return false;
        if (!std.mem.eql(u8, left.severity, right.severity)) return false;
        if (!std.mem.eql(u8, left.message, right.message)) return false;
        if (left.line != right.line or left.column != right.column) return false;
        if (left.introduced_by_patch != right.introduced_by_patch) return false;
    }
    return true;
}

fn sameProveSummary(
    left: ?ui_payload.ProveSummary,
    right: ?ui_payload.ProveSummary,
) bool {
    if (left == null and right == null) return true;
    if (left == null or right == null) return false;
    const a = left.?;
    const b = right.?;
    if (!std.mem.eql(u8, a.classification, b.classification)) return false;
    if (!std.mem.eql(u8, a.proof_level, b.proof_level)) return false;
    if (!optionalStringEqual(a.counterexample, b.counterexample)) return false;
    if (a.laws_used.len != b.laws_used.len) return false;
    for (a.laws_used, b.laws_used) |law_a, law_b| {
        if (!std.mem.eql(u8, law_a, law_b)) return false;
    }
    return true;
}

fn sameSystemSummary(
    left: ?ui_payload.SystemProofSummary,
    right: ?ui_payload.SystemProofSummary,
) bool {
    if (left == null and right == null) return true;
    if (left == null or right == null) return false;
    const a = left.?;
    const b = right.?;
    return std.mem.eql(u8, a.proof_level, b.proof_level) and
        a.all_links_resolved == b.all_links_resolved and
        a.all_responses_covered == b.all_responses_covered and
        a.payload_compatible == b.payload_compatible and
        a.injection_safe == b.injection_safe and
        a.no_secret_leakage == b.no_secret_leakage and
        a.no_credential_leakage == b.no_credential_leakage and
        a.retry_safe == b.retry_safe and
        a.fault_covered == b.fault_covered and
        a.state_isolated == b.state_isolated and
        a.max_system_io_depth == b.max_system_io_depth and
        a.dynamic_links == b.dynamic_links;
}

fn valueIsString(value: ?std.json.Value, expected: []const u8) bool {
    const actual = value orelse return false;
    return actual == .string and std.mem.eql(u8, actual.string, expected);
}

fn getString(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const value = obj.get(key) orelse return null;
    return if (value == .string) value.string else null;
}

fn getOptionalString(obj: std.json.ObjectMap, key: []const u8) !?[]const u8 {
    const value = obj.get(key) orelse return null;
    return switch (value) {
        .null => null,
        .string => value.string,
        else => error.InvalidLedgerFile,
    };
}

fn getInteger(obj: std.json.ObjectMap, key: []const u8) ?i64 {
    const value = obj.get(key) orelse return null;
    return if (value == .integer) value.integer else null;
}

fn nowUnixMs() i64 {
    var ts: std.posix.timespec = undefined;
    _ = std.c.clock_gettime(@enumFromInt(@intFromEnum(std.posix.CLOCK.REALTIME)), &ts);
    return @as(i64, ts.sec) * 1000 + @divTrunc(@as(i64, ts.nsec), 1_000_000);
}

fn printReplayResult(result: ReplayResult) void {
    const line = std.fmt.allocPrint(std.heap.smp_allocator, "{s}: patch {d} {s} - {s}\n", .{
        @tagName(result.kind),
        result.patch_index,
        result.file,
        result.detail,
    }) catch return;
    defer std.heap.smp_allocator.free(line);
    _ = std.c.write(std.c.STDERR_FILENO, line.ptr, line.len);
}

fn printHelp() void {
    const help =
        \\zttp ledger - export, replay, or aggregate verified_patch ledgers
        \\
        \\Usage:
        \\  zttp ledger export --session <id> --out <path>
        \\  zttp ledger replay --input <path> --onto <git-ref>
        \\  zttp ledger stats
        \\
        \\`stats` aggregates every session's summary for the current workspace
        \\into the staked metrics: expert success rate, median round-trips to
        \\first green proof, and median proven-path ratio.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

const testing = std.testing;
const IsolatedTmp = @import("test_support/tmp.zig").IsolatedTmp;
const EnvOverride = @import("test_support/env.zig").EnvOverride;

fn initTmp(allocator: std.mem.Allocator) !IsolatedTmp {
    return IsolatedTmp.init(allocator, "ledger");
}

test "collectSessionLedger exports empty ledger when events file is missing" {
    var tmp = try initTmp(testing.allocator);
    defer tmp.cleanup(testing.allocator);

    var env_override = try EnvOverride.set(testing.allocator, "ZTTP_SESSIONS_DIR", tmp.abs_path);
    defer env_override.restore(testing.allocator);

    const hash = try session_paths.cwdHashFull(testing.allocator);
    const session_dir = try std.fs.path.join(testing.allocator, &.{ tmp.abs_path, hash[0..], "sess-fresh" });
    defer testing.allocator.free(session_dir);

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io_backend.io(), session_dir);

    const meta_path = try std.fs.path.join(testing.allocator, &.{ session_dir, "meta.json" });
    defer testing.allocator.free(meta_path);
    try session_events.writeMeta(testing.allocator, meta_path, .{
        .session_id = "sess-fresh",
        .workspace_realpath = tmp.abs_path,
        .created_at_unix_ms = 1,
        .parent_id = null,
    });

    var bundle = try collectSessionLedger(testing.allocator, "sess-fresh");
    defer bundle.deinit(testing.allocator);

    try testing.expectEqualStrings("sess-fresh", bundle.meta.session_id);
    try testing.expectEqual(@as(usize, 0), bundle.patches.len);
}

test "readLedgerFile round-trips exported verified patch payload" {
    var tmp = try initTmp(testing.allocator);
    defer tmp.cleanup(testing.allocator);

    const path = try tmp.childPath(testing.allocator, "ledger.ndjson");
    defer testing.allocator.free(path);

    const file_copy = try testing.allocator.dupe(u8, "handler.ts");
    const policy_copy = try testing.allocator.dupe(u8, "a" ** 64);
    const after_copy = try testing.allocator.dupe(u8, "after");
    const diff_copy = try testing.allocator.dupe(u8, "@@ -0,0 +1,1 @@\n+after\n");
    const citations = try testing.allocator.alloc([]u8, 1);
    citations[0] = try testing.allocator.dupe(u8, "ZTS204");
    const hunks = try testing.allocator.alloc(ui_payload.DiffHunk, 1);
    hunks[0] = .{ .old_start = 0, .old_count = 0, .new_start = 1, .new_count = 1 };

    const patch = LedgerPatch{
        .session_id = try testing.allocator.dupe(u8, "sess-1"),
        .summary = try testing.allocator.dupe(u8, "verified: handler.ts"),
        .payload = .{
            .file = file_copy,
            .policy_hash = policy_copy,
            .applied_at_unix_ms = 42,
            .stats = .{ .total = 0, .new = 0, .preexisting = 0 },
            .before = null,
            .after = after_copy,
            .unified_diff = diff_copy,
            .hunks = hunks,
            .violations = try testing.allocator.alloc(ui_payload.ViolationDeltaItem, 0),
            .before_properties = null,
            .after_properties = null,
            .prove = null,
            .system = null,
            .rule_citations = citations,
            .post_apply_ok = true,
            .post_apply_summary = null,
        },
    };
    defer {
        var owned = patch;
        owned.deinit(testing.allocator);
    }

    const meta = ExportMeta{
        .workspace_hash = try testing.allocator.dupe(u8, "h" ** 64),
        .session_id = try testing.allocator.dupe(u8, "sess-1"),
        .parent_session_id = null,
        .exported_at_unix_ms = 99,
    };
    defer {
        var owned = meta;
        owned.deinit(testing.allocator);
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);
    try writeMetaLine(&aw.writer, meta);
    try aw.writer.writeByte('\n');
    try writePatchLine(&aw.writer, patch);
    try aw.writer.writeByte('\n');
    buf = aw.toArrayList();
    try zts.file_io.writeFile(testing.allocator, path, buf.items);

    var bundle = try readLedgerFile(testing.allocator, path);
    defer bundle.deinit(testing.allocator);

    try testing.expectEqualStrings("sess-1", bundle.meta.session_id);
    try testing.expectEqual(@as(usize, 1), bundle.patches.len);
    try testing.expectEqualStrings("handler.ts", bundle.patches[0].payload.file);
    try testing.expectEqualStrings("ZTS204", bundle.patches[0].payload.rule_citations[0]);
}

test "replayBundleInWorkspace detects policy drift" {
    var tmp = try initTmp(testing.allocator);
    defer tmp.cleanup(testing.allocator);

    const file_path = try tmp.childPath(testing.allocator, "handler.ts");
    defer testing.allocator.free(file_path);
    try zts.file_io.writeFile(testing.allocator, file_path, "function handler(req: Request): Response { return Response.json({ ok: true }); }");

    const patches = try testing.allocator.alloc(LedgerPatch, 1);
    patches[0] = .{
        .session_id = try testing.allocator.dupe(u8, "sess-1"),
        .summary = try testing.allocator.dupe(u8, "verified: handler.ts"),
        .payload = .{
            .file = try testing.allocator.dupe(u8, "handler.ts"),
            .policy_hash = try testing.allocator.dupe(u8, "b" ** 64),
            .applied_at_unix_ms = 1,
            .stats = .{ .total = 0, .new = 0, .preexisting = 0 },
            .before = try testing.allocator.dupe(u8, "function handler(req: Request): Response { return Response.json({ ok: true }); }"),
            .after = try testing.allocator.dupe(u8, "function handler(req: Request): Response { return Response.json({ ok: true }); }"),
            .unified_diff = try testing.allocator.alloc(u8, 0),
            .hunks = try testing.allocator.alloc(ui_payload.DiffHunk, 0),
            .violations = try testing.allocator.alloc(ui_payload.ViolationDeltaItem, 0),
            .before_properties = null,
            .after_properties = null,
            .prove = null,
            .system = null,
            .rule_citations = try testing.allocator.alloc([]u8, 0),
            .post_apply_ok = true,
            .post_apply_summary = null,
        },
    };
    var bundle = ExportBundle{
        .meta = .{
            .workspace_hash = try testing.allocator.dupe(u8, "h" ** 64),
            .session_id = try testing.allocator.dupe(u8, "sess-1"),
            .parent_session_id = null,
            .exported_at_unix_ms = 1,
        },
        .patches = patches,
    };
    defer bundle.deinit(testing.allocator);

    var result = try replayBundleInWorkspace(testing.allocator, tmp.abs_path, &bundle);
    defer result.deinit(testing.allocator);

    try testing.expectEqual(ReplayKind.policy_drift, result.kind);
    try testing.expectEqualStrings("handler.ts", result.file);
}

test "medianU32 handles odd, even, and empty inputs" {
    var odd = [_]u32{ 3, 1, 2 };
    try testing.expectEqual(@as(f64, 2), medianU32(&odd));
    var even = [_]u32{ 4, 1, 3, 2 };
    try testing.expectEqual(@as(f64, 2.5), medianU32(&even));
    var empty = [_]u32{};
    try testing.expectEqual(@as(f64, 0), medianU32(&empty));
}

test "readLastSessionSummary returns the final summary row's staked signals" {
    var tmp = try initTmp(testing.allocator);
    defer tmp.cleanup(testing.allocator);
    const events_path = try tmp.childPath(testing.allocator, "events.jsonl");
    defer testing.allocator.free(events_path);

    // An early summary that did not reach proof...
    try session_events.appendEvent(testing.allocator, events_path, .{ .session_summary = .{
        .reached_proof = false,
    } });
    // ...superseded by a later one that did (e.g. after --resume). The last row
    // wins, so the aggregate reflects the session's final state.
    try session_events.appendEvent(testing.allocator, events_path, .{ .session_summary = .{
        .reached_proof = true,
        .round_trips_to_first_green = 5,
        .proven_properties = 12,
        .tracked_properties = 16,
    } });

    const stat = (try readLastSessionSummary(testing.allocator, events_path)).?;
    try testing.expect(stat.reached_proof);
    try testing.expectEqual(@as(u32, 5), stat.round_trips_to_first_green);
    try testing.expectEqual(@as(u32, 12), stat.proven);
    try testing.expectEqual(@as(u32, 16), stat.tracked);
}

test "readLastSessionSummary returns null when a session wrote no summary" {
    var tmp = try initTmp(testing.allocator);
    defer tmp.cleanup(testing.allocator);
    const events_path = try tmp.childPath(testing.allocator, "events.jsonl");
    defer testing.allocator.free(events_path);
    try session_events.appendEvent(testing.allocator, events_path, .{ .user_text = "hello" });
    try testing.expect((try readLastSessionSummary(testing.allocator, events_path)) == null);
}
