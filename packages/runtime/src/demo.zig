const std = @import("std");
const zigts = @import("zigts");
const pi_app = @import("pi_app");
const proof_ledger = @import("proof_ledger.zig");
const self_extract = @import("self_extract.zig");
const shared = @import("cli_shared.zig");

extern "c" fn setenv(name: [*:0]const u8, value: [*:0]const u8, overwrite: c_int) c_int;
extern "c" fn unsetenv(name: [*:0]const u8) c_int;

pub const action_path = "/_zigttp/studio/demo/action";
pub const state_path = "/_zigttp/studio/demo/state.json";
const deploy_marker_path = ".zigttp/demo-deployed";

pub const Action = enum {
    introduce_bug,
    repair_bug,
    deploy,
    reset,

    pub fn toString(self: Action) []const u8 {
        return switch (self) {
            .introduce_bug => "introduce_bug",
            .repair_bug => "repair_bug",
            .deploy => "deploy",
            .reset => "reset",
        };
    }
};

pub const Step = enum {
    baseline,
    witness,
    repaired,
    deployed,

    pub fn toString(self: Step) []const u8 {
        return switch (self) {
            .baseline => "baseline",
            .witness => "witness",
            .repaired => "repaired",
            .deployed => "deployed",
        };
    }
};

pub const Config = struct {
    workspace_root: []const u8,
    handler_path: []const u8,
};

pub const PassportExport = struct {
    out_dir: []const u8,
    step: Step,
};

pub const Workspace = struct {
    root: []u8,
    owned_temp: bool,

    pub fn deinit(self: *Workspace, allocator: std.mem.Allocator) void {
        allocator.free(self.root);
        self.* = undefined;
    }

    pub fn cleanup(self: *Workspace, allocator: std.mem.Allocator) void {
        if (!self.owned_temp) return;
        var io_backend = shared.threadedIo(allocator);
        defer io_backend.deinit();
        const io = io_backend.io();
        const parent = std.fs.path.dirname(self.root) orelse return;
        const name = std.fs.path.basename(self.root);
        var dir = std.Io.Dir.openDirAbsolute(io, parent, .{}) catch return;
        defer dir.close(io);
        dir.deleteTree(io, name) catch {};
    }
};

pub fn parseActionName(name: []const u8) ?Action {
    if (std.mem.eql(u8, name, "introduce_bug")) return .introduce_bug;
    if (std.mem.eql(u8, name, "repair_bug")) return .repair_bug;
    if (std.mem.eql(u8, name, "deploy")) return .deploy;
    if (std.mem.eql(u8, name, "reset")) return .reset;
    return null;
}

pub fn parseActionBody(allocator: std.mem.Allocator, body: ?[]const u8) !Action {
    const bytes = body orelse return error.MissingDemoAction;
    if (bytes.len > 1024) return error.InvalidDemoAction;

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, bytes, .{}) catch return error.InvalidDemoAction;
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidDemoAction;
    const value = parsed.value.object.get("action") orelse return error.MissingDemoAction;
    if (value != .string) return error.InvalidDemoAction;
    return parseActionName(value.string) orelse error.UnknownDemoAction;
}

pub fn createWorkspace(
    allocator: std.mem.Allocator,
    out_dir: ?[]const u8,
    port: u16,
) !Workspace {
    const root = if (out_dir) |dir| blk: {
        try validateOutputDir(dir);
        if (pathExists(dir)) return error.OutputExists;
        break :blk try std.fs.path.resolve(allocator, &.{dir});
    } else try tempWorkspacePath(allocator);
    errdefer allocator.free(root);

    try scaffoldWorkspace(allocator, root, port);
    return .{ .root = root, .owned_temp = out_dir == null };
}

fn validateOutputDir(path: []const u8) !void {
    if (path.len == 0) return error.InvalidOutputPath;
    const base = std.fs.path.basename(path);
    if (base.len == 0) return error.InvalidOutputPath;
    if (std.mem.eql(u8, base, ".") or std.mem.eql(u8, base, "..")) return error.InvalidOutputPath;
    if (base[0] == '.') return error.InvalidOutputPath;
    for (base) |c| {
        const ok = std.ascii.isAlphanumeric(c) or c == '-' or c == '_';
        if (!ok) return error.InvalidOutputPath;
    }
}

pub fn scaffoldWorkspace(allocator: std.mem.Allocator, root: []const u8, port: u16) !void {
    var io_backend = shared.threadedIo(allocator);
    defer io_backend.deinit();
    const io = io_backend.io();

    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, root);
    try createChildDir(allocator, io, root, "src");
    try createChildDir(allocator, io, root, "tests");
    try createChildDir(allocator, io, root, ".zigttp");

    const manifest = try std.fmt.allocPrint(
        allocator,
        "{{\n  \"entry\": \"src/handler.tsx\",\n  \"host\": \"127.0.0.1\",\n  \"port\": {d}\n}}\n",
        .{port},
    );
    defer allocator.free(manifest);

    try writeRelativeFile(allocator, root, "zigttp.json", manifest);
    try writeRelativeFile(allocator, root, "src/handler.tsx", baseline_source);
    try writeRelativeFile(allocator, root, "tests/handler.test.jsonl", demo_tests);
    try writeRelativeFile(allocator, root, "README.md", demo_readme);
    try writeRelativeFile(allocator, root, ".gitignore", demo_gitignore);
}

pub fn applyAction(allocator: std.mem.Allocator, config: Config, action: Action) !Step {
    switch (action) {
        .introduce_bug => {
            try zigts.file_io.writeFile(allocator, config.handler_path, bug_source);
            var info = try pi_app.demo_passport.appendStep(allocator, .{
                .workspace_root = config.workspace_root,
                .handler_path = config.handler_path,
                .step = .witness,
            });
            info.deinit(allocator);
            return .witness;
        },
        .repair_bug => {
            const before = zigts.file_io.readFile(allocator, config.handler_path, 1024 * 1024) catch try allocator.dupe(u8, bug_source);
            defer allocator.free(before);
            try zigts.file_io.writeFile(allocator, config.handler_path, repaired_source);
            var info = try pi_app.demo_passport.appendStep(allocator, .{
                .workspace_root = config.workspace_root,
                .handler_path = config.handler_path,
                .step = .repaired,
                .before = before,
                .after = repaired_source,
            });
            info.deinit(allocator);
            return .repaired;
        },
        .reset => {
            try zigts.file_io.writeFile(allocator, config.handler_path, baseline_source);
            deleteLedger(allocator);
            var info = try pi_app.demo_passport.resetToBaseline(allocator, config.workspace_root);
            info.deinit(allocator);
            return .baseline;
        },
        .deploy => {
            const step = try detectStep(allocator, config);
            if (step == .witness) return error.DemoNeedsRepair;
            try runLocalDeploy(allocator);
            const artifact = try deployArtifactPath(allocator, config.workspace_root);
            defer allocator.free(artifact);
            var info = try pi_app.demo_passport.appendStep(allocator, .{
                .workspace_root = config.workspace_root,
                .handler_path = config.handler_path,
                .step = .deployed,
                .deploy_artifact = artifact,
            });
            info.deinit(allocator);
            return .deployed;
        },
    }
}

pub fn detectStep(allocator: std.mem.Allocator, config: Config) !Step {
    if (deployMarkerExists(allocator)) return .deployed;
    const source = zigts.file_io.readFile(allocator, config.handler_path, 1024 * 1024) catch return .baseline;
    defer allocator.free(source);
    if (std.mem.indexOf(u8, source, "SECRET_KEY") != null) return .witness;
    if (std.mem.indexOf(u8, source, "repair marker") != null) return .repaired;
    return .baseline;
}

pub fn writeStateJson(
    allocator: std.mem.Allocator,
    config: Config,
    proof_json: []const u8,
) ![]u8 {
    const step = try detectStep(allocator, config);
    var passport = try pi_app.demo_passport.ensureSession(allocator, config.workspace_root);
    defer passport.deinit(allocator);

    var parsed_proof = std.json.parseFromSlice(std.json.Value, allocator, proof_json, .{}) catch null;
    defer if (parsed_proof) |*parsed| parsed.deinit();
    const proof_certificate = if (parsed_proof) |*parsed| proofCertificate(parsed.value) else null;
    const caller_receipt = if (parsed_proof) |*parsed| callerReceiptObject(parsed.value) else null;

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("mode");
    try json.write("proof_theater");
    try json.objectField("workspace");
    try json.write(config.workspace_root);
    try json.objectField("handlerPath");
    try json.write(config.handler_path);
    try json.objectField("sessionId");
    try json.write(passport.session_id);
    try json.objectField("expertCommand");
    try json.write(passport.expert_command);
    if (caller_receipt) |receipt| {
        if (objectString(receipt, "verifyUrl")) |value| {
            try json.objectField("verifyUrl");
            try json.write(value);
        }
        if (objectString(receipt, "verifyCommand")) |value| {
            try json.objectField("verifyCommand");
            try json.write(value);
        }
        if (objectString(receipt, "wellKnownUrl")) |value| {
            try json.objectField("wellKnownUrl");
            try json.write(value);
        }
        if (objectString(receipt, "keyFingerprint")) |value| {
            try json.objectField("attestationFingerprint");
            try json.write(value);
        }
        try json.objectField("callerReceiptReady");
        try json.write(true);
    } else {
        try json.objectField("callerReceiptReady");
        try json.write(false);
    }
    try json.objectField("step");
    try json.write(step.toString());
    try json.objectField("title");
    try json.write(stepTitle(step));
    try json.objectField("availableActions");
    try writeAvailableActions(&json, step);
    if (step == .witness) {
        try json.objectField("witness");
        try writeWitness(&json, config.handler_path);
    }
    if (step == .deployed) {
        try json.objectField("receipt");
        try writeReceipt(&json, config.handler_path);
    }
    try json.objectField("proofState");
    if (parsed_proof) |*parsed| {
        try json.write(parsed.value);
    } else {
        try json.write(null);
    }
    try json.objectField("proofPassport");
    try writeProofPassport(&json, step, passport, proof_certificate);
    try json.endObject();
    return try allocator.dupe(u8, aw.writer.buffered());
}

pub fn exportPassport(
    allocator: std.mem.Allocator,
    config: Config,
    options: PassportExport,
) !void {
    if (options.out_dir.len == 0) return error.InvalidOutputPath;

    var io_backend = shared.threadedIo(allocator);
    defer io_backend.deinit();
    const io = io_backend.io();
    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, options.out_dir);

    var session = try pi_app.demo_passport.ensureSession(allocator, config.workspace_root);
    defer session.deinit(allocator);

    const events_export = try std.fs.path.join(allocator, &.{ options.out_dir, "events.jsonl" });
    defer allocator.free(events_export);
    try copyOptionalFile(allocator, session.events_path, events_export, "");

    const meta_export = try std.fs.path.join(allocator, &.{ options.out_dir, "session-meta.json" });
    defer allocator.free(meta_export);
    try copyOptionalFile(allocator, session.meta_path, meta_export, "{}\n");

    const verify_text = try renderVerifyText(allocator, config, options.step);
    defer allocator.free(verify_text);
    try writeRelativeFile(allocator, options.out_dir, "verify.txt", verify_text);

    const passport_json = try renderPassportJson(allocator, config, session, options.step);
    defer allocator.free(passport_json);
    try writeRelativeFile(allocator, options.out_dir, "passport.json", passport_json);

    const index_html = try renderPassportHtml(allocator, config, session, options.step);
    defer allocator.free(index_html);
    try writeRelativeFile(allocator, options.out_dir, "index.html", index_html);
}

fn tempWorkspacePath(allocator: std.mem.Allocator) ![]u8 {
    var ts: std.posix.timespec = undefined;
    _ = std.c.clock_gettime(@enumFromInt(@intFromEnum(std.posix.CLOCK.REALTIME)), &ts);
    return try std.fmt.allocPrint(
        allocator,
        "/tmp/zigttp-proof-theater-{d}-{d}-{d}",
        .{ std.c.getpid(), @as(u64, @intCast(ts.sec)), @as(u64, @intCast(ts.nsec)) },
    );
}

fn pathExists(path: []const u8) bool {
    var io_backend = shared.threadedIo(std.heap.smp_allocator);
    defer io_backend.deinit();
    std.Io.Dir.access(std.Io.Dir.cwd(), io_backend.io(), path, .{}) catch return false;
    return true;
}

fn createChildDir(allocator: std.mem.Allocator, io: std.Io, root: []const u8, child: []const u8) !void {
    const path = try std.fs.path.join(allocator, &.{ root, child });
    defer allocator.free(path);
    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, path);
}

fn writeRelativeFile(allocator: std.mem.Allocator, root: []const u8, rel: []const u8, data: []const u8) !void {
    const path = try std.fs.path.join(allocator, &.{ root, rel });
    defer allocator.free(path);
    try zigts.file_io.writeFile(allocator, path, data);
}

fn copyOptionalFile(allocator: std.mem.Allocator, source: []const u8, dest: []const u8, fallback: []const u8) !void {
    const bytes = zigts.file_io.readFile(allocator, source, 16 * 1024 * 1024) catch |err| switch (err) {
        error.FileNotFound => return zigts.file_io.writeFile(allocator, dest, fallback),
        else => return err,
    };
    defer allocator.free(bytes);
    try zigts.file_io.writeFile(allocator, dest, bytes);
}

fn deleteLedger(allocator: std.mem.Allocator) void {
    var io_backend = shared.threadedIo(allocator);
    defer io_backend.deinit();
    std.Io.Dir.cwd().deleteFile(io_backend.io(), proof_ledger.ledgerPath()) catch {};
    std.Io.Dir.cwd().deleteFile(io_backend.io(), deploy_marker_path) catch {};
}

fn deployMarkerExists(allocator: std.mem.Allocator) bool {
    var io_backend = shared.threadedIo(allocator);
    defer io_backend.deinit();
    std.Io.Dir.access(std.Io.Dir.cwd(), io_backend.io(), deploy_marker_path, .{}) catch return false;
    return true;
}

fn runLocalDeploy(allocator: std.mem.Allocator) !void {
    const self_path = try self_extract.getSelfExePath(allocator);
    defer allocator.free(self_path);

    var io_backend = shared.threadedIo(allocator);
    defer io_backend.deinit();
    const io = io_backend.io();

    var child = try std.process.spawn(io, .{
        .argv = &.{ self_path, "deploy", "--no-attest" },
        .stdin = .ignore,
        .stdout = .ignore,
        .stderr = .inherit,
    });
    _ = child.wait(io) catch return error.DemoDeployFailed;

    const cwd = try std.process.currentPathAlloc(io, allocator);
    defer allocator.free(cwd);
    const artifact = try std.fs.path.join(allocator, &.{ ".zigttp", "deploy", std.fs.path.basename(cwd) });
    defer allocator.free(artifact);
    std.Io.Dir.access(std.Io.Dir.cwd(), io, artifact, .{}) catch return error.DemoDeployFailed;

    try zigts.file_io.writeFile(allocator, deploy_marker_path, "ok\n");
}

fn stepTitle(step: Step) []const u8 {
    return switch (step) {
        .baseline => "green baseline: declared specs pass",
        .witness => "unsafe edit: secret flow witness captured",
        .repaired => "repair applied: proof is green again",
        .deployed => "local deploy receipt written",
    };
}

fn stepReached(current: Step, target: Step) bool {
    return switch (target) {
        .baseline => true,
        .witness => current == .witness or current == .repaired or current == .deployed,
        .repaired => current == .repaired or current == .deployed,
        .deployed => current == .deployed,
    };
}

fn writeAvailableActions(json: *std.json.Stringify, step: Step) !void {
    try json.beginArray();
    switch (step) {
        .baseline => try json.write(Action.introduce_bug.toString()),
        .witness => try json.write(Action.repair_bug.toString()),
        .repaired => try json.write(Action.deploy.toString()),
        .deployed => try json.write(Action.reset.toString()),
    }
    if (step != .baseline and step != .deployed) try json.write(Action.reset.toString());
    try json.endArray();
}

fn writeWitness(json: *std.json.Stringify, handler_path: []const u8) !void {
    try json.beginObject();
    try json.objectField("property");
    try json.write("no_secret_leakage");
    try json.objectField("request");
    try json.write("GET /status");
    try json.objectField("source");
    try json.write(handler_path);
    try json.objectField("span");
    try json.write("env(\"SECRET_KEY\") reaches Response.json");
    try json.objectField("failingPath");
    try json.write("handler -> status route -> env SECRET_KEY -> response body");
    try json.endObject();
}

fn writeReceipt(json: *std.json.Stringify, handler_path: []const u8) !void {
    try json.beginObject();
    try json.objectField("ledger");
    try json.write(proof_ledger.ledgerPath());
    try json.objectField("handler");
    try json.write(handler_path);
    try json.objectField("service");
    try json.write("proof-inbox");
    try json.endObject();
}

fn writeProofPassport(
    json: *std.json.Stringify,
    step: Step,
    passport: pi_app.demo_passport.SessionInfo,
    proof_certificate: ?[]const u8,
) !void {
    try json.beginObject();
    try json.objectField("step");
    try json.write(step.toString());
    try json.objectField("sessionId");
    try json.write(passport.session_id);
    try json.objectField("expertCommand");
    try json.write(passport.expert_command);
    try json.objectField("eventsPath");
    try json.write(passport.events_path);
    try json.objectField("ledgerReady");
    try json.write(step == .repaired or step == .deployed);
    try json.objectField("certificate");
    if (proof_certificate) |cert| {
        try json.write(cert);
    } else {
        try json.write(null);
    }
    try json.objectField("handoff");
    try json.write("Open the seeded session with zigttp expert or inspect it with zigttp ledger.");
    try json.endObject();
}

fn proofCertificate(value: std.json.Value) ?[]const u8 {
    if (value != .object) return null;
    const cert = value.object.get("proofCertificate") orelse return null;
    return if (cert == .string) cert.string else null;
}

fn callerReceiptObject(value: std.json.Value) ?std.json.ObjectMap {
    if (value != .object) return null;
    const receipt = value.object.get("callerReceipt") orelse return null;
    return if (receipt == .object) receipt.object else null;
}

fn objectString(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const value = obj.get(key) orelse return null;
    return if (value == .string) value.string else null;
}

fn deployArtifactPath(allocator: std.mem.Allocator, workspace_root: []const u8) ![]u8 {
    return try std.fs.path.join(allocator, &.{ ".zigttp", "deploy", std.fs.path.basename(workspace_root) });
}

const LedgerSummary = struct {
    contract_sha: ?[]const u8 = null,
    kind: ?proof_ledger.EventKind = null,
};

fn latestLedgerSummary(allocator: std.mem.Allocator) !LedgerSummary {
    const events = try proof_ledger.readEvents(allocator);
    defer proof_ledger.freeEvents(allocator, events);
    if (events.len == 0) return .{};

    const latest = events[events.len - 1];
    return .{
        .contract_sha = try allocator.dupe(u8, latest.facts.contract_sha),
        .kind = latest.kind,
    };
}

fn renderPassportJson(
    allocator: std.mem.Allocator,
    config: Config,
    session: pi_app.demo_passport.SessionInfo,
    step: Step,
) ![]u8 {
    const latest = try latestLedgerSummary(allocator);
    defer if (latest.contract_sha) |sha| allocator.free(sha);

    const policy_hash = zigts.rule_registry.policyHash();
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };

    try json.beginObject();
    try json.objectField("schemaVersion");
    try json.write(@as(u8, 1));
    try json.objectField("kind");
    try json.write("zigttp-proof-passport");
    try json.objectField("step");
    try json.write(step.toString());
    try json.objectField("title");
    try json.write(stepTitle(step));
    try json.objectField("generatedAtUnixMs");
    try json.write(proof_ledger.defaultNowMs());
    try json.objectField("workspace");
    try json.write(config.workspace_root);
    try json.objectField("handlerPath");
    try json.write(config.handler_path);
    try json.objectField("sessionId");
    try json.write(session.session_id);
    try json.objectField("sessionDir");
    try json.write(session.session_dir);
    try json.objectField("policyHash");
    try json.write(policy_hash[0..]);
    try json.objectField("contractHash");
    if (latest.contract_sha) |sha| try json.write(sha) else try json.write(null);
    try json.objectField("latestLedgerKind");
    if (latest.kind) |kind| try json.write(kind.toString()) else try json.write(null);
    try json.objectField("proofs");
    try json.beginArray();
    try json.write("injection_safe");
    try json.write("no_secret_leakage");
    try json.endArray();
    try json.objectField("files");
    try json.beginObject();
    try json.objectField("events");
    try json.write("events.jsonl");
    try json.objectField("meta");
    try json.write("session-meta.json");
    try json.objectField("verify");
    try json.write("verify.txt");
    try json.objectField("html");
    try json.write("index.html");
    try json.endObject();
    try json.objectField("steps");
    try json.beginArray();
    try writeStepJson(&json, .baseline, stepReached(step, .baseline));
    try writeStepJson(&json, .witness, stepReached(step, .witness));
    try writeStepJson(&json, .repaired, stepReached(step, .repaired));
    try writeStepJson(&json, .deployed, stepReached(step, .deployed));
    try json.endArray();
    try json.endObject();
    try aw.writer.writeByte('\n');
    return try allocator.dupe(u8, aw.writer.buffered());
}

fn writeStepJson(json: *std.json.Stringify, step: Step, complete: bool) !void {
    try json.beginObject();
    try json.objectField("step");
    try json.write(step.toString());
    try json.objectField("title");
    try json.write(stepTitle(step));
    try json.objectField("complete");
    try json.write(complete);
    try json.endObject();
}

fn renderVerifyText(allocator: std.mem.Allocator, config: Config, step: Step) ![]u8 {
    const artifact = try std.fs.path.join(allocator, &.{ config.workspace_root, ".zigttp", "deploy", std.fs.path.basename(config.workspace_root) });
    defer allocator.free(artifact);
    return try std.fmt.allocPrint(allocator,
        \\Proof Passport verification
        \\
        \\Workspace:
        \\  cd {s}
        \\
        \\Checks:
        \\  zigttp check
        \\  zigttp test
        \\  zigttp deploy
        \\  zigttp proofs show HEAD
        \\
        \\Run deployed artifact:
        \\  {s} -p 3001
        \\  curl http://127.0.0.1:3001/
        \\
        \\Exported step: {s}
        \\
    , .{ config.workspace_root, artifact, step.toString() });
}

fn renderPassportHtml(
    allocator: std.mem.Allocator,
    config: Config,
    session: pi_app.demo_passport.SessionInfo,
    step: Step,
) ![]u8 {
    const latest = try latestLedgerSummary(allocator);
    defer if (latest.contract_sha) |sha| allocator.free(sha);

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    const w = &aw.writer;
    try w.writeAll(
        \\<!doctype html><html lang="en"><meta charset="utf-8">
        \\<title>zigttp Proof Passport</title>
        \\<style>body{font:16px/1.5 system-ui,-apple-system,BlinkMacSystemFont,sans-serif;margin:40px;max-width:880px;color:#18202a}code,pre{font-family:ui-monospace,SFMono-Regular,Menlo,monospace}.ok{color:#116329}.pending{color:#7a4b00}.box{border:1px solid #d9dee7;border-radius:8px;padding:16px;margin:16px 0}li{margin:8px 0}</style>
        \\<body><h1>zigttp Proof Passport</h1>
    );
    try w.print("<p class=\"box\">Step: <strong>{s}</strong> - ", .{step.toString()});
    try writeHtmlEscaped(w, stepTitle(step));
    try w.writeAll("</p><dl>");
    try w.writeAll("<dt>Workspace</dt><dd><code>");
    try writeHtmlEscaped(w, config.workspace_root);
    try w.writeAll("</code></dd><dt>Handler</dt><dd><code>");
    try writeHtmlEscaped(w, config.handler_path);
    try w.writeAll("</code></dd><dt>Session</dt><dd><code>");
    try writeHtmlEscaped(w, session.session_id);
    try w.writeAll("</code></dd><dt>Contract</dt><dd><code>");
    if (latest.contract_sha) |sha| try writeHtmlEscaped(w, sha) else try w.writeAll("not recorded");
    try w.writeAll(
        \\</code></dd></dl>
        \\<h2>Flow</h2><ol>
    );
    try writeHtmlStep(w, .baseline, stepReached(step, .baseline));
    try writeHtmlStep(w, .witness, stepReached(step, .witness));
    try writeHtmlStep(w, .repaired, stepReached(step, .repaired));
    try writeHtmlStep(w, .deployed, stepReached(step, .deployed));
    try w.writeAll(
        \\</ol>
        \\<h2>Files</h2><ul>
        \\<li><a href="passport.json">passport.json</a></li>
        \\<li><a href="events.jsonl">events.jsonl</a></li>
        \\<li><a href="session-meta.json">session-meta.json</a></li>
        \\<li><a href="verify.txt">verify.txt</a></li>
        \\</ul></body></html>
    );
    return try allocator.dupe(u8, w.buffered());
}

fn writeHtmlStep(writer: anytype, step: Step, complete: bool) !void {
    try writer.print("<li class=\"{s}\"><strong>{s}</strong>: ", .{ if (complete) "ok" else "pending", step.toString() });
    try writeHtmlEscaped(writer, stepTitle(step));
    try writer.writeAll(if (complete) " (complete)</li>" else " (not reached)</li>");
}

fn writeHtmlEscaped(writer: anytype, value: []const u8) !void {
    for (value) |c| {
        switch (c) {
            '&' => try writer.writeAll("&amp;"),
            '<' => try writer.writeAll("&lt;"),
            '>' => try writer.writeAll("&gt;"),
            '"' => try writer.writeAll("&quot;"),
            '\'' => try writer.writeAll("&#39;"),
            else => try writer.writeByte(c),
        }
    }
}

pub const baseline_source =
    \\import type { Spec } from "zigttp:types";
    \\import { routerMatch } from "zigttp:router";
    \\import { schemaCompile, validateJson } from "zigttp:validate";
    \\import { cacheGet, cacheSet, cacheStats } from "zigttp:cache";
    \\import { env } from "zigttp:env";
    \\
    \\type Guardrails = Spec<"injection_safe" | "no_secret_leakage">;
    \\
    \\const routes = {
    \\    "GET /": "home",
    \\    "GET /status": "status",
    \\    "POST /message": "message"
    \\};
    \\
    \\schemaCompile("message", JSON.stringify({
    \\    type: "object",
    \\    required: ["text"],
    \\    properties: { text: { type: "string", minLength: 1, maxLength: 120 } }
    \\}));
    \\
    \\function Page(): JSX.Element {
    \\    return (
    \\        <html>
    \\            <body>
    \\                <h1>Proof Inbox</h1>
    \\                <p>ready</p>
    \\            </body>
    \\        </html>
    \\    );
    \\}
    \\
    \\function handler(req: Request): Response & Guardrails {
    \\    const route = routerMatch(routes, { method: req.method, path: req.path });
    \\    if (route === undefined) return Response.text("Not Found", { status: 404 });
    \\
    \\    const configured = env("PUBLIC_INBOX_TITLE") !== undefined;
    \\
    \\    if (route.route === "message") {
    \\        const result = validateJson("message", req.body ?? "{}");
    \\        if (!result.ok) return Response.json({ errors: result.errors }, { status: 400 });
    \\        cacheSet("proof-inbox", "latest", result.value.text, 60);
    \\        return Response.json({ ok: true });
    \\    }
    \\
    \\    if (route.route === "status") {
    \\        return Response.json({ ok: true, configured: configured, cache: cacheStats("proof-inbox") });
    \\    }
    \\
    \\    cacheGet("proof-inbox", "latest");
    \\    return Response.html(renderToString(<Page></Page>));
    \\}
;

pub const bug_source =
    \\import type { Spec } from "zigttp:types";
    \\import { routerMatch } from "zigttp:router";
    \\import { schemaCompile, validateJson } from "zigttp:validate";
    \\import { cacheGet, cacheSet, cacheStats } from "zigttp:cache";
    \\import { env } from "zigttp:env";
    \\
    \\type Guardrails = Spec<"injection_safe" | "no_secret_leakage">;
    \\
    \\const routes = {
    \\    "GET /": "home",
    \\    "GET /status": "status",
    \\    "POST /message": "message"
    \\};
    \\
    \\schemaCompile("message", JSON.stringify({
    \\    type: "object",
    \\    required: ["text"],
    \\    properties: { text: { type: "string", minLength: 1, maxLength: 120 } }
    \\}));
    \\
    \\function Page(): JSX.Element {
    \\    return (
    \\        <html>
    \\            <body>
    \\                <h1>Proof Inbox</h1>
    \\                <p>ready</p>
    \\            </body>
    \\        </html>
    \\    );
    \\}
    \\
    \\function handler(req: Request): Response & Guardrails {
    \\    const route = routerMatch(routes, { method: req.method, path: req.path });
    \\    if (route === undefined) return Response.text("Not Found", { status: 404 });
    \\
    \\    const configured = env("PUBLIC_INBOX_TITLE") !== undefined;
    \\
    \\    if (route.route === "message") {
    \\        const result = validateJson("message", req.body ?? "{}");
    \\        if (!result.ok) return Response.json({ errors: result.errors }, { status: 400 });
    \\        cacheSet("proof-inbox", "latest", result.value.text, 60);
    \\        return Response.json({ ok: true });
    \\    }
    \\
    \\    if (route.route === "status") {
    \\        const secret = env("SECRET_KEY");
    \\        return Response.json({ ok: true, configured: configured, secret: secret, cache: cacheStats("proof-inbox") });
    \\    }
    \\
    \\    cacheGet("proof-inbox", "latest");
    \\    return Response.html(renderToString(<Page></Page>));
    \\}
;

pub const repaired_source =
    \\// repair marker: the status route now proves no_secret_leakage again.
    \\import type { Spec } from "zigttp:types";
    \\import { routerMatch } from "zigttp:router";
    \\import { schemaCompile, validateJson } from "zigttp:validate";
    \\import { cacheGet, cacheSet, cacheStats } from "zigttp:cache";
    \\import { env } from "zigttp:env";
    \\
    \\type Guardrails = Spec<"injection_safe" | "no_secret_leakage">;
    \\
    \\const routes = {
    \\    "GET /": "home",
    \\    "GET /status": "status",
    \\    "POST /message": "message"
    \\};
    \\
    \\schemaCompile("message", JSON.stringify({
    \\    type: "object",
    \\    required: ["text"],
    \\    properties: { text: { type: "string", minLength: 1, maxLength: 120 } }
    \\}));
    \\
    \\function Page(): JSX.Element {
    \\    return (
    \\        <html>
    \\            <body>
    \\                <h1>Proof Inbox</h1>
    \\                <p>ready</p>
    \\            </body>
    \\        </html>
    \\    );
    \\}
    \\
    \\function handler(req: Request): Response & Guardrails {
    \\    const route = routerMatch(routes, { method: req.method, path: req.path });
    \\    if (route === undefined) return Response.text("Not Found", { status: 404 });
    \\
    \\    const configured = env("PUBLIC_INBOX_TITLE") !== undefined;
    \\
    \\    if (route.route === "message") {
    \\        const result = validateJson("message", req.body ?? "{}");
    \\        if (!result.ok) return Response.json({ errors: result.errors }, { status: 400 });
    \\        cacheSet("proof-inbox", "latest", result.value.text, 60);
    \\        return Response.json({ ok: true });
    \\    }
    \\
    \\    if (route.route === "status") {
    \\        return Response.json({ ok: true, configured: configured, cache: cacheStats("proof-inbox") });
    \\    }
    \\
    \\    cacheGet("proof-inbox", "latest");
    \\    return Response.html(renderToString(<Page></Page>));
    \\}
;

const demo_tests =
    \\{"type":"test","name":"status is green"}
    \\{"type":"request","method":"GET","url":"/status","headers":{},"body":null}
    \\{"type":"expect","status":200,"bodyContains":"ok"}
;

const demo_gitignore =
    \\.zig-cache/
    \\zig-cache/
    \\zig-out/
    \\.zigttp/deploy/
;

const demo_readme =
    \\# Proof Inbox
    \\
    \\Generated by `zigttp demo`.
    \\
    \\The Studio demo controls apply deterministic edits to `src/handler.tsx`.
    \\No cloud credentials, API keys, or network access are required.
;

test "demo action parser accepts only known enum actions" {
    const allocator = std.testing.allocator;
    try std.testing.expectEqual(Action.introduce_bug, try parseActionBody(allocator, "{\"action\":\"introduce_bug\"}"));
    try std.testing.expectEqual(Action.repair_bug, try parseActionBody(allocator, "{\"action\":\"repair_bug\"}"));
    try std.testing.expectEqual(Action.deploy, try parseActionBody(allocator, "{\"action\":\"deploy\"}"));
    try std.testing.expectEqual(Action.reset, try parseActionBody(allocator, "{\"action\":\"reset\"}"));
    try std.testing.expectError(error.UnknownDemoAction, parseActionBody(allocator, "{\"action\":\"rm -rf\"}"));
    try std.testing.expectError(error.MissingDemoAction, parseActionBody(allocator, "{\"patch\":\"text\"}"));
}

test "demo workspace creation and overwrite refusal" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try proof_ledger.chdirTmpForTest(&tmp);
    defer allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var ws = try createWorkspace(allocator, "proof-demo", 4567);
    defer ws.deinit(allocator);

    var io_backend = shared.threadedIo(allocator);
    defer io_backend.deinit();
    const io = io_backend.io();
    try std.Io.Dir.access(std.Io.Dir.cwd(), io, "proof-demo/zigttp.json", .{});
    try std.Io.Dir.access(std.Io.Dir.cwd(), io, "proof-demo/src/handler.tsx", .{});
    try std.testing.expectError(error.OutputExists, createWorkspace(allocator, "proof-demo", 4567));
}

test "demo output directory rejects unsafe names" {
    const allocator = std.testing.allocator;
    try std.testing.expectError(error.InvalidOutputPath, createWorkspace(allocator, ".hidden", 4567));
    try std.testing.expectError(error.InvalidOutputPath, createWorkspace(allocator, "bad.name", 4567));
    try std.testing.expectError(error.InvalidOutputPath, createWorkspace(allocator, "bad name", 4567));
}

test "demo temp workspace cleanup removes owned directory" {
    const allocator = std.testing.allocator;
    var ws = try createWorkspace(allocator, null, 3456);
    defer ws.deinit(allocator);
    const root = try allocator.dupe(u8, ws.root);
    defer allocator.free(root);

    var io_backend = shared.threadedIo(allocator);
    defer io_backend.deinit();
    const io = io_backend.io();
    try std.Io.Dir.access(std.Io.Dir.cwd(), io, root, .{});
    ws.cleanup(allocator);
    try std.testing.expectError(error.FileNotFound, std.Io.Dir.access(std.Io.Dir.cwd(), io, root, .{}));
}

test "demo passport export writes offline files" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try proof_ledger.chdirTmpForTest(&tmp);
    defer allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const sessions_dir = try std.fs.path.resolve(allocator, &.{"sessions"});
    defer allocator.free(sessions_dir);
    const sessions_z = try allocator.dupeZ(u8, sessions_dir);
    defer allocator.free(sessions_z);
    _ = setenv("ZIGTTP_SESSIONS_DIR", sessions_z.ptr, 1);
    defer _ = unsetenv("ZIGTTP_SESSIONS_DIR");

    var ws = try createWorkspace(allocator, "proof-demo", 4567);
    defer ws.deinit(allocator);

    var session = try pi_app.demo_passport.resetToBaseline(allocator, ws.root);
    defer session.deinit(allocator);

    const handler_path = try std.fs.path.join(allocator, &.{ ws.root, "src", "handler.tsx" });
    defer allocator.free(handler_path);
    const passport_dir = try std.fs.path.join(allocator, &.{ ws.root, "passport" });
    defer allocator.free(passport_dir);

    try exportPassport(allocator, .{
        .workspace_root = ws.root,
        .handler_path = handler_path,
    }, .{
        .out_dir = passport_dir,
        .step = .baseline,
    });

    const passport_json_path = try std.fs.path.join(allocator, &.{ passport_dir, "passport.json" });
    defer allocator.free(passport_json_path);
    const passport_json = try zigts.file_io.readFile(allocator, passport_json_path, 64 * 1024);
    defer allocator.free(passport_json);
    try std.testing.expect(std.mem.indexOf(u8, passport_json, "\"kind\":\"zigttp-proof-passport\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, passport_json, "\"step\":\"baseline\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, passport_json, "\"contractHash\":null") != null);

    const index_path = try std.fs.path.join(allocator, &.{ passport_dir, "index.html" });
    defer allocator.free(index_path);
    const index_html = try zigts.file_io.readFile(allocator, index_path, 64 * 1024);
    defer allocator.free(index_html);
    try std.testing.expect(std.mem.indexOf(u8, index_html, "zigttp Proof Passport") != null);

    const verify_path = try std.fs.path.join(allocator, &.{ passport_dir, "verify.txt" });
    defer allocator.free(verify_path);
    const verify_text = try zigts.file_io.readFile(allocator, verify_path, 64 * 1024);
    defer allocator.free(verify_text);
    try std.testing.expect(std.mem.indexOf(u8, verify_text, "zigttp proofs show HEAD") != null);
}
