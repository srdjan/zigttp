//! `zigttp doctor --release` implementation extracted from dev_cli.zig.
//!
//! Self-contained release-passport pipeline: parses CLI options, collects
//! a set of release-readiness checks, renders the result as text or JSON,
//! and returns a verdict (ready / ready_with_known_issues / blocked).
//! Each check reads existing files only — no benchmark or test suite
//! invocation — so the command is safe to run repeatedly.
//!
//! The dev_cli `doctorCommand` dispatches into `releaseDoctorCommand`
//! here when `--release` is the first argument; it passes the basic
//! `printDoctorHelp` function in so this module stays free of dev_cli
//! dependencies.

const std = @import("std");
const zigts = @import("zigts");

const release_verify_commands = [_][]const u8{
    "zig fmt --check build.zig packages/",
    "zig build test",
    "zig build test-zruntime",
    "zig build test-docs-drift test-doc-links",
    "zig build -Doptimize=ReleaseFast",
    "zig build smoke-v1",
    "zig build test-panic-isolation",
    "zig build smoke-getting-started",
    "zig build smoke-demo",
    "zig build smoke-studio",
    "bash scripts/test-examples.sh",
    "bash scripts/test-install-archive-safety.sh",
    "bash scripts/check-semantics-spec.sh",
    "zig build bench-check",
    "./zig-out/bin/zigttp doctor --release --json",
};

const build_gate_markers = [_][]const u8{
    "smoke-v1",
    "test-panic-isolation",
    "smoke-getting-started",
    "smoke-demo",
    "smoke-studio",
    "test-module-governance",
    "test-capability-audit",
    "test-docs-drift",
};

const ci_gate_markers = [_][]const u8{
    "zig fmt --check build.zig packages/",
    "zig build test",
    "zig build test-zruntime",
    "zig build test-docs-drift test-doc-links",
    "zig build -Doptimize=ReleaseFast",
    "zig build smoke-v1",
    "zig build test-panic-isolation",
    "bash scripts/test-examples.sh",
    "bash scripts/test-install-archive-safety.sh",
    "bash scripts/check-semantics-spec.sh",
};

const verify_script_markers = [_][]const u8{
    "zig build test",
    "zig build test-zruntime",
    "zig build test-docs-drift test-doc-links",
    "zig build -Doptimize=ReleaseFast",
    "zig build smoke-v1",
    "zig build test-panic-isolation",
    "bash scripts/test-examples.sh",
    "bash scripts/test-install-archive-safety.sh",
    "bash scripts/check-semantics-spec.sh",
    "zigts meta --json",
};

const release_only_gate_markers = [_][]const u8{
    "zig build smoke-getting-started",
    "zig build smoke-demo",
    "zig build smoke-studio",
    "./zig-out/bin/zigttp doctor --release --json",
    "contents: write",
};

pub const ReleaseDoctorOptions = struct {
    json: bool = false,
    out_path: ?[]const u8 = null,
};

pub const ReleaseVerdict = enum {
    ready,
    ready_with_known_issues,
    blocked,

    pub fn toString(self: ReleaseVerdict) []const u8 {
        return switch (self) {
            .ready => "ready",
            .ready_with_known_issues => "ready_with_known_issues",
            .blocked => "blocked",
        };
    }
};

pub const ReleaseCheckStatus = enum {
    ok,
    warn,
    fail,

    pub fn toString(self: ReleaseCheckStatus) []const u8 {
        return switch (self) {
            .ok => "ok",
            .warn => "warn",
            .fail => "fail",
        };
    }
};

pub const ReleaseCheck = struct {
    id: []u8,
    label: []u8,
    status: ReleaseCheckStatus,
    detail: []u8,
    command: ?[]u8 = null,

    pub fn deinit(self: *ReleaseCheck, allocator: std.mem.Allocator) void {
        allocator.free(self.id);
        allocator.free(self.label);
        allocator.free(self.detail);
        if (self.command) |cmd| allocator.free(cmd);
    }

    pub fn writeJson(self: *const ReleaseCheck, json: *std.json.Stringify) !void {
        try json.beginObject();
        try json.objectField("id");
        try json.write(self.id);
        try json.objectField("label");
        try json.write(self.label);
        try json.objectField("status");
        try json.write(self.status.toString());
        try json.objectField("detail");
        try json.write(self.detail);
        if (self.command) |cmd| {
            try json.objectField("command");
            try json.write(cmd);
        }
        try json.endObject();
    }
};

pub const ReleasePassport = struct {
    version: []u8,
    checks: std.ArrayList(ReleaseCheck) = .empty,

    pub fn init(allocator: std.mem.Allocator, version: []const u8) !ReleasePassport {
        return .{ .version = try allocator.dupe(u8, version) };
    }

    pub fn deinit(self: *ReleasePassport, allocator: std.mem.Allocator) void {
        allocator.free(self.version);
        for (self.checks.items) |*check| check.deinit(allocator);
        self.checks.deinit(allocator);
    }

    pub fn add(
        self: *ReleasePassport,
        allocator: std.mem.Allocator,
        id: []const u8,
        label: []const u8,
        status: ReleaseCheckStatus,
        detail: []const u8,
        command: ?[]const u8,
    ) !void {
        const owned_id = try allocator.dupe(u8, id);
        errdefer allocator.free(owned_id);
        const owned_label = try allocator.dupe(u8, label);
        errdefer allocator.free(owned_label);
        const owned_detail = try allocator.dupe(u8, detail);
        errdefer allocator.free(owned_detail);
        const owned_command = if (command) |cmd| try allocator.dupe(u8, cmd) else null;
        errdefer if (owned_command) |cmd| allocator.free(cmd);
        try self.checks.append(allocator, .{
            .id = owned_id,
            .label = owned_label,
            .status = status,
            .detail = owned_detail,
            .command = owned_command,
        });
    }

    pub fn verdict(self: *const ReleasePassport) ReleaseVerdict {
        var saw_warn = false;
        for (self.checks.items) |check| {
            switch (check.status) {
                .fail => return .blocked,
                .warn => saw_warn = true,
                .ok => {},
            }
        }
        return if (saw_warn) .ready_with_known_issues else .ready;
    }

    pub fn writeJson(self: *const ReleasePassport, json: *std.json.Stringify) !void {
        try json.beginObject();
        try json.objectField("release");
        try json.write(self.version);
        try json.objectField("verdict");
        try json.write(self.verdict().toString());
        try json.objectField("checks");
        try json.beginArray();
        for (self.checks.items) |*check| {
            try check.writeJson(json);
        }
        try json.endArray();
        try json.objectField("verifyCommands");
        try json.beginArray();
        for (release_verify_commands) |cmd| {
            try json.write(cmd);
        }
        try json.endArray();
        try json.endObject();
    }
};

pub fn releaseDoctorCommand(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    print_help: *const fn () void,
) !void {
    const opts = parseReleaseDoctorOptions(argv) catch |err| {
        std.debug.print("Invalid release doctor arguments.\n\n", .{});
        print_help();
        return err;
    };
    var passport = try collectReleasePassport(allocator);
    defer passport.deinit(allocator);

    const json_bytes = try renderReleasePassportJson(allocator, &passport);
    defer allocator.free(json_bytes);
    if (opts.out_path) |path| {
        try zigts.file_io.writeFile(allocator, path, json_bytes);
    }

    const output = if (opts.json)
        try allocator.dupe(u8, json_bytes)
    else
        try renderReleasePassportText(allocator, &passport, opts.out_path);
    defer allocator.free(output);

    _ = std.c.write(std.c.STDOUT_FILENO, output.ptr, output.len);
    if (passport.verdict() == .blocked) return error.DoctorFailed;
}

pub fn parseReleaseDoctorOptions(argv: []const []const u8) !ReleaseDoctorOptions {
    var opts: ReleaseDoctorOptions = .{};
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--json")) {
            opts.json = true;
        } else if (std.mem.eql(u8, arg, "--out")) {
            i += 1;
            if (i >= argv.len) {
                return error.InvalidArgument;
            }
            opts.out_path = argv[i];
        } else {
            return error.InvalidArgument;
        }
    }
    return opts;
}

pub fn collectReleasePassport(allocator: std.mem.Allocator) !ReleasePassport {
    const zon = readOptionalFile(allocator, "build.zig.zon", 256 * 1024);
    defer if (zon) |bytes| allocator.free(bytes);
    const version = if (zon) |bytes| extractZonVersion(bytes) orelse "unknown" else "unknown";
    var passport = try ReleasePassport.init(allocator, version);
    errdefer passport.deinit(allocator);

    try addVersionCheck(allocator, &passport, zon);
    try addReleaseEvidenceCheck(allocator, &passport);
    try addReleaseGateCheck(allocator, &passport);
    try addPublicClaimsCheck(allocator, &passport);
    try addCurrentDocsScopeCheck(allocator, &passport);
    try addReliabilityKnownIssuesCheck(allocator, &passport);
    try addProofSurfaceCheck(allocator, &passport);

    return passport;
}

fn addVersionCheck(allocator: std.mem.Allocator, passport: *ReleasePassport, zon: ?[]const u8) !void {
    const root = readOptionalFile(allocator, "packages/zigts/src/root.zig", 256 * 1024);
    defer if (root) |bytes| allocator.free(bytes);

    const version = if (zon) |bytes| extractZonVersion(bytes) else null;
    if (version == null or root == null) {
        try passport.add(allocator, "version", "Version alignment", .fail, "build.zig.zon or packages/zigts/src/root.zig is missing", "zig build test-zigts");
        return;
    }

    const root_bytes = root.?;
    const expected = try std.fmt.allocPrint(allocator, "string = \"{s}\"", .{version.?});
    defer allocator.free(expected);
    if (std.mem.indexOf(u8, root_bytes, expected) == null) {
        try passport.add(allocator, "version", "Version alignment", .fail, "build.zig.zon version does not match packages/zigts/src/root.zig", "zig build test-zigts");
        return;
    }

    try passport.add(allocator, "version", "Version alignment", .ok, "build.zig.zon and zigts version string agree", "zig build test-zigts");
}

fn addReleaseEvidenceCheck(allocator: std.mem.Allocator, passport: *ReleasePassport) !void {
    const docs_ok =
        zigts.file_io.fileExists(allocator, "README.md") and
        zigts.file_io.fileExists(allocator, "docs/README.md") and
        zigts.file_io.fileExists(allocator, "docs/user-guide.md") and
        zigts.file_io.fileExists(allocator, "docs/roadmap.md") and
        zigts.file_io.fileExists(allocator, "docs/virtual-modules/README.md");
    if (docs_ok) {
        try passport.add(allocator, "release_evidence", "Documentation evidence", .ok, "front door, user guide, roadmap, and virtual-module index exist", "bash scripts/audit-docs.sh .");
    } else {
        try passport.add(allocator, "release_evidence", "Documentation evidence", .fail, "missing maintained README, user guide, roadmap, or module index", "bash scripts/audit-docs.sh .");
    }
}

fn addReleaseGateCheck(allocator: std.mem.Allocator, passport: *ReleasePassport) !void {
    const build_zig = readOptionalFile(allocator, "build.zig", 1024 * 1024);
    defer if (build_zig) |bytes| allocator.free(bytes);
    const ci_yml = readOptionalFile(allocator, ".github/workflows/ci.yml", 512 * 1024);
    defer if (ci_yml) |bytes| allocator.free(bytes);
    const release_yml = readOptionalFile(allocator, ".github/workflows/release.yml", 512 * 1024);
    defer if (release_yml) |bytes| allocator.free(bytes);
    const verify_sh = readOptionalFile(allocator, "scripts/verify.sh", 256 * 1024);
    defer if (verify_sh) |bytes| allocator.free(bytes);

    const smoke_ok = zigts.file_io.fileExists(allocator, "scripts/smoke-v1.sh");
    const examples_ok = zigts.file_io.fileExists(allocator, "scripts/test-examples.sh");
    const installer_ok = zigts.file_io.fileExists(allocator, "scripts/test-install-archive-safety.sh");
    const semantics_ok = zigts.file_io.fileExists(allocator, "scripts/check-semantics-spec.sh");
    const workflow_ok = if (build_zig != null and ci_yml != null and release_yml != null and verify_sh != null)
        releaseGateRequirementsPresent(build_zig.?, ci_yml.?, release_yml.?, verify_sh.?)
    else
        false;

    if (smoke_ok and examples_ok and installer_ok and semantics_ok and workflow_ok) {
        try passport.add(allocator, "release_gates", "Release gates", .ok, "CI, release workflow, local verifier, installer, semantics, docs, smoke, and doctor gates are wired", "bash scripts/verify.sh && ./zig-out/bin/zigttp doctor --release --json");
    } else {
        try passport.add(allocator, "release_gates", "Release gates", .fail, "one or more release gates are missing from build wiring, scripts, or workflows", "bash scripts/verify.sh && ./zig-out/bin/zigttp doctor --release --json");
    }
}

fn addPublicClaimsCheck(allocator: std.mem.Allocator, passport: *ReleasePassport) !void {
    const readme = readOptionalFile(allocator, "README.md", 2 * 1024 * 1024);
    defer if (readme) |bytes| allocator.free(bytes);
    const perf = readOptionalFile(allocator, "docs/performance.md", 2 * 1024 * 1024);
    defer if (perf) |bytes| allocator.free(bytes);

    if (readme == null or perf == null) {
        try passport.add(allocator, "public_claims", "Public performance claims", .fail, "README or performance doc is missing", null);
        return;
    }

    const stale_readme =
        containsAny(readme.?, &.{ "1.2MB binary", "4MB memory baseline", "3ms runtime init", "71ms", "79,743" });
    const stale_perf =
        containsAny(perf.?, &.{ "71ms", "71 ms", "79,743", "0.76x Deno" });
    const has_measured_baseline =
        std.mem.indexOf(u8, readme.?, "3.5") != null and
        std.mem.indexOf(u8, readme.?, "7-15") != null and
        std.mem.indexOf(u8, readme.?, "13 MB") != null and
        std.mem.indexOf(u8, readme.?, "112k") != null and
        std.mem.indexOf(u8, perf.?, "3.5") != null and
        std.mem.indexOf(u8, perf.?, "7-15") != null and
        std.mem.indexOf(u8, perf.?, "13 MB") != null and
        std.mem.indexOf(u8, perf.?, "112k") != null;
    const has_pending_receipt_note =
        hasPendingReceiptBackedMeasurementNote(readme.?) and
        hasPendingReceiptBackedMeasurementNote(perf.?);

    if (stale_readme or stale_perf or !has_measured_baseline) {
        try passport.add(allocator, "public_claims", "Public performance claims", .fail, "public numbers are stale or missing from README/performance docs", "zig build bench-check");
    } else if (!has_pending_receipt_note) {
        try passport.add(allocator, "public_claims", "Public performance claims", .fail, "public numbers must be marked pending receipt-backed measurement until the in-repo measurement path exists", "zig build bench-check");
    } else {
        try passport.add(allocator, "public_claims", "Public performance claims", .warn, "README and performance docs carry current public numbers, explicitly marked pending receipt-backed measurement", "zig build bench-check");
    }
}

fn addCurrentDocsScopeCheck(allocator: std.mem.Allocator, passport: *ReleasePassport) !void {
    const readme = readOptionalFile(allocator, "README.md", 2 * 1024 * 1024);
    defer if (readme) |bytes| allocator.free(bytes);
    const docs_index = readOptionalFile(allocator, "docs/README.md", 512 * 1024);
    defer if (docs_index) |bytes| allocator.free(bytes);
    const roadmap = readOptionalFile(allocator, "docs/roadmap.md", 512 * 1024);
    defer if (roadmap) |bytes| allocator.free(bytes);

    if (readme == null or docs_index == null or roadmap == null) {
        try passport.add(allocator, "docs_scope", "Current docs scope", .fail, "README, docs index, or roadmap is missing", null);
        return;
    }

    const stale_markers = [_][]const u8{
        "Release Scope",
        "beta checklist",
        "docs/releases",
        "migration instructions",
        "old plans",
    };
    if (containsAny(readme.?, &stale_markers) or
        containsAny(docs_index.?, &stale_markers) or
        containsAny(roadmap.?, &stale_markers))
    {
        try passport.add(allocator, "docs_scope", "Current docs scope", .fail, "front-door docs still point at historical release material", null);
    } else {
        try passport.add(allocator, "docs_scope", "Current docs scope", .ok, "front-door docs describe the current codebase and use one roadmap/user guide", null);
    }
}

fn addReliabilityKnownIssuesCheck(allocator: std.mem.Allocator, passport: *ReleasePassport) !void {
    const reliability = readOptionalFile(allocator, "docs/reliability.md", 512 * 1024);
    defer if (reliability) |bytes| allocator.free(bytes);
    if (reliability == null) {
        try passport.add(allocator, "known_issues", "Known reliability issues", .fail, "docs/reliability.md is missing", null);
        return;
    }
    if (std.mem.indexOf(u8, reliability.?, "closes the connection without") != null and
        std.mem.indexOf(u8, reliability.?, "413") != null)
    {
        try passport.add(allocator, "known_issues", "Known reliability issues", .warn, "oversized request bodies are documented as a known 413 gap", null);
    } else {
        try passport.add(allocator, "known_issues", "Known reliability issues", .ok, "no documented release-blocking reliability gap found", null);
    }
}

fn addProofSurfaceCheck(allocator: std.mem.Allocator, passport: *ReleasePassport) !void {
    const dev_cli = readOptionalFile(allocator, "packages/runtime/src/dev_cli.zig", 2 * 1024 * 1024);
    defer if (dev_cli) |bytes| allocator.free(bytes);
    const proofs_cli_source = readOptionalFile(allocator, "packages/runtime/src/proofs_cli.zig", 2 * 1024 * 1024);
    defer if (proofs_cli_source) |bytes| allocator.free(bytes);

    if (dev_cli == null or proofs_cli_source == null) {
        try passport.add(allocator, "proof_surface", "Proof surface", .fail, "developer CLI or proof ledger CLI source is missing", "zig build test-cli");
        return;
    }

    const dev_ok =
        std.mem.indexOf(u8, dev_cli.?, "zigttp verify <url>") != null and
        std.mem.indexOf(u8, dev_cli.?, "proofs") != null and
        std.mem.indexOf(u8, dev_cli.?, "--no-attest") != null;
    const proofs_ok =
        std.mem.indexOf(u8, proofs_cli_source.?, "badge") != null and
        std.mem.indexOf(u8, proofs_cli_source.?, "bundle") != null and
        std.mem.indexOf(u8, proofs_cli_source.?, "verify") != null;
    if (dev_ok and proofs_ok) {
        try passport.add(allocator, "proof_surface", "Proof surface", .ok, "proof receipts, ledger, badge, bundle, and verify surfaces are present", "zig build test-cli");
    } else {
        try passport.add(allocator, "proof_surface", "Proof surface", .fail, "proof receipt, ledger, badge, bundle, or verify surface is missing", "zig build test-cli");
    }
}

fn renderReleasePassportText(allocator: std.mem.Allocator, passport: *const ReleasePassport, out_path: ?[]const u8) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    const verdict = passport.verdict();
    try aw.writer.print(
        \\zigttp release doctor
        \\Release:  {s}
        \\Verdict:  {s}
        \\
    , .{ passport.version, verdict.toString() });
    for (passport.checks.items) |check| {
        try aw.writer.print("[{s}] {s}: {s}\n", .{ check.status.toString(), check.label, check.detail });
        if (check.command) |cmd| try aw.writer.print("      verify: {s}\n", .{cmd});
    }
    try aw.writer.writeAll("\nRelease verification commands:\n");
    for (release_verify_commands) |cmd| {
        try aw.writer.print("  {s}\n", .{cmd});
    }
    try aw.writer.writeByte('\n');
    if (out_path) |path| {
        try aw.writer.print("Wrote JSON passport: {s}\n", .{path});
    }
    if (verdict == .blocked) {
        try aw.writer.writeAll("Next: resolve the failed release rows, then run `zigttp doctor --release` again.\n");
    }
    return try allocator.dupe(u8, aw.writer.buffered());
}

pub fn renderReleasePassportJson(allocator: std.mem.Allocator, passport: *const ReleasePassport) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try passport.writeJson(&json);
    try aw.writer.writeByte('\n');
    return try allocator.dupe(u8, aw.writer.buffered());
}

fn readOptionalFile(allocator: std.mem.Allocator, path: []const u8, max_size: usize) ?[]u8 {
    return zigts.file_io.readFile(allocator, path, max_size) catch null;
}

fn extractZonVersion(bytes: []const u8) ?[]const u8 {
    const marker = ".version = \"";
    const start = std.mem.indexOf(u8, bytes, marker) orelse return null;
    const value_start = start + marker.len;
    const rest = bytes[value_start..];
    const value_end = std.mem.indexOfScalar(u8, rest, '"') orelse return null;
    return rest[0..value_end];
}

fn containsAny(haystack: []const u8, needles: []const []const u8) bool {
    for (needles) |needle| {
        if (std.mem.indexOf(u8, haystack, needle) != null) return true;
    }
    return false;
}

fn containsAll(haystack: []const u8, needles: []const []const u8) bool {
    for (needles) |needle| {
        if (std.mem.indexOf(u8, haystack, needle) == null) return false;
    }
    return true;
}

fn hasPendingReceiptBackedMeasurementNote(haystack: []const u8) bool {
    return std.mem.indexOf(u8, haystack, "pending receipt-backed measurement") != null or
        std.mem.indexOf(u8, haystack, "pending receipt-backed\nmeasurement") != null or
        std.mem.indexOf(u8, haystack, "pending\nreceipt-backed measurement") != null;
}

fn releaseGateRequirementsPresent(build_zig: []const u8, ci_yml: []const u8, release_yml: []const u8, verify_sh: []const u8) bool {
    return containsAll(build_zig, &build_gate_markers) and
        containsAll(ci_yml, &ci_gate_markers) and
        containsAll(release_yml, &ci_gate_markers) and
        containsAll(release_yml, &release_only_gate_markers) and
        containsAll(verify_sh, &verify_script_markers);
}

fn hasReleaseVerifyCommand(command: []const u8) bool {
    for (release_verify_commands) |candidate| {
        if (std.mem.eql(u8, candidate, command)) return true;
    }
    return false;
}

test "release verify commands cover release gates" {
    try std.testing.expect(hasReleaseVerifyCommand("zig fmt --check build.zig packages/"));
    try std.testing.expect(hasReleaseVerifyCommand("zig build test-zruntime"));
    try std.testing.expect(hasReleaseVerifyCommand("zig build test-docs-drift test-doc-links"));
    try std.testing.expect(hasReleaseVerifyCommand("bash scripts/test-install-archive-safety.sh"));
    try std.testing.expect(hasReleaseVerifyCommand("bash scripts/check-semantics-spec.sh"));
    try std.testing.expect(hasReleaseVerifyCommand("zig build bench-check"));
    try std.testing.expect(hasReleaseVerifyCommand("./zig-out/bin/zigttp doctor --release --json"));
}

test "pending receipt-backed measurement note tolerates markdown wrapping" {
    try std.testing.expect(hasPendingReceiptBackedMeasurementNote("pending receipt-backed measurement"));
    try std.testing.expect(hasPendingReceiptBackedMeasurementNote("pending receipt-backed\nmeasurement"));
    try std.testing.expect(hasPendingReceiptBackedMeasurementNote("pending\nreceipt-backed measurement"));
    try std.testing.expect(!hasPendingReceiptBackedMeasurementNote("pending manual benchmark measurement"));
}

test "release gate requirements require semantics and doctor wiring" {
    const build_zig =
        "smoke-v1 test-panic-isolation smoke-getting-started smoke-demo smoke-studio " ++
        "test-module-governance test-capability-audit test-docs-drift";
    const ci_yml =
        "zig fmt --check build.zig packages/\nzig build test\nzig build test-zruntime\nzig build test-docs-drift test-doc-links\n" ++
        "zig build -Doptimize=ReleaseFast\nzig build smoke-v1\nzig build test-panic-isolation\n" ++
        "bash scripts/test-examples.sh\nbash scripts/test-install-archive-safety.sh\n" ++
        "bash scripts/check-semantics-spec.sh\n";
    const release_yml =
        ci_yml ++
        "zig build smoke-getting-started\nzig build smoke-demo\nzig build smoke-studio\n" ++
        "./zig-out/bin/zigttp doctor --release --json\ncontents: write\n";
    const verify_sh =
        "zig build test\nzig build test-zruntime\nzig build test-docs-drift test-doc-links\n" ++
        "zig build -Doptimize=ReleaseFast\nzig build smoke-v1\nzig build test-panic-isolation\n" ++
        "bash scripts/test-examples.sh\nbash scripts/test-install-archive-safety.sh\n" ++
        "bash scripts/check-semantics-spec.sh\nzigts meta --json\n";

    try std.testing.expect(releaseGateRequirementsPresent(build_zig, ci_yml, release_yml, verify_sh));
    try std.testing.expect(!releaseGateRequirementsPresent(build_zig, ci_yml, "zig build test\n", verify_sh));
    try std.testing.expect(!releaseGateRequirementsPresent(build_zig, "zig build test\n", release_yml, verify_sh));
    try std.testing.expect(!releaseGateRequirementsPresent(build_zig, ci_yml, release_yml, "zig build test\n"));
}
