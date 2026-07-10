//! pi_forge_spec - proof-intent authoring for handler Spec/Effects capsules.
//!
//! The forge never writes files. It turns an explicit proof intent into a
//! candidate source file, runs the same compiler veto used by edit tools, and
//! returns an inspectable forge run payload.

const std = @import("std");
const zigts = @import("zigts");
const registry_mod = @import("../registry/registry.zig");
const ui_payload = @import("../ui_payload.zig");
const proof_enrichment = @import("../proof_enrichment.zig");
const common = @import("common.zig");

const json_utils = zigts.json_utils;

const name = "pi_forge_spec";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "forge-spec",
    .effect = .analyze,
    .description =
    \\Run a compiler-native proof intent forge. Input is a handler file,
    \\a v1 Spec<...> set, optional Effects<...> capability budget, and
    \\an optional mode. The tool annotates the handler, applies narrow
    \\compiler-owned repairs when supported, verifies the candidate in
    \\memory, and returns an approved-for-apply candidate only when no new
    \\compiler violations are introduced.
    ,
    .input_schema =
    \\{"type":"object","properties":{"kind":{"type":"string","enum":["spec"]},"file":{"type":"string"},"specs":{"type":"array","items":{"type":"string"}},"effect_budget":{"type":"array","items":{"type":"string"}},"mode":{"type":"string","enum":["annotate_only","repair_then_annotate"]}},"required":["kind","file","specs"]}
    ,
    .decode_json = decodeJson,
    .execute = execute,
};

const Mode = enum {
    annotate_only,
    repair_then_annotate,
};

const SpecIntent = struct {
    file: []const u8,
    specs: []const []const u8,
    effect_budget: []const []const u8,
    mode: Mode = .repair_then_annotate,
};

fn decodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    const trimmed = std.mem.trim(u8, args_json, " \t\r\n");
    if (trimmed.len == 0) return error.InvalidToolArgsJson;
    const out = try allocator.alloc([]const u8, 1);
    out[0] = try allocator.dupe(u8, trimmed);
    return out;
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    var spec_arena = std.heap.ArenaAllocator.init(allocator);
    defer spec_arena.deinit();
    const intent = parseSpecIntentArgs(spec_arena.allocator(), args) catch |err| switch (err) {
        error.InvalidToolArgsJson => return registry_mod.ToolResult.err(allocator, usage()),
        else => return err,
    };

    if (intent.specs.len == 0) {
        return registry_mod.ToolResult.err(allocator, name ++ ": specs must not be empty\n");
    }
    for (intent.specs) |spec| {
        if (!isV1Spec(spec)) {
            return registry_mod.ToolResult.errFmt(allocator, name ++ ": unknown v1 spec '{s}'\n", .{spec});
        }
    }

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const absolute = try common.resolveInsideWorkspace(allocator, root, intent.file);
    defer allocator.free(absolute);
    const relative = common.relativeToRoot(root, absolute);

    const source = zigts.file_io.readFile(allocator, absolute, common.default_output_limit) catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": failed to read {s}: {s}\n",
            .{ absolute, @errorName(e) },
        );
    };
    defer allocator.free(source);

    var steps: std.ArrayList(ui_payload.ForgeRunStep) = .empty;
    defer {
        for (steps.items) |*step| step.deinit(allocator);
        steps.deinit(allocator);
    }

    var proposed = try allocator.dupe(u8, source);
    defer allocator.free(proposed);

    if (intent.mode == .repair_then_annotate and wantsDeterminismRepair(intent.specs)) {
        if (try rewriteNondeterminism(allocator, proposed)) |rewritten| {
            allocator.free(proposed);
            proposed = rewritten;
            try appendStep(allocator, &steps, "repair_deterministic", "repair deterministic sources", "passed", "Wrapped Date.now() / Math.random() calls in durable step(...).");
        } else {
            try appendStep(allocator, &steps, "repair_deterministic", "repair deterministic sources", "skipped", "No Date.now() or Math.random() call was found.");
        }
    } else {
        try appendStep(allocator, &steps, "repair_structural", "repair structural specs", "skipped", "Mode is annotate_only or no deterministic/idempotent spec was requested.");
    }

    const annotated = annotateHandler(allocator, proposed, intent) catch |err| switch (err) {
        error.NoHandlerFunction => {
            return registry_mod.ToolResult.err(allocator, name ++ ": no function handler(...) declaration found\n");
        },
        error.UnsupportedHandlerSignature => {
            return registry_mod.ToolResult.err(allocator, name ++ ": handler signature is not in the supported single-line form\n");
        },
        else => return err,
    };
    allocator.free(proposed);
    proposed = annotated;
    try appendStep(allocator, &steps, "annotate_handler", "annotate handler proof intent", "passed", "Added Spec<...> and optional Effects<...> return markers.");

    var analysis = try proof_enrichment.analyzePatch(
        allocator,
        root,
        relative,
        source,
        proposed,
        null,
    );
    defer analysis.deinit(allocator);

    const verification_summary = try std.fmt.allocPrint(
        allocator,
        "{d} total, {d} new, {d} preexisting",
        .{ analysis.stats.total, analysis.stats.new, analysis.stats.preexisting orelse 0 },
    );
    defer allocator.free(verification_summary);

    const success = analysis.stats.new == 0;
    try appendStep(
        allocator,
        &steps,
        "prove_candidate",
        "prove proof-intent candidate",
        if (success) "passed" else "failed",
        verification_summary,
    );

    const run_id = try runId(allocator, intent.specs);
    defer allocator.free(run_id);
    const spec_label = try joinCsv(allocator, intent.specs);
    defer allocator.free(spec_label);
    const effect_label = try joinCsv(allocator, intent.effect_budget);
    defer allocator.free(effect_label);
    const terminal_reason = if (success)
        "candidate has zero new compiler violations; ready for approval"
    else
        "candidate still introduces compiler violations; inspect diagnostics before applying";

    var payload: ui_payload.UiPayload = .{ .forge_run = try ui_payload.ForgeRunPayload.init(
        allocator,
        run_id,
        relative,
        "spec",
        spec_label,
        if (effect_label.len == 0) "none" else effect_label,
        "handler",
        steps.items,
        proposed,
        analysis.unified_diff,
        success,
        terminal_reason,
        verification_summary,
        analysis.stats,
    ) };
    errdefer payload.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;
    try w.writeAll("{\"ok\":");
    try w.writeAll(if (success) "true" else "false");
    try w.writeAll(",\"run_id\":");
    try json_utils.writeJsonString(w, run_id);
    try w.writeAll(",\"file\":");
    try json_utils.writeJsonString(w, relative);
    try w.writeAll(",\"specs\":[");
    for (intent.specs, 0..) |spec, i| {
        if (i > 0) try w.writeByte(',');
        try json_utils.writeJsonString(w, spec);
    }
    try w.writeAll("],\"effect_budget\":[");
    for (intent.effect_budget, 0..) |cap, i| {
        if (i > 0) try w.writeByte(',');
        try json_utils.writeJsonString(w, cap);
    }
    try w.writeAll("],\"terminal_reason\":");
    try json_utils.writeJsonString(w, terminal_reason);
    try w.writeAll(",\"verification_summary\":");
    try json_utils.writeJsonString(w, verification_summary);
    try w.writeAll("}\n");
    buf = aw.toArrayList();

    return .{
        .ok = success,
        .llm_text = try buf.toOwnedSlice(allocator),
        .ui_payload = payload,
    };
}

fn usage() []const u8 {
    return name ++ ": usage: /forge spec file=<handler.ts> specs=<spec[,spec...]> [effects=<cap[,cap...]>] [mode=annotate_only|repair_then_annotate]\n";
}

fn parseSpecIntentArgs(allocator: std.mem.Allocator, args: []const []const u8) !SpecIntent {
    if (args.len == 0) return error.InvalidToolArgsJson;
    if (args.len == 1 and std.mem.indexOfScalar(u8, args[0], '{') != null) {
        return parseJsonSpec(allocator, args[0]);
    }
    return parseKvSpec(allocator, args);
}

fn parseJsonSpec(allocator: std.mem.Allocator, raw: []const u8) !SpecIntent {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, raw, .{});
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidToolArgsJson;
    const obj = parsed.value.object;
    const kind = getString(obj, "kind") orelse return error.InvalidToolArgsJson;
    if (!std.mem.eql(u8, kind, "spec")) return error.InvalidToolArgsJson;
    return .{
        .file = try allocator.dupe(u8, getString(obj, "file") orelse return error.InvalidToolArgsJson),
        .specs = try parseStringArrayValue(allocator, obj.get("specs") orelse return error.InvalidToolArgsJson),
        .effect_budget = if (obj.get("effect_budget")) |value| try parseStringArrayValue(allocator, value) else &.{},
        .mode = parseMode(getString(obj, "mode") orelse "repair_then_annotate") orelse return error.InvalidToolArgsJson,
    };
}

fn parseKvSpec(allocator: std.mem.Allocator, args: []const []const u8) !SpecIntent {
    var file: []const u8 = "";
    var specs: []const []const u8 = &.{};
    var effects: []const []const u8 = &.{};
    var mode: Mode = .repair_then_annotate;

    var start: usize = 0;
    if (std.mem.eql(u8, args[0], "spec")) start = 1;
    for (args[start..]) |arg| {
        const eq = std.mem.indexOfScalar(u8, arg, '=') orelse return error.InvalidToolArgsJson;
        const key = arg[0..eq];
        const value = arg[eq + 1 ..];
        if (std.mem.eql(u8, key, "file")) {
            file = try allocator.dupe(u8, value);
        } else if (std.mem.eql(u8, key, "specs")) {
            specs = try splitCsv(allocator, value);
        } else if (std.mem.eql(u8, key, "effects") or std.mem.eql(u8, key, "effect_budget")) {
            effects = try splitCsv(allocator, value);
        } else if (std.mem.eql(u8, key, "mode")) {
            mode = parseMode(value) orelse return error.InvalidToolArgsJson;
        } else {
            return error.InvalidToolArgsJson;
        }
    }

    if (file.len == 0 or specs.len == 0) return error.InvalidToolArgsJson;
    return .{ .file = file, .specs = specs, .effect_budget = effects, .mode = mode };
}

fn getString(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const value = obj.get(key) orelse return null;
    if (value != .string) return null;
    return value.string;
}

fn parseStringArrayValue(allocator: std.mem.Allocator, value: std.json.Value) ![]const []const u8 {
    if (value != .array) return error.InvalidToolArgsJson;
    const out = try allocator.alloc([]const u8, value.array.items.len);
    for (value.array.items, 0..) |item, i| {
        if (item != .string) return error.InvalidToolArgsJson;
        out[i] = try allocator.dupe(u8, item.string);
    }
    return out;
}

fn splitCsv(allocator: std.mem.Allocator, raw: []const u8) ![]const []const u8 {
    var out: std.ArrayList([]const u8) = .empty;
    var it = std.mem.splitScalar(u8, raw, ',');
    while (it.next()) |part_raw| {
        const part = std.mem.trim(u8, part_raw, " \t\r\n");
        if (part.len == 0) continue;
        try out.append(allocator, try allocator.dupe(u8, part));
    }
    return try out.toOwnedSlice(allocator);
}

fn parseMode(raw: []const u8) ?Mode {
    return std.meta.stringToEnum(Mode, raw);
}

fn isV1Spec(spec: []const u8) bool {
    const allowed = [_][]const u8{
        "deterministic",
        "read_only",
        "retry_safe",
        "idempotent",
        "state_isolated",
        "fault_covered",
        "no_secret_leakage",
        "no_credential_leakage",
        "input_validated",
        "pii_contained",
        "injection_safe",
        "pure",
        "stateless",
        "result_safe",
        "optional_safe",
    };
    for (allowed) |candidate| {
        if (std.mem.eql(u8, candidate, spec)) return true;
    }
    return false;
}

fn wantsDeterminismRepair(specs: []const []const u8) bool {
    for (specs) |spec| {
        if (std.mem.eql(u8, spec, "deterministic") or std.mem.eql(u8, spec, "idempotent")) return true;
    }
    return false;
}

fn annotateHandler(allocator: std.mem.Allocator, source: []const u8, intent: SpecIntent) ![]u8 {
    const marker = "function handler(";
    const fn_start = std.mem.indexOf(u8, source, marker) orelse return error.NoHandlerFunction;
    const open_brace = std.mem.indexOfScalarPos(u8, source, fn_start, '{') orelse return error.UnsupportedHandlerSignature;
    const line_end = std.mem.indexOfScalarPos(u8, source, fn_start, '\n') orelse source.len;
    if (open_brace > line_end) return error.UnsupportedHandlerSignature;
    const close_paren = std.mem.lastIndexOfScalar(u8, source[fn_start..open_brace], ')') orelse return error.UnsupportedHandlerSignature;
    const params_end = fn_start + close_paren + 1;
    const between = std.mem.trim(u8, source[params_end..open_brace], " \t\r\n");

    const return_type = try buildReturnType(allocator, intent);
    defer allocator.free(return_type);

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    try appendMissingTypeImports(allocator, &out, source, intent.effect_budget.len != 0);
    try out.appendSlice(allocator, source[0..params_end]);
    try out.print(allocator, ": {s} ", .{return_type});
    if (between.len > 0 and between[0] != ':') return error.UnsupportedHandlerSignature;
    try out.appendSlice(allocator, source[open_brace..]);
    return try out.toOwnedSlice(allocator);
}

fn appendMissingTypeImports(
    allocator: std.mem.Allocator,
    out: *std.ArrayList(u8),
    source: []const u8,
    needs_effects: bool,
) !void {
    if (std.mem.indexOf(u8, source, "Spec") == null or std.mem.indexOf(u8, source, "from \"zigttp:types\"") == null) {
        try out.appendSlice(allocator, "import type { Spec } from \"zigttp:types\";\n");
    }
    if (needs_effects and (std.mem.indexOf(u8, source, "Effects") == null or std.mem.indexOf(u8, source, "from \"zigttp:types\"") == null)) {
        try out.appendSlice(allocator, "import type { Effects } from \"zigttp:types\";\n");
    }
    if (out.items.len > 0 and source.len > 0) try out.append(allocator, '\n');
}

fn buildReturnType(allocator: std.mem.Allocator, intent: SpecIntent) ![]u8 {
    const spec_expr = try buildStringUnion(allocator, intent.specs);
    defer allocator.free(spec_expr);
    if (intent.effect_budget.len == 0) {
        return try std.fmt.allocPrint(allocator, "Response & Spec<{s}>", .{spec_expr});
    }
    const effect_expr = try buildStringUnion(allocator, intent.effect_budget);
    defer allocator.free(effect_expr);
    return try std.fmt.allocPrint(allocator, "Effects<Response, {s}> & Spec<{s}>", .{ effect_expr, spec_expr });
}

fn buildStringUnion(allocator: std.mem.Allocator, values: []const []const u8) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    for (values, 0..) |value, i| {
        if (i > 0) try out.appendSlice(allocator, " | ");
        try out.append(allocator, '"');
        try out.appendSlice(allocator, value);
        try out.append(allocator, '"');
    }
    return try out.toOwnedSlice(allocator);
}

fn rewriteNondeterminism(allocator: std.mem.Allocator, source: []const u8) !?[]u8 {
    var changed = false;
    const next = try replaceCalls(allocator, source, "Date.now()", "step(\"deterministic.clock.1\", () => Date.now())", &changed);
    defer allocator.free(next);
    const next2 = try replaceCalls(allocator, next, "Math.random()", "step(\"deterministic.random.1\", () => Math.random())", &changed);
    errdefer allocator.free(next2);
    if (!changed) {
        allocator.free(next2);
        return null;
    }
    const with_import = try ensureStepImport(allocator, next2);
    allocator.free(next2);
    return with_import;
}

fn replaceCalls(
    allocator: std.mem.Allocator,
    source: []const u8,
    needle: []const u8,
    replacement: []const u8,
    changed: *bool,
) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    var start: usize = 0;
    while (std.mem.indexOfPos(u8, source, start, needle)) |idx| {
        try out.appendSlice(allocator, source[start..idx]);
        try out.appendSlice(allocator, replacement);
        start = idx + needle.len;
        changed.* = true;
    }
    try out.appendSlice(allocator, source[start..]);
    return try out.toOwnedSlice(allocator);
}

fn ensureStepImport(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    if (std.mem.indexOf(u8, source, "zigttp:durable") != null and std.mem.indexOf(u8, source, "step") != null) {
        return try allocator.dupe(u8, source);
    }
    return try std.fmt.allocPrint(allocator, "import {{ step }} from \"zigttp:durable\";\n\n{s}", .{source});
}

fn appendStep(
    allocator: std.mem.Allocator,
    steps: *std.ArrayList(ui_payload.ForgeRunStep),
    id: []const u8,
    title: []const u8,
    state: []const u8,
    detail: []const u8,
) !void {
    try steps.append(allocator, try ui_payload.ForgeRunStep.init(allocator, id, title, state, detail));
}

fn runId(allocator: std.mem.Allocator, specs: []const []const u8) ![]u8 {
    const joined = try joinCsv(allocator, specs);
    defer allocator.free(joined);
    return try std.fmt.allocPrint(allocator, "forge:spec:{s}", .{joined});
}

fn joinCsv(allocator: std.mem.Allocator, values: []const []const u8) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    for (values, 0..) |value, i| {
        if (i > 0) try out.append(allocator, ',');
        try out.appendSlice(allocator, value);
    }
    return try out.toOwnedSlice(allocator);
}

const testing = std.testing;

test "parse kv spec intent" {
    const args = [_][]const u8{ "spec", "file=handler.ts", "specs=deterministic,idempotent", "effects=clock" };
    const intent = try parseSpecIntentArgs(testing.allocator, &args);
    defer {
        testing.allocator.free(intent.file);
        for (intent.specs) |item| testing.allocator.free(item);
        testing.allocator.free(intent.specs);
        for (intent.effect_budget) |item| testing.allocator.free(item);
        testing.allocator.free(intent.effect_budget);
    }
    try testing.expectEqualStrings("handler.ts", intent.file);
    try testing.expectEqual(@as(usize, 2), intent.specs.len);
    try testing.expectEqualStrings("deterministic", intent.specs[0]);
    try testing.expectEqualStrings("clock", intent.effect_budget[0]);
}

test "annotate handler adds spec return type and import" {
    const specs = [_][]const u8{ "deterministic", "idempotent" };
    const intent: SpecIntent = .{ .file = "handler.ts", .specs = &specs, .effect_budget = &.{} };
    const source =
        \\function handler(req: Request) {
        \\    return Response.json({ ok: true });
        \\}
        \\
    ;
    const out = try annotateHandler(testing.allocator, source, intent);
    defer testing.allocator.free(out);
    try testing.expect(std.mem.indexOf(u8, out, "import type { Spec } from \"zigttp:types\";") != null);
    try testing.expect(std.mem.indexOf(u8, out, "function handler(req: Request): Response & Spec<\"deterministic\" | \"idempotent\"> {") != null);
}

test "rewrite nondeterminism wraps Date.now and imports step" {
    const source =
        \\function handler(req: Request): Response {
        \\    return Response.json({ now: Date.now() });
        \\}
        \\
    ;
    const out = (try rewriteNondeterminism(testing.allocator, source)) orelse return error.MissingRewrite;
    defer testing.allocator.free(out);
    try testing.expect(std.mem.indexOf(u8, out, "import { step } from \"zigttp:durable\";") != null);
    try testing.expect(std.mem.indexOf(u8, out, "step(\"deterministic.clock.1\", () => Date.now())") != null);
}

test "execute forges deterministic spec candidate that clears veto" {
    const rel_path = ".zig-cache/tmp/spec-forge-handler.ts";
    const source =
        \\function handler(req: Request): Response {
        \\    return Response.json({ now: Date.now() });
        \\}
        \\
    ;
    try zigts.file_io.writeFile(testing.allocator, rel_path, source);

    var result = try execute(testing.allocator, &.{ "spec", "file=.zig-cache/tmp/spec-forge-handler.ts", "specs=deterministic,idempotent" });
    defer result.deinit(testing.allocator);
    try testing.expect(result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"ok\":true") != null);
    try testing.expect(result.ui_payload != null);
    switch (result.ui_payload.?) {
        .forge_run => |run| {
            try testing.expect(run.success);
            try testing.expect(std.mem.indexOf(u8, run.final_content, "Spec<\"deterministic\" | \"idempotent\">") != null);
            try testing.expect(std.mem.indexOf(u8, run.final_content, "step(\"deterministic.clock.1\", () => Date.now())") != null);
        },
        else => return error.TestFailed,
    }
}
