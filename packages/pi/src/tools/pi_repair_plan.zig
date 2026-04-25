//! pi_repair_plan - compile a handler proof failure into typed repair plans.

const std = @import("std");
const zigts = @import("zigts");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");
const property_goals = @import("../property_goals.zig");

const ir = zigts.parser;
const counterexample = zigts.counterexample;
const flow_checker = zigts.flow_checker;
const handler_verifier = zigts.handler_verifier;
const json_utils = zigts.json_utils;
const repair_plan = zigts.repair_plan;

const name = "pi_repair_plan";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "repair-plan",
    .description =
    \\Generate compiler-native typed repair plans for a handler.
    \\The tool runs handler verification and property witness analysis,
    \\then returns structured repair intents for the narrow supported v1
    \\cases: unchecked Result.value, unchecked optional use, missing
    \\fallback Response, no_secret_leakage, no_credential_leakage, and
    \\injection_safe. The compiler does not edit files; apply one plan,
    \\then re-run the veto / goal check until the proof passes.
    ,
    .input_schema =
    \\{"type":"object","properties":{"path":{"type":"string"},"goals":{"type":"array","items":{"type":"string"}}},"required":["path"]}
    ,
    .decode_json = decodeJson,
    .execute = execute,
};

fn decodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, args_json, .{});
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidToolArgsJson;
    const obj = parsed.value.object;

    const path_val = obj.get("path") orelse return error.InvalidToolArgsJson;
    if (path_val != .string) return error.InvalidToolArgsJson;

    var goals_count: usize = 0;
    if (obj.get("goals")) |g| {
        if (g != .array) return error.InvalidToolArgsJson;
        goals_count = g.array.items.len;
    }

    const out = try allocator.alloc([]const u8, 1 + goals_count);
    errdefer allocator.free(out);
    out[0] = try allocator.dupe(u8, path_val.string);

    if (obj.get("goals")) |g| {
        for (g.array.items, 0..) |item, i| {
            if (item != .string) return error.InvalidToolArgsJson;
            out[i + 1] = try allocator.dupe(u8, item.string);
        }
    }
    return out;
}

fn parseGoal(s: []const u8) ?counterexample.PropertyTag {
    return property_goals.parseDriveableGoal(s);
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) return registry_mod.ToolResult.err(allocator, name ++ ": requires a path\n");

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const absolute = try common.resolveInsideWorkspace(allocator, root, args[0]);
    defer allocator.free(absolute);

    const source = zigts.file_io.readFile(allocator, absolute, common.default_output_limit) catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": failed to read {s}: {s}\n",
            .{ absolute, @errorName(e) },
        );
    };
    defer allocator.free(source);

    var goals: std.ArrayListUnmanaged(counterexample.PropertyTag) = .empty;
    defer goals.deinit(allocator);
    if (args.len == 1) {
        try goals.appendSlice(allocator, &property_goals.supported_goals);
    } else {
        for (args[1..]) |g| {
            const tag = parseGoal(g) orelse {
                return registry_mod.ToolResult.errFmt(allocator, name ++ ": unknown goal {s}\n", .{g});
            };
            try goals.append(allocator, tag);
        }
    }

    var strip_result = zigts.strip(allocator, source, .{ .comptime_env = .{} }) catch |e| {
        return registry_mod.ToolResult.errFmt(allocator, name ++ ": TypeScript strip failed: {s}\n", .{@errorName(e)});
    };
    defer strip_result.deinit();

    var atoms = zigts.context.AtomTable.init(allocator);
    defer atoms.deinit();
    var js_parser = zigts.parser.JsParser.init(allocator, strip_result.code);
    defer js_parser.deinit();
    js_parser.setAtomTable(&atoms);

    const program_root = js_parser.parse() catch |e| {
        return registry_mod.ToolResult.errFmt(allocator, name ++ ": parse failed: {s}\n", .{@errorName(e)});
    };
    const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
    const handler_fn = handler_verifier.findHandlerFunction(ir_view, program_root) orelse {
        return registry_mod.ToolResult.err(allocator, name ++ ": no handler function found in file\n");
    };

    var verifier = zigts.HandlerVerifier.init(allocator, ir_view, &atoms, null, null);
    defer verifier.deinit();
    _ = try verifier.verify(handler_fn);

    var checker = zigts.FlowChecker.init(allocator, ir_view, &atoms);
    defer checker.deinit();
    _ = try checker.check(handler_fn);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    const policy_hash = zigts.rule_registry.policyHash();
    try w.writeAll("{\"ok\":");
    const has_failures = hasVerifierErrors(verifier.getDiagnostics()) or
        hasRequestedFlowDiagnostics(checker.getDiagnostics(), goals.items);
    try w.writeAll(if (has_failures) "false" else "true");
    try w.writeAll(",\"policy_hash\":\"");
    try w.writeAll(&policy_hash);
    try w.writeAll("\",\"goals\":[");
    for (goals.items, 0..) |g, i| {
        if (i > 0) try w.writeByte(',');
        try json_utils.writeJsonString(w, g.asString());
    }
    try w.writeAll("],\"diagnostics\":[");

    var plan_index: usize = 0;
    var diag_index: usize = 0;
    var first_diag = true;
    for (verifier.getDiagnostics()) |diag| {
        const loc = ir_view.getLoc(diag.node) orelse continue;
        if (!first_diag) try w.writeByte(',');
        first_diag = false;
        diag_index += 1;
        const diag_id = try std.fmt.allocPrint(allocator, "diag_{d:0>3}", .{diag_index});
        defer allocator.free(diag_id);
        try writeVerifierDiagnostic(w, diag_id, diag, loc.line, loc.column);
    }
    try w.writeAll("],\"witnesses\":[");

    var first_witness = true;
    var witness_index: usize = 0;
    for (checker.getDiagnostics()) |diag| {
        const tag = flow_checker.propertyTagForKind(diag.kind) orelse continue;
        if (!goalRequested(goals.items, tag)) continue;
        const loc = ir_view.getLoc(diag.node) orelse continue;
        const constraints: []const counterexample.WitnessConstraint = if (diag.witness) |wit| wit.path_constraints else &.{};
        const io_calls: []const counterexample.TrackedIoCall = if (diag.witness) |wit| wit.io_calls else &.{};
        var witness = counterexample.solve(allocator, .{
            .property = tag,
            .origin = .{ .line = loc.line, .column = loc.column },
            .sink = .{ .line = loc.line, .column = loc.column },
            .summary = diag.message,
            .constraints = constraints,
            .io_calls = io_calls,
        }) catch continue;
        defer witness.deinit(allocator);

        if (!first_witness) try w.writeByte(',');
        first_witness = false;
        witness_index += 1;
        const witness_id = try std.fmt.allocPrint(allocator, "wit_{d:0>3}", .{witness_index});
        defer allocator.free(witness_id);
        try writeWitness(w, witness_id, diag, witness);
    }

    try w.writeAll("],\"plans\":[");
    var first_plan = true;
    diag_index = 0;
    for (verifier.getDiagnostics()) |diag| {
        const loc = ir_view.getLoc(diag.node) orelse continue;
        diag_index += 1;
        const plan = repair_plan.fromVerifierDiagnostic(diag, .{ .line = loc.line, .column = loc.column }) orelse continue;
        const subject_name = repairSubjectName(ir_view, &atoms, diag);
        plan_index += 1;
        if (!first_plan) try w.writeByte(',');
        first_plan = false;
        try writePlan(w, allocator, plan_index, plan, "diag", diag_index, subject_name);
    }

    witness_index = 0;
    for (checker.getDiagnostics()) |diag| {
        const tag = flow_checker.propertyTagForKind(diag.kind) orelse continue;
        if (!goalRequested(goals.items, tag)) continue;
        const loc = ir_view.getLoc(diag.node) orelse continue;
        witness_index += 1;
        const plan = repair_plan.fromFlowDiagnostic(diag, tag, .{ .line = loc.line, .column = loc.column }) orelse continue;
        plan_index += 1;
        if (!first_plan) try w.writeByte(',');
        first_plan = false;
        try writePlan(w, allocator, plan_index, plan, "wit", witness_index, null);
    }
    try w.writeAll("]}\n");

    buf = aw.toArrayList();
    const llm_text = try buf.toOwnedSlice(allocator);
    errdefer allocator.free(llm_text);
    return .{ .ok = !has_failures, .llm_text = llm_text };
}

fn writeVerifierDiagnostic(
    writer: *std.Io.Writer,
    id: []const u8,
    diag: handler_verifier.Diagnostic,
    line: u32,
    column: u16,
) !void {
    try writer.writeAll("{\"id\":");
    try json_utils.writeJsonString(writer, id);
    try writer.writeAll(",\"kind\":");
    try json_utils.writeJsonString(writer, @tagName(diag.kind));
    try writer.writeAll(",\"severity\":");
    try json_utils.writeJsonString(writer, diag.severity.label());
    try writer.writeAll(",\"line\":");
    try writer.print("{d}", .{line});
    try writer.writeAll(",\"column\":");
    try writer.print("{d}", .{column});
    try writer.writeAll(",\"message\":");
    try json_utils.writeJsonString(writer, diag.message);
    if (diag.help) |help| {
        try writer.writeAll(",\"help\":");
        try json_utils.writeJsonString(writer, help);
    }
    try writer.writeByte('}');
}

fn writeWitness(
    writer: *std.Io.Writer,
    id: []const u8,
    diag: flow_checker.Diagnostic,
    witness: counterexample.CounterexampleWitness,
) !void {
    try writer.writeAll("{\"id\":");
    try json_utils.writeJsonString(writer, id);
    try writer.writeAll(",\"property\":");
    try json_utils.writeJsonString(writer, witness.property.asString());
    try writer.writeAll(",\"summary\":");
    try json_utils.writeJsonString(writer, diag.message);
    try writer.writeAll(",\"origin\":{\"line\":");
    try writer.print("{d}", .{witness.origin.line});
    try writer.writeAll(",\"column\":");
    try writer.print("{d}", .{witness.origin.column});
    try writer.writeAll("},\"sink\":{\"line\":");
    try writer.print("{d}", .{witness.sink.line});
    try writer.writeAll(",\"column\":");
    try writer.print("{d}", .{witness.sink.column});
    try writer.writeAll("},\"request\":{\"method\":");
    try json_utils.writeJsonString(writer, witness.request.method);
    try writer.writeAll(",\"url\":");
    try json_utils.writeJsonString(writer, witness.request.url);
    try writer.writeAll(",\"has_auth_header\":");
    try writer.writeAll(if (witness.request.has_auth_header) "true" else "false");
    try writer.writeAll("},\"io_stubs\":[");
    for (witness.io_stubs, 0..) |stub, i| {
        if (i > 0) try writer.writeByte(',');
        try writer.writeAll("{\"seq\":");
        try writer.print("{d}", .{stub.seq});
        try writer.writeAll(",\"module\":");
        try json_utils.writeJsonString(writer, stub.module);
        try writer.writeAll(",\"fn\":");
        try json_utils.writeJsonString(writer, stub.func);
        try writer.writeAll(",\"result\":");
        try writer.writeAll(stub.result_json);
        try writer.writeByte('}');
    }
    try writer.writeAll("]}");
}

fn writePlan(
    writer: *std.Io.Writer,
    allocator: std.mem.Allocator,
    index: usize,
    plan: repair_plan.Plan,
    closes_prefix: []const u8,
    closes_index: usize,
    subject_name: ?[]const u8,
) !void {
    const id = try std.fmt.allocPrint(allocator, "rp_{d:0>3}", .{index});
    defer allocator.free(id);
    const closes = try std.fmt.allocPrint(allocator, "{s}_{d:0>3}", .{ closes_prefix, closes_index });
    defer allocator.free(closes);
    const template = try concreteTemplate(allocator, plan, subject_name);
    defer allocator.free(template);

    try writer.writeAll("{\"id\":");
    try json_utils.writeJsonString(writer, id);
    try writer.writeAll(",\"kind\":");
    try json_utils.writeJsonString(writer, plan.kind.asString());
    try writer.writeAll(",\"target\":{\"line\":");
    try writer.print("{d}", .{plan.target.line});
    try writer.writeAll(",\"column\":");
    try writer.print("{d}", .{plan.target.column});
    try writer.writeAll("},\"behavioral_change\":");
    try writer.writeAll(if (plan.behavioral_change) "true" else "false");
    try writer.writeAll(",\"summary\":");
    try json_utils.writeJsonString(writer, plan.summary);
    try writer.writeAll(",\"edit_intent\":{\"kind\":");
    try json_utils.writeJsonString(writer, plan.edit_intent.kind.asString());
    try writer.writeAll(",\"line\":");
    try writer.print("{d}", .{plan.edit_intent.line});
    try writer.writeAll(",\"column\":");
    try writer.print("{d}", .{plan.edit_intent.column});
    try writer.writeAll(",\"template\":");
    try json_utils.writeJsonString(writer, template);
    if (subject_name) |name_text| {
        try writer.writeAll(",\"bindings\":{\"subject\":");
        try json_utils.writeJsonString(writer, name_text);
        try writer.writeByte('}');
    }
    try writer.writeAll("},\"closes\":[");
    try json_utils.writeJsonString(writer, closes);
    try writer.writeAll("]}");
}

fn concreteTemplate(
    allocator: std.mem.Allocator,
    plan: repair_plan.Plan,
    subject_name: ?[]const u8,
) ![]u8 {
    return switch (plan.kind) {
        .check_result_before_value => {
            const name_text = subject_name orelse "result";
            return std.fmt.allocPrint(
                allocator,
                "if (!{s}.ok) return Response.json({{ error: {s}.error }}, {{ status: 400 }});",
                .{ name_text, name_text },
            );
        },
        .narrow_optional_before_use => {
            const name_text = subject_name orelse "value";
            return std.fmt.allocPrint(
                allocator,
                "if ({s} === undefined) return Response.json({{ error: \"missing value\" }}, {{ status: 400 }});",
                .{name_text},
            );
        },
        else => allocator.dupe(u8, plan.edit_intent.template),
    };
}

fn repairSubjectName(
    ir_view: ir.IrView,
    atoms: *zigts.context.AtomTable,
    diag: handler_verifier.Diagnostic,
) ?[]const u8 {
    return switch (diag.kind) {
        .unchecked_result_value, .unchecked_optional_access => memberObjectName(ir_view, atoms, diag.node),
        .unchecked_optional_use => identifierName(ir_view, atoms, diag.node),
        else => null,
    };
}

fn memberObjectName(
    ir_view: ir.IrView,
    atoms: *zigts.context.AtomTable,
    node: zigts.parser.NodeIndex,
) ?[]const u8 {
    const tag = ir_view.getTag(node) orelse return null;
    if (tag == .identifier) return identifierName(ir_view, atoms, node);
    if (tag != .member_access and tag != .optional_chain) return null;
    const member = ir_view.getMember(node) orelse return null;
    return identifierName(ir_view, atoms, member.object);
}

fn identifierName(
    ir_view: ir.IrView,
    atoms: *zigts.context.AtomTable,
    node: zigts.parser.NodeIndex,
) ?[]const u8 {
    const tag = ir_view.getTag(node) orelse return null;
    if (tag != .identifier) return null;
    const binding = ir_view.getBinding(node) orelse return null;
    return atoms.getName(@enumFromInt(binding.slot));
}

fn hasRequestedFlowDiagnostics(
    diagnostics: []const flow_checker.Diagnostic,
    goals: []const counterexample.PropertyTag,
) bool {
    for (diagnostics) |diag| {
        const tag = flow_checker.propertyTagForKind(diag.kind) orelse continue;
        if (goalRequested(goals, tag)) return true;
    }
    return false;
}

fn hasVerifierErrors(diagnostics: []const handler_verifier.Diagnostic) bool {
    for (diagnostics) |diag| {
        if (diag.severity == .err) return true;
    }
    return false;
}

fn goalRequested(goals: []const counterexample.PropertyTag, tag: counterexample.PropertyTag) bool {
    for (goals) |g| if (g == tag) return true;
    return false;
}

const testing = std.testing;

test "decodeJson accepts path and goals" {
    const args = try decodeJson(testing.allocator, "{\"path\":\"h.ts\",\"goals\":[\"injection_safe\"]}");
    defer {
        for (args) |arg| testing.allocator.free(arg);
        testing.allocator.free(args);
    }
    try testing.expectEqual(@as(usize, 2), args.len);
    try testing.expectEqualStrings("h.ts", args[0]);
    try testing.expectEqualStrings("injection_safe", args[1]);
}

test "parseGoal rejects structural property tags" {
    try testing.expectEqual(counterexample.PropertyTag.no_secret_leakage, parseGoal("no_secret_leakage").?);
    try testing.expectEqual(counterexample.PropertyTag.no_credential_leakage, parseGoal("no_credential_leakage").?);
    try testing.expectEqual(counterexample.PropertyTag.injection_safe, parseGoal("injection_safe").?);
    try testing.expect(parseGoal("input_validated") == null);
    try testing.expect(parseGoal("pii_contained") == null);
}

test "execute rejects structural property goals" {
    var result = try execute(testing.allocator, &.{ "examples/handler/handler.ts", "pii_contained" });
    defer result.deinit(testing.allocator);

    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "unknown goal pii_contained") != null);
}

test "tool description names repair plan authority boundary" {
    try testing.expect(std.mem.indexOf(u8, tool.description, "does not edit files") != null);
    try testing.expect(std.mem.indexOf(u8, tool.description, "unchecked Result.value") != null);
}

test "concreteTemplate uses extracted subject names" {
    const plan = repair_plan.fromVerifierDiagnostic(
        .{
            .severity = .err,
            .kind = .unchecked_result_value,
            .node = 0,
            .message = "result.value accessed without checking result.ok first",
            .help = null,
        },
        .{ .line = 8, .column = 12 },
    ) orelse return error.MissingPlan;
    const template = try concreteTemplate(testing.allocator, plan, "authResult");
    defer testing.allocator.free(template);
    try testing.expect(std.mem.indexOf(u8, template, "authResult.ok") != null);
    try testing.expect(std.mem.indexOf(u8, template, "authResult.error") != null);
}
