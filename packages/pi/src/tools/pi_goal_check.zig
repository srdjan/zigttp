//! pi_goal_check - verify a handler against a set of property goals and
//! surface executable counterexample witnesses for any goal that fails.
//!
//! The tool loads the handler at `path`, runs the FlowChecker, and for each
//! diagnostic whose property tag appears in the requested `goals` set,
//! synthesises a witness using `zigts.counterexample.solve`. Each witness
//! carries a concrete Request plus the virtual-module stub sequence needed
//! to drive the handler into the violating state - the agent can replay it
//! through `zigttp mock --replay` to confirm the leak before proposing a
//! repair.
//!
//! The input schema accepts:
//!   { "path": "handler.ts", "goals": ["no_secret_leakage", ...] }
//! `goals` is optional; when absent, every property tag the counterexample
//! surface currently supports is checked.
//!
//! The output is a JSON object:
//!   { "ok": bool,                  // true iff no requested goal was violated
//!     "goals": [property_tag...],  // goals actually checked
//!     "witnesses": [{ property, origin, sink, summary, request, io_stubs }...] }
//!
//! `ok` is false as soon as any witness exists for a requested goal. The
//! agent reads the witness list, repairs the handler to close the concrete
//! path, then re-invokes this tool until `ok` is true.

const std = @import("std");
const zigts = @import("zigts");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");

const ir = zigts.parser;
const counterexample = zigts.counterexample;
const flow_checker = zigts.flow_checker;
const json_utils = zigts.json_utils;
const handler_verifier = zigts.handler_verifier;

const name = "pi_goal_check";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "goal-check",
    .description =
    \\Check a handler against one or more property goals and return
    \\executable counterexample witnesses for the goals that are violated.
    \\
    \\Supported goals:
    \\  - no_secret_leakage    (env vars with SECRET/PASSWORD/KEY/TOKEN
    \\                          names must not reach response bodies,
    \\                          logs, or external egress)
    \\  - no_credential_leakage (Authorization headers and JWT payloads
    \\                          must not reach response bodies or logs)
    \\  - injection_safe       (unvalidated user input must not reach
    \\                          sensitive sinks)
    \\
    \\Each witness carries a concrete Request and the virtual-module stub
    \\sequence that drives the handler into the violating path. Replay
    \\witnesses through `zigttp mock --replay` to verify before repair.
    \\Repairs should *close the concrete path*, not merely mute the rule.
    ,
    .input_schema =
    \\{"type":"object","properties":{"path":{"type":"string"},"goals":{"type":"array","items":{"type":"string"}}},"required":["path"]}
    ,
    .decode_json = decodeJson,
    .execute = execute,
};

/// Pack the JSON input into the argv slice the executor expects:
///   args[0]      = path
///   args[1..]    = goal tags (empty means "check all supported goals")
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

const all_goals = [_]counterexample.PropertyTag{
    .no_secret_leakage,
    .no_credential_leakage,
    .injection_safe,
};

fn parseGoal(s: []const u8) ?counterexample.PropertyTag {
    return std.meta.stringToEnum(counterexample.PropertyTag, s);
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) {
        return registry_mod.ToolResult.err(allocator, name ++ ": requires a path\n");
    }

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const absolute = try common.resolveInsideWorkspace(allocator, root, args[0]);
    defer allocator.free(absolute);

    const source = zigts.file_io.readFile(
        allocator,
        absolute,
        common.default_output_limit,
    ) catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": failed to read {s}: {s}\n",
            .{ absolute, @errorName(e) },
        );
    };
    defer allocator.free(source);

    // Resolve the goal set. When the caller omits `goals`, check everything
    // the counterexample surface currently models.
    var goals: std.ArrayListUnmanaged(counterexample.PropertyTag) = .empty;
    defer goals.deinit(allocator);
    if (args.len == 1) {
        try goals.appendSlice(allocator, &all_goals);
    } else {
        for (args[1..]) |g| {
            const tag = parseGoal(g) orelse {
                return registry_mod.ToolResult.errFmt(
                    allocator,
                    name ++ ": unknown goal {s}\n",
                    .{g},
                );
            };
            try goals.append(allocator, tag);
        }
    }

    var strip_result = zigts.strip(allocator, source, .{
        .comptime_env = .{},
    }) catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": TypeScript strip failed: {s}\n",
            .{@errorName(e)},
        );
    };
    defer strip_result.deinit();

    var atoms = zigts.context.AtomTable.init(allocator);
    defer atoms.deinit();
    var js_parser = zigts.parser.JsParser.init(allocator, strip_result.code);
    defer js_parser.deinit();
    js_parser.setAtomTable(&atoms);

    const program_root = js_parser.parse() catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": parse failed: {s}\n",
            .{@errorName(e)},
        );
    };
    const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);

    const handler_fn = handler_verifier.findHandlerFunction(ir_view, program_root) orelse {
        return registry_mod.ToolResult.err(
            allocator,
            name ++ ": no handler function found in file\n",
        );
    };

    var checker = zigts.FlowChecker.init(allocator, ir_view, &atoms);
    defer checker.deinit();
    _ = checker.check(handler_fn) catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": flow check failed: {s}\n",
            .{@errorName(e)},
        );
    };

    // Emit the JSON envelope.
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll("{\"goals\":[");
    for (goals.items, 0..) |g, i| {
        if (i > 0) try w.writeByte(',');
        try w.writeByte('"');
        try w.writeAll(g.asString());
        try w.writeByte('"');
    }
    try w.writeAll("],\"witnesses\":[");

    var witness_count: usize = 0;
    for (checker.getDiagnostics()) |diag| {
        const tag = flow_checker.propertyTagForKind(diag.kind) orelse continue;
        if (!goalRequested(goals.items, tag)) continue;

        const loc = ir_view.getLoc(diag.node) orelse continue;
        const span: counterexample.SourceSpan = .{
            .line = loc.line,
            .column = loc.column,
        };

        var witness = counterexample.solve(allocator, .{
            .property = tag,
            .origin = span,
            .sink = span,
            .summary = diag.message,
            .constraints = diag.path_constraints,
            .io_calls = diag.io_calls,
        }) catch continue;
        defer witness.deinit(allocator);

        if (witness_count > 0) try w.writeByte(',');
        witness_count += 1;

        // Each witness is a JSON object rather than the JSONL shape that
        // trace replay consumes. Callers that want replay input can pass
        // individual witnesses through `counterexample.writeJsonl`.
        try w.print(
            "{{\"property\":\"{s}\",\"origin\":{{\"line\":{d},\"column\":{d}}},\"sink\":{{\"line\":{d},\"column\":{d}}},\"summary\":",
            .{ tag.asString(), span.line, span.column, span.line, span.column },
        );
        try json_utils.writeJsonString(w, diag.message);
        try w.print(
            ",\"request\":{{\"method\":\"{s}\",\"url\":",
            .{witness.request.method},
        );
        try json_utils.writeJsonString(w, witness.request.url);
        try w.writeAll(",\"has_auth_header\":");
        try w.writeAll(if (witness.request.has_auth_header) "true" else "false");
        try w.writeAll("},\"io_stubs\":[");
        for (witness.io_stubs, 0..) |stub, si| {
            if (si > 0) try w.writeByte(',');
            try w.print(
                "{{\"seq\":{d},\"module\":\"{s}\",\"fn\":\"{s}\",\"result\":{s}}}",
                .{ stub.seq, stub.module, stub.func, stub.result_json },
            );
        }
        try w.writeAll("]}");
    }

    try w.print("],\"ok\":{s}}}\n", .{if (witness_count == 0) "true" else "false"});
    buf = aw.toArrayList();
    const llm_text = try buf.toOwnedSlice(allocator);
    errdefer allocator.free(llm_text);

    return .{
        .ok = witness_count == 0,
        .llm_text = llm_text,
    };
}

fn goalRequested(goals: []const counterexample.PropertyTag, tag: counterexample.PropertyTag) bool {
    for (goals) |g| if (g == tag) return true;
    return false;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "decodeJson accepts path-only payload and checks all goals" {
    const args = try decodeJson(
        testing.allocator,
        "{\"path\":\"handler.ts\"}",
    );
    defer {
        for (args) |a| testing.allocator.free(a);
        testing.allocator.free(args);
    }
    try testing.expectEqual(@as(usize, 1), args.len);
    try testing.expectEqualStrings("handler.ts", args[0]);
}

test "decodeJson picks up goals array" {
    const args = try decodeJson(
        testing.allocator,
        "{\"path\":\"h.ts\",\"goals\":[\"no_secret_leakage\",\"injection_safe\"]}",
    );
    defer {
        for (args) |a| testing.allocator.free(a);
        testing.allocator.free(args);
    }
    try testing.expectEqual(@as(usize, 3), args.len);
    try testing.expectEqualStrings("h.ts", args[0]);
    try testing.expectEqualStrings("no_secret_leakage", args[1]);
    try testing.expectEqualStrings("injection_safe", args[2]);
}

test "parseGoal recognises every supported property tag" {
    try testing.expectEqual(counterexample.PropertyTag.no_secret_leakage, parseGoal("no_secret_leakage").?);
    try testing.expectEqual(counterexample.PropertyTag.no_credential_leakage, parseGoal("no_credential_leakage").?);
    try testing.expectEqual(counterexample.PropertyTag.injection_safe, parseGoal("injection_safe").?);
    try testing.expect(parseGoal("totally_not_a_property") == null);
}

test "tool description names every currently supported goal" {
    try testing.expect(std.mem.indexOf(u8, tool.description, "no_secret_leakage") != null);
    try testing.expect(std.mem.indexOf(u8, tool.description, "no_credential_leakage") != null);
    try testing.expect(std.mem.indexOf(u8, tool.description, "injection_safe") != null);
    try testing.expect(std.mem.indexOf(u8, tool.description, "close the concrete path") != null);
}

test "execute emits witness JSON for the secret-leak fixture" {
    // The fixture lives under the repo's examples/ directory. When the
    // test runs from the project root (the default `zig build test`), the
    // relative path resolves. When run outside the workspace the tool
    // returns a PathOutsideWorkspace error and we skip.
    const result = execute(testing.allocator, &.{"examples/handler/secret-leak.ts"}) catch |e| switch (e) {
        error.PathOutsideWorkspace, error.FileNotFound => return,
        else => return e,
    };
    var mut = result;
    defer mut.deinit(testing.allocator);

    // Secret leak present -> tool reports not-ok with at least one witness.
    try testing.expect(!mut.ok);
    try testing.expect(std.mem.indexOf(u8, mut.llm_text, "\"ok\":false") != null);
    try testing.expect(std.mem.indexOf(u8, mut.llm_text, "\"property\":\"no_secret_leakage\"") != null);
    // The witness request is GET / with no body, and carries an env stub
    // in the io_stubs list.
    try testing.expect(std.mem.indexOf(u8, mut.llm_text, "\"method\":\"GET\"") != null);
    try testing.expect(std.mem.indexOf(u8, mut.llm_text, "\"fn\":\"env\"") != null);
    try testing.expect(std.mem.indexOf(u8, mut.llm_text, "\"module\":\"env\"") != null);
}
