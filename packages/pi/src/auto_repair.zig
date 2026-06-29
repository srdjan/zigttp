//! auto_repair - turn a failed-veto draft into compiler-authored retry guidance.
//!
//! When a model edit fails the compiler veto, the repair lane (pi_repair_plan)
//! often already knows the exact fix: a typed repair plan with a concrete
//! template keyed to a diagnostic line. This module formats those plans plus
//! the single smallest counterexample witness into a compact block the retry
//! prompt injects, so a needed model retry converges in one shot instead of
//! re-deriving the fix across several round-trips.
//!
//! Pure: it parses a pi_repair_plan JSON envelope and emits text. It never
//! writes files, calls the model, or applies an edit.

const std = @import("std");

/// Maximum repair templates to surface in a retry block. Past a handful the
/// signal-to-token ratio drops and the model is better served re-reading the
/// handler; the cap also bounds prompt growth.
pub const max_templates: usize = 8;

/// Maximum io stubs to show for the chosen counterexample witness. A failing
/// path with more than this is summarised by its first few calls; the goal is
/// to ground the model, not to dump the whole trace.
pub const max_witness_stubs: usize = 4;

/// True when the deterministic repair lane is worth running on a veto failure.
/// SQL failures have their own escalation path in the loop, and a failure with
/// zero NEW violations carries no plans to author from.
pub fn shouldRunLane(sql_failure: bool, new_violations: u32) bool {
    return !sql_failure and new_violations > 0;
}

/// Build a retry-guidance block from a pi_repair_plan JSON envelope. Returns
/// null when the envelope carries no actionable repair plans (the caller then
/// falls back to the raw diagnostics alone). The returned bytes are owned by
/// `allocator` and end with a newline.
pub fn buildRetryBlock(allocator: std.mem.Allocator, plan_json: []const u8) !?[]u8 {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, plan_json, .{}) catch return null;
    defer parsed.deinit();
    if (parsed.value != .object) return null;
    const obj = parsed.value.object;

    const plans = blk: {
        const v = obj.get("plans") orelse break :blk null;
        break :blk if (v == .array) v.array.items else null;
    };
    if (plans == null or plans.?.len == 0) return null;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    // fromArrayList empties `buf` and moves the buffer into `aw`, so the only
    // cleanup that reclaims written bytes on an early `return null` (or a write
    // error) is aw.deinit(). On the success path toArrayList() empties aw before
    // this defer runs, so it is a no-op there.
    defer aw.deinit();
    const w = &aw.writer;

    try w.writeAll(
        "COMPILER-AUTHORED FIX. The analyzer already derived these exact repairs " ++
            "for your failed draft. Apply them at the given lines and emit a new, " ++
            "complete edit:\n",
    );

    var shown: usize = 0;
    for (plans.?) |plan_val| {
        if (shown >= max_templates) break;
        if (plan_val != .object) continue;
        const po = plan_val.object;
        const intent = objField(po, "edit_intent");
        const template = (if (intent) |i| strField(i, "template") else null) orelse continue;
        const kind = strField(po, "kind") orelse "repair";
        const line = lineFromTarget(po);
        shown += 1;
        try w.print("  {d}. line {d} [{s}]: {s}\n", .{ shown, line, kind, template });
    }
    if (shown == 0) return null;

    if (smallestWitness(obj)) |wit| {
        try w.writeAll("FAILING INPUT (smallest counterexample):\n");
        try writeWitness(w, wit);
    }

    buf = aw.toArrayList();
    return try buf.toOwnedSlice(allocator);
}

fn objField(obj: std.json.ObjectMap, key: []const u8) ?std.json.ObjectMap {
    const v = obj.get(key) orelse return null;
    return if (v == .object) v.object else null;
}

fn strField(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const v = obj.get(key) orelse return null;
    return if (v == .string) v.string else null;
}

fn boolField(obj: std.json.ObjectMap, key: []const u8) bool {
    const v = obj.get(key) orelse return false;
    return if (v == .bool) v.bool else false;
}

fn lineFromTarget(plan: std.json.ObjectMap) i64 {
    const target = objField(plan, "target") orelse return 0;
    const v = target.get("line") orelse return 0;
    return if (v == .integer) v.integer else 0;
}

fn ioStubCount(wit: std.json.ObjectMap) usize {
    const v = wit.get("io_stubs") orelse return 0;
    return if (v == .array) v.array.items.len else 0;
}

/// Pick the witness with the fewest io stubs (the simplest failing path). Ties
/// resolve to the first such witness, which keeps selection deterministic.
fn smallestWitness(obj: std.json.ObjectMap) ?std.json.ObjectMap {
    const v = obj.get("witnesses") orelse return null;
    if (v != .array) return null;
    var best: ?std.json.ObjectMap = null;
    var best_len: usize = std.math.maxInt(usize);
    for (v.array.items) |item| {
        if (item != .object) continue;
        const len = ioStubCount(item.object);
        if (len < best_len) {
            best_len = len;
            best = item.object;
        }
    }
    return best;
}

fn writeWitness(w: *std.Io.Writer, wit: std.json.ObjectMap) !void {
    const req = objField(wit, "request");
    const method = (if (req) |r| strField(r, "method") else null) orelse "GET";
    const url = (if (req) |r| strField(r, "url") else null) orelse "/";
    const auth = if (req) |r| boolField(r, "has_auth_header") else false;
    const summary = strField(wit, "summary") orelse "";
    try w.print("  {s} {s} (auth: {s}): {s}\n", .{ method, url, if (auth) "yes" else "no", summary });

    const stubs_v = wit.get("io_stubs") orelse return;
    if (stubs_v != .array) return;
    var shown: usize = 0;
    for (stubs_v.array.items) |stub_v| {
        if (shown >= max_witness_stubs) break;
        if (stub_v != .object) continue;
        const so = stub_v.object;
        const module = strField(so, "module") orelse "";
        const func = strField(so, "fn") orelse "";
        shown += 1;
        try w.print("   io: {s}.{s} -> ", .{ module, func });
        if (so.get("result")) |rv| {
            try std.json.Stringify.value(rv, .{}, w);
        } else {
            try w.writeAll("?");
        }
        try w.writeByte('\n');
    }
}

const testing = std.testing;

test "shouldRunLane gates on sql failure and new violations" {
    try testing.expect(shouldRunLane(false, 1));
    try testing.expect(!shouldRunLane(true, 1)); // sql has its own path
    try testing.expect(!shouldRunLane(false, 0)); // nothing new to repair
}

test "buildRetryBlock surfaces compiler templates and smallest witness" {
    const plan_json =
        \\{"ok":false,"plans":[
        \\{"id":"rp_001","kind":"check_result_before_value","target":{"line":5,"column":3},
        \\"edit_intent":{"kind":"insert_guard_before_line","line":5,"column":3,
        \\"template":"if (!result.ok) return Response.json({ error: result.error }, { status: 400 });"}}
        \\],"witnesses":[
        \\{"id":"wit_001","property":"injection_safe","summary":"tainted body reaches sql",
        \\"request":{"method":"POST","url":"/items","has_auth_header":false},
        \\"io_stubs":[{"seq":0,"module":"zigttp:sql","fn":"sqlExec","result":{"rowsAffected":1}}]}
        \\]}
    ;
    const block = (try buildRetryBlock(testing.allocator, plan_json)) orelse return error.ExpectedBlock;
    defer testing.allocator.free(block);
    try testing.expect(std.mem.indexOf(u8, block, "COMPILER-AUTHORED FIX") != null);
    try testing.expect(std.mem.indexOf(u8, block, "line 5 [check_result_before_value]") != null);
    try testing.expect(std.mem.indexOf(u8, block, "if (!result.ok)") != null);
    try testing.expect(std.mem.indexOf(u8, block, "FAILING INPUT") != null);
    try testing.expect(std.mem.indexOf(u8, block, "POST /items") != null);
    try testing.expect(std.mem.indexOf(u8, block, "zigttp:sql.sqlExec") != null);
}

test "buildRetryBlock returns null when there are no plans" {
    const empty = (try buildRetryBlock(testing.allocator, "{\"ok\":true,\"plans\":[]}"));
    try testing.expect(empty == null);
    const malformed = (try buildRetryBlock(testing.allocator, "not json"));
    try testing.expect(malformed == null);
}
