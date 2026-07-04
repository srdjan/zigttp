//! Diagnostic call/type tracing for the interpreter.
//!
//! Env vars: `ZTS_TRACE_CALLS` (master switch), `ZTS_TRACE_CALLS_LIMIT`
//! (per-thread call-event budget), `ZTS_CALL_GUARD` (max call depth before
//! guard logging), `ZTS_TRACE_BC_FULL` (dump full bytecode window instead
//! of a +/-12 op slice). `call_trace_count` is threadlocal so per-worker
//! budgets don't bleed across pooled handler runs.

const std = @import("std");
const value = @import("../value.zig");
const bytecode = @import("../bytecode.zig");
const object = @import("../object.zig");
const env_cache = @import("env_cache.zig");
const interpreter = @import("../interpreter.zig");
const Interpreter = interpreter.Interpreter;

var call_trace_cache: ?bool = null;
var call_trace_limit_cache: ?usize = null;
var call_guard_cache: ?usize = null;
var trace_bc_full_cache: ?bool = null;
threadlocal var call_trace_count: usize = 0;

pub inline fn callTraceEnabled() bool {
    return env_cache.cachedBoolPresent("ZTS_TRACE_CALLS", &call_trace_cache);
}

pub fn callTraceLimit() usize {
    return env_cache.cachedUintNonzero(usize, "ZTS_TRACE_CALLS_LIMIT", &call_trace_limit_cache, 200);
}

pub fn callGuardDepth() usize {
    return env_cache.cachedUint(usize, "ZTS_CALL_GUARD", &call_guard_cache, 0);
}

pub fn traceCall(self: *Interpreter, label: []const u8, argc: u8, is_method: bool) void {
    if (!callTraceEnabled()) return;
    const limit = callTraceLimit();
    if (call_trace_count >= limit) return;
    call_trace_count += 1;
    std.debug.print(
        "[call] {s} depth={} sp={} fp={} argc={} method={}\n",
        .{ label, self.ctx.call_depth, self.ctx.sp, self.ctx.fp, argc, @intFromBool(is_method) },
    );
}

fn pcOffset(self: *const Interpreter, cur: *const bytecode.FunctionBytecode) usize {
    return @intCast(@intFromPtr(self.pc) - @intFromPtr(cur.code.ptr));
}

pub fn sourceLocationForOffset(func: *const bytecode.FunctionBytecode, offset: usize) ?bytecode.LineEntry {
    const entries = func.line_table orelse return null;
    if (entries.len == 0) return null;

    var lo: usize = 0;
    var hi: usize = entries.len;
    while (lo < hi) {
        const mid = lo + (hi - lo) / 2;
        if (entries[mid].offset <= offset) {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    if (lo == 0) return null;
    return entries[lo - 1];
}

pub fn lastOpSourceLocation(self: *const Interpreter) ?bytecode.LineEntry {
    const cur = self.current_func orelse return null;
    const pc_off = pcOffset(self, cur);
    const op_off = if (pc_off > 0) pc_off - 1 else 0;
    return sourceLocationForOffset(cur, op_off);
}

pub fn traceTypeError(self: *Interpreter, label: []const u8, a: value.JSValue, b: value.JSValue) void {
    if (!callTraceEnabled()) return;
    std.debug.print(
        "[typeerror] {s} a_type={s} a={} b_type={s} b={} depth={} sp={} fp={}\n",
        .{ label, a.typeOf(), a, b.typeOf(), b, self.ctx.call_depth, self.ctx.sp, self.ctx.fp },
    );
    if (self.current_func) |cur| {
        const pc_off = pcOffset(self, cur);
        std.debug.print("[typeerror] pc_off={} last_op={s}\n", .{ pc_off, @tagName(self.last_op) });
        const op_off = if (pc_off > 0) pc_off - 1 else 0;
        traceBytecodeWindow(self, op_off);
    }
}

pub fn traceLastOp(self: *Interpreter, label: []const u8) void {
    if (self.last_error_location == null) {
        self.last_error_location = lastOpSourceLocation(self);
    }
    if (!callTraceEnabled()) return;
    if (self.current_func) |cur| {
        const pc_off = pcOffset(self, cur);
        if (lastOpSourceLocation(self)) |loc| {
            std.debug.print(
                "[typeerror] {s} op={s} pc_off={} source={}:{} depth={} sp={} fp={}\n",
                .{ label, @tagName(self.last_op), pc_off, loc.line, loc.column, self.ctx.call_depth, self.ctx.sp, self.ctx.fp },
            );
        } else {
            std.debug.print(
                "[typeerror] {s} op={s} pc_off={} depth={} sp={} fp={}\n",
                .{ label, @tagName(self.last_op), pc_off, self.ctx.call_depth, self.ctx.sp, self.ctx.fp },
            );
        }
        const op_off = if (pc_off > 0) pc_off - 1 else 0;
        traceBytecodeWindow(self, op_off);
    } else {
        std.debug.print(
            "[typeerror] {s} op={s} depth={} sp={} fp={}\n",
            .{ label, @tagName(self.last_op), self.ctx.call_depth, self.ctx.sp, self.ctx.fp },
        );
    }
}

pub fn traceNotCallable(self: *Interpreter, func_val: value.JSValue, this_val: value.JSValue) void {
    if (!callTraceEnabled()) return;
    std.debug.print(
        "[call] not-callable type={s} func={} this={} depth={} sp={} fp={}\n",
        .{ func_val.typeOf(), func_val, this_val, self.ctx.call_depth, self.ctx.sp, self.ctx.fp },
    );
    if (func_val.isObject()) {
        const obj = object.JSObject.fromValue(func_val);
        std.debug.print(
            "[call] not-callable object class={} callable={} generator={} async={}\n",
            .{
                @intFromEnum(obj.class_id),
                @intFromBool(obj.flags.is_callable),
                @intFromBool(obj.flags.is_generator),
                @intFromBool(obj.flags.is_async),
            },
        );
    }
    if (self.current_func) |cur| {
        std.debug.print("[call] not-callable pc_off={} func_locals={}\n", .{ pcOffset(self, cur), cur.local_count });
    }
}

pub fn traceBytecodeWindow(self: *Interpreter, center_off: usize) void {
    if (!callTraceEnabled()) return;
    const cur = self.current_func orelse return;
    const code = cur.code;
    if (code.len == 0) return;
    const full = env_cache.cachedBoolPresent("ZTS_TRACE_BC_FULL", &trace_bc_full_cache);
    const window: usize = 12;
    const start = if (full) 0 else if (center_off > window) center_off - window else 0;
    const end = if (full) code.len else @min(code.len, center_off + window);
    var pos: usize = start;
    while (pos < end) {
        const op: bytecode.Opcode = @enumFromInt(code[pos]);
        const info = bytecode.getOpcodeInfo(op);
        std.debug.print("[bytecode] +{} {s}\n", .{ pos, @tagName(op) });
        if (info.size == 0) break;
        pos += info.size;
    }
}

test "interpreter TypeError records source line" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("../gc.zig");
    const heap_mod = @import("../heap.zig");
    const context_mod = @import("../context.zig");
    const parser_mod = @import("../parser/root.zig");
    const string_mod = @import("../string.zig");

    const prev_policy = interpreter.getJitPolicy();
    defer interpreter.setJitPolicy(prev_policy);
    interpreter.setJitPolicy(.disabled);

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var heap_state = heap_mod.Heap.init(allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context_mod.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var strings = string_mod.StringTable.init(allocator);
    defer strings.deinit();

    const source =
        \\function handler(req) {
        \\  const u = undefined;
        \\  return u + 1;
        \\}
        \\handler(undefined);
    ;

    var p = parser_mod.Parser.init(allocator, source, &strings, &ctx.atoms);
    defer p.deinit();

    const code = try p.parse();
    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
        .line_table = p.getLineTable(),
    };

    var interp = Interpreter.init(ctx);
    try std.testing.expectError(error.TypeError, interp.run(&func));

    const loc = interp.last_error_location orelse return error.MissingSourceLocation;
    try std.testing.expectEqual(@as(u32, 3), loc.line);
}
