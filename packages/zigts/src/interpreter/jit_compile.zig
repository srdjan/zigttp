//! JIT tier-promotion and type-feedback lifecycle helpers.
//!
//! These ran as methods on `Interpreter` until the Slice B split. They live
//! here as free functions taking `*Interpreter` so the dispatch-heavy
//! interpreter.zig stays focused on the dispatch loop. All `Interpreter`
//! field accesses (`pic_cache`, `tier_promotions`, `current_func`, ...)
//! work because struct fields are pub by default in Zig.
//!
//! The hot-path recorders (recordBinaryOpFeedback / recordPropertyFeedback /
//! recordCallSiteFeedback) stay `pub inline fn` so call sites in dispatch
//! still inline.

const std = @import("std");
const compat = @import("../compat.zig");
const value = @import("../value.zig");
const bytecode = @import("../bytecode.zig");
const context = @import("../context.zig");
const type_feedback = @import("../type_feedback.zig");
const jit = @import("../jit/root.zig");
const interpreter = @import("../interpreter.zig");
const Interpreter = interpreter.Interpreter;

pub fn setTier(interp: *Interpreter, func: *bytecode.FunctionBytecode, new_tier: bytecode.CompilationTier) void {
    const old_tier = func.tier;
    if (old_tier == new_tier) return;
    func.tier = new_tier;
    if (@intFromEnum(new_tier) > @intFromEnum(old_tier)) {
        interp.tier_promotions[@intFromEnum(new_tier)] +%= 1;
    }
}

/// Try to compile a function using the baseline JIT compiler.
/// On success, stores compiled code in func.compiled_code and sets tier to .baseline.
/// On UnsupportedOpcode, marks the function as interpreted (won't retry).
/// Other errors are propagated.
pub fn tryCompileBaseline(interp: *Interpreter, func: *bytecode.FunctionBytecode) !void {
    const code_alloc = try interp.ctx.getOrCreateCodeAllocator();

    var timer: ?compat.Timer = null;
    if (context.enable_jit_metrics) {
        timer = compat.Timer.start() catch null;
    }

    const compiled = jit.compileFunction(interp.ctx.allocator, code_alloc, func, interp.ctx.hidden_class_pool) catch |err| {
        switch (err) {
            jit.CompileError.UnsupportedOpcode => {
                setTier(interp, func, .interpreted);
                interp.ctx.recordJitFailure();
                return;
            },
            else => return err,
        }
    };

    if (timer) |*t| {
        interp.ctx.recordJitCompile(t.read(), compiled.code.len, func.code.len);
    }

    const compiled_ptr = try interp.ctx.allocator.create(jit.CompiledCode);
    compiled_ptr.* = compiled;
    func.compiled_code = compiled_ptr;
    setTier(interp, func, .baseline);
}

/// Try to compile a function using the optimized JIT tier.
/// On success, stores compiled code in func.compiled_code and sets tier to .optimized.
/// On UnsupportedOpcode (no optimizable loops), stays at baseline tier.
pub fn tryCompileOptimized(interp: *Interpreter, func: *bytecode.FunctionBytecode) !void {
    const code_alloc = try interp.ctx.getOrCreateCodeAllocator();

    var timer: ?compat.Timer = null;
    if (context.enable_jit_metrics) {
        timer = compat.Timer.start() catch null;
    }

    const compiled = jit.compileOptimized(interp.ctx.allocator, code_alloc, func, interp.ctx.hidden_class_pool) catch |err| {
        switch (err) {
            jit.CompileError.UnsupportedOpcode => {
                setTier(interp, func, .baseline);
                return;
            },
            else => return err,
        }
    };

    if (timer) |*t| {
        interp.ctx.recordJitCompile(t.read(), compiled.code.len, func.code.len);
    }

    if (func.compiled_code) |old_ptr| {
        const old_code: *jit.CompiledCode = @ptrCast(@alignCast(old_ptr));
        interp.ctx.allocator.destroy(old_code);
    }

    const compiled_ptr = try interp.ctx.allocator.create(jit.CompiledCode);
    compiled_ptr.* = compiled;
    func.compiled_code = compiled_ptr;
    setTier(interp, func, .optimized);
}

/// Allocate type feedback vector for a function.
/// Scans bytecode to count and map feedback sites.
///
/// CRITICAL: This function performs heap allocations that may trigger
/// arena growth. It MUST only be called at safe boundaries when no
/// active stack frames rely on stable memory addresses.
pub fn allocateTypeFeedback(interp: *Interpreter, func: *bytecode.FunctionBytecode) !void {
    if (func.type_feedback_ptr != null) return;

    var binary_op_count: u32 = 0;
    var call_site_count: u32 = 0;
    var pc: usize = 0;

    while (pc < func.code.len) {
        const op: bytecode.Opcode = @enumFromInt(func.code[pc]);
        pc += 1;

        switch (op) {
            .add, .sub, .mul, .div, .mod => {
                binary_op_count += 2;
            },
            .get_field_ic, .put_field_ic => {
                binary_op_count += 1;
                pc += 4;
            },
            .call, .call_method => {
                call_site_count += 1;
                pc += 1;
            },
            .call_ic => {
                call_site_count += 1;
                pc += 3;
            },
            .push_const_call => {
                call_site_count += 1;
                pc += 3;
            },
            .get_field_call => {
                call_site_count += 1;
                pc += 3;
            },
            else => {
                pc += bytecode.getOpcodeInfo(op).size - 1;
            },
        }
    }

    if (binary_op_count == 0 and call_site_count == 0) return;

    const tf = try type_feedback.TypeFeedback.init(
        interp.ctx.allocator,
        binary_op_count,
        call_site_count,
    );
    errdefer tf.deinit();

    const site_map = try interp.ctx.allocator.alloc(u16, func.code.len);
    errdefer interp.ctx.allocator.free(site_map);
    @memset(site_map, 0xFFFF);

    var site_idx: u16 = 0;
    var call_idx: u16 = 0;
    pc = 0;

    while (pc < func.code.len) {
        const op: bytecode.Opcode = @enumFromInt(func.code[pc]);
        const op_offset = pc;
        pc += 1;

        switch (op) {
            .add, .sub, .mul, .div, .mod => {
                site_map[op_offset] = site_idx;
                site_idx += 2;
            },
            .get_field_ic, .put_field_ic => {
                site_map[op_offset] = site_idx;
                site_idx += 1;
                pc += 4;
            },
            .call, .call_method => {
                site_map[op_offset] = call_idx | 0x8000;
                call_idx += 1;
                pc += 1;
            },
            .call_ic => {
                site_map[op_offset] = call_idx | 0x8000;
                call_idx += 1;
                pc += 3;
            },
            .push_const_call => {
                site_map[op_offset] = call_idx | 0x8000;
                call_idx += 1;
                pc += 3;
            },
            .get_field_call => {
                site_map[op_offset] = call_idx | 0x8000;
                call_idx += 1;
                pc += 3;
            },
            else => {
                pc += bytecode.getOpcodeInfo(op).size - 1;
            },
        }
    }

    func.type_feedback_ptr = tf;
    func.feedback_site_map = site_map;
}

/// Allocate type feedback for a function after it completes execution.
/// This ensures stack and register state remain stable during execution.
pub fn allocateTypeFeedbackDeferred(interp: *Interpreter, func: *bytecode.FunctionBytecode) !void {
    if (func.type_feedback_ptr != null) return;
    if (func.tier != .baseline_candidate) return;
    if (func.execution_count < 3) return;

    try allocateTypeFeedback(interp, func);
}

/// Record type feedback for a binary operation.
/// Called from dispatch loop when type_feedback is allocated.
pub inline fn recordBinaryOpFeedback(interp: *Interpreter, a: value.JSValue, b: value.JSValue) void {
    const func = interp.current_func orelse return;
    const tf = func.type_feedback_ptr orelse return;
    const site_map = func.feedback_site_map orelse return;

    const bc_offset = @intFromPtr(interp.pc) - @intFromPtr(func.code.ptr) - 1;
    if (bc_offset >= site_map.len) return;

    const site_idx = site_map[bc_offset];
    if (site_idx == 0xFFFF) return;

    if (site_idx + 1 < tf.sites.len) {
        tf.sites[site_idx].record(a);
        tf.sites[site_idx + 1].record(b);
    }
}

/// Record type feedback for property access.
pub inline fn recordPropertyFeedback(interp: *Interpreter, obj: value.JSValue) void {
    const func = interp.current_func orelse return;
    const tf = func.type_feedback_ptr orelse return;
    const site_map = func.feedback_site_map orelse return;

    // For get_field_ic: opcode (1) + atom_idx (2) + cache_idx (2) = 5 bytes total.
    const bc_offset = @intFromPtr(interp.pc) - @intFromPtr(func.code.ptr) - 5;
    if (bc_offset >= site_map.len) return;

    const site_idx = site_map[bc_offset];
    if (site_idx == 0xFFFF) return;

    if (site_idx < tf.sites.len) {
        tf.sites[site_idx].record(obj);
    }
}

/// Record call site feedback for function inlining decisions.
/// Records which function is being called at this call site.
pub inline fn recordCallSiteFeedback(interp: *Interpreter, callee_bc: ?*const bytecode.FunctionBytecode) void {
    const func = interp.current_func orelse return;
    const tf = func.type_feedback_ptr orelse return;
    const site_map = func.feedback_site_map orelse return;

    const pc_offset = interp.call_opcode_offset;
    const func_code_start = @intFromPtr(func.code.ptr);
    const pc_addr = @intFromPtr(interp.pc);
    if (pc_addr < func_code_start + pc_offset) return;
    const bc_offset = pc_addr - func_code_start - pc_offset;
    if (bc_offset >= site_map.len) return;

    const site_idx = site_map[bc_offset];
    if ((site_idx & 0x8000) == 0) return;
    const call_idx = site_idx & 0x7FFF;

    if (call_idx < tf.call_sites.len) {
        tf.call_sites[call_idx].recordCallee(callee_bc);
    }
}

pub fn cleanupTypeFeedback(allocator: std.mem.Allocator, func: *bytecode.FunctionBytecode) void {
    if (func.type_feedback_ptr) |tf| {
        tf.deinit();
        func.type_feedback_ptr = null;
    }
    if (func.feedback_site_map) |site_map| {
        allocator.free(site_map);
        func.feedback_site_map = null;
    }
}

pub fn cleanupCompiledCode(allocator: std.mem.Allocator, func: *bytecode.FunctionBytecode) void {
    if (func.compiled_code) |cc| {
        const compiled: *jit.CompiledCode = @ptrCast(@alignCast(cc));
        allocator.destroy(compiled);
        func.compiled_code = null;
    }
}
