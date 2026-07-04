//! C-ABI helpers that compiled JIT code calls into.
//!
//! `jit/baseline.zig` declares each helper as `extern fn jitFoo(...)` and
//! captures its address with `&jitFoo`. The linker resolves the symbol by
//! its C export name, so it does not matter which Zig file defines it -
//! only that the symbol exists. root.zig keeps a side-effect
//! `_ = @import("interpreter/jit_intrinsics.zig");` to ensure this module
//! is part of the build graph.

const std = @import("std");
const value = @import("../value.zig");
const bytecode = @import("../bytecode.zig");
const context = @import("../context.zig");
const object = @import("../object.zig");
const builtins = @import("../builtins/root.zig");
const interpreter = @import("../interpreter.zig");
const Interpreter = interpreter.Interpreter;
const arith = @import("arith.zig");
const cmp = @import("cmp.zig");
const frame = @import("frame.zig");
const call = @import("call.zig");

/// Shared entry guard for the call intrinsics: when a fault is pending,
/// refuse to invoke the callee AND consume the operands the compiled code
/// pushed (args + callee, plus `this` for method calls), so the guarded
/// path's stack effect matches a normal call's. Without the pop, every
/// refused post-fault call would leave argc+1(+1) values behind and a
/// post-fault tail loop would ratchet sp upward.
fn refusePendingFaultCall(ctx: *context.Context, argc: u8, is_method: u8) bool {
    if (!ctx.hasException()) return false;
    const consumed = @as(usize, argc) + 1 + @as(usize, @intFromBool(is_method != 0));
    ctx.sp -= @min(ctx.sp, consumed);
    return true;
}

/// JIT helper: perform a call/call_method from compiled code.
/// Pops arguments and function from the context stack using interpreter logic,
/// then returns the result as a JSValue.
pub export fn jitCall(ctx: *context.Context, argc: u8, is_method: u8) value.JSValue {
    // A compiled frame keeps running tail opcodes after a faulting helper
    // (the fault is reconciled only at the executeCompiled boundary). Refuse
    // to invoke a callee while a fault is pending: the interpreter aborts at
    // the fault and never reaches this call, so running the callee here would
    // execute user code - with observable side effects - that the interpreter
    // tier never executes. Store-side twins: Context.jitPutGlobal,
    // jitPutFieldIC.
    if (refusePendingFaultCall(ctx, argc, is_method)) return value.JSValue.exception_val;
    const interp = interpreter.current_interpreter orelse {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    call.doCall(interp, argc, is_method != 0) catch {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    return ctx.pop();
}

/// JIT helper: perform a monomorphic bytecode call from compiled code.
/// Verifies the callee matches the expected bytecode function and then
/// dispatches directly to callBytecodeFunction. Falls back to doCall otherwise.
pub export fn jitCallBytecode(
    ctx: *context.Context,
    expected_bc: *const bytecode.FunctionBytecode,
    argc: u8,
    is_method: u8,
) value.JSValue {
    // Post-fault call guard; see jitCall.
    if (refusePendingFaultCall(ctx, argc, is_method)) return value.JSValue.exception_val;
    const interp = interpreter.current_interpreter orelse {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    const is_method_bool = is_method != 0;
    const sp = ctx.sp;
    const needed: usize = @as(usize, argc) + 1 + @as(usize, @intFromBool(is_method_bool));
    if (sp < needed) {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    }

    const func_idx = sp - 1 - @as(usize, argc);
    const func_val = ctx.stack[func_idx];

    if (!func_val.isCallable()) {
        call.doCall(interp, argc, is_method_bool) catch {
            ctx.throwException(value.JSValue.exception_val);
            return value.JSValue.exception_val;
        };
        return ctx.pop();
    }

    const func_obj = object.JSObject.fromValue(func_val);
    if (func_obj.flags.is_generator or func_obj.flags.is_async) {
        call.doCall(interp, argc, is_method_bool) catch {
            ctx.throwException(value.JSValue.exception_val);
            return value.JSValue.exception_val;
        };
        return ctx.pop();
    }

    const closure_data = func_obj.getClosureData();
    const func_bc_opt = if (closure_data) |cd|
        cd.bytecode
    else if (func_obj.getBytecodeFunctionData()) |bc_data|
        bc_data.bytecode
    else
        null;

    if (func_bc_opt == null or func_bc_opt.? != expected_bc) {
        call.doCall(interp, argc, is_method_bool) catch {
            ctx.throwException(value.JSValue.exception_val);
            return value.JSValue.exception_val;
        };
        return ctx.pop();
    }

    if (argc > call.MAX_STACK_ARGS) {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    }
    var args: [call.MAX_STACK_ARGS]value.JSValue = undefined;
    var i: usize = argc;
    while (i > 0) {
        i -= 1;
        args[i] = ctx.pop();
    }

    _ = ctx.pop();
    const this_val = if (is_method_bool) ctx.pop() else value.JSValue.undefined_val;

    const prev_closure = interp.current_closure;
    interp.current_closure = closure_data;
    defer interp.current_closure = prev_closure;

    const result = frame.callBytecodeFunction(interp, func_val, expected_bc, this_val, args[0..argc]) catch {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    return result;
}

/// JIT helper: fast path for monomorphic bytecode call when guards have already verified:
/// - The value is a pointer to an object
/// - The object is a bytecode function (not generator/async)
/// - The bytecode matches expected_bc
/// This skips redundant validation for maximum performance.
pub export fn jitCallBytecodeFast(
    ctx: *context.Context,
    expected_bc: *const bytecode.FunctionBytecode,
    argc: u8,
    is_method: u8,
) value.JSValue {
    // Post-fault call guard; see jitCall.
    if (refusePendingFaultCall(ctx, argc, is_method)) return value.JSValue.exception_val;
    const interp = interpreter.current_interpreter orelse {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    const is_method_bool = is_method != 0;
    const sp = ctx.sp;

    const needed: usize = @as(usize, argc) + 1 + @as(usize, @intFromBool(is_method_bool));
    if (sp < needed) {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    }

    const func_idx = sp - 1 - @as(usize, argc);
    const func_val = ctx.stack[func_idx];
    const func_obj = object.JSObject.fromValue(func_val);

    const closure_data = func_obj.getClosureData();

    if (argc > call.MAX_STACK_ARGS) {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    }
    var args: [call.MAX_STACK_ARGS]value.JSValue = undefined;
    var i: usize = argc;
    while (i > 0) {
        i -= 1;
        args[i] = ctx.pop();
    }

    _ = ctx.pop();
    const this_val = if (is_method_bool) ctx.pop() else value.JSValue.undefined_val;

    const prev_closure = interp.current_closure;
    interp.current_closure = closure_data;
    defer interp.current_closure = prev_closure;

    const result = frame.callBytecodeFunction(interp, func_val, expected_bc, this_val, args[0..argc]) catch {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    return result;
}

/// JIT helper: get_field_ic using the interpreter's PIC cache.
pub export fn jitGetFieldIC(ctx: *context.Context, obj_val: value.JSValue, atom_idx: u16, cache_idx: u16) value.JSValue {
    const interp = interpreter.current_interpreter orelse {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    const atom: object.Atom = @enumFromInt(atom_idx);
    if (obj_val.isObject()) {
        const obj = object.JSObject.fromValue(obj_val);
        const pic = &interp.pic_cache[cache_idx];

        if (pic.lookup(obj.hidden_class_idx)) |slot_offset| {
            interp.pic_hits +%= 1;
            return obj.getSlot(slot_offset);
        }

        interp.pic_misses +%= 1;
        const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
        if (pool.findProperty(obj.hidden_class_idx, atom)) |slot_offset| {
            interp.updatePic(pic, obj.hidden_class_idx, slot_offset);
            return obj.getSlot(slot_offset);
        }
        if (obj.getProperty(pool, atom)) |prop_val| {
            return prop_val;
        }
        return value.JSValue.undefined_val;
    } else if (obj_val.isAnyString()) {
        if (atom == .length) {
            return cmp.getAnyStringLength(obj_val);
        }
        if (ctx.string_prototype) |proto| {
            const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
            return proto.getProperty(pool, atom) orelse value.JSValue.undefined_val;
        }
        return value.JSValue.undefined_val;
    }

    return value.JSValue.undefined_val;
}

/// JIT helper: put_field_ic using the interpreter's PIC cache.
pub export fn jitPutFieldIC(ctx: *context.Context, obj_val: value.JSValue, atom_idx: u16, val: value.JSValue, cache_idx: u16) value.JSValue {
    // A compiled frame keeps running tail opcodes after a faulting call; refuse
    // the store while a fault is pending so the exception sentinel never lands
    // in a field of a module-scope object that outlives the request (see
    // Context.jitPutGlobal).
    if (ctx.hasException()) return value.JSValue.exception_val;
    const interp = interpreter.current_interpreter orelse {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    const atom: object.Atom = @enumFromInt(atom_idx);
    if (obj_val.isObject()) {
        const obj = object.JSObject.fromValue(obj_val);
        const pic = &interp.pic_cache[cache_idx];

        if (ctx.enforce_arena_escape and ctx.hybrid != null and !obj.flags.is_arena and ctx.isEphemeralValue(val)) {
            ctx.throwException(value.JSValue.exception_val);
            return value.JSValue.exception_val;
        }

        if (pic.lookup(obj.hidden_class_idx)) |slot_offset| {
            interp.pic_hits +%= 1;
            // Arena-escape already checked above; barrier the fast-path store.
            ctx.setSlotBarriered(obj, slot_offset, val) catch {
                ctx.throwException(value.JSValue.exception_val);
                return value.JSValue.exception_val;
            };
            return val;
        }

        interp.pic_misses +%= 1;
        const pool = ctx.hidden_class_pool orelse {
            ctx.setPropertyChecked(obj, atom, val) catch {
                ctx.throwException(value.JSValue.exception_val);
                return value.JSValue.exception_val;
            };
            return val;
        };

        if (pool.findProperty(obj.hidden_class_idx, atom)) |slot_offset| {
            interp.updatePic(pic, obj.hidden_class_idx, slot_offset);
            // Arena-escape already checked above; barrier the fast-path store.
            ctx.setSlotBarriered(obj, slot_offset, val) catch {
                ctx.throwException(value.JSValue.exception_val);
                return value.JSValue.exception_val;
            };
            return val;
        }

        ctx.setPropertyChecked(obj, atom, val) catch {
            ctx.throwException(value.JSValue.exception_val);
            return value.JSValue.exception_val;
        };
    }

    return val;
}

pub export fn jitMathFloor(ctx: *context.Context, arg: value.JSValue) value.JSValue {
    return builtins.mathFloor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{arg});
}

pub export fn jitMathCeil(ctx: *context.Context, arg: value.JSValue) value.JSValue {
    return builtins.mathCeil(ctx, value.JSValue.undefined_val, &[_]value.JSValue{arg});
}

pub export fn jitMathRound(ctx: *context.Context, arg: value.JSValue) value.JSValue {
    return builtins.mathRound(ctx, value.JSValue.undefined_val, &[_]value.JSValue{arg});
}

pub export fn jitMathAbs(ctx: *context.Context, arg: value.JSValue) value.JSValue {
    return builtins.mathAbs(ctx, value.JSValue.undefined_val, &[_]value.JSValue{arg});
}

pub export fn jitMathMin2(ctx: *context.Context, arg1: value.JSValue, arg2: value.JSValue) value.JSValue {
    return builtins.mathMin(ctx, value.JSValue.undefined_val, &[_]value.JSValue{ arg1, arg2 });
}

pub export fn jitMathMax2(ctx: *context.Context, arg1: value.JSValue, arg2: value.JSValue) value.JSValue {
    return builtins.mathMax(ctx, value.JSValue.undefined_val, &[_]value.JSValue{ arg1, arg2 });
}

/// JIT helper: concatenate N values from the stack into a single string.
/// Uses the interpreter's concatNValues which leverages concatMany for single-allocation.
pub export fn jitConcatN(ctx: *context.Context, count: u8) value.JSValue {
    const interp = interpreter.current_interpreter orelse {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    const result = arith.concatNValues(interp, count) catch {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    return result;
}

/// JIT helper: for_of_next iteration step.
/// Stack before: [iterable, index]
/// If not done: updates index to index+1, pushes element, returns 1.
/// If done: returns 0 (caller should jump to end_offset).
/// On error: returns 0 (treat as done).
pub export fn jitForOfNext(ctx: *context.Context) u64 {
    const sp = ctx.sp;
    if (sp < 2) return 0;

    const idx_val = ctx.stack[sp - 1];
    const iter_val = ctx.stack[sp - 2];

    if (iter_val.isObject() and idx_val.isInt()) {
        const obj = object.JSObject.fromValue(iter_val);
        const idx = idx_val.getInt();
        if (idx >= 0 and obj.class_id == .array) {
            const idx_u: u32 = @intCast(idx);
            const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.ARRAY_LENGTH].getInt());
            if (idx_u < len) {
                const element = obj.getIndexUnchecked(idx_u);
                ctx.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                ctx.stack[sp] = element;
                ctx.sp = sp + 1;
                return 1;
            }
        } else if (idx >= 0 and obj.class_id == .range_iterator) {
            const idx_u: u32 = @intCast(idx);
            const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.RANGE_LENGTH].getInt());
            if (idx_u < len) {
                const start = obj.inline_slots[object.JSObject.Slots.RANGE_START].getInt();
                const step = obj.inline_slots[object.JSObject.Slots.RANGE_STEP].getInt();
                const element = value.JSValue.fromInt(start + @as(i32, @intCast(idx_u)) * step);
                ctx.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                ctx.stack[sp] = element;
                ctx.sp = sp + 1;
                return 1;
            }
        }
    }
    return 0;
}

// Tripwire: every effectful intrinsic compiled code can reach must refuse to
// act while a fault is pending, or post-fault tail opcodes diverge from the
// interpreter (which aborts at the fault). When adding a new intrinsic that
// stores state or invokes user code, add it here.
test "effectful jit intrinsics refuse work while a fault is pending" {
    const testing = @import("std").testing;
    const gc = @import("../gc.zig");

    var gc_state = try gc.GC.init(testing.allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();
    var ctx = try context.Context.init(testing.allocator, &gc_state, .{});
    defer ctx.deinit();

    ctx.throwException(value.JSValue.exception_val);
    defer ctx.clearException();

    // Calls: refused at entry (no interpreter needed - the guard fires first),
    // and the guard consumes the operands a compiled call site pushed.
    try ctx.push(value.JSValue.fromInt(1)); // callee a compiled site pushed
    const sp_before = ctx.sp;
    try testing.expect(jitCall(ctx, 0, 0).isException());
    try testing.expectEqual(sp_before - 1, ctx.sp);

    var dummy_code = [_]u8{0}; // never executed
    var dummy = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 4,
        .flags = .{},
        .code = &dummy_code,
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    try ctx.push(value.JSValue.fromInt(1));
    try testing.expect(jitCallBytecode(ctx, &dummy, 0, 0).isException());
    try ctx.push(value.JSValue.fromInt(1));
    try testing.expect(jitCallBytecodeFast(ctx, &dummy, 0, 0).isException());

    // Stores: refused; no value lands.
    const atom = try ctx.atoms.intern("__fault_guard_probe");
    const atom_idx: u16 = @intCast(@intFromEnum(atom));
    try testing.expect(ctx.jitPutGlobal(atom_idx, value.JSValue.fromInt(7)).isException());
    try testing.expect(ctx.getGlobal(atom) == null);

    const obj = try ctx.createObject(null);
    defer obj.destroy(testing.allocator);
    try testing.expect(ctx.jitPutField(obj.toValue(), atom_idx, value.JSValue.fromInt(7)).isException());
    try testing.expect(ctx.jitSetSlot(obj.toValue(), 0, value.JSValue.fromInt(7)).isException());
    try testing.expect(ctx.jitPutElem(obj.toValue(), value.JSValue.fromInt(0), value.JSValue.fromInt(7)).isException());
    try testing.expect(jitPutFieldIC(ctx, obj.toValue(), atom_idx, value.JSValue.fromInt(7), 0).isException());
}

/// JIT helper: for_of_next_put_loc - same as forOfNext but stores to local.
/// Stack before: [iterable, index]
/// If not done: updates index to index+1, stores element to local, returns 1.
/// If done: returns 0 (caller should jump to end_offset).
pub export fn jitForOfNextPutLoc(ctx: *context.Context, local_idx: u8) u64 {
    const sp = ctx.sp;
    const fp = ctx.fp;
    if (sp < 2) return 0;

    const idx_val = ctx.stack[sp - 1];
    const iter_val = ctx.stack[sp - 2];

    if (iter_val.isObject() and idx_val.isInt()) {
        const obj = object.JSObject.fromValue(iter_val);
        const idx = idx_val.getInt();
        if (idx >= 0 and obj.class_id == .array) {
            const idx_u: u32 = @intCast(idx);
            const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.ARRAY_LENGTH].getInt());
            if (idx_u < len) {
                const element = obj.getIndexUnchecked(idx_u);
                ctx.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                ctx.stack[fp + local_idx] = element;
                return 1;
            }
        } else if (idx >= 0 and obj.class_id == .range_iterator) {
            const idx_u: u32 = @intCast(idx);
            const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.RANGE_LENGTH].getInt());
            if (idx_u < len) {
                const start = obj.inline_slots[object.JSObject.Slots.RANGE_START].getInt();
                const step = obj.inline_slots[object.JSObject.Slots.RANGE_STEP].getInt();
                const element = value.JSValue.fromInt(start + @as(i32, @intCast(idx_u)) * step);
                ctx.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                ctx.stack[fp + local_idx] = element;
                return 1;
            }
        }
    }
    return 0;
}
