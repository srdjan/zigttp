//! Function-call dispatch: native vs bytecode vs generator vs async.

const std = @import("std");
const value = @import("../value.zig");
const object = @import("../object.zig");
const builtins = @import("../builtins/root.zig");
const jit_compile = @import("jit_compile.zig");
const trace = @import("trace.zig");
const frame = @import("frame.zig");
const interpreter = @import("../interpreter.zig");
const Interpreter = interpreter.Interpreter;
const InterpreterError = Interpreter.InterpreterError;

/// Stack-allocated argument cap. argc is u8 so the runtime ceiling is 255.
pub const MAX_STACK_ARGS = 256;

/// Perform a function call, dispatching to native, bytecode, generator, or
/// async based on the callable's flags.
///
/// Stack on entry:
///   Regular: [func, arg0, ..., argN-1]
///   Method:  [obj, func, arg0, ..., argN-1]   (obj becomes `this`)
pub fn doCall(self: *Interpreter, argc: u8, is_method: bool) InterpreterError!void {
    if (argc > MAX_STACK_ARGS) {
        return error.TooManyArguments;
    }
    trace.traceCall(self, "enter", argc, is_method);
    defer trace.traceCall(self, "exit", argc, is_method);
    const guard = trace.callGuardDepth();
    if (guard != 0 and self.ctx.call_depth >= guard) {
        trace.traceCall(self, "guard", argc, is_method);
        return error.CallStackOverflow;
    }

    // Collect arguments (in reverse order from stack)
    var args: [MAX_STACK_ARGS]value.JSValue = undefined;
    var i: usize = argc;
    while (i > 0) {
        i -= 1;
        args[i] = self.ctx.pop();
    }

    const func_val = self.ctx.pop();
    const this_val = if (is_method) self.ctx.pop() else value.JSValue.undefined_val;

    if (!func_val.isCallable()) {
        if (trace.callTraceEnabled()) {
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
                const pc_off = @as(usize, @intCast(@intFromPtr(self.pc) - @intFromPtr(cur.code.ptr)));
                std.debug.print("[call] not-callable pc_off={} func_locals={}\n", .{ pc_off, cur.local_count });
            }
        }
        return error.NotCallable;
    }

    const func_obj = object.JSObject.fromValue(func_val);

    // Native function fast dispatch
    if (func_obj.getNativeFunctionData()) |native_data| {
        // Hot builtins bypass the wrapper to keep the call site monomorphic.
        const result: value.JSValue = switch (native_data.builtin_id) {
            .json_parse => builtins.jsonParse(self.ctx, this_val, args[0..argc]),
            .json_stringify => builtins.jsonStringify(self.ctx, this_val, args[0..argc]),
            .string_index_of => builtins.stringIndexOf(self.ctx, this_val, args[0..argc]),
            .string_slice => builtins.stringSlice(self.ctx, this_val, args[0..argc]),
            .math_floor => builtins.mathFloor(self.ctx, this_val, args[0..argc]),
            .math_ceil => builtins.mathCeil(self.ctx, this_val, args[0..argc]),
            .math_round => builtins.mathRound(self.ctx, this_val, args[0..argc]),
            .math_abs => builtins.mathAbs(self.ctx, this_val, args[0..argc]),
            .math_min => builtins.mathMin(self.ctx, this_val, args[0..argc]),
            .math_max => builtins.mathMax(self.ctx, this_val, args[0..argc]),
            .parse_int => builtins.numberParseInt(self.ctx, this_val, args[0..argc]),
            .parse_float => builtins.numberParseFloat(self.ctx, this_val, args[0..argc]),
            .none => blk: {
                break :blk native_data.func(self.ctx, this_val, args[0..argc]) catch |err| {
                    if (err == error.DurableSuspended) return error.DurableSuspended;
                    const func_name = if (native_data.name.toPredefinedName()) |name| name else "<native>";
                    std.log.err("Native function '{s}' error: {}", .{ func_name, err });
                    self.ctx.throwException(value.JSValue.exception_val);
                    return error.NativeFunctionError;
                };
            },
        };
        if (self.ctx.hasException()) {
            return error.NativeFunctionError;
        }
        try self.ctx.push(result);
        return;
    }

    // Closure or bytecode function
    const closure_data = func_obj.getClosureData();
    const func_bc_opt = if (closure_data) |cd|
        cd.bytecode
    else if (func_obj.getBytecodeFunctionData()) |bc_data|
        bc_data.bytecode
    else
        null;

    jit_compile.recordCallSiteFeedback(self, func_bc_opt);

    if (func_bc_opt) |func_bc| {
        const prev_closure = self.current_closure;
        self.current_closure = closure_data;
        defer self.current_closure = prev_closure;

        if (func_obj.flags.is_generator) {
            // Materialize a generator object instead of executing.
            const root_class_idx = self.ctx.root_class_idx;
            const gen_obj = if (self.ctx.hybrid) |h|
                (object.JSObject.createGeneratorWithArena(
                    h.arena,
                    root_class_idx,
                    func_bc,
                    self.ctx.generator_prototype,
                ) orelse return error.OutOfMemory)
            else
                (object.JSObject.createGenerator(
                    self.ctx.allocator,
                    root_class_idx,
                    func_bc,
                    self.ctx.generator_prototype,
                ) catch return error.OutOfMemory);

            const gen_data = gen_obj.getGeneratorData().?;
            var local_idx: usize = 0;
            while (local_idx < gen_data.locals.len) : (local_idx += 1) {
                if (local_idx < argc) {
                    gen_data.locals[local_idx] = args[local_idx];
                }
            }

            try self.ctx.push(gen_obj.toValue());
            return;
        }

        if (func_obj.flags.is_async) {
            // Simplified async: execute synchronously, return a resolved
            // Promise-like {value, then}. Full async support requires an
            // event loop and proper Promise integration.
            const result = try frame.callBytecodeFunction(self, func_val, func_bc, this_val, args[0..argc]);

            const promise_obj = self.ctx.createObject(null) catch {
                return error.OutOfMemory;
            };

            const value_atom = self.ctx.atoms.intern("value") catch return error.OutOfMemory;
            self.ctx.setPropertyChecked(promise_obj, value_atom, result) catch |err| {
                return switch (err) {
                    error.ArenaObjectEscape => error.ArenaObjectEscape,
                    else => error.OutOfMemory,
                };
            };

            const then_atom = self.ctx.atoms.intern("then") catch return error.OutOfMemory;
            self.ctx.setPropertyChecked(promise_obj, then_atom, value.JSValue.true_val) catch |err| {
                return switch (err) {
                    error.ArenaObjectEscape => error.ArenaObjectEscape,
                    else => error.OutOfMemory,
                };
            };

            try self.ctx.push(promise_obj.toValue());
            return;
        }

        const result = try frame.callBytecodeFunction(self, func_val, func_bc, this_val, args[0..argc]);
        try self.ctx.push(result);
        return;
    }

    // Unknown callable shape -- preserve historical behavior of pushing undefined.
    try self.ctx.push(value.JSValue.undefined_val);
}
