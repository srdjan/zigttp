//! zigttp:scope - request-scoped lifecycle management
//!
//! Exports:
//!   scope(name: string, fn: () => T) -> T
//!   using(resource: T, closeFn: (resource: T) => void) -> T
//!   ensure(fn: () => void) -> undefined
//!
//! The handler request is wrapped in an implicit root scope by the runtime.
//! Nested scopes unwind in reverse registration order, and all stored cleanup
//! callbacks/resources are explicitly rooted in the GC while live.

const std = @import("std");
const context = @import("../context.zig");
const value = @import("../value.zig");
const util = @import("util.zig");
const mb = @import("../module_binding.zig");

pub const MODULE_STATE_SLOT = @intFromEnum(@import("../module_slots.zig").Slot.scope);

pub const ScopeCallbacks = struct {
    call0_fn: *const fn (*anyopaque, value.JSValue) anyerror!value.JSValue,
    call1_fn: *const fn (*anyopaque, value.JSValue, value.JSValue) anyerror!value.JSValue,
    runtime_ptr: *anyopaque,
    current_state: ?*ScopeState = null,

    pub fn deinitOpaque(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *ScopeCallbacks = @ptrCast(@alignCast(ptr));
        if (self.current_state) |state| {
            state.deinitDiscard();
            self.current_state = null;
        }
        allocator.destroy(self);
    }
};

const ScopeEntry = struct {
    close_fn: value.JSValue,
    resource_value: value.JSValue = value.JSValue.undefined_val,
    has_resource: bool = false,
};

const ScopeFrame = struct {
    parent: ?*ScopeFrame,
    entries: std.ArrayList(ScopeEntry) = .empty,
};

const ScopeState = struct {
    ctx: *context.Context,
    current_frame: ?*ScopeFrame = null,
    is_unwinding: bool = false,

    fn init(alloc: std.mem.Allocator, ctx: *context.Context) !*ScopeState {
        const self = try alloc.create(ScopeState);
        self.* = .{
            .ctx = ctx,
            .current_frame = null,
            .is_unwinding = false,
        };
        return self;
    }

    fn allocator(self: *ScopeState) std.mem.Allocator {
        return self.ctx.allocator;
    }

    fn pushFrame(self: *ScopeState) !*ScopeFrame {
        const frame = try self.allocator().create(ScopeFrame);
        frame.* = .{
            .parent = self.current_frame,
            .entries = .empty,
        };
        self.current_frame = frame;
        return frame;
    }

    fn appendEntry(
        self: *ScopeState,
        close_fn: value.JSValue,
        resource_value: value.JSValue,
        has_resource: bool,
    ) !void {
        const frame = self.current_frame orelse return error.NoActiveScope;
        try self.ctx.gc_state.addRoot(close_fn);
        errdefer self.ctx.gc_state.removeRoot(close_fn);

        if (has_resource) {
            try self.ctx.gc_state.addRoot(resource_value);
            errdefer self.ctx.gc_state.removeRoot(resource_value);
        }

        try frame.entries.append(self.allocator(), .{
            .close_fn = close_fn,
            .resource_value = resource_value,
            .has_resource = has_resource,
        });
    }

    fn unwindAndDestroyFrame(self: *ScopeState, callbacks: *ScopeCallbacks, frame: *ScopeFrame) void {
        self.is_unwinding = true;
        defer self.is_unwinding = false;

        var idx = frame.entries.items.len;
        while (idx > 0) : (idx -= 1) {
            const entry = frame.entries.items[idx - 1];
            self.runCleanup(callbacks, entry);
            self.ctx.gc_state.removeRoot(entry.close_fn);
            if (entry.has_resource) {
                self.ctx.gc_state.removeRoot(entry.resource_value);
            }
        }

        frame.entries.deinit(self.allocator());
        self.allocator().destroy(frame);
    }

    fn runCleanup(self: *ScopeState, callbacks: *ScopeCallbacks, entry: ScopeEntry) void {
        if (entry.has_resource) {
            _ = callbacks.call1_fn(callbacks.runtime_ptr, entry.close_fn, entry.resource_value) catch |err| {
                logCleanupFailure(self.ctx, err);
                return;
            };
        } else {
            _ = callbacks.call0_fn(callbacks.runtime_ptr, entry.close_fn) catch |err| {
                logCleanupFailure(self.ctx, err);
                return;
            };
        }
        if (self.ctx.hasException()) {
            std.log.warn("scope cleanup callback threw JS exception", .{});
            self.ctx.clearException();
        }
    }

    fn deinitDiscard(self: *ScopeState) void {
        var frame = self.current_frame;
        while (frame) |current| {
            frame = current.parent;
            for (current.entries.items) |entry| {
                self.ctx.gc_state.removeRoot(entry.close_fn);
                if (entry.has_resource) {
                    self.ctx.gc_state.removeRoot(entry.resource_value);
                }
            }
            current.entries.deinit(self.allocator());
            self.allocator().destroy(current);
        }
        self.allocator().destroy(self);
    }
};

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:scope",
    .name = "scope",
    .required_capabilities = &.{.runtime_callback},
    .stateful = true,
    .contract_section = "scope",
    .exports = &.{
        .{
            .name = "scope",
            .func = scopeNative,
            .arg_count = 2,
            .effect = .write,
            .returns = .unknown,
            .param_types = &.{ .string, .unknown },
            .traceable = false,
            .contract_extractions = &.{.{ .category = .scope_name }},
            .contract_flags = .{ .sets_scope_used = true },
        },
        .{
            .name = "using",
            .func = usingNative,
            .arg_count = 2,
            .effect = .write,
            .returns = .unknown,
            .param_types = &.{ .unknown, .unknown },
            .traceable = false,
            .contract_flags = .{ .sets_scope_used = true },
        },
        .{
            .name = "ensure",
            .func = ensureNative,
            .arg_count = 1,
            .effect = .write,
            .returns = .undefined,
            .param_types = &.{.unknown},
            .traceable = false,
            .contract_flags = .{ .sets_scope_used = true },
        },
    },
};

pub const exports = binding.toModuleExports();

pub fn beginRequest(ctx: *context.Context) !void {
    const callbacks = getCallbacksUnchecked(ctx) orelse return error.ScopeCallbacksUnavailable;
    if (callbacks.current_state != null) return error.ScopeStateAlreadyActive;

    const state = try ScopeState.init(ctx.allocator, ctx);
    errdefer state.deinitDiscard();
    _ = try state.pushFrame();
    callbacks.current_state = state;
}

pub fn endRequest(ctx: *context.Context) void {
    const callbacks = getCallbacksUnchecked(ctx) orelse return;
    const state = callbacks.current_state orelse return;

    while (state.current_frame) |frame| {
        state.current_frame = frame.parent;
        state.unwindAndDestroyFrame(callbacks, frame);
    }

    callbacks.current_state = null;
    state.allocator().destroy(state);
}

fn scopeNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "scope() requires request lifecycle support");
    };
    const state = callbacks.current_state orelse {
        return util.throwError(ctx, "Error", "scope() requires an active request scope");
    };

    if (state.is_unwinding) {
        return util.throwError(ctx, "Error", "scope() is not allowed during scope cleanup");
    }
    if (args.len < 2) {
        return util.throwError(ctx, "TypeError", "scope() expects a string name and function");
    }
    _ = util.extractString(args[0]) orelse {
        return util.throwError(ctx, "TypeError", "scope() name must be a string");
    };
    if (!args[1].isCallable()) {
        return util.throwError(ctx, "TypeError", "scope() expects a callable function");
    }

    const frame = try state.pushFrame();
    defer {
        state.current_frame = frame.parent;
        state.unwindAndDestroyFrame(callbacks, frame);
    }

    return callbacks.call0_fn(callbacks.runtime_ptr, args[1]);
}

fn usingNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "using() requires request lifecycle support");
    };
    const state = callbacks.current_state orelse {
        return util.throwError(ctx, "Error", "using() requires an active scope");
    };

    if (state.is_unwinding) {
        return util.throwError(ctx, "Error", "using() is not allowed during scope cleanup");
    }
    if (args.len < 2) {
        return util.throwError(ctx, "TypeError", "using() expects a resource and cleanup function");
    }
    if (args[0].isUndefined()) {
        return util.throwError(ctx, "TypeError", "using() resource must not be undefined");
    }
    if (!args[1].isCallable()) {
        return util.throwError(ctx, "TypeError", "using() expects a callable cleanup function");
    }

    try state.appendEntry(args[1], args[0], true);
    return args[0];
}

fn ensureNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "ensure() requires request lifecycle support");
    };
    const state = callbacks.current_state orelse {
        return util.throwError(ctx, "Error", "ensure() requires an active scope");
    };

    if (state.is_unwinding) {
        return util.throwError(ctx, "Error", "ensure() is not allowed during scope cleanup");
    }
    if (args.len < 1) {
        return util.throwError(ctx, "TypeError", "ensure() expects a cleanup function");
    }
    if (!args[0].isCallable()) {
        return util.throwError(ctx, "TypeError", "ensure() expects a callable cleanup function");
    }

    try state.appendEntry(args[0], value.JSValue.undefined_val, false);
    return value.JSValue.undefined_val;
}

fn getCallbacks(ctx: *context.Context) ?*ScopeCallbacks {
    return mb.getRuntimeCallbackStateChecked(ctx, ScopeCallbacks, MODULE_STATE_SLOT);
}

fn getCallbacksUnchecked(ctx: *context.Context) ?*ScopeCallbacks {
    return ctx.getModuleState(ScopeCallbacks, MODULE_STATE_SLOT);
}

fn logCleanupFailure(ctx: *context.Context, err: anyerror) void {
    std.log.warn("scope cleanup callback failed: {}", .{err});
    if (ctx.hasException()) {
        ctx.clearException();
    }
}

test "scope begin/end request initializes and clears root scope" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("../gc.zig");
    const heap_mod = @import("../heap.zig");

    const gc_state = try allocator.create(gc_mod.GC);
    gc_state.* = try gc_mod.GC.init(allocator, .{});
    const heap_state = try allocator.create(heap_mod.Heap);
    heap_state.* = heap_mod.Heap.init(allocator, .{});
    gc_state.setHeap(heap_state);
    const ctx = try context.Context.init(allocator, gc_state, .{});
    defer {
        ctx.deinit();
        heap_state.deinit();
        gc_state.deinit();
        allocator.destroy(heap_state);
        allocator.destroy(gc_state);
    }

    const callbacks = try allocator.create(ScopeCallbacks);
    callbacks.* = .{
        .call0_fn = struct {
            fn call(_: *anyopaque, _: value.JSValue) anyerror!value.JSValue {
                return value.JSValue.undefined_val;
            }
        }.call,
        .call1_fn = struct {
            fn call(_: *anyopaque, _: value.JSValue, arg: value.JSValue) anyerror!value.JSValue {
                return arg;
            }
        }.call,
        .runtime_ptr = @ptrFromInt(0x1),
        .current_state = null,
    };
    ctx.setModuleState(MODULE_STATE_SLOT, @ptrCast(callbacks), &ScopeCallbacks.deinitOpaque);

    try beginRequest(ctx);
    try std.testing.expect(callbacks.current_state != null);
    try std.testing.expect(callbacks.current_state.?.current_frame != null);

    endRequest(ctx);
    try std.testing.expect(callbacks.current_state == null);
}
