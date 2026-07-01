//! Durable workflow executor: native implementations behind the
//! `zigttp:durable` module exports (run / step / stepWithTimeout / sleepUntil /
//! waitSignal / signal / signalAt) plus the oplog persistence and recovery
//! helpers. Extracted from zruntime.zig to keep the request-lifecycle struct
//! focused.
//!
//! These operate on the owning Runtime, passed explicitly as `rt`. The
//! `active_durable_run` / `pending_durable_recovery` state fields and the
//! `ActiveDurableRun` / `PendingDurableRecovery` / `PendingDurableWait` types
//! stay on Runtime in zruntime.zig (pub there, aliased below). The seven
//! `*Callback` wrappers are registered by `Runtime.installDurableModuleState`.
//!
//! Cross-module helpers are re-declared from their source modules (the same
//! ones zruntime aliases); only callFunction / extractResponseInternal /
//! createFetchResponse / splitHeaderKV reach back into zruntime (pub there).

const std = @import("std");
const ascii = std.ascii;
const zq = @import("zigts");
const durable_store_mod = @import("durable_store.zig");
const runtime_config_mod = @import("runtime_config.zig");
const natives = @import("runtime_natives.zig");
const trace_helpers = @import("trace_helpers.zig");
const zruntime = @import("zruntime.zig");

const Runtime = zruntime.Runtime;
const HttpResponse = zruntime.HttpResponse;
const HttpHeader = @import("http_types.zig").HttpHeader;
const compat = zq.compat;
const ActiveDurableRun = Runtime.ActiveDurableRun;
const PendingDurableRecovery = Runtime.PendingDurableRecovery;
const PendingDurableWait = Runtime.PendingDurableWait;
const DurableStore = durable_store_mod.DurableStore;

// Helpers re-declared from the same source modules zruntime aliases.
const upgradeResponseValue = natives.upgradeResponseValue;
const getHeaderAtom = natives.getHeaderAtom;
const getStringData = natives.getStringData;
const statusTextFor = natives.statusTextFor;
const unixMillis = zq.trace.unixMillis;
const appendEscapedJson = durable_store_mod.appendEscaped;
const openLockedFreshOplog = runtime_config_mod.openLockedFreshOplog;
const openOplogAppendFile = runtime_config_mod.openOplogAppendFile;
const tryLockOplogFd = runtime_config_mod.tryLockOplogFd;
const parseHeadersFromJson = trace_helpers.parseHeadersFromJson;

// The few helpers that genuinely live in zruntime (pub there).
const createFetchResponse = zruntime.createFetchResponse;
const splitHeaderKV = zruntime.splitHeaderKV;

const StepDeadlineGuard = struct {
    deadline_ns: u64,
    interrupt_requested: bool,
    jit_inhibited: bool,

    fn arm(rt: *Runtime, timeout_ms: i64) StepDeadlineGuard {
        const guard: StepDeadlineGuard = .{
            .deadline_ns = rt.ctx.deadline_ns,
            .interrupt_requested = rt.ctx.interrupt_requested.load(.monotonic),
            .jit_inhibited = rt.ctx.jit_inhibited,
        };
        const now = compat.monotonicNowNs() catch return guard;
        const timeout_ns = if (timeout_ms <= 0)
            @as(u64, 0)
        else
            @as(u64, @intCast(timeout_ms)) *| std.time.ns_per_ms;
        const step_deadline = now +| timeout_ns;
        rt.ctx.deadline_ns = if (guard.deadline_ns == 0) step_deadline else @min(guard.deadline_ns, step_deadline);
        rt.ctx.interrupt_requested.store(false, .monotonic);
        rt.ctx.jit_inhibited = true;
        return guard;
    }

    fn restore(self: StepDeadlineGuard, rt: *Runtime) void {
        rt.ctx.deadline_ns = self.deadline_ns;
        rt.ctx.interrupt_requested.store(self.interrupt_requested, .monotonic);
        rt.ctx.jit_inhibited = self.jit_inhibited;
    }
};

fn timeoutResult(rt: *Runtime, ctx: *zq.Context, name: []const u8) !zq.JSValue {
    const result = try zq.modules.util.createPlainResultErr(ctx, "timeout");
    try rt.active_durable_run.?.state.persistStepResult(name, ctx, result);
    return result;
}

fn timeoutExpired(deadline_ms: i64, now_ms: i64) bool {
    return now_ms >= deadline_ms;
}

fn requestIdempotencyKeyForRun(rt: *Runtime, durable_key: []const u8) ?[]const u8 {
    const request = rt.active_request orelse return null;
    for (request.headers.items) |header| {
        if (ascii.eqlIgnoreCase(header.key, "idempotency-key") and
            std.mem.eql(u8, header.value, durable_key))
        {
            return header.value;
        }
    }
    return null;
}

fn durableCompletedResponseAllowed(rt: *Runtime, idempotency_key: ?[]const u8) bool {
    const props = rt.config.durable_workflow_properties;
    if (!props.enforced) return true;
    if (props.idempotent) return true;
    return idempotency_key != null;
}

fn durableRetryAllowed(rt: *Runtime, durable_key: []const u8, idempotency_key: ?[]const u8) !bool {
    const props = rt.config.durable_workflow_properties;
    if (!props.enforced) return true;
    if (props.retry_safe) return true;
    if (!try durableRetryWouldResume(rt, durable_key)) return true;

    const ledger_key = idempotency_key orelse return false;
    var store = try initDurableStore(rt);
    return store.hasIdempotencyLedger(ledger_key, durable_key);
}

fn durableRetryWouldResume(rt: *Runtime, durable_key: []const u8) !bool {
    if (rt.pending_durable_recovery != null) return true;

    const path = try buildDurableOplogPath(rt, durable_key);
    defer rt.allocator.free(path);

    const source = try readDurableLogIfExists(rt, path) orelse return false;
    defer rt.allocator.free(source);

    var parsed = try zq.trace.parseDurableOplog(rt.allocator, source);
    defer parsed.deinit();
    if (parsed.run_key) |existing_key| {
        if (!std.mem.eql(u8, existing_key, durable_key)) return error.DurableKeyCollision;
    }
    return !parsed.complete;
}

fn recordIdempotencyLedger(
    rt: *Runtime,
    idempotency_key: []const u8,
    durable_key: []const u8,
    state: durable_store_mod.IdempotencyLedgerState,
) !void {
    var store = try initDurableStore(rt);
    try store.writeIdempotencyLedger(idempotency_key, durable_key, state);
}

fn durableSoftErrorResponse(rt: *Runtime, code: []const u8, detail: []const u8) !zq.JSValue {
    var body: std.ArrayList(u8) = .empty;
    defer body.deinit(rt.allocator);

    try body.appendSlice(rt.allocator, "{\"error\":\"");
    try appendEscapedJson(&body, rt.allocator, code);
    try body.appendSlice(rt.allocator, "\",\"detail\":\"");
    try appendEscapedJson(&body, rt.allocator, detail);
    try body.appendSlice(rt.allocator, "\"}");

    const created = try createFetchResponse(rt, 599, statusTextFor(599), body.items, "application/json");
    return created.value;
}

pub fn setPendingDurableRecovery(
    rt: *Runtime,
    key: []const u8,
    oplog_path: []const u8,
    events: []const zq.trace.DurableEvent,
) void {
    rt.pending_durable_recovery = .{
        .key = key,
        .oplog_path = oplog_path,
        .events = events,
    };
}

pub fn durableRun(rt: *Runtime, ctx: *zq.Context, key: []const u8, run_val: zq.JSValue) anyerror!zq.JSValue {
    if (rt.active_durable_run != null) {
        return zq.modules.util.throwError(ctx, "Error", "nested run() is not supported");
    }
    if (!run_val.isObject()) {
        return zq.modules.util.throwError(ctx, "TypeError", "run() expects a callable function");
    }
    _ = rt.active_request orelse {
        return zq.modules.util.throwError(ctx, "Error", "run() is only valid during request handling");
    };

    const idempotency_key = requestIdempotencyKeyForRun(rt, key);

    if (try tryLoadCompletedDurableResponse(rt, key)) |cached| {
        if (!durableCompletedResponseAllowed(rt, idempotency_key)) {
            return durableSoftErrorResponse(
                rt,
                "DurableIdempotencyUnproven",
                "reusing a completed durable response requires an idempotent workflow proof or an Idempotency-Key header matching run() key",
            );
        }
        if (idempotency_key) |ledger_key| {
            try recordIdempotencyLedger(rt, ledger_key, key, .completed);
        }
        return cached;
    }

    if (!try durableRetryAllowed(rt, key, idempotency_key)) {
        return durableSoftErrorResponse(
            rt,
            "DurableRetryUnproven",
            "automatic durable retry requires a retry_safe workflow proof or an Idempotency-Key header matching run() key from the original attempt",
        );
    }
    if (idempotency_key) |ledger_key| {
        try recordIdempotencyLedger(rt, ledger_key, key, .started);
    }

    const active = openActiveDurableRun(rt, key) catch |err| switch (err) {
        error.DurableRecoveryKeyMismatch => return zq.modules.util.throwError(ctx, "Error", "durable recovery key did not match run() key"),
        error.DurableKeyCollision => return zq.modules.util.throwError(ctx, "Error", "durable key collision detected for oplog path"),
        error.DurableRunBusy => return zq.modules.util.throwError(ctx, "Error", "durable run is busy: another process holds this run's oplog"),
        else => return err,
    };

    rt.active_durable_run = active;
    rt.ctx.setModuleState(
        zq.trace.DURABLE_STATE_SLOT,
        @ptrCast(rt.active_durable_run.?.state),
        &zq.trace.DurableState.deinitOpaque,
    );
    defer {
        rt.ctx.module_state[zq.trace.DURABLE_STATE_SLOT] = null;
        if (rt.active_durable_run) |*run| {
            run.deinit(rt.allocator);
        }
        rt.active_durable_run = null;
    }

    const func_obj = run_val.toPtr(zq.JSObject);
    const result = rt.callFunction(func_obj, &.{}) catch |err| switch (err) {
        error.DurableSuspended => return try buildPendingDurableResponseValue(rt),
        else => return err,
    };
    const upgraded = try upgradeResponseValue(rt.ctx, result);
    if (!isResponseLike(rt, upgraded)) {
        return zq.modules.util.throwError(ctx, "TypeError", "run() callback must return a Response");
    }

    var response = try rt.extractResponseInternal(upgraded, false);
    defer response.deinit();
    try persistActiveDurableResponse(rt, &response);
    if (idempotency_key) |ledger_key| {
        try recordIdempotencyLedger(rt, ledger_key, key, .completed);
    }
    return upgraded;
}

pub fn durableStep(rt: *Runtime, ctx: *zq.Context, name: []const u8, step_val: zq.JSValue) anyerror!zq.JSValue {
    const active = rt.active_durable_run orelse {
        return zq.modules.util.throwError(ctx, "Error", "step() must be called inside run()");
    };
    if (!step_val.isObject()) {
        return zq.modules.util.throwError(ctx, "TypeError", "step() expects a callable function");
    }
    if (active.step_depth > 0) {
        return zq.modules.util.throwError(ctx, "Error", "nested step() is not supported");
    }

    switch (active.state.beginStep(name)) {
        .cached => |result_json| {
            return zq.trace.jsonToJSValue(ctx, result_json);
        },
        .execute => {},
        .live => {
            try active.state.persistStepStart(name);
        },
    }

    rt.active_durable_run.?.step_depth += 1;
    defer rt.active_durable_run.?.step_depth -= 1;

    const func_obj = step_val.toPtr(zq.JSObject);
    const result = try rt.callFunction(func_obj, &.{});
    try rt.active_durable_run.?.state.persistStepResult(name, ctx, result);
    return result;
}

pub fn durableStepWithTimeout(rt: *Runtime, ctx: *zq.Context, name: []const u8, timeout_ms: i64, step_val: zq.JSValue) anyerror!zq.JSValue {
    const active = rt.active_durable_run orelse {
        return zq.modules.util.throwError(ctx, "Error", "stepWithTimeout() must be called inside run()");
    };
    if (!step_val.isObject()) {
        return zq.modules.util.throwError(ctx, "TypeError", "stepWithTimeout() expects a callable function");
    }
    if (active.step_depth > 0) {
        return zq.modules.util.throwError(ctx, "Error", "nested stepWithTimeout() is not supported");
    }

    switch (active.state.beginStep(name)) {
        .cached => |result_json| {
            return zq.trace.jsonToJSValue(ctx, result_json);
        },
        .execute => {},
        .live => {
            try active.state.persistStepStart(name);
        },
    }

    rt.active_durable_run.?.step_depth += 1;
    defer rt.active_durable_run.?.step_depth -= 1;

    const deadline_ms = std.math.add(i64, unixMillis(), timeout_ms) catch std.math.maxInt(i64);
    const prev_deadline_ms = rt.active_durable_run.?.step_timeout_deadline_ms;
    rt.active_durable_run.?.step_timeout_deadline_ms = deadline_ms;
    defer rt.active_durable_run.?.step_timeout_deadline_ms = prev_deadline_ms;

    const deadline_guard = StepDeadlineGuard.arm(rt, timeout_ms);
    defer deadline_guard.restore(rt);

    const func_obj = step_val.toPtr(zq.JSObject);
    const result = rt.callFunction(func_obj, &.{}) catch |err| {
        if (err == error.DurableSuspended) return err;
        if (err == error.DurableStepTimedOut or err == error.RequestTimeout) {
            return timeoutResult(rt, ctx, name);
        }
        if (timeoutExpired(deadline_ms, unixMillis())) {
            return timeoutResult(rt, ctx, name);
        }
        return err;
    };

    if (timeoutExpired(deadline_ms, unixMillis())) {
        return timeoutResult(rt, ctx, name);
    }

    const ok_result = try zq.modules.util.createPlainResultOk(ctx, result);
    try rt.active_durable_run.?.state.persistStepResult(name, ctx, ok_result);
    return ok_result;
}

pub fn durableSleepUntil(rt: *Runtime, ctx: *zq.Context, until_ms: i64) anyerror!zq.JSValue {
    if (rt.active_durable_run == null) {
        return zq.modules.util.throwError(ctx, "Error", "sleepUntil() must be called inside run()");
    }
    const active = &rt.active_durable_run.?;
    const timeout_deadline_ms = active.step_timeout_deadline_ms;
    if (active.step_depth > 0 and timeout_deadline_ms == null) {
        return zq.modules.util.throwError(ctx, "Error", "sleepUntil() is not supported inside step()");
    }

    const now_ms = unixMillis();
    if (timeout_deadline_ms) |deadline| {
        if (timeoutExpired(deadline, now_ms)) return error.DurableStepTimedOut;
    }
    const effective_until_ms = if (timeout_deadline_ms) |deadline| @min(until_ms, deadline) else until_ms;
    switch (active.state.beginTimerWait()) {
        .ready => return zq.JSValue.undefined_val,
        .pending => |wait| {
            if (wait.timeout_ms) |deadline| {
                if (timeoutExpired(deadline, now_ms)) return error.DurableStepTimedOut;
            }
            if (timeout_deadline_ms) |deadline| {
                if (timeoutExpired(deadline, now_ms)) return error.DurableStepTimedOut;
            }
            if (wait.until_ms <= now_ms) {
                try active.state.persistResumeTimer(now_ms);
                return zq.JSValue.undefined_val;
            }
            const pending_until_ms = if (timeout_deadline_ms) |deadline| @min(wait.until_ms, deadline) else wait.until_ms;
            try active.setPendingTimer(rt.allocator, pending_until_ms);
            return error.DurableSuspended;
        },
        .live => {
            const timer_timeout_ms = if (timeout_deadline_ms) |deadline|
                if (effective_until_ms == deadline and deadline < until_ms) deadline else null
            else
                null;
            try active.state.persistWaitTimer(effective_until_ms, timer_timeout_ms);
            if (effective_until_ms <= now_ms) {
                try active.state.persistResumeTimer(now_ms);
                return zq.JSValue.undefined_val;
            }
            try active.setPendingTimer(rt.allocator, effective_until_ms);
            return error.DurableSuspended;
        },
    }
}

pub fn durableWaitSignal(rt: *Runtime, ctx: *zq.Context, name: []const u8) anyerror!zq.JSValue {
    if (rt.active_durable_run == null) {
        return zq.modules.util.throwError(ctx, "Error", "waitSignal() must be called inside run()");
    }
    const active = &rt.active_durable_run.?;
    const timeout_deadline_ms = active.step_timeout_deadline_ms;
    if (active.step_depth > 0 and timeout_deadline_ms == null) {
        return zq.modules.util.throwError(ctx, "Error", "waitSignal() is not supported inside step()");
    }

    var store = try initDurableStore(rt);
    const now_ms = unixMillis();
    if (timeout_deadline_ms) |deadline| {
        if (timeoutExpired(deadline, now_ms)) return error.DurableStepTimedOut;
    }

    const recover_claimed_signal = switch (active.state.beginSignalWait(name)) {
        .delivered => |payload_json| {
            _ = try store.finalizeResumedSignalClaims(active.key, name, payload_json);
            return zq.trace.jsonToJSValue(ctx, payload_json);
        },
        .live => blk: {
            try active.state.persistWaitSignal(name, timeout_deadline_ms);
            break :blk false;
        },
        .pending => |wait| blk: {
            if (wait.timeout_ms) |deadline| {
                if (timeoutExpired(deadline, now_ms)) return error.DurableStepTimedOut;
            }
            break :blk true;
        },
    };

    const maybe_signal = if (recover_claimed_signal)
        try store.tryRecoverSignal(active.key, name, now_ms)
    else
        try store.tryConsumeSignal(active.key, name, now_ms);
    if (maybe_signal) |payload| {
        var consumed = payload;
        defer consumed.deinit();
        try active.state.persistResumeSignal(name, consumed.payload_json);
        // Unlink only after the consumption is durably in the oplog; the
        // reverse order loses the signal on a crash in between.
        store.finalizeConsumedSignal(&consumed);
        return zq.trace.jsonToJSValue(ctx, consumed.payload_json);
    }
    try active.setPendingSignal(rt.allocator, name);
    return error.DurableSuspended;
}

pub fn durableSignal(rt: *Runtime, ctx: *zq.Context, key: []const u8, name: []const u8, payload: zq.JSValue) anyerror!zq.JSValue {
    const exists = durableSignalTargetExists(rt, key) catch |err| switch (err) {
        error.DurableKeyCollision => return zq.modules.util.throwError(ctx, "Error", "durable key collision detected for oplog path"),
        else => return err,
    };
    if (!exists) return zq.JSValue.false_val;

    const payload_json = serializeDurablePayload(rt, ctx, payload) catch |err| switch (err) {
        error.InvalidDurablePayload => return zq.modules.util.throwError(ctx, "TypeError", "signal() payload must be JSON-serializable"),
        else => return err,
    };
    defer rt.allocator.free(payload_json);

    var store = try initDurableStore(rt);
    try store.enqueueSignal(key, name, payload_json);
    return zq.JSValue.true_val;
}

pub fn durableSignalAt(
    rt: *Runtime,
    ctx: *zq.Context,
    key: []const u8,
    name: []const u8,
    at_ms: i64,
    payload: zq.JSValue,
) anyerror!zq.JSValue {
    const exists = durableSignalTargetExists(rt, key) catch |err| switch (err) {
        error.DurableKeyCollision => return zq.modules.util.throwError(ctx, "Error", "durable key collision detected for oplog path"),
        else => return err,
    };
    if (!exists) return zq.JSValue.false_val;

    const payload_json = serializeDurablePayload(rt, ctx, payload) catch |err| switch (err) {
        error.InvalidDurablePayload => return zq.modules.util.throwError(ctx, "TypeError", "signalAt() payload must be JSON-serializable"),
        else => return err,
    };
    defer rt.allocator.free(payload_json);

    var store = try initDurableStore(rt);
    try store.enqueueSignalAt(key, name, at_ms, payload_json);
    return zq.JSValue.true_val;
}

pub fn initDurableStore(rt: *Runtime) !durable_store_mod.DurableStore {
    const dir = rt.config.durable_oplog_dir orelse return error.DurableDisabled;
    return durable_store_mod.DurableStore.initFs(rt.allocator, dir);
}

pub fn durableSignalTargetExists(rt: *Runtime, key: []const u8) !bool {
    const path = try buildDurableOplogPath(rt, key);
    defer rt.allocator.free(path);

    const source = try readDurableLogIfExists(rt, path) orelse return false;
    defer rt.allocator.free(source);

    var parsed = try zq.trace.parseDurableOplog(rt.allocator, source);
    defer parsed.deinit();

    if (parsed.run_key) |existing_key| {
        if (!std.mem.eql(u8, existing_key, key)) return error.DurableKeyCollision;
    }

    return !parsed.complete;
}

pub fn serializeDurablePayload(rt: *Runtime, ctx: *zq.Context, payload: zq.JSValue) ![]u8 {
    if (payload.isUndefined()) {
        return rt.allocator.dupe(u8, "null");
    }

    const json_val = zq.builtins.jsonStringify(ctx, zq.JSValue.undefined_val, &.{payload});
    if (ctx.hasException()) {
        ctx.clearException();
        return error.InvalidDurablePayload;
    }

    const json = getStringData(json_val) orelse return error.InvalidDurablePayload;
    return rt.allocator.dupe(u8, json);
}

pub fn buildPendingDurableResponseValue(rt: *Runtime) !zq.JSValue {
    const active = rt.active_durable_run orelse return error.NoActiveDurableRun;
    const wait = active.pending_wait orelse return error.MissingDurableWait;
    var body: std.ArrayList(u8) = .empty;
    defer body.deinit(rt.allocator);

    try body.appendSlice(rt.allocator, "{\"pending\":true,\"durableKey\":\"");
    try appendEscapedJson(&body, rt.allocator, active.key);
    try body.appendSlice(rt.allocator, "\",\"wait\":{");

    switch (wait) {
        .timer => |until_ms| {
            try body.appendSlice(rt.allocator, "\"type\":\"timer\",\"until\":");
            var tmp: [32]u8 = undefined;
            const printed = try std.fmt.bufPrint(&tmp, "{d}", .{until_ms});
            try body.appendSlice(rt.allocator, printed);
        },
        .signal => |name| {
            try body.appendSlice(rt.allocator, "\"type\":\"signal\",\"name\":\"");
            try appendEscapedJson(&body, rt.allocator, name);
            try body.appendSlice(rt.allocator, "\"");
        },
    }

    try body.appendSlice(rt.allocator, "}}");
    const created = try createFetchResponse(rt, 202, "Accepted", body.items, "application/json");
    return created.value;
}

pub fn tryLoadCompletedDurableResponse(rt: *Runtime, key: []const u8) !?zq.JSValue {
    if (rt.pending_durable_recovery != null) return null;

    const path = try buildDurableOplogPath(rt, key);
    defer rt.allocator.free(path);

    const source = try readDurableLogIfExists(rt, path) orelse return null;
    defer rt.allocator.free(source);

    var parsed = try zq.trace.parseDurableOplog(rt.allocator, source);
    defer parsed.deinit();

    if (!parsed.complete) return null;
    if (parsed.run_key) |existing_key| {
        if (!std.mem.eql(u8, existing_key, key)) return error.DurableKeyCollision;
    }

    const response = parsed.response orelse return error.DurableMissingResponse;
    return try buildStoredResponseValue(rt, response);
}

pub fn openActiveDurableRun(rt: *Runtime, key: []const u8) !ActiveDurableRun {
    if (rt.pending_durable_recovery) |pending| {
        if (!std.mem.eql(u8, pending.key, key)) return error.DurableRecoveryKeyMismatch;

        // No lock here: the recovery pass (durable_recovery.OplogClaim)
        // already holds the oplog lock on its own fd for the duration of
        // this re-execution; taking it again on a second fd would
        // self-deadlock.
        const fd = try openOplogAppendFile(rt.allocator, pending.oplog_path);
        const state = try rt.allocator.create(zq.trace.DurableState);
        state.* = zq.trace.DurableState.init(rt.allocator, pending.events, fd);
        return .{
            .key = try rt.allocator.dupe(u8, key),
            .oplog_path = try rt.allocator.dupe(u8, pending.oplog_path),
            .oplog_fd = fd,
            .state = state,
        };
    }

    const path = try buildDurableOplogPath(rt, key);
    errdefer rt.allocator.free(path);

    if (zq.file_io.fileExists(rt.allocator, path)) {
        // Lock before reading the snapshot: a concurrent recovery pass
        // (or a second live request on the same key) appending between
        // the read and our first write would double-apply effects.
        const fd = try openOplogAppendFile(rt.allocator, path);
        errdefer std.Io.Threaded.closeFd(fd);
        try tryLockOplogFd(fd);

        const source = (try readDurableLogIfExists(rt, path)) orelse
            return error.FileOpenFailed;
        errdefer rt.allocator.free(source);

        var parsed = try zq.trace.parseDurableOplog(rt.allocator, source);
        defer parsed.deinit();

        if (parsed.run_key) |existing_key| {
            if (!std.mem.eql(u8, existing_key, key)) return error.DurableKeyCollision;
        }
        if (parsed.complete) return error.DurableAlreadyComplete;

        const events = parsed.events;
        parsed.events = &.{};

        const state = try rt.allocator.create(zq.trace.DurableState);
        state.* = zq.trace.DurableState.init(rt.allocator, events, fd);
        return .{
            .key = try rt.allocator.dupe(u8, key),
            .oplog_path = path,
            .oplog_fd = fd,
            .state = state,
            .owned_events = events,
            .source_snapshot = source,
        };
    }

    const request = rt.active_request orelse return error.NoActiveRequest;
    const fd = try openLockedFreshOplog(rt.allocator, path);
    errdefer std.Io.Threaded.closeFd(fd);

    const state = try rt.allocator.create(zq.trace.DurableState);
    errdefer rt.allocator.destroy(state);
    state.* = zq.trace.DurableState.init(rt.allocator, &.{}, fd);
    try state.persistRunKey(key);

    var h_names: [64][]const u8 = undefined;
    var h_values: [64][]const u8 = undefined;
    const hcount = splitHeaderKV(request.headers.items, &h_names, &h_values);
    try state.persistRequest(
        request.method,
        request.url,
        h_names[0..hcount],
        h_values[0..hcount],
        request.body,
    );

    return .{
        .key = try rt.allocator.dupe(u8, key),
        .oplog_path = path,
        .oplog_fd = fd,
        .state = state,
    };
}

pub fn buildDurableOplogPath(rt: *Runtime, key: []const u8) ![]u8 {
    const dir = rt.config.durable_oplog_dir orelse return error.DurableDisabled;
    return std.fmt.allocPrint(
        rt.allocator,
        "{s}/durable-{x}.jsonl",
        .{ dir, std.hash.Fnv1a_64.hash(key) },
    );
}

pub fn readDurableLogIfExists(rt: *Runtime, path: []const u8) !?[]u8 {
    return zq.file_io.readFile(rt.allocator, path, 100 * 1024 * 1024) catch |err| switch (err) {
        error.FileNotFound => null,
        else => err,
    };
}

pub fn buildStoredResponseValue(rt: *Runtime, response: zq.trace.ResponseTrace) !zq.JSValue {
    var headers: std.ArrayListUnmanaged(HttpHeader) = .empty;
    defer headers.deinit(rt.allocator);
    try parseHeadersFromJson(rt.allocator, response.headers_json, &headers);
    const body = try zq.trace.unescapeJson(rt.allocator, response.body);
    defer rt.allocator.free(body);

    var content_type: ?[]const u8 = null;
    for (headers.items) |header| {
        if (ascii.eqlIgnoreCase(header.key, "content-type")) {
            content_type = header.value;
            break;
        }
    }

    const created = try createFetchResponse(
        rt,
        response.status,
        statusTextFor(response.status),
        body,
        content_type,
    );

    for (headers.items) |header| {
        const key_atom = try getHeaderAtom(rt.ctx, header.key);
        const value_str = try rt.ctx.createString(header.value);
        try rt.ctx.setPropertyChecked(created.headers, key_atom, value_str);
    }

    return created.value;
}

pub fn isResponseLike(rt: *Runtime, value: zq.JSValue) bool {
    if (!value.isObject()) return false;
    const pool = rt.ctx.hidden_class_pool orelse return false;
    const obj = value.toPtr(zq.JSObject);
    return obj.getProperty(pool, zq.Atom.status) != null and
        obj.getProperty(pool, zq.Atom.body) != null and
        obj.getProperty(pool, zq.Atom.headers) != null;
}

pub fn persistActiveDurableResponse(rt: *Runtime, response: *const HttpResponse) !void {
    if (rt.active_durable_run) |*active| {
        var h_names: [64][]const u8 = undefined;
        var h_values: [64][]const u8 = undefined;
        const hcount = splitHeaderKV(response.headers.items, &h_names, &h_values);
        try active.state.persistResponse(
            response.status,
            h_names[0..hcount],
            h_values[0..hcount],
            response.body,
        );
        try active.state.markComplete();
    }
}

pub fn durableRunCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    key: []const u8,
    run_val: zq.JSValue,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return durableRun(rt, ctx, key, run_val);
}

pub fn durableStepCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    name: []const u8,
    step_val: zq.JSValue,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return durableStep(rt, ctx, name, step_val);
}

pub fn durableStepWithTimeoutCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    name: []const u8,
    timeout_ms: i64,
    step_val: zq.JSValue,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return durableStepWithTimeout(rt, ctx, name, timeout_ms, step_val);
}

pub fn durableSleepUntilCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    until_ms: i64,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return durableSleepUntil(rt, ctx, until_ms);
}

pub fn durableWaitSignalCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    name: []const u8,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return durableWaitSignal(rt, ctx, name);
}

pub fn durableSignalCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    key: []const u8,
    name: []const u8,
    payload: zq.JSValue,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return durableSignal(rt, ctx, key, name, payload);
}

pub fn durableSignalAtCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    key: []const u8,
    name: []const u8,
    at_ms: i64,
    payload: zq.JSValue,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return durableSignalAt(rt, ctx, key, name, at_ms, payload);
}
