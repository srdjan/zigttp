//! Per-request trace recording: lazily creates the request's TraceRecorder,
//! records the request, and on completion records timing meta + flushes.
//! Extracted from zruntime.zig's executeHandlerInternal hot path to keep that
//! function focused; the trace_file / trace_mutex / trace_recorder state stays
//! on Runtime (these operate on it through `rt`).
//!
//! Usage in the hot path mirrors the original inline block:
//!     var timer = trace_request_recorder.setupRequestRecorder(self, request);
//!     defer trace_request_recorder.finishRequestRecorder(self, timer);
//! plus `recordResponse(self, &response)` at each response site.

const std = @import("std");
const zq = @import("zigts");
const compat = @import("zigts").compat;
const zruntime = @import("zruntime.zig");
const http_types = @import("http_types.zig");

const Runtime = zruntime.Runtime;
const HttpResponse = zruntime.HttpResponse;
const HttpRequestView = http_types.HttpRequestView;
const splitHeaderKV = zruntime.splitHeaderKV;

/// Set up the per-request recorder (lazily creating it on first request) and
/// record the inbound request. Returns the timer to hand to
/// `finishRequestRecorder`, or null when tracing is not configured.
pub fn setupRequestRecorder(rt: *Runtime, request: HttpRequestView) ?compat.Timer {
    var trace_timer: ?compat.Timer = null;
    if (rt.trace_file != null and rt.trace_mutex != null) {
        if (rt.trace_recorder == null) {
            rt.trace_recorder = rt.allocator.create(zq.TraceRecorder) catch null;
            if (rt.trace_recorder) |rec| {
                rec.* = zq.TraceRecorder.init(
                    rt.allocator,
                    rt.trace_file.?,
                    rt.trace_mutex.?,
                );
            }
        }
        if (rt.trace_recorder) |rec| {
            rec.reset();
            rt.ctx.setModuleState(
                zq.TRACE_STATE_SLOT,
                @ptrCast(rec),
                &zq.TraceRecorder.deinitOpaque,
            );

            var h_names: [64][]const u8 = undefined;
            var h_values: [64][]const u8 = undefined;
            const hcount = splitHeaderKV(request.headers.items, &h_names, &h_values);
            rec.recordRequestRaw(
                request.method,
                request.url,
                h_names[0..hcount],
                h_values[0..hcount],
                request.body,
            );
            trace_timer = compat.Timer.start() catch null;
        }
    }
    return trace_timer;
}

/// Record timing meta and flush after the handler completes, then detach the
/// recorder from module_state so the next request starts fresh.
pub fn finishRequestRecorder(rt: *Runtime, trace_timer: ?compat.Timer) void {
    if (rt.trace_recorder) |rec| {
        var timer = trace_timer;
        const duration_us: u64 = if (timer) |*t| t.read() / 1000 else 0;
        rec.recordMeta(duration_us, rt.config.trace_file_path orelse "unknown", 0);
        rec.flush();
        rt.ctx.module_state[zq.TRACE_STATE_SLOT] = null;
    }
}

/// Record a response into the active recorder (no-op when tracing is inactive).
pub fn recordResponse(rt: *Runtime, response: *const HttpResponse) void {
    const rec = rt.trace_recorder orelse return;
    if (!rec.active) return;

    var h_names: [64][]const u8 = undefined;
    var h_values: [64][]const u8 = undefined;
    const hcount = splitHeaderKV(response.headers.items, &h_names, &h_values);
    rec.recordResponse(
        response.status,
        h_names[0..hcount],
        h_values[0..hcount],
        response.body,
    );
}
