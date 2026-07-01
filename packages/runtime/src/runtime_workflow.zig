//! Workflow / saga / fan-out / follow native callbacks extracted from
//! zruntime.zig (review finding M1).
//!
//! These implement the runtime side of `zigttp:workflow` (call, saga, fanout,
//! follow). They orchestrate co-located sub-handlers through the in-process
//! `SystemRuntime` registry and copy borrowed sub-handler responses into
//! orchestrator-owned JS values. They depend on the ambient `current_runtime`
//! threadlocal plus the response/request helpers that remain in zruntime.zig,
//! reached here by back-import. zruntime depends on this module only for the
//! four callbacks registered in `Runtime.installWorkflowModuleState`.

const std = @import("std");
const ascii = std.ascii;
const zq = @import("zigts");
const zruntime = @import("zruntime.zig");
const natives = @import("runtime_natives.zig");
const http_parser = @import("http_parser.zig");
const durable_executor = @import("durable_executor.zig");
const http = @import("runtime_http.zig");
const workflow_queue = @import("workflow_queue.zig");

const Runtime = zruntime.Runtime;
const HttpResponse = zruntime.HttpResponse;
const HttpRequestView = zruntime.HttpRequestView;
const QueryParam = zruntime.QueryParam;
const SystemRuntime = zruntime.SystemRuntime;
const Target = @import("in_process_dispatch.zig").Target;

// Response/request helpers now in runtime_http.zig and runtime_natives.zig,
// reached by alias.
const createFetchResponse = http.createFetchResponse;
const createFetchErrorResponse = http.createFetchErrorResponse;
const httpRequestErrorJsonAlloc = http.httpRequestErrorJsonAlloc;
const parseHttpRequestParts = http.parseHttpRequestParts;
const extractResponseStatus = http.extractResponseStatus;
const appendPercentEncodedInto = http.appendPercentEncodedInto;
const getDynamicProperty = natives.getDynamicProperty;
const splitPathAndQuery = natives.splitPathAndQuery;
const getHeaderAtom = natives.getHeaderAtom;
const statusTextFor = natives.statusTextFor;
const getStringDataCtx = zq.builtins.helpers.getStringDataCtx;
const appendEscapedJson = @import("durable_store.zig").appendEscaped;

const WORKFLOW_QUEUE_RETRY_DELAY_MS: i64 = 1_000;

/// Runtime side of `zigttp:workflow.call(name, init)`. Builds an
/// `HttpRequestView` from the `init` object, dispatches it to a co-located
/// sub-handler in-process via the `SystemRuntime` registry (a separate pooled
/// runtime, own GC/arena, setjmp panic isolation), then copies the borrowed
/// sub-handler response into an orchestrator-owned JS `Response` BEFORE
/// `handle.deinit()` reclaims the sub-runtime's bytes.
///
/// `init` shape (all optional): `{ method?, path?, body?, headers? }`. Errors
/// fail soft as a 599 error `Response` (the JS subset has no try/catch, so an
/// orchestrator branches on `.status` rather than catching a throw).
pub fn workflowCallCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    name: []const u8,
    init_val: zq.JSValue,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    const registry = rt.system_registry_ref orelse {
        return createFetchErrorResponse(rt, "WorkflowUnavailable", "workflow.call requires a --system handler bundle");
    };

    // Owned storage for the request view: everything is duped out of the JS
    // heap into this arena so no view field borrows orchestrator-managed memory
    // across the in-process dispatch boundary. The arena outlives dispatch
    // (synchronous), then frees in one shot.
    var view_arena = std.heap.ArenaAllocator.init(rt.allocator);
    defer view_arena.deinit();
    const arena = view_arena.allocator();

    const parts = parseHttpRequestParts(ctx, init_val, arena) catch |err| {
        if (err == error.InvalidWorkflowInit) {
            return createFetchErrorResponse(rt, "InvalidInit", "workflow.call init fields must be { method?, path?, body? } strings and headers an object<string,string>");
        }
        return err;
    };
    const view = parts.view();

    // Durable: when inside a durable.run at step depth 0, record this call as
    // its own durable step so a COMPLETED sub-call replays from the oplog and is
    // never re-dispatched on recovery (don't re-charge a payment). Nested inside
    // a user durable.step (depth > 0) it falls back to a raw dispatch - the flat
    // oplog forbids steps within steps.
    if (rt.active_durable_run) |*adr| {
        if (adr.step_depth == 0) {
            return workflowCallDurable(rt, ctx, registry, name, view);
        }
    }
    if (rt.config.workflow_queue_enabled) {
        return createFetchErrorResponse(rt, "WorkflowQueueRequiresRun", "queued workflow.call must run at top level inside durable.run");
    }

    // The sub-handler dispatch reuses executeHandlerBorrowed, which sets-then-
    // clears these per-thread globals around the run. A nested dispatch would
    // otherwise leave them null for the orchestrator's continuation; on a
    // sub-handler panic `clearThreadStateAfterPanic` nulls them too while the
    // sub's restore-defers are skipped by longjmp. Snapshot the orchestrator's
    // values and restore them unconditionally after dispatch. (current_interpreter
    // is already save/restored per interpreter frame, but the panic path nulls it,
    // so restore it here as well.)
    const saved_runtime = zruntime.current_runtime;
    const saved_interpreter = zq.interpreter.current_interpreter;
    const saved_call_cb = zq.http.call_function_callback;
    const dispatch_result = registry.dispatch(name, view);
    zruntime.current_runtime = saved_runtime;
    zq.interpreter.current_interpreter = saved_interpreter;
    zq.http.call_function_callback = saved_call_cb;

    var handle = dispatch_result catch |err| {
        return switch (err) {
            error.UnknownHandler => createFetchErrorResponse(rt, "UnknownHandler", name),
            else => createFetchErrorResponse(rt, "WorkflowDispatchFailed", @errorName(err)),
        };
    };
    defer handle.deinit();

    // Copy the borrowed sub-handler response into orchestrator-owned memory
    // before the handle (and its sub-runtime bytes) are released.
    const resp = &handle.response;
    var content_type: ?[]const u8 = null;
    for (resp.headers.items) |hh| {
        if (ascii.eqlIgnoreCase(hh.key, "content-type")) {
            content_type = hh.value;
            break;
        }
    }
    const created = try createFetchResponse(rt, resp.status, statusTextFor(resp.status), resp.body, content_type);
    for (resp.headers.items) |hh| {
        const key_atom = try getHeaderAtom(rt.ctx, hh.key);
        const value_str = try rt.ctx.createString(hh.value);
        try rt.ctx.setPropertyChecked(created.headers, key_atom, value_str);
    }
    return created.value;
}

/// Durable variant of workflow.call: records the dispatch as one durable step.
/// On a cached replay the sub-handler is NOT invoked - the stored
/// {status,headers,body} is reconstructed into an identical Response. Live and
/// replay both reconstruct through `responseFromPartsObject`, so the bytes match.
fn workflowCallDurable(
    rt: *Runtime,
    ctx: *zq.Context,
    registry: *SystemRuntime,
    name: []const u8,
    view: HttpRequestView,
) anyerror!zq.JSValue {
    var name_buf: [48]u8 = undefined;
    const seq = rt.active_durable_run.?.call_seq;
    const step_name = std.fmt.bufPrint(&name_buf, "workflow.call#{d}", .{seq}) catch "workflow.call";
    rt.active_durable_run.?.call_seq = seq + 1;

    switch (rt.active_durable_run.?.state.beginStep(step_name)) {
        .cached => |result_json| {
            const parts = zq.trace.jsonToJSValue(ctx, result_json);
            return responseFromPartsObject(rt, parts);
        },
        .execute => {},
        .live => try rt.active_durable_run.?.state.persistStepStart(step_name),
    }

    const parts = if (rt.config.workflow_queue_enabled)
        try workflowQueuedDispatchParts(rt, ctx, registry, step_name, name, view)
    else
        try workflowDirectDispatchParts(rt, ctx, registry, name, view);

    try rt.active_durable_run.?.state.persistStepResult(step_name, ctx, parts);
    return responseFromPartsObject(rt, parts);
}

fn workflowDirectDispatchParts(
    rt: *Runtime,
    ctx: *zq.Context,
    registry: *SystemRuntime,
    name: []const u8,
    view: HttpRequestView,
) !zq.JSValue {
    const saved_runtime = zruntime.current_runtime;
    const saved_interpreter = zq.interpreter.current_interpreter;
    const saved_call_cb = zq.http.call_function_callback;
    const dispatch_result = registry.dispatch(name, view);
    zruntime.current_runtime = saved_runtime;
    zq.interpreter.current_interpreter = saved_interpreter;
    zq.http.call_function_callback = saved_call_cb;

    var handle = dispatch_result catch |err| {
        const code = if (err == error.UnknownHandler) "UnknownHandler" else "WorkflowDispatchFailed";
        const detail = if (err == error.UnknownHandler) name else @errorName(err);
        return workflowErrorParts(rt, ctx, code, detail);
    };
    defer handle.deinit();
    return workflowResponseParts(ctx, &handle.response);
}

fn workflowDirectTargetDispatchParts(
    rt: *Runtime,
    ctx: *zq.Context,
    target: *Target,
    view: HttpRequestView,
) !zq.JSValue {
    const saved_runtime = zruntime.current_runtime;
    const saved_interpreter = zq.interpreter.current_interpreter;
    const saved_call_cb = zq.http.call_function_callback;
    const dispatch_result = target.pool.executeHandlerBorrowed(view);
    zruntime.current_runtime = saved_runtime;
    zq.interpreter.current_interpreter = saved_interpreter;
    zq.http.call_function_callback = saved_call_cb;

    var handle = dispatch_result catch |err| {
        return workflowErrorParts(rt, ctx, "WorkflowDispatchFailed", @errorName(err));
    };
    defer handle.deinit();
    return workflowResponseParts(ctx, &handle.response);
}

fn workflowQueuedDispatchParts(
    rt: *Runtime,
    ctx: *zq.Context,
    registry: *SystemRuntime,
    item_step_name: []const u8,
    target_name: []const u8,
    view: HttpRequestView,
) anyerror!zq.JSValue {
    const durable_dir = rt.config.durable_oplog_dir orelse return error.WorkflowQueueRequiresDurable;
    if (rt.active_durable_run == null) return error.NoActiveDurableRun;
    const active = &rt.active_durable_run.?;
    const item_id = try workflow_queue.itemId(rt.allocator, active.key, item_step_name);
    defer rt.allocator.free(item_id);

    try workflow_queue.enqueueRequest(rt.allocator, durable_dir, item_id, target_name, view);

    const now_ms = zq.trace.unixMillis();
    var claim = try workflow_queue.tryClaim(
        rt.allocator,
        durable_dir,
        item_id,
        now_ms,
        workflow_queue.defaultLeaseMs(),
    );
    defer claim.deinit(rt.allocator);

    switch (claim) {
        .done => |result_json| {
            return zq.trace.jsonToJSValue(ctx, result_json);
        },
        .dead => {
            // A dead-lettered child needs an operator to act (`workflow-queue
            // replay` moves it back to pending; `discard` clears it so a
            // later attempt starts fresh) before this can resolve
            // differently. Suspend like `.busy` instead of returning a
            // terminal error Response: `durableRun`'s
            // `persistActiveDurableResponse` marks the run complete on any
            // returned Response, and once complete the run never re-checks
            // the queue again - `workflow-queue replay` would un-stick the
            // queue item but never the parent workflow waiting on it.
            try suspendQueuedWorkflowDispatch(rt, saturatingAddMs(now_ms, WORKFLOW_QUEUE_RETRY_DELAY_MS));
            return error.DurableSuspended;
        },
        .busy => |lease_until_ms| {
            // Wake close to the real lease expiry rather than polling on a
            // fixed interval, but never sooner than the minimum delay so a
            // near-expired lease can't cause a tight retry loop.
            const retry_at_ms = @max(lease_until_ms, saturatingAddMs(now_ms, WORKFLOW_QUEUE_RETRY_DELAY_MS));
            try suspendQueuedWorkflowDispatch(rt, retry_at_ms);
            return error.DurableSuspended;
        },
        .claimed => |*queued_request| {
            var queued_view = try queued_request.toView(rt.allocator);
            defer queued_view.headers.deinit(rt.allocator);
            try completeQueuedDispatch(rt, registry, durable_dir, item_id, queued_request, queued_view);

            const result_json = (try workflow_queue.readResult(rt.allocator, durable_dir, item_id)) orelse {
                try suspendQueuedWorkflowDispatch(rt, saturatingAddMs(now_ms, WORKFLOW_QUEUE_RETRY_DELAY_MS));
                return error.DurableSuspended;
            };
            defer rt.allocator.free(result_json);
            return zq.trace.jsonToJSValue(ctx, result_json);
        },
    }
}

fn completeQueuedDispatch(
    rt: *Runtime,
    registry: *SystemRuntime,
    durable_dir: []const u8,
    item_id: []const u8,
    queued_request: *const workflow_queue.QueuedRequest,
    view: HttpRequestView,
) !void {
    const saved_runtime = zruntime.current_runtime;
    const saved_interpreter = zq.interpreter.current_interpreter;
    const saved_call_cb = zq.http.call_function_callback;
    const dispatch_result = registry.dispatch(queued_request.target, view);
    zruntime.current_runtime = saved_runtime;
    zq.interpreter.current_interpreter = saved_interpreter;
    zq.http.call_function_callback = saved_call_cb;

    var handle = dispatch_result catch |err| {
        const code = if (err == error.UnknownHandler) "UnknownHandler" else "WorkflowDispatchFailed";
        const detail = if (err == error.UnknownHandler) queued_request.target else @errorName(err);
        try workflow_queue.completeError(rt.allocator, durable_dir, item_id, code, detail);
        return;
    };
    defer handle.deinit();
    try workflow_queue.completeResponse(rt.allocator, durable_dir, item_id, handle.response);
}

fn saturatingAddMs(now_ms: i64, delay_ms: i64) i64 {
    return std.math.add(i64, now_ms, delay_ms) catch std.math.maxInt(i64);
}

fn suspendQueuedWorkflowDispatch(rt: *Runtime, retry_at_ms: i64) !void {
    if (rt.active_durable_run == null) return error.NoActiveDurableRun;
    const active = &rt.active_durable_run.?;
    try active.state.persistWaitTimer(retry_at_ms, null);
    try active.setPendingTimer(rt.allocator, retry_at_ms);
}

const ResolvedAffordance = struct {
    href: []const u8,
    method: []const u8,
};

/// Resolve affordance `rel` on a branded `resource()` to its `{href, method}`,
/// duped into `arena`. Fails closed: a non-resource, a missing rel, or a
/// non-literal href surfaces as a distinct error the caller maps to a 599.
fn resolveAffordance(
    ctx: *zq.Context,
    resource_val: zq.JSValue,
    rel: []const u8,
    arena: std.mem.Allocator,
) !ResolvedAffordance {
    const aff_set = zq.http.resourceAffordances(ctx, resource_val) orelse return error.NotAResource;
    if (!aff_set.isObject()) return error.NoSuchAffordance;
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const set_obj = aff_set.toPtr(zq.JSObject);

    const aff_val = getDynamicProperty(ctx, set_obj, pool, rel) orelse return error.NoSuchAffordance;
    if (!aff_val.isObject()) return error.NoSuchAffordance;
    const aff_obj = aff_val.toPtr(zq.JSObject);

    const href_val = getDynamicProperty(ctx, aff_obj, pool, "href") orelse return error.DynamicAffordance;
    const href = getStringDataCtx(href_val, ctx) orelse return error.DynamicAffordance;

    var method: []const u8 = "GET";
    if (getDynamicProperty(ctx, aff_obj, pool, "method")) |mv| {
        if (!mv.isUndefined() and !mv.isNull()) {
            method = getStringDataCtx(mv, ctx) orelse return error.DynamicAffordance;
        }
    }
    return .{ .href = try arena.dupe(u8, href), .method = try arena.dupe(u8, method) };
}

/// Substitute `{key}` placeholders in a templated affordance href with values
/// from `init.params`, into `arena`. Returns the href unchanged when it carries
/// no `{`. A missing param or an unclosed brace fails closed (error) rather than
/// dispatching a literal `{key}` that the link proof never certifies routable.
fn substituteHrefTemplate(ctx: *zq.Context, href: []const u8, init_val: zq.JSValue, arena: std.mem.Allocator) ![]const u8 {
    if (std.mem.indexOfScalar(u8, href, '{') == null) return href;

    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    var params_obj: ?*zq.JSObject = null;
    if (init_val.isObject()) {
        if (getDynamicProperty(ctx, init_val.toPtr(zq.JSObject), pool, "params")) |pv| {
            if (pv.isObject()) params_obj = pv.toPtr(zq.JSObject);
        }
    }

    var out: std.ArrayListUnmanaged(u8) = .empty;
    var i: usize = 0;
    while (i < href.len) {
        if (href[i] == '{') {
            const close = std.mem.indexOfScalarPos(u8, href, i, '}') orelse return error.TemplateUnclosed;
            const key = href[i + 1 .. close];
            const po = params_obj orelse return error.MissingTemplateParam;
            const val = getDynamicProperty(ctx, po, pool, key) orelse return error.MissingTemplateParam;
            const s = getStringDataCtx(val, ctx) orelse return error.MissingTemplateParam;
            try appendPercentEncodedInto(arena, &out, s);
            i = close + 1;
        } else {
            try out.append(arena, href[i]);
            i += 1;
        }
    }
    return out.items;
}

const FollowHref = struct {
    url: []const u8,
    path: []const u8,
    query_params: []const QueryParam,
};

fn parseFollowHref(arena: std.mem.Allocator, href: []const u8) !FollowHref {
    const url = if (std.mem.indexOfScalar(u8, href, '#')) |idx| href[0..idx] else href;
    const split = splitPathAndQuery(url);
    const parsed_query = try http_parser.parseQueryString(arena, split.query_string, http_parser.DEFAULT_MAX_QUERY_LENGTH);
    return .{
        .url = url,
        .path = split.path,
        .query_params = parsed_query.params,
    };
}

/// Runtime side of `zigttp:workflow.follow(resource, rel, init?)`. Resolves the
/// affordance `rel` on a structured `resource()` to `{href, method}`, routes the
/// href to a co-located handler by the "/<name>" mount convention, and
/// dispatches in-process - HATEOAS link-following without the orchestrator
/// hardcoding the target name. `init` (optional) supplies a request body and
/// headers; the affordance's method and href drive routing. Inside a durable.run
/// at step depth 0 the dispatch is recorded as one durable step (replays from
/// the oplog on recovery, like workflow.call). Errors fail soft as a 599.
pub fn workflowFollowCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    resource_val: zq.JSValue,
    rel: []const u8,
    init_val: zq.JSValue,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    const registry = rt.system_registry_ref orelse {
        return createFetchErrorResponse(rt, "WorkflowUnavailable", "workflow.follow requires a --system handler bundle");
    };

    var view_arena = std.heap.ArenaAllocator.init(rt.allocator);
    defer view_arena.deinit();
    const arena = view_arena.allocator();

    const aff = resolveAffordance(ctx, resource_val, rel, arena) catch |err| {
        return switch (err) {
            error.NotAResource => createFetchErrorResponse(rt, "NotAResource", "follow() first argument must be a resource()"),
            error.NoSuchAffordance => createFetchErrorResponse(rt, "NoSuchAffordance", rel),
            error.DynamicAffordance => createFetchErrorResponse(rt, "DynamicAffordance", rel),
            else => err,
        };
    };

    // A templated affordance href ("/orders/{id}") is dispatched by substituting
    // `{key}` from init.params; an unsupplied param fails soft rather than
    // sending a literal `{id}` to the target (which the link proof never
    // certifies as routable).
    const dispatch_href = substituteHrefTemplate(ctx, aff.href, init_val, arena) catch |err| {
        return switch (err) {
            error.MissingTemplateParam, error.TemplateUnclosed => createFetchErrorResponse(rt, "MissingTemplateParam", aff.href),
            else => err,
        };
    };
    const follow_href = parseFollowHref(arena, dispatch_href) catch |err| {
        return switch (err) {
            error.QueryTooLong => createFetchErrorResponse(rt, "InvalidHref", "workflow.follow href query is too long"),
            else => err,
        };
    };

    // init carries an optional body/headers/params; the affordance's method and
    // (substituted) href are authoritative for routing - any method/path on init
    // is intentionally ignored (the InvalidInit message documents this).
    var parts = parseHttpRequestParts(ctx, init_val, arena) catch |err| {
        if (err == error.InvalidWorkflowInit) {
            return createFetchErrorResponse(rt, "InvalidInit", "workflow.follow init fields must be { params?, body?, headers? }");
        }
        return err;
    };
    parts.method = aff.method;
    parts.url = follow_href.url;
    parts.path = follow_href.path;
    parts.query_params = follow_href.query_params;
    const view = parts.view();

    const target = registry.findByRoute(follow_href.path) orelse {
        return createFetchErrorResponse(rt, "UnknownRoute", follow_href.path);
    };

    if (rt.active_durable_run) |*adr| {
        if (adr.step_depth == 0) {
            return workflowFollowDurable(rt, ctx, registry, target, view);
        }
    }
    if (rt.config.workflow_queue_enabled) {
        return createFetchErrorResponse(rt, "WorkflowQueueRequiresRun", "queued workflow.follow must run at top level inside durable.run");
    }

    // Guarded nested dispatch (see workflowCallCallback for the per-thread
    // global snapshot/restore rationale).
    const saved_runtime = zruntime.current_runtime;
    const saved_interpreter = zq.interpreter.current_interpreter;
    const saved_call_cb = zq.http.call_function_callback;
    const dispatch_result = target.pool.executeHandlerBorrowed(view);
    zruntime.current_runtime = saved_runtime;
    zq.interpreter.current_interpreter = saved_interpreter;
    zq.http.call_function_callback = saved_call_cb;

    var handle = dispatch_result catch |err| {
        return createFetchErrorResponse(rt, "WorkflowDispatchFailed", @errorName(err));
    };
    defer handle.deinit();

    const resp = &handle.response;
    var content_type: ?[]const u8 = null;
    for (resp.headers.items) |hh| {
        if (ascii.eqlIgnoreCase(hh.key, "content-type")) {
            content_type = hh.value;
            break;
        }
    }
    const created = try createFetchResponse(rt, resp.status, statusTextFor(resp.status), resp.body, content_type);
    for (resp.headers.items) |hh| {
        const key_atom = try getHeaderAtom(rt.ctx, hh.key);
        const value_str = try rt.ctx.createString(hh.value);
        try rt.ctx.setPropertyChecked(created.headers, key_atom, value_str);
    }
    return created.value;
}

/// Durable variant of workflow.follow: records the routed dispatch as one
/// durable step ("workflow.follow#<seq>"), sharing the run's call sequence so a
/// completed follow replays from the oplog and is never re-dispatched.
fn workflowFollowDurable(
    rt: *Runtime,
    ctx: *zq.Context,
    registry: *SystemRuntime,
    target: *Target,
    view: HttpRequestView,
) anyerror!zq.JSValue {
    var name_buf: [48]u8 = undefined;
    const seq = rt.active_durable_run.?.call_seq;
    const step_name = std.fmt.bufPrint(&name_buf, "workflow.follow#{d}", .{seq}) catch "workflow.follow";
    rt.active_durable_run.?.call_seq = seq + 1;

    switch (rt.active_durable_run.?.state.beginStep(step_name)) {
        .cached => |result_json| {
            const parts = zq.trace.jsonToJSValue(ctx, result_json);
            return responseFromPartsObject(rt, parts);
        },
        .execute => {},
        .live => try rt.active_durable_run.?.state.persistStepStart(step_name),
    }

    const parts = if (rt.config.workflow_queue_enabled)
        try workflowQueuedDispatchParts(rt, ctx, registry, step_name, target.name, view)
    else
        try workflowDirectTargetDispatchParts(rt, ctx, target, view);

    try rt.active_durable_run.?.state.persistStepResult(step_name, ctx, parts);
    return responseFromPartsObject(rt, parts);
}

/// Snapshot a borrowed sub-handler response as a plain `{status, headers, body}`
/// JS object - the durable step result. Strings are copied into the orchestrator
/// heap, so it stays valid after the borrowed handle is released.
fn workflowResponseParts(ctx: *zq.Context, resp: *const HttpResponse) !zq.JSValue {
    const parts = try ctx.createObject(null);
    try ctx.setPropertyChecked(parts, zq.Atom.status, zq.JSValue.fromInt(@intCast(resp.status)));
    const hdrs = try ctx.createObject(null);
    for (resp.headers.items) |hh| {
        const key_atom = try getHeaderAtom(ctx, hh.key);
        try ctx.setPropertyChecked(hdrs, key_atom, try ctx.createString(hh.value));
    }
    try ctx.setPropertyChecked(parts, zq.Atom.headers, hdrs.toValue());
    try ctx.setPropertyChecked(parts, zq.Atom.body, try ctx.createString(resp.body));
    return parts.toValue();
}

/// Build a `{status, headers, body}` step result for a dispatch error (599),
/// mirroring the error JSON the non-durable path returns.
fn workflowErrorParts(rt: *Runtime, ctx: *zq.Context, code: []const u8, detail: []const u8) !zq.JSValue {
    const body = try httpRequestErrorJsonAlloc(rt.allocator, code, detail);
    defer rt.allocator.free(body);
    const parts = try ctx.createObject(null);
    try ctx.setPropertyChecked(parts, zq.Atom.status, zq.JSValue.fromInt(599));
    const hdrs = try ctx.createObject(null);
    try ctx.setPropertyChecked(hdrs, try getHeaderAtom(ctx, "content-type"), try ctx.createString("application/json"));
    try ctx.setPropertyChecked(parts, zq.Atom.headers, hdrs.toValue());
    try ctx.setPropertyChecked(parts, zq.Atom.body, try ctx.createString(body));
    return parts.toValue();
}

/// Reconstruct a real Response (with .json()/.text(), proper prototype) from a
/// `{status, headers, body}` object. Used on both the live and cached durable
/// paths so a replay is byte-identical to the first run.
fn responseFromPartsObject(rt: *Runtime, parts: zq.JSValue) !zq.JSValue {
    const pool = rt.ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    if (!parts.isObject()) {
        const fallback = try createFetchResponse(rt, 502, statusTextFor(502), "", null);
        return fallback.value;
    }
    const obj = parts.toPtr(zq.JSObject);

    const status: u16 = blk: {
        const sv = obj.getProperty(pool, zq.Atom.status) orelse break :blk 200;
        if (sv.isInt()) {
            const raw = sv.getInt();
            if (raw >= 100 and raw <= 599) break :blk @intCast(raw);
        }
        break :blk 200;
    };

    const body_val = obj.getProperty(pool, zq.Atom.body) orelse zq.JSValue.undefined_val;
    const body = getStringDataCtx(body_val, rt.ctx) orelse "";

    const headers_obj: ?*zq.JSObject = blk: {
        const hv = obj.getProperty(pool, zq.Atom.headers) orelse break :blk null;
        if (!hv.isObject()) break :blk null;
        break :blk hv.toPtr(zq.JSObject);
    };

    // Enumerate the headers object once: find content-type for the Response
    // constructor, then reuse the same key slice to copy every header.
    var content_type: ?[]const u8 = null;
    const header_keys = if (headers_obj) |ho|
        try ho.getOwnEnumerableKeys(rt.allocator, pool)
    else
        null;
    defer if (header_keys) |k| rt.allocator.free(k);

    if (headers_obj) |ho| {
        for (header_keys.?) |ka| {
            const kn = rt.ctx.atoms.getName(ka) orelse continue;
            if (ascii.eqlIgnoreCase(kn, "content-type")) {
                const vv = ho.getOwnProperty(pool, ka) orelse continue;
                content_type = getStringDataCtx(vv, rt.ctx);
                break;
            }
        }
    }

    const created = try createFetchResponse(rt, status, statusTextFor(status), body, content_type);
    if (headers_obj) |ho| {
        for (header_keys.?) |ka| {
            const kn = rt.ctx.atoms.getName(ka) orelse continue;
            const vv = ho.getOwnProperty(pool, ka) orelse continue;
            const vs = getStringDataCtx(vv, rt.ctx) orelse continue;
            const value_atom = try getHeaderAtom(rt.ctx, kn);
            try rt.ctx.setPropertyChecked(created.headers, value_atom, try rt.ctx.createString(vs));
        }
    }
    return created.value;
}

/// Runtime side of zigttp:workflow.saga(steps). Each step's `run` runs as a
/// durable step "do:<name>"; on a step failure (its Response status >= 400) the
/// completed steps are compensated in REVERSE via "undo:<name>" durable steps.
/// The compensation order is an emergent property of deterministic replay: on
/// recovery the orchestrator re-executes, completed do:/undo: steps return cached
/// results, so the same rollback replays without re-invoking anything (design
/// D1 - no oplog schema change). A compensation that itself fails yields a
/// terminal 500 "manual intervention", with the oplog as the audit trail.
pub fn workflowSagaCallback(runtime_ptr: *anyopaque, ctx: *zq.Context, steps_val: zq.JSValue) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    if (rt.config.workflow_queue_enabled) {
        return zq.modules.util.throwError(ctx, "Error", "saga() is not supported with --workflow-queue; use top-level durable call/follow/fanout");
    }
    if (rt.active_durable_run == null) {
        return zq.modules.util.throwError(ctx, "Error", "saga() must be called inside run()");
    }
    if (rt.active_durable_run.?.step_depth > 0) {
        return zq.modules.util.throwError(ctx, "Error", "saga() cannot be nested inside a step");
    }
    if (!steps_val.isObject()) {
        return zq.modules.util.throwError(ctx, "TypeError", "saga() expects an array of steps");
    }
    if (steps_val.toPtr(zq.JSObject).class_id != .array) {
        return zq.modules.util.throwError(ctx, "TypeError", "saga() expects an array of steps");
    }
    const count = steps_val.toPtr(zq.JSObject).getArrayLength();
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    var arena_inst = std.heap.ArenaAllocator.init(rt.allocator);
    defer arena_inst.deinit();
    const arena = arena_inst.allocator();

    // Each durableStep below runs orchestrator JS, which (unlike fanout's
    // sub-runtime dispatch) can drive THIS context's GC and move JS objects. So
    // every JSValue held across a step is a tracked GC root, read back through
    // gc_state.getRoot so the address is always current - never a cached raw
    // pointer. Mirrors zigttp:scope's resource-handle pattern. removeRootAt on
    // every exit path keeps the root set from growing.
    const steps_root = try ctx.gc_state.addRootTracked(steps_val);
    defer ctx.gc_state.removeRootAt(steps_root);

    const Completed = struct { name: []const u8, compensate_root: usize };
    var completed: std.ArrayListUnmanaged(Completed) = .empty;
    defer for (completed.items) |c| ctx.gc_state.removeRootAt(c.compensate_root);

    var i: u32 = 0;
    while (i < count) : (i += 1) {
        // Re-derive the (possibly-forwarded) steps array each iteration.
        const arr = ctx.gc_state.getRoot(steps_root).toPtr(zq.JSObject);
        const step = arr.getIndex(i) orelse continue;
        if (!step.isObject()) return zq.modules.util.throwError(ctx, "TypeError", "saga() steps must be objects");
        const step_obj = step.toPtr(zq.JSObject);

        const name_val = getDynamicProperty(ctx, step_obj, pool, "name") orelse return zq.modules.util.throwError(ctx, "TypeError", "saga() step is missing a name");
        const name_raw = getStringDataCtx(name_val, ctx) orelse return zq.modules.util.throwError(ctx, "TypeError", "saga() step name must be a string");
        // Dupe the name before the durable step runs - durableStep drives the
        // interpreter (and GC), which can move the backing JS string.
        const name = try arena.dupe(u8, name_raw);

        const run_val = getDynamicProperty(ctx, step_obj, pool, "run") orelse return zq.modules.util.throwError(ctx, "TypeError", "saga() step is missing a run function");
        if (!run_val.isCallable()) return zq.modules.util.throwError(ctx, "TypeError", "saga() step run must be a function");
        // Root the compensate closure: it is held in `completed` across later
        // durableStep calls that can move it. `root_owned` transfers cleanup to
        // `completed`'s defer on success, while errdefer covers an OOM/engine
        // error between here and the append.
        const compensate_val = if (getDynamicProperty(ctx, step_obj, pool, "compensate")) |c| c else zq.JSValue.undefined_val;
        const compensate_root = try ctx.gc_state.addRootTracked(compensate_val);
        var root_owned = true;
        errdefer if (root_owned) ctx.gc_state.removeRootAt(compensate_root);

        const do_name = try std.fmt.allocPrint(arena, "do:{s}", .{name});
        const result = try durable_executor.durableStep(rt, ctx, do_name, run_val);

        if (!sagaStepSucceeded(rt, result)) {
            root_owned = false;
            ctx.gc_state.removeRootAt(compensate_root);
            // Roll back completed steps in reverse declaration order.
            var j = completed.items.len;
            while (j > 0) {
                j -= 1;
                const c = completed.items[j];
                const compensate = ctx.gc_state.getRoot(c.compensate_root);
                if (!compensate.isCallable()) continue;
                const undo_name = try std.fmt.allocPrint(arena, "undo:{s}", .{c.name});
                const undo_result = try durable_executor.durableStep(rt, ctx, undo_name, compensate);
                if (!sagaStepSucceeded(rt, undo_result)) {
                    return buildSagaResponse(rt, 500, false, name, c.name);
                }
            }
            const failed_status = extractResponseStatus(rt, result);
            const status: u16 = if (failed_status >= 400) failed_status else 500;
            return buildSagaResponse(rt, status, false, name, null);
        }

        try completed.append(arena, .{ .name = name, .compensate_root = compensate_root });
        root_owned = false;
    }

    return buildSagaResponse(rt, 200, true, null, null);
}

/// A saga step succeeds iff its result is a Response with a 1xx-3xx status. A
/// missing/invalid status (e.g. a dispatch error's 599, or a non-Response) is a
/// failure that triggers compensation. Works on both the live Response and the
/// replay-reconstructed plain object (both expose an integer `status`).
fn sagaStepSucceeded(rt: *Runtime, result: zq.JSValue) bool {
    const status = extractResponseStatus(rt, result);
    return status >= 100 and status < 400;
}

/// Build saga's summary Response - always freshly constructed so the live and
/// replay results are byte-identical. 200 {ok:true} on full success; the failed
/// step's status with {ok:false,failed,compensated:true} after a clean rollback;
/// 500 {ok:false,failed,compensationFailed} when a compensation itself failed.
fn buildSagaResponse(rt: *Runtime, status: u16, ok: bool, failed: ?[]const u8, comp_failed: ?[]const u8) !zq.JSValue {
    var body: std.ArrayList(u8) = .empty;
    defer body.deinit(rt.allocator);
    try body.appendSlice(rt.allocator, "{\"ok\":");
    try body.appendSlice(rt.allocator, if (ok) "true" else "false");
    if (failed) |f| {
        try body.appendSlice(rt.allocator, ",\"failed\":\"");
        try appendEscapedJson(&body, rt.allocator, f);
        try body.append(rt.allocator, '"');
    }
    if (comp_failed) |c| {
        try body.appendSlice(rt.allocator, ",\"compensationFailed\":\"");
        try appendEscapedJson(&body, rt.allocator, c);
        try body.append(rt.allocator, '"');
    } else if (failed != null) {
        try body.appendSlice(rt.allocator, ",\"compensated\":true");
    }
    try body.append(rt.allocator, '}');

    const created = try createFetchResponse(rt, status, statusTextFor(status), body.items, "application/json");
    return created.value;
}

const MAX_PARALLEL_CALLS: u32 = 16;

/// Runtime side of zigttp:workflow.fanout(calls). Dispatches N co-located
/// sub-handlers and returns their Responses as an array in DECLARATION ORDER.
/// Inside a durable.run the whole fan-out is recorded as ONE durable step, so on
/// recovery the aggregate replays from a single oplog entry - concurrency (if
/// any) stays strictly below the oplog boundary, keeping the sequential oplog
/// the single source of truth. Each `calls[i]` is
/// `{ name, method?, path?, body?, headers? }`.
pub fn workflowFanoutCallback(runtime_ptr: *anyopaque, ctx: *zq.Context, calls_val: zq.JSValue) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    const registry = rt.system_registry_ref orelse {
        return createFetchErrorResponse(rt, "WorkflowUnavailable", "workflow.fanout requires a --system handler bundle");
    };
    if (!calls_val.isObject()) return zq.modules.util.throwError(ctx, "TypeError", "fanout() expects an array of calls");
    const arr = calls_val.toPtr(zq.JSObject);
    if (arr.class_id != .array) return zq.modules.util.throwError(ctx, "TypeError", "fanout() expects an array of calls");
    const count = arr.getArrayLength();
    if (count > MAX_PARALLEL_CALLS) return zq.modules.util.throwError(ctx, "RangeError", "fanout() supports at most 16 calls");

    const durable = if (rt.active_durable_run) |*adr| adr.step_depth == 0 else false;
    if (durable) {
        var name_buf: [48]u8 = undefined;
        const seq = rt.active_durable_run.?.call_seq;
        // Compatibility: this persisted prefix shipped before the public API was
        // named `fanout()`. Keep it stable so existing durable oplogs replay.
        const step_name = std.fmt.bufPrint(&name_buf, "workflow.parallel#{d}", .{seq}) catch "workflow.parallel";
        rt.active_durable_run.?.call_seq = seq + 1;

        switch (rt.active_durable_run.?.state.beginStep(step_name)) {
            .cached => |result_json| {
                const parts_arr = zq.trace.jsonToJSValue(ctx, result_json);
                return responsesFromPartsArray(rt, ctx, parts_arr);
            },
            .execute => {},
            .live => try rt.active_durable_run.?.state.persistStepStart(step_name),
        }
        const parts_arr = if (rt.config.workflow_queue_enabled)
            try dispatchAllQueuedToPartsArray(rt, ctx, registry, arr, count, step_name)
        else
            try dispatchAllToPartsArray(rt, ctx, registry, arr, count);
        try rt.active_durable_run.?.state.persistStepResult(step_name, ctx, parts_arr);
        return responsesFromPartsArray(rt, ctx, parts_arr);
    }

    if (rt.config.workflow_queue_enabled) {
        return zq.modules.util.throwError(ctx, "Error", "queued workflow.fanout must run at top level inside durable.run");
    }

    const parts_arr = try dispatchAllToPartsArray(rt, ctx, registry, arr, count);
    return responsesFromPartsArray(rt, ctx, parts_arr);
}

/// Dispatch every call descriptor and collect a JS array of `{status,headers,
/// body}` parts in declaration order. A per-call failure becomes a 599 parts
/// entry rather than aborting the whole fan-out. (Sequential dispatch; the
/// aggregate is order-deterministic regardless of execution order.)
fn dispatchAllToPartsArray(rt: *Runtime, ctx: *zq.Context, registry: *SystemRuntime, arr: *zq.JSObject, count: u32) !zq.JSValue {
    var view_arena = std.heap.ArenaAllocator.init(rt.allocator);
    defer view_arena.deinit();
    const arena = view_arena.allocator();

    // Each nested dispatch clears these per-thread globals (see workflowCallCallback);
    // snapshot once and restore after the whole fan-out.
    const saved_runtime = zruntime.current_runtime;
    const saved_interpreter = zq.interpreter.current_interpreter;
    const saved_call_cb = zq.http.call_function_callback;
    defer {
        zruntime.current_runtime = saved_runtime;
        zq.interpreter.current_interpreter = saved_interpreter;
        zq.http.call_function_callback = saved_call_cb;
    }

    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const parts_arr = try ctx.createArray();
    var i: u32 = 0;
    while (i < count) : (i += 1) {
        const desc = arr.getIndex(i) orelse {
            try parts_arr.arrayPush(ctx.allocator, try workflowErrorParts(rt, ctx, "InvalidCall", "missing call descriptor"));
            continue;
        };
        const name: ?[]const u8 = blk: {
            if (!desc.isObject()) break :blk null;
            const dobj = desc.toPtr(zq.JSObject);
            const nv = getDynamicProperty(ctx, dobj, pool, "name") orelse break :blk null;
            break :blk getStringDataCtx(nv, ctx);
        };
        if (name == null) {
            try parts_arr.arrayPush(ctx.allocator, try workflowErrorParts(rt, ctx, "InvalidCall", "call is missing a name"));
            continue;
        }
        const parts = parseHttpRequestParts(ctx, desc, arena) catch {
            try parts_arr.arrayPush(ctx.allocator, try workflowErrorParts(rt, ctx, "InvalidInit", "malformed call init"));
            continue;
        };

        var handle = registry.dispatch(name.?, parts.view()) catch |err| {
            const code = if (err == error.UnknownHandler) "UnknownHandler" else "WorkflowDispatchFailed";
            const detail = if (err == error.UnknownHandler) name.? else @errorName(err);
            try parts_arr.arrayPush(ctx.allocator, try workflowErrorParts(rt, ctx, code, detail));
            continue;
        };
        // Copy out before releasing the borrowed handle (and its pool slot).
        const item_parts = workflowResponseParts(ctx, &handle.response) catch |e| {
            handle.deinit();
            return e;
        };
        handle.deinit();
        try parts_arr.arrayPush(ctx.allocator, item_parts);
    }
    return parts_arr.toValue();
}

fn dispatchAllQueuedToPartsArray(
    rt: *Runtime,
    ctx: *zq.Context,
    registry: *SystemRuntime,
    arr: *zq.JSObject,
    count: u32,
    step_name: []const u8,
) !zq.JSValue {
    var view_arena = std.heap.ArenaAllocator.init(rt.allocator);
    defer view_arena.deinit();
    const arena = view_arena.allocator();

    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const parts_arr = try ctx.createArray();
    var i: u32 = 0;
    while (i < count) : (i += 1) {
        const desc = arr.getIndex(i) orelse {
            try parts_arr.arrayPush(ctx.allocator, try workflowErrorParts(rt, ctx, "InvalidCall", "missing call descriptor"));
            continue;
        };
        const name: ?[]const u8 = blk: {
            if (!desc.isObject()) break :blk null;
            const dobj = desc.toPtr(zq.JSObject);
            const nv = getDynamicProperty(ctx, dobj, pool, "name") orelse break :blk null;
            break :blk getStringDataCtx(nv, ctx);
        };
        if (name == null) {
            try parts_arr.arrayPush(ctx.allocator, try workflowErrorParts(rt, ctx, "InvalidCall", "call is missing a name"));
            continue;
        }
        const parts = parseHttpRequestParts(ctx, desc, arena) catch {
            try parts_arr.arrayPush(ctx.allocator, try workflowErrorParts(rt, ctx, "InvalidInit", "malformed call init"));
            continue;
        };

        const item_step_name = try std.fmt.allocPrint(arena, "{s}.{d}", .{ step_name, i });
        const item_parts = try workflowQueuedDispatchParts(rt, ctx, registry, item_step_name, name.?, parts.view());
        try parts_arr.arrayPush(ctx.allocator, item_parts);
    }
    return parts_arr.toValue();
}

/// Reconstruct an array of real Responses from a `{status,headers,body}` parts
/// array - used on both the live and cached durable paths so a replay matches.
fn responsesFromPartsArray(rt: *Runtime, ctx: *zq.Context, parts_arr_val: zq.JSValue) !zq.JSValue {
    const result = try ctx.createArray();
    if (!parts_arr_val.isObject()) return result.toValue();
    const parts_arr = parts_arr_val.toPtr(zq.JSObject);
    if (parts_arr.class_id != .array) return result.toValue();
    const count = parts_arr.getArrayLength();
    var i: u32 = 0;
    while (i < count) : (i += 1) {
        const elem = parts_arr.getIndex(i) orelse zq.JSValue.undefined_val;
        const resp = try responseFromPartsObject(rt, elem);
        try result.arrayPush(ctx.allocator, resp);
    }
    return result.toValue();
}
