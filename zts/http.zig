//! HTTP Request/Response and JSX runtime for zigttp integration
//!
//! Native implementations of:
//! - Request class
//! - Response class with helpers (json, text, html, redirect)
//! - h() hyperscript function
//! - renderToString() for SSR

const std = @import("std");
const value = @import("value.zig");
const object = @import("object.zig");
const context = @import("context.zig");
const string = @import("string.zig");

// ============================================================================
// Function Component Callback
// ============================================================================

/// Callback for calling JS function components during SSR
/// Set by the runtime before executing handlers
pub const CallFunctionFn = *const fn (func: *object.JSObject, args: []const value.JSValue) anyerror!value.JSValue;
pub threadlocal var call_function_callback: ?CallFunctionFn = null;

/// Set the function calling callback (called by runtime before handler execution)
pub fn setCallFunctionCallback(callback: CallFunctionFn) void {
    call_function_callback = callback;
}

/// Clear the callback (called by runtime after handler execution)
pub fn clearCallFunctionCallback() void {
    call_function_callback = null;
}

// ============================================================================
// Request Object
// ============================================================================

/// Create a Request object from HTTP request data
pub fn createRequest(
    ctx: *context.Context,
    method: []const u8,
    url: []const u8,
    headers: []const [2][]const u8,
    body: ?[]const u8,
) !value.JSValue {
    if (ctx.http_shapes) |shapes| {
        const req_obj = try ctx.createObjectWithClass(shapes.request.class_idx, null);

        const method_str = try ctx.createString(method);
        req_obj.setSlot(shapes.request.method_slot, method_str);

        const url_str = try ctx.createString(url);
        req_obj.setSlot(shapes.request.url_slot, url_str);

        if (body) |b| {
            const body_str = try ctx.createString(b);
            req_obj.setSlot(shapes.request.body_slot, body_str);
        } else {
            req_obj.setSlot(shapes.request.body_slot, value.JSValue.null_val);
        }

        const headers_obj = try ctx.createObject(null);
        for (headers) |header| {
            const name_atom = try ctx.atoms.intern(header[0]);
            const value_str = try ctx.createString(header[1]);
            try ctx.setPropertyChecked(headers_obj, name_atom, value_str);
        }
        req_obj.setSlot(shapes.request.headers_slot, headers_obj.toValue());
        return req_obj.toValue();
    }

    const req_obj = try ctx.createObject(null);

    // Set method
    const method_atom = try ctx.atoms.intern("method");
    const method_str = try ctx.createString(method);
    try ctx.setPropertyChecked(req_obj, method_atom, method_str);

    // Set url
    const url_atom = try ctx.atoms.intern("url");
    const url_str = try ctx.createString(url);
    try ctx.setPropertyChecked(req_obj, url_atom, url_str);

    // Set body
    const body_atom = try ctx.atoms.intern("body");
    if (body) |b| {
        const body_str = try ctx.createString(b);
        try ctx.setPropertyChecked(req_obj, body_atom, body_str);
    } else {
        try ctx.setPropertyChecked(req_obj, body_atom, value.JSValue.null_val);
    }

    // Set headers object
    const headers_atom = try ctx.atoms.intern("headers");
    const headers_obj = try ctx.createObject(null);
    for (headers) |header| {
        const name_atom = try ctx.atoms.intern(header[0]);
        const value_str = try ctx.createString(header[1]);
        try ctx.setPropertyChecked(headers_obj, name_atom, value_str);
    }
    try ctx.setPropertyChecked(req_obj, headers_atom, headers_obj.toValue());

    return req_obj.toValue();
}

// ============================================================================
// Response Object
// ============================================================================

/// Create a basic Response object
pub fn createResponse(
    ctx: *context.Context,
    body: []const u8,
    status: u16,
    content_type: []const u8,
) !value.JSValue {
    if (ctx.http_shapes) |shapes| {
        const resp_obj = try ctx.createObjectWithClass(shapes.response.class_idx, null);

        const body_str = try ctx.createString(body);
        resp_obj.setSlot(shapes.response.body_slot, body_str);

        resp_obj.setSlot(shapes.response.status_slot, value.JSValue.fromInt(@intCast(status)));

        const status_text_val: value.JSValue = if (ctx.getCachedStatusText(status)) |cached| value.JSValue.fromPtr(cached) else blk: {
            const status_text = switch (status) {
                200 => "OK",
                201 => "Created",
                204 => "No Content",
                301 => "Moved Permanently",
                302 => "Found",
                400 => "Bad Request",
                401 => "Unauthorized",
                403 => "Forbidden",
                404 => "Not Found",
                500 => "Internal Server Error",
                else => "Unknown",
            };
            break :blk try ctx.createString(status_text);
        };
        resp_obj.setSlot(shapes.response.status_text_slot, status_text_val);

        resp_obj.setSlot(shapes.response.ok_slot, value.JSValue.fromBool(status >= 200 and status < 300));

        const headers_obj = try ctx.createObjectWithClass(shapes.response_headers.class_idx, null);
        const ct_val: value.JSValue = if (ctx.getCachedContentType(content_type)) |cached| value.JSValue.fromPtr(cached) else try ctx.createString(content_type);
        headers_obj.setSlot(shapes.response_headers.content_type_slot, ct_val);
        resp_obj.setSlot(shapes.response.headers_slot, headers_obj.toValue());

        return resp_obj.toValue();
    }

    const resp_obj = try ctx.createObject(null);

    // Set body (using predefined atom for efficiency)
    const body_str = try ctx.createString(body);
    try ctx.setPropertyChecked(resp_obj, object.Atom.body, body_str);

    // Set status (using predefined atom)
    try ctx.setPropertyChecked(resp_obj, object.Atom.status, value.JSValue.fromInt(@intCast(status)));

    // Set statusText
    const status_text_atom = if (ctx.http_strings) |cache| cache.status_text_atom else try ctx.atoms.intern("statusText");
    const status_text = switch (status) {
        200 => "OK",
        201 => "Created",
        204 => "No Content",
        301 => "Moved Permanently",
        302 => "Found",
        400 => "Bad Request",
        401 => "Unauthorized",
        403 => "Forbidden",
        404 => "Not Found",
        500 => "Internal Server Error",
        else => "Unknown",
    };
    const status_text_val: value.JSValue = if (ctx.getCachedStatusText(status)) |cached| value.JSValue.fromPtr(cached) else try ctx.createString(status_text);
    try ctx.setPropertyChecked(resp_obj, status_text_atom, status_text_val);

    // Set ok (status 200-299)
    const ok_atom = object.Atom.ok;
    try ctx.setPropertyChecked(resp_obj, ok_atom, value.JSValue.fromBool(status >= 200 and status < 300));

    // Set headers (using predefined atom)
    const headers_obj = try ctx.createObject(null);
    const ct_atom = if (ctx.http_strings) |cache| cache.content_type_atom else try ctx.atoms.intern("Content-Type");
    const ct_val: value.JSValue = if (ctx.getCachedContentType(content_type)) |cached| value.JSValue.fromPtr(cached) else try ctx.createString(content_type);
    try ctx.setPropertyChecked(headers_obj, ct_atom, ct_val);
    try ctx.setPropertyChecked(resp_obj, object.Atom.headers, headers_obj.toValue());

    return resp_obj.toValue();
}

/// Create a Response object using an existing JSString body (avoids extra copy).
pub fn createResponseFromString(
    ctx: *context.Context,
    body_str: *string.JSString,
    status: u16,
    content_type: []const u8,
) !value.JSValue {
    if (ctx.http_shapes) |shapes| {
        const resp_obj = try ctx.createObjectWithClass(shapes.response.class_idx, null);

        resp_obj.setSlot(shapes.response.body_slot, value.JSValue.fromPtr(body_str));
        resp_obj.setSlot(shapes.response.status_slot, value.JSValue.fromInt(@intCast(status)));

        const status_text_val: value.JSValue = if (ctx.getCachedStatusText(status)) |cached| value.JSValue.fromPtr(cached) else blk: {
            const status_text = switch (status) {
                200 => "OK",
                201 => "Created",
                204 => "No Content",
                301 => "Moved Permanently",
                302 => "Found",
                400 => "Bad Request",
                401 => "Unauthorized",
                403 => "Forbidden",
                404 => "Not Found",
                500 => "Internal Server Error",
                else => "Unknown",
            };
            break :blk try ctx.createString(status_text);
        };
        resp_obj.setSlot(shapes.response.status_text_slot, status_text_val);

        resp_obj.setSlot(shapes.response.ok_slot, value.JSValue.fromBool(status >= 200 and status < 300));

        const headers_obj = try ctx.createObjectWithClass(shapes.response_headers.class_idx, null);
        const ct_val: value.JSValue = if (ctx.getCachedContentType(content_type)) |cached| value.JSValue.fromPtr(cached) else try ctx.createString(content_type);
        headers_obj.setSlot(shapes.response_headers.content_type_slot, ct_val);
        resp_obj.setSlot(shapes.response.headers_slot, headers_obj.toValue());

        return resp_obj.toValue();
    }

    const resp_obj = try ctx.createObject(null);

    // Set body using existing JSString
    try ctx.setPropertyChecked(resp_obj, object.Atom.body, value.JSValue.fromPtr(body_str));

    // Set status (using predefined atom)
    try ctx.setPropertyChecked(resp_obj, object.Atom.status, value.JSValue.fromInt(@intCast(status)));

    // Set statusText
    const status_text_atom = if (ctx.http_strings) |cache| cache.status_text_atom else try ctx.atoms.intern("statusText");
    const status_text = switch (status) {
        200 => "OK",
        201 => "Created",
        204 => "No Content",
        301 => "Moved Permanently",
        302 => "Found",
        400 => "Bad Request",
        401 => "Unauthorized",
        403 => "Forbidden",
        404 => "Not Found",
        500 => "Internal Server Error",
        else => "Unknown",
    };
    const status_text_val: value.JSValue = if (ctx.getCachedStatusText(status)) |cached| value.JSValue.fromPtr(cached) else try ctx.createString(status_text);
    try ctx.setPropertyChecked(resp_obj, status_text_atom, status_text_val);

    // Set ok (status 200-299)
    const ok_atom = object.Atom.ok;
    try ctx.setPropertyChecked(resp_obj, ok_atom, value.JSValue.fromBool(status >= 200 and status < 300));

    // Set headers (using predefined atom)
    const headers_obj = try ctx.createObject(null);
    const ct_atom = if (ctx.http_strings) |cache| cache.content_type_atom else try ctx.atoms.intern("Content-Type");
    const ct_val: value.JSValue = if (ctx.getCachedContentType(content_type)) |cached| value.JSValue.fromPtr(cached) else try ctx.createString(content_type);
    try ctx.setPropertyChecked(headers_obj, ct_atom, ct_val);
    try ctx.setPropertyChecked(resp_obj, object.Atom.headers, headers_obj.toValue());

    return resp_obj.toValue();
}

fn normalizeStatus(default_status: u16, status_val: value.JSValue) u16 {
    if (!status_val.isInt()) return default_status;
    const raw = status_val.getInt();
    if (raw < 100 or raw > 599) return 500;
    return @intCast(raw);
}

fn statusFromInitObject(ctx: *context.Context, init: *object.JSObject, default_status: u16) u16 {
    const pool = ctx.hidden_class_pool orelse return default_status;
    const status_atom = ctx.atoms.intern("status") catch return default_status;
    if (init.getProperty(pool, status_atom)) |status_val| {
        return normalizeStatus(default_status, status_val);
    }
    return default_status;
}

/// Response.json(data) - Create JSON response
pub fn responseJson(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) {
        return createResponse(ctx, "{}", 200, "application/json");
    }

    // Serialize the data to JSON
    const data = args[0];
    const json_js = try valueToJsonString(ctx, data);

    var status: u16 = 200;
    if (args.len > 1 and args[1].isObject()) {
        const init = object.JSObject.fromValue(args[1]);
        status = statusFromInitObject(ctx, init, status);
    }

    return createResponseFromString(ctx, json_js, status, "application/json");
}

/// Response.rawJson(jsonString) - Create JSON response from pre-serialized string
/// Bypasses object serialization for maximum performance when JSON is already built.
pub fn responseRawJson(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) {
        return createResponse(ctx, "{}", 200, "application/json");
    }

    // Get the pre-serialized JSON string
    const json_str = try getStringArgWithCtx(args[0], ctx);

    var status: u16 = 200;
    if (args.len > 1 and args[1].isObject()) {
        const init = object.JSObject.fromValue(args[1]);
        status = statusFromInitObject(ctx, init, status);
    }

    return createResponse(ctx, json_str, status, "application/json");
}

/// Response.text(text) - Create text response
pub fn responseText(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) {
        return createResponse(ctx, "", 200, "text/plain; charset=utf-8");
    }

    const text = try getStringArgWithCtx(args[0], ctx);
    var status: u16 = 200;
    if (args.len > 1 and args[1].isObject()) {
        const init = object.JSObject.fromValue(args[1]);
        status = statusFromInitObject(ctx, init, status);
    }

    return createResponse(ctx, text, status, "text/plain; charset=utf-8");
}

/// Response.html(html) - Create HTML response
pub fn responseHtml(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) {
        return createResponse(ctx, "", 200, "text/html; charset=utf-8");
    }

    const html = try getStringArgWithCtx(args[0], ctx);
    var status: u16 = 200;
    if (args.len > 1 and args[1].isObject()) {
        const init = object.JSObject.fromValue(args[1]);
        status = statusFromInitObject(ctx, init, status);
    }

    return createResponse(ctx, html, status, "text/html; charset=utf-8");
}

/// Response.redirect(url, status?) - Create redirect response
pub fn responseRedirect(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) {
        return value.JSValue.undefined_val;
    }

    const url = try getStringArgWithCtx(args[0], ctx);
    var status: u16 = 302;
    if (args.len > 1 and args[1].isInt()) {
        status = normalizeStatus(status, args[1]);
    }

    // Create response with Location header
    const resp_obj = try ctx.createObject(null);

    const body_atom = try ctx.atoms.intern("_body");
    const body_str = try ctx.createString("");
    try ctx.setPropertyChecked(resp_obj, body_atom, body_str);

    const status_atom = try ctx.atoms.intern("status");
    try ctx.setPropertyChecked(resp_obj, status_atom, value.JSValue.fromInt(@intCast(status)));

    const headers_atom = try ctx.atoms.intern("headers");
    const headers_obj = try ctx.createObject(null);
    const location_atom = try ctx.atoms.intern("Location");
    const location_str = try ctx.createString(url);
    try ctx.setPropertyChecked(headers_obj, location_atom, location_str);
    try ctx.setPropertyChecked(resp_obj, headers_atom, headers_obj.toValue());

    return resp_obj.toValue();
}

/// new Response(body, init) - Response constructor (Web API compatible)
pub fn responseConstructor(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    // Default to empty text response
    var body: []const u8 = "";
    var status: u16 = 200;
    var content_type: []const u8 = "text/plain; charset=utf-8";

    // Get body from first argument
    if (args.len > 0 and args[0].isString()) {
        body = try getStringArgWithCtx(args[0], ctx);
    }

    // Get init options from second argument
    if (args.len > 1 and args[1].isObject()) {
        const init = object.JSObject.fromValue(args[1]);
        const pool = ctx.hidden_class_pool orelse return createResponse(ctx, body, status, content_type);

        // Get status
        status = statusFromInitObject(ctx, init, status);

        // Get headers for content-type
        const headers_atom = ctx.atoms.intern("headers") catch return createResponse(ctx, body, status, content_type);
        if (init.getProperty(pool, headers_atom)) |hdr| {
            if (hdr.isObject()) {
                const headers_obj = object.JSObject.fromValue(hdr);
                const ct_atom = ctx.atoms.intern("Content-Type") catch return createResponse(ctx, body, status, content_type);
                if (headers_obj.getProperty(pool, ct_atom)) |ct| {
                    if (ct.isString()) {
                        content_type = try getStringArgWithCtx(ct, ctx);
                    }
                }
            }
        }
    }

    return createResponse(ctx, body, status, content_type);
}

// ============================================================================
// JSX Runtime
// ============================================================================

/// Fragment is represented as a null tag value in vnodes.
/// Both <> shorthand (codegen push_null) and <Fragment> (global = null)
/// produce null tags, detected via O(1) isNull() check in renderNode.

fn appendChild(
    ctx: *context.Context,
    children_arr: *object.JSObject,
    child_count: *u32,
    child: value.JSValue,
) !void {
    if (child.isNull() or child.isUndefined()) return;

    if (child.isObject()) {
        const obj = object.JSObject.fromValue(child);
        if (obj.class_id == .array) {
            const len = obj.getArrayLength();
            var i: u32 = 0;
            while (i < len) : (i += 1) {
                if (obj.getIndex(i)) |nested| {
                    try appendChild(ctx, children_arr, child_count, nested);
                }
            }
            return;
        }
    }

    try ctx.setIndexChecked(children_arr, child_count.*, child);
    child_count.* += 1;
}

/// h(tag, props, ...children) - Create virtual DOM node
pub fn h(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    // Tag (first arg), defaults to "div"
    const tag_val = if (args.len > 0) args[0] else blk: {
        break :blk try ctx.createString("div");
    };

    // Props (second arg) - pass null through instead of allocating empty object
    const props_val = if (args.len > 1 and args[1].isObject())
        args[1]
    else
        value.JSValue.null_val;

    // Collect children (remaining args)
    const children_arr = try ctx.createArray();
    children_arr.prototype = ctx.array_prototype;
    var child_count: u32 = 0;
    if (args.len > 2) {
        for (args[2..]) |child| {
            try appendChild(ctx, children_arr, &child_count, child);
        }
    }
    children_arr.setArrayLength(child_count);

    // Create vnode with preallocated shape for direct slot writes
    if (ctx.vnode_shape) |shape| {
        const node = try ctx.createObjectWithClass(shape.class_idx, null);
        node.setSlot(shape.tag_slot, tag_val);
        node.setSlot(shape.props_slot, props_val);
        node.setSlot(shape.children_slot, children_arr.toValue());
        return node.toValue();
    }

    // Fallback: dynamic property setting
    const node = try ctx.createObject(null);
    try ctx.setPropertyChecked(node, .tag, tag_val);
    try ctx.setPropertyChecked(node, .props, props_val);
    try ctx.setPropertyChecked(node, .children, children_arr.toValue());
    return node.toValue();
}

/// renderToString(node) - Render virtual DOM to HTML string
pub fn renderToString(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) {
        return ctx.createString("") catch return value.JSValue.undefined_val;
    }

    // Use per-context reusable buffer to avoid allocation per call
    var aw = &ctx.render_writer;
    aw.clearRetainingCapacity();
    try renderNode(ctx, args[0], &aw.writer);
    const result = try ctx.createString(aw.written());
    return result;
}

const RenderError = std.Io.Writer.Error || error{ OutOfMemory, ArenaObjectEscape, NoHiddenClassPool };

/// Render a single node to the writer
fn renderNode(ctx: *context.Context, node: value.JSValue, writer: *std.Io.Writer) RenderError!void {
    // Handle null/undefined
    if (node.isNull() or node.isUndefined()) return;

    // Handle string-like values (flat, rope, slice)
    if (node.isStringOrRope()) {
        const text = try getStringArgWithCtx(node, ctx);
        try escapeHtml(text, writer);
        return;
    }

    // Handle number
    if (node.isInt()) {
        try writer.print("{d}", .{node.getInt()});
        return;
    }

    // Handle boolean (don't render)
    if (node.isBool()) return;

    // Handle object (virtual DOM node or array)
    if (node.isObject()) {
        const obj = object.JSObject.fromValue(node);

        // Check if it's an array (nested children from {props.children})
        if (obj.class_id == .array) {
            const len = obj.getArrayLength();
            var i: u32 = 0;
            while (i < len) : (i += 1) {
                if (obj.getIndex(i)) |child| {
                    try renderNode(ctx, child, writer);
                }
            }
            return;
        }

        const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

        // Check for tag property (vnode)
        const tag_val = obj.getProperty(pool, .tag) orelse return;

        // Handle function components (tag is a callable function)
        if (tag_val.isCallable()) {
            const call_fn = call_function_callback orelse return;

            // Get props (or create empty object if not present but children exist)
            var props_val = obj.getProperty(pool, .props) orelse value.JSValue.null_val;
            const children_val = obj.getProperty(pool, .children);

            // Add children to props if present
            if (children_val) |cv| {
                if (props_val.isObject()) {
                    // Add children to existing props
                    const props_obj = object.JSObject.fromValue(props_val);
                    try ctx.setPropertyChecked(props_obj, .children, cv);
                } else {
                    // Create new props object with children
                    const new_props = try ctx.createObject(null);
                    try ctx.setPropertyChecked(new_props, .children, cv);
                    props_val = new_props.toValue();
                }
            }

            // Call the component function with props
            const func_obj = object.JSObject.fromValue(tag_val);
            const call_args = [_]value.JSValue{props_val};
            const component_result = call_fn(func_obj, &call_args) catch |err| {
                std.log.err("Component function call failed: {}", .{err});
                return;
            };

            // Recursively render the result
            try renderNode(ctx, component_result, writer);
            return;
        }

        // Fragment: null tag means render children without wrapper
        if (tag_val.isNull()) {
            if (obj.getProperty(pool, .children)) |children| {
                try renderChildren(ctx, children, writer);
            }
            return;
        }

        // HTML element
        if (tag_val.isString()) {
            const tag_str = tag_val.toPtr(string.JSString);
            try writer.writeByte('<');
            try writer.writeAll(tag_str.data());

            // Render attributes from props
            if (obj.getProperty(pool, .props)) |props_val| {
                if (props_val.isObject()) {
                    try renderAttributes(ctx, props_val, writer);
                }
            }

            // Check for void elements
            const tag_data = tag_str.data();
            if (isVoidElement(tag_data)) {
                try writer.writeAll(" />");
                return;
            }

            try writer.writeByte('>');

            // Render children - use raw output for style/script elements
            if (obj.getProperty(pool, .children)) |children| {
                if (isRawTextElement(tag_data)) {
                    try renderRawChildren(ctx, children, writer);
                } else {
                    try renderChildren(ctx, children, writer);
                }
            }

            try writer.writeAll("</");
            try writer.writeAll(tag_data);
            try writer.writeByte('>');
        }
    }
}

/// Render children value - delegates to renderNode which handles all types.
fn renderChildren(ctx: *context.Context, children: value.JSValue, writer: *std.Io.Writer) RenderError!void {
    try renderNode(ctx, children, writer);
}

/// Render HTML attributes from props object
fn renderAttributes(ctx: *context.Context, props: value.JSValue, writer: anytype) !void {
    if (!props.isObject()) return;

    const props_obj = object.JSObject.fromValue(props);

    // Get pool for property iteration
    const pool = ctx.hidden_class_pool orelse return;
    const class_idx = props_obj.hidden_class_idx;
    if (class_idx.isNone()) return;

    const idx = class_idx.toInt();
    if (idx >= pool.count) return;

    const prop_count = pool.property_counts.items[idx];
    if (prop_count == 0) return;

    const start = pool.properties_starts.items[idx];
    const names = pool.property_names.items[start..][0..prop_count];
    const offsets = pool.property_offsets.items[start..][0..prop_count];

    for (names, offsets) |prop_name, prop_offset| {
        // Skip reserved slots (used by native functions)
        const atom_int = @intFromEnum(prop_name);
        if (atom_int >= 0xFFFE) continue;

        // Get the property name string
        const name = ctx.atoms.getName(prop_name) orelse continue;

        // Skip internal properties (children) and event handlers (onClick, etc)
        if (std.mem.eql(u8, name, "children")) continue;
        if (name.len >= 2 and name[0] == 'o' and name[1] == 'n') continue;

        const val = props_obj.getSlot(prop_offset);
        if (val.isNull() or val.isUndefined()) continue;
        if (val.isFalse()) continue;

        try writer.writeByte(' ');

        // Handle className -> class
        if (std.mem.eql(u8, name, "className")) {
            try writer.writeAll("class");
        } else {
            try writer.writeAll(name);
        }

        if (!val.isTrue()) {
            try writer.writeAll("=\"");
            if (val.isString()) {
                try escapeAttr(val.toPtr(string.JSString).data(), writer);
            } else if (val.isInt()) {
                try writer.print("{d}", .{val.getInt()});
            }
            try writer.writeByte('"');
        }
    }
}

/// Escape HTML special characters using scan-then-copy for bulk writes.
fn escapeHtml(text: []const u8, writer: *std.Io.Writer) RenderError!void {
    var start: usize = 0;
    for (text, 0..) |c, i| {
        const replacement: []const u8 = switch (c) {
            '&' => "&amp;",
            '<' => "&lt;",
            '>' => "&gt;",
            else => continue,
        };
        if (i > start) try writer.writeAll(text[start..i]);
        try writer.writeAll(replacement);
        start = i + 1;
    }
    if (start < text.len) try writer.writeAll(text[start..]);
}

/// Escape attribute value using scan-then-copy for bulk writes.
fn escapeAttr(text: []const u8, writer: anytype) !void {
    var start: usize = 0;
    for (text, 0..) |c, i| {
        const replacement: []const u8 = switch (c) {
            '&' => "&amp;",
            '"' => "&quot;",
            '\'' => "&#x27;",
            else => continue,
        };
        if (i > start) try writer.writeAll(text[start..i]);
        try writer.writeAll(replacement);
        start = i + 1;
    }
    if (start < text.len) try writer.writeAll(text[start..]);
}

/// Check if element is a void element (self-closing).
/// Uses first-char dispatch to avoid linear scan of all 14 void elements.
fn isVoidElement(tag: []const u8) bool {
    if (tag.len < 2) return false;
    return switch (tag[0]) {
        'a' => std.mem.eql(u8, tag, "area"),
        'b' => std.mem.eql(u8, tag, "base") or std.mem.eql(u8, tag, "br"),
        'c' => std.mem.eql(u8, tag, "col"),
        'e' => std.mem.eql(u8, tag, "embed"),
        'h' => std.mem.eql(u8, tag, "hr"),
        'i' => std.mem.eql(u8, tag, "img") or std.mem.eql(u8, tag, "input"),
        'l' => std.mem.eql(u8, tag, "link"),
        'm' => std.mem.eql(u8, tag, "meta"),
        'p' => std.mem.eql(u8, tag, "param"),
        's' => std.mem.eql(u8, tag, "source"),
        't' => std.mem.eql(u8, tag, "track"),
        'w' => std.mem.eql(u8, tag, "wbr"),
        else => false,
    };
}

/// Check if element should have raw (unescaped) text content
fn isRawTextElement(tag: []const u8) bool {
    return std.mem.eql(u8, tag, "style") or std.mem.eql(u8, tag, "script");
}

/// Render children without HTML escaping (for style/script elements)
fn renderRawChildren(ctx: *context.Context, children: value.JSValue, writer: anytype) !void {
    if (children.isNull() or children.isUndefined()) return;

    if (children.isStringOrRope()) {
        const text = try getStringArgWithCtx(children, ctx);
        try writer.writeAll(text);
        return;
    }

    if (children.isInt()) {
        try writer.print("{d}", .{children.getInt()});
        return;
    }

    // Handle array of children
    if (children.isObject()) {
        const children_obj = object.JSObject.fromValue(children);
        if (children_obj.class_id == .array) {
            const len = children_obj.getArrayLength();
            var i: u32 = 0;
            while (i < len) : (i += 1) {
                if (children_obj.getIndex(i)) |child| {
                    try renderRawChildren(ctx, child, writer);
                }
            }
        }
    }
}

// ============================================================================
// Helpers
// ============================================================================

/// Get string data from a JSValue (handles flat strings, ropes, and slices)
/// When ctx is provided, uses arena allocation for rope flattening to avoid
/// leaking c_allocator memory when arena resets.
fn getStringArg(val: value.JSValue) ![]const u8 {
    return getStringArgWithCtx(val, null);
}

fn getStringArgWithCtx(val: value.JSValue, ctx: ?*context.Context) ![]const u8 {
    if (val.isString()) {
        return val.toPtr(string.JSString).data();
    }
    if (val.isStringSlice()) {
        return val.toPtr(string.SliceString).data();
    }
    if (val.isRope()) {
        const rope = val.toPtr(string.RopeNode);
        if (rope.kind == .leaf) {
            return rope.payload.leaf.data();
        }
        // Flatten concat rope and cache result
        // Use arena when available to avoid leaking heap memory on arena reset
        if (ctx) |c| {
            if (c.hybrid) |hybrid| {
                if (rope.flattenWithArena(hybrid.arena)) |flat| {
                    rope.kind = .leaf;
                    rope.payload = .{ .leaf = flat };
                    return flat.data();
                }
            }
        }
        const flat = try rope.flatten(std.heap.c_allocator);
        rope.kind = .leaf;
        rope.payload = .{ .leaf = flat };
        return flat.data();
    }
    return "";
}

/// Estimate JSON size for buffer pre-allocation
fn estimateJsonSize(val: value.JSValue) usize {
    if (val.isObject()) {
        const obj = object.JSObject.fromValue(val);
        if (obj.class_id == .array) {
            return 32 + obj.getArrayLength() * 16;
        }
        // Conservative estimate for objects (property count not directly accessible)
        return 256;
    }
    if (val.isString()) {
        const str = val.toPtr(string.JSString);
        return str.len + 2; // quotes
    }
    return 32;
}

/// Fast integer to string writing (avoids format string parsing overhead)
fn writeInt(writer: *std.Io.Writer, val: i32) RenderError!void {
    var n: u32 = if (val < 0)
        @intCast(-@as(i64, val))
    else
        @intCast(val);
    var buf: [12]u8 = undefined; // -2147483648 is 11 chars + null
    var i: usize = buf.len;

    const negative = val < 0;

    // Write digits in reverse
    if (n == 0) {
        i -= 1;
        buf[i] = '0';
    } else {
        while (n > 0) {
            i -= 1;
            buf[i] = @intCast((n % 10) + '0');
            n /= 10;
        }
    }

    if (negative) {
        i -= 1;
        buf[i] = '-';
    }

    try writer.writeAll(buf[i..]);
}

/// Convert a JSValue to JSON string
pub fn valueToJson(ctx: *context.Context, val: value.JSValue) ![]u8 {
    const json = try valueToJsonScratch(ctx, val);
    const owned = try ctx.allocator.dupe(u8, json);
    ctx.json_writer.clearRetainingCapacity();
    return owned;
}

/// Convert a JSValue to JSON in a reusable buffer (valid until next call).
fn valueToJsonScratch(ctx: *context.Context, val: value.JSValue) ![]const u8 {
    const estimated = estimateJsonSize(val);
    var aw = &ctx.json_writer;
    aw.clearRetainingCapacity();
    try aw.ensureTotalCapacity(estimated);
    try writeJson(ctx, val, &aw.writer);
    return aw.written();
}

/// Convert a JSValue to a JS string using the reusable JSON buffer.
pub fn valueToJsonString(ctx: *context.Context, val: value.JSValue) !*string.JSString {
    const json = try valueToJsonScratch(ctx, val);
    // Copy into JSString storage (buffer reused for subsequent calls).
    // Use arena when hybrid mode is enabled.
    const js_str = try ctx.createStringPtr(json);
    ctx.json_writer.clearRetainingCapacity();
    return js_str;
}

fn writeJson(ctx: *context.Context, val: value.JSValue, writer: *std.Io.Writer) RenderError!void {
    if (val.isNull()) {
        try writer.writeAll("null");
    } else if (val.isUndefined()) {
        try writer.writeAll("null");
    } else if (val.isTrue()) {
        try writer.writeAll("true");
    } else if (val.isFalse()) {
        try writer.writeAll("false");
    } else if (val.isInt()) {
        try writeInt(writer, val.getInt());
    } else if (val.isFloat64()) {
        const f = val.getFloat64();
        // Handle special float values
        if (std.math.isNan(f) or std.math.isInf(f)) {
            try writer.writeAll("null");
        } else {
            try writer.print("{d}", .{f});
        }
    } else if (val.isAnyString()) {
        try writer.writeByte('"');
        // Handle all string types: flat strings, ropes, and slices
        const data = blk: {
            if (val.isString()) {
                break :blk val.toPtr(string.JSString).data();
            } else if (val.isRope()) {
                const rope = val.toPtr(string.RopeNode);
                if (ctx.hybrid) |hyb| {
                    const flat = rope.flattenWithArena(hyb.arena) orelse return RenderError.OutOfMemory;
                    break :blk flat.data();
                } else {
                    const flat = rope.flatten(ctx.allocator) catch return RenderError.OutOfMemory;
                    break :blk flat.data();
                }
            } else {
                // String slice
                const slice = val.toPtr(string.SliceString);
                break :blk slice.data();
            }
        };
        // Fast path: check if any escaping is needed
        var needs_escape = false;
        for (data) |c| {
            if (c == '"' or c == '\\' or c == '\n' or c == '\r' or c == '\t' or c < 0x20) {
                needs_escape = true;
                break;
            }
        }
        if (!needs_escape) {
            // No escaping needed - write entire string at once
            try writer.writeAll(data);
        } else {
            // Slow path: escape special characters
            for (data) |c| {
                switch (c) {
                    '"' => try writer.writeAll("\\\""),
                    '\\' => try writer.writeAll("\\\\"),
                    '\n' => try writer.writeAll("\\n"),
                    '\r' => try writer.writeAll("\\r"),
                    '\t' => try writer.writeAll("\\t"),
                    else => {
                        if (c < 0x20) {
                            // Control characters as \u00XX
                            try writer.writeAll("\\u00");
                            const hex = "0123456789abcdef";
                            try writer.writeByte(hex[c >> 4]);
                            try writer.writeByte(hex[c & 0xF]);
                        } else {
                            try writer.writeByte(c);
                        }
                    },
                }
            }
        }
        try writer.writeByte('"');
    } else if (val.isObject()) {
        const obj = object.JSObject.fromValue(val);
        if (obj.class_id == .array) {
            try writer.writeByte('[');
            const len = obj.getArrayLength();
            for (0..len) |i| {
                if (i > 0) try writer.writeByte(',');
                // Use getIndex for array elements (stored in inline_slots[1..])
                if (obj.getIndex(@intCast(i))) |elem| {
                    try writeJson(ctx, elem, writer);
                } else {
                    try writer.writeAll("null");
                }
            }
            try writer.writeByte(']');
        } else {
            try writer.writeByte('{');
            var first = true;

            // Get properties from the pool using SoA layout
            const pool = ctx.hidden_class_pool orelse {
                try writer.writeByte('}');
                return;
            };
            const class_idx = obj.hidden_class_idx;
            if (!class_idx.isNone()) {
                const idx = class_idx.toInt();
                if (idx < pool.count) {
                    const prop_count = pool.property_counts.items[idx];
                    if (prop_count > 0) {
                        const start = pool.properties_starts.items[idx];
                        if (start + prop_count > pool.property_names.items.len or
                            start + prop_count > pool.property_offsets.items.len)
                        {
                            try writer.writeByte('}');
                            return;
                        }
                        const names = pool.property_names.items[start..][0..prop_count];
                        const offsets = pool.property_offsets.items[start..][0..prop_count];

                        for (names, offsets) |prop_name, prop_offset| {
                            const v = obj.getSlot(prop_offset);
                            if (v.isUndefined()) continue;

                            // Get property name from atom table
                            const name = ctx.atoms.getName(prop_name) orelse continue;

                            if (!first) try writer.writeByte(',');
                            first = false;

                            try writer.writeByte('"');
                            try writer.writeAll(name);
                            try writer.writeAll("\":");
                            try writeJson(ctx, v, writer);
                        }
                    }
                }
            }
            try writer.writeByte('}');
        }
    } else {
        try writer.writeAll("null");
    }
}

// ============================================================================
// Tests
// ============================================================================

test "createResponse" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const resp = try createResponse(ctx, "Hello", 200, "text/plain");
    try std.testing.expect(resp.isObject());

    const resp_obj = object.JSObject.fromValue(resp);
    if (ctx.http_shapes) |shapes| {
        try std.testing.expectEqual(shapes.response.class_idx, resp_obj.hidden_class_idx);
    }
    const status_atom = try ctx.atoms.intern("status");
    const pool = ctx.hidden_class_pool.?;
    const status = resp_obj.getProperty(pool, status_atom);
    try std.testing.expect(status != null);
    try std.testing.expectEqual(@as(i32, 200), status.?.getInt());
}

test "Response helpers normalize invalid status" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const init_obj = try ctx.createObject(null);
    try ctx.setPropertyChecked(init_obj, .status, value.JSValue.fromInt(42));
    const ctx_ptr: *anyopaque = @ptrCast(@alignCast(ctx));

    const text_val = try ctx.createString("hello");
    const resp = try responseText(ctx_ptr, value.JSValue.undefined_val, &[_]value.JSValue{
        text_val,
        init_obj.toValue(),
    });
    const resp_obj = object.JSObject.fromValue(resp);
    const pool = ctx.hidden_class_pool.?;
    const status_opt = resp_obj.getProperty(pool, .status);
    try std.testing.expect(status_opt != null);
    const status = status_opt.?;
    try std.testing.expect(status.isInt());
    try std.testing.expectEqual(@as(i32, 500), status.getInt());

    const redirect = try responseRedirect(ctx_ptr, value.JSValue.undefined_val, &[_]value.JSValue{
        try ctx.createString("/next"),
        value.JSValue.fromInt(-1),
    });
    const redirect_obj = object.JSObject.fromValue(redirect);
    const redirect_status_opt = redirect_obj.getProperty(pool, .status);
    try std.testing.expect(redirect_status_opt != null);
    const redirect_status = redirect_status_opt.?;
    try std.testing.expect(redirect_status.isInt());
    try std.testing.expectEqual(@as(i32, 500), redirect_status.getInt());
}

test "escapeHtml" {
    var writer: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer writer.deinit();

    try escapeHtml("<script>alert('xss')</script>", &writer.writer);
    try std.testing.expectEqualStrings("&lt;script&gt;alert('xss')&lt;/script&gt;", writer.written());
}

test "isVoidElement" {
    try std.testing.expect(isVoidElement("br"));
    try std.testing.expect(isVoidElement("img"));
    try std.testing.expect(isVoidElement("input"));
    try std.testing.expect(!isVoidElement("div"));
    try std.testing.expect(!isVoidElement("span"));
}

test "valueToJson basic" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Integer
    {
        const json = try valueToJson(ctx, value.JSValue.fromInt(42));
        defer allocator.free(json);
        try std.testing.expectEqualStrings("42", json);
    }

    // Min i32 regression
    {
        const json = try valueToJson(ctx, value.JSValue.fromInt(std.math.minInt(i32)));
        defer allocator.free(json);
        try std.testing.expectEqualStrings("-2147483648", json);
    }

    // Boolean
    {
        const json = try valueToJson(ctx, value.JSValue.true_val);
        defer allocator.free(json);
        try std.testing.expectEqualStrings("true", json);
    }

    // Null
    {
        const json = try valueToJson(ctx, value.JSValue.null_val);
        defer allocator.free(json);
        try std.testing.expectEqualStrings("null", json);
    }
}

test "valueToJson object with properties" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Create object with two properties: {hello: "world", count: 42}
    const pool = ctx.hidden_class_pool.?;
    const obj = try object.JSObject.create(allocator, ctx.root_class_idx, null, pool);

    const hello_atom = try ctx.atoms.intern("hello");
    const hello_str = try string.createString(allocator, "world");
    try ctx.setPropertyChecked(obj, hello_atom, value.JSValue.fromPtr(hello_str));

    const count_atom = try ctx.atoms.intern("count");
    try ctx.setPropertyChecked(obj, count_atom, value.JSValue.fromInt(42));

    const json = try valueToJson(ctx, obj.toValue());
    defer allocator.free(json);

    // Must contain both properties
    try std.testing.expect(std.mem.indexOf(u8, json, "\"hello\":\"world\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"count\":42") != null);
    // Must start and end with braces
    try std.testing.expect(json.len >= 2);
    try std.testing.expectEqual(@as(u8, '{'), json[0]);
    try std.testing.expectEqual(@as(u8, '}'), json[json.len - 1]);
}

test "renderToString with attributes and nested elements" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // props = { href: "/api/health" }
    const props = try object.JSObject.create(allocator, ctx.root_class_idx, null, ctx.hidden_class_pool);
    const href_atom = try ctx.atoms.intern("href");
    const href_str = try string.createString(allocator, "/api/health");
    try ctx.setPropertyChecked(props, href_atom, value.JSValue.fromPtr(href_str));

    // child text
    const child_str = try string.createString(allocator, "GET /api/health");
    const child_val = value.JSValue.fromPtr(child_str);

    // tag "a"
    const tag_str = try string.createString(allocator, "a");
    const tag_val = value.JSValue.fromPtr(tag_str);

    const ctx_ptr: *anyopaque = @ptrCast(@alignCast(ctx));
    const node = try h(ctx_ptr, value.JSValue.undefined_val, &[_]value.JSValue{
        tag_val,
        props.toValue(),
        child_val,
    });

    const rendered = try renderToString(ctx_ptr, value.JSValue.undefined_val, &[_]value.JSValue{node});
    const rendered_str = rendered.toPtr(string.JSString).data();

    try std.testing.expectEqualStrings(
        "<a href=\"/api/health\">GET /api/health</a>",
        rendered_str,
    );
}

test "h flattens array children" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const child_arr = try ctx.createArray();
    child_arr.prototype = ctx.array_prototype;

    const child1 = try ctx.createString("a");
    const child2 = try ctx.createString("b");
    try ctx.setIndexChecked(child_arr, 0, child1);
    try ctx.setIndexChecked(child_arr, 1, child2);
    child_arr.setArrayLength(2);

    const tag_val = try ctx.createString("div");

    const ctx_ptr: *anyopaque = @ptrCast(@alignCast(ctx));
    const node = try h(ctx_ptr, value.JSValue.undefined_val, &[_]value.JSValue{
        tag_val,
        value.JSValue.undefined_val,
        child_arr.toValue(),
    });

    const pool = ctx.hidden_class_pool.?;
    const node_obj = object.JSObject.fromValue(node);
    const children_val_opt = node_obj.getProperty(pool, .children);
    try std.testing.expect(children_val_opt != null);
    const children_val = children_val_opt.?;
    try std.testing.expect(children_val.isObject());
    const children_obj = object.JSObject.fromValue(children_val);
    try std.testing.expectEqual(@as(u32, 2), children_obj.getArrayLength());
}

test "renderToString flattens nested children arrays" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const child_arr = try ctx.createArray();
    child_arr.prototype = ctx.array_prototype;

    const child1 = try ctx.createString("a");
    const child2 = try ctx.createString("b");
    try ctx.setIndexChecked(child_arr, 0, child1);
    try ctx.setIndexChecked(child_arr, 1, child2);
    child_arr.setArrayLength(2);

    const tag_val = try ctx.createString("div");

    const ctx_ptr: *anyopaque = @ptrCast(@alignCast(ctx));
    const node = try h(ctx_ptr, value.JSValue.undefined_val, &[_]value.JSValue{
        tag_val,
        value.JSValue.undefined_val,
        child_arr.toValue(),
    });

    const rendered = try renderToString(ctx_ptr, value.JSValue.undefined_val, &[_]value.JSValue{node});
    try std.testing.expectEqualStrings("<div>ab</div>", rendered.toPtr(string.JSString).data());
}

test "renderToString handles string slices" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const parent = try string.createString(allocator, "hello");
    const slice = try string.createSlice(allocator, parent, 1, 3); // "ell"

    const tag_val = try ctx.createString("div");

    const ctx_ptr: *anyopaque = @ptrCast(@alignCast(ctx));
    const node = try h(ctx_ptr, value.JSValue.undefined_val, &[_]value.JSValue{
        tag_val,
        value.JSValue.undefined_val,
        value.JSValue.fromPtr(slice),
    });

    const rendered = try renderToString(ctx_ptr, value.JSValue.undefined_val, &[_]value.JSValue{node});
    try std.testing.expectEqualStrings("<div>ell</div>", rendered.toPtr(string.JSString).data());
}

test "Hybrid: h and renderToString accept arena values" {
    const gc = @import("gc.zig");
    const heap_mod = @import("heap.zig");
    const arena_mod = @import("arena.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var heap_state = heap_mod.Heap.init(allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var req_arena = try arena_mod.Arena.init(allocator, .{ .size = 4096 });
    defer req_arena.deinit();
    var hybrid = arena_mod.HybridAllocator{
        .persistent = allocator,
        .arena = &req_arena,
    };
    ctx.setHybridAllocator(&hybrid);

    const tag_val = try ctx.createString("div");
    const child_val = try ctx.createString("hello");

    const ctx_ptr: *anyopaque = @ptrCast(@alignCast(ctx));
    const node = try h(ctx_ptr, value.JSValue.undefined_val, &[_]value.JSValue{
        tag_val,
        value.JSValue.undefined_val,
        child_val,
    });

    const rendered = try renderToString(ctx_ptr, value.JSValue.undefined_val, &[_]value.JSValue{node});
    try std.testing.expect(rendered.isString());
    try std.testing.expectEqualStrings("<div>hello</div>", rendered.toPtr(string.JSString).data());
}
