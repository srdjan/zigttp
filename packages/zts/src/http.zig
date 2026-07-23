//! HTTP Request/Response and JSX runtime for zttp integration
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
const util = @import("modules/internal/util.zig");

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
            req_obj.setSlot(shapes.request.body_slot, value.JSValue.undefined_val);
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
        try ctx.setPropertyChecked(req_obj, body_atom, value.JSValue.undefined_val);
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
    const ctx = util.castContext(ctx_ptr);

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
    const ctx = util.castContext(ctx_ptr);

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
    const ctx = util.castContext(ctx_ptr);

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
    const ctx = util.castContext(ctx_ptr);

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
    const ctx = util.castContext(ctx_ptr);

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
    const ctx = util.castContext(ctx_ptr);

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
    const ctx = util.castContext(ctx_ptr);

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
    const ctx = util.castContext(ctx_ptr);

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

const RenderError = std.Io.Writer.Error || error{ OutOfMemory, ArenaObjectEscape, NoHiddenClassPool, JsonDepthExceeded };

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
    try writeJsonDepth(ctx, val, &aw.writer, 0);
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

const JSON_MAX_DEPTH: u32 = 512;

fn writeJsonDepth(ctx: *context.Context, val: value.JSValue, writer: *std.Io.Writer, depth: u32) RenderError!void {
    if (depth >= JSON_MAX_DEPTH) return error.JsonDepthExceeded;
    return writeJson(ctx, val, writer, depth);
}

fn writeJson(ctx: *context.Context, val: value.JSValue, writer: *std.Io.Writer, depth: u32) RenderError!void {
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
                    try writeJsonDepth(ctx, elem, writer, depth + 1);
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
                            try writeJsonDepth(ctx, v, writer, depth + 1);
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
// Hypermedia Resource (zttp:hypermedia)
// ============================================================================

/// A GET/HEAD affordance becomes a HAL `_links` entry; any other method
/// becomes a HAL-FORMS `_templates` entry. This single predicate is the one
/// branch point shared by the HAL and HTMX renderings.
fn affordanceIsLink(method: ?[]const u8) bool {
    const m = method orelse return true;
    return std.ascii.eqlIgnoreCase(m, "GET") or std.ascii.eqlIgnoreCase(m, "HEAD");
}

/// Read a property as a JSValue, or null when absent.
fn readObjProp(ctx: *context.Context, obj: *object.JSObject, name: []const u8) ?value.JSValue {
    const pool = ctx.hidden_class_pool orelse return null;
    const atom = ctx.atoms.intern(name) catch return null;
    return obj.getProperty(pool, atom);
}

/// An own (atom, slot-offset) pair, snapshotted out of the shared hidden-class
/// pool so a caller can safely create objects / set properties (which mutate
/// the pool's backing arrays) while iterating an object's own properties.
const OwnProp = struct { atom: object.Atom, offset: u16 };

/// Copy `obj`'s own non-reserved (atom, offset) pairs into an owned slice.
/// Mirrors getOwnEnumerableKeys' copy-first safety: holding a raw slice into
/// pool.property_names/property_offsets across a setPropertyChecked call is a
/// use-after-free, because addProperty appends to those very arrays and can
/// reallocate them. Caller frees the returned slice.
fn snapshotOwnProps(ctx: *context.Context, obj: *object.JSObject) ![]OwnProp {
    const pool = ctx.hidden_class_pool orelse return ctx.allocator.alloc(OwnProp, 0);
    const class_idx = obj.hidden_class_idx;
    if (class_idx.isNone()) return ctx.allocator.alloc(OwnProp, 0);
    const idx = class_idx.toInt();
    if (idx >= pool.count) return ctx.allocator.alloc(OwnProp, 0);

    const prop_count = pool.property_counts.items[idx];
    const start = pool.properties_starts.items[idx];
    const names = pool.property_names.items[start..][0..prop_count];
    const offsets = pool.property_offsets.items[start..][0..prop_count];

    const out = try ctx.allocator.alloc(OwnProp, prop_count);
    for (names, offsets, 0..) |atom, off, i| out[i] = .{ .atom = atom, .offset = off };
    return out;
}

/// Read a string property's bytes, or null when absent / not a string.
fn readObjStrProp(ctx: *context.Context, obj: *object.JSObject, name: []const u8) !?[]const u8 {
    const v = readObjProp(ctx, obj, name) orelse return null;
    if (!v.isAnyString()) return null;
    return try getStringArgWithCtx(v, ctx);
}

/// Copy `src`'s own (non-reserved, defined) properties onto `dst`, preserving
/// insertion order. Data fields named `_links`/`_templates` are skipped so the
/// generated HAL affordances are not silently clobbered (and vice versa).
/// Values are shared by reference; serialization deep-copies.
fn appendOwnProps(ctx: *context.Context, src: *object.JSObject, dst: *object.JSObject) !void {
    const props = try snapshotOwnProps(ctx, src);
    defer ctx.allocator.free(props);

    for (props) |p| {
        if (@intFromEnum(p.atom) >= 0xFFFE) continue;
        const name = ctx.atoms.getName(p.atom) orelse continue;
        if (std.mem.eql(u8, name, "_links") or std.mem.eql(u8, name, "_templates")) continue;
        const v = src.getSlot(p.offset);
        if (v.isUndefined()) continue;
        try ctx.setPropertyChecked(dst, p.atom, v);
    }
}

/// Build the HAL-JSON serialization of a hypermedia resource: `data`'s own
/// properties merged with `_links` (GET/HEAD affordances) and `_templates`
/// (write affordances) derived from `affordances`. One walk over the
/// affordance set; see also buildHtmlFragment for the HTMX rendering.
pub fn buildHalJson(
    ctx: *context.Context,
    data: value.JSValue,
    affordances: value.JSValue,
) !*string.JSString {
    const result = try ctx.createObject(null);

    // 1. Copy the resource data as the body of the HAL document.
    if (data.isObject()) {
        try appendOwnProps(ctx, object.JSObject.fromValue(data), result);
    }

    // 2. Walk affordances once, splitting into _links (GET) and _templates (write).
    var links: ?*object.JSObject = null;
    var templates: ?*object.JSObject = null;

    if (affordances.isObject()) {
        const aff_obj = object.JSObject.fromValue(affordances);
        // Snapshot first: the loop creates link/template objects, which mutate
        // the shared hidden-class pool and would dangle a raw slice into it.
        const props = try snapshotOwnProps(ctx, aff_obj);
        defer ctx.allocator.free(props);

        for (props) |p| {
            if (@intFromEnum(p.atom) >= 0xFFFE) continue;
            const aff_val = aff_obj.getSlot(p.offset);
            if (!aff_val.isObject()) continue;
            const a = object.JSObject.fromValue(aff_val);

            const method = try readObjStrProp(ctx, a, "method");
            const href_val = readObjProp(ctx, a, "href") orelse value.JSValue.undefined_val;
            const title_val = readObjProp(ctx, a, "title");

            if (affordanceIsLink(method)) {
                if (links == null) links = try ctx.createObject(null);
                const l = try ctx.createObject(null);
                try ctx.setPropertyChecked(l, try ctx.atoms.intern("href"), href_val);
                if (title_val) |tv| try ctx.setPropertyChecked(l, try ctx.atoms.intern("title"), tv);
                try ctx.setPropertyChecked(links.?, p.atom, l.toValue());
            } else {
                if (templates == null) templates = try ctx.createObject(null);
                const t = try ctx.createObject(null);
                if (readObjProp(ctx, a, "method")) |mv| try ctx.setPropertyChecked(t, try ctx.atoms.intern("method"), mv);
                try ctx.setPropertyChecked(t, try ctx.atoms.intern("target"), href_val);
                if (title_val) |tv| try ctx.setPropertyChecked(t, try ctx.atoms.intern("title"), tv);
                if (readObjProp(ctx, a, "fields")) |fv| try ctx.setPropertyChecked(t, try ctx.atoms.intern("properties"), fv);
                try ctx.setPropertyChecked(templates.?, p.atom, t.toValue());
            }
        }
    }

    if (links) |l| try ctx.setPropertyChecked(result, try ctx.atoms.intern("_links"), l.toValue());
    if (templates) |t| try ctx.setPropertyChecked(result, try ctx.atoms.intern("_templates"), t.toValue());

    return valueToJsonString(ctx, result.toValue());
}

/// Build the HTMX/HTML fragment for a hypermedia resource: scalar `data`
/// fields as a <dl>, then the affordance controls (GET -> <a>, write ->
/// <button hx-{method}>). Same affordance set as buildHalJson.
pub fn buildHtmlFragment(
    ctx: *context.Context,
    data: value.JSValue,
    affordances: value.JSValue,
) !*string.JSString {
    var aw = &ctx.render_writer;
    aw.clearRetainingCapacity();
    const w = &aw.writer;

    try w.writeAll("<div class=\"resource\">");

    // Scalar data fields as a definition list (nested objects are _embedded, omitted here).
    if (data.isObject()) {
        const d = object.JSObject.fromValue(data);
        const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
        const class_idx = d.hidden_class_idx;
        if (!class_idx.isNone()) {
            const idx = class_idx.toInt();
            if (idx < pool.count) {
                const prop_count = pool.property_counts.items[idx];
                if (prop_count > 0) {
                    const start = pool.properties_starts.items[idx];
                    const names = pool.property_names.items[start..][0..prop_count];
                    const offsets = pool.property_offsets.items[start..][0..prop_count];

                    var opened = false;
                    for (names, offsets) |name_atom, offset| {
                        if (@intFromEnum(name_atom) >= 0xFFFE) continue;
                        const v = d.getSlot(offset);
                        if (v.isUndefined() or v.isNull() or v.isObject()) continue;
                        const key = ctx.atoms.getName(name_atom) orelse continue;
                        if (!opened) {
                            try w.writeAll("<dl>");
                            opened = true;
                        }
                        try w.writeAll("<dt>");
                        try escapeHtml(key, w);
                        try w.writeAll("</dt><dd>");
                        if (v.isInt()) {
                            try w.print("{d}", .{v.getInt()});
                        } else if (v.isFloat64()) {
                            try w.print("{d}", .{v.getFloat64()});
                        } else if (v.isTrue()) {
                            try w.writeAll("true");
                        } else if (v.isFalse()) {
                            try w.writeAll("false");
                        } else if (v.isAnyString()) {
                            try escapeHtml(try getStringArgWithCtx(v, ctx), w);
                        }
                        try w.writeAll("</dd>");
                    }
                    if (opened) try w.writeAll("</dl>");
                }
            }
        }
    }

    // Affordance controls.
    if (affordances.isObject()) {
        const aff_obj = object.JSObject.fromValue(affordances);
        const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
        const class_idx = aff_obj.hidden_class_idx;
        if (!class_idx.isNone()) {
            const idx = class_idx.toInt();
            if (idx < pool.count) {
                const prop_count = pool.property_counts.items[idx];
                const start = pool.properties_starts.items[idx];
                const names = pool.property_names.items[start..][0..prop_count];
                const offsets = pool.property_offsets.items[start..][0..prop_count];

                for (names, offsets) |rel_atom, rel_offset| {
                    if (@intFromEnum(rel_atom) >= 0xFFFE) continue;
                    const aff_val = aff_obj.getSlot(rel_offset);
                    if (!aff_val.isObject()) continue;
                    const a = object.JSObject.fromValue(aff_val);

                    const rel = ctx.atoms.getName(rel_atom) orelse "";
                    const href = (try readObjStrProp(ctx, a, "href")) orelse "";
                    const method = try readObjStrProp(ctx, a, "method");
                    const title = (try readObjStrProp(ctx, a, "title")) orelse rel;
                    const target = try readObjStrProp(ctx, a, "target");
                    const swap = try readObjStrProp(ctx, a, "swap");

                    if (affordanceIsLink(method)) {
                        try w.writeAll("<a href=\"");
                        try escapeAttr(href, w);
                        try w.writeAll("\">");
                        try escapeHtml(title, w);
                        try w.writeAll("</a>");
                    } else {
                        try w.writeAll("<button hx-");
                        // The method forms the attribute NAME, which escapeAttr
                        // cannot protect; restrict to ASCII letters so a method
                        // derived from untrusted input cannot inject attributes.
                        for (method.?) |c| {
                            if (std.ascii.isAlphabetic(c)) try w.writeByte(std.ascii.toLower(c));
                        }
                        try w.writeAll("=\"");
                        try escapeAttr(href, w);
                        try w.writeByte('"');
                        if (target) |t| {
                            try w.writeAll(" hx-target=\"");
                            try escapeAttr(t, w);
                            try w.writeByte('"');
                        }
                        if (swap) |s| {
                            try w.writeAll(" hx-swap=\"");
                            try escapeAttr(s, w);
                            try w.writeByte('"');
                        }
                        try w.writeByte('>');
                        try escapeHtml(title, w);
                        try w.writeAll("</button>");
                    }
                }
            }
        }
    }

    try w.writeAll("</div>");
    return ctx.createStringPtr(aw.written());
}

/// Reserved property names used to brand and carry a hypermedia resource.
/// Invisible to JSON serialization because the branded object is never
/// serialized directly - renderResource extracts data/affordances and builds
/// a fresh HAL document or HTML fragment.
const resource_brand = "__zttp_resource";
const resource_data = "__hm_data";
const resource_aff = "__hm_aff";

/// resource(data, affordances) - build a branded hypermedia resource. The
/// runtime detects the brand at the response boundary and renders it as HAL
/// or HTMX per content negotiation (see renderResource).
pub fn resource(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const data = if (args.len > 0) args[0] else value.JSValue.undefined_val;
    const affordances = if (args.len > 1) args[1] else value.JSValue.undefined_val;

    const obj = try ctx.createObject(null);
    try ctx.setPropertyChecked(obj, try ctx.atoms.intern(resource_brand), value.JSValue.true_val);
    try ctx.setPropertyChecked(obj, try ctx.atoms.intern(resource_data), data);
    try ctx.setPropertyChecked(obj, try ctx.atoms.intern(resource_aff), affordances);
    return obj.toValue();
}

/// Read the affordances object from a branded hypermedia resource, or null if
/// `val` is not a resource() (or carries no affordances). Used by
/// `zttp:workflow.follow` to resolve an affordance `rel` to a dispatch target
/// without re-rendering the resource at the response boundary.
pub fn resourceAffordances(ctx: *context.Context, val: value.JSValue) ?value.JSValue {
    if (!isResource(ctx, val)) return null;
    const obj = object.JSObject.fromValue(val);
    return readObjProp(ctx, obj, resource_aff);
}

/// True when `val` is a branded hypermedia resource produced by resource().
pub fn isResource(ctx: *context.Context, val: value.JSValue) bool {
    if (!val.isObject()) return false;
    const obj = object.JSObject.fromValue(val);
    const pool = ctx.hidden_class_pool orelse return false;
    const atom = ctx.atoms.intern(resource_brand) catch return false;
    // Own property only - the normal Response path uses getOwnProperty too, so
    // an object that merely inherits the brand is not misclassified.
    const brand = obj.getOwnProperty(pool, atom) orelse return false;
    return brand.isTrue();
}

/// A service consumer wants HAL when it accepts hal+json or generic JSON;
/// everything else (browsers, */*) gets HTML. Minimal substring negotiation;
/// the runtime boundary may pre-negotiate with zttp:http.negotiate.
fn wantsHal(accept: []const u8) bool {
    return std.mem.indexOf(u8, accept, "hal+json") != null or
        std.mem.indexOf(u8, accept, "application/json") != null;
}

/// Wrap an HTMX fragment in a full-page shell that loads htmx (used for
/// non-HX navigations / direct browser loads).
fn wrapHtmlShell(ctx: *context.Context, fragment: []const u8) !*string.JSString {
    var aw = &ctx.render_writer;
    aw.clearRetainingCapacity();
    const w = &aw.writer;
    try w.writeAll("<!doctype html><html><head><meta charset=\"utf-8\">");
    try w.writeAll("<script src=\"https://unpkg.com/htmx.org@2\"></script></head><body>");
    try w.writeAll(fragment);
    try w.writeAll("</body></html>");
    return ctx.createStringPtr(aw.written());
}

/// Render a branded hypermedia resource to a Response per content negotiation:
/// HAL-JSON for services, an HTMX fragment for HX requests, a full HTML page
/// otherwise.
pub fn renderResource(
    ctx: *context.Context,
    branded: value.JSValue,
    accept: []const u8,
    hx_request: bool,
) !value.JSValue {
    const obj = object.JSObject.fromValue(branded);
    const data = readObjProp(ctx, obj, resource_data) orelse value.JSValue.undefined_val;
    const aff = readObjProp(ctx, obj, resource_aff) orelse value.JSValue.undefined_val;

    if (wantsHal(accept)) {
        const json = try buildHalJson(ctx, data, aff);
        return createResponseFromString(ctx, json, 200, "application/hal+json");
    }

    const frag = try buildHtmlFragment(ctx, data, aff);
    if (hx_request) {
        return createResponseFromString(ctx, frag, 200, "text/html; charset=utf-8");
    }
    const page = try wrapHtmlShell(ctx, frag.data());
    return createResponseFromString(ctx, page, 200, "text/html; charset=utf-8");
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

test "buildHalJson merges data and _links for GET affordances" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // data = { id: 42 }
    const data = try ctx.createObject(null);
    try ctx.setPropertyChecked(data, try ctx.atoms.intern("id"), value.JSValue.fromInt(42));

    // affordances = { self: { href: "/orders/42" } }
    const self_aff = try ctx.createObject(null);
    try ctx.setPropertyChecked(self_aff, try ctx.atoms.intern("href"), try ctx.createString("/orders/42"));
    const affordances = try ctx.createObject(null);
    try ctx.setPropertyChecked(affordances, try ctx.atoms.intern("self"), self_aff.toValue());

    const json_str = try buildHalJson(ctx, data.toValue(), affordances.toValue());
    const json = json_str.data();

    try std.testing.expect(std.mem.indexOf(u8, json, "\"id\":42") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"_links\":{\"self\":{\"href\":\"/orders/42\"}}") != null);
}

test "buildHalJson stays correct across many affordances (no pool-realloc UAF)" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // data with several fields, like the shipped example.
    const data = try ctx.createObject(null);
    try ctx.setPropertyChecked(data, try ctx.atoms.intern("id"), value.JSValue.fromInt(42));
    try ctx.setPropertyChecked(data, try ctx.atoms.intern("total"), value.JSValue.fromInt(1999));
    try ctx.setPropertyChecked(data, try ctx.atoms.intern("status"), try ctx.createString("pending"));

    // Build N distinct affordances so the loop iterates many times while
    // creating link/template child objects (which mutate the hidden-class pool).
    const affordances = try ctx.createObject(null);
    const rels = [_][]const u8{ "self", "next", "prev", "pay", "cancel", "ship", "refund", "audit" };
    for (rels) |rel| {
        const a = try ctx.createObject(null);
        try ctx.setPropertyChecked(a, try ctx.atoms.intern("href"), try ctx.createString("/x"));
        // Make half of them write actions (templates), half links.
        if (rel.len % 2 == 0) {
            try ctx.setPropertyChecked(a, try ctx.atoms.intern("method"), try ctx.createString("POST"));
        }
        try ctx.setPropertyChecked(affordances, try ctx.atoms.intern(rel), a.toValue());
    }

    const json_str = try buildHalJson(ctx, data.toValue(), affordances.toValue());
    const json = json_str.data();

    // Every data field and every rel must appear exactly once and intact -
    // a pool-realloc UAF would corrupt rel keys / drop entries / crash.
    try std.testing.expect(std.mem.indexOf(u8, json, "\"id\":42") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"status\":\"pending\"") != null);
    for (rels) |rel| {
        try std.testing.expect(std.mem.indexOf(u8, json, rel) != null);
    }
}

test "buildHtmlFragment neutralizes a method that would inject attributes" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const data = try ctx.createObject(null);
    // A hostile method value: spaces/quotes/equals that would break out of the
    // attribute name if written raw.
    const evil = try ctx.createObject(null);
    try ctx.setPropertyChecked(evil, try ctx.atoms.intern("href"), try ctx.createString("/x"));
    try ctx.setPropertyChecked(evil, try ctx.atoms.intern("method"), try ctx.createString("post onclick=alert(1) x"));
    const affordances = try ctx.createObject(null);
    try ctx.setPropertyChecked(affordances, try ctx.atoms.intern("go"), evil.toValue());

    const frag = try buildHtmlFragment(ctx, data.toValue(), affordances.toValue());
    const html = frag.data();

    // Attribute-breaking characters (space, '=', '(') from the method are
    // stripped, so it cannot become an event handler or a second attribute.
    // Only ASCII letters survive, collapsed into one harmless attribute name.
    try std.testing.expect(std.mem.indexOf(u8, html, "onclick=") == null);
    try std.testing.expect(std.mem.indexOf(u8, html, "alert(") == null);
    try std.testing.expect(std.mem.indexOf(u8, html, "hx-postonclickalertx=\"/x\"") != null);
}

test "buildHalJson does not let a data _links field clobber generated affordances" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // data carries a field literally named _links - it must not survive into
    // the HAL document and overwrite the generated _links.
    const data = try ctx.createObject(null);
    try ctx.setPropertyChecked(data, try ctx.atoms.intern("id"), value.JSValue.fromInt(1));
    try ctx.setPropertyChecked(data, try ctx.atoms.intern("_links"), try ctx.createString("BOGUS"));

    const self_aff = try ctx.createObject(null);
    try ctx.setPropertyChecked(self_aff, try ctx.atoms.intern("href"), try ctx.createString("/y"));
    const affordances = try ctx.createObject(null);
    try ctx.setPropertyChecked(affordances, try ctx.atoms.intern("self"), self_aff.toValue());

    const json_str = try buildHalJson(ctx, data.toValue(), affordances.toValue());
    const json = json_str.data();

    try std.testing.expect(std.mem.indexOf(u8, json, "BOGUS") == null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"_links\":{\"self\":{\"href\":\"/y\"}}") != null);
}

test "buildHtmlFragment renders data dl and hx controls" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // data = { id: 42 }
    const data = try ctx.createObject(null);
    try ctx.setPropertyChecked(data, try ctx.atoms.intern("id"), value.JSValue.fromInt(42));

    // self: GET link
    const self_aff = try ctx.createObject(null);
    try ctx.setPropertyChecked(self_aff, try ctx.atoms.intern("href"), try ctx.createString("/orders/42"));

    // pay: POST action with target/swap
    const pay_aff = try ctx.createObject(null);
    try ctx.setPropertyChecked(pay_aff, try ctx.atoms.intern("href"), try ctx.createString("/orders/42/pay"));
    try ctx.setPropertyChecked(pay_aff, try ctx.atoms.intern("method"), try ctx.createString("POST"));
    try ctx.setPropertyChecked(pay_aff, try ctx.atoms.intern("title"), try ctx.createString("Pay now"));
    try ctx.setPropertyChecked(pay_aff, try ctx.atoms.intern("target"), try ctx.createString("#order"));
    try ctx.setPropertyChecked(pay_aff, try ctx.atoms.intern("swap"), try ctx.createString("outerHTML"));

    const affordances = try ctx.createObject(null);
    try ctx.setPropertyChecked(affordances, try ctx.atoms.intern("self"), self_aff.toValue());
    try ctx.setPropertyChecked(affordances, try ctx.atoms.intern("pay"), pay_aff.toValue());

    const frag = try buildHtmlFragment(ctx, data.toValue(), affordances.toValue());

    try std.testing.expectEqualStrings(
        "<div class=\"resource\"><dl><dt>id</dt><dd>42</dd></dl>" ++
            "<a href=\"/orders/42\">self</a>" ++
            "<button hx-post=\"/orders/42/pay\" hx-target=\"#order\" hx-swap=\"outerHTML\">Pay now</button></div>",
        frag.data(),
    );
}

fn respBodyForTest(ctx: *context.Context, resp: value.JSValue) []const u8 {
    const obj = object.JSObject.fromValue(resp);
    if (ctx.http_shapes) |shapes| {
        const b = obj.getSlot(shapes.response.body_slot);
        if (b.isString()) return b.toPtr(string.JSString).data();
    }
    const pool = ctx.hidden_class_pool orelse return "";
    if (obj.getProperty(pool, object.Atom.body)) |b| {
        if (b.isString()) return b.toPtr(string.JSString).data();
    }
    return "";
}

test "renderResource negotiates HAL JSON, HTMX fragment, and full page" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();
    const ctx_ptr: *anyopaque = @ptrCast(@alignCast(ctx));

    const data = try ctx.createObject(null);
    try ctx.setPropertyChecked(data, try ctx.atoms.intern("id"), value.JSValue.fromInt(9));
    const self_aff = try ctx.createObject(null);
    try ctx.setPropertyChecked(self_aff, try ctx.atoms.intern("href"), try ctx.createString("/x/9"));
    const aff = try ctx.createObject(null);
    try ctx.setPropertyChecked(aff, try ctx.atoms.intern("self"), self_aff.toValue());

    const res = try resource(ctx_ptr, value.JSValue.undefined_val, &[_]value.JSValue{ data.toValue(), aff.toValue() });

    // Service -> HAL JSON
    const hal = try renderResource(ctx, res, "application/hal+json", false);
    const hb = respBodyForTest(ctx, hal);
    try std.testing.expect(std.mem.indexOf(u8, hb, "\"id\":9") != null);
    try std.testing.expect(std.mem.indexOf(u8, hb, "\"_links\"") != null);

    // HX request -> bare fragment, no shell
    const frag = try renderResource(ctx, res, "text/html", true);
    const fb = respBodyForTest(ctx, frag);
    try std.testing.expect(std.mem.startsWith(u8, fb, "<div class=\"resource\">"));
    try std.testing.expect(std.mem.indexOf(u8, fb, "<!doctype") == null);

    // Direct browser load -> full page shell wrapping the fragment
    const page = try renderResource(ctx, res, "text/html", false);
    const pb = respBodyForTest(ctx, page);
    try std.testing.expect(std.mem.indexOf(u8, pb, "<!doctype html") != null);
    try std.testing.expect(std.mem.indexOf(u8, pb, "<div class=\"resource\">") != null);
}

test "resource builds branded object detected by isResource" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();
    const ctx_ptr: *anyopaque = @ptrCast(@alignCast(ctx));

    const data = try ctx.createObject(null);
    try ctx.setPropertyChecked(data, try ctx.atoms.intern("id"), value.JSValue.fromInt(7));
    const aff = try ctx.createObject(null);

    const res = try resource(ctx_ptr, value.JSValue.undefined_val, &[_]value.JSValue{ data.toValue(), aff.toValue() });
    try std.testing.expect(isResource(ctx, res));

    // A plain object (and undefined) is not a resource.
    const plain = try ctx.createObject(null);
    try std.testing.expect(!isResource(ctx, plain.toValue()));
    try std.testing.expect(!isResource(ctx, value.JSValue.undefined_val));
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
