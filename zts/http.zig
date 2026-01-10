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
    const resp_obj = try ctx.createObject(null);

    // Set body (using predefined atom for efficiency)
    const body_str = try ctx.createString(body);
    try ctx.setPropertyChecked(resp_obj, object.Atom.body, body_str);

    // Set status (using predefined atom)
    try ctx.setPropertyChecked(resp_obj, object.Atom.status, value.JSValue.fromInt(@intCast(status)));

    // Set statusText
    const status_text_atom = try ctx.atoms.intern("statusText");
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
    const status_text_str = try ctx.createString(status_text);
    try ctx.setPropertyChecked(resp_obj, status_text_atom, status_text_str);

    // Set ok (status 200-299)
    const ok_atom = try ctx.atoms.intern("ok");
    try ctx.setPropertyChecked(resp_obj, ok_atom, value.JSValue.fromBool(status >= 200 and status < 300));

    // Set headers (using predefined atom)
    const headers_obj = try ctx.createObject(null);
    const ct_atom = try ctx.atoms.intern("Content-Type");
    const ct_str = try ctx.createString(content_type);
    try ctx.setPropertyChecked(headers_obj, ct_atom, ct_str);
    try ctx.setPropertyChecked(resp_obj, object.Atom.headers, headers_obj.toValue());

    return resp_obj.toValue();
}

/// Response.json(data) - Create JSON response
pub fn responseJson(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) {
        return createResponse(ctx, "{}", 200, "application/json");
    }

    // Serialize the data to JSON
    const data = args[0];
    const json_str = try valueToJson(ctx, data);
    defer ctx.allocator.free(json_str);

    var status: u16 = 200;
    if (args.len > 1 and args[1].isObject()) {
        const init = object.JSObject.fromValue(args[1]);
        const status_atom = ctx.atoms.intern("status") catch return createResponse(ctx, json_str, status, "application/json");
        if (init.getProperty(status_atom)) |s| {
            if (s.isInt()) status = @intCast(s.getInt());
        }
    }

    return createResponse(ctx, json_str, status, "application/json");
}

/// Response.text(text) - Create text response
pub fn responseText(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) {
        return createResponse(ctx, "", 200, "text/plain; charset=utf-8");
    }

    const text = try getStringArg(args[0]);
    var status: u16 = 200;
    if (args.len > 1 and args[1].isObject()) {
        const init = object.JSObject.fromValue(args[1]);
        const status_atom = ctx.atoms.intern("status") catch return createResponse(ctx, text, status, "text/plain; charset=utf-8");
        if (init.getProperty(status_atom)) |s| {
            if (s.isInt()) status = @intCast(s.getInt());
        }
    }

    return createResponse(ctx, text, status, "text/plain; charset=utf-8");
}

/// Response.html(html) - Create HTML response
pub fn responseHtml(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) {
        return createResponse(ctx, "", 200, "text/html; charset=utf-8");
    }

    const html = try getStringArg(args[0]);
    var status: u16 = 200;
    if (args.len > 1 and args[1].isObject()) {
        const init = object.JSObject.fromValue(args[1]);
        const status_atom = ctx.atoms.intern("status") catch return createResponse(ctx, html, status, "text/html; charset=utf-8");
        if (init.getProperty(status_atom)) |s| {
            if (s.isInt()) status = @intCast(s.getInt());
        }
    }

    return createResponse(ctx, html, status, "text/html; charset=utf-8");
}

/// Response.redirect(url, status?) - Create redirect response
pub fn responseRedirect(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) {
        return value.JSValue.undefined_val;
    }

    const url = try getStringArg(args[0]);
    var status: u16 = 302;
    if (args.len > 1 and args[1].isInt()) {
        status = @intCast(args[1].getInt());
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
        body = try getStringArg(args[0]);
    }

    // Get init options from second argument
    if (args.len > 1 and args[1].isObject()) {
        const init = object.JSObject.fromValue(args[1]);

        // Get status
        const status_atom = ctx.atoms.intern("status") catch return createResponse(ctx, body, status, content_type);
        if (init.getProperty(status_atom)) |s| {
            if (s.isInt()) status = @intCast(s.getInt());
        }

        // Get headers for content-type
        const headers_atom = ctx.atoms.intern("headers") catch return createResponse(ctx, body, status, content_type);
        if (init.getProperty(headers_atom)) |hdr| {
            if (hdr.isObject()) {
                const headers_obj = object.JSObject.fromValue(hdr);
                const ct_atom = ctx.atoms.intern("Content-Type") catch return createResponse(ctx, body, status, content_type);
                if (headers_obj.getProperty(ct_atom)) |ct| {
                    if (ct.isString()) {
                        content_type = try getStringArg(ct);
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

/// Fragment marker for JSX
pub const FRAGMENT_MARKER = "__fragment__";

/// h(tag, props, ...children) - Create virtual DOM node
pub fn h(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    // Debug: log what h() receives
    if (args.len > 0) {
        const tag_arg = args[0];
        if (tag_arg.isString()) {
            std.log.debug("h() called with string tag: {s}", .{tag_arg.toPtr(string.JSString).data()});
        } else if (tag_arg.isCallable()) {
            std.log.debug("h() called with callable tag", .{});
        } else if (tag_arg.isUndefined()) {
            std.log.debug("h() called with UNDEFINED tag!", .{});
        } else if (tag_arg.isNull()) {
            std.log.debug("h() called with NULL tag!", .{});
        } else {
            std.log.debug("h() called with unknown tag type: {x}", .{tag_arg.raw});
        }
    } else {
        std.log.debug("h() called with NO args!", .{});
    }

    // Create node object with { tag, props, children }
    const node = try ctx.createObject(null);

    // Set tag (first arg)
    const tag_atom: object.Atom = .tag;
    if (args.len > 0) {
        try ctx.setPropertyChecked(node, tag_atom, args[0]);
    } else {
        const div_str = try ctx.createString("div");
        try ctx.setPropertyChecked(node, tag_atom, div_str);
    }

    // Set props (second arg or empty object)
    const props_atom: object.Atom = .props;
    if (args.len > 1 and args[1].isObject()) {
        try ctx.setPropertyChecked(node, props_atom, args[1]);
    } else {
        const empty_props = try ctx.createObject(null);
        try ctx.setPropertyChecked(node, props_atom, empty_props.toValue());
    }

    // Collect children (remaining args)
    const children_atom: object.Atom = .children;
    const children_arr = try ctx.createArray();
    children_arr.prototype = ctx.array_prototype;

    var child_count: u32 = 0;
    for (args[2..]) |child| {
        // Skip null/undefined
        if (child.isNull() or child.isUndefined()) continue;

        // TODO: Flatten arrays of children
        try ctx.setIndexChecked(children_arr, child_count, child);
        child_count += 1;
    }
    children_arr.setArrayLength(child_count);
    try ctx.setPropertyChecked(node, children_atom, children_arr.toValue());

    return node.toValue();
}

/// renderToString(node) - Render virtual DOM to HTML string
pub fn renderToString(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) {
        return ctx.createString("") catch return value.JSValue.undefined_val;
    }

    var buffer = std.ArrayList(u8).empty;
    defer buffer.deinit(ctx.allocator);

    var aw: std.Io.Writer.Allocating = .fromArrayList(ctx.allocator, &buffer);
    try renderNode(ctx, args[0], &aw.writer);
    buffer = aw.toArrayList();

    return ctx.createString(buffer.items) catch return value.JSValue.undefined_val;
}

const RenderError = std.Io.Writer.Error || error{OutOfMemory};

/// Render a single node to the writer
fn renderNode(ctx: *context.Context, node: value.JSValue, writer: *std.Io.Writer) RenderError!void {
    // Handle null/undefined
    if (node.isNull() or node.isUndefined()) return;

    // Handle string
    if (node.isString()) {
        const str = node.toPtr(string.JSString);
        try escapeHtml(str.data(), writer);
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

        // Check for tag property (vnode)
        const tag_val = obj.getProperty(.tag) orelse return;

        // Handle function components (tag is a callable function)
        if (tag_val.isCallable()) {
            const call_fn = call_function_callback orelse return;

            // Get props (or create empty object if not present but children exist)
            var props_val = obj.getProperty(.props) orelse value.JSValue.null_val;
            const children_val = obj.getProperty(.children);

            // Add children to props if present
            if (children_val) |cv| {
                if (props_val.isObject()) {
                    // Add children to existing props
                    const props_obj = object.JSObject.fromValue(props_val);
                    ctx.setPropertyChecked(props_obj, .children, cv) catch {};
                } else {
                    // Create new props object with children
                    const new_props = ctx.createObject(null) catch return;
                    ctx.setPropertyChecked(new_props, .children, cv) catch {};
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

        // Fragment: just render children
        if (tag_val.isString()) {
            const tag_str = tag_val.toPtr(string.JSString);
            if (std.mem.eql(u8, tag_str.data(), FRAGMENT_MARKER)) {
                if (obj.getProperty(.children)) |children| {
                    try renderChildren(ctx, children, writer);
                }
                return;
            }

            // HTML element
            try writer.writeByte('<');
            try writer.writeAll(tag_str.data());

            // Render attributes from props
            if (obj.getProperty(.props)) |props_val| {
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
            if (obj.getProperty(.children)) |children| {
                if (isRawTextElement(tag_data)) {
                    try renderRawChildren(children, writer);
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

/// Render array of children or single child
fn renderChildren(ctx: *context.Context, children: value.JSValue, writer: anytype) !void {
    // Handle null/undefined
    if (children.isNull() or children.isUndefined()) return;

    // Handle string children
    if (children.isString()) {
        const str = children.toPtr(string.JSString);
        try escapeHtml(str.data(), writer);
        return;
    }

    // Handle number children
    if (children.isInt()) {
        try writer.print("{d}", .{children.getInt()});
        return;
    }

    // Handle object (array or single vnode)
    if (!children.isObject()) return;

    const children_obj = object.JSObject.fromValue(children);

    // If it's an array, iterate
    if (children_obj.class_id == .array) {
        const len = children_obj.getArrayLength();
        var i: u32 = 0;
        while (i < len) : (i += 1) {
            if (children_obj.getIndex(i)) |child| {
                try renderNode(ctx, child, writer);
            }
        }
    } else {
        // Single child vnode - render it directly
        try renderNode(ctx, children, writer);
    }
}

/// Render HTML attributes from props object
fn renderAttributes(ctx: *context.Context, props: value.JSValue, writer: anytype) !void {
    if (!props.isObject()) return;

    const props_obj = object.JSObject.fromValue(props);

    // Iterate through properties (simplified - uses hidden class properties)
    for (props_obj.hidden_class.properties) |prop| {
        // Skip reserved slots (used by native functions)
        const atom_int = @intFromEnum(prop.name);
        if (atom_int >= 0xFFFE) continue;

        // Get the property name string
        const name = ctx.atoms.getName(prop.name) orelse continue;

        // Skip internal properties (children) and event handlers (onClick, etc)
        if (std.mem.eql(u8, name, "children")) continue;
        if (name.len >= 2 and name[0] == 'o' and name[1] == 'n') continue;

        const val = props_obj.getSlot(prop.offset);
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

/// Escape HTML special characters
fn escapeHtml(text: []const u8, writer: *std.Io.Writer) RenderError!void {
    for (text) |c| {
        switch (c) {
            '&' => try writer.writeAll("&amp;"),
            '<' => try writer.writeAll("&lt;"),
            '>' => try writer.writeAll("&gt;"),
            else => try writer.writeByte(c),
        }
    }
}

/// Escape attribute value
fn escapeAttr(text: []const u8, writer: anytype) !void {
    for (text) |c| {
        switch (c) {
            '&' => try writer.writeAll("&amp;"),
            '"' => try writer.writeAll("&quot;"),
            else => try writer.writeByte(c),
        }
    }
}

/// Check if element is a void element (self-closing)
fn isVoidElement(tag: []const u8) bool {
    const void_elements = [_][]const u8{
        "area",  "base", "br",   "col",   "embed",  "hr",    "img",
        "input", "link", "meta", "param", "source", "track", "wbr",
    };
    for (void_elements) |ve| {
        if (std.mem.eql(u8, tag, ve)) return true;
    }
    return false;
}

/// Check if element should have raw (unescaped) text content
fn isRawTextElement(tag: []const u8) bool {
    return std.mem.eql(u8, tag, "style") or std.mem.eql(u8, tag, "script");
}

/// Render children without HTML escaping (for style/script elements)
fn renderRawChildren(children: value.JSValue, writer: anytype) !void {
    if (children.isNull() or children.isUndefined()) return;

    if (children.isString()) {
        const str = children.toPtr(string.JSString);
        try writer.writeAll(str.data());
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
                    try renderRawChildren(child, writer);
                }
            }
        }
    }
}

// ============================================================================
// Helpers
// ============================================================================

/// Get string data from a JSValue
fn getStringArg(val: value.JSValue) ![]const u8 {
    if (val.isString()) {
        return val.toPtr(string.JSString).data();
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
        return 32 + obj.hidden_class.property_count * 24;
    }
    if (val.isString()) {
        const str = val.toPtr(string.JSString);
        return str.len + 2; // quotes
    }
    return 32;
}

/// Convert a JSValue to JSON string
pub fn valueToJson(ctx: *context.Context, val: value.JSValue) ![]u8 {
    const estimated = estimateJsonSize(val);
    var buffer = try std.ArrayList(u8).initCapacity(ctx.allocator, estimated);
    errdefer buffer.deinit(ctx.allocator);

    var aw: std.Io.Writer.Allocating = .fromArrayList(ctx.allocator, &buffer);
    try writeJson(ctx, val, &aw.writer);
    buffer = aw.toArrayList();
    return buffer.toOwnedSlice(ctx.allocator);
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
        try writer.print("{d}", .{val.getInt()});
    } else if (val.isFloat64()) {
        const f = val.getFloat64();
        // Handle special float values
        if (std.math.isNan(f) or std.math.isInf(f)) {
            try writer.writeAll("null");
        } else {
            try writer.print("{d}", .{f});
        }
    } else if (val.isString()) {
        try writer.writeByte('"');
        const str = val.toPtr(string.JSString);
        for (str.data()) |c| {
            switch (c) {
                '"' => try writer.writeAll("\\\""),
                '\\' => try writer.writeAll("\\\\"),
                '\n' => try writer.writeAll("\\n"),
                '\r' => try writer.writeAll("\\r"),
                '\t' => try writer.writeAll("\\t"),
                else => try writer.writeByte(c),
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
            for (obj.hidden_class.properties) |prop| {
                const v = obj.getSlot(prop.offset);
                if (v.isUndefined()) continue;

                // Get property name from atom table (handles both predefined and dynamic atoms)
                const name = ctx.atoms.getName(prop.name) orelse continue;

                if (!first) try writer.writeByte(',');
                first = false;

                try writer.writeByte('"');
                try writer.writeAll(name);
                try writer.writeAll("\":");
                try writeJson(ctx, v, writer);
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
    const status_atom = try ctx.atoms.intern("status");
    const status = resp_obj.getProperty(status_atom);
    try std.testing.expect(status != null);
    try std.testing.expectEqual(@as(i32, 200), status.?.getInt());
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

test "renderToString with attributes and nested elements" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const root_class = ctx.root_class orelse return error.NoRootClass;

    // props = { href: "/api/health" }
    const props = try object.JSObject.create(allocator, root_class, null);
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
