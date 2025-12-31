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
    const root_class = ctx.root_class orelse return error.NoRootClass;
    const allocator = ctx.allocator;

    const req_obj = try object.JSObject.create(allocator, root_class, null);

    // Set method
    const method_atom = try ctx.atoms.intern("method");
    const method_str = try string.createString(allocator, method);
    try req_obj.setProperty(allocator, method_atom, value.JSValue.fromPtr(method_str));

    // Set url
    const url_atom = try ctx.atoms.intern("url");
    const url_str = try string.createString(allocator, url);
    try req_obj.setProperty(allocator, url_atom, value.JSValue.fromPtr(url_str));

    // Set body
    const body_atom = try ctx.atoms.intern("body");
    if (body) |b| {
        const body_str = try string.createString(allocator, b);
        try req_obj.setProperty(allocator, body_atom, value.JSValue.fromPtr(body_str));
    } else {
        try req_obj.setProperty(allocator, body_atom, value.JSValue.null_val);
    }

    // Set headers object
    const headers_atom = try ctx.atoms.intern("headers");
    const headers_obj = try object.JSObject.create(allocator, root_class, null);
    for (headers) |header| {
        const name_atom = try ctx.atoms.intern(header[0]);
        const value_str = try string.createString(allocator, header[1]);
        try headers_obj.setProperty(allocator, name_atom, value.JSValue.fromPtr(value_str));
    }
    try req_obj.setProperty(allocator, headers_atom, headers_obj.toValue());

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
    const root_class = ctx.root_class orelse return error.NoRootClass;
    const allocator = ctx.allocator;

    const resp_obj = try object.JSObject.create(allocator, root_class, null);

    // Set body (using predefined atom for efficiency)
    const body_str = try string.createString(allocator, body);
    try resp_obj.setProperty(allocator, object.Atom.body, value.JSValue.fromPtr(body_str));

    // Set status (using predefined atom)
    try resp_obj.setProperty(allocator, object.Atom.status, value.JSValue.fromInt(@intCast(status)));

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
    const status_text_str = try string.createString(allocator, status_text);
    try resp_obj.setProperty(allocator, status_text_atom, value.JSValue.fromPtr(status_text_str));

    // Set ok (status 200-299)
    const ok_atom = try ctx.atoms.intern("ok");
    try resp_obj.setProperty(allocator, ok_atom, value.JSValue.fromBool(status >= 200 and status < 300));

    // Set headers (using predefined atom)
    const headers_obj = try object.JSObject.create(allocator, root_class, null);
    const ct_atom = try ctx.atoms.intern("Content-Type");
    const ct_str = try string.createString(allocator, content_type);
    try headers_obj.setProperty(allocator, ct_atom, value.JSValue.fromPtr(ct_str));
    try resp_obj.setProperty(allocator, object.Atom.headers, headers_obj.toValue());

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
    const allocator = ctx.allocator;
    const root_class = ctx.root_class orelse return error.NoRootClass;

    if (args.len == 0) {
        return value.JSValue.undefined_val;
    }

    const url = try getStringArg(args[0]);
    var status: u16 = 302;
    if (args.len > 1 and args[1].isInt()) {
        status = @intCast(args[1].getInt());
    }

    // Create response with Location header
    const resp_obj = try object.JSObject.create(allocator, root_class, null);

    const body_atom = try ctx.atoms.intern("_body");
    const body_str = try string.createString(allocator, "");
    try resp_obj.setProperty(allocator, body_atom, value.JSValue.fromPtr(body_str));

    const status_atom = try ctx.atoms.intern("status");
    try resp_obj.setProperty(allocator, status_atom, value.JSValue.fromInt(@intCast(status)));

    const headers_atom = try ctx.atoms.intern("headers");
    const headers_obj = try object.JSObject.create(allocator, root_class, null);
    const location_atom = try ctx.atoms.intern("Location");
    const location_str = try string.createString(allocator, url);
    try headers_obj.setProperty(allocator, location_atom, value.JSValue.fromPtr(location_str));
    try resp_obj.setProperty(allocator, headers_atom, headers_obj.toValue());

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
    const allocator = ctx.allocator;
    const root_class = ctx.root_class orelse return error.NoRootClass;

    // Create node object with { tag, props, children }
    const node = try object.JSObject.create(allocator, root_class, null);

    // Set tag (first arg)
    const tag_atom: object.Atom = .tag;
    if (args.len > 0) {
        try node.setProperty(allocator, tag_atom, args[0]);
    } else {
        const div_str = try string.createString(allocator, "div");
        try node.setProperty(allocator, tag_atom, value.JSValue.fromPtr(div_str));
    }

    // Set props (second arg or empty object)
    const props_atom: object.Atom = .props;
    if (args.len > 1 and args[1].isObject()) {
        try node.setProperty(allocator, props_atom, args[1]);
    } else {
        const empty_props = try object.JSObject.create(allocator, root_class, null);
        try node.setProperty(allocator, props_atom, empty_props.toValue());
    }

    // Collect children (remaining args)
    const children_atom: object.Atom = .children;
    const children_arr = try object.JSObject.createArray(allocator, root_class);
    children_arr.prototype = ctx.array_prototype;

    var child_count: u32 = 0;
    for (args[2..]) |child| {
        // Skip null/undefined
        if (child.isNull() or child.isUndefined()) continue;

        // TODO: Flatten arrays of children
        try children_arr.setIndex(allocator, child_count, child);
        child_count += 1;
    }
    children_arr.setArrayLength(child_count);
    try node.setProperty(allocator, children_atom, children_arr.toValue());

    return node.toValue();
}

/// renderToString(node) - Render virtual DOM to HTML string
pub fn renderToString(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) {
        const empty = try string.createString(ctx.allocator, "");
        return value.JSValue.fromPtr(empty);
    }

    var buffer = std.ArrayList(u8).empty;
    defer buffer.deinit(ctx.allocator);

    var aw: std.Io.Writer.Allocating = .fromArrayList(ctx.allocator, &buffer);
    try renderNode(ctx, args[0], &aw.writer);
    buffer = aw.toArrayList();

    const result = try string.createString(ctx.allocator, buffer.items);
    return value.JSValue.fromPtr(result);
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
                    props_obj.setProperty(ctx.allocator, .children, cv) catch {};
                } else if (ctx.root_class) |root_class| {
                    // Create new props object with children
                    const new_props = object.JSObject.create(ctx.allocator, root_class, null) catch null;
                    if (new_props) |np| {
                        np.setProperty(ctx.allocator, .children, cv) catch {};
                        props_val = value.JSValue.fromPtr(np);
                    }
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

            // Render children
            if (obj.getProperty(.children)) |children| {
                try renderChildren(ctx, children, writer);
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

/// Convert a JSValue to JSON string (simplified)
fn valueToJson(ctx: *context.Context, val: value.JSValue) ![]u8 {
    var buffer = std.ArrayList(u8).empty;
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
            const len_val = obj.getProperty(.length) orelse value.JSValue.fromInt(0);
            const len: usize = if (len_val.isInt()) @intCast(len_val.getInt()) else 0;
            for (0..len) |i| {
                if (i > 0) try writer.writeByte(',');
                const idx_atom: object.Atom = @enumFromInt(@as(u32, @intCast(i)) + object.Atom.FIRST_DYNAMIC);
                if (obj.getProperty(idx_atom)) |elem| {
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
    try props.setProperty(allocator, href_atom, value.JSValue.fromPtr(href_str));

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
