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

    // Set _body
    const body_atom = try ctx.atoms.intern("_body");
    const body_str = try string.createString(allocator, body);
    try resp_obj.setProperty(allocator, body_atom, value.JSValue.fromPtr(body_str));

    // Set status
    const status_atom = try ctx.atoms.intern("status");
    try resp_obj.setProperty(allocator, status_atom, value.JSValue.fromInt(@intCast(status)));

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

    // Set headers
    const headers_atom = try ctx.atoms.intern("headers");
    const headers_obj = try object.JSObject.create(allocator, root_class, null);
    const ct_atom = try ctx.atoms.intern("Content-Type");
    const ct_str = try string.createString(allocator, content_type);
    try headers_obj.setProperty(allocator, ct_atom, value.JSValue.fromPtr(ct_str));
    try resp_obj.setProperty(allocator, headers_atom, headers_obj.toValue());

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
    const json_str = try valueToJson(ctx.allocator, data);
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
    const tag_atom = try ctx.atoms.intern("tag");
    if (args.len > 0) {
        try node.setProperty(allocator, tag_atom, args[0]);
    } else {
        const div_str = try string.createString(allocator, "div");
        try node.setProperty(allocator, div_str, value.JSValue.fromPtr(div_str));
    }

    // Set props (second arg or empty object)
    const props_atom = try ctx.atoms.intern("props");
    if (args.len > 1 and args[1].isObject()) {
        try node.setProperty(allocator, props_atom, args[1]);
    } else {
        const empty_props = try object.JSObject.create(allocator, root_class, null);
        try node.setProperty(allocator, props_atom, empty_props.toValue());
    }

    // Collect children (remaining args)
    const children_atom = try ctx.atoms.intern("children");
    const children_arr = try object.JSObject.create(allocator, root_class, null);
    children_arr.class_id = .array;

    var child_count: i32 = 0;
    for (args[2..]) |child| {
        // Skip null/undefined
        if (child.isNull() or child.isUndefined()) continue;

        // TODO: Flatten arrays of children
        const idx_atom: object.Atom = @enumFromInt(@as(u32, @intCast(child_count)) + object.Atom.FIRST_DYNAMIC);
        try children_arr.setProperty(allocator, idx_atom, child);
        child_count += 1;
    }
    try children_arr.setProperty(allocator, .length, value.JSValue.fromInt(child_count));
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

    var buffer = std.ArrayList(u8).init(ctx.allocator);
    defer buffer.deinit();

    try renderNode(ctx, args[0], buffer.writer());

    const result = try string.createString(ctx.allocator, buffer.items);
    return value.JSValue.fromPtr(result);
}

/// Render a single node to the writer
fn renderNode(ctx: *context.Context, node: value.JSValue, writer: anytype) !void {
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

    // Handle object (virtual DOM node)
    if (node.isObject()) {
        const obj = object.JSObject.fromValue(node);

        // Check for tag property
        const tag_atom = ctx.atoms.intern("tag") catch return;
        const tag_val = obj.getProperty(tag_atom) orelse return;

        // Fragment: just render children
        if (tag_val.isString()) {
            const tag_str = tag_val.toPtr(string.JSString);
            if (std.mem.eql(u8, tag_str.data(), FRAGMENT_MARKER)) {
                const children_atom = ctx.atoms.intern("children") catch return;
                if (obj.getProperty(children_atom)) |children| {
                    try renderChildren(ctx, children, writer);
                }
                return;
            }

            // HTML element
            try writer.writeByte('<');
            try writer.writeAll(tag_str.data());

            // Render attributes from props
            const props_atom = ctx.atoms.intern("props") catch return;
            if (obj.getProperty(props_atom)) |props_val| {
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
            const children_atom = ctx.atoms.intern("children") catch return;
            if (obj.getProperty(children_atom)) |children| {
                try renderChildren(ctx, children, writer);
            }

            try writer.writeAll("</");
            try writer.writeAll(tag_data);
            try writer.writeByte('>');
        }
        // TODO: Handle function components
    }
}

/// Render array of children
fn renderChildren(ctx: *context.Context, children: value.JSValue, writer: anytype) !void {
    if (!children.isObject()) return;

    const children_obj = object.JSObject.fromValue(children);
    const len_val = children_obj.getProperty(.length) orelse return;
    if (!len_val.isInt()) return;

    const len = len_val.getInt();
    for (0..@intCast(len)) |i| {
        const idx_atom: object.Atom = @enumFromInt(@as(u32, @intCast(i)) + object.Atom.FIRST_DYNAMIC);
        if (children_obj.getProperty(idx_atom)) |child| {
            try renderNode(ctx, child, writer);
        }
    }
}

/// Render HTML attributes from props object
fn renderAttributes(_: *context.Context, props: value.JSValue, writer: anytype) !void {
    if (!props.isObject()) return;

    const props_obj = object.JSObject.fromValue(props);

    // Iterate through properties (simplified - uses hidden class properties)
    for (props_obj.hidden_class.properties) |prop| {
        // Skip internal properties and event handlers
        const name = @tagName(prop.name);
        if (name.len > 2 and name[0] == 'o' and name[1] == 'n') continue;

        const val = props_obj.getSlot(prop.offset);
        if (val.isNull() or val.isUndefined()) continue;
        if (val.isFalse()) continue;

        try writer.writeByte(' ');

        // Handle className -> class
        if (prop.name == .constructor) { // Would need proper className atom
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
fn escapeHtml(text: []const u8, writer: anytype) !void {
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
        "area", "base", "br", "col", "embed", "hr", "img",
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
fn valueToJson(allocator: std.mem.Allocator, val: value.JSValue) ![]u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    errdefer buffer.deinit();

    try writeJson(val, buffer.writer());
    return buffer.toOwnedSlice();
}

fn writeJson(val: value.JSValue, writer: anytype) !void {
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
                    try writeJson(elem, writer);
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

                if (!first) try writer.writeByte(',');
                first = false;

                try writer.writeByte('"');
                try writer.writeAll(@tagName(prop.name));
                try writer.writeAll("\":");
                try writeJson(v, writer);
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
    const allocator = std.testing.allocator;

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
    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    try escapeHtml("<script>alert('xss')</script>", buffer.writer());
    try std.testing.expectEqualStrings("&lt;script&gt;alert('xss')&lt;/script&gt;", buffer.items);
}

test "isVoidElement" {
    try std.testing.expect(isVoidElement("br"));
    try std.testing.expect(isVoidElement("img"));
    try std.testing.expect(isVoidElement("input"));
    try std.testing.expect(!isVoidElement("div"));
    try std.testing.expect(!isVoidElement("span"));
}

test "valueToJson basic" {
    const allocator = std.testing.allocator;

    // Integer
    {
        const json = try valueToJson(allocator, value.JSValue.fromInt(42));
        defer allocator.free(json);
        try std.testing.expectEqualStrings("42", json);
    }

    // Boolean
    {
        const json = try valueToJson(allocator, value.JSValue.true_val);
        defer allocator.free(json);
        try std.testing.expectEqualStrings("true", json);
    }

    // Null
    {
        const json = try valueToJson(allocator, value.JSValue.null_val);
        defer allocator.free(json);
        try std.testing.expectEqualStrings("null", json);
    }
}
