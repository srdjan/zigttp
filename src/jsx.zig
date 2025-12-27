//! JSX to JavaScript Transformer for MicroQuickJS
//!
//! Transforms JSX syntax to h() function calls:
//!   <div class="foo">Hello</div>
//!   -> h('div', {'class': 'foo'}, 'Hello')
//!
//! Supports:
//! - Elements: <div>, <span>, etc.
//! - Components: <MyComponent /> (uppercase = reference, not string)
//! - Attributes: class="foo", id={expr}, disabled
//! - Children: text, {expressions}, nested elements
//! - Fragments: <>...</>
//! - Self-closing: <br />, <Component />

const std = @import("std");

pub const TransformError = error{
    UnexpectedToken,
    UnclosedTag,
    MismatchedClosingTag,
    InvalidAttributeName,
    UnclosedExpression,
    UnclosedString,
    OutOfMemory,
    InvalidJsxElement,
};

pub const TransformResult = struct {
    code: []const u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *TransformResult) void {
        self.allocator.free(self.code);
    }
};

/// Transform JSX source code to JavaScript with h() calls
pub fn transform(allocator: std.mem.Allocator, source: []const u8) TransformError!TransformResult {
    var transformer = Transformer.init(allocator, source);
    return transformer.transform();
}

// ============================================================================
// Transformer (combined tokenizer + parser + code generator)
// ============================================================================

const Transformer = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    pos: usize,
    output: std.ArrayList(u8),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Self {
        return .{
            .allocator = allocator,
            .source = source,
            .pos = 0,
            .output = .empty,
        };
    }

    pub fn transform(self: *Self) TransformError!TransformResult {
        while (self.pos < self.source.len) {
            // Look for JSX start
            if (self.looksLikeJsxStart()) {
                try self.transformJsxElement();
            } else {
                // Pass through JS code
                try self.passJsCode();
            }
        }

        // Get the transformed code (zquickjs is pure Zig, no null terminator needed)
        const code = self.output.toOwnedSlice(self.allocator) catch return TransformError.OutOfMemory;
        return TransformResult{
            .code = code,
            .allocator = self.allocator,
        };
    }

    /// Check if current position looks like JSX start (< followed by tag)
    fn looksLikeJsxStart(self: *Self) bool {
        if (self.pos >= self.source.len) return false;
        if (self.source[self.pos] != '<') return false;
        if (self.pos + 1 >= self.source.len) return false;

        const next = self.source[self.pos + 1];

        // Fragment: <>
        if (next == '>') return true;

        // Tag name: <div, <MyComponent
        if (std.ascii.isAlphabetic(next) or next == '_') return true;

        return false;
    }

    /// Pass through JavaScript code until we hit potential JSX
    fn passJsCode(self: *Self) TransformError!void {
        const start = self.pos;

        while (self.pos < self.source.len) {
            const c = self.source[self.pos];

            // Skip string literals
            if (c == '"' or c == '\'' or c == '`') {
                try self.skipString(c);
                continue;
            }

            // Skip comments
            if (c == '/' and self.pos + 1 < self.source.len) {
                const next_char = self.source[self.pos + 1];
                if (next_char == '/') {
                    self.skipLineComment();
                    continue;
                }
                if (next_char == '*') {
                    self.skipBlockComment();
                    continue;
                }
            }

            // Check for JSX start
            if (self.looksLikeJsxStart()) {
                break;
            }

            self.pos += 1;
        }

        // Append passed JS code
        if (self.pos > start) {
            self.output.appendSlice(self.allocator, self.source[start..self.pos]) catch return TransformError.OutOfMemory;
        }
    }

    /// Skip a string literal (handles escape sequences)
    fn skipString(self: *Self, quote: u8) TransformError!void {
        self.pos += 1; // skip opening quote

        if (quote == '`') {
            // Template literal - handle ${} interpolation
            while (self.pos < self.source.len) {
                const c = self.source[self.pos];
                if (c == '`') {
                    self.pos += 1;
                    return;
                }
                if (c == '\\' and self.pos + 1 < self.source.len) {
                    self.pos += 2;
                    continue;
                }
                if (c == '$' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '{') {
                    self.pos += 2;
                    var depth: usize = 1;
                    while (self.pos < self.source.len and depth > 0) {
                        const ic = self.source[self.pos];
                        if (ic == '{') depth += 1;
                        if (ic == '}') depth -= 1;
                        self.pos += 1;
                    }
                    continue;
                }
                self.pos += 1;
            }
        } else {
            // Regular string
            while (self.pos < self.source.len) {
                const c = self.source[self.pos];
                if (c == quote) {
                    self.pos += 1;
                    return;
                }
                if (c == '\\' and self.pos + 1 < self.source.len) {
                    self.pos += 2;
                    continue;
                }
                if (c == '\n') {
                    // Unterminated string
                    return TransformError.UnclosedString;
                }
                self.pos += 1;
            }
        }
    }

    /// Skip single-line comment
    fn skipLineComment(self: *Self) void {
        while (self.pos < self.source.len and self.source[self.pos] != '\n') {
            self.pos += 1;
        }
    }

    /// Skip block comment
    fn skipBlockComment(self: *Self) void {
        self.pos += 2; // skip /*
        while (self.pos + 1 < self.source.len) {
            if (self.source[self.pos] == '*' and self.source[self.pos + 1] == '/') {
                self.pos += 2;
                return;
            }
            self.pos += 1;
        }
        self.pos = self.source.len;
    }

    /// Transform a JSX element to h() call
    fn transformJsxElement(self: *Self) TransformError!void {
        // Skip <
        self.pos += 1;

        // Check for fragment <>
        if (self.pos < self.source.len and self.source[self.pos] == '>') {
            self.pos += 1;
            self.output.appendSlice(self.allocator, "h(Fragment, null") catch return TransformError.OutOfMemory;
            try self.transformChildren(null); // null = fragment, match </>
            self.output.append(self.allocator, ')') catch return TransformError.OutOfMemory;
            return;
        }

        // Parse tag name
        const tag_start = self.pos;
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (std.ascii.isAlphanumeric(c) or c == '_' or c == '-' or c == '.' or c == ':') {
                self.pos += 1;
            } else {
                break;
            }
        }
        const tag = self.source[tag_start..self.pos];

        if (tag.len == 0) {
            return TransformError.InvalidJsxElement;
        }

        // Determine if component (uppercase) or element (lowercase)
        const is_component = std.ascii.isUpper(tag[0]);

        // Output: h('tag' or h(Component
        self.output.appendSlice(self.allocator, "h(") catch return TransformError.OutOfMemory;
        if (is_component) {
            self.output.appendSlice(self.allocator, tag) catch return TransformError.OutOfMemory;
        } else {
            self.output.append(self.allocator, '\'') catch return TransformError.OutOfMemory;
            self.output.appendSlice(self.allocator, tag) catch return TransformError.OutOfMemory;
            self.output.append(self.allocator, '\'') catch return TransformError.OutOfMemory;
        }

        // Parse attributes
        try self.transformAttributes();

        self.skipWhitespace();

        // Check for self-closing /> or >
        if (self.pos + 1 < self.source.len and self.source[self.pos] == '/' and self.source[self.pos + 1] == '>') {
            self.pos += 2;
            self.output.append(self.allocator, ')') catch return TransformError.OutOfMemory;
            return;
        }

        if (self.pos < self.source.len and self.source[self.pos] == '>') {
            self.pos += 1;
            // Parse children until closing tag
            try self.transformChildren(tag);
            self.output.append(self.allocator, ')') catch return TransformError.OutOfMemory;
            return;
        }

        return TransformError.UnclosedTag;
    }

    /// Transform JSX attributes to object literal or null
    fn transformAttributes(self: *Self) TransformError!void {
        var attrs: std.ArrayList(u8) = .empty;
        defer attrs.deinit(self.allocator);

        var has_attrs = false;
        var has_spread = false;

        while (true) {
            self.skipWhitespace();

            if (self.pos >= self.source.len) break;

            const c = self.source[self.pos];

            // End of attributes
            if (c == '>' or c == '/') break;

            // Spread: {...props}
            if (c == '{') {
                self.pos += 1;
                self.skipWhitespace();
                if (self.pos + 2 < self.source.len and
                    self.source[self.pos] == '.' and
                    self.source[self.pos + 1] == '.' and
                    self.source[self.pos + 2] == '.')
                {
                    // Handle spread
                    self.pos += 3;
                    const spread_expr = try self.consumeExpression();

                    if (!has_attrs and !has_spread) {
                        // First item is spread - use Object.assign pattern
                        has_spread = true;
                        attrs.appendSlice(self.allocator, "Object.assign({}, ") catch return TransformError.OutOfMemory;
                        attrs.appendSlice(self.allocator, spread_expr) catch return TransformError.OutOfMemory;
                    } else if (has_spread) {
                        attrs.appendSlice(self.allocator, ", ") catch return TransformError.OutOfMemory;
                        attrs.appendSlice(self.allocator, spread_expr) catch return TransformError.OutOfMemory;
                    } else {
                        // Mix of regular attrs and spread
                        // Convert to Object.assign pattern
                        const existing = attrs.toOwnedSlice(self.allocator) catch return TransformError.OutOfMemory;
                        defer self.allocator.free(existing);
                        attrs.appendSlice(self.allocator, "Object.assign(") catch return TransformError.OutOfMemory;
                        attrs.appendSlice(self.allocator, existing) catch return TransformError.OutOfMemory;
                        attrs.appendSlice(self.allocator, "}, ") catch return TransformError.OutOfMemory;
                        attrs.appendSlice(self.allocator, spread_expr) catch return TransformError.OutOfMemory;
                        has_spread = true;
                    }
                    continue;
                } else {
                    // Not a spread, backup
                    self.pos -= 1;
                }
            }

            // Attribute name
            if (std.ascii.isAlphabetic(c) or c == '_' or c == '$') {
                const name_start = self.pos;
                while (self.pos < self.source.len) {
                    const nc = self.source[self.pos];
                    if (std.ascii.isAlphanumeric(nc) or nc == '_' or nc == '-' or nc == ':') {
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                const name = self.source[name_start..self.pos];

                if (has_attrs or has_spread) {
                    attrs.appendSlice(self.allocator, ", ") catch return TransformError.OutOfMemory;
                }
                if (!has_attrs and !has_spread) {
                    attrs.append(self.allocator, '{') catch return TransformError.OutOfMemory;
                }
                has_attrs = true;

                // Quote attribute name if needed
                if (self.needsQuoting(name)) {
                    attrs.append(self.allocator, '\'') catch return TransformError.OutOfMemory;
                    attrs.appendSlice(self.allocator, name) catch return TransformError.OutOfMemory;
                    attrs.append(self.allocator, '\'') catch return TransformError.OutOfMemory;
                } else {
                    attrs.appendSlice(self.allocator, name) catch return TransformError.OutOfMemory;
                }

                self.skipWhitespace();

                // Check for = value
                if (self.pos < self.source.len and self.source[self.pos] == '=') {
                    self.pos += 1;
                    self.skipWhitespace();

                    attrs.appendSlice(self.allocator, ": ") catch return TransformError.OutOfMemory;

                    if (self.pos < self.source.len) {
                        const vc = self.source[self.pos];
                        if (vc == '"' or vc == '\'') {
                            // String value
                            const str = try self.consumeAttrString(vc);
                            attrs.append(self.allocator, '\'') catch return TransformError.OutOfMemory;
                            try self.appendEscapedString(&attrs, str);
                            attrs.append(self.allocator, '\'') catch return TransformError.OutOfMemory;
                        } else if (vc == '{') {
                            // Expression value
                            self.pos += 1;
                            const expr = try self.consumeExpression();
                            attrs.appendSlice(self.allocator, expr) catch return TransformError.OutOfMemory;
                        } else {
                            return TransformError.UnexpectedToken;
                        }
                    }
                } else {
                    // Boolean attribute: disabled -> disabled: true
                    attrs.appendSlice(self.allocator, ": true") catch return TransformError.OutOfMemory;
                }
            } else {
                break;
            }
        }

        if (has_spread) {
            attrs.append(self.allocator, ')') catch return TransformError.OutOfMemory;
            self.output.appendSlice(self.allocator, ", ") catch return TransformError.OutOfMemory;
            self.output.appendSlice(self.allocator, attrs.items) catch return TransformError.OutOfMemory;
        } else if (has_attrs) {
            attrs.append(self.allocator, '}') catch return TransformError.OutOfMemory;
            self.output.appendSlice(self.allocator, ", ") catch return TransformError.OutOfMemory;
            self.output.appendSlice(self.allocator, attrs.items) catch return TransformError.OutOfMemory;
        } else {
            self.output.appendSlice(self.allocator, ", null") catch return TransformError.OutOfMemory;
        }
    }

    /// Check if attribute name needs quoting
    fn needsQuoting(self: *Self, name: []const u8) bool {
        _ = self;
        for (name) |c| {
            if (c == '-' or c == ':') return true;
        }
        return false;
    }

    /// Consume an attribute string value
    fn consumeAttrString(self: *Self, quote: u8) TransformError![]const u8 {
        self.pos += 1; // skip opening quote
        const start = self.pos;

        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == quote) {
                const result = self.source[start..self.pos];
                self.pos += 1;
                return result;
            }
            if (c == '\\' and self.pos + 1 < self.source.len) {
                self.pos += 2;
                continue;
            }
            self.pos += 1;
        }

        return TransformError.UnclosedString;
    }

    /// Consume a JSX expression {...} and return the inner content
    fn consumeExpression(self: *Self) TransformError![]const u8 {
        const start = self.pos;
        var depth: usize = 1;

        while (self.pos < self.source.len and depth > 0) {
            const c = self.source[self.pos];

            // Skip strings inside expression
            if (c == '"' or c == '\'' or c == '`') {
                try self.skipString(c);
                continue;
            }

            if (c == '{') depth += 1;
            if (c == '}') depth -= 1;

            if (depth > 0) {
                self.pos += 1;
            }
        }

        if (depth > 0) {
            return TransformError.UnclosedExpression;
        }

        const result = self.source[start..self.pos];
        self.pos += 1; // skip closing }
        return result;
    }

    /// Transform children until closing tag
    fn transformChildren(self: *Self, parent_tag: ?[]const u8) TransformError!void {
        while (self.pos < self.source.len) {
            // Check for closing tag
            if (self.source[self.pos] == '<' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '/') {
                self.pos += 2;

                if (parent_tag == null) {
                    // Fragment: expect just >
                    self.skipWhitespace();
                    if (self.pos < self.source.len and self.source[self.pos] == '>') {
                        self.pos += 1;
                        return;
                    }
                    return TransformError.MismatchedClosingTag;
                }

                // Parse closing tag name
                self.skipWhitespace();
                const close_start = self.pos;
                while (self.pos < self.source.len) {
                    const c = self.source[self.pos];
                    if (std.ascii.isAlphanumeric(c) or c == '_' or c == '-' or c == '.' or c == ':') {
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                const close_tag = self.source[close_start..self.pos];

                if (!std.mem.eql(u8, close_tag, parent_tag.?)) {
                    return TransformError.MismatchedClosingTag;
                }

                self.skipWhitespace();
                if (self.pos < self.source.len and self.source[self.pos] == '>') {
                    self.pos += 1;
                    return;
                }
                return TransformError.UnclosedTag;
            }

            // Check for nested JSX element
            if (self.looksLikeJsxStart()) {
                self.output.appendSlice(self.allocator, ", ") catch return TransformError.OutOfMemory;
                try self.transformJsxElement();
                continue;
            }

            // Check for expression {..}
            if (self.source[self.pos] == '{') {
                self.pos += 1;

                // Check for JSX comment {/* */}
                self.skipWhitespace();
                if (self.pos + 1 < self.source.len and self.source[self.pos] == '/' and self.source[self.pos + 1] == '*') {
                    // Skip JSX comment
                    self.pos += 2;
                    while (self.pos + 1 < self.source.len) {
                        if (self.source[self.pos] == '*' and self.source[self.pos + 1] == '/') {
                            self.pos += 2;
                            break;
                        }
                        self.pos += 1;
                    }
                    self.skipWhitespace();
                    if (self.pos < self.source.len and self.source[self.pos] == '}') {
                        self.pos += 1;
                    }
                    continue;
                }

                const expr = try self.consumeExpression();
                const trimmed = std.mem.trim(u8, expr, " \t\n\r");
                if (trimmed.len > 0) {
                    self.output.appendSlice(self.allocator, ", ") catch return TransformError.OutOfMemory;
                    self.output.appendSlice(self.allocator, trimmed) catch return TransformError.OutOfMemory;
                }
                continue;
            }

            // Text content
            const text_start = self.pos;
            while (self.pos < self.source.len) {
                const c = self.source[self.pos];
                if (c == '<' or c == '{') break;
                self.pos += 1;
            }

            if (self.pos > text_start) {
                const text = self.source[text_start..self.pos];
                const trimmed = std.mem.trim(u8, text, " \t\n\r");
                if (trimmed.len > 0) {
                    self.output.appendSlice(self.allocator, ", '") catch return TransformError.OutOfMemory;
                    try self.appendEscapedString(&self.output, trimmed);
                    self.output.append(self.allocator, '\'') catch return TransformError.OutOfMemory;
                }
            }
        }

        if (parent_tag != null) {
            return TransformError.UnclosedTag;
        }
    }

    /// Append escaped string for JS output
    fn appendEscapedString(self: *Self, list: *std.ArrayList(u8), str: []const u8) TransformError!void {
        for (str) |c| {
            switch (c) {
                '\'' => list.appendSlice(self.allocator, "\\'") catch return TransformError.OutOfMemory,
                '\\' => list.appendSlice(self.allocator, "\\\\") catch return TransformError.OutOfMemory,
                '\n' => list.appendSlice(self.allocator, "\\n") catch return TransformError.OutOfMemory,
                '\r' => list.appendSlice(self.allocator, "\\r") catch return TransformError.OutOfMemory,
                '\t' => list.appendSlice(self.allocator, "\\t") catch return TransformError.OutOfMemory,
                else => list.append(self.allocator, c) catch return TransformError.OutOfMemory,
            }
        }
    }

    fn skipWhitespace(self: *Self) void {
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
                self.pos += 1;
            } else {
                break;
            }
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "simple element" {
    const result = try transform(std.testing.allocator, "<div>hello</div>");
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings("h('div', null, 'hello')", result.code);
}

test "element with class attribute" {
    const result = try transform(std.testing.allocator, "<div class=\"foo\">text</div>");
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings("h('div', {class: 'foo'}, 'text')", result.code);
}

test "element with expression attribute" {
    const result = try transform(std.testing.allocator, "<div id={myId}>text</div>");
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings("h('div', {id: myId}, 'text')", result.code);
}

test "self-closing element" {
    const result = try transform(std.testing.allocator, "<br />");
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings("h('br', null)", result.code);
}

test "component" {
    const result = try transform(std.testing.allocator, "<MyComponent prop={value} />");
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings("h(MyComponent, {prop: value})", result.code);
}

test "fragment" {
    const result = try transform(std.testing.allocator, "<><span>a</span><span>b</span></>");
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings("h(Fragment, null, h('span', null, 'a'), h('span', null, 'b'))", result.code);
}

test "nested elements" {
    const result = try transform(std.testing.allocator, "<div><span>inner</span></div>");
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings("h('div', null, h('span', null, 'inner'))", result.code);
}

test "expression child" {
    const result = try transform(std.testing.allocator, "<div>{items}</div>");
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings("h('div', null, items)", result.code);
}

test "boolean attribute" {
    const result = try transform(std.testing.allocator, "<input disabled />");
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings("h('input', {disabled: true})", result.code);
}

test "preserve js code around jsx" {
    const result = try transform(std.testing.allocator, "var x = 1;\nvar el = <div>hi</div>;\nconsole.log(el);");
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings("var x = 1;\nvar el = h('div', null, 'hi');\nconsole.log(el);", result.code);
}

test "jsx in function return" {
    const result = try transform(std.testing.allocator, "function f() { return <div>x</div>; }");
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings("function f() { return h('div', null, 'x'); }", result.code);
}

test "multiple attributes" {
    const result = try transform(std.testing.allocator, "<div class=\"a\" id=\"b\">x</div>");
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings("h('div', {class: 'a', id: 'b'}, 'x')", result.code);
}

test "jsx comment" {
    const result = try transform(std.testing.allocator, "<div>{/* comment */}text</div>");
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings("h('div', null, 'text')", result.code);
}

test "comparison not treated as jsx" {
    const result = try transform(std.testing.allocator, "if (a < b) { x = 1; }");
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings("if (a < b) { x = 1; }", result.code);
}

test "string with angle brackets not treated as jsx" {
    const result = try transform(std.testing.allocator, "var s = \"<div>not jsx</div>\";");
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings("var s = \"<div>not jsx</div>\";", result.code);
}

test "complex component example" {
    const source =
        \\function Card(props) {
        \\    return (
        \\        <div class="card">
        \\            <h2>{props.title}</h2>
        \\        </div>
        \\    );
        \\}
        \\function handler() {
        \\    return <Card title="Hello" />;
        \\}
    ;
    const result = try transform(std.testing.allocator, source);
    defer @constCast(&result).deinit();
    // Just check it doesn't crash and contains h(
    try std.testing.expect(std.mem.indexOf(u8, result.code, "h(") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "h(Card") != null);
}

test "jsx in loop" {
    const source =
        \\var items = ['a', 'b'];
        \\function handler() {
        \\    var els = [];
        \\    for (var i = 0; i < items.length; i++) {
        \\        els.push(<li>{items[i]}</li>);
        \\    }
        \\    return <ul>{els}</ul>;
        \\}
    ;
    const result = try transform(std.testing.allocator, source);
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "h('li'") != null);
}

test "no jsx passthrough" {
    const source =
        \\console.log("Test");
        \\var items = ['a', 'b'];
        \\function handler(request) {
        \\    return Response.text("ok");
        \\}
    ;
    const result = try transform(std.testing.allocator, source);
    defer @constCast(&result).deinit();
    std.debug.print("\n=== No JSX ===\n{s}\n=== End ===\n", .{result.code});
    try std.testing.expectEqualStrings(source, result.code);
}
