//! HTTP Handler Pattern Analyzer
//!
//! Detects static response patterns at compile time and builds a fast dispatch table.
//! Enables native fast path for requests that bypass the bytecode interpreter entirely.
//!
//! Detectable patterns:
//! - `if (url === '/api/health') return Response.json({status: 'ok'})`
//! - `if (url.indexOf('/api/greet/') === 0) ...` (prefix match)

const std = @import("std");
const bytecode = @import("bytecode.zig");
const ir = @import("parser/ir.zig");
const object = @import("object.zig");
const context = @import("context.zig");

const HandlerPattern = bytecode.HandlerPattern;
const PatternDispatchTable = bytecode.PatternDispatchTable;
const PatternType = bytecode.PatternType;
const HandlerFlags = bytecode.HandlerFlags;
const Node = ir.Node;
const NodeIndex = ir.NodeIndex;
const NodeTag = ir.NodeTag;
const IrView = ir.IrView;
const null_node = ir.null_node;

/// Handler analyzer state
pub const HandlerAnalyzer = struct {
    allocator: std.mem.Allocator,
    ir: IrView,
    atoms: ?*context.AtomTable,

    // Analysis state
    patterns: std.ArrayList(HandlerPattern),
    url_binding_slot: ?u16, // Local slot of `url` variable
    request_binding_slot: ?u16, // Local slot of request parameter

    pub fn init(
        allocator: std.mem.Allocator,
        ir_view: IrView,
        atoms: ?*context.AtomTable,
    ) HandlerAnalyzer {
        return .{
            .allocator = allocator,
            .ir = ir_view,
            .atoms = atoms,
            .patterns = .{},
            .url_binding_slot = null,
            .request_binding_slot = null,
        };
    }

    pub fn deinit(self: *HandlerAnalyzer) void {
        // Don't free patterns - they're transferred to the dispatch table
        self.patterns.deinit(self.allocator);
    }

    /// Main entry point: analyze a function for static handler patterns.
    /// Returns a dispatch table if patterns were found, null otherwise.
    pub fn analyze(self: *HandlerAnalyzer, func_node: NodeIndex) !?*PatternDispatchTable {
        const func = self.ir.getFunction(func_node) orelse return null;

        // Handler must have exactly 1 parameter (request)
        if (func.params_count != 1) return null;

        // Find the URL binding in the function body
        // Pattern: const url = request.url
        self.findUrlBinding(func.body);
        if (self.url_binding_slot == null) return null;

        // Extract patterns from if-chain in function body
        try self.extractPatternsFromBlock(func.body);

        // Build dispatch table if we found any patterns
        if (self.patterns.items.len == 0) return null;

        const dispatch = try self.allocator.create(PatternDispatchTable);
        dispatch.* = PatternDispatchTable.init(self.allocator);

        // Transfer patterns
        dispatch.patterns = try self.allocator.dupe(HandlerPattern, self.patterns.items);

        // Build exact match hash map
        for (dispatch.patterns, 0..) |pattern, i| {
            if (pattern.pattern_type == .exact) {
                const hash = std.hash.Wyhash.hash(0, pattern.url_bytes);
                try dispatch.exact_match_map.put(self.allocator, hash, @intCast(i));
            }
        }

        return dispatch;
    }

    /// Find `const url = request.url` binding
    fn findUrlBinding(self: *HandlerAnalyzer, body_node: NodeIndex) void {
        const tag = self.ir.getTag(body_node) orelse return;

        if (tag == .block or tag == .program) {
            const block = self.ir.getBlock(body_node) orelse return;
            var i: u16 = 0;
            while (i < block.stmts_count) : (i += 1) {
                const stmt_idx = self.ir.getListIndex(block.stmts_start, i);
                self.findUrlBindingInStmt(stmt_idx);
                if (self.url_binding_slot != null) return;
            }
        }
    }

    fn findUrlBindingInStmt(self: *HandlerAnalyzer, stmt_node: NodeIndex) void {
        const tag = self.ir.getTag(stmt_node) orelse return;

        if (tag == .var_decl) {
            const decl = self.ir.getVarDecl(stmt_node) orelse return;

            // Check if init is a member access
            if (decl.init == null_node) return;
            const init_tag = self.ir.getTag(decl.init) orelse return;
            if (init_tag != .member_access) return;

            const member = self.ir.getMember(decl.init) orelse return;

            // Check if property is 'url' (atom 168)
            if (member.property == @intFromEnum(object.Atom.url)) {
                // This is `const url = something.url`
                // Store the binding slot
                if (decl.binding.kind == .local or decl.binding.kind == .argument) {
                    self.url_binding_slot = decl.binding.slot;
                }
            }
        }
    }

    /// Extract patterns from if-chain in a block
    fn extractPatternsFromBlock(self: *HandlerAnalyzer, body_node: NodeIndex) !void {
        const tag = self.ir.getTag(body_node) orelse return;

        if (tag == .block or tag == .program) {
            const block = self.ir.getBlock(body_node) orelse return;
            var i: u16 = 0;
            while (i < block.stmts_count) : (i += 1) {
                const stmt_idx = self.ir.getListIndex(block.stmts_start, i);
                try self.extractPatternFromStmt(stmt_idx);
            }
        }
    }

    /// Extract pattern from a single if statement
    fn extractPatternFromStmt(self: *HandlerAnalyzer, stmt_node: NodeIndex) !void {
        const tag = self.ir.getTag(stmt_node) orelse return;

        if (tag != .if_stmt) return;

        const if_stmt = self.ir.getIfStmt(stmt_node) orelse return;

        // Analyze condition for URL pattern
        const url_pattern = self.analyzeCondition(if_stmt.condition) orelse return;

        // Analyze then branch for static response
        const static_response = try self.analyzeStaticReturn(if_stmt.then_branch);
        if (static_response == null) return;

        // Found a static pattern!
        const response = static_response.?;
        try self.patterns.append(self.allocator, .{
            .pattern_type = url_pattern.pattern_type,
            .url_atom = url_pattern.url_atom,
            .url_bytes = url_pattern.url_bytes,
            .static_body = response.body,
            .status = response.status,
            .content_type_idx = response.content_type_idx,
        });
    }

    const UrlPatternInfo = struct {
        pattern_type: PatternType,
        url_atom: object.Atom,
        url_bytes: []const u8,
    };

    /// Analyze condition for URL matching pattern
    fn analyzeCondition(self: *HandlerAnalyzer, cond_node: NodeIndex) ?UrlPatternInfo {
        const tag = self.ir.getTag(cond_node) orelse return null;

        if (tag == .binary_op) {
            const binary = self.ir.getBinary(cond_node) orelse return null;

            // Pattern 1: url === '/api/health'
            if (binary.op == .strict_eq) {
                return self.analyzeExactMatch(binary);
            }

            // Pattern 2: url.indexOf('/api/greet/') === 0
            if (binary.op == .strict_eq or binary.op == .eq) {
                return self.analyzePrefixMatch(binary);
            }
        }

        return null;
    }

    /// Analyze `url === '/api/health'` pattern
    fn analyzeExactMatch(self: *HandlerAnalyzer, binary: Node.BinaryExpr) ?UrlPatternInfo {
        // Left side should be identifier (url)
        var url_side: NodeIndex = null_node;
        var str_side: NodeIndex = null_node;

        const left_tag = self.ir.getTag(binary.left);
        const right_tag = self.ir.getTag(binary.right);

        if (left_tag == .identifier and right_tag == .lit_string) {
            url_side = binary.left;
            str_side = binary.right;
        } else if (left_tag == .lit_string and right_tag == .identifier) {
            url_side = binary.right;
            str_side = binary.left;
        } else {
            return null;
        }

        // Verify url_side is the url binding
        const binding = self.ir.getBinding(url_side) orelse return null;
        if (binding.slot != self.url_binding_slot.?) return null;

        // Get URL string
        const str_idx = self.ir.getStringIdx(str_side) orelse return null;
        const url_str = self.ir.getString(str_idx) orelse return null;

        // Duplicate the URL string for storage
        const url_bytes = self.allocator.dupe(u8, url_str) catch return null;

        return .{
            .pattern_type = .exact,
            .url_atom = object.Atom.url,
            .url_bytes = url_bytes,
        };
    }

    /// Analyze `url.indexOf('/api/greet/') === 0` pattern
    fn analyzePrefixMatch(self: *HandlerAnalyzer, binary: Node.BinaryExpr) ?UrlPatternInfo {
        // Right side should be 0
        const right_tag = self.ir.getTag(binary.right) orelse return null;
        if (right_tag != .lit_int) return null;
        const right_val = self.ir.getIntValue(binary.right) orelse return null;
        if (right_val != 0) return null;

        // Left side should be call to indexOf
        const left_tag = self.ir.getTag(binary.left) orelse return null;
        if (left_tag != .call) return null;

        const call = self.ir.getCall(binary.left) orelse return null;
        if (call.args_count != 1) return null;

        // Callee should be member access
        const callee_tag = self.ir.getTag(call.callee) orelse return null;
        if (callee_tag != .member_access) return null;

        const member = self.ir.getMember(call.callee) orelse return null;

        // Property should be 'indexOf'
        if (member.property != @intFromEnum(object.Atom.indexOf)) return null;

        // Object should be the url identifier
        const obj_tag = self.ir.getTag(member.object) orelse return null;
        if (obj_tag != .identifier) return null;

        const binding = self.ir.getBinding(member.object) orelse return null;
        if (binding.slot != self.url_binding_slot.?) return null;

        // Get the prefix string from the argument
        const arg_idx = self.ir.getListIndex(call.args_start, 0);
        const arg_tag = self.ir.getTag(arg_idx) orelse return null;
        if (arg_tag != .lit_string) return null;

        const str_idx = self.ir.getStringIdx(arg_idx) orelse return null;
        const prefix_str = self.ir.getString(str_idx) orelse return null;

        const url_bytes = self.allocator.dupe(u8, prefix_str) catch return null;

        return .{
            .pattern_type = .prefix,
            .url_atom = object.Atom.url,
            .url_bytes = url_bytes,
        };
    }

    const StaticResponseInfo = struct {
        body: []const u8,
        status: u16,
        content_type_idx: u8,
    };

    /// Analyze then branch for static Response.json({...}) return
    fn analyzeStaticReturn(self: *HandlerAnalyzer, then_node: NodeIndex) !?StaticResponseInfo {
        const tag = self.ir.getTag(then_node) orelse return null;

        // Could be a block or direct return
        if (tag == .block) {
            const block = self.ir.getBlock(then_node) orelse return null;
            // Look for first return statement
            var i: u16 = 0;
            while (i < block.stmts_count) : (i += 1) {
                const stmt_idx = self.ir.getListIndex(block.stmts_start, i);
                const stmt_tag = self.ir.getTag(stmt_idx) orelse continue;
                if (stmt_tag == .return_stmt) {
                    return try self.analyzeReturnStmt(stmt_idx);
                }
            }
        } else if (tag == .return_stmt) {
            return try self.analyzeReturnStmt(then_node);
        }

        return null;
    }

    fn analyzeReturnStmt(self: *HandlerAnalyzer, return_node: NodeIndex) !?StaticResponseInfo {
        const return_val = self.ir.getOptValue(return_node) orelse return null;
        if (return_val == null_node) return null;

        return try self.analyzeResponseCall(return_val);
    }

    /// Analyze Response.json({...}) or Response.text("...")
    fn analyzeResponseCall(self: *HandlerAnalyzer, call_node: NodeIndex) !?StaticResponseInfo {
        const tag = self.ir.getTag(call_node) orelse return null;
        if (tag != .call) return null;

        const call = self.ir.getCall(call_node) orelse return null;
        if (call.args_count < 1) return null;

        // Callee should be Response.json or Response.text
        const callee_tag = self.ir.getTag(call.callee) orelse return null;
        if (callee_tag != .member_access) return null;

        const member = self.ir.getMember(call.callee) orelse return null;

        // Check if object is Response
        const obj_tag = self.ir.getTag(member.object) orelse return null;
        if (obj_tag != .identifier) return null;

        const binding = self.ir.getBinding(member.object) orelse return null;
        if (binding.kind != .global) return null;
        if (binding.slot != @intFromEnum(object.Atom.Response)) return null;

        // Check method
        const content_type_idx: u8 = if (member.property == @intFromEnum(object.Atom.json))
            0
        else if (member.property == @intFromEnum(object.Atom.text))
            1
        else if (member.property == @intFromEnum(object.Atom.html))
            2
        else
            return null;

        // Get the first argument
        const arg_idx = self.ir.getListIndex(call.args_start, 0);

        // Try to serialize the argument
        if (content_type_idx == 0) {
            // JSON - serialize object literal
            const body = try self.serializeObjectLiteral(arg_idx);
            if (body == null) return null;

            // Check for status in second arg (options object)
            var status: u16 = 200;
            if (call.args_count >= 2) {
                const opts_idx = self.ir.getListIndex(call.args_start, 1);
                status = self.extractStatusFromOptions(opts_idx) orelse 200;
            }

            return .{
                .body = body.?,
                .status = status,
                .content_type_idx = content_type_idx,
            };
        } else {
            // Text/HTML - get string literal
            const arg_tag = self.ir.getTag(arg_idx) orelse return null;
            if (arg_tag != .lit_string) return null;

            const str_idx = self.ir.getStringIdx(arg_idx) orelse return null;
            const str = self.ir.getString(str_idx) orelse return null;
            const body = try self.allocator.dupe(u8, str);

            return .{
                .body = body,
                .status = 200,
                .content_type_idx = content_type_idx,
            };
        }
    }

    /// Extract status from options object: {status: 404}
    fn extractStatusFromOptions(self: *HandlerAnalyzer, opts_node: NodeIndex) ?u16 {
        const tag = self.ir.getTag(opts_node) orelse return null;
        if (tag != .object_literal) return null;

        const obj = self.ir.getObject(opts_node) orelse return null;

        var i: u16 = 0;
        while (i < obj.properties_count) : (i += 1) {
            const prop_idx = self.ir.getListIndex(obj.properties_start, i);
            const prop_tag = self.ir.getTag(prop_idx) orelse continue;
            if (prop_tag != .object_property) continue;

            const prop = self.ir.getProperty(prop_idx) orelse continue;

            // Check if key is 'status'
            const key_tag = self.ir.getTag(prop.key) orelse continue;
            if (key_tag == .lit_string) {
                const key_str_idx = self.ir.getStringIdx(prop.key) orelse continue;
                const key_str = self.ir.getString(key_str_idx) orelse continue;
                if (std.mem.eql(u8, key_str, "status")) {
                    const val_tag = self.ir.getTag(prop.value) orelse continue;
                    if (val_tag == .lit_int) {
                        const val = self.ir.getIntValue(prop.value) orelse continue;
                        if (val >= 100 and val <= 599) {
                            return @intCast(val);
                        }
                    }
                }
            }
        }

        return null;
    }

    /// Serialize an object literal to JSON at compile time
    fn serializeObjectLiteral(self: *HandlerAnalyzer, node: NodeIndex) std.mem.Allocator.Error!?[]const u8 {
        const tag = self.ir.getTag(node) orelse return null;
        if (tag != .object_literal) return null;

        const obj = self.ir.getObject(node) orelse return null;

        var buf: std.ArrayList(u8) = .empty;
        errdefer buf.deinit(self.allocator);

        try buf.append(self.allocator, '{');

        var first = true;
        var i: u16 = 0;
        while (i < obj.properties_count) : (i += 1) {
            const prop_idx = self.ir.getListIndex(obj.properties_start, i);
            const prop_tag = self.ir.getTag(prop_idx) orelse return null;
            if (prop_tag != .object_property) return null;

            const prop = self.ir.getProperty(prop_idx) orelse return null;

            // Get key
            const key_tag = self.ir.getTag(prop.key) orelse return null;
            if (key_tag != .lit_string) return null;
            const key_str_idx = self.ir.getStringIdx(prop.key) orelse return null;
            const key_str = self.ir.getString(key_str_idx) orelse return null;

            if (!first) {
                try buf.append(self.allocator, ',');
            }
            first = false;

            // Write key
            try buf.append(self.allocator, '"');
            try buf.appendSlice(self.allocator, key_str);
            try buf.appendSlice(self.allocator, "\":");

            // Serialize value
            const val_serialized = try self.serializeValue(prop.value);
            if (val_serialized == null) return null;
            try buf.appendSlice(self.allocator, val_serialized.?);
            self.allocator.free(val_serialized.?);
        }

        try buf.append(self.allocator, '}');

        return try buf.toOwnedSlice(self.allocator);
    }

    /// Serialize a single value to JSON
    fn serializeValue(self: *HandlerAnalyzer, node: NodeIndex) std.mem.Allocator.Error!?[]const u8 {
        const tag = self.ir.getTag(node) orelse return null;

        switch (tag) {
            .lit_string => {
                const str_idx = self.ir.getStringIdx(node) orelse return null;
                const str = self.ir.getString(str_idx) orelse return null;

                var buf: std.ArrayList(u8) = .empty;
                errdefer buf.deinit(self.allocator);

                try buf.append(self.allocator, '"');
                // Escape string
                for (str) |c| {
                    switch (c) {
                        '"' => try buf.appendSlice(self.allocator, "\\\""),
                        '\\' => try buf.appendSlice(self.allocator, "\\\\"),
                        '\n' => try buf.appendSlice(self.allocator, "\\n"),
                        '\r' => try buf.appendSlice(self.allocator, "\\r"),
                        '\t' => try buf.appendSlice(self.allocator, "\\t"),
                        else => try buf.append(self.allocator, c),
                    }
                }
                try buf.append(self.allocator, '"');

                return try buf.toOwnedSlice(self.allocator);
            },
            .lit_int => {
                const val = self.ir.getIntValue(node) orelse return null;
                var tmp_buf: [32]u8 = undefined;
                const s = std.fmt.bufPrint(&tmp_buf, "{d}", .{val}) catch return null;
                return try self.allocator.dupe(u8, s);
            },
            .lit_bool => {
                const val = self.ir.getBoolValue(node) orelse return null;
                return try self.allocator.dupe(u8, if (val) "true" else "false");
            },
            .lit_null => {
                return try self.allocator.dupe(u8, "null");
            },
            .object_literal => {
                return try self.serializeObjectLiteral(node);
            },
            .array_literal => {
                return try self.serializeArrayLiteral(node);
            },
            else => return null,
        }
    }

    fn serializeArrayLiteral(self: *HandlerAnalyzer, node: NodeIndex) std.mem.Allocator.Error!?[]const u8 {
        const arr = self.ir.getArray(node) orelse return null;

        var buf: std.ArrayList(u8) = .empty;
        errdefer buf.deinit(self.allocator);

        try buf.append(self.allocator, '[');

        var i: u16 = 0;
        while (i < arr.elements_count) : (i += 1) {
            if (i > 0) {
                try buf.append(self.allocator, ',');
            }
            const elem_idx = self.ir.getListIndex(arr.elements_start, i);
            const elem_serialized = try self.serializeValue(elem_idx);
            if (elem_serialized == null) return null;
            try buf.appendSlice(self.allocator, elem_serialized.?);
            self.allocator.free(elem_serialized.?);
        }

        try buf.append(self.allocator, ']');

        return try buf.toOwnedSlice(self.allocator);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "HandlerAnalyzer init and deinit" {
    const allocator = std.testing.allocator;

    var nodes = ir.NodeList.init(allocator);
    defer nodes.deinit();

    var constants = ir.ConstantPool.init(allocator);
    defer constants.deinit();

    const ir_view = ir.IrView.fromNodeList(&nodes, &constants);

    var analyzer = HandlerAnalyzer.init(allocator, ir_view, null);
    defer analyzer.deinit();

    try std.testing.expect(analyzer.url_binding_slot == null);
}
