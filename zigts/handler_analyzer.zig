//! HTTP Handler Pattern Analyzer
//!
//! Detects static response patterns at compile time and builds a fast dispatch table.
//! Enables native fast path for requests that bypass the bytecode interpreter entirely.
//!
//! Detectable patterns:
//! - `if (url === '/api/health') return Response.json({status: 'ok'})`
//! - `if (path === '/api/health') return Response.json({status: 'ok'})`
//! - `if (url.indexOf('/api/greet/') === 0) ...` (prefix match)
//! - `if (path.indexOf('/api/greet/') === 0) ...` (prefix match)

const std = @import("std");
const bytecode = @import("bytecode.zig");
const ir = @import("parser/ir.zig");
const object = @import("object.zig");
const context = @import("context.zig");

const HandlerPattern = bytecode.HandlerPattern;
const PatternDispatchTable = bytecode.PatternDispatchTable;
const PatternType = bytecode.PatternType;
const ResponseBodySource = bytecode.ResponseBodySource;
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
    route_binding_slot: ?u16, // Local slot of `url` or `path` variable
    route_atom: object.Atom, // Which request field is bound (`url` or `path`)
    request_binding_slot: ?u16, // Local slot of request parameter
    body_binding_slot: ?u16, // Local slot of `body` variable bound to request.body
    enable_json_body_parse: bool,

    pub fn init(
        allocator: std.mem.Allocator,
        ir_view: IrView,
        atoms: ?*context.AtomTable,
    ) HandlerAnalyzer {
        return .{
            .allocator = allocator,
            .ir = ir_view,
            .atoms = atoms,
            .patterns = .empty,
            .route_binding_slot = null,
            .route_atom = .url,
            .request_binding_slot = null,
            .body_binding_slot = null,
            .enable_json_body_parse = false,
        };
    }

    /// Enable detection for `Response.json(JSON.parse(request.body))` patterns.
    /// This is intended for AOT precompile paths.
    pub fn enableJsonBodyParsePatterns(self: *HandlerAnalyzer) void {
        self.enable_json_body_parse = true;
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

        self.patterns.clearRetainingCapacity();
        self.route_binding_slot = null;
        self.route_atom = .url;
        self.request_binding_slot = null;
        self.body_binding_slot = null;

        self.findRequestBinding(func);
        self.findBodyBinding(func.body);

        // Find route binding in function body:
        //   const url = request.url
        //   const path = request.path
        self.findRouteBinding(func.body);
        if (self.route_binding_slot == null) return null;

        // Extract patterns from if-chain in function body
        try self.extractPatternsFromBlock(func.body);

        // Build dispatch table if we found any patterns
        if (self.patterns.items.len == 0) return null;

        const dispatch = try self.allocator.create(PatternDispatchTable);
        dispatch.* = PatternDispatchTable.init(self.allocator);

        // Transfer patterns
        dispatch.patterns = try self.allocator.dupe(HandlerPattern, self.patterns.items);

        // Build exact match hash map and pre-built responses for exact matches
        for (dispatch.patterns, 0..) |*pattern, i| {
            if (pattern.pattern_type == .exact) {
                const hash = std.hash.Wyhash.hash(0, pattern.url_bytes);
                try dispatch.exact_match_map.put(self.allocator, hash, @intCast(i));
                // Build pre-serialized full HTTP response for fast path
                try pattern.buildPrebuiltResponse(self.allocator);
            }
        }

        return dispatch;
    }

    /// Find `const route = request.url` or `const route = request.path` binding.
    fn findRequestBinding(self: *HandlerAnalyzer, func: Node.FunctionExpr) void {
        if (func.params_count == 0) return;
        const req_param = self.ir.getListIndex(func.params_start, 0);
        if (self.getBindableSlot(req_param)) |slot| {
            self.request_binding_slot = slot;
            return;
        }
        const req_tag = self.ir.getTag(req_param) orelse return;
        if (req_tag != .pattern_element) return;
        const elem = self.ir.getPatternElem(req_param) orelse return;
        if (bindingSlotIfBindable(elem.binding)) |slot| {
            self.request_binding_slot = slot;
        }
    }

    /// Find `const body = request.body` binding for JSON.parse detection.
    fn findBodyBinding(self: *HandlerAnalyzer, body_node: NodeIndex) void {
        const tag = self.ir.getTag(body_node) orelse return;

        if (tag == .block or tag == .program) {
            const block = self.ir.getBlock(body_node) orelse return;
            var i: u16 = 0;
            while (i < block.stmts_count) : (i += 1) {
                const stmt_idx = self.ir.getListIndex(block.stmts_start, i);
                const stmt_tag = self.ir.getTag(stmt_idx) orelse continue;
                if (stmt_tag != .var_decl) continue;

                const decl = self.ir.getVarDecl(stmt_idx) orelse continue;
                if (decl.init == null_node) continue;
                const member = self.getMemberAccess(decl.init) orelse continue;
                if (member.property != @intFromEnum(object.Atom.body)) continue;

                // Body binding must come from request.body.
                if (!self.matchesOptionalSlot(member.object, self.request_binding_slot)) continue;

                self.body_binding_slot = bindingSlotIfBindable(decl.binding) orelse continue;
                return;
            }
        }
    }

    /// Find `const route = request.url` or `const route = request.path` binding.
    fn findRouteBinding(self: *HandlerAnalyzer, body_node: NodeIndex) void {
        const tag = self.ir.getTag(body_node) orelse return;

        if (tag == .block or tag == .program) {
            const block = self.ir.getBlock(body_node) orelse return;
            var i: u16 = 0;
            while (i < block.stmts_count) : (i += 1) {
                const stmt_idx = self.ir.getListIndex(block.stmts_start, i);
                self.findRouteBindingInStmt(stmt_idx);
                if (self.route_binding_slot != null) return;
            }
        }
    }

    fn findRouteBindingInStmt(self: *HandlerAnalyzer, stmt_node: NodeIndex) void {
        const tag = self.ir.getTag(stmt_node) orelse return;

        if (tag == .var_decl) {
            const decl = self.ir.getVarDecl(stmt_node) orelse return;

            // Check if init is a member access
            if (decl.init == null_node) return;
            const member = self.getMemberAccess(decl.init) orelse return;

            // Check if property is request.url or request.path
            if (member.property == @intFromEnum(object.Atom.url) or
                member.property == @intFromEnum(object.Atom.path))
            {
                // Route binding must come from request.url/path.
                if (!self.matchesOptionalSlot(member.object, self.request_binding_slot)) return;

                // This is `const route = something.url/path`
                // Store the binding slot
                const slot = bindingSlotIfBindable(decl.binding) orelse return;
                self.route_binding_slot = slot;
                self.route_atom = @enumFromInt(member.property);
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

    /// Extract pattern from a single statement (`if` or `switch`).
    fn extractPatternFromStmt(self: *HandlerAnalyzer, stmt_node: NodeIndex) !void {
        const tag = self.ir.getTag(stmt_node) orelse return;

        switch (tag) {
            .if_stmt => try self.extractPatternFromIfStmt(stmt_node),
            .return_stmt => try self.extractPatternFromReturnMatch(stmt_node),
            else => return,
        }
    }

    fn extractPatternFromIfStmt(self: *HandlerAnalyzer, stmt_node: NodeIndex) !void {
        const if_stmt = self.ir.getIfStmt(stmt_node) orelse return;

        // Analyze condition for route pattern
        const route_pattern = self.analyzeCondition(if_stmt.condition) orelse return;

        // For prefix patterns, try to detect template responses (Response.rawJson with concatenation)
        if (route_pattern.pattern_type == .prefix) {
            const template = try self.analyzeTemplateReturn(if_stmt.then_branch);
            if (template != null) {
                const t = template.?;
                try self.patterns.append(self.allocator, .{
                    .pattern_type = route_pattern.pattern_type,
                    .url_atom = route_pattern.url_atom,
                    .url_bytes = route_pattern.url_bytes,
                    .static_body = "", // Not used for templates
                    .status = t.status,
                    .content_type_idx = t.content_type_idx,
                    .body_source = .static,
                    .response_template_prefix = t.prefix,
                    .response_template_suffix = t.suffix,
                });
                return;
            }
        }

        // Analyze then branch for static response (exact matches)
        const static_response = try self.analyzeStaticReturn(if_stmt.then_branch);
        if (static_response == null) {
            self.allocator.free(route_pattern.url_bytes);
            return;
        }

        // Found a static pattern!
        const response = static_response.?;
        try self.patterns.append(self.allocator, .{
            .pattern_type = route_pattern.pattern_type,
            .url_atom = route_pattern.url_atom,
            .url_bytes = route_pattern.url_bytes,
            .static_body = response.body,
            .status = response.status,
            .content_type_idx = response.content_type_idx,
            .body_source = response.body_source,
        });
    }

    fn extractPatternFromReturnMatch(self: *HandlerAnalyzer, stmt_node: NodeIndex) !void {
        // Check if this return statement returns a match expression
        const return_val = self.ir.getOptValue(stmt_node) orelse return;
        if (return_val == null_node) return;
        const val_tag = self.ir.getTag(return_val) orelse return;
        if (val_tag != .match_expr) return;

        const match_e = self.ir.getMatchExpr(return_val) orelse return;

        // Check if discriminant is the request binding
        const discr_tag = self.ir.getTag(match_e.discriminant) orelse return;
        if (discr_tag != .identifier) return;

        var i: u8 = 0;
        while (i < match_e.arms_count) : (i += 1) {
            const arm_idx = self.ir.getListIndex(match_e.arms_start, i);
            const arm = self.ir.getMatchArm(arm_idx) orelse continue;
            if (arm.pattern == null_node) continue; // default arm

            const pattern_tag = self.ir.getTag(arm.pattern) orelse continue;

            if (pattern_tag == .match_pattern) {
                // Object pattern: extract method/path from properties
                try self.extractPatternFromMatchObject(arm.pattern, arm.body);
            } else if (pattern_tag == .lit_string) {
                // Literal pattern on request - treat as route match
                const route_info = self.analyzeSwitchCaseExact(arm.pattern) orelse continue;
                const response = (try self.analyzeResponseCall(arm.body)) orelse {
                    self.allocator.free(route_info.url_bytes);
                    continue;
                };
                try self.patterns.append(self.allocator, .{
                    .pattern_type = route_info.pattern_type,
                    .url_atom = route_info.url_atom,
                    .url_bytes = route_info.url_bytes,
                    .static_body = response.body,
                    .status = response.status,
                    .content_type_idx = response.content_type_idx,
                    .body_source = response.body_source,
                });
            }
        }
    }

    fn extractPatternFromMatchObject(self: *HandlerAnalyzer, pattern_node: NodeIndex, body: NodeIndex) !void {
        const pattern = self.ir.getMatchPattern(pattern_node) orelse return;
        var path_str: ?[]const u8 = null;

        var j: u8 = 0;
        while (j < pattern.props_count) : (j += 1) {
            const prop_idx = self.ir.getListIndex(pattern.props_start, j);
            const prop = self.ir.getProperty(prop_idx) orelse continue;
            const key_str_idx = self.ir.getStringIdx(prop.key) orelse continue;
            const key_str = self.ir.getString(key_str_idx) orelse continue;

            if (std.mem.eql(u8, key_str, "path") or std.mem.eql(u8, key_str, "url")) {
                if (prop.value != null_node) {
                    const val_tag = self.ir.getTag(prop.value) orelse continue;
                    if (val_tag == .lit_string) {
                        const val_str_idx = self.ir.getStringIdx(prop.value) orelse continue;
                        path_str = self.ir.getString(val_str_idx);
                    }
                }
                break;
            }
        }

        const route_str = path_str orelse return;
        const response = (try self.analyzeResponseCall(body)) orelse return;
        const url_bytes = self.allocator.dupe(u8, route_str) catch return;
        try self.patterns.append(self.allocator, .{
            .pattern_type = .exact,
            .url_atom = self.route_atom,
            .url_bytes = url_bytes,
            .static_body = response.body,
            .status = response.status,
            .content_type_idx = response.content_type_idx,
            .body_source = response.body_source,
        });
    }

    fn analyzeSwitchCaseExact(self: *HandlerAnalyzer, test_expr: NodeIndex) ?UrlPatternInfo {
        const test_tag = self.ir.getTag(test_expr) orelse return null;
        if (test_tag != .lit_string) return null;
        const str_idx = self.ir.getStringIdx(test_expr) orelse return null;
        const route_str = self.ir.getString(str_idx) orelse return null;
        const url_bytes = self.allocator.dupe(u8, route_str) catch return null;
        return .{
            .pattern_type = .exact,
            .url_atom = self.route_atom,
            .url_bytes = url_bytes,
        };
    }

    const UrlPatternInfo = struct {
        pattern_type: PatternType,
        url_atom: object.Atom,
        url_bytes: []const u8,
    };

    fn getBindableSlot(self: *HandlerAnalyzer, node: NodeIndex) ?u16 {
        const tag = self.ir.getTag(node) orelse return null;
        if (tag != .identifier) return null;
        const binding = self.ir.getBinding(node) orelse return null;
        return bindingSlotIfBindable(binding);
    }

    fn getMemberAccess(self: *HandlerAnalyzer, node: NodeIndex) ?Node.MemberExpr {
        const tag = self.ir.getTag(node) orelse return null;
        if (tag != .member_access) return null;
        return self.ir.getMember(node);
    }

    fn matchesSlot(self: *HandlerAnalyzer, node: NodeIndex, slot: u16) bool {
        const binding = self.ir.getBinding(node) orelse return false;
        return binding.slot == slot;
    }

    fn matchesOptionalSlot(self: *HandlerAnalyzer, node: NodeIndex, maybe_slot: ?u16) bool {
        const slot = maybe_slot orelse return true;
        return self.matchesSlot(node, slot);
    }

    fn isGlobalIdentifier(self: *HandlerAnalyzer, node: NodeIndex, atom: object.Atom) bool {
        const tag = self.ir.getTag(node) orelse return false;
        if (tag != .identifier) return false;

        const binding = self.ir.getBinding(node) orelse return false;
        if (binding.kind != .global and binding.kind != .undeclared_global) return false;
        return binding.slot == @intFromEnum(atom);
    }

    /// Analyze condition for URL matching pattern
    fn analyzeCondition(self: *HandlerAnalyzer, cond_node: NodeIndex) ?UrlPatternInfo {
        const tag = self.ir.getTag(cond_node) orelse return null;

        if (tag == .binary_op) {
            const binary = self.ir.getBinary(cond_node) orelse return null;

            // Pattern 1: url === '/api/health'
            if (binary.op == .strict_eq) {
                const exact = self.analyzeExactMatch(binary);
                if (exact != null) return exact;
                // If exact match fails, try prefix match
                return self.analyzePrefixMatch(binary);
            }

            // Pattern 2: url.indexOf('/api/greet/') === 0
            if (binary.op == .eq) {
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

        // Verify route side matches the discovered route binding
        const binding = self.ir.getBinding(url_side) orelse return null;
        if (binding.slot != self.route_binding_slot.?) return null;

        // Get URL string
        const str_idx = self.ir.getStringIdx(str_side) orelse return null;
        const url_str = self.ir.getString(str_idx) orelse return null;

        // Duplicate the URL string for storage
        const url_bytes = self.allocator.dupe(u8, url_str) catch return null;

        return .{
            .pattern_type = .exact,
            .url_atom = self.route_atom,
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
        const member = self.getMemberAccess(call.callee) orelse return null;

        // Property should be 'indexOf'
        if (member.property != @intFromEnum(object.Atom.indexOf)) return null;

        // Object should be the url identifier
        if (!self.matchesSlot(member.object, self.route_binding_slot.?)) return null;

        // Get the prefix string from the argument
        const arg_idx = self.ir.getListIndex(call.args_start, 0);
        const arg_tag = self.ir.getTag(arg_idx) orelse return null;
        if (arg_tag != .lit_string) return null;

        const str_idx = self.ir.getStringIdx(arg_idx) orelse return null;
        const prefix_str = self.ir.getString(str_idx) orelse return null;

        const url_bytes = self.allocator.dupe(u8, prefix_str) catch return null;

        return .{
            .pattern_type = .prefix,
            .url_atom = self.route_atom,
            .url_bytes = url_bytes,
        };
    }

    pub const StaticResponseInfo = struct {
        body: []const u8,
        status: u16,
        content_type_idx: u8,
        body_source: ResponseBodySource = .static,
    };

    const TemplateInfo = struct {
        prefix: []const u8,
        suffix: []const u8,
        status: u16,
        content_type_idx: u8,
    };

    const ResponseKind = enum {
        json_obj,
        raw_json,
        text,
        html,
    };

    /// Analyze then branch for template-based Response.rawJson with string concatenation
    /// Pattern: return Response.rawJson('{"greeting":"Hello, ' + name + '!"}');
    fn analyzeTemplateReturn(self: *HandlerAnalyzer, then_node: NodeIndex) !?TemplateInfo {
        const tag = self.ir.getTag(then_node) orelse return null;

        // Could be a block or direct return
        if (tag == .block) {
            const block = self.ir.getBlock(then_node) orelse return null;
            // Look for return statement in block
            var i: u16 = 0;
            while (i < block.stmts_count) : (i += 1) {
                const stmt_idx = self.ir.getListIndex(block.stmts_start, i);
                const stmt_tag = self.ir.getTag(stmt_idx) orelse continue;
                if (stmt_tag == .return_stmt) {
                    return try self.analyzeTemplateReturnStmt(stmt_idx);
                }
            }
        } else if (tag == .return_stmt) {
            return try self.analyzeTemplateReturnStmt(then_node);
        }

        return null;
    }

    fn analyzeTemplateReturnStmt(self: *HandlerAnalyzer, return_node: NodeIndex) !?TemplateInfo {
        const return_val = self.ir.getOptValue(return_node) orelse return null;
        if (return_val == null_node) return null;

        return try self.analyzeTemplateCall(return_val);
    }

    /// Analyze Response.rawJson('prefix' + var + 'suffix') pattern
    fn analyzeTemplateCall(self: *HandlerAnalyzer, call_node: NodeIndex) !?TemplateInfo {
        const tag = self.ir.getTag(call_node) orelse return null;
        if (tag != .call) return null;

        const call = self.ir.getCall(call_node) orelse return null;
        if (call.args_count < 1) return null;

        // Callee should be Response.rawJson
        const member = self.getMemberAccess(call.callee) orelse return null;
        if (!self.isGlobalIdentifier(member.object, object.Atom.Response)) return null;

        // Check method is rawJson
        if (member.property != @intFromEnum(object.Atom.rawJson)) return null;

        // Get the first argument (should be binary_op concatenation)
        const arg_idx = self.ir.getListIndex(call.args_start, 0);

        // Extract template parts from concatenation
        const template = try self.extractTemplateParts(arg_idx);
        if (template == null) return null;

        return .{
            .prefix = template.?.prefix,
            .suffix = template.?.suffix,
            .status = 200,
            .content_type_idx = 0, // JSON
        };
    }

    /// Extract prefix and suffix from string concatenation pattern
    /// Handles: 'prefix' + var + 'suffix'
    fn extractTemplateParts(self: *HandlerAnalyzer, node: NodeIndex) !?struct { prefix: []const u8, suffix: []const u8 } {
        const tag = self.ir.getTag(node) orelse return null;

        // We're looking for: binary_op(+, binary_op(+, 'prefix', var), 'suffix')
        // Which represents: ('prefix' + var) + 'suffix'
        if (tag != .binary_op) return null;

        const outer_binary = self.ir.getBinary(node) orelse return null;
        if (outer_binary.op != .add) return null;

        // Right side should be the suffix string
        const right_tag = self.ir.getTag(outer_binary.right) orelse return null;
        if (right_tag != .lit_string) return null;

        const suffix_str_idx = self.ir.getStringIdx(outer_binary.right) orelse return null;
        const suffix_str = self.ir.getString(suffix_str_idx) orelse return null;

        // Left side should be another binary addition
        const left_tag = self.ir.getTag(outer_binary.left) orelse return null;
        if (left_tag != .binary_op) return null;

        const inner_binary = self.ir.getBinary(outer_binary.left) orelse return null;
        if (inner_binary.op != .add) return null;

        // Inner left should be the prefix string
        const prefix_tag = self.ir.getTag(inner_binary.left) orelse return null;
        if (prefix_tag != .lit_string) return null;

        const prefix_str_idx = self.ir.getStringIdx(inner_binary.left) orelse return null;
        const prefix_str = self.ir.getString(prefix_str_idx) orelse return null;

        // Inner right should be a variable (we don't need to validate which one)
        // The runtime will extract the param from URL

        return .{
            .prefix = try self.allocator.dupe(u8, prefix_str),
            .suffix = try self.allocator.dupe(u8, suffix_str),
        };
    }

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

    /// Analyze handler body for a direct static return (no if-chain)
    /// Returns null if the body is not a static Response.* return.
    pub fn analyzeDirectReturn(self: *HandlerAnalyzer, func_node: NodeIndex) !?StaticResponseInfo {
        const func = self.ir.getFunction(func_node) orelse return null;
        self.request_binding_slot = null;
        self.body_binding_slot = null;
        self.findRequestBinding(func);
        self.findBodyBinding(func.body);
        return try self.analyzeStaticReturn(func.body);
    }

    fn analyzeReturnStmt(self: *HandlerAnalyzer, return_node: NodeIndex) !?StaticResponseInfo {
        const return_val = self.ir.getOptValue(return_node) orelse return null;
        if (return_val == null_node) return null;

        return try self.analyzeResponseCall(return_val);
    }

    /// Analyze Response.json({...}), Response.rawJson("..."), Response.text("..."), or Response.html("...")
    fn analyzeResponseCall(self: *HandlerAnalyzer, call_node: NodeIndex) !?StaticResponseInfo {
        const tag = self.ir.getTag(call_node) orelse return null;
        if (tag != .call) return null;

        const call = self.ir.getCall(call_node) orelse return null;
        if (call.args_count < 1) return null;

        // Callee should be Response.json or Response.text
        const member = self.getMemberAccess(call.callee) orelse return null;
        if (!self.isGlobalIdentifier(member.object, object.Atom.Response)) return null;

        const response_kind: ResponseKind = if (member.property == @intFromEnum(object.Atom.json))
            .json_obj
        else if (member.property == @intFromEnum(object.Atom.rawJson))
            .raw_json
        else if (member.property == @intFromEnum(object.Atom.text))
            .text
        else if (member.property == @intFromEnum(object.Atom.html))
            .html
        else
            return null;

        const content_type_idx: u8 = switch (response_kind) {
            .text => 1,
            .html => 2,
            .json_obj, .raw_json => 0,
        };

        // Get the first argument
        const arg_idx = self.ir.getListIndex(call.args_start, 0);

        // Try to serialize the argument
        switch (response_kind) {
            .json_obj => {
                // Check for status in second arg (options object)
                var status: u16 = 200;
                if (call.args_count >= 2) {
                    const opts_idx = self.ir.getListIndex(call.args_start, 1);
                    status = self.extractStatusFromOptions(opts_idx) orelse 200;
                }

                // JSON - serialize object literal when possible.
                const body = try self.serializeObjectLiteral(arg_idx);
                if (body != null) {
                    return .{
                        .body = body.?,
                        .status = status,
                        .content_type_idx = content_type_idx,
                        .body_source = .static,
                    };
                }

                // AOT-only dynamic pattern: Response.json(JSON.parse(request.body))
                if (self.enable_json_body_parse and self.isJsonParseOfRequestBody(arg_idx)) {
                    return .{
                        .body = "",
                        .status = status,
                        .content_type_idx = content_type_idx,
                        .body_source = .request_json_parse,
                    };
                }

                return null;
            },
            .raw_json, .text, .html => {
                // Raw JSON/text/html - first arg must be a static string literal
                const arg_tag = self.ir.getTag(arg_idx) orelse return null;
                if (arg_tag != .lit_string) return null;

                const str_idx = self.ir.getStringIdx(arg_idx) orelse return null;
                const str = self.ir.getString(str_idx) orelse return null;
                const body = try self.allocator.dupe(u8, str);

                var status: u16 = 200;
                if (call.args_count >= 2) {
                    const opts_idx = self.ir.getListIndex(call.args_start, 1);
                    status = self.extractStatusFromOptions(opts_idx) orelse 200;
                }

                return .{
                    .body = body,
                    .status = status,
                    .content_type_idx = content_type_idx,
                    .body_source = .static,
                };
            },
        }

        // Unreachable with current response_kind variants
        return null;
    }

    fn isJsonParseOfRequestBody(self: *HandlerAnalyzer, node: NodeIndex) bool {
        const tag = self.ir.getTag(node) orelse return false;
        if (tag != .call) return false;

        const call = self.ir.getCall(node) orelse return false;
        if (call.args_count != 1) return false;

        const member = self.getMemberAccess(call.callee) orelse return false;
        if (member.property != @intFromEnum(object.Atom.parse)) return false;

        // Must be JSON.parse(...)
        if (!self.isGlobalIdentifier(member.object, object.Atom.JSON)) return false;

        // Argument must be request.body or a local bound from request.body
        const arg_idx = self.ir.getListIndex(call.args_start, 0);
        return self.isRequestBodySource(arg_idx);
    }

    fn isRequestBodySource(self: *HandlerAnalyzer, node: NodeIndex) bool {
        if (self.body_binding_slot) |slot| {
            if (self.matchesSlot(node, slot)) return true;
        }

        const member = self.getMemberAccess(node) orelse return false;
        if (member.property != @intFromEnum(object.Atom.body)) return false;
        return self.matchesOptionalSlot(member.object, self.request_binding_slot);
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

            // Write key (with JSON escaping)
            try buf.append(self.allocator, '"');
            for (key_str) |c| {
                switch (c) {
                    '"' => try buf.appendSlice(self.allocator, "\\\""),
                    '\\' => try buf.appendSlice(self.allocator, "\\\\"),
                    '\n' => try buf.appendSlice(self.allocator, "\\n"),
                    '\r' => try buf.appendSlice(self.allocator, "\\r"),
                    '\t' => try buf.appendSlice(self.allocator, "\\t"),
                    else => try buf.append(self.allocator, c),
                }
            }
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

    try std.testing.expect(analyzer.route_binding_slot == null);
}

fn findHandlerFunctionForTest(ir_view: IrView, root: NodeIndex) ?NodeIndex {
    const tag = ir_view.getTag(root) orelse return null;
    if (tag != .program and tag != .block) return null;
    const block = ir_view.getBlock(root) orelse return null;

    var i: u16 = 0;
    while (i < block.stmts_count) : (i += 1) {
        const stmt_idx = ir_view.getListIndex(block.stmts_start, i);
        const stmt_tag = ir_view.getTag(stmt_idx) orelse continue;
        if (stmt_tag != .function_decl and stmt_tag != .var_decl) continue;

        const decl = ir_view.getVarDecl(stmt_idx) orelse continue;
        if (decl.binding.kind != .global) continue;
        if (decl.binding.slot != @intFromEnum(object.Atom.handler)) continue;

        const init_tag = ir_view.getTag(decl.init) orelse continue;
        if (init_tag == .function_expr or init_tag == .arrow_function) {
            return decl.init;
        }
    }

    return null;
}

fn bindingSlotIfBindable(binding: anytype) ?u16 {
    return switch (binding.kind) {
        .local, .argument => binding.slot,
        else => null,
    };
}

test "HandlerAnalyzer extracts match route patterns" {
    const allocator = std.testing.allocator;

    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();

    const source =
        \\function handler(request) {
        \\  const url = request.url;
        \\  return match (url) {
        \\    when '/api/a': Response.json({ ok: true }),
        \\    when '/api/b': Response.text('plain'),
        \\    default: Response.json({ error: 'nf' }, { status: 404 }),
        \\  };
        \\}
    ;

    var js_parser = @import("parser/parse.zig").Parser.init(allocator, source);
    defer js_parser.deinit();
    js_parser.setAtomTable(&atoms);

    const root = try js_parser.parse();
    const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
    const handler_fn = findHandlerFunctionForTest(ir_view, root) orelse return error.InvalidRequest;

    var analyzer = HandlerAnalyzer.init(allocator, ir_view, &atoms);
    defer analyzer.deinit();

    const dispatch_opt = try analyzer.analyze(handler_fn);
    try std.testing.expect(dispatch_opt != null);

    const dispatch = dispatch_opt.?;
    defer {
        dispatch.deinit();
        allocator.destroy(dispatch);
    }

    try std.testing.expectEqual(@as(usize, 2), dispatch.patterns.len);
    try std.testing.expectEqual(PatternType.exact, dispatch.patterns[0].pattern_type);
    try std.testing.expectEqualStrings("/api/a", dispatch.patterns[0].url_bytes);
    try std.testing.expectEqualStrings("{\"ok\":true}", dispatch.patterns[0].static_body);
    try std.testing.expectEqual(PatternType.exact, dispatch.patterns[1].pattern_type);
    try std.testing.expectEqualStrings("/api/b", dispatch.patterns[1].url_bytes);
    try std.testing.expectEqualStrings("plain", dispatch.patterns[1].static_body);
}

test "HandlerAnalyzer detects Response.json(JSON.parse(request.body))" {
    const allocator = std.testing.allocator;

    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();

    const source =
        \\function handler(request) {
        \\  const url = request.url;
        \\  const body = request.body;
        \\  if (url === '/api/echo') {
        \\    return Response.json(JSON.parse(body), { status: 201 });
        \\  }
        \\}
    ;

    var js_parser = @import("parser/parse.zig").Parser.init(allocator, source);
    defer js_parser.deinit();
    js_parser.setAtomTable(&atoms);

    const root = try js_parser.parse();
    const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
    const handler_fn = findHandlerFunctionForTest(ir_view, root) orelse return error.InvalidRequest;

    var analyzer = HandlerAnalyzer.init(allocator, ir_view, &atoms);
    defer analyzer.deinit();
    analyzer.enableJsonBodyParsePatterns();

    const dispatch_opt = try analyzer.analyze(handler_fn);
    try std.testing.expect(dispatch_opt != null);

    const dispatch = dispatch_opt.?;
    defer {
        dispatch.deinit();
        allocator.destroy(dispatch);
    }

    try std.testing.expectEqual(@as(usize, 1), dispatch.patterns.len);
    const p = dispatch.patterns[0];
    try std.testing.expectEqual(PatternType.exact, p.pattern_type);
    try std.testing.expectEqualStrings("/api/echo", p.url_bytes);
    try std.testing.expectEqual(ResponseBodySource.request_json_parse, p.body_source);
    try std.testing.expectEqual(@as(u16, 201), p.status);
    try std.testing.expectEqual(@as(u8, 0), p.content_type_idx);
    try std.testing.expectEqual(@as(usize, 0), p.static_body.len);
}
