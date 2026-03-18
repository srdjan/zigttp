//! Handler Contract Manifest
//!
//! Extracts a machine-readable contract from a handler's IR describing what
//! the handler is allowed to do: which routes it serves, which virtual modules
//! it uses, which env vars / outbound hosts / cache namespaces it references.
//!
//! This is purely an emission pass (v1). It does not enforce anything at runtime.
//! Enforcement and doc generation are v2 concerns.
//!
//! Usage: called by precompile.zig when --contract is passed.
//!
//! All string data in HandlerContract is owned (duped). The contract outlives
//! the parser and atom table that produced it.

const std = @import("std");
const ir = @import("parser/ir.zig");
const object = @import("object.zig");
const context = @import("context.zig");
const modules_resolver = @import("modules/resolver.zig");
const bytecode = @import("bytecode.zig");

const Node = ir.Node;
const NodeIndex = ir.NodeIndex;
const NodeTag = ir.NodeTag;
const IrView = ir.IrView;
const null_node = ir.null_node;
const HandlerPattern = bytecode.HandlerPattern;
const PatternDispatchTable = bytecode.PatternDispatchTable;

// -------------------------------------------------------------------------
// Contract data types
// -------------------------------------------------------------------------

pub const HandlerLoc = struct {
    path: []const u8, // owned
    line: u32,
    column: u32,
};

pub const RouteInfo = struct {
    pattern: []const u8, // owned
    route_type: []const u8, // static literal, not owned
    field: []const u8, // static literal, not owned
    status: u16,
    content_type: []const u8, // static literal, not owned
    aot: bool,
};

pub const EnvInfo = struct {
    literal: std.ArrayList([]const u8), // each entry owned
    dynamic: bool,
};

pub const EgressInfo = struct {
    hosts: std.ArrayList([]const u8), // each entry owned
    dynamic: bool,
};

pub const CacheInfo = struct {
    namespaces: std.ArrayList([]const u8), // each entry owned
    dynamic: bool,
};

pub const VerificationInfo = struct {
    exhaustive_returns: bool,
    results_safe: bool,
    unreachable_code: bool,
    /// Bytecode passed structural verification (opcode validity, bounds, stack discipline)
    bytecode_verified: bool = false,
    /// Sound mode checks passed (boolean enforcement, type inference)
    sound_mode_passed: bool = false,
};

pub const AotInfo = struct {
    pattern_count: u32,
    has_default: bool,
};

pub const HandlerContract = struct {
    version: u32 = 1,
    handler: HandlerLoc,
    routes: std.ArrayList(RouteInfo),
    modules: std.ArrayList([]const u8), // each entry owned
    functions: std.ArrayList(FunctionEntry),
    env: EnvInfo,
    egress: EgressInfo,
    cache: CacheInfo,
    verification: ?VerificationInfo,
    aot: ?AotInfo,

    pub const FunctionEntry = struct {
        module: []const u8, // owned
        names: std.ArrayList([]const u8), // each entry owned
    };

    pub fn deinit(self: *HandlerContract, allocator: std.mem.Allocator) void {
        allocator.free(self.handler.path);
        for (self.routes.items) |route| {
            allocator.free(route.pattern);
        }
        self.routes.deinit(allocator);
        for (self.modules.items) |m| {
            allocator.free(m);
        }
        self.modules.deinit(allocator);
        for (self.functions.items) |*entry| {
            allocator.free(entry.module);
            for (entry.names.items) |n| {
                allocator.free(n);
            }
            entry.names.deinit(allocator);
        }
        self.functions.deinit(allocator);
        for (self.env.literal.items) |s| {
            allocator.free(s);
        }
        self.env.literal.deinit(allocator);
        for (self.egress.hosts.items) |s| {
            allocator.free(s);
        }
        self.egress.hosts.deinit(allocator);
        for (self.cache.namespaces.items) |s| {
            allocator.free(s);
        }
        self.cache.namespaces.deinit(allocator);
    }
};

// -------------------------------------------------------------------------
// Contract builder
// -------------------------------------------------------------------------

/// Builds a HandlerContract by walking the IR once.
/// All strings stored in the contract are duped via the allocator, making
/// the contract safe to use after the parser and atom table are freed.
pub const ContractBuilder = struct {
    allocator: std.mem.Allocator,
    ir_view: IrView,
    atoms: ?*context.AtomTable,

    // Binding tracking: maps local slot -> category for call-site analysis
    env_binding_slots: std.ArrayList(u16),
    cache_binding_slots: std.ArrayList(CacheBinding),

    // Collected data (all strings are duped/owned)
    modules_list: std.ArrayList([]const u8),
    functions_map: std.ArrayList(HandlerContract.FunctionEntry),
    env_literals: std.ArrayList([]const u8),
    env_dynamic: bool,
    egress_hosts: std.ArrayList([]const u8),
    egress_dynamic: bool,
    cache_namespaces: std.ArrayList([]const u8),
    cache_dynamic: bool,

    const CacheBinding = struct {
        slot: u16,
        has_namespace_arg: bool, // true for cacheGet/Set/Delete/Incr, false for cacheStats
    };

    pub fn init(allocator: std.mem.Allocator, ir_view: IrView, atoms: ?*context.AtomTable) ContractBuilder {
        return .{
            .allocator = allocator,
            .ir_view = ir_view,
            .atoms = atoms,
            .env_binding_slots = .empty,
            .cache_binding_slots = .empty,
            .modules_list = .empty,
            .functions_map = .empty,
            .env_literals = .empty,
            .env_dynamic = false,
            .egress_hosts = .empty,
            .egress_dynamic = false,
            .cache_namespaces = .empty,
            .cache_dynamic = false,
        };
    }

    /// Free all builder-owned resources. Safe to call whether or not build()
    /// was called: if build() moved the lists into a HandlerContract, the
    /// items slices are empty and these loops are no-ops.
    pub fn deinit(self: *ContractBuilder) void {
        self.env_binding_slots.deinit(self.allocator);
        self.cache_binding_slots.deinit(self.allocator);
        for (self.env_literals.items) |s| self.allocator.free(s);
        self.env_literals.deinit(self.allocator);
        for (self.egress_hosts.items) |s| self.allocator.free(s);
        self.egress_hosts.deinit(self.allocator);
        for (self.cache_namespaces.items) |s| self.allocator.free(s);
        self.cache_namespaces.deinit(self.allocator);
        for (self.modules_list.items) |s| self.allocator.free(s);
        self.modules_list.deinit(self.allocator);
        for (self.functions_map.items) |*entry| {
            self.allocator.free(entry.module);
            for (entry.names.items) |n| self.allocator.free(n);
            entry.names.deinit(self.allocator);
        }
        self.functions_map.deinit(self.allocator);
    }

    /// Build the contract from the IR. Single-pass walk over all nodes.
    /// The returned HandlerContract owns all its string data.
    pub fn build(
        self: *ContractBuilder,
        handler_path: []const u8,
        handler_loc: ?ir.SourceLocation,
        dispatch: ?*const PatternDispatchTable,
        default_response: bool,
        verification: ?VerificationInfo,
    ) !HandlerContract {
        // Phase 1: Scan imports to discover modules, functions, and binding slots
        try self.scanImports();

        // Phase 2: Scan all call sites for env/fetchSync/cache usage
        try self.scanCallSites();

        // Build routes from dispatch table
        var routes: std.ArrayList(RouteInfo) = .empty;
        errdefer {
            for (routes.items) |r| self.allocator.free(r.pattern);
            routes.deinit(self.allocator);
        }
        if (dispatch) |d| {
            for (d.patterns) |pattern| {
                const is_aot = switch (pattern.pattern_type) {
                    .exact => true,
                    .prefix => pattern.response_template_prefix != null,
                    else => false,
                };
                if (!is_aot) continue;

                const pattern_dupe = try self.allocator.dupe(u8, pattern.url_bytes);
                errdefer self.allocator.free(pattern_dupe);

                try routes.append(self.allocator, .{
                    .pattern = pattern_dupe,
                    .route_type = switch (pattern.pattern_type) {
                        .exact => "exact",
                        .prefix => "prefix",
                        else => "unknown",
                    },
                    .field = switch (pattern.url_atom) {
                        .path => "path",
                        else => "url",
                    },
                    .status = pattern.status,
                    .content_type = contentTypeFor(pattern.content_type_idx),
                    .aot = is_aot,
                });
            }
        }

        // Compute AOT info
        var aot_info: ?AotInfo = null;
        if (dispatch) |d| {
            var pattern_count: u32 = 0;
            for (d.patterns) |pattern| {
                switch (pattern.pattern_type) {
                    .exact => pattern_count += 1,
                    .prefix => {
                        if (pattern.response_template_prefix != null) pattern_count += 1;
                    },
                    else => {},
                }
            }
            aot_info = .{
                .pattern_count = pattern_count,
                .has_default = default_response,
            };
        }

        const contract = HandlerContract{
            .handler = .{
                .path = try self.allocator.dupe(u8, handler_path),
                .line = if (handler_loc) |loc| loc.line else 0,
                .column = if (handler_loc) |loc| loc.column else 0,
            },
            .routes = routes,
            .modules = self.modules_list,
            .functions = self.functions_map,
            .env = .{
                .literal = self.env_literals,
                .dynamic = self.env_dynamic,
            },
            .egress = .{
                .hosts = self.egress_hosts,
                .dynamic = self.egress_dynamic,
            },
            .cache = .{
                .namespaces = self.cache_namespaces,
                .dynamic = self.cache_dynamic,
            },
            .verification = verification,
            .aot = aot_info,
        };

        // Clear moved lists so deinit() won't double-free
        self.modules_list = .empty;
        self.functions_map = .empty;
        self.env_literals = .empty;
        self.egress_hosts = .empty;
        self.cache_namespaces = .empty;

        return contract;
    }

    // -----------------------------------------------------------------
    // Phase 1: Import scanning
    // -----------------------------------------------------------------

    fn scanImports(self: *ContractBuilder) !void {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            if (tag != .import_decl) continue;

            const import_decl = self.ir_view.getImportDecl(idx) orelse continue;
            const module_str = self.ir_view.getString(import_decl.module_idx) orelse continue;

            // Only track virtual modules
            const vm = modules_resolver.VirtualModule.fromSpecifier(module_str) orelse continue;

            // Add module to list (deduplicated, duped)
            if (!containsString(self.modules_list.items, module_str)) {
                const duped = try self.allocator.dupe(u8, module_str);
                errdefer self.allocator.free(duped);
                try self.modules_list.append(self.allocator, duped);
            }

            // Scan specifiers to track function names and binding slots
            var func_names: std.ArrayList([]const u8) = .empty;
            var func_module_str: []const u8 = "";

            var j: u8 = 0;
            while (j < import_decl.specifiers_count) : (j += 1) {
                const spec_idx = self.ir_view.getListIndex(import_decl.specifiers_start, j);
                const spec = self.ir_view.getImportSpec(spec_idx) orelse continue;
                const imported_name = self.resolveAtomName(spec.imported_atom) orelse continue;

                const name_duped = try self.allocator.dupe(u8, imported_name);
                errdefer self.allocator.free(name_duped);
                try func_names.append(self.allocator, name_duped);
                func_module_str = module_str;

                // Track env bindings
                if (vm == .env and std.mem.eql(u8, imported_name, "env")) {
                    try self.env_binding_slots.append(self.allocator, spec.local_binding.slot);
                }

                // Track cache bindings (functions with namespace as first arg)
                if (vm == .cache) {
                    const has_ns = std.mem.eql(u8, imported_name, "cacheGet") or
                        std.mem.eql(u8, imported_name, "cacheSet") or
                        std.mem.eql(u8, imported_name, "cacheDelete") or
                        std.mem.eql(u8, imported_name, "cacheIncr");
                    try self.cache_binding_slots.append(self.allocator, .{
                        .slot = spec.local_binding.slot,
                        .has_namespace_arg = has_ns,
                    });
                }
            }

            if (func_names.items.len > 0) {
                // Merge with existing entry for same module, or add new
                var merged = false;
                for (self.functions_map.items) |*existing| {
                    if (std.mem.eql(u8, existing.module, func_module_str)) {
                        for (func_names.items) |name| {
                            if (!containsString(existing.names.items, name)) {
                                try existing.names.append(self.allocator, name);
                            } else {
                                self.allocator.free(name);
                            }
                        }
                        func_names.deinit(self.allocator);
                        merged = true;
                        break;
                    }
                }
                if (!merged) {
                    const module_duped = try self.allocator.dupe(u8, func_module_str);
                    errdefer self.allocator.free(module_duped);
                    try self.functions_map.append(self.allocator, .{
                        .module = module_duped,
                        .names = func_names,
                    });
                }
            } else {
                func_names.deinit(self.allocator);
            }
        }
    }

    // -----------------------------------------------------------------
    // Phase 2: Call site scanning
    // -----------------------------------------------------------------

    fn scanCallSites(self: *ContractBuilder) !void {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            if (tag != .call) continue;

            const call = self.ir_view.getCall(idx) orelse continue;

            // Check what the callee is
            const callee_tag = self.ir_view.getTag(call.callee) orelse continue;

            if (callee_tag == .identifier) {
                const binding = self.ir_view.getBinding(call.callee) orelse continue;

                // Check for fetchSync (undeclared global)
                if (binding.kind == .undeclared_global) {
                    const name = self.resolveAtomName(binding.slot) orelse continue;
                    if (std.mem.eql(u8, name, "fetchSync")) {
                        try self.extractLiteralArg(call, &self.egress_hosts, &self.egress_dynamic, &extractHost);
                        continue;
                    }
                }

                // Check for env() calls
                if (self.isEnvBinding(binding.slot)) {
                    try self.extractLiteralArg(call, &self.env_literals, &self.env_dynamic, null);
                    continue;
                }

                // Check for cache calls
                if (self.isCacheBindingWithNamespace(binding.slot)) {
                    try self.extractLiteralArg(call, &self.cache_namespaces, &self.cache_dynamic, null);
                    continue;
                }
            }
        }
    }

    /// Extract a literal string from the first argument of a call and append
    /// it (deduped, owned) to `target`. If the argument is non-literal, set
    /// `dynamic_flag`. An optional `transform` narrows the extracted string
    /// (e.g. extractHost for URLs) - returning empty means "treat as dynamic".
    fn extractLiteralArg(
        self: *ContractBuilder,
        call: Node.CallExpr,
        target: *std.ArrayList([]const u8),
        dynamic_flag: *bool,
        transform: ?*const fn ([]const u8) []const u8,
    ) !void {
        if (call.args_count < 1) return;

        const arg_idx = self.ir_view.getListIndex(call.args_start, 0);
        const arg_tag = self.ir_view.getTag(arg_idx) orelse return;

        if (arg_tag == .lit_string) {
            const str_idx = self.ir_view.getStringIdx(arg_idx) orelse return;
            const raw = self.ir_view.getString(str_idx) orelse return;
            const value = if (transform) |t| t(raw) else raw;
            if (value.len > 0) {
                if (!containsString(target.items, value)) {
                    const duped = try self.allocator.dupe(u8, value);
                    errdefer self.allocator.free(duped);
                    try target.append(self.allocator, duped);
                }
            } else {
                dynamic_flag.* = true;
            }
        } else {
            dynamic_flag.* = true;
        }
    }

    // -----------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------

    fn isEnvBinding(self: *const ContractBuilder, slot: u16) bool {
        for (self.env_binding_slots.items) |s| {
            if (s == slot) return true;
        }
        return false;
    }

    fn isCacheBindingWithNamespace(self: *const ContractBuilder, slot: u16) bool {
        for (self.cache_binding_slots.items) |cb| {
            if (cb.slot == slot and cb.has_namespace_arg) return true;
        }
        return false;
    }

    fn resolveAtomName(self: *const ContractBuilder, atom_idx: u16) ?[]const u8 {
        // Try predefined atoms first
        const atom: object.Atom = @enumFromInt(atom_idx);
        if (atom.toPredefinedName()) |name| return name;

        // Try dynamic atom table
        if (self.atoms) |table| {
            return table.getName(atom);
        }
        return null;
    }
};

fn containsString(items: []const []const u8, needle: []const u8) bool {
    for (items) |item| {
        if (std.mem.eql(u8, item, needle)) return true;
    }
    return false;
}

// -------------------------------------------------------------------------
// Host extraction from URL strings
// -------------------------------------------------------------------------

/// Extract hostname from a URL string (e.g. "https://api.example.com/path" -> "api.example.com")
fn extractHost(url: []const u8) []const u8 {
    // Skip scheme
    var start: usize = 0;
    if (std.mem.indexOf(u8, url, "://")) |scheme_end| {
        start = scheme_end + 3;
    } else {
        return "";
    }

    // Find end of host (first / or : after scheme, or end of string)
    var end = start;
    while (end < url.len) : (end += 1) {
        if (url[end] == '/' or url[end] == ':') break;
    }

    if (end <= start) return "";
    return url[start..end];
}

fn contentTypeFor(idx: u8) []const u8 {
    return switch (idx) {
        0 => "application/json",
        1 => "text/plain; charset=utf-8",
        else => "text/html; charset=utf-8",
    };
}

// -------------------------------------------------------------------------
// JSON serialization
// -------------------------------------------------------------------------

/// Write the contract as JSON to a writer.
pub fn writeContractJson(contract: *const HandlerContract, writer: anytype) !void {
    try writer.writeAll("{\n");

    // version
    try writer.print("  \"version\": {d},\n", .{contract.version});

    // handler
    try writer.writeAll("  \"handler\": {\n");
    try writer.writeAll("    \"path\": ");
    try writeJsonString(writer, contract.handler.path);
    try writer.writeAll(",\n");
    try writer.print("    \"line\": {d},\n", .{contract.handler.line});
    try writer.print("    \"column\": {d}\n", .{contract.handler.column});
    try writer.writeAll("  },\n");

    // routes
    try writer.writeAll("  \"routes\": [");
    for (contract.routes.items, 0..) |route, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("\n    {\n");
        try writer.writeAll("      \"pattern\": ");
        try writeJsonString(writer, route.pattern);
        try writer.writeAll(",\n");
        try writer.writeAll("      \"type\": ");
        try writeJsonString(writer, route.route_type);
        try writer.writeAll(",\n");
        try writer.writeAll("      \"field\": ");
        try writeJsonString(writer, route.field);
        try writer.writeAll(",\n");
        try writer.print("      \"status\": {d},\n", .{route.status});
        try writer.writeAll("      \"contentType\": ");
        try writeJsonString(writer, route.content_type);
        try writer.writeAll(",\n");
        try writer.print("      \"aot\": {s}\n", .{if (route.aot) "true" else "false"});
        try writer.writeAll("    }");
    }
    if (contract.routes.items.len > 0) {
        try writer.writeAll("\n  ");
    }
    try writer.writeAll("],\n");

    // modules
    try writer.writeAll("  \"modules\": [");
    for (contract.modules.items, 0..) |mod, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, mod);
    }
    try writer.writeAll("],\n");

    // functions
    try writer.writeAll("  \"functions\": {");
    for (contract.functions.items, 0..) |entry, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("\n    ");
        try writeJsonString(writer, entry.module);
        try writer.writeAll(": [");
        for (entry.names.items, 0..) |name, j| {
            if (j > 0) try writer.writeAll(", ");
            try writeJsonString(writer, name);
        }
        try writer.writeAll("]");
    }
    if (contract.functions.items.len > 0) {
        try writer.writeAll("\n  ");
    }
    try writer.writeAll("},\n");

    // env
    try writer.writeAll("  \"env\": {\n");
    try writer.writeAll("    \"literal\": [");
    for (contract.env.literal.items, 0..) |name, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, name);
    }
    try writer.writeAll("],\n");
    try writer.print("    \"dynamic\": {s}\n", .{if (contract.env.dynamic) "true" else "false"});
    try writer.writeAll("  },\n");

    // egress
    try writer.writeAll("  \"egress\": {\n");
    try writer.writeAll("    \"hosts\": [");
    for (contract.egress.hosts.items, 0..) |host, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, host);
    }
    try writer.writeAll("],\n");
    try writer.print("    \"dynamic\": {s}\n", .{if (contract.egress.dynamic) "true" else "false"});
    try writer.writeAll("  },\n");

    // cache
    try writer.writeAll("  \"cache\": {\n");
    try writer.writeAll("    \"namespaces\": [");
    for (contract.cache.namespaces.items, 0..) |ns, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, ns);
    }
    try writer.writeAll("],\n");
    try writer.print("    \"dynamic\": {s}\n", .{if (contract.cache.dynamic) "true" else "false"});
    try writer.writeAll("  },\n");

    // verification (optional)
    if (contract.verification) |v| {
        try writer.writeAll("  \"verification\": {\n");
        try writer.print("    \"exhaustiveReturns\": {s},\n", .{if (v.exhaustive_returns) "true" else "false"});
        try writer.print("    \"resultsSafe\": {s},\n", .{if (v.results_safe) "true" else "false"});
        try writer.print("    \"unreachableCode\": {s},\n", .{if (v.unreachable_code) "true" else "false"});
        try writer.print("    \"bytecodeVerified\": {s},\n", .{if (v.bytecode_verified) "true" else "false"});
        try writer.print("    \"soundModePassed\": {s}\n", .{if (v.sound_mode_passed) "true" else "false"});
        try writer.writeAll("  },\n");
    } else {
        try writer.writeAll("  \"verification\": null,\n");
    }

    // aot (optional)
    if (contract.aot) |a| {
        try writer.writeAll("  \"aot\": {\n");
        try writer.print("    \"patternCount\": {d},\n", .{a.pattern_count});
        try writer.print("    \"hasDefault\": {s}\n", .{if (a.has_default) "true" else "false"});
        try writer.writeAll("  }\n");
    } else {
        try writer.writeAll("  \"aot\": null\n");
    }

    try writer.writeAll("}\n");
}

fn writeJsonString(writer: anytype, s: []const u8) !void {
    try writer.writeByte('"');
    for (s) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            0x00...0x08, 0x0b...0x0c, 0x0e...0x1f => {
                try writer.print("\\u{x:0>4}", .{@as(u16, c)});
            },
            else => try writer.writeByte(c),
        }
    }
    try writer.writeByte('"');
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

test "extractHost from URL" {
    try std.testing.expectEqualStrings("api.example.com", extractHost("https://api.example.com/path"));
    try std.testing.expectEqualStrings("api.example.com", extractHost("http://api.example.com:8080/path"));
    try std.testing.expectEqualStrings("localhost", extractHost("http://localhost/test"));
    try std.testing.expectEqualStrings("", extractHost("not-a-url"));
    try std.testing.expectEqualStrings("", extractHost(""));
}

test "writeJsonString escapes correctly" {
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &output);

    try writeJsonString(&aw.writer, "hello \"world\"");
    output = aw.toArrayList();
    try std.testing.expectEqualStrings("\"hello \\\"world\\\"\"", output.items);
}

test "writeContractJson minimal" {
    const allocator = std.testing.allocator;

    const path = try allocator.dupe(u8, "handler.ts");

    var contract = HandlerContract{
        .handler = .{ .path = path, .line = 1, .column = 0 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = .empty, .dynamic = false },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .verification = null,
        .aot = null,
    };
    defer contract.deinit(allocator);

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    try writeContractJson(&contract, &aw.writer);
    output = aw.toArrayList();

    // Should be valid-looking JSON with expected fields
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"version\": 1") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"handler.ts\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"modules\": []") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"verification\": null") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"aot\": null") != null);
}

test "writeContractJson with data" {
    const allocator = std.testing.allocator;

    const path = try allocator.dupe(u8, "handler.ts");

    var modules: std.ArrayList([]const u8) = .empty;
    const mod_str = try allocator.dupe(u8, "zigttp:auth");
    try modules.append(allocator, mod_str);

    var func_names: std.ArrayList([]const u8) = .empty;
    const fn_str = try allocator.dupe(u8, "jwtVerify");
    try func_names.append(allocator, fn_str);

    var functions: std.ArrayList(HandlerContract.FunctionEntry) = .empty;
    const func_mod = try allocator.dupe(u8, "zigttp:auth");
    try functions.append(allocator, .{
        .module = func_mod,
        .names = func_names,
    });

    var env_lit: std.ArrayList([]const u8) = .empty;
    const env_str = try allocator.dupe(u8, "JWT_SECRET");
    try env_lit.append(allocator, env_str);

    var hosts: std.ArrayList([]const u8) = .empty;
    const host_str = try allocator.dupe(u8, "api.example.com");
    try hosts.append(allocator, host_str);

    var namespaces: std.ArrayList([]const u8) = .empty;
    const ns_str = try allocator.dupe(u8, "sessions");
    try namespaces.append(allocator, ns_str);

    var contract = HandlerContract{
        .handler = .{ .path = path, .line = 20, .column = 1 },
        .routes = .empty,
        .modules = modules,
        .functions = functions,
        .env = .{ .literal = env_lit, .dynamic = false },
        .egress = .{ .hosts = hosts, .dynamic = true },
        .cache = .{ .namespaces = namespaces, .dynamic = false },
        .verification = .{
            .exhaustive_returns = true,
            .results_safe = true,
            .unreachable_code = false,
        },
        .aot = .{ .pattern_count = 3, .has_default = true },
    };
    defer contract.deinit(allocator);

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    try writeContractJson(&contract, &aw.writer);
    output = aw.toArrayList();

    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"zigttp:auth\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"jwtVerify\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"JWT_SECRET\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"api.example.com\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"sessions\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"exhaustiveReturns\": true") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"patternCount\": 3") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"dynamic\": true") != null);
}
