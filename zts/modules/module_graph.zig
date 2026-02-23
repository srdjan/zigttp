//! Module Dependency Graph
//!
//! Builds a dependency graph from an entry file by recursively scanning
//! import declarations. Performs topological sort for correct execution
//! order and detects circular imports.

const std = @import("std");
const file_resolver = @import("file_resolver.zig");
const resolver = @import("resolver.zig");

// Engine types
const zts_parser = @import("../parser/root.zig");
const context = @import("../context.zig");
const stripper = @import("../stripper.zig");

pub const ModuleIndex = u16;

/// Maximum import nesting depth to prevent runaway recursion
const MAX_NESTING_DEPTH = 32;

/// Callback type for reading file contents. Returns owned slice or error.
pub const ReadFileFn = *const fn (allocator: std.mem.Allocator, path: []const u8) ReadFileError![]const u8;

pub const ReadFileError = error{
    FileNotFound,
    OutOfMemory,
    FileTooBig,
    AccessDenied,
    Unexpected,
    InputOutput,
    IsDir,
    SystemResources,
    InvalidArgument,
    BrokenPipe,
    ConnectionResetByPeer,
    ConnectionTimedOut,
    NotOpenForReading,
    SocketNotConnected,
    WouldBlock,
    OperationAborted,
    ProcessNotFound,
};

pub const Module = struct {
    path: []const u8,
    source: []const u8,
    stripped_source: ?[]const u8,
    dependencies: []ModuleIndex,
    state: DfsState,

    const DfsState = enum { unvisited, visiting, visited };

    fn deinit(self: *Module, allocator: std.mem.Allocator) void {
        allocator.free(self.path);
        allocator.free(self.source);
        if (self.stripped_source) |ss| {
            allocator.free(ss);
        }
        if (self.dependencies.len > 0) {
            allocator.free(self.dependencies);
        }
    }
};

pub const ModuleGraph = struct {
    allocator: std.mem.Allocator,
    modules: std.StringHashMap(ModuleIndex),
    module_list: std.ArrayList(Module),
    execution_order: []ModuleIndex,

    pub fn init(allocator: std.mem.Allocator) ModuleGraph {
        return .{
            .allocator = allocator,
            .modules = std.StringHashMap(ModuleIndex).init(allocator),
            .module_list = std.ArrayList(Module).empty,
            .execution_order = &.{},
        };
    }

    pub fn deinit(self: *ModuleGraph) void {
        for (self.module_list.items) |*m| {
            m.deinit(self.allocator);
        }
        self.module_list.deinit(self.allocator);
        self.modules.deinit();
        if (self.execution_order.len > 0) {
            self.allocator.free(self.execution_order);
        }
    }

    /// Get the number of modules (excluding the entry file)
    pub fn dependencyCount(self: *const ModuleGraph) usize {
        if (self.execution_order.len == 0) return 0;
        // Last element in execution_order is the entry file
        return self.execution_order.len - 1;
    }

    /// Build the module graph starting from the entry file.
    /// The entry file itself is added as the last module in execution_order.
    pub fn build(
        self: *ModuleGraph,
        entry_path: []const u8,
        entry_source: []const u8,
        read_file: ReadFileFn,
    ) !void {
        // Add entry module (dupe path and source since addModule takes ownership)
        const owned_path = try self.allocator.dupe(u8, entry_path);
        const owned_source = try self.allocator.dupe(u8, entry_source);
        const entry_idx = try self.addModule(owned_path, owned_source);

        // Recursively discover dependencies
        try self.discoverDependencies(entry_idx, read_file, 0);

        // Topological sort
        try self.topologicalSort();
    }

    /// Add a module to the graph. Takes ownership of both path and source.
    fn addModule(self: *ModuleGraph, owned_path: []const u8, owned_source: []const u8) !ModuleIndex {
        const idx: ModuleIndex = @intCast(self.module_list.items.len);

        // Strip TypeScript if needed
        var stripped: ?[]const u8 = null;
        if (std.mem.endsWith(u8, owned_path, ".ts") or std.mem.endsWith(u8, owned_path, ".tsx")) {
            const is_tsx = std.mem.endsWith(u8, owned_path, ".tsx");
            const strip_result = stripper.strip(self.allocator, owned_source, .{
                .tsx_mode = is_tsx,
            }) catch null;
            if (strip_result) |sr| {
                stripped = sr.code;
            }
        }

        try self.module_list.append(self.allocator, .{
            .path = owned_path,
            .source = owned_source,
            .stripped_source = stripped,
            .dependencies = &.{},
            .state = .unvisited,
        });

        // HashMap borrows path from the module (module owns it)
        try self.modules.put(owned_path, idx);

        return idx;
    }

    fn discoverDependencies(self: *ModuleGraph, module_idx: ModuleIndex, read_file: ReadFileFn, depth: u32) !void {
        if (depth > MAX_NESTING_DEPTH) return error.ImportNestingTooDeep;

        var module = &self.module_list.items[module_idx];
        const source = module.stripped_source orelse module.source;

        // Quick-parse to extract import declarations
        var js_parser = zts_parser.JsParser.init(self.allocator, source);
        defer js_parser.deinit();

        // Enable JSX if needed
        if (std.mem.endsWith(u8, module.path, ".jsx") or std.mem.endsWith(u8, module.path, ".tsx")) {
            js_parser.tokenizer.enableJsx();
        }

        _ = js_parser.parse() catch return; // Parse errors handled later during compilation
        const view = zts_parser.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);

        // Extract file imports
        var deps = std.ArrayList(ModuleIndex).empty;
        errdefer deps.deinit(self.allocator);

        const node_count = view.nodeCount();
        for (0..node_count) |idx| {
            const tag = view.getTag(@intCast(idx)) orelse continue;
            if (tag != .import_decl) continue;

            const import_decl = view.getImportDecl(@intCast(idx)) orelse continue;
            const module_str = view.getString(import_decl.module_idx) orelse continue;

            // Only process file imports (skip virtual modules)
            const resolve_result = resolver.resolve(module_str);
            switch (resolve_result) {
                .file => |specifier| {
                    const importing_dir = file_resolver.dirName(module.path);
                    const resolved_path = try file_resolver.resolve(self.allocator, specifier, importing_dir, null);

                    // Check if already in graph (dedup)
                    if (self.modules.get(resolved_path)) |existing_idx| {
                        self.allocator.free(resolved_path);
                        try deps.append(self.allocator, existing_idx);
                        continue;
                    }

                    // Read the file
                    const dep_source = read_file(self.allocator, resolved_path) catch {
                        self.allocator.free(resolved_path);
                        return error.ImportFileNotFound;
                    };

                    // addModule takes ownership of resolved_path and dep_source
                    const dep_idx = try self.addModule(resolved_path, dep_source);

                    try deps.append(self.allocator, dep_idx);

                    // Recurse into the dependency
                    try self.discoverDependencies(dep_idx, read_file, depth + 1);
                },
                .virtual, .unknown => {},
            }
        }

        // Re-fetch module pointer (module_list may have been reallocated)
        module = &self.module_list.items[module_idx];
        module.dependencies = try deps.toOwnedSlice(self.allocator);
    }

    fn topologicalSort(self: *ModuleGraph) !void {
        var order = std.ArrayList(ModuleIndex).empty;
        errdefer order.deinit(self.allocator);

        // Reset all states
        for (self.module_list.items) |*m| {
            m.state = .unvisited;
        }

        // DFS from each unvisited module
        for (0..self.module_list.items.len) |i| {
            if (self.module_list.items[i].state == .unvisited) {
                try self.dfsVisit(@intCast(i), &order);
            }
        }

        self.execution_order = try order.toOwnedSlice(self.allocator);
    }

    fn dfsVisit(self: *ModuleGraph, idx: ModuleIndex, order: *std.ArrayList(ModuleIndex)) !void {
        var module = &self.module_list.items[idx];

        if (module.state == .visiting) return error.CircularImport;
        if (module.state == .visited) return;

        module.state = .visiting;

        for (module.dependencies) |dep_idx| {
            try self.dfsVisit(dep_idx, order);
        }

        module.state = .visited;
        try order.append(self.allocator, idx);
    }
};

// ============================================================================
// Errors
// ============================================================================

pub const Error = error{
    CircularImport,
    ImportFileNotFound,
    ImportNestingTooDeep,
    OutOfMemory,
};

// ============================================================================
// Tests
// ============================================================================

fn testReadFile(allocator: std.mem.Allocator, path: []const u8) ReadFileError![]const u8 {
    // Mock: return allocated source based on path (caller owns result)
    const source: []const u8 = if (std.mem.endsWith(u8, path, "utils.ts"))
        "export function greet(name) { return \"Hello \" + name; }"
    else if (std.mem.endsWith(u8, path, "helpers.ts"))
        "export function add(a, b) { return a + b; }"
    else if (std.mem.endsWith(u8, path, "circular_a.ts"))
        "import { b } from \"./circular_b.ts\"; export const a = 1;"
    else if (std.mem.endsWith(u8, path, "circular_b.ts"))
        "import { a } from \"./circular_a.ts\"; export const b = 2;"
    else
        return error.FileNotFound;

    return allocator.dupe(u8, source) catch return error.OutOfMemory;
}

test "module graph: no imports" {
    const allocator = std.testing.allocator;
    var graph = ModuleGraph.init(allocator);
    defer graph.deinit();

    const source = "export function handler(req) { return Response.json({ok: true}); }";
    try graph.build("/app/handler.ts", source, testReadFile);

    try std.testing.expectEqual(@as(usize, 1), graph.module_list.items.len);
    try std.testing.expectEqual(@as(usize, 0), graph.dependencyCount());
}

test "module graph: single file import" {
    const allocator = std.testing.allocator;
    var graph = ModuleGraph.init(allocator);
    defer graph.deinit();

    const source = "import { greet } from \"./utils.ts\"; export function handler(req) { return Response.text(greet(\"world\")); }";
    try graph.build("/app/handler.ts", source, testReadFile);

    try std.testing.expectEqual(@as(usize, 2), graph.module_list.items.len);
    try std.testing.expectEqual(@as(usize, 1), graph.dependencyCount());
    // utils.ts should come before handler.ts in execution order
    try std.testing.expectEqual(@as(usize, 2), graph.execution_order.len);
}

test "module graph: fan-out imports" {
    const allocator = std.testing.allocator;
    var graph = ModuleGraph.init(allocator);
    defer graph.deinit();

    const source = "import { greet } from \"./utils.ts\"; import { add } from \"./helpers.ts\"; export function handler(req) { return Response.json({ok: true}); }";
    try graph.build("/app/handler.ts", source, testReadFile);

    try std.testing.expectEqual(@as(usize, 3), graph.module_list.items.len);
    try std.testing.expectEqual(@as(usize, 2), graph.dependencyCount());
}

test "module graph: circular import detected" {
    const allocator = std.testing.allocator;
    var graph = ModuleGraph.init(allocator);
    defer graph.deinit();

    try std.testing.expectError(
        error.CircularImport,
        graph.build("/app/circular_a.ts", "import { b } from \"./circular_b.ts\"; export const a = 1;", testReadFile),
    );
}
