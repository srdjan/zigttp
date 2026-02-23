//! Multi-File Module Compiler
//!
//! Compiles all modules in a dependency graph with shared AtomTable and StringTable.
//! Atom consistency is guaranteed because all files share the same tables:
//! `put_global(atom)` in file A matches `get_global(atom)` in file B.

const std = @import("std");
const module_graph = @import("module_graph.zig");
const zts_parser = @import("../parser/root.zig");
const context = @import("../context.zig");
const string = @import("../string.zig");
const bytecode = @import("../bytecode.zig");
const object = @import("../object.zig");

pub const CompiledModule = struct {
    func: bytecode.FunctionBytecode,
    shapes: []const []const object.Atom,
    local_count: u8,
    constants: []const @import("../value.zig").JSValue,
};

pub const ModuleCompiler = struct {
    allocator: std.mem.Allocator,
    atoms: *context.AtomTable,
    strings: *string.StringTable,

    pub fn init(
        allocator: std.mem.Allocator,
        atoms: *context.AtomTable,
        strings: *string.StringTable,
    ) ModuleCompiler {
        return .{
            .allocator = allocator,
            .atoms = atoms,
            .strings = strings,
        };
    }

    /// Compile all modules in the graph's execution order.
    /// Returns a list of CompiledModule structs. The caller must manage
    /// the lifetime of the underlying parser/codegen state through the
    /// returned ParserState handles.
    ///
    /// Each module is compiled independently but shares the same atom table,
    /// so global variable references are consistent across modules.
    pub fn compileAll(
        self: *ModuleCompiler,
        graph: *module_graph.ModuleGraph,
    ) !CompileResult {
        var compiled = std.ArrayList(CompiledModule).empty;
        errdefer compiled.deinit(self.allocator);

        // Keep parser/codegen state alive - bytecode slices borrow from these
        var parsers = std.ArrayList(zts_parser.JsParser).empty;
        errdefer {
            for (parsers.items) |*p| p.deinit();
            parsers.deinit(self.allocator);
        }
        var codegens = std.ArrayList(zts_parser.CodeGen).empty;
        errdefer {
            for (codegens.items) |*cg| cg.deinit();
            codegens.deinit(self.allocator);
        }

        for (graph.execution_order) |mod_idx| {
            const module = &graph.module_list.items[mod_idx];
            const source = module.stripped_source orelse module.source;

            // Parse with shared atom table
            var js_parser = zts_parser.JsParser.init(self.allocator, source);
            js_parser.setAtomTable(self.atoms);

            // Enable JSX if needed
            if (std.mem.endsWith(u8, module.path, ".jsx") or
                std.mem.endsWith(u8, module.path, ".tsx"))
            {
                js_parser.tokenizer.enableJsx();
            }

            const root = js_parser.parse() catch |err| {
                std.log.err("Parse error in module '{s}': {}", .{ module.path, err });
                js_parser.deinit();
                return err;
            };

            // Optimize IR
            _ = zts_parser.ir_opt.optimizeIR(
                self.allocator,
                &js_parser.nodes,
                &js_parser.constants,
                root,
            ) catch {};

            // Generate bytecode
            var code_gen = zts_parser.CodeGen.initWithIRStore(
                self.allocator,
                &js_parser.nodes,
                &js_parser.constants,
                &js_parser.scopes,
                self.strings,
                self.atoms,
            );

            const func = code_gen.generate(root) catch |err| {
                std.log.err("Codegen error in module '{s}': {}", .{ module.path, err });
                code_gen.deinit();
                js_parser.deinit();
                return err;
            };

            const shapes = code_gen.shapes.items;

            try compiled.append(self.allocator, .{
                .func = func,
                .shapes = shapes,
                .local_count = @intCast(func.local_count),
                .constants = func.constants,
            });

            // Keep parser and codegen alive (bytecode borrows from them)
            try parsers.append(self.allocator, js_parser);
            try codegens.append(self.allocator, code_gen);
        }

        return .{
            .modules = try compiled.toOwnedSlice(self.allocator),
            .parsers = try parsers.toOwnedSlice(self.allocator),
            .codegens = try codegens.toOwnedSlice(self.allocator),
            .allocator = self.allocator,
        };
    }
};

/// Result of compiling all modules. Owns the parser/codegen state that
/// bytecode slices borrow from. Must be deinitialized when done.
pub const CompileResult = struct {
    modules: []CompiledModule,
    parsers: []zts_parser.JsParser,
    codegens: []zts_parser.CodeGen,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *CompileResult) void {
        for (self.codegens) |*cg| cg.deinit();
        self.allocator.free(self.codegens);
        for (self.parsers) |*p| p.deinit();
        self.allocator.free(self.parsers);
        self.allocator.free(self.modules);
    }
};
