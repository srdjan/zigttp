//! Virtual Module System
//!
//! Compile-time module resolution for zigttp:* virtual modules
//! and relative file imports. Virtual modules map to native Zig
//! implementations with zero JS interpretation overhead.
//! File imports use bytecode concatenation: dependencies are compiled
//! and executed first, exporting globals that the entry file reads.

pub const resolver = @import("resolver.zig");
pub const env = @import("env.zig");
pub const crypto = @import("crypto.zig");
pub const router = @import("router.zig");
pub const auth = @import("auth.zig");
pub const validate = @import("validate.zig");
pub const decode = @import("decode.zig");
pub const cache = @import("cache.zig");
pub const sql = @import("sql.zig");
pub const io = @import("io.zig");
pub const compose = @import("compose.zig");
pub const durable = @import("durable.zig");
pub const util = @import("util.zig");
pub const file_resolver = @import("file_resolver.zig");
pub const module_graph = @import("module_graph.zig");
pub const compiler = @import("compiler.zig");
pub const types = @import("types.zig");

pub const ModuleExport = resolver.ModuleExport;
pub const ResolveResult = resolver.ResolveResult;

pub const ModuleGraph = module_graph.ModuleGraph;
pub const ModuleCompiler = compiler.ModuleCompiler;
pub const CompileResult = compiler.CompileResult;

pub const resolve = resolver.resolve;
pub const registerVirtualModule = resolver.registerVirtualModule;
pub const registerVirtualModuleTraced = resolver.registerVirtualModuleTraced;
pub const registerVirtualModuleReplay = resolver.registerVirtualModuleReplay;
pub const registerVirtualModuleDurable = resolver.registerVirtualModuleDurable;
pub const validateImports = resolver.validateImports;
pub const populateModuleTypes = types.populateModuleTypes;
