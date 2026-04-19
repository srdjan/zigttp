//! Virtual Module System
//!
//! Compile-time module resolution for zigttp:* virtual modules
//! and relative file imports. Virtual modules map to native Zig
//! implementations with zero JS interpretation overhead.
//! File imports use bytecode concatenation: dependencies are compiled
//! and executed first, exporting globals that the entry file reads.
//!
//! Most module implementations live in the peer `zigttp-modules`
//! package and are registered via `builtin_modules.zig`. The files
//! here are the ones that still need zigts-internal access (runtime
//! callback installState shims, or modules tightly coupled to the
//! runtime): sql, service, fetch, websocket, io, scope, durable.

pub const resolver = @import("internal/resolver.zig");
pub const util = @import("internal/util.zig");
pub const file_resolver = @import("internal/file_resolver.zig");
pub const module_graph = @import("internal/module_graph.zig");
pub const compiler = @import("internal/compiler.zig");
pub const types = @import("internal/types.zig");

pub const sql = @import("data/sql.zig");

pub const io = @import("workflow/io.zig");
pub const scope = @import("workflow/scope.zig");
pub const durable = @import("workflow/durable.zig");

pub const service = @import("net/service.zig");
pub const fetch = @import("net/fetch.zig");
pub const websocket = @import("net/websocket.zig");

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
