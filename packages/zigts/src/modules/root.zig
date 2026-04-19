//! Virtual Module System
//!
//! Compile-time module resolution for zigttp:* virtual modules
//! and relative file imports. Virtual modules map to native Zig
//! implementations with zero JS interpretation overhead.
//! File imports use bytecode concatenation: dependencies are compiled
//! and executed first, exporting globals that the entry file reads.

pub const resolver = @import("resolver.zig");
pub const util = @import("util.zig");
pub const file_resolver = @import("file_resolver.zig");
pub const module_graph = @import("module_graph.zig");
pub const compiler = @import("compiler.zig");
pub const types = @import("types.zig");

pub const env = @import("platform/env.zig");
pub const id = @import("platform/id.zig");
pub const log = @import("platform/log.zig");
pub const text = @import("platform/text.zig");
pub const time = @import("platform/time.zig");

pub const crypto = @import("security/crypto.zig");
pub const auth = @import("security/auth.zig");
pub const validate = @import("security/validate.zig");
pub const decode = @import("security/decode.zig");

pub const cache = @import("data/cache.zig");
pub const sql = @import("data/sql.zig");
pub const ratelimit = @import("data/ratelimit.zig");

pub const router = @import("http/router.zig");
pub const http = @import("http/http_mod.zig");
pub const url = @import("http/url.zig");

pub const io = @import("workflow/io.zig");
pub const scope = @import("workflow/scope.zig");
pub const compose = @import("workflow/compose.zig");
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
