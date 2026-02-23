//! Virtual Module System
//!
//! Compile-time module resolution for zigttp:* virtual modules
//! and relative file imports. Virtual modules map to native Zig
//! implementations with zero JS interpretation overhead.

pub const resolver = @import("resolver.zig");
pub const env = @import("env.zig");
pub const crypto = @import("crypto.zig");
pub const router = @import("router.zig");

pub const VirtualModule = resolver.VirtualModule;
pub const ModuleExport = resolver.ModuleExport;
pub const ResolveResult = resolver.ResolveResult;

pub const resolve = resolver.resolve;
pub const registerVirtualModule = resolver.registerVirtualModule;
pub const validateImports = resolver.validateImports;
