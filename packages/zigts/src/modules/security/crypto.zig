//! zigttp:crypto - adapter stub
//!
//! The canonical implementation lives in packages/modules/src/security/crypto.zig.
//! This file exists so builtin_modules.zig's import path stays stable during
//! the peer-package migration (see docs/virtual-modules-peer-package.md).
//! Phase 8 deletes this stub and updates the import.

const adapter = @import("../../module_binding_adapter.zig");
const modules = @import("zigttp-modules");

pub const binding = adapter.adaptModuleBinding(modules.security.crypto.binding);
pub const exports = binding.toModuleExports();
