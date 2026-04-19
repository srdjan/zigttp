const adapter = @import("../../module_binding_adapter.zig");
const modules = @import("zigttp-modules");

pub const binding = adapter.adaptModuleBinding(modules.platform.time.binding);
pub const exports = binding.toModuleExports();
