const adapter = @import("module_binding_adapter.zig");
const ext_demo = @import("zigttp-ext-demo");

pub const all = [_]adapter.ModuleBinding{
    adapter.adaptModuleBinding(ext_demo.binding),
};
