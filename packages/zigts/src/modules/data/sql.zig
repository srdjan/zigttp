const context = @import("../../context.zig");
const adapter = @import("../../module_binding_adapter.zig");
const modules = @import("zigttp-modules");
const sql_module = modules.data.sql;

pub const binding = adapter.adaptModuleBinding(sql_module.binding);
pub const exports = binding.toModuleExports();

pub const MODULE_STATE_SLOT: usize = sql_module.MODULE_STATE_SLOT;

/// Install the SqlStore into the runtime context. Called once during
/// runtime bootstrap, outside any module invocation, so the SDK's
/// handle-gated setModuleState isn't reachable; we write the slot
/// directly via the context API using the same store type the module
/// uses at runtime.
pub fn installStore(ctx: *context.Context, db_path: ?[]const u8) !void {
    if (ctx.getModuleState(sql_module.SqlStore, MODULE_STATE_SLOT)) |existing| {
        try existing.configure(db_path);
        return;
    }
    const store = try ctx.allocator.create(sql_module.SqlStore);
    store.* = try sql_module.SqlStore.init(ctx.allocator, db_path);
    ctx.setModuleState(MODULE_STATE_SLOT, @ptrCast(store), &storeDeinitAdapter);
}

fn storeDeinitAdapter(ptr: *anyopaque, _: @import("std").mem.Allocator) void {
    sql_module.SqlStore.sdkDeinit(ptr);
}
