const context = @import("../../context.zig");
const module_binding = @import("../../module_binding.zig");
const adapter = @import("../../module_binding_adapter.zig");
const modules = @import("zigttp-modules");
const sql_module = modules.data.sql;

pub const binding = adapter.adaptModuleBinding(sql_module.binding);
pub const exports = binding.toModuleExports();

pub const MODULE_STATE_SLOT: usize = sql_module.MODULE_STATE_SLOT;

/// Install the SqlStore into the runtime context. Called once during
/// runtime bootstrap, outside any module invocation, so the SDK's
/// handle-gated setModuleState isn't reachable. We bypass the handle but
/// preserve the SDK envelope layout so the module's own `getModuleState`
/// (which expects an `SdkStateEnvelope`) resolves the pointer correctly.
pub fn installStore(ctx: *context.Context, db_path: ?[]const u8) !void {
    if (module_binding.getSdkModuleStatePtr(ctx, MODULE_STATE_SLOT)) |existing_ptr| {
        const existing: *sql_module.SqlStore = @ptrCast(@alignCast(existing_ptr));
        try existing.configure(db_path);
        return;
    }
    const store = try ctx.allocator.create(sql_module.SqlStore);
    errdefer ctx.allocator.destroy(store);
    store.* = try sql_module.SqlStore.init(ctx.allocator, db_path);
    try module_binding.installSdkModuleState(
        ctx,
        MODULE_STATE_SLOT,
        @ptrCast(store),
        sql_module.SqlStore.sdkDeinit,
    );
}
