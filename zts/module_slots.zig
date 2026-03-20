/// Centralized registry of module_state slot indices.
/// Context.module_state is a fixed-size [8]?*anyopaque array.
/// Each slot is reserved for a specific subsystem to avoid collisions.
pub const Slot = enum(u3) {
    durable = 0,
    durable_api = 1,
    // 2 unused
    replay = 3,
    validate = 4,
    cache = 5,
    io = 6,
    trace = 7,
};
