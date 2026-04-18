/// Centralized registry of module_state slot indices.
/// Context.module_state is a fixed-size [16]?ModuleStateEntry array.
/// Each slot is reserved for a specific subsystem to avoid collisions.
pub const Slot = enum(u4) {
    durable = 0,
    durable_api = 1,
    sql = 2,
    replay = 3,
    validate = 4,
    cache = 5,
    io = 6,
    trace = 7,
    ratelimit = 8,
    service = 9,
    scope = 10,
    fetch = 11,
};
