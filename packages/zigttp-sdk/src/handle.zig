pub const ModuleHandle = opaque {};

pub const RuntimeError = error{
    OutOfMemory,
    RuntimeFailure,
};
