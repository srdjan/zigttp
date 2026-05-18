//! Frame-snapshot record held in the interpreter's state stack.
//! Pushed when entering a nested call, popped on return.

const value = @import("../value.zig");
const bytecode = @import("../bytecode.zig");

pub const MAX_STATE_DEPTH: usize = 1024;

pub const SavedState = struct {
    pc: [*]const u8,
    code_end: [*]const u8,
    constants: []const value.JSValue,
    current_func: ?*const bytecode.FunctionBytecode,
    sp: usize,
    fp: usize,
    call_depth: usize,
    catch_depth: usize,
    exception: value.JSValue,
};
