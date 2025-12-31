//! Bytecode definitions with superinstructions
//!
//! Variable-size instruction encoding with versioned format.

const std = @import("std");
const value = @import("value.zig");

/// Bytecode file magic number ("ZQJS")
pub const MAGIC: u32 = 0x5A514A53;

/// Current bytecode version
pub const VERSION_MAJOR: u8 = 1;
pub const VERSION_MINOR: u8 = 0;

/// Bytecode header
pub const BytecodeHeader = packed struct {
    magic: u32 = MAGIC,
    version_major: u8 = VERSION_MAJOR,
    version_minor: u8 = VERSION_MINOR,
    flags: BytecodeFlags = .{},
    checksum: u32 = 0,
};

/// Bytecode flags
pub const BytecodeFlags = packed struct {
    has_source_map: bool = false,
    optimized: bool = false,
    jit_hints: bool = false,
    strict_mode: bool = true,
    _reserved: u4 = 0,
};

/// Opcode definitions
pub const Opcode = enum(u8) {
    // Stack operations
    nop = 0x00,
    push_const = 0x01, // +u16 const_idx
    push_0 = 0x02,
    push_1 = 0x03,
    push_2 = 0x04,
    push_3 = 0x05,
    push_i8 = 0x06, // +i8 value
    push_i16 = 0x07, // +i16 value
    push_null = 0x08,
    push_undefined = 0x09,
    push_true = 0x0A,
    push_false = 0x0B,
    dup = 0x0C,
    drop = 0x0D,
    swap = 0x0E,
    rot3 = 0x0F,

    // Extended stack operations
    halt = 0x1A,
    loop = 0x1B, // +i16 offset (loop back)

    // Local variables
    get_loc = 0x10, // +u8 local_idx
    put_loc = 0x11, // +u8 local_idx
    get_loc_0 = 0x12,
    get_loc_1 = 0x13,
    get_loc_2 = 0x14,
    get_loc_3 = 0x15,
    put_loc_0 = 0x16,
    put_loc_1 = 0x17,
    put_loc_2 = 0x18,
    put_loc_3 = 0x19,

    // Arithmetic
    add = 0x20,
    sub = 0x21,
    mul = 0x22,
    div = 0x23,
    mod = 0x24,
    pow = 0x25,
    neg = 0x26,
    inc = 0x27,
    dec = 0x28,

    // Bitwise
    bit_and = 0x30,
    bit_or = 0x31,
    bit_xor = 0x32,
    bit_not = 0x33,
    shl = 0x34,
    shr = 0x35,
    ushr = 0x36,

    // Comparison
    lt = 0x40,
    lte = 0x41,
    gt = 0x42,
    gte = 0x43,
    eq = 0x44,
    neq = 0x45,
    strict_eq = 0x46,
    strict_neq = 0x47,

    // Logical
    not = 0x48,

    // Control flow
    goto = 0x50, // +i16 offset
    if_true = 0x51, // +i16 offset
    if_false = 0x52, // +i16 offset
    ret = 0x53,
    ret_undefined = 0x54,

    // Function calls
    call = 0x60, // +u8 argc
    call_method = 0x61, // +u8 argc
    tail_call = 0x63, // +u8 argc

    // Property access
    get_field = 0x70, // +u16 atom_idx
    put_field = 0x71, // +u16 atom_idx
    get_elem = 0x72,
    put_elem = 0x73,
    delete_field = 0x74, // +u16 atom_idx
    delete_elem = 0x75,
    put_field_keep = 0x76, // +u16 atom_idx - set property and keep value on stack

    // Object operations
    new_object = 0x80,
    new_array = 0x81, // +u16 length
    get_global = 0x83, // +u16 atom_idx
    put_global = 0x84, // +u16 atom_idx
    define_global = 0x85, // +u16 atom_idx (declare global var)
    make_function = 0x86, // +u16 const_idx (creates function from bytecode in constant pool)
    array_spread = 0x87, // Spread source array into target array (pops source, target on stack)
    call_spread = 0x88, // Call function with spread args (special handling)

    // Type checks
    typeof = 0x90,
    instanceof = 0x91,

    // Async operations (kept for future implementation)
    await_val = 0x98, // Await a value/Promise, suspends execution
    make_async = 0x99, // Create async function from bytecode

    // Module operations
    import_module = 0x9A, // +u16 module_name_idx (load module, push namespace)
    import_name = 0x9B, // +u16 name_idx (get named export from module namespace)
    import_default = 0x9C, // Get default export from module namespace
    export_name = 0x9D, // +u16 name_idx (export a binding)
    export_default = 0x9E, // Export default value

    // Superinstructions (fused hot paths)
    get_loc_add = 0xA0, // get_loc + add
    get_loc_get_loc_add = 0xA1, // get_loc + get_loc + add
    push_const_call = 0xA2, // push_const + call
    get_field_call = 0xA3, // get_field + call
    if_false_goto = 0xA4, // if_false + goto

    // Inline cache instructions
    get_field_ic = 0xB0, // +u16 atom_idx +u16 cache_idx
    put_field_ic = 0xB1, // +u16 atom_idx +u16 cache_idx
    call_ic = 0xB2, // +u8 argc +u16 cache_idx

    // Closure operations
    get_upvalue = 0xC0, // +u8 upvalue_idx
    put_upvalue = 0xC1, // +u8 upvalue_idx
    close_upvalue = 0xC2, // +u8 local_idx (close when leaving scope)
    make_closure = 0xC3, // +u16 func_idx +u8 upvalue_count

    // Reserved for future
    _,
};

/// Instruction format metadata
pub const OpcodeInfo = struct {
    size: u8, // Total instruction size in bytes
    n_pop: u8, // Stack values consumed
    n_push: u8, // Stack values produced
    name: []const u8,
};

/// Get opcode metadata
pub fn getOpcodeInfo(op: Opcode) OpcodeInfo {
    return switch (op) {
        .nop => .{ .size = 1, .n_pop = 0, .n_push = 0, .name = "nop" },
        .push_const => .{ .size = 3, .n_pop = 0, .n_push = 1, .name = "push_const" },
        .push_0 => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "push_0" },
        .push_1 => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "push_1" },
        .push_i8 => .{ .size = 2, .n_pop = 0, .n_push = 1, .name = "push_i8" },
        .add => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "add" },
        .sub => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "sub" },
        .mul => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "mul" },
        .div => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "div" },
        .get_loc => .{ .size = 2, .n_pop = 0, .n_push = 1, .name = "get_loc" },
        .put_loc => .{ .size = 2, .n_pop = 1, .n_push = 0, .name = "put_loc" },
        .goto => .{ .size = 3, .n_pop = 0, .n_push = 0, .name = "goto" },
        .if_true => .{ .size = 3, .n_pop = 1, .n_push = 0, .name = "if_true" },
        .if_false => .{ .size = 3, .n_pop = 1, .n_push = 0, .name = "if_false" },
        .call => .{ .size = 2, .n_pop = 0, .n_push = 1, .name = "call" }, // Dynamic pop
        .ret => .{ .size = 1, .n_pop = 1, .n_push = 0, .name = "ret" },
        .get_field => .{ .size = 3, .n_pop = 1, .n_push = 1, .name = "get_field" },
        .put_field => .{ .size = 3, .n_pop = 2, .n_push = 0, .name = "put_field" },
        .put_field_keep => .{ .size = 3, .n_pop = 2, .n_push = 1, .name = "put_field_keep" },
        else => .{ .size = 1, .n_pop = 0, .n_push = 0, .name = "unknown" },
    };
}

/// Constant pool entry types
pub const ConstType = enum(u8) {
    int32 = 0,
    float64 = 1,
    string = 2,
    atom = 3,
    function = 4,
    regexp = 5,
};

/// Upvalue info for closures
pub const UpvalueInfo = struct {
    is_local: bool, // true: from parent's locals, false: from parent's upvalues
    index: u8, // Index in parent's locals or upvalues array
};

/// Function bytecode structure
pub const FunctionBytecode = struct {
    header: BytecodeHeader,
    name_atom: u32,
    arg_count: u16,
    local_count: u16,
    stack_size: u16,
    flags: FunctionFlags,
    upvalue_count: u8 = 0, // Number of upvalues this function captures
    upvalue_info: []const UpvalueInfo = &.{}, // Info for each upvalue
    code: []const u8,
    constants: []const value.JSValue,
    source_map: ?[]const u8,
};

pub const FunctionFlags = packed struct {
    is_strict: bool = true,
    is_generator: bool = false,
    is_async: bool = false,
    has_arguments: bool = false,
    has_rest: bool = false,
    _reserved: u3 = 0,
};

test "Opcode encoding" {
    try std.testing.expectEqual(@as(u8, 0x20), @intFromEnum(Opcode.add));
    try std.testing.expectEqual(@as(u8, 0x50), @intFromEnum(Opcode.goto));
}

test "OpcodeInfo lookup" {
    const add_info = getOpcodeInfo(.add);
    try std.testing.expectEqual(@as(u8, 1), add_info.size);
    try std.testing.expectEqual(@as(u8, 2), add_info.n_pop);
    try std.testing.expectEqual(@as(u8, 1), add_info.n_push);
}

test "BytecodeHeader" {
    const header = BytecodeHeader{};
    try std.testing.expectEqual(MAGIC, header.magic);
    try std.testing.expectEqual(VERSION_MAJOR, header.version_major);
}
