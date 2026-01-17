//! Bytecode definitions with superinstructions
//!
//! Variable-size instruction encoding with versioned format.

const std = @import("std");
const value = @import("value.zig");
const type_feedback = @import("type_feedback.zig");

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
    get_length = 0x1C, // Get length of array/string/range (fast path, no atom lookup)
    dup2 = 0x1D, // Duplicate top 2 stack values

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
    new_object_literal = 0x82, // +u16 shape_idx +u8 prop_count (creates object with pre-compiled shape)
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

    // Fused arithmetic-modulo (for patterns like (a + b) % c)
    // Uses i64 intermediate to avoid overflow, result guaranteed to fit in i32
    add_mod = 0xA5, // +u16 divisor_const_idx: pop a,b; push (a+b) % divisor
    sub_mod = 0xA6, // +u16 divisor_const_idx: pop a,b; push (a-b) % divisor
    mul_mod = 0xA7, // +u16 divisor_const_idx: pop a,b; push (a*b) % divisor

    // Loop optimization superinstructions
    for_of_next = 0xA8, // +i16 end_offset: check bounds, push element, increment index
    for_of_next_put_loc = 0xAC, // +u8 local_idx +i16 end_offset: for_of_next + store to local

    // Specialized constant opcodes
    shr_1 = 0xA9, // Shift right by 1 (common pattern x >> 1)
    mul_2 = 0xAA, // Multiply by 2 (common pattern x * 2)
    mod_const = 0xAB, // +u16 divisor_const_idx: pop a; push a % divisor

    // Inline i8 constant opcodes (no constant pool lookup)
    mod_const_i8 = 0xAD, // +i8 divisor: pop a; push a % divisor
    add_const_i8 = 0xAE, // +i8 value: pop a; push a + value
    sub_const_i8 = 0xAF, // +i8 value: pop a; push a - value

    // Inline cache instructions
    get_field_ic = 0xB0, // +u16 atom_idx +u16 cache_idx
    put_field_ic = 0xB1, // +u16 atom_idx +u16 cache_idx
    call_ic = 0xB2, // +u8 argc +u16 cache_idx

    // More inline i8 constant opcodes
    mul_const_i8 = 0xB3, // +i8 value: pop a; push a * value
    lt_const_i8 = 0xB4, // +i8 value: pop a; push a < value
    le_const_i8 = 0xB5, // +i8 value: pop a; push a <= value

    // Closure operations
    get_upvalue = 0xC0, // +u8 upvalue_idx
    put_upvalue = 0xC1, // +u8 upvalue_idx
    close_upvalue = 0xC2, // +u8 local_idx (close when leaving scope)
    make_closure = 0xC3, // +u16 func_idx +u8 upvalue_count

    // Object literal optimization
    set_slot = 0xC4, // +u8 slot_idx (direct slot write for pre-compiled object literals)

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

/// Get opcode metadata for debugging, disassembly, and validation
pub fn getOpcodeInfo(op: Opcode) OpcodeInfo {
    return switch (op) {
        // Stack operations
        .nop => .{ .size = 1, .n_pop = 0, .n_push = 0, .name = "nop" },
        .push_const => .{ .size = 3, .n_pop = 0, .n_push = 1, .name = "push_const" },
        .push_0 => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "push_0" },
        .push_1 => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "push_1" },
        .push_2 => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "push_2" },
        .push_3 => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "push_3" },
        .push_i8 => .{ .size = 2, .n_pop = 0, .n_push = 1, .name = "push_i8" },
        .push_i16 => .{ .size = 3, .n_pop = 0, .n_push = 1, .name = "push_i16" },
        .push_null => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "push_null" },
        .push_undefined => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "push_undefined" },
        .push_true => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "push_true" },
        .push_false => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "push_false" },
        .dup => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "dup" },
        .drop => .{ .size = 1, .n_pop = 1, .n_push = 0, .name = "drop" },
        .swap => .{ .size = 1, .n_pop = 2, .n_push = 2, .name = "swap" },
        .rot3 => .{ .size = 1, .n_pop = 3, .n_push = 3, .name = "rot3" },

        // Extended stack operations
        .halt => .{ .size = 1, .n_pop = 0, .n_push = 0, .name = "halt" },
        .loop => .{ .size = 3, .n_pop = 0, .n_push = 0, .name = "loop" },
        .get_length => .{ .size = 1, .n_pop = 1, .n_push = 1, .name = "get_length" },
        .dup2 => .{ .size = 1, .n_pop = 0, .n_push = 2, .name = "dup2" },

        // Local variables
        .get_loc => .{ .size = 2, .n_pop = 0, .n_push = 1, .name = "get_loc" },
        .put_loc => .{ .size = 2, .n_pop = 1, .n_push = 0, .name = "put_loc" },
        .get_loc_0 => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "get_loc_0" },
        .get_loc_1 => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "get_loc_1" },
        .get_loc_2 => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "get_loc_2" },
        .get_loc_3 => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "get_loc_3" },
        .put_loc_0 => .{ .size = 1, .n_pop = 1, .n_push = 0, .name = "put_loc_0" },
        .put_loc_1 => .{ .size = 1, .n_pop = 1, .n_push = 0, .name = "put_loc_1" },
        .put_loc_2 => .{ .size = 1, .n_pop = 1, .n_push = 0, .name = "put_loc_2" },
        .put_loc_3 => .{ .size = 1, .n_pop = 1, .n_push = 0, .name = "put_loc_3" },

        // Arithmetic
        .add => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "add" },
        .sub => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "sub" },
        .mul => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "mul" },
        .div => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "div" },
        .mod => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "mod" },
        .pow => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "pow" },
        .neg => .{ .size = 1, .n_pop = 1, .n_push = 1, .name = "neg" },
        .inc => .{ .size = 1, .n_pop = 1, .n_push = 1, .name = "inc" },
        .dec => .{ .size = 1, .n_pop = 1, .n_push = 1, .name = "dec" },

        // Bitwise
        .bit_and => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "bit_and" },
        .bit_or => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "bit_or" },
        .bit_xor => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "bit_xor" },
        .bit_not => .{ .size = 1, .n_pop = 1, .n_push = 1, .name = "bit_not" },
        .shl => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "shl" },
        .shr => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "shr" },
        .ushr => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "ushr" },

        // Comparison
        .lt => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "lt" },
        .lte => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "lte" },
        .gt => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "gt" },
        .gte => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "gte" },
        .eq => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "eq" },
        .neq => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "neq" },
        .strict_eq => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "strict_eq" },
        .strict_neq => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "strict_neq" },

        // Logical
        .not => .{ .size = 1, .n_pop = 1, .n_push = 1, .name = "not" },

        // Control flow
        .goto => .{ .size = 3, .n_pop = 0, .n_push = 0, .name = "goto" },
        .if_true => .{ .size = 3, .n_pop = 1, .n_push = 0, .name = "if_true" },
        .if_false => .{ .size = 3, .n_pop = 1, .n_push = 0, .name = "if_false" },
        .ret => .{ .size = 1, .n_pop = 1, .n_push = 0, .name = "ret" },
        .ret_undefined => .{ .size = 1, .n_pop = 0, .n_push = 0, .name = "ret_undefined" },

        // Function calls (n_pop is dynamic based on argc)
        .call => .{ .size = 2, .n_pop = 0, .n_push = 1, .name = "call" },
        .call_method => .{ .size = 2, .n_pop = 0, .n_push = 1, .name = "call_method" },
        .tail_call => .{ .size = 2, .n_pop = 0, .n_push = 0, .name = "tail_call" },

        // Property access
        .get_field => .{ .size = 3, .n_pop = 1, .n_push = 1, .name = "get_field" },
        .put_field => .{ .size = 3, .n_pop = 2, .n_push = 0, .name = "put_field" },
        .get_elem => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "get_elem" },
        .put_elem => .{ .size = 1, .n_pop = 3, .n_push = 0, .name = "put_elem" },
        .delete_field => .{ .size = 3, .n_pop = 1, .n_push = 1, .name = "delete_field" },
        .delete_elem => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "delete_elem" },
        .put_field_keep => .{ .size = 3, .n_pop = 2, .n_push = 1, .name = "put_field_keep" },

        // Object operations
        .new_object => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "new_object" },
        .new_array => .{ .size = 3, .n_pop = 0, .n_push = 1, .name = "new_array" },
        .new_object_literal => .{ .size = 4, .n_pop = 0, .n_push = 1, .name = "new_object_literal" },
        .get_global => .{ .size = 3, .n_pop = 0, .n_push = 1, .name = "get_global" },
        .put_global => .{ .size = 3, .n_pop = 1, .n_push = 0, .name = "put_global" },
        .define_global => .{ .size = 3, .n_pop = 1, .n_push = 0, .name = "define_global" },
        .make_function => .{ .size = 3, .n_pop = 0, .n_push = 1, .name = "make_function" },
        .array_spread => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "array_spread" },
        .call_spread => .{ .size = 1, .n_pop = 0, .n_push = 1, .name = "call_spread" },

        // Type checks
        .typeof => .{ .size = 1, .n_pop = 1, .n_push = 1, .name = "typeof" },
        .instanceof => .{ .size = 1, .n_pop = 2, .n_push = 1, .name = "instanceof" },

        // Async operations
        .await_val => .{ .size = 1, .n_pop = 1, .n_push = 1, .name = "await_val" },
        .make_async => .{ .size = 3, .n_pop = 0, .n_push = 1, .name = "make_async" },

        // Module operations
        .import_module => .{ .size = 3, .n_pop = 0, .n_push = 1, .name = "import_module" },
        .import_name => .{ .size = 3, .n_pop = 1, .n_push = 1, .name = "import_name" },
        .import_default => .{ .size = 1, .n_pop = 1, .n_push = 1, .name = "import_default" },
        .export_name => .{ .size = 3, .n_pop = 1, .n_push = 0, .name = "export_name" },
        .export_default => .{ .size = 1, .n_pop = 1, .n_push = 0, .name = "export_default" },

        // Superinstructions
        .get_loc_add => .{ .size = 2, .n_pop = 1, .n_push = 1, .name = "get_loc_add" },
        .get_loc_get_loc_add => .{ .size = 3, .n_pop = 0, .n_push = 1, .name = "get_loc_get_loc_add" },
        .push_const_call => .{ .size = 4, .n_pop = 0, .n_push = 1, .name = "push_const_call" },
        .get_field_call => .{ .size = 4, .n_pop = 1, .n_push = 1, .name = "get_field_call" },
        .if_false_goto => .{ .size = 3, .n_pop = 1, .n_push = 0, .name = "if_false_goto" },

        // Fused arithmetic-modulo
        .add_mod => .{ .size = 3, .n_pop = 2, .n_push = 1, .name = "add_mod" },
        .sub_mod => .{ .size = 3, .n_pop = 2, .n_push = 1, .name = "sub_mod" },
        .mul_mod => .{ .size = 3, .n_pop = 2, .n_push = 1, .name = "mul_mod" },

        // Loop optimization superinstructions
        .for_of_next => .{ .size = 3, .n_pop = 0, .n_push = 1, .name = "for_of_next" },
        .for_of_next_put_loc => .{ .size = 4, .n_pop = 0, .n_push = 0, .name = "for_of_next_put_loc" },

        // Specialized constant opcodes
        .shr_1 => .{ .size = 1, .n_pop = 1, .n_push = 1, .name = "shr_1" },
        .mul_2 => .{ .size = 1, .n_pop = 1, .n_push = 1, .name = "mul_2" },
        .mod_const => .{ .size = 3, .n_pop = 1, .n_push = 1, .name = "mod_const" },

        // Inline i8 constant opcodes
        .mod_const_i8 => .{ .size = 2, .n_pop = 1, .n_push = 1, .name = "mod_const_i8" },
        .add_const_i8 => .{ .size = 2, .n_pop = 1, .n_push = 1, .name = "add_const_i8" },
        .sub_const_i8 => .{ .size = 2, .n_pop = 1, .n_push = 1, .name = "sub_const_i8" },

        // Inline cache instructions
        .get_field_ic => .{ .size = 5, .n_pop = 1, .n_push = 1, .name = "get_field_ic" },
        .put_field_ic => .{ .size = 5, .n_pop = 2, .n_push = 0, .name = "put_field_ic" },
        .call_ic => .{ .size = 4, .n_pop = 0, .n_push = 1, .name = "call_ic" },

        // More inline i8 constant opcodes
        .mul_const_i8 => .{ .size = 2, .n_pop = 1, .n_push = 1, .name = "mul_const_i8" },
        .lt_const_i8 => .{ .size = 2, .n_pop = 1, .n_push = 1, .name = "lt_const_i8" },
        .le_const_i8 => .{ .size = 2, .n_pop = 1, .n_push = 1, .name = "le_const_i8" },

        // Closure operations
        .get_upvalue => .{ .size = 2, .n_pop = 0, .n_push = 1, .name = "get_upvalue" },
        .put_upvalue => .{ .size = 2, .n_pop = 1, .n_push = 0, .name = "put_upvalue" },
        .close_upvalue => .{ .size = 2, .n_pop = 0, .n_push = 0, .name = "close_upvalue" },
        .make_closure => .{ .size = 4, .n_pop = 0, .n_push = 1, .name = "make_closure" },

        // Object literal optimization
        .set_slot => .{ .size = 2, .n_pop = 2, .n_push = 0, .name = "set_slot" },

        // Unknown/reserved opcodes
        _ => .{ .size = 1, .n_pop = 0, .n_push = 0, .name = "unknown" },
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

/// Compilation tier for JIT profiling (Phase 11)
pub const CompilationTier = enum(u8) {
    interpreted,        // Running in bytecode interpreter
    baseline_candidate, // Hit threshold, queued for compilation
    baseline,           // Simple native code (dispatch elimination)
    optimized,          // With type specialization
};

/// Call count threshold before a function becomes a JIT candidate
pub const JIT_THRESHOLD: u32 = 100;

/// Back-edge threshold for detecting hot loops
pub const LOOP_THRESHOLD: u32 = 1000;

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

    // JIT profiling fields (Phase 11)
    execution_count: u32 = 0, // Incremented on each call
    tier: CompilationTier = .interpreted,
    compiled_code: ?*anyopaque = null, // Pointer to CompiledCode when JIT'd

    // Type feedback for speculative optimization (Phase 12)
    type_feedback_ptr: ?*type_feedback.TypeFeedback = null,
    feedback_site_map: ?[]u16 = null, // bytecode_offset -> site_index

    /// Get the type feedback vector if allocated
    pub fn getTypeFeedback(self: *const FunctionBytecode) ?*type_feedback.TypeFeedback {
        return self.type_feedback_ptr;
    }

    /// Set the type feedback vector
    pub fn setTypeFeedback(self: *FunctionBytecode, tf: *type_feedback.TypeFeedback) void {
        self.type_feedback_ptr = tf;
    }
};

pub const FunctionFlags = packed struct {
    is_strict: bool = true,
    is_generator: bool = false,
    is_async: bool = false,
    has_arguments: bool = false,
    has_rest: bool = false,
    _reserved: u3 = 0,
};

// ============================================================================
// Phase 4: Compact Bytecode with Extra Data Pattern
// ============================================================================

/// Compact function bytecode with single allocation
/// All variable-length data is stored inline after the header
/// Memory layout: [Header | code bytes | constant indices | upvalue info]
pub const FunctionBytecodeCompact = struct {
    /// Fixed header (always present)
    header: BytecodeHeader,
    name_atom: u32,
    arg_count: u16,
    local_count: u16,
    stack_size: u16,
    flags: FunctionFlags,
    upvalue_count: u8,
    _pad: u8 = 0,

    /// Inline data sizes (used to calculate offsets)
    code_len: u32,
    const_count: u16,
    _pad2: u16 = 0,

    // Variable-length data follows in memory:
    // - code: [code_len]u8
    // - constants: [const_count]u32 (indices into InternPool or raw values)
    // - upvalue_info: [upvalue_count]UpvalueInfo

    pub const HEADER_SIZE = @sizeOf(FunctionBytecodeCompact);

    /// Calculate total size needed for allocation
    pub fn calcSize(code_len: u32, const_count: u16, upvalue_count: u8) usize {
        var size: usize = HEADER_SIZE;
        size += code_len; // code bytes
        size = std.mem.alignForward(usize, size, @alignOf(u32)); // align for constants
        size += @as(usize, const_count) * @sizeOf(u32);
        size = std.mem.alignForward(usize, size, @alignOf(UpvalueInfo)); // align for upvalue info
        size += @as(usize, upvalue_count) * @sizeOf(UpvalueInfo);
        return size;
    }

    /// Get code bytes slice
    pub fn getCode(self: *const FunctionBytecodeCompact) []const u8 {
        const base = @as([*]const u8, @ptrCast(self));
        return base[HEADER_SIZE..][0..self.code_len];
    }

    /// Get constants as u32 indices
    pub fn getConstants(self: *const FunctionBytecodeCompact) []const u32 {
        const base = @as([*]const u8, @ptrCast(self));
        const code_end = HEADER_SIZE + self.code_len;
        const const_start = std.mem.alignForward(usize, code_end, @alignOf(u32));
        const const_ptr: [*]const u32 = @ptrCast(@alignCast(base + const_start));
        return const_ptr[0..self.const_count];
    }

    /// Get upvalue info slice
    pub fn getUpvalues(self: *const FunctionBytecodeCompact) []const UpvalueInfo {
        const base = @as([*]const u8, @ptrCast(self));
        const code_end = HEADER_SIZE + self.code_len;
        const const_start = std.mem.alignForward(usize, code_end, @alignOf(u32));
        const const_end = const_start + @as(usize, self.const_count) * @sizeOf(u32);
        const upval_start = std.mem.alignForward(usize, const_end, @alignOf(UpvalueInfo));
        const upval_ptr: [*]const UpvalueInfo = @ptrCast(@alignCast(base + upval_start));
        return upval_ptr[0..self.upvalue_count];
    }

    /// Create a compact bytecode from components
    pub fn create(
        allocator: std.mem.Allocator,
        header: BytecodeHeader,
        name_atom: u32,
        arg_count: u16,
        local_count: u16,
        stack_size: u16,
        flags: FunctionFlags,
        code: []const u8,
        constants: []const u32,
        upvalues: []const UpvalueInfo,
    ) !*FunctionBytecodeCompact {
        const upvalue_count: u8 = @intCast(@min(upvalues.len, 255));
        const code_len: u32 = @intCast(code.len);
        const const_count: u16 = @intCast(@min(constants.len, 65535));

        const total_size = calcSize(code_len, const_count, upvalue_count);
        const bytes = try allocator.alignedAlloc(u8, .@"8", total_size);
        errdefer allocator.free(bytes);

        // Initialize header
        const self: *FunctionBytecodeCompact = @ptrCast(@alignCast(bytes.ptr));
        self.* = .{
            .header = header,
            .name_atom = name_atom,
            .arg_count = arg_count,
            .local_count = local_count,
            .stack_size = stack_size,
            .flags = flags,
            .upvalue_count = upvalue_count,
            .code_len = code_len,
            .const_count = const_count,
        };

        // Copy code
        const code_dest = bytes[HEADER_SIZE..][0..code_len];
        @memcpy(code_dest, code);

        // Copy constants
        const code_end = HEADER_SIZE + code_len;
        const const_start = std.mem.alignForward(usize, code_end, @alignOf(u32));
        const const_dest: [*]u32 = @ptrCast(@alignCast(bytes.ptr + const_start));
        @memcpy(const_dest[0..const_count], constants);

        // Copy upvalue info
        const const_end = const_start + @as(usize, const_count) * @sizeOf(u32);
        const upval_start = std.mem.alignForward(usize, const_end, @alignOf(UpvalueInfo));
        const upval_dest: [*]UpvalueInfo = @ptrCast(@alignCast(bytes.ptr + upval_start));
        @memcpy(upval_dest[0..upvalue_count], upvalues);

        return self;
    }

    /// Free the compact bytecode
    pub fn destroy(self: *FunctionBytecodeCompact, allocator: std.mem.Allocator) void {
        const total_size = calcSize(self.code_len, self.const_count, self.upvalue_count);
        const bytes: [*]align(8) u8 = @ptrCast(self);
        allocator.free(bytes[0..total_size]);
    }

    /// Get raw bytes for serialization
    pub fn asBytes(self: *const FunctionBytecodeCompact) []const u8 {
        const total_size = calcSize(self.code_len, self.const_count, self.upvalue_count);
        const base = @as([*]const u8, @ptrCast(self));
        return base[0..total_size];
    }
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

test "FunctionBytecodeCompact creation and access" {
    const allocator = std.testing.allocator;

    const code = [_]u8{ 0x01, 0x00, 0x02, 0x20, 0x53 }; // push_const 2, add, ret
    const constants = [_]u32{ 42, 100, 200 };
    const upvalues = [_]UpvalueInfo{
        .{ .is_local = true, .index = 0 },
        .{ .is_local = false, .index = 1 },
    };

    const func = try FunctionBytecodeCompact.create(
        allocator,
        .{},
        0, // name_atom
        2, // arg_count
        4, // local_count
        8, // stack_size
        .{}, // flags
        &code,
        &constants,
        &upvalues,
    );
    defer func.destroy(allocator);

    // Verify header fields
    try std.testing.expectEqual(@as(u16, 2), func.arg_count);
    try std.testing.expectEqual(@as(u16, 4), func.local_count);
    try std.testing.expectEqual(@as(u16, 8), func.stack_size);

    // Verify code access
    const retrieved_code = func.getCode();
    try std.testing.expectEqual(@as(usize, 5), retrieved_code.len);
    try std.testing.expectEqual(@as(u8, 0x01), retrieved_code[0]);
    try std.testing.expectEqual(@as(u8, 0x53), retrieved_code[4]);

    // Verify constants access
    const retrieved_const = func.getConstants();
    try std.testing.expectEqual(@as(usize, 3), retrieved_const.len);
    try std.testing.expectEqual(@as(u32, 42), retrieved_const[0]);
    try std.testing.expectEqual(@as(u32, 200), retrieved_const[2]);

    // Verify upvalue access
    const retrieved_upval = func.getUpvalues();
    try std.testing.expectEqual(@as(usize, 2), retrieved_upval.len);
    try std.testing.expect(retrieved_upval[0].is_local);
    try std.testing.expect(!retrieved_upval[1].is_local);
}

test "FunctionBytecodeCompact serialization" {
    const allocator = std.testing.allocator;

    const code = [_]u8{ 0x08, 0x53 }; // push_null, ret
    const constants = [_]u32{123};

    const func = try FunctionBytecodeCompact.create(
        allocator,
        .{},
        0,
        0,
        1,
        2,
        .{},
        &code,
        &constants,
        &.{},
    );
    defer func.destroy(allocator);

    // Get bytes for serialization
    const bytes = func.asBytes();

    // Verify we can read back the header
    const deserialized: *const FunctionBytecodeCompact = @ptrCast(@alignCast(bytes.ptr));
    try std.testing.expectEqual(func.code_len, deserialized.code_len);
    try std.testing.expectEqual(func.const_count, deserialized.const_count);
}

test "FunctionBytecodeCompact size calculation" {
    // Verify header size is reasonable (includes padding)
    // BytecodeHeader(12) + name_atom(4) + arg/local/stack(6) + flags(1) + upvalue(1) + pad(1) + code_len(4) + const_count(2) + pad2(2) = 33, but struct alignment adds padding
    try std.testing.expect(FunctionBytecodeCompact.HEADER_SIZE <= 48);

    // Small function
    const small_size = FunctionBytecodeCompact.calcSize(10, 2, 0);
    try std.testing.expect(small_size < 80);

    // Medium function
    const medium_size = FunctionBytecodeCompact.calcSize(256, 16, 4);
    try std.testing.expect(medium_size < 512);
}

// ============================================================================
// Phase 16: Code Versioning for Hot Reload
// ============================================================================

/// CodeVersion wraps bytecode with reference counting and epoch tracking
/// for safe hot reload without use-after-free.
pub const CodeVersion = struct {
    /// The actual bytecode
    bytecode: *FunctionBytecode,
    /// Version number (epoch)
    version: u32,
    /// Reference count for safe deallocation
    ref_count: std.atomic.Value(u32),
    /// Allocator used to create this version
    allocator: std.mem.Allocator,
    /// Whether this version owns its bytecode (should free on destroy)
    owns_bytecode: bool,

    /// Create a new CodeVersion wrapping bytecode
    pub fn create(
        allocator: std.mem.Allocator,
        bytecode_ptr: *FunctionBytecode,
        version: u32,
        owns_bytecode: bool,
    ) !*CodeVersion {
        const cv = try allocator.create(CodeVersion);
        cv.* = .{
            .bytecode = bytecode_ptr,
            .version = version,
            .ref_count = std.atomic.Value(u32).init(1),
            .allocator = allocator,
            .owns_bytecode = owns_bytecode,
        };
        return cv;
    }

    /// Increment reference count
    pub fn retain(self: *CodeVersion) void {
        // Use acq_rel to match release() and ensure visibility across threads
        _ = self.ref_count.fetchAdd(1, .acq_rel);
    }

    /// Decrement reference count, destroy when it reaches 0
    pub fn release(self: *CodeVersion) void {
        // Use acq_rel to ensure all prior writes are visible before destruction
        const prev = self.ref_count.fetchSub(1, .acq_rel);
        if (prev == 1) {
            // We were the last reference
            self.destroy();
        }
    }

    /// Get current reference count (for debugging/testing)
    pub fn getRefCount(self: *const CodeVersion) u32 {
        return self.ref_count.load(.acquire);
    }

    /// Destroy this CodeVersion
    fn destroy(self: *CodeVersion) void {
        if (self.owns_bytecode) {
            // Free the bytecode
            self.allocator.free(self.bytecode.code);
            if (self.bytecode.constants.len > 0) {
                self.allocator.free(self.bytecode.constants);
            }
            if (self.bytecode.upvalue_info.len > 0) {
                self.allocator.free(self.bytecode.upvalue_info);
            }
            self.allocator.destroy(self.bytecode);
        }
        self.allocator.destroy(self);
    }
};

/// VersionedFunction holds an atomically-swappable code version
/// Used by function objects to support hot reload.
pub const VersionedFunction = struct {
    /// Atomic pointer to current code version
    code: std.atomic.Value(*CodeVersion),
    /// Name atom for identification
    name_atom: u32,

    /// Create a new versioned function
    pub fn create(allocator: std.mem.Allocator, initial_code: *CodeVersion, name_atom: u32) !*VersionedFunction {
        const vf = try allocator.create(VersionedFunction);
        vf.* = .{
            .code = std.atomic.Value(*CodeVersion).init(initial_code),
            .name_atom = name_atom,
        };
        // Retain the initial code since we're storing a reference
        initial_code.retain();
        return vf;
    }

    /// Get current bytecode (for execution)
    pub fn getBytecode(self: *VersionedFunction) *FunctionBytecode {
        return self.code.load(.acquire).bytecode;
    }

    /// Get current code version
    pub fn getCodeVersion(self: *VersionedFunction) *CodeVersion {
        return self.code.load(.acquire);
    }

    /// Get current version number
    pub fn getVersion(self: *VersionedFunction) u32 {
        return self.code.load(.acquire).version;
    }

    /// Atomically update to a new code version
    /// Returns the old version (caller should release it)
    pub fn updateCode(self: *VersionedFunction, new_version: *CodeVersion) *CodeVersion {
        new_version.retain();
        const old = self.code.swap(new_version, .acq_rel);
        return old;
    }

    /// Destroy this versioned function
    pub fn destroy(self: *VersionedFunction, allocator: std.mem.Allocator) void {
        // Release our reference to the code version
        self.code.load(.acquire).release();
        allocator.destroy(self);
    }
};

/// HotReloadManager coordinates code updates across multiple runtimes
pub const HotReloadManager = struct {
    /// All registered code versions (for cleanup)
    versions: std.ArrayListUnmanaged(*CodeVersion),
    /// Current epoch number
    current_epoch: u32,
    /// Allocator
    allocator: std.mem.Allocator,
    /// Mutex for version list updates
    mutex: std.Thread.Mutex,

    pub fn init(allocator: std.mem.Allocator) HotReloadManager {
        return .{
            .versions = .{},
            .current_epoch = 0,
            .allocator = allocator,
            .mutex = .{},
        };
    }

    pub fn deinit(self: *HotReloadManager) void {
        // Release all tracked versions
        for (self.versions.items) |version| {
            version.release();
        }
        self.versions.deinit(self.allocator);
    }

    /// Register a new code version
    pub fn registerVersion(self: *HotReloadManager, version: *CodeVersion) !void {
        self.mutex.lock();
        defer self.mutex.unlock();

        version.retain();
        try self.versions.append(self.allocator, version);
    }

    /// Create a new code version for hot reload
    pub fn createVersion(
        self: *HotReloadManager,
        bytecode_ptr: *FunctionBytecode,
        owns_bytecode: bool,
    ) !*CodeVersion {
        self.mutex.lock();
        const epoch = self.current_epoch + 1;
        self.current_epoch = epoch;
        self.mutex.unlock();

        const version = try CodeVersion.create(self.allocator, bytecode_ptr, epoch, owns_bytecode);
        try self.registerVersion(version);
        return version;
    }

    /// Get current epoch
    pub fn getEpoch(self: *HotReloadManager) u32 {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.current_epoch;
    }

    /// Cleanup unreferenced old versions
    pub fn collectOldVersions(self: *HotReloadManager) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        // Remove versions with ref_count == 1 (only our reference)
        var i: usize = 0;
        while (i < self.versions.items.len) {
            const version = self.versions.items[i];
            if (version.getRefCount() == 1) {
                // Only we hold a reference, safe to remove
                _ = self.versions.swapRemove(i);
                version.release();
            } else {
                i += 1;
            }
        }
    }
};

// ============================================================================
// Phase 16 Tests
// ============================================================================

test "CodeVersion reference counting" {
    const allocator = std.testing.allocator;

    // Create a simple bytecode
    const code = try allocator.alloc(u8, 4);
    code[0] = @intFromEnum(Opcode.push_1);
    code[1] = @intFromEnum(Opcode.ret);
    code[2] = 0;
    code[3] = 0;

    const constants = try allocator.alloc(value.JSValue, 0);
    const upvalue_info = try allocator.alloc(UpvalueInfo, 0);

    const bc = try allocator.create(FunctionBytecode);
    bc.* = .{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .upvalue_count = 0,
        .upvalue_info = upvalue_info,
        .code = code,
        .constants = constants,
        .source_map = null,
    };

    // Create code version (owns bytecode)
    const cv = try CodeVersion.create(allocator, bc, 1, true);

    // Initial ref count is 1
    try std.testing.expectEqual(@as(u32, 1), cv.getRefCount());

    // Retain increases count
    cv.retain();
    try std.testing.expectEqual(@as(u32, 2), cv.getRefCount());

    // Release decreases count
    cv.release();
    try std.testing.expectEqual(@as(u32, 1), cv.getRefCount());

    // Final release frees everything
    cv.release();
    // cv is now freed, test passes if no leak
}

test "VersionedFunction atomic update" {
    const allocator = std.testing.allocator;

    // Create initial bytecode v1
    const code1 = try allocator.alloc(u8, 2);
    code1[0] = @intFromEnum(Opcode.push_1);
    code1[1] = @intFromEnum(Opcode.ret);
    const bc1 = try allocator.create(FunctionBytecode);
    bc1.* = .{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .upvalue_count = 0,
        .upvalue_info = &.{},
        .code = code1,
        .constants = &.{},
        .source_map = null,
    };
    const cv1 = try CodeVersion.create(allocator, bc1, 1, true);
    defer cv1.release();

    // Create new bytecode v2
    const code2 = try allocator.alloc(u8, 2);
    code2[0] = @intFromEnum(Opcode.push_2);
    code2[1] = @intFromEnum(Opcode.ret);
    const bc2 = try allocator.create(FunctionBytecode);
    bc2.* = .{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .upvalue_count = 0,
        .upvalue_info = &.{},
        .code = code2,
        .constants = &.{},
        .source_map = null,
    };
    const cv2 = try CodeVersion.create(allocator, bc2, 2, true);
    defer cv2.release();

    // Create versioned function with v1
    const vf = try VersionedFunction.create(allocator, cv1, 0);
    defer vf.destroy(allocator);

    // Check initial version
    try std.testing.expectEqual(@as(u32, 1), vf.getVersion());
    try std.testing.expectEqual(@intFromEnum(Opcode.push_1), vf.getBytecode().code[0]);

    // Update to v2
    const old = vf.updateCode(cv2);
    old.release(); // Release the old version

    // Check updated version
    try std.testing.expectEqual(@as(u32, 2), vf.getVersion());
    try std.testing.expectEqual(@intFromEnum(Opcode.push_2), vf.getBytecode().code[0]);
}

test "HotReloadManager epoch tracking" {
    const allocator = std.testing.allocator;

    var manager = HotReloadManager.init(allocator);
    defer manager.deinit();

    // Initial epoch is 0
    try std.testing.expectEqual(@as(u32, 0), manager.getEpoch());

    // Create version increments epoch
    const code = try allocator.alloc(u8, 2);
    code[0] = @intFromEnum(Opcode.push_0);
    code[1] = @intFromEnum(Opcode.ret);
    const bc = try allocator.create(FunctionBytecode);
    bc.* = .{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .upvalue_count = 0,
        .upvalue_info = &.{},
        .code = code,
        .constants = &.{},
        .source_map = null,
    };

    const cv = try manager.createVersion(bc, true);
    try std.testing.expectEqual(@as(u32, 1), cv.version);
    try std.testing.expectEqual(@as(u32, 1), manager.getEpoch());

    // Release our reference (manager still holds one)
    cv.release();

    // Collect should clean up (only manager reference remains)
    manager.collectOldVersions();
}

test "CodeVersion concurrent retain/release" {
    const allocator = std.testing.allocator;

    // Create bytecode
    const code = try allocator.alloc(u8, 2);
    code[0] = @intFromEnum(Opcode.push_0);
    code[1] = @intFromEnum(Opcode.ret);

    const bc = try allocator.create(FunctionBytecode);
    bc.* = .{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .upvalue_count = 0,
        .upvalue_info = &.{},
        .code = code,
        .constants = &.{},
        .source_map = null,
    };

    // Create CodeVersion with initial ref_count = 1
    const cv = try CodeVersion.create(allocator, bc, 1, true);

    // We need an extra reference to keep cv alive during the test
    cv.retain(); // ref_count = 2

    const thread_count: usize = 8;
    const iterations: usize = 100;

    const ThreadCtx = struct {
        cv: *CodeVersion,
    };

    const Worker = struct {
        fn run(ctx: *ThreadCtx) void {
            for (0..iterations) |_| {
                ctx.cv.retain();
                // Small delay to increase chance of interleaving
                std.atomic.spinLoopHint();
                ctx.cv.release();
            }
        }
    };

    var threads: [thread_count]std.Thread = undefined;
    var contexts: [thread_count]ThreadCtx = undefined;
    for (0..thread_count) |i| {
        contexts[i] = .{ .cv = cv };
        threads[i] = std.Thread.spawn(.{}, Worker.run, .{&contexts[i]}) catch unreachable;
    }
    for (threads) |t| t.join();

    // After all threads complete, ref_count should be exactly 2
    // (initial + our extra retain)
    const final_count = cv.getRefCount();
    try std.testing.expectEqual(@as(u32, 2), final_count);

    // Release our extra reference
    cv.release(); // ref_count = 1

    // Release the original reference (should trigger destroy)
    cv.release(); // ref_count = 0, destroyed
}
