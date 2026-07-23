//! Compile-time field-offset and size constants the baseline JIT emits
//! into native code. Centralised so a struct-layout change shows up as
//! one source of truth instead of being smeared across the emitter.

const context_mod = @import("../context.zig");
const value_mod = @import("../value.zig");
const arena_mod = @import("../arena.zig");
const object = @import("../object.zig");
const interpreter_mod = @import("../interpreter.zig");

const Context = context_mod.Context;
const JSObject = object.JSObject;
const Interpreter = interpreter_mod.Interpreter;
const PolymorphicInlineCache = interpreter_mod.PolymorphicInlineCache;
const PICEntry = interpreter_mod.PICEntry;

// Context field offsets for JIT code generation
pub const CTX_STACK_PTR_OFF: i32 = @intCast(@offsetOf(Context, "stack"));
pub const CTX_STACK_LEN_OFF: i32 = @intCast(@offsetOf(Context, "stack") + @sizeOf(*value_mod.JSValue));
pub const CTX_SP_OFF: i32 = @intCast(@offsetOf(Context, "sp"));
pub const CTX_FP_OFF: i32 = @intCast(@offsetOf(Context, "fp"));
pub const CTX_JIT_INTERP_OFF: i32 = @intCast(@offsetOf(Context, "jit_interpreter"));
pub const CTX_HYBRID_OFF: i32 = @intCast(@offsetOf(Context, "hybrid"));

// Arena offsets for inline bump allocation
pub const HYBRID_ARENA_OFF: i32 = @intCast(@offsetOf(arena_mod.HybridAllocator, "arena"));
pub const ARENA_PTR_OFF: i32 = @intCast(@offsetOf(arena_mod.Arena, "ptr"));
pub const ARENA_LIMIT_OFF: i32 = @intCast(@offsetOf(arena_mod.Arena, "limit"));
pub const JSOBJECT_SIZE: u32 = @sizeOf(JSObject);
pub const JSOBJECT_SIZE_ALIGNED: u32 = (JSOBJECT_SIZE + 7) & ~@as(u32, 7);

pub const PTR_EXTRACT_MASK: u64 = value_mod.JSValue.PAYLOAD_MASK;

// JSObject field offsets for inline cache fast path
pub const OBJ_HIDDEN_CLASS_OFF: i32 = @intCast(@offsetOf(JSObject, "hidden_class_idx"));
pub const OBJ_INLINE_SLOTS_OFF: i32 = @intCast(@offsetOf(JSObject, "inline_slots"));
pub const OBJ_FUNC_DATA_OFF: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, @intCast(object.JSObject.Slots.FUNC_DATA)) * 8;
pub const OBJ_FUNC_IS_BYTECODE_OFF: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, @intCast(object.JSObject.Slots.FUNC_IS_BYTECODE)) * 8;
pub const OBJ_FUNC_GUARD_ID_OFF: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, @intCast(object.JSObject.Slots.FUNC_GUARD_ID)) * 8;
pub const OBJ_CLASS_ID_OFF: i32 = @intCast(@offsetOf(JSObject, "class_id"));
pub const OBJ_RANGE_START_OFF: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, @intCast(object.JSObject.Slots.RANGE_START)) * 8;
pub const OBJ_RANGE_STEP_OFF: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, @intCast(object.JSObject.Slots.RANGE_STEP)) * 8;
pub const OBJ_RANGE_LENGTH_OFF: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, @intCast(object.JSObject.Slots.RANGE_LENGTH)) * 8;
pub const NATIVE_ARGCOUNT_OFF: i32 = @intCast(@offsetOf(object.NativeFunctionData, "arg_count"));

// Interpreter field offsets for inline cache fast path
pub const INTERP_PIC_CACHE_OFF: i32 = @intCast(@offsetOf(Interpreter, "pic_cache"));

// PIC structure sizes and offsets
pub const PIC_SIZE: i32 = @intCast(@sizeOf(PolymorphicInlineCache));
pub const PIC_ENTRY_SIZE: i32 = @intCast(@sizeOf(PICEntry));
pub const PIC_ENTRY_HIDDEN_CLASS_OFF: i32 = 0;
pub const PIC_ENTRY_SLOT_OFF: i32 = @intCast(@sizeOf(object.HiddenClassIndex));
pub const PIC_CHECK_COUNT: i32 = 4;
