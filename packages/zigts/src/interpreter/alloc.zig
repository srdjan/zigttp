//! Object/array/string allocation helpers used by the dispatch loop.
//! Each helper picks arena vs heap based on the context's hybrid mode.

const value = @import("../value.zig");
const object = @import("../object.zig");
const string = @import("../string.zig");
const interpreter = @import("../interpreter.zig");
const Interpreter = interpreter.Interpreter;

pub fn createObject(self: *Interpreter) !*object.JSObject {
    const root_class_idx = self.ctx.root_class_idx;
    if (self.ctx.hybrid) |h| {
        return object.JSObject.createWithArena(h.arena, root_class_idx, null, self.ctx.hidden_class_pool) orelse
            return error.OutOfMemory;
    }
    return try object.JSObject.create(self.ctx.allocator, root_class_idx, null, self.ctx.hidden_class_pool);
}

pub fn createArray(self: *Interpreter) !*object.JSObject {
    const root_class_idx = self.ctx.root_class_idx;
    if (self.ctx.hybrid) |h| {
        const obj = object.JSObject.createArrayWithArena(h.arena, root_class_idx) orelse
            return error.OutOfMemory;
        obj.prototype = self.ctx.array_prototype;
        return obj;
    }
    const obj = try object.JSObject.createArray(self.ctx.allocator, root_class_idx);
    obj.prototype = self.ctx.array_prototype;
    return obj;
}

/// Build a "condition rejected: <type> has no falsy state" exception
/// payload for sound-mode boolean checks. Buffer-bounded to 80 bytes.
pub fn createBoolError(self: *Interpreter, val: value.JSValue) !value.JSValue {
    const type_name = val.typeOf();
    const prefix = "condition rejected: ";
    const suffix = " has no falsy state";
    var buf: [80]u8 = undefined;
    const total = @min(prefix.len + type_name.len + suffix.len, buf.len);
    @memcpy(buf[0..prefix.len], prefix);
    const type_copy_len = @min(type_name.len, total - prefix.len);
    @memcpy(buf[prefix.len..][0..type_copy_len], type_name[0..type_copy_len]);
    const suffix_len = @min(suffix.len, total - prefix.len - type_copy_len);
    @memcpy(buf[prefix.len + type_copy_len ..][0..suffix_len], suffix[0..suffix_len]);
    const msg = buf[0 .. prefix.len + type_copy_len + suffix_len];
    const js_str = blk: {
        if (self.ctx.hybrid) |h| {
            break :blk string.createStringWithArena(h.arena, msg) orelse return error.OutOfMemory;
        }
        break :blk try string.createString(self.ctx.allocator, msg);
    };
    return value.JSValue.fromPtr(js_str);
}

pub inline fn getConstant(self: *Interpreter, idx: u16) !value.JSValue {
    if (idx >= self.constants.len) return error.InvalidConstant;
    return self.constants[idx];
}
