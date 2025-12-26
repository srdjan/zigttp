//! JavaScript objects with hidden classes and inline caches
//!
//! Fast property access via shape-based inline caching.

const std = @import("std");
const heap = @import("heap.zig");
const value = @import("value.zig");

/// Atom - interned property name identifier
pub const Atom = enum(u32) {
    // Predefined atoms (0-160)
    @"null" = 0,
    @"true" = 1,
    @"false" = 2,
    undefined = 3,
    length = 4,
    prototype = 5,
    constructor = 6,
    toString = 7,
    valueOf = 8,
    name = 9,
    message = 10,
    arguments = 11,
    caller = 12,
    callee = 13,
    @"this" = 14,
    @"return" = 15,
    @"throw" = 16,
    @"try" = 17,
    @"catch" = 18,
    finally = 19,
    @"if" = 20,
    @"else" = 21,
    @"while" = 22,
    @"for" = 23,
    @"do" = 24,
    @"switch" = 25,
    case = 26,
    default = 27,
    @"break" = 28,
    @"continue" = 29,
    @"var" = 30,
    let = 31,
    @"const" = 32,
    function = 33,
    new = 34,
    delete = 35,
    typeof = 36,
    @"void" = 37,
    in = 38,
    instanceof = 39,
    // Object properties
    __proto__ = 40,
    hasOwnProperty = 41,
    isPrototypeOf = 42,
    propertyIsEnumerable = 43,
    toLocaleString = 44,
    // Array
    push = 45,
    pop = 46,
    shift = 47,
    unshift = 48,
    slice = 49,
    splice = 50,
    concat = 51,
    join = 52,
    reverse = 53,
    sort = 54,
    indexOf = 55,
    lastIndexOf = 56,
    forEach = 57,
    map = 58,
    filter = 59,
    reduce = 60,
    reduceRight = 61,
    every = 62,
    some = 63,
    find = 64,
    findIndex = 65,
    includes = 66,
    fill = 67,
    copyWithin = 68,
    entries = 69,
    keys = 70,
    values = 71,
    // String
    charAt = 72,
    charCodeAt = 73,
    codePointAt = 74,
    split = 75,
    substring = 76,
    substr = 77,
    toLowerCase = 78,
    toUpperCase = 79,
    trim = 80,
    trimStart = 81,
    trimEnd = 82,
    padStart = 83,
    padEnd = 84,
    repeat = 85,
    replace = 86,
    replaceAll = 87,
    match = 88,
    search = 89,
    startsWith = 90,
    endsWith = 91,
    // Function
    call = 92,
    apply = 93,
    bind = 94,
    // Math
    abs = 95,
    floor = 96,
    ceil = 97,
    round = 98,
    min = 99,
    max = 100,
    pow = 101,
    sqrt = 102,
    random = 103,
    sin = 104,
    cos = 105,
    tan = 106,
    log = 107,
    exp = 108,
    // JSON
    parse = 109,
    stringify = 110,
    // Symbol
    iterator = 111,
    toStringTag = 112,
    // Promise
    then = 113,
    catch_method = 114,
    finally_method = 115,
    resolve = 116,
    reject = 117,
    all = 118,
    race = 119,
    // Error types
    Error = 120,
    TypeError = 121,
    RangeError = 122,
    SyntaxError = 123,
    ReferenceError = 124,
    URIError = 125,
    EvalError = 126,
    // Global
    console = 127,
    globalThis = 128,
    Infinity = 129,
    NaN_atom = 130,
    Object = 131,
    Array = 132,
    String = 133,
    Number = 134,
    Boolean = 135,
    Function = 136,
    Symbol = 137,
    Date = 138,
    RegExp = 139,
    Math = 140,
    JSON = 141,
    Promise = 142,
    Proxy = 143,
    Reflect = 144,
    Map = 145,
    Set = 146,
    WeakMap = 147,
    WeakSet = 148,
    ArrayBuffer = 149,
    DataView = 150,
    Int8Array = 151,
    Uint8Array = 152,
    Int16Array = 153,
    Uint16Array = 154,
    Int32Array = 155,
    Uint32Array = 156,
    Float32Array = 157,
    Float64Array = 158,
    // Reserved for more builtins
    __count__ = 160,

    // Dynamic atoms start at 161
    _,

    pub const FIRST_DYNAMIC: u32 = 161;

    /// Check if atom is a predefined (static) atom
    pub fn isPredefined(self: Atom) bool {
        return @intFromEnum(self) < FIRST_DYNAMIC;
    }
};

/// Property descriptor flags
pub const PropertyFlags = packed struct {
    writable: bool = true,
    enumerable: bool = true,
    configurable: bool = true,
    is_accessor: bool = false,
    _reserved: u4 = 0,
};

/// Property slot in hidden class
pub const PropertySlot = struct {
    name: Atom,
    offset: u16,
    flags: PropertyFlags,
};

/// Hidden class (shape) for objects
pub const HiddenClass = struct {
    properties: []const PropertySlot,
    transitions: TransitionMap,
    prototype: ?*HiddenClass,
    property_count: u16,

    const TransitionMap = std.AutoHashMap(Atom, *HiddenClass);

    pub fn init(allocator: std.mem.Allocator) !*HiddenClass {
        const class = try allocator.create(HiddenClass);
        class.* = .{
            .properties = &.{},
            .transitions = TransitionMap.init(allocator),
            .prototype = null,
            .property_count = 0,
        };
        return class;
    }

    pub fn deinit(self: *HiddenClass, allocator: std.mem.Allocator) void {
        self.transitions.deinit();
        allocator.destroy(self);
    }

    /// Get or create transition to new shape with added property
    pub fn addProperty(self: *HiddenClass, allocator: std.mem.Allocator, name: Atom) !*HiddenClass {
        // Check for existing transition
        if (self.transitions.get(name)) |existing| {
            return existing;
        }

        // Create new hidden class
        const new_class = try allocator.create(HiddenClass);
        errdefer allocator.destroy(new_class);

        // Copy properties and add new one
        var new_props = try allocator.alloc(PropertySlot, self.property_count + 1);
        @memcpy(new_props[0..self.property_count], self.properties);
        new_props[self.property_count] = .{
            .name = name,
            .offset = self.property_count,
            .flags = .{},
        };

        new_class.* = .{
            .properties = new_props,
            .transitions = TransitionMap.init(allocator),
            .prototype = self.prototype,
            .property_count = self.property_count + 1,
        };

        // Cache transition
        try self.transitions.put(name, new_class);

        return new_class;
    }

    /// Find property slot by name
    pub fn findProperty(self: *HiddenClass, name: Atom) ?*const PropertySlot {
        for (self.properties) |*slot| {
            if (slot.name == name) return slot;
        }
        return null;
    }
};

/// Object class ID for built-in types
pub const ClassId = enum(u8) {
    object = 0,
    array = 1,
    function = 2,
    bound_function = 3,
    generator = 4,
    @"error" = 5,
    array_buffer = 6,
    typed_array = 7,
    data_view = 8,
    promise = 9,
    map = 10,
    set = 11,
    weak_map = 12,
    weak_set = 13,
    regexp = 14,
    date = 15,
    proxy = 16,
    string_object = 17,
    number_object = 18,
    boolean_object = 19,
    symbol_object = 20,
    arguments = 21,
    // Custom class IDs start here
    _,

    pub const FIRST_CUSTOM: u8 = 64;
};

/// JavaScript object
/// Uses extern struct for predictable C-compatible memory layout
pub const JSObject = extern struct {
    header: heap.MemBlockHeader,
    hidden_class: *HiddenClass,
    prototype: ?*JSObject,
    class_id: ClassId,
    flags: ObjectFlags,
    inline_slots: [INLINE_SLOT_COUNT]value.JSValue,
    overflow_slots: ?[*]value.JSValue,
    overflow_capacity: u16,

    pub const INLINE_SLOT_COUNT = 8;

    pub const ObjectFlags = packed struct {
        extensible: bool = true,
        is_exotic: bool = false, // Array, TypedArray, etc.
        is_callable: bool = false,
        is_constructor: bool = false,
        has_small_array: bool = false, // Dense array optimization
        _reserved: u3 = 0,
    };

    /// Create a new object
    pub fn create(allocator: std.mem.Allocator, class: *HiddenClass, prototype: ?*JSObject) !*JSObject {
        const obj = try allocator.create(JSObject);
        obj.* = .{
            .header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject)),
            .hidden_class = class,
            .prototype = prototype,
            .class_id = .object,
            .flags = .{},
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
        };
        return obj;
    }

    /// Destroy object
    pub fn destroy(self: *JSObject, allocator: std.mem.Allocator) void {
        if (self.overflow_slots) |slots| {
            allocator.free(slots[0..self.overflow_capacity]);
        }
        allocator.destroy(self);
    }

    /// Fast property access by slot offset
    pub inline fn getSlot(self: *const JSObject, slot: u16) value.JSValue {
        if (slot < INLINE_SLOT_COUNT) {
            return self.inline_slots[slot];
        }
        if (self.overflow_slots) |slots| {
            return slots[slot - INLINE_SLOT_COUNT];
        }
        return value.JSValue.undefined_val;
    }

    /// Fast property store by slot offset
    pub inline fn setSlot(self: *JSObject, slot: u16, val: value.JSValue) void {
        if (slot < INLINE_SLOT_COUNT) {
            self.inline_slots[slot] = val;
        } else if (self.overflow_slots) |slots| {
            slots[slot - INLINE_SLOT_COUNT] = val;
        }
    }

    /// Ultra-fast property access for inline caching (no bounds check)
    pub inline fn getPropertyFast(self: *const JSObject, slot: u16) value.JSValue {
        return self.getSlot(slot);
    }

    /// Ultra-fast property store for inline caching
    pub inline fn setPropertyFast(self: *JSObject, slot: u16, val: value.JSValue) void {
        self.setSlot(slot, val);
    }

    /// Get property by name (with prototype chain lookup)
    pub fn getProperty(self: *const JSObject, name: Atom) ?value.JSValue {
        // Check own properties first
        if (self.hidden_class.findProperty(name)) |slot| {
            return self.getSlot(slot.offset);
        }

        // Walk prototype chain
        var proto = self.prototype;
        while (proto) |p| {
            if (p.hidden_class.findProperty(name)) |slot| {
                return p.getSlot(slot.offset);
            }
            proto = p.prototype;
        }

        return null;
    }

    /// Get own property (no prototype lookup)
    pub fn getOwnProperty(self: *const JSObject, name: Atom) ?value.JSValue {
        if (self.hidden_class.findProperty(name)) |slot| {
            return self.getSlot(slot.offset);
        }
        return null;
    }

    /// Set property (with hidden class transition)
    pub fn setProperty(self: *JSObject, allocator: std.mem.Allocator, name: Atom, val: value.JSValue) !void {
        // Check if property exists
        if (self.hidden_class.findProperty(name)) |slot| {
            self.setSlot(slot.offset, val);
            return;
        }

        // Check extensibility
        if (!self.flags.extensible) {
            return; // Silently fail in non-strict mode
        }

        // Transition to new hidden class
        const new_class = try self.hidden_class.addProperty(allocator, name);
        const new_slot = new_class.property_count - 1;

        // Ensure we have space
        if (new_slot >= INLINE_SLOT_COUNT) {
            try self.ensureOverflowCapacity(allocator, new_slot - INLINE_SLOT_COUNT + 1);
        }

        self.hidden_class = new_class;
        self.setSlot(new_slot, val);
    }

    /// Ensure overflow slots have enough capacity
    fn ensureOverflowCapacity(self: *JSObject, allocator: std.mem.Allocator, min_capacity: u16) !void {
        if (self.overflow_capacity >= min_capacity) return;

        const new_capacity = @max(min_capacity, self.overflow_capacity * 2, 4);
        const new_slots = try allocator.alloc(value.JSValue, new_capacity);

        // Copy existing
        if (self.overflow_slots) |old_slots| {
            @memcpy(new_slots[0..self.overflow_capacity], old_slots[0..self.overflow_capacity]);
            allocator.free(old_slots[0..self.overflow_capacity]);
        }

        // Initialize new slots
        for (self.overflow_capacity..new_capacity) |i| {
            new_slots[i] = value.JSValue.undefined_val;
        }

        self.overflow_slots = new_slots.ptr;
        self.overflow_capacity = new_capacity;
    }

    /// Check if object has own property
    pub fn hasOwnProperty(self: *const JSObject, name: Atom) bool {
        return self.hidden_class.findProperty(name) != null;
    }

    /// Check if property exists (including prototype chain)
    pub fn hasProperty(self: *const JSObject, name: Atom) bool {
        return self.getProperty(name) != null;
    }

    /// Delete property
    pub fn deleteProperty(self: *JSObject, name: Atom) bool {
        if (self.hidden_class.findProperty(name)) |slot| {
            if (!slot.flags.configurable) return false;
            // Note: In a full implementation, we'd transition to a new hidden class
            // For now, just set to undefined
            self.setSlot(slot.offset, value.JSValue.undefined_val);
            return true;
        }
        return true; // Deleting non-existent property succeeds
    }

    /// Prevent extensions
    pub fn preventExtensions(self: *JSObject) void {
        self.flags.extensible = false;
    }

    /// Check if object is extensible
    pub fn isExtensible(self: *const JSObject) bool {
        return self.flags.extensible;
    }

    /// Get all own enumerable property names
    pub fn getOwnEnumerableKeys(self: *const JSObject, allocator: std.mem.Allocator) ![]Atom {
        var keys = std.ArrayList(Atom).init(allocator);
        errdefer keys.deinit();

        for (self.hidden_class.properties) |prop| {
            if (prop.flags.enumerable) {
                try keys.append(prop.name);
            }
        }

        return keys.toOwnedSlice();
    }

    /// Convert to JSValue
    pub fn toValue(self: *JSObject) value.JSValue {
        return value.JSValue.fromPtr(self);
    }

    /// Get from JSValue (unsafe - caller must verify isObject)
    pub fn fromValue(val: value.JSValue) *JSObject {
        return val.toPtr(JSObject);
    }
};

/// Inline cache for property access
pub const InlineCache = struct {
    cached_class: ?*HiddenClass = null,
    cached_slot: u16 = 0,
    hit_count: u32 = 0,
    miss_count: u32 = 0,

    /// Try fast property lookup via cached shape
    pub fn get(self: *InlineCache, obj: *JSObject) ?value.JSValue {
        if (obj.hidden_class == self.cached_class) {
            self.hit_count += 1;
            return obj.getPropertyFast(self.cached_slot);
        }
        self.miss_count += 1;
        return null;
    }

    /// Update cache after miss
    pub fn update(self: *InlineCache, class: *HiddenClass, slot: u16) void {
        self.cached_class = class;
        self.cached_slot = slot;
    }

    /// Cache hit ratio (for profiling)
    pub fn hitRatio(self: *InlineCache) f64 {
        const total = self.hit_count + self.miss_count;
        if (total == 0) return 0.0;
        return @as(f64, @floatFromInt(self.hit_count)) / @as(f64, @floatFromInt(total));
    }
};

// ============================================================================
// Tests
// ============================================================================

test "Atom predefined check" {
    try std.testing.expect(Atom.length.isPredefined());
    try std.testing.expect(Atom.prototype.isPredefined());
    try std.testing.expect(!(@as(Atom, @enumFromInt(200)).isPredefined()));
}

test "HiddenClass property transitions" {
    const allocator = std.testing.allocator;

    var root = try HiddenClass.init(allocator);
    defer root.deinit(allocator);

    // Add 'length' property
    var class_x = try root.addProperty(allocator, .length);
    defer {
        allocator.free(class_x.properties);
        class_x.deinit(allocator);
    }

    try std.testing.expectEqual(@as(u16, 1), class_x.property_count);

    // Same transition should return cached class
    const class_x2 = try root.addProperty(allocator, .length);
    try std.testing.expectEqual(class_x, class_x2);
}

test "HiddenClass multiple properties" {
    const allocator = std.testing.allocator;

    var root = try HiddenClass.init(allocator);
    defer root.deinit(allocator);

    // Add 'x' then 'y'
    var class_x = try root.addProperty(allocator, .length);
    defer {
        allocator.free(class_x.properties);
        class_x.deinit(allocator);
    }

    var class_xy = try class_x.addProperty(allocator, .prototype);
    defer {
        allocator.free(class_xy.properties);
        class_xy.deinit(allocator);
    }

    try std.testing.expectEqual(@as(u16, 2), class_xy.property_count);

    // Find properties
    try std.testing.expect(class_xy.findProperty(.length) != null);
    try std.testing.expect(class_xy.findProperty(.prototype) != null);
    try std.testing.expect(class_xy.findProperty(.constructor) == null);
}

test "InlineCache hit tracking" {
    var ic = InlineCache{};
    try std.testing.expectEqual(@as(f64, 0.0), ic.hitRatio());

    ic.hit_count = 9;
    ic.miss_count = 1;
    try std.testing.expectEqual(@as(f64, 0.9), ic.hitRatio());
}

test "JSObject creation and property access" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var obj = try JSObject.create(allocator, root_class, null);
    defer obj.destroy(allocator);

    // Initially no properties
    try std.testing.expect(obj.getOwnProperty(.length) == null);

    // Set property
    try obj.setProperty(allocator, .length, value.JSValue.fromInt(42));
    defer allocator.free(obj.hidden_class.properties);
    defer obj.hidden_class.deinit(allocator);

    // Get property
    const val = obj.getOwnProperty(.length);
    try std.testing.expect(val != null);
    try std.testing.expectEqual(@as(i32, 42), val.?.getInt());
}

test "JSObject prototype chain lookup" {
    const allocator = std.testing.allocator;

    // Create prototype
    var proto_class = try HiddenClass.init(allocator);
    defer proto_class.deinit(allocator);

    var proto = try JSObject.create(allocator, proto_class, null);
    defer proto.destroy(allocator);

    try proto.setProperty(allocator, .toString, value.JSValue.fromInt(1));
    defer allocator.free(proto.hidden_class.properties);
    defer proto.hidden_class.deinit(allocator);

    // Create child object with prototype
    var child_class = try HiddenClass.init(allocator);
    defer child_class.deinit(allocator);

    var child = try JSObject.create(allocator, child_class, proto);
    defer child.destroy(allocator);

    // Child can access prototype property
    const val = child.getProperty(.toString);
    try std.testing.expect(val != null);
    try std.testing.expectEqual(@as(i32, 1), val.?.getInt());

    // But not as own property
    try std.testing.expect(child.getOwnProperty(.toString) == null);
}

test "JSObject hasProperty" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var obj = try JSObject.create(allocator, root_class, null);
    defer obj.destroy(allocator);

    try std.testing.expect(!obj.hasOwnProperty(.length));
    try std.testing.expect(!obj.hasProperty(.length));

    try obj.setProperty(allocator, .length, value.JSValue.fromInt(5));
    defer allocator.free(obj.hidden_class.properties);
    defer obj.hidden_class.deinit(allocator);

    try std.testing.expect(obj.hasOwnProperty(.length));
    try std.testing.expect(obj.hasProperty(.length));
}

test "JSObject extensibility" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var obj = try JSObject.create(allocator, root_class, null);
    defer obj.destroy(allocator);

    try std.testing.expect(obj.isExtensible());

    obj.preventExtensions();
    try std.testing.expect(!obj.isExtensible());

    // Can't add new properties
    try obj.setProperty(allocator, .length, value.JSValue.fromInt(10));
    try std.testing.expect(obj.getOwnProperty(.length) == null);
}

test "JSObject to/from value" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var obj = try JSObject.create(allocator, root_class, null);
    defer obj.destroy(allocator);

    const js_val = obj.toValue();
    try std.testing.expect(js_val.isPtr());

    const recovered = JSObject.fromValue(js_val);
    try std.testing.expectEqual(obj, recovered);
}

test "InlineCache with object" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var obj = try JSObject.create(allocator, root_class, null);
    defer obj.destroy(allocator);

    try obj.setProperty(allocator, .length, value.JSValue.fromInt(100));
    defer allocator.free(obj.hidden_class.properties);
    defer obj.hidden_class.deinit(allocator);

    var ic = InlineCache{};

    // First access - cache miss
    const miss_result = ic.get(obj);
    try std.testing.expect(miss_result == null);
    try std.testing.expectEqual(@as(u32, 1), ic.miss_count);

    // Update cache
    if (obj.hidden_class.findProperty(.length)) |slot| {
        ic.update(obj.hidden_class, slot.offset);
    }

    // Second access - cache hit
    const hit_result = ic.get(obj);
    try std.testing.expect(hit_result != null);
    try std.testing.expectEqual(@as(i32, 100), hit_result.?.getInt());
    try std.testing.expectEqual(@as(u32, 1), ic.hit_count);
}
