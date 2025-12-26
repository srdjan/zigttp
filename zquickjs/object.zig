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
    // ... more predefined atoms

    // Dynamic atoms start at 161
    _,

    pub const FIRST_DYNAMIC: u32 = 161;
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

/// JavaScript object
pub const JSObject = struct {
    header: heap.MemBlockHeader,
    hidden_class: *HiddenClass,
    inline_slots: [INLINE_SLOT_COUNT]value.JSValue,
    overflow_slots: ?[*]value.JSValue,

    pub const INLINE_SLOT_COUNT = 8;

    /// Fast property access by slot offset
    pub inline fn getPropertyFast(self: *JSObject, slot: u16) value.JSValue {
        if (slot < INLINE_SLOT_COUNT) {
            return self.inline_slots[slot];
        }
        return self.overflow_slots.?[slot - INLINE_SLOT_COUNT];
    }

    /// Fast property store by slot offset
    pub inline fn setPropertyFast(self: *JSObject, slot: u16, val: value.JSValue) void {
        if (slot < INLINE_SLOT_COUNT) {
            self.inline_slots[slot] = val;
        } else {
            self.overflow_slots.?[slot - INLINE_SLOT_COUNT] = val;
        }
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

test "HiddenClass property transitions" {
    const allocator = std.testing.allocator;

    var root = try HiddenClass.init(allocator);
    defer root.deinit(allocator);

    // Add 'x' property
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

test "InlineCache hit tracking" {
    var ic = InlineCache{};
    try std.testing.expectEqual(@as(f64, 0.0), ic.hitRatio());

    ic.hit_count = 9;
    ic.miss_count = 1;
    try std.testing.expectEqual(@as(f64, 0.9), ic.hitRatio());
}
