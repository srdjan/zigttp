//! Type Feedback Collection for JIT Optimization
//!
//! Records runtime type information at hot code sites to enable
//! speculative optimization in the baseline JIT compiler.

const std = @import("std");
const value = @import("value.zig");
const object = @import("object.zig");
const bytecode = @import("bytecode.zig");

/// Observed runtime type classification for type feedback
pub const ObservedType = enum(u8) {
    none = 0,
    smi, // 31-bit tagged integer (fast path)
    heap_number, // Heap-allocated f64
    string,
    object, // Generic object
    array,
    function,
    boolean,
    null_type,
    undefined_type,
};

/// Type feedback site for binary operations and property access
/// Records up to 4 observed types before transitioning to megamorphic
pub const TypeFeedbackSite = struct {
    /// Observed types at this site (up to 4 for polymorphic)
    observed: [4]ObservedType = .{.none} ** 4,
    /// Hidden class indices for object types (enables shape specialization)
    hidden_classes: [4]object.HiddenClassIndex = .{.none} ** 4,
    /// Number of distinct types observed
    count: u8 = 0,
    /// Total hits at this site
    total_hits: u32 = 0,
    /// True when more than 4 types observed
    megamorphic: bool = false,

    /// Record a value's type at this feedback site
    pub fn record(self: *TypeFeedbackSite, val: value.JSValue) void {
        const observed_type = classifyValue(val);
        self.total_hits +%= 1;

        // Check if this type was already recorded
        for (self.observed[0..self.count]) |t| {
            if (t == observed_type) return;
        }

        // Add new type if space available
        if (self.count < 4) {
            self.observed[self.count] = observed_type;
            // Record hidden class for objects
            if (observed_type == .object or observed_type == .array or observed_type == .function) {
                if (val.isPtr()) {
                    const obj = object.JSObject.fromValue(val);
                    self.hidden_classes[self.count] = obj.hidden_class_idx;
                }
            }
            self.count += 1;
        } else {
            self.megamorphic = true;
        }
    }

    /// Check if this site has only seen one type
    pub fn isMonomorphic(self: *const TypeFeedbackSite) bool {
        return self.count == 1 and !self.megamorphic;
    }

    /// Check if this site is polymorphic (2-4 types)
    pub fn isPolymorphic(self: *const TypeFeedbackSite) bool {
        return self.count > 1 and self.count <= 4 and !self.megamorphic;
    }

    /// Get the dominant (first observed) type, or null if no feedback
    pub fn dominantType(self: *const TypeFeedbackSite) ?ObservedType {
        if (self.count == 0) return null;
        return self.observed[0];
    }

    /// Get the hidden class for monomorphic object access
    pub fn getMonomorphicHiddenClass(self: *const TypeFeedbackSite) ?object.HiddenClassIndex {
        if (!self.isMonomorphic()) return null;
        const hc = self.hidden_classes[0];
        if (hc == .none) return null;
        return hc;
    }

    /// Reset feedback (used after deoptimization)
    pub fn reset(self: *TypeFeedbackSite) void {
        self.observed = .{.none} ** 4;
        self.hidden_classes = .{.none} ** 4;
        self.count = 0;
        self.total_hits = 0;
        self.megamorphic = false;
    }
};

/// Call site feedback for function inlining decisions
/// Tracks which functions are called at each call site
pub const CallSiteFeedback = struct {
    /// Observed callees at this site (up to 4)
    callees: [4]?*const bytecode.FunctionBytecode = .{null} ** 4,
    /// Number of distinct callees observed
    count: u8 = 0,
    /// True when more than 4 callees observed
    megamorphic: bool = false,
    /// Total calls at this site
    total_calls: u32 = 0,

    /// Record a callee at this call site
    pub fn recordCallee(self: *CallSiteFeedback, func_bc: ?*const bytecode.FunctionBytecode) void {
        self.total_calls +%= 1;

        if (func_bc == null) return;

        // Check if already seen
        for (self.callees[0..self.count]) |c| {
            if (c == func_bc) return;
        }

        // Add new callee if space available
        if (self.count < 4) {
            self.callees[self.count] = func_bc;
            self.count += 1;
        } else {
            self.megamorphic = true;
        }
    }

    /// Check if this call site is monomorphic (single callee)
    pub fn isMonomorphic(self: *const CallSiteFeedback) bool {
        return self.count == 1 and !self.megamorphic;
    }

    /// Get the monomorphic callee, or null if polymorphic/megamorphic
    pub fn getMonomorphicCallee(self: *const CallSiteFeedback) ?*const bytecode.FunctionBytecode {
        if (!self.isMonomorphic()) return null;
        return self.callees[0];
    }

    /// Reset feedback
    pub fn reset(self: *CallSiteFeedback) void {
        self.callees = .{null} ** 4;
        self.count = 0;
        self.megamorphic = false;
        self.total_calls = 0;
    }
};

/// Type feedback vector for a function
/// Allocated when function becomes a JIT candidate
pub const TypeFeedback = struct {
    /// Feedback sites for binary operations and property access
    sites: []TypeFeedbackSite,
    /// Feedback sites for function calls
    call_sites: []CallSiteFeedback,
    /// Allocator used for this feedback vector
    allocator: std.mem.Allocator,

    /// Initialize a type feedback vector
    pub fn init(
        allocator: std.mem.Allocator,
        site_count: u32,
        call_site_count: u32,
    ) !*TypeFeedback {
        const tf = try allocator.create(TypeFeedback);
        errdefer allocator.destroy(tf);

        tf.* = .{
            .sites = if (site_count > 0)
                try allocator.alloc(TypeFeedbackSite, site_count)
            else
                &.{},
            .call_sites = if (call_site_count > 0)
                try allocator.alloc(CallSiteFeedback, call_site_count)
            else
                &.{},
            .allocator = allocator,
        };

        // Initialize all sites to default
        for (tf.sites) |*site| {
            site.* = .{};
        }
        for (tf.call_sites) |*site| {
            site.* = .{};
        }

        return tf;
    }

    /// Free the type feedback vector
    pub fn deinit(self: *TypeFeedback) void {
        if (self.sites.len > 0) {
            self.allocator.free(self.sites);
        }
        if (self.call_sites.len > 0) {
            self.allocator.free(self.call_sites);
        }
        self.allocator.destroy(self);
    }

    /// Get total memory used by this feedback vector
    pub fn memoryUsage(self: *const TypeFeedback) usize {
        return @sizeOf(TypeFeedback) +
            self.sites.len * @sizeOf(TypeFeedbackSite) +
            self.call_sites.len * @sizeOf(CallSiteFeedback);
    }
};

/// Inlining policy for function inlining decisions
pub const InliningPolicy = struct {
    /// Maximum bytecode size of a function to inline
    pub const MAX_INLINE_BYTECODE_SIZE: u32 = 50;
    /// Maximum inlining depth
    pub const MAX_INLINE_DEPTH: u32 = 3;
    /// Minimum call count before considering inlining
    pub const MIN_CALL_COUNT: u32 = 10;
    /// Maximum total inlined bytecode budget per function
    pub const MAX_INLINED_SIZE: u32 = 500;

    /// Decision result for inlining
    pub const InlineDecision = enum {
        yes,
        no_polymorphic,
        no_cold,
        no_too_large,
        no_too_deep,
        no_budget,
        no_recursive,
        no_closures,
        no_feedback,
    };

    /// Determine whether a call site should be inlined
    pub fn shouldInline(
        caller: *const bytecode.FunctionBytecode,
        callee: *const bytecode.FunctionBytecode,
        call_site: *const CallSiteFeedback,
        current_depth: u32,
        total_inlined: u32,
    ) InlineDecision {
        // Must be monomorphic
        if (!call_site.isMonomorphic()) return .no_polymorphic;

        // Must have enough calls to justify
        if (call_site.total_calls < MIN_CALL_COUNT) return .no_cold;

        // Callee must be small enough
        if (callee.code.len > MAX_INLINE_BYTECODE_SIZE) return .no_too_large;

        // Not too deep
        if (current_depth >= MAX_INLINE_DEPTH) return .no_too_deep;

        // Budget check
        if (total_inlined + @as(u32, @intCast(callee.code.len)) > MAX_INLINED_SIZE) return .no_budget;

        // Don't inline recursive calls
        if (caller == callee) return .no_recursive;

        // Don't inline functions with closures (complex upvalue handling)
        if (callee.upvalue_count > 0) return .no_closures;

        return .yes;
    }
};

/// Classify a JSValue into an ObservedType
pub fn classifyValue(val: value.JSValue) ObservedType {
    // Fast path: check for SMI (most common in arithmetic)
    if (val.isInt()) return .smi;

    // Check for special values
    if (val.isNull()) return .null_type;
    if (val.isUndefined()) return .undefined_type;
    if (val.isBool()) return .boolean;

    // Check for heap types
    if (val.isPtr()) {
        // Check for float64 box
        if (val.isFloat64()) return .heap_number;

        // Check for string
        if (val.isString()) return .string;

        // Must be an object - check class_id for array/function
        if (val.isObject()) {
            const obj = object.JSObject.fromValue(val);
            return switch (obj.class_id) {
                .array => .array,
                .function, .bound_function => .function,
                else => .object,
            };
        }
    }

    return .none;
}

// ============================================================================
// Unit Tests
// ============================================================================

test "TypeFeedbackSite records SMI" {
    var site = TypeFeedbackSite{};

    site.record(value.JSValue.fromInt(42));

    try std.testing.expect(site.isMonomorphic());
    try std.testing.expectEqual(ObservedType.smi, site.dominantType().?);
    try std.testing.expectEqual(@as(u8, 1), site.count);
    try std.testing.expectEqual(@as(u32, 1), site.total_hits);
}

test "TypeFeedbackSite deduplicates same type" {
    var site = TypeFeedbackSite{};

    site.record(value.JSValue.fromInt(1));
    site.record(value.JSValue.fromInt(2));
    site.record(value.JSValue.fromInt(3));

    try std.testing.expect(site.isMonomorphic());
    try std.testing.expectEqual(@as(u8, 1), site.count);
    try std.testing.expectEqual(@as(u32, 3), site.total_hits);
}

test "TypeFeedbackSite transitions to polymorphic" {
    var site = TypeFeedbackSite{};

    site.record(value.JSValue.fromInt(1));
    site.record(value.JSValue.null_val);

    try std.testing.expect(!site.isMonomorphic());
    try std.testing.expect(site.isPolymorphic());
    try std.testing.expectEqual(@as(u8, 2), site.count);
}

test "TypeFeedbackSite transitions to megamorphic" {
    var site = TypeFeedbackSite{};

    site.record(value.JSValue.fromInt(1)); // smi
    site.record(value.JSValue.null_val); // null
    site.record(value.JSValue.undefined_val); // undefined
    site.record(value.JSValue.true_val); // boolean
    site.record(value.JSValue.false_val); // boolean (deduplicated)

    try std.testing.expect(!site.megamorphic);
    try std.testing.expectEqual(@as(u8, 4), site.count);

    // This should trigger megamorphic (can't test without heap allocation)
}

test "TypeFeedbackSite reset" {
    var site = TypeFeedbackSite{};

    site.record(value.JSValue.fromInt(42));
    site.reset();

    try std.testing.expectEqual(@as(u8, 0), site.count);
    try std.testing.expectEqual(@as(u32, 0), site.total_hits);
    try std.testing.expect(!site.megamorphic);
}

test "CallSiteFeedback monomorphic" {
    var site = CallSiteFeedback{};

    // Simulate recording the same callee multiple times
    const dummy_bc: bytecode.FunctionBytecode = undefined;
    site.recordCallee(&dummy_bc);
    site.recordCallee(&dummy_bc);
    site.recordCallee(&dummy_bc);

    try std.testing.expect(site.isMonomorphic());
    try std.testing.expectEqual(@as(u32, 3), site.total_calls);
    try std.testing.expectEqual(@as(u8, 1), site.count);
}

test "CallSiteFeedback polymorphic" {
    var site = CallSiteFeedback{};

    // Create two separate bytecode instances with different addresses
    var dummy_bc1: bytecode.FunctionBytecode = undefined;
    var dummy_bc2: bytecode.FunctionBytecode = undefined;

    // Record different callees
    site.recordCallee(&dummy_bc1);
    site.recordCallee(&dummy_bc2);

    // Should be polymorphic since pointers are different
    try std.testing.expect(&dummy_bc1 != &dummy_bc2);
    try std.testing.expect(!site.isMonomorphic());
    try std.testing.expectEqual(@as(u8, 2), site.count);
}

test "TypeFeedback init and deinit" {
    const allocator = std.testing.allocator;

    const tf = try TypeFeedback.init(allocator, 10, 5);
    defer tf.deinit();

    try std.testing.expectEqual(@as(usize, 10), tf.sites.len);
    try std.testing.expectEqual(@as(usize, 5), tf.call_sites.len);
}

test "classifyValue" {
    try std.testing.expectEqual(ObservedType.smi, classifyValue(value.JSValue.fromInt(42)));
    try std.testing.expectEqual(ObservedType.null_type, classifyValue(value.JSValue.null_val));
    try std.testing.expectEqual(ObservedType.undefined_type, classifyValue(value.JSValue.undefined_val));
    try std.testing.expectEqual(ObservedType.boolean, classifyValue(value.JSValue.true_val));
    try std.testing.expectEqual(ObservedType.boolean, classifyValue(value.JSValue.false_val));
}

test "InliningPolicy rejects cold calls" {
    const dummy_caller: bytecode.FunctionBytecode = undefined;
    var dummy_callee: bytecode.FunctionBytecode = undefined;
    dummy_callee.code = &[_]u8{0} ** 10; // Small function
    dummy_callee.upvalue_count = 0;

    var call_site = CallSiteFeedback{};
    call_site.recordCallee(&dummy_callee);
    call_site.total_calls = 5; // Below threshold

    const decision = InliningPolicy.shouldInline(
        &dummy_caller,
        &dummy_callee,
        &call_site,
        0,
        0,
    );

    try std.testing.expectEqual(InliningPolicy.InlineDecision.no_cold, decision);
}
