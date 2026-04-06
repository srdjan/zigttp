//! Type Feedback Collection for JIT Optimization
//!
//! Records runtime type information at hot code sites to enable
//! speculative optimization in the baseline JIT compiler.

const std = @import("std");
const value = @import("value.zig");
const object = @import("object.zig");
const bytecode = @import("bytecode.zig");

const debug_log = false; // Set to true for type feedback diagnostics

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
        self.total_hits +|= 1;

        // Skip recording if already megamorphic - no point checking types
        if (self.megamorphic) return;

        const observed_type = classifyValue(val);

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
        self.total_calls +|= 1;

        // Skip recording if already megamorphic - no point checking callees
        if (self.megamorphic) return;

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

    /// Get total hits across all feedback sites
    pub fn totalHits(self: *const TypeFeedback) u32 {
        var total: u32 = 0;
        for (self.sites) |site| {
            total +|= site.total_hits;
        }
        for (self.call_sites) |site| {
            total +|= site.total_calls;
        }
        return total;
    }
};

/// Inlining policy for function inlining decisions
pub const InliningPolicy = struct {
    /// Maximum bytecode size of a function to inline (increased for small utility functions)
    pub const MAX_INLINE_BYTECODE_SIZE: u32 = 64;
    /// Maximum inlining depth
    pub const MAX_INLINE_DEPTH: u32 = 3;
    /// Minimum call count before considering inlining (lower for faster FaaS warmup)
    pub const MIN_CALL_COUNT: u32 = 5;
    /// Reduced call count threshold for hot callees (execution_count > HOT_CALLEE_THRESHOLD)
    pub const MIN_CALL_COUNT_HOT: u32 = 2;
    /// Threshold for considering a callee "hot" based on its execution count
    pub const HOT_CALLEE_THRESHOLD: u32 = 1000;
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

        // Must have enough calls to justify inlining
        // Use lower threshold for hot callees (already proven to be frequently executed)
        const min_calls = if (callee.execution_count > HOT_CALLEE_THRESHOLD)
            MIN_CALL_COUNT_HOT
        else
            MIN_CALL_COUNT;
        if (call_site.total_calls < min_calls) return .no_cold;

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
// Loop Analysis for Optimized JIT Tier
// ============================================================================

/// Maximum number of binary operation sites to track per loop
pub const MAX_LOOP_BINARY_OPS: usize = 32;

/// Loop information for optimized tier compilation
/// Identifies loops suitable for type-specialized code generation
pub const LoopInfo = struct {
    /// Bytecode offset of loop header (target of backward jump)
    header_bc_offset: u32,
    /// Bytecode offset of back edge (the loop opcode)
    back_edge_bc_offset: u32,
    /// Type feedback site indices for binary ops within the loop
    binary_op_sites: [MAX_LOOP_BINARY_OPS]u16,
    /// Number of binary op sites recorded
    binary_op_count: u8,
    /// Local variables accessed in the loop (bitmask, up to 16 locals)
    accessed_locals: u16,
    /// True if all binary op sites in this loop are monomorphic SMI
    all_smi: bool,
    /// True if loop contains operations that prevent optimization
    /// (e.g., function calls, property access, closures)
    has_side_effects: bool,

    pub fn init(header: u32, back_edge: u32) LoopInfo {
        return .{
            .header_bc_offset = header,
            .back_edge_bc_offset = back_edge,
            .binary_op_sites = .{0xFFFF} ** MAX_LOOP_BINARY_OPS,
            .binary_op_count = 0,
            .accessed_locals = 0,
            .all_smi = true,
            .has_side_effects = false,
        };
    }

    /// Add a binary operation site to this loop
    pub fn addBinaryOpSite(self: *LoopInfo, site_idx: u16) void {
        if (self.binary_op_count < MAX_LOOP_BINARY_OPS) {
            self.binary_op_sites[self.binary_op_count] = site_idx;
            self.binary_op_count += 1;
        }
    }

    /// Mark a local variable as accessed in this loop
    pub fn markLocalAccessed(self: *LoopInfo, local_idx: u8) void {
        if (local_idx < 16) {
            self.accessed_locals |= @as(u16, 1) << @intCast(local_idx);
        }
    }

    /// Check if a local variable is accessed in this loop
    pub fn isLocalAccessed(self: *const LoopInfo, local_idx: u8) bool {
        if (local_idx >= 16) return false;
        return (self.accessed_locals & (@as(u16, 1) << @intCast(local_idx))) != 0;
    }

    /// Get number of locals accessed in this loop
    pub fn accessedLocalCount(self: *const LoopInfo) u8 {
        return @popCount(self.accessed_locals);
    }
};

/// Analyze a loop to determine if it's suitable for optimized compilation.
/// Returns true if the loop contains only monomorphic SMI operations and
/// no side effects that would prevent type specialization.
pub fn analyzeLoopTypes(
    tf: *const TypeFeedback,
    site_map: []const u16,
    loop: *LoopInfo,
) bool {
    // Check all binary operation sites in the loop
    for (loop.binary_op_sites[0..loop.binary_op_count]) |site_idx| {
        if (site_idx == 0xFFFF) continue;
        if (site_idx >= tf.sites.len) {
            if (comptime debug_log) std.debug.print("[TF] analyzeLoopTypes: site_idx={} >= sites.len={}\n", .{ site_idx, tf.sites.len });
            loop.all_smi = false;
            return false;
        }

        const site = &tf.sites[site_idx];

        // Must be monomorphic SMI
        if (!site.isMonomorphic()) {
            if (comptime debug_log) std.debug.print("[TF] analyzeLoopTypes: site {} not monomorphic (count={} hits={})\n", .{
                site_idx,
                site.count,
                site.total_hits,
            });
            loop.all_smi = false;
            return false;
        }
        const dom_type = site.dominantType() orelse {
            if (comptime debug_log) std.debug.print("[TF] analyzeLoopTypes: site {} no dominant type\n", .{site_idx});
            loop.all_smi = false;
            return false;
        };
        if (dom_type != .smi) {
            if (comptime debug_log) std.debug.print("[TF] analyzeLoopTypes: site {} not SMI (type={s})\n", .{
                site_idx,
                @tagName(dom_type),
            });
            loop.all_smi = false;
            return false;
        }
    }

    // Also check right operand sites (site_idx + 1 for binary ops)
    for (loop.binary_op_sites[0..loop.binary_op_count]) |site_idx| {
        if (site_idx == 0xFFFF) continue;
        const right_idx = site_idx + 1;
        if (right_idx >= tf.sites.len) {
            loop.all_smi = false;
            return false;
        }

        const right_site = &tf.sites[right_idx];
        if (!right_site.isMonomorphic()) {
            loop.all_smi = false;
            return false;
        }
        if (right_site.dominantType() != .smi) {
            loop.all_smi = false;
            return false;
        }
    }

    _ = site_map;
    return loop.all_smi and !loop.has_side_effects;
}

/// Check if a loop is suitable for optimized tier compilation
pub fn isLoopOptimizable(loop: *const LoopInfo) bool {
    // Must have at least one binary op to optimize
    if (loop.binary_op_count == 0) return false;

    // All operations must be monomorphic SMI
    if (!loop.all_smi) return false;

    // Must not have side effects
    if (loop.has_side_effects) return false;

    return true;
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
    dummy_callee.execution_count = 0; // Cold callee - uses standard MIN_CALL_COUNT threshold

    var call_site = CallSiteFeedback{};
    call_site.recordCallee(&dummy_callee);
    call_site.total_calls = 4; // Below threshold (MIN_CALL_COUNT = 5)

    const decision = InliningPolicy.shouldInline(
        &dummy_caller,
        &dummy_callee,
        &call_site,
        0,
        0,
    );

    try std.testing.expectEqual(InliningPolicy.InlineDecision.no_cold, decision);
}

test "InliningPolicy uses lower threshold for hot callees" {
    const dummy_caller: bytecode.FunctionBytecode = undefined;
    var dummy_callee: bytecode.FunctionBytecode = undefined;
    dummy_callee.code = &[_]u8{0} ** 10; // Small function
    dummy_callee.upvalue_count = 0;
    dummy_callee.execution_count = InliningPolicy.HOT_CALLEE_THRESHOLD + 1; // Hot callee

    var call_site = CallSiteFeedback{};
    call_site.recordCallee(&dummy_callee);
    call_site.total_calls = InliningPolicy.MIN_CALL_COUNT_HOT; // At reduced threshold

    const decision = InliningPolicy.shouldInline(
        &dummy_caller,
        &dummy_callee,
        &call_site,
        0,
        0,
    );

    // Should pass because hot callees use MIN_CALL_COUNT_HOT (2) threshold
    try std.testing.expectEqual(InliningPolicy.InlineDecision.yes, decision);
}
