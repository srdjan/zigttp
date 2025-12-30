//! Scope Analysis for JavaScript
//!
//! Tracks variable bindings across scopes and detects closures (upvalues).
//! This is the key component that enables proper closure support.

const std = @import("std");
const ir = @import("ir.zig");

pub const ScopeId = ir.ScopeId;
pub const null_scope = ir.null_scope;
pub const BindingRef = ir.BindingRef;

/// A variable binding within a scope
pub const Binding = struct {
    name: []const u8,
    name_atom: u16,
    slot: u8,
    kind: BindingKind,
    is_const: bool,
    is_captured: bool, // Set when captured by an inner function

    pub const BindingKind = enum {
        variable, // var/let/const
        parameter, // Function parameter
        function, // Function declaration (hoisted)
        catch_param, // catch(e) parameter
        class, // Class declaration
        import, // Import binding
    };
};

/// Upvalue - a variable captured from an outer function scope
pub const Upvalue = struct {
    /// Reference to the binding in the outer scope
    outer_scope_id: ScopeId,
    outer_slot: u8,

    /// Slot in this function's upvalue array
    slot: u8,

    /// true: captured from immediate parent's locals
    /// false: captured from parent's upvalues (transitive)
    is_direct: bool,

    /// Name for debugging
    name: []const u8,
};

/// Scope kind determines variable behavior
pub const ScopeKind = enum {
    global, // Global scope
    function, // Function scope (has own 'this', arguments)
    block, // Block scope (let/const are block-scoped)
    for_loop, // For loop initializer scope
    catch_block, // Catch clause scope
    class_body, // Class body scope
    module, // ES module scope
};

/// A single scope in the scope tree
pub const Scope = struct {
    id: ScopeId,
    parent: ?ScopeId,
    kind: ScopeKind,

    /// Bindings declared in this scope
    bindings: std.ArrayList(Binding),

    /// Upvalues captured by this function (only for function scopes)
    upvalues: std.ArrayList(Upvalue),

    /// The function scope this scope belongs to (for finding upvalues)
    enclosing_function: ScopeId,

    /// Maximum local slot used (for bytecode local_count)
    max_local_count: u8,

    /// Current local slot for next declaration
    current_local_slot: u8,

    pub fn init(id: ScopeId, parent: ?ScopeId, kind: ScopeKind, enclosing_fn: ScopeId) Scope {
        return .{
            .id = id,
            .parent = parent,
            .kind = kind,
            .bindings = .empty,
            .upvalues = .empty,
            .enclosing_function = enclosing_fn,
            .max_local_count = 0,
            .current_local_slot = 0,
        };
    }

    pub fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
        self.bindings.deinit(allocator);
        self.upvalues.deinit(allocator);
    }

    /// Find a binding by name in this scope only
    pub fn findLocal(self: *const Scope, name: []const u8) ?*const Binding {
        for (self.bindings.items) |*binding| {
            if (std.mem.eql(u8, binding.name, name)) {
                return binding;
            }
        }
        return null;
    }

    /// Find a mutable binding by name
    pub fn findLocalMut(self: *Scope, name: []const u8) ?*Binding {
        for (self.bindings.items) |*binding| {
            if (std.mem.eql(u8, binding.name, name)) {
                return binding;
            }
        }
        return null;
    }

    /// Check if this is a function scope
    pub fn isFunction(self: *const Scope) bool {
        return self.kind == .function;
    }
};

/// The scope analyzer - manages the scope tree and variable resolution
pub const ScopeAnalyzer = struct {
    allocator: std.mem.Allocator,
    scopes: std.ArrayList(Scope),
    current_scope: ScopeId,
    next_scope_id: ScopeId,

    pub fn init(allocator: std.mem.Allocator) ScopeAnalyzer {
        var analyzer = ScopeAnalyzer{
            .allocator = allocator,
            .scopes = .empty,
            .current_scope = 0,
            .next_scope_id = 0,
        };

        // Create global scope
        _ = analyzer.pushScopeInternal(.global, 0) catch unreachable;

        return analyzer;
    }

    pub fn deinit(self: *ScopeAnalyzer) void {
        for (self.scopes.items) |*scope| {
            scope.deinit(self.allocator);
        }
        self.scopes.deinit(self.allocator);
    }

    /// Push a new scope onto the stack
    pub fn pushScope(self: *ScopeAnalyzer, kind: ScopeKind) !ScopeId {
        const enclosing_fn = if (kind == .function)
            self.next_scope_id
        else
            self.scopes.items[self.current_scope].enclosing_function;

        return try self.pushScopeInternal(kind, enclosing_fn);
    }

    fn pushScopeInternal(self: *ScopeAnalyzer, kind: ScopeKind, enclosing_fn: ScopeId) !ScopeId {
        const id = self.next_scope_id;
        self.next_scope_id += 1;

        const parent: ?ScopeId = if (self.scopes.items.len > 0) self.current_scope else null;

        // Inherit local slot from parent (for block scopes in same function)
        var initial_slot: u8 = 0;
        if (parent) |p| {
            const parent_scope = &self.scopes.items[p];
            if (kind != .function and parent_scope.enclosing_function == enclosing_fn) {
                initial_slot = parent_scope.current_local_slot;
            }
        }

        var scope = Scope.init(id, parent, kind, enclosing_fn);
        scope.current_local_slot = initial_slot;

        try self.scopes.append(self.allocator, scope);
        self.current_scope = id;

        return id;
    }

    /// Pop the current scope
    pub fn popScope(self: *ScopeAnalyzer) void {
        const scope = &self.scopes.items[self.current_scope];

        // Update parent's max_local_count if needed
        if (scope.parent) |parent_id| {
            const parent = &self.scopes.items[parent_id];
            if (scope.enclosing_function == parent.enclosing_function) {
                // Same function - propagate max
                if (scope.max_local_count > parent.max_local_count) {
                    parent.max_local_count = scope.max_local_count;
                }
            }
            self.current_scope = parent_id;
        }
    }

    /// Declare a new binding in the current scope
    pub fn declareBinding(
        self: *ScopeAnalyzer,
        name: []const u8,
        name_atom: u16,
        kind: Binding.BindingKind,
        is_const: bool,
    ) !BindingRef {
        const scope = &self.scopes.items[self.current_scope];
        const slot = scope.current_local_slot;

        // Check for 255 local limit
        if (slot >= 255) {
            return error.TooManyLocals;
        }

        try scope.bindings.append(self.allocator, .{
            .name = name,
            .name_atom = name_atom,
            .slot = slot,
            .kind = kind,
            .is_const = is_const,
            .is_captured = false,
        });

        scope.current_local_slot = slot + 1;
        if (scope.current_local_slot > scope.max_local_count) {
            scope.max_local_count = scope.current_local_slot;
        }

        return .{
            .scope_id = self.current_scope,
            .slot = slot,
            .kind = if (kind == .parameter) .argument else .local,
        };
    }

    /// Resolve a variable name, potentially creating upvalues
    pub fn resolveBinding(self: *ScopeAnalyzer, name: []const u8, name_atom: u16) BindingRef {
        var scope_id = self.current_scope;
        var crossed_function = false;
        var current_function_scope: ScopeId = self.scopes.items[scope_id].enclosing_function;

        while (true) {
            const scope = &self.scopes.items[scope_id];

            // Look for binding in current scope
            if (scope.findLocalMut(name)) |binding| {
                if (crossed_function) {
                    // We need to create an upvalue chain
                    binding.is_captured = true;
                    const upvalue_slot = self.createUpvalueChain(
                        current_function_scope,
                        scope_id,
                        binding.slot,
                        name,
                    ) catch return .{ .scope_id = 0, .slot = 0, .kind = .global };

                    return .{
                        .scope_id = current_function_scope,
                        .slot = upvalue_slot,
                        .kind = .upvalue,
                    };
                } else {
                    // Same function - direct local access
                    return .{
                        .scope_id = scope_id,
                        .slot = binding.slot,
                        .kind = if (binding.kind == .parameter) .argument else .local,
                    };
                }
            }

            // Track if we crossed a function boundary
            if (scope.kind == .function) {
                crossed_function = true;
                current_function_scope = scope_id;
            }

            // Move to parent scope
            if (scope.parent) |parent| {
                scope_id = parent;
            } else {
                // Not found - it's a global
                return .{
                    .scope_id = 0,
                    .slot = @truncate(name_atom), // Atom index stored in slot (for globals)
                    .kind = .global,
                };
            }
        }
    }

    /// Create upvalue chain from inner function to captured variable
    fn createUpvalueChain(
        self: *ScopeAnalyzer,
        inner_function_scope: ScopeId,
        outer_scope: ScopeId,
        outer_slot: u8,
        name: []const u8,
    ) !u8 {
        const inner_scope = &self.scopes.items[inner_function_scope];
        const outer = &self.scopes.items[outer_scope];

        // Check if the outer scope is a direct parent function
        const is_direct = inner_scope.parent != null and
            self.scopes.items[inner_scope.parent.?].enclosing_function == outer.enclosing_function;

        // Check if upvalue already exists
        for (inner_scope.upvalues.items, 0..) |uv, i| {
            if (uv.outer_scope_id == outer_scope and uv.outer_slot == outer_slot) {
                return @intCast(i);
            }
        }

        // Create new upvalue
        const slot: u8 = @intCast(inner_scope.upvalues.items.len);
        if (slot >= 255) return error.TooManyUpvalues;

        try inner_scope.upvalues.append(self.allocator, .{
            .outer_scope_id = outer_scope,
            .outer_slot = outer_slot,
            .slot = slot,
            .is_direct = is_direct,
            .name = name,
        });

        return slot;
    }

    /// Check if a name is already declared in current scope
    pub fn isDeclaredInCurrentScope(self: *ScopeAnalyzer, name: []const u8) bool {
        const scope = &self.scopes.items[self.current_scope];
        return scope.findLocal(name) != null;
    }

    /// Get the current scope
    pub fn getCurrentScope(self: *ScopeAnalyzer) *Scope {
        return &self.scopes.items[self.current_scope];
    }

    /// Get a scope by ID
    pub fn getScope(self: *ScopeAnalyzer, id: ScopeId) *Scope {
        return &self.scopes.items[id];
    }

    /// Get the enclosing function scope
    pub fn getEnclosingFunction(self: *ScopeAnalyzer) *Scope {
        const current = &self.scopes.items[self.current_scope];
        return &self.scopes.items[current.enclosing_function];
    }

    /// Get upvalues for a function scope
    pub fn getUpvalues(self: *const ScopeAnalyzer, function_scope: ScopeId) []const Upvalue {
        return self.scopes.items[function_scope].upvalues.items;
    }

    /// Get local count for a function scope
    pub fn getLocalCount(self: *const ScopeAnalyzer, function_scope: ScopeId) u8 {
        return self.scopes.items[function_scope].max_local_count;
    }

    /// Check if we're currently in a function (not global)
    pub fn inFunction(self: *ScopeAnalyzer) bool {
        const current = &self.scopes.items[self.current_scope];
        return current.enclosing_function != 0;
    }

    /// Check if we're in a loop (for break/continue validation)
    pub fn inLoop(self: *ScopeAnalyzer) bool {
        var scope_id = self.current_scope;
        while (true) {
            const scope = &self.scopes.items[scope_id];
            if (scope.kind == .for_loop) return true;
            if (scope.kind == .function) return false; // Don't look past function boundary
            if (scope.parent) |p| {
                scope_id = p;
            } else {
                return false;
            }
        }
    }
};

/// Upvalue info for bytecode generation
pub const UpvalueInfo = struct {
    is_local: bool, // true: from parent's locals, false: from parent's upvalues
    index: u8,
};

/// Convert upvalues to bytecode format
pub fn getUpvalueInfo(analyzer: *const ScopeAnalyzer, function_scope: ScopeId) []const Upvalue {
    return analyzer.scopes.items[function_scope].upvalues.items;
}

// --- Tests ---

test "basic scope and binding" {
    var analyzer = ScopeAnalyzer.init(std.testing.allocator);
    defer analyzer.deinit();

    // Declare in global scope
    const x_ref = try analyzer.declareBinding("x", 1, .variable, false);
    try std.testing.expectEqual(BindingRef.BindingKind.local, x_ref.kind);
    try std.testing.expectEqual(@as(u8, 0), x_ref.slot);

    // Resolve x - should find it
    const resolved = analyzer.resolveBinding("x", 1);
    try std.testing.expectEqual(BindingRef.BindingKind.local, resolved.kind);
    try std.testing.expectEqual(@as(u8, 0), resolved.slot);

    // Resolve undefined - should be global
    const undefined_ref = analyzer.resolveBinding("undefined", 2);
    try std.testing.expectEqual(BindingRef.BindingKind.global, undefined_ref.kind);
}

test "nested function creates upvalue" {
    var analyzer = ScopeAnalyzer.init(std.testing.allocator);
    defer analyzer.deinit();

    // Global scope: declare x
    _ = try analyzer.declareBinding("x", 1, .variable, false);

    // Enter outer function
    _ = try analyzer.pushScope(.function);
    const outer_y = try analyzer.declareBinding("y", 2, .variable, false);
    try std.testing.expectEqual(@as(u8, 0), outer_y.slot);

    // Enter inner function
    const inner_scope_id = try analyzer.pushScope(.function);

    // Resolve y from inner function - should create upvalue
    const y_ref = analyzer.resolveBinding("y", 2);
    try std.testing.expectEqual(BindingRef.BindingKind.upvalue, y_ref.kind);
    try std.testing.expectEqual(@as(u8, 0), y_ref.slot);

    // Check upvalue was created
    const upvalues = analyzer.getUpvalues(inner_scope_id);
    try std.testing.expectEqual(@as(usize, 1), upvalues.len);
    try std.testing.expectEqualStrings("y", upvalues[0].name);
}

test "block scope inherits local slots" {
    var analyzer = ScopeAnalyzer.init(std.testing.allocator);
    defer analyzer.deinit();

    // Enter function
    _ = try analyzer.pushScope(.function);
    _ = try analyzer.declareBinding("a", 1, .variable, false); // slot 0

    // Enter block
    _ = try analyzer.pushScope(.block);
    const b = try analyzer.declareBinding("b", 2, .variable, false); // slot 1

    try std.testing.expectEqual(@as(u8, 1), b.slot);

    // Leave block and enter another
    analyzer.popScope();
    _ = try analyzer.pushScope(.block);
    const c = try analyzer.declareBinding("c", 3, .variable, false);

    // c reuses slot 1 since b is out of scope
    try std.testing.expectEqual(@as(u8, 1), c.slot);
}

test "function parameters" {
    var analyzer = ScopeAnalyzer.init(std.testing.allocator);
    defer analyzer.deinit();

    // Enter function
    _ = try analyzer.pushScope(.function);

    // Declare parameters
    const param_a = try analyzer.declareBinding("a", 1, .parameter, false);
    const param_b = try analyzer.declareBinding("b", 2, .parameter, false);

    try std.testing.expectEqual(BindingRef.BindingKind.argument, param_a.kind);
    try std.testing.expectEqual(@as(u8, 0), param_a.slot);
    try std.testing.expectEqual(BindingRef.BindingKind.argument, param_b.kind);
    try std.testing.expectEqual(@as(u8, 1), param_b.slot);

    // Resolve parameter
    const resolved = analyzer.resolveBinding("a", 1);
    try std.testing.expectEqual(BindingRef.BindingKind.argument, resolved.kind);
}
