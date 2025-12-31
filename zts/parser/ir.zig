//! Intermediate Representation for JavaScript/JSX
//!
//! A lightweight IR that captures scope information for proper closure support.
//! Expressions are tree-structured; statements are linearized.

const std = @import("std");
const token = @import("token.zig");

pub const SourceLocation = token.SourceLocation;

/// Index into the node array
pub const NodeIndex = u32;

/// Sentinel value for "no node"
pub const null_node: NodeIndex = std.math.maxInt(NodeIndex);

/// Scope identifier
pub const ScopeId = u16;

/// Sentinel value for "no scope"
pub const null_scope: ScopeId = std.math.maxInt(ScopeId);

/// Reference to a variable binding
pub const BindingRef = struct {
    scope_id: ScopeId,
    slot: u16, // Changed from u8 to support atom indices > 255
    kind: BindingKind,

    pub const BindingKind = enum(u2) {
        local, // Local variable in current function
        upvalue, // Captured from outer scope
        global, // Global variable
        argument, // Function parameter
    };

    /// Create a global reference (slot is atom index)
    pub fn global(atom_idx: u16) BindingRef {
        return .{
            .scope_id = 0,
            .slot = atom_idx, // No truncation needed now
            .kind = .global,
        };
    }
};

/// Binary operator types
pub const BinaryOp = enum(u8) {
    // Arithmetic
    add,
    sub,
    mul,
    div,
    mod,
    pow,

    // Comparison
    eq,
    neq,
    strict_eq,
    strict_neq,
    lt,
    lte,
    gt,
    gte,

    // Bitwise
    bit_and,
    bit_or,
    bit_xor,
    shl,
    shr,
    ushr,

    // Logical (short-circuit)
    and_op,
    or_op,
    nullish,

    // Other
    instanceof,
    in_op,
};

/// Unary operator types
pub const UnaryOp = enum(u4) {
    neg,
    not,
    bit_not,
    typeof_op,
    void_op,
    delete_op,
};

/// Function flags
pub const FunctionFlags = packed struct {
    is_generator: bool = false,
    is_async: bool = false,
    is_arrow: bool = false,
    has_rest_param: bool = false,
    has_default_params: bool = false,
    is_method: bool = false,
    is_getter: bool = false,
    is_setter: bool = false,
};

/// IR Node tag - determines which payload union field is active
pub const NodeTag = enum(u8) {
    // Literals
    lit_int,
    lit_float,
    lit_string,
    lit_bool,
    lit_null,
    lit_undefined,
    lit_regex,

    // Identifiers and references
    identifier,

    // Expressions
    binary_op,
    unary_op,
    ternary,
    call,
    method_call,
    member_access,
    computed_access,
    optional_chain,
    optional_call,
    assignment,
    array_literal,
    object_literal,
    object_property,
    object_method,
    object_getter,
    object_setter,
    object_spread,
    function_expr,
    arrow_function,
    template_literal,
    template_part_string,
    template_part_expr,
    spread,
    await_expr,
    sequence_expr,
    comma_expr,

    // Statements
    expr_stmt,
    var_decl,
    if_stmt,
    for_stmt,
    for_of_stmt,
    switch_stmt,
    case_clause,
    return_stmt,
    block,
    empty_stmt,
    labeled_stmt,
    debugger_stmt,

    // Declarations
    function_decl,

    // Patterns (for destructuring)
    array_pattern,
    object_pattern,
    pattern_element,
    pattern_rest,
    pattern_default,

    // JSX
    jsx_element,
    jsx_fragment,
    jsx_text,
    jsx_expr_container,
    jsx_attribute,
    jsx_spread_attribute,

    // Module
    import_decl,
    import_specifier,
    import_default,
    import_namespace,
    export_decl,
    export_specifier,
    export_default,
    export_all,

    // Internal markers
    program,
    param_list,
    arg_list,
    stmt_list,
};

/// IR Node - tagged union with source location
pub const Node = struct {
    tag: NodeTag,
    loc: SourceLocation,
    data: Data,

    pub const Data = union {
        // Literals
        int_value: i32,
        float_idx: u16, // Index into float constant pool
        string_idx: u16, // Index into string constant pool
        bool_value: bool,
        regex: RegexData,

        // Identifier/binding reference
        binding: BindingRef,

        // Binary expression
        binary: BinaryExpr,

        // Unary expression
        unary: UnaryExpr,

        // Ternary expression
        ternary: TernaryExpr,

        // Function call
        call: CallExpr,

        // Property access
        member: MemberExpr,

        // Assignment
        assignment: AssignExpr,

        // Array literal
        array: ArrayExpr,

        // Object literal
        object: ObjectExpr,

        // Object property (key-value)
        property: PropertyExpr,

        // Function expression
        function: FunctionExpr,

        // Template literal
        template: TemplateExpr,

        // Variable declaration
        var_decl: VarDecl,

        // If statement
        if_stmt: IfStmt,

        // Loop statements
        loop: LoopStmt,

        // For-of/for-in
        for_iter: ForIterStmt,

        // Switch statement
        switch_stmt: SwitchStmt,

        // Case clause
        case_clause: CaseClause,

        // Return/throw with optional value
        opt_value: ?NodeIndex,

        // Break/continue with optional label
        opt_label: ?u16, // atom index of label

        // Try-catch-finally
        try_stmt: TryStmt,

        // Block/program/list
        block: BlockData,

        // JSX element
        jsx_element: JsxElement,

        // JSX attribute
        jsx_attr: JsxAttr,

        // JSX text
        jsx_text: u16, // string constant index

        // Import declaration
        import_decl: ImportDecl,

        // Import specifier
        import_spec: ImportSpec,

        // Export declaration
        export_decl: ExportDecl,

        // Class declaration
        class_decl: ClassDecl,

        // Class method/field
        class_member: ClassMember,

        // Pattern element
        pattern_elem: PatternElem,

        // Labeled statement
        labeled: LabeledStmt,

        // No data needed
        none: void,
    };

    // --- Nested data structures ---

    pub const RegexData = struct {
        pattern_idx: u16, // String constant index for pattern
        flags_idx: u16, // String constant index for flags
    };

    pub const BinaryExpr = struct {
        op: BinaryOp,
        left: NodeIndex,
        right: NodeIndex,
    };

    pub const UnaryExpr = struct {
        op: UnaryOp,
        operand: NodeIndex,
    };

    pub const TernaryExpr = struct {
        condition: NodeIndex,
        then_branch: NodeIndex,
        else_branch: NodeIndex,
    };

    pub const CallExpr = struct {
        callee: NodeIndex,
        args_start: NodeIndex, // First argument node
        args_count: u8,
        is_optional: bool, // foo?.()
    };

    pub const MemberExpr = struct {
        object: NodeIndex,
        property: u16, // Atom index for .property access
        computed: NodeIndex, // For [expr] access, null_node if not computed
        is_optional: bool, // obj?.prop
    };

    pub const AssignExpr = struct {
        target: NodeIndex,
        value: NodeIndex,
        op: ?BinaryOp, // null for =, set for +=, etc.
    };

    pub const ArrayExpr = struct {
        elements_start: NodeIndex,
        elements_count: u16,
        has_spread: bool,
    };

    pub const ObjectExpr = struct {
        properties_start: NodeIndex,
        properties_count: u16,
    };

    pub const PropertyExpr = struct {
        key: NodeIndex, // Can be identifier, string, or computed
        value: NodeIndex,
        is_computed: bool,
        is_shorthand: bool, // { x } === { x: x }
    };

    pub const FunctionExpr = struct {
        scope_id: ScopeId,
        name_atom: u16, // 0 for anonymous
        params_start: NodeIndex, // First parameter node
        params_count: u8,
        body: NodeIndex,
        flags: FunctionFlags,
    };

    pub const TemplateExpr = struct {
        parts_start: NodeIndex, // Interleaved string/expr nodes
        parts_count: u8,
        tag: NodeIndex, // Tagged template function, null_node if none
    };

    pub const VarDecl = struct {
        binding: BindingRef,
        pattern: NodeIndex, // For destructuring, null_node for simple
        init: NodeIndex, // Initializer, null_node if none
        kind: VarKind,

        pub const VarKind = enum(u2) { @"var", let, @"const" };
    };

    pub const IfStmt = struct {
        condition: NodeIndex,
        then_branch: NodeIndex,
        else_branch: NodeIndex, // null_node if no else
    };

    pub const LoopStmt = struct {
        kind: LoopKind,
        init: NodeIndex, // For for-loop, null_node otherwise
        condition: NodeIndex, // null_node for unconditional
        update: NodeIndex, // For for-loop, null_node otherwise
        body: NodeIndex,

        pub const LoopKind = enum(u2) { while_loop, do_while, for_loop };
    };

    pub const ForIterStmt = struct {
        is_for_in: bool, // false = for-of
        binding: BindingRef,
        pattern: NodeIndex, // For destructuring, null_node for simple
        iterable: NodeIndex,
        body: NodeIndex,
        is_const: bool, // const vs let/var
    };

    pub const SwitchStmt = struct {
        discriminant: NodeIndex,
        cases_start: NodeIndex, // First case clause
        cases_count: u8,
    };

    pub const CaseClause = struct {
        test_expr: NodeIndex, // null_node for default
        body_start: NodeIndex,
        body_count: u16,
    };

    pub const TryStmt = struct {
        try_block: NodeIndex,
        catch_binding: BindingRef, // slot=255 means no binding (catch {})
        catch_block: NodeIndex, // null_node if no catch
        finally_block: NodeIndex, // null_node if no finally
    };

    pub const BlockData = struct {
        stmts_start: NodeIndex,
        stmts_count: u16,
        scope_id: ScopeId, // null_scope if block doesn't create scope
    };

    pub const JsxElement = struct {
        tag_atom: u16, // 0 for fragment
        is_component: bool, // Uppercase = component reference
        props_start: NodeIndex,
        props_count: u8,
        children_start: NodeIndex,
        children_count: u8,
        self_closing: bool,
    };

    pub const JsxAttr = struct {
        name_atom: u16,
        value: NodeIndex, // null_node for boolean attrs like `disabled`
        is_spread: bool, // {...props}
    };

    pub const ImportDecl = struct {
        module_idx: u16, // String constant index for module path
        specifiers_start: NodeIndex,
        specifiers_count: u8,
    };

    pub const ImportSpec = struct {
        kind: ImportKind,
        imported_atom: u16, // Original name in source module
        local_binding: BindingRef, // Local binding

        pub const ImportKind = enum(u2) { named, default, namespace };
    };

    pub const ExportDecl = struct {
        kind: ExportKind,
        declaration: NodeIndex, // The exported declaration, or null_node
        specifiers_start: NodeIndex,
        specifiers_count: u8,
        from_module_idx: u16, // For re-exports, 0 if not re-export

        pub const ExportKind = enum(u2) { named, default, all };
    };

    pub const ClassDecl = struct {
        scope_id: ScopeId,
        name_atom: u16, // 0 for anonymous class expressions
        super_class: NodeIndex, // null_node if no extends
        members_start: NodeIndex,
        members_count: u8,
        binding: BindingRef, // Where the class is bound
    };

    pub const ClassMember = struct {
        key: NodeIndex,
        value: NodeIndex, // Method body or field initializer
        kind: MemberKind,
        is_static: bool,
        is_computed: bool,

        pub const MemberKind = enum(u2) { method, getter, setter, field };
    };

    pub const PatternElem = struct {
        kind: PatternKind,
        binding: BindingRef, // Final binding slot
        key: NodeIndex, // For nested patterns, points to the nested pattern
        key_atom: u16, // For object pattern: property name atom (for get_field)
        default_value: NodeIndex, // null_node if no default

        pub const PatternKind = enum(u2) { simple, array, object, rest };
    };

    pub const LabeledStmt = struct {
        label_atom: u16,
        body: NodeIndex,
    };

    /// Create a literal integer node
    pub fn litInt(loc: SourceLocation, value: i32) Node {
        return .{
            .tag = .lit_int,
            .loc = loc,
            .data = .{ .int_value = value },
        };
    }

    /// Create a literal string node
    pub fn litString(loc: SourceLocation, str_idx: u16) Node {
        return .{
            .tag = .lit_string,
            .loc = loc,
            .data = .{ .string_idx = str_idx },
        };
    }

    /// Create a literal boolean node
    pub fn litBool(loc: SourceLocation, value: bool) Node {
        return .{
            .tag = .lit_bool,
            .loc = loc,
            .data = .{ .bool_value = value },
        };
    }

    /// Create a null literal node
    pub fn litNull(loc: SourceLocation) Node {
        return .{
            .tag = .lit_null,
            .loc = loc,
            .data = .{ .none = {} },
        };
    }

    /// Create an undefined literal node
    pub fn litUndefined(loc: SourceLocation) Node {
        return .{
            .tag = .lit_undefined,
            .loc = loc,
            .data = .{ .none = {} },
        };
    }

    /// Create an identifier reference node
    pub fn identifier(loc: SourceLocation, binding: BindingRef) Node {
        return .{
            .tag = .identifier,
            .loc = loc,
            .data = .{ .binding = binding },
        };
    }

    /// Create a this expression node
    pub fn thisExpr(loc: SourceLocation) Node {
        return .{
            .tag = .this_expr,
            .loc = loc,
            .data = .{ .none = {} },
        };
    }

    /// Create a binary expression node
    pub fn binaryOp(loc: SourceLocation, op: BinaryOp, left: NodeIndex, right: NodeIndex) Node {
        return .{
            .tag = .binary_op,
            .loc = loc,
            .data = .{ .binary = .{ .op = op, .left = left, .right = right } },
        };
    }

    /// Create a unary expression node
    pub fn unaryOp(loc: SourceLocation, op: UnaryOp, operand: NodeIndex) Node {
        return .{
            .tag = .unary_op,
            .loc = loc,
            .data = .{ .unary = .{ .op = op, .operand = operand } },
        };
    }
};

/// Node storage with arena-style allocation
pub const NodeList = struct {
    nodes: std.ArrayList(Node),
    allocator: std.mem.Allocator,
    // Storage for node index lists (statements, arguments, etc.)
    // Lists are stored contiguously, start position is returned when adding
    index_lists: std.ArrayList(NodeIndex),

    pub fn init(allocator: std.mem.Allocator) NodeList {
        return .{
            .nodes = .empty,
            .allocator = allocator,
            .index_lists = .empty,
        };
    }

    pub fn deinit(self: *NodeList) void {
        self.nodes.deinit(self.allocator);
        self.index_lists.deinit(self.allocator);
    }

    /// Add a node and return its index
    pub fn add(self: *NodeList, node: Node) !NodeIndex {
        const idx = @as(NodeIndex, @intCast(self.nodes.items.len));
        try self.nodes.append(self.allocator, node);
        return idx;
    }

    /// Get a node by index
    pub fn get(self: *const NodeList, idx: NodeIndex) ?*const Node {
        if (idx == null_node or idx >= self.nodes.items.len) return null;
        return &self.nodes.items[idx];
    }

    /// Get a mutable node by index
    pub fn getMut(self: *NodeList, idx: NodeIndex) ?*Node {
        if (idx == null_node or idx >= self.nodes.items.len) return null;
        return &self.nodes.items[idx];
    }

    /// Reserve space for multiple nodes (returns first index)
    pub fn reserve(self: *NodeList, count: usize) !NodeIndex {
        const first_idx = @as(NodeIndex, @intCast(self.nodes.items.len));
        try self.nodes.ensureUnusedCapacity(self.allocator, count);
        return first_idx;
    }

    /// Get slice of nodes
    pub fn slice(self: *const NodeList) []const Node {
        return self.nodes.items;
    }

    /// Add a list of indices and return the starting position
    pub fn addIndexList(self: *NodeList, indices: []const NodeIndex) !NodeIndex {
        if (indices.len == 0) return null_node;
        const start = @as(NodeIndex, @intCast(self.index_lists.items.len));
        try self.index_lists.appendSlice(self.allocator, indices);
        return start;
    }

    /// Get an index from the index list storage
    pub fn getListIndex(self: *const NodeList, list_start: NodeIndex, offset: u16) NodeIndex {
        const pos = list_start + offset;
        if (pos >= self.index_lists.items.len) return null_node;
        return self.index_lists.items[pos];
    }
};

/// Constant pool for strings and floats
pub const ConstantPool = struct {
    strings: std.ArrayList([]const u8),
    floats: std.ArrayList(f64),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) ConstantPool {
        return .{
            .strings = .empty,
            .floats = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ConstantPool) void {
        self.strings.deinit(self.allocator);
        self.floats.deinit(self.allocator);
    }

    pub fn addString(self: *ConstantPool, str: []const u8) !u16 {
        // Check for existing (simple dedup)
        for (self.strings.items, 0..) |s, i| {
            if (std.mem.eql(u8, s, str)) return @intCast(i);
        }
        const idx = @as(u16, @intCast(self.strings.items.len));
        try self.strings.append(self.allocator, str);
        return idx;
    }

    pub fn addFloat(self: *ConstantPool, f: f64) !u16 {
        // Check for existing
        for (self.floats.items, 0..) |v, i| {
            if (v == f) return @intCast(i);
        }
        const idx = @as(u16, @intCast(self.floats.items.len));
        try self.floats.append(self.allocator, f);
        return idx;
    }

    pub fn getString(self: *const ConstantPool, idx: u16) ?[]const u8 {
        if (idx >= self.strings.items.len) return null;
        return self.strings.items[idx];
    }

    pub fn getFloat(self: *const ConstantPool, idx: u16) ?f64 {
        if (idx >= self.floats.items.len) return null;
        return self.floats.items[idx];
    }
};

test "node creation" {
    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    const int_node = Node.litInt(loc, 42);
    try std.testing.expectEqual(NodeTag.lit_int, int_node.tag);
    try std.testing.expectEqual(@as(i32, 42), int_node.data.int_value);

    const bool_node = Node.litBool(loc, true);
    try std.testing.expectEqual(NodeTag.lit_bool, bool_node.tag);
    try std.testing.expect(bool_node.data.bool_value);
}

test "node list operations" {
    var list = NodeList.init(std.testing.allocator);
    defer list.deinit();

    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    const idx1 = try list.add(Node.litInt(loc, 10));
    const idx2 = try list.add(Node.litInt(loc, 20));

    try std.testing.expectEqual(@as(NodeIndex, 0), idx1);
    try std.testing.expectEqual(@as(NodeIndex, 1), idx2);

    const node = list.get(idx1).?;
    try std.testing.expectEqual(@as(i32, 10), node.data.int_value);
}

test "constant pool" {
    var pool = ConstantPool.init(std.testing.allocator);
    defer pool.deinit();

    const idx1 = try pool.addString("hello");
    const idx2 = try pool.addString("world");
    const idx3 = try pool.addString("hello"); // Duplicate

    try std.testing.expectEqual(@as(u16, 0), idx1);
    try std.testing.expectEqual(@as(u16, 1), idx2);
    try std.testing.expectEqual(@as(u16, 0), idx3); // Should return existing

    try std.testing.expectEqualStrings("hello", pool.getString(0).?);
    try std.testing.expectEqualStrings("world", pool.getString(1).?);
}
