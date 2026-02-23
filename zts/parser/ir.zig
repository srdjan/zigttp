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

/// Pack an index-list start/count pair into a single u32.
/// Layout: low 16 bits = start index, high 16 bits = count.
fn packListRef(start: NodeIndex, count: u16) u32 {
    const start16: u16 = @truncate(start);
    std.debug.assert(start == start16);
    return @as(u32, start16) | (@as(u32, count) << 16);
}

/// Unpack an index-list start/count pair encoded by `packListRef`.
const ListRef = struct {
    start: NodeIndex,
    count: u16,
};

fn unpackListRef(packed_value: u32) ListRef {
    return .{
        .start = @as(NodeIndex, packed_value & 0xFFFF),
        .count = @as(u16, @truncate(packed_value >> 16)),
    };
}

/// Reference to a variable binding
pub const BindingRef = struct {
    scope_id: ScopeId,
    slot: u16, // Changed from u8 to support atom indices > 255
    kind: BindingKind,

    pub const BindingKind = enum(u3) {
        local, // Local variable in current function
        upvalue, // Captured from outer scope
        global, // User-declared global variable
        argument, // Function parameter
        undeclared_global, // Implicit/builtin global (not declared by user)
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
    in_op,
};

/// Unary operator types
pub const UnaryOp = enum(u4) {
    neg,
    not,
    bit_not,
    typeof_op,
    void_op,
    pos, // Unary +: coerce to number
    // delete_op removed - delete operator not supported
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
    yield_expr,
    sequence_expr,
    comma_expr,

    // Statements
    expr_stmt,
    var_decl,
    if_stmt,
    for_stmt,
    for_of_stmt,
    for_in_stmt,
    while_stmt,
    do_while_stmt,
    switch_stmt,
    case_clause,
    return_stmt,
    throw_stmt,
    break_stmt,
    continue_stmt,
    try_stmt,
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

        // Pattern element
        pattern_elem: PatternElem,

        // Labeled statement
        labeled: LabeledStmt,

        // No data needed
        none: void,
    };

    // --- Nested data structures ---

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
/// Uses hash maps for O(1) deduplication instead of O(n) linear scan
pub const ConstantPool = struct {
    strings: std.ArrayList([]const u8),
    floats: std.ArrayList(f64),
    allocator: std.mem.Allocator,

    // Hash maps for O(1) deduplication (keys point to indices in arrays)
    string_index: std.StringHashMapUnmanaged(u16),
    float_index: std.AutoHashMapUnmanaged(u64, u16),

    pub fn init(allocator: std.mem.Allocator) ConstantPool {
        return .{
            .strings = .empty,
            .floats = .empty,
            .allocator = allocator,
            .string_index = .{},
            .float_index = .{},
        };
    }

    pub fn deinit(self: *ConstantPool) void {
        self.string_index.deinit(self.allocator);
        self.float_index.deinit(self.allocator);
        self.strings.deinit(self.allocator);
        self.floats.deinit(self.allocator);
    }

    pub fn addString(self: *ConstantPool, str: []const u8) !u16 {
        // O(1) hash lookup for deduplication
        const result = try self.string_index.getOrPut(self.allocator, str);
        if (result.found_existing) {
            return result.value_ptr.*;
        }
        // New string - add to array and update hash map
        const idx: u16 = @intCast(self.strings.items.len);
        try self.strings.append(self.allocator, str);
        result.value_ptr.* = idx;
        return idx;
    }

    pub fn addFloat(self: *ConstantPool, f: f64) !u16 {
        // Use bit representation as key for stable hashing
        const bits: u64 = @bitCast(f);

        // O(1) hash lookup for deduplication
        const result = try self.float_index.getOrPut(self.allocator, bits);
        if (result.found_existing) {
            return result.value_ptr.*;
        }
        // New float - add to array and update hash map
        const idx: u16 = @intCast(self.floats.items.len);
        try self.floats.append(self.allocator, f);
        result.value_ptr.* = idx;
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

// ============================================================================
// Phase 3: Optimized IR Storage using MultiArrayList (Zig Compiler Pattern)
// ============================================================================

/// Compact 8-byte data payload for IR nodes
/// Larger data is stored in extra[] and referenced by index
pub const DataPayload = extern struct {
    /// First 4 bytes - primary data
    a: u32,
    /// Second 4 bytes - secondary data or extra index
    b: u32,

    pub const Tag = enum(u4) {
        /// Direct integer value (a = value, b unused)
        int_direct,
        /// Index references: a = index, b = count or secondary index
        index_pair,
        /// Extra data reference: a = extra_start, b = extra_len
        extra_ref,
        /// Binding reference packed
        binding,
        /// Binary op packed: a[0:8] = op, a[8:32] = left, b = right
        binary_packed,
        /// Flags and indices
        flags_index,
    };

    /// Pack an integer directly
    pub fn fromInt(val: i32) DataPayload {
        return .{ .a = @bitCast(val), .b = 0 };
    }

    /// Pack two indices
    pub fn fromIndices(idx1: NodeIndex, idx2: NodeIndex) DataPayload {
        return .{ .a = idx1, .b = idx2 };
    }

    /// Pack index and count
    pub fn fromIndexCount(start: NodeIndex, count: u16) DataPayload {
        return .{ .a = start, .b = count };
    }

    /// Pack binary expression
    pub fn fromBinary(op: BinaryOp, left: NodeIndex, right: NodeIndex) DataPayload {
        return .{
            .a = (@as(u32, @intFromEnum(op)) << 24) | (left & 0xFFFFFF),
            .b = right,
        };
    }

    /// Unpack binary expression
    pub fn toBinary(self: DataPayload) struct { op: BinaryOp, left: NodeIndex, right: NodeIndex } {
        return .{
            .op = @enumFromInt(@as(u8, @truncate(self.a >> 24))),
            .left = self.a & 0xFFFFFF,
            .right = self.b,
        };
    }

    /// Get as signed integer
    pub fn toInt(self: DataPayload) i32 {
        return @bitCast(self.a);
    }

    /// Get first index
    pub fn getIndex(self: DataPayload) NodeIndex {
        return self.a;
    }

    /// Get count (from b)
    pub fn getCount(self: DataPayload) u16 {
        return @truncate(self.b);
    }

    /// Get second index
    pub fn getIndex2(self: DataPayload) NodeIndex {
        return self.b;
    }
};

/// Optimized IR storage using Structure-of-Arrays pattern
/// ~47% memory reduction compared to Array-of-Structs Node
///
/// Memory layout comparison (per node):
///   AoS (Node struct):  tag(1) + pad(3) + loc(12) + data(~24) = ~40 bytes
///   SoA (IRStore):      tag(1) + loc(10) + data(8) = 19 bytes
pub const IRStore = struct {
    allocator: std.mem.Allocator,

    /// Node tags - 1 byte each, contiguous for fast dispatch
    tags: std.ArrayListUnmanaged(NodeTag),

    /// Source locations - 10 bytes each (packed)
    locs: std.ArrayListUnmanaged(SourceLocation),

    /// Compact data payloads - 8 bytes each
    data: std.ArrayListUnmanaged(DataPayload),

    /// Extra data for variable-length content
    /// Used for: function params, object properties, switch cases, etc.
    extra: std.ArrayListUnmanaged(u32),

    /// Storage for node index lists (statements, arguments, etc.)
    index_lists: std.ArrayListUnmanaged(NodeIndex),

    pub fn init(allocator: std.mem.Allocator) IRStore {
        return .{
            .allocator = allocator,
            .tags = .empty,
            .locs = .empty,
            .data = .empty,
            .extra = .empty,
            .index_lists = .empty,
        };
    }

    pub fn deinit(self: *IRStore) void {
        self.tags.deinit(self.allocator);
        self.locs.deinit(self.allocator);
        self.data.deinit(self.allocator);
        self.extra.deinit(self.allocator);
        self.index_lists.deinit(self.allocator);
    }

    /// Add a node and return its index
    pub fn addNode(self: *IRStore, tag: NodeTag, loc: SourceLocation, payload: DataPayload) !NodeIndex {
        const idx = @as(NodeIndex, @intCast(self.tags.items.len));
        try self.tags.append(self.allocator, tag);
        try self.locs.append(self.allocator, loc);
        try self.data.append(self.allocator, payload);
        return idx;
    }

    /// Add extra data and return starting index
    pub fn addExtra(self: *IRStore, values: []const u32) !u32 {
        const start = @as(u32, @intCast(self.extra.items.len));
        try self.extra.appendSlice(self.allocator, values);
        return start;
    }

    /// Add a single extra value
    pub fn addExtraValue(self: *IRStore, value: u32) !u32 {
        const idx = @as(u32, @intCast(self.extra.items.len));
        try self.extra.append(self.allocator, value);
        return idx;
    }

    /// Get node tag
    pub fn getTag(self: *const IRStore, idx: NodeIndex) NodeTag {
        return self.tags.items[idx];
    }

    /// Get node location
    pub fn getLoc(self: *const IRStore, idx: NodeIndex) SourceLocation {
        return self.locs.items[idx];
    }

    /// Get node data payload
    pub fn getData(self: *const IRStore, idx: NodeIndex) DataPayload {
        return self.data.items[idx];
    }

    /// Get extra data slice
    pub fn getExtra(self: *const IRStore, start: u32, count: u32) []const u32 {
        return self.extra.items[start..][0..count];
    }

    /// Get single extra value
    pub fn getExtraValue(self: *const IRStore, idx: u32) u32 {
        return self.extra.items[idx];
    }

    /// Add index list and return start position
    pub fn addIndexList(self: *IRStore, indices: []const NodeIndex) !NodeIndex {
        if (indices.len == 0) return null_node;
        const start = @as(NodeIndex, @intCast(self.index_lists.items.len));
        try self.index_lists.appendSlice(self.allocator, indices);
        return start;
    }

    /// Get index from list
    pub fn getListIndex(self: *const IRStore, list_start: NodeIndex, offset: u16) NodeIndex {
        const pos = list_start + offset;
        if (pos >= self.index_lists.items.len) return null_node;
        return self.index_lists.items[pos];
    }

    /// Node count
    pub fn len(self: *const IRStore) usize {
        return self.tags.items.len;
    }

    // --- Convenience methods for common node types ---

    /// Add integer literal
    pub fn addLitInt(self: *IRStore, loc: SourceLocation, value: i32) !NodeIndex {
        return self.addNode(.lit_int, loc, DataPayload.fromInt(value));
    }

    /// Add float literal (idx into constant pool)
    pub fn addLitFloat(self: *IRStore, loc: SourceLocation, pool_idx: u16) !NodeIndex {
        return self.addNode(.lit_float, loc, .{ .a = pool_idx, .b = 0 });
    }

    /// Add string literal (idx into constant pool)
    pub fn addLitString(self: *IRStore, loc: SourceLocation, pool_idx: u16) !NodeIndex {
        return self.addNode(.lit_string, loc, .{ .a = pool_idx, .b = 0 });
    }

    /// Add binary expression
    pub fn addBinary(self: *IRStore, loc: SourceLocation, op: BinaryOp, left: NodeIndex, right: NodeIndex) !NodeIndex {
        return self.addNode(.binary_op, loc, DataPayload.fromBinary(op, left, right));
    }

    /// Add unary expression
    pub fn addUnary(self: *IRStore, loc: SourceLocation, op: UnaryOp, operand: NodeIndex) !NodeIndex {
        return self.addNode(.unary_op, loc, .{ .a = @intFromEnum(op), .b = operand });
    }

    /// Add identifier reference
    pub fn addIdentifier(self: *IRStore, loc: SourceLocation, binding: BindingRef) !NodeIndex {
        // Pack binding: scope_id(16) | slot(16) in a, kind(8) in b
        return self.addNode(.identifier, loc, .{
            .a = (@as(u32, binding.scope_id) << 16) | binding.slot,
            .b = @intFromEnum(binding.kind),
        });
    }

    /// Unpack identifier binding
    pub fn getBinding(self: *const IRStore, idx: NodeIndex) BindingRef {
        const d = self.getData(idx);
        return .{
            .scope_id = @truncate(d.a >> 16),
            .slot = @truncate(d.a),
            .kind = @enumFromInt(@as(u3, @truncate(d.b))),
        };
    }

    // ============================================================
    // Node Adapter: Accept full Node struct for seamless migration
    // ============================================================

    /// Add a Node struct, converting to compact SoA representation.
    /// This method enables drop-in replacement of NodeList with IRStore
    /// in the parser without changing node creation code.
    pub fn add(self: *IRStore, node: Node) !NodeIndex {
        const loc = node.loc;
        return switch (node.tag) {
            // --- Literals ---
            .lit_int => self.addNode(.lit_int, loc, DataPayload.fromInt(node.data.int_value)),
            .lit_float => self.addNode(.lit_float, loc, .{ .a = node.data.float_idx, .b = 0 }),
            .lit_string => self.addNode(.lit_string, loc, .{ .a = node.data.string_idx, .b = 0 }),
            .lit_bool => self.addNode(.lit_bool, loc, .{ .a = if (node.data.bool_value) 1 else 0, .b = 0 }),
            .lit_null => self.addNode(.lit_null, loc, .{ .a = 0, .b = 0 }),
            .lit_undefined => self.addNode(.lit_undefined, loc, .{ .a = 0, .b = 0 }),
            // --- Expressions ---
            .identifier => blk: {
                const b = node.data.binding;
                break :blk self.addNode(.identifier, loc, .{
                    .a = (@as(u32, b.scope_id) << 16) | b.slot,
                    .b = @intFromEnum(b.kind),
                });
            },
            .binary_op => self.addNode(.binary_op, loc, DataPayload.fromBinary(
                node.data.binary.op,
                node.data.binary.left,
                node.data.binary.right,
            )),
            .unary_op => blk: {
                const u = node.data.unary;
                break :blk self.addNode(.unary_op, loc, .{ .a = @intFromEnum(u.op), .b = u.operand });
            },
            .ternary => blk: {
                const t = node.data.ternary;
                const extra_start = try self.addExtra(&.{ t.condition, t.then_branch, t.else_branch });
                break :blk self.addNode(.ternary, loc, .{ .a = extra_start, .b = 0 });
            },
            .call => blk: {
                const c = node.data.call;
                // Pack: a = callee, b = args_count(8) | args_start(16) | is_optional(8)
                const b_val = @as(u32, c.args_count) |
                    (@as(u32, @as(u16, @truncate(c.args_start))) << 8) |
                    (@as(u32, if (c.is_optional) 1 else 0) << 24);
                break :blk self.addNode(.call, loc, .{ .a = c.callee, .b = b_val });
            },
            .member_access => blk: {
                const m = node.data.member;
                // Pack: a = object, b = property(16) | is_optional(1) << 16
                const b_val = @as(u32, m.property) | (@as(u32, if (m.is_optional) 1 else 0) << 16);
                break :blk self.addNode(.member_access, loc, .{ .a = m.object, .b = b_val });
            },
            .computed_access => blk: {
                const m = node.data.member;
                // Pack: a = object, b = computed | is_optional << 24
                const b_val = m.computed | (@as(u32, if (m.is_optional) 1 else 0) << 24);
                break :blk self.addNode(.computed_access, loc, .{ .a = m.object, .b = b_val });
            },
            .optional_chain => blk: {
                const m = node.data.member;
                const b_val = m.computed | (@as(u32, m.property) << 16);
                break :blk self.addNode(.optional_chain, loc, .{ .a = m.object, .b = b_val });
            },
            .assignment => blk: {
                const a = node.data.assignment;
                // Pack: a = target, b = value(24) | op(8) (0 = no op, 1+ = op enum + 1)
                const op_byte: u8 = if (a.op) |op| @as(u8, @intFromEnum(op)) + 1 else 0;
                const b_val = @as(u32, @as(u24, @truncate(a.value))) | (@as(u32, op_byte) << 24);
                break :blk self.addNode(.assignment, loc, .{ .a = a.target, .b = b_val });
            },
            .array_literal => blk: {
                const arr = node.data.array;
                // Pack: a = elements_start, b = elements_count(16) | has_spread(1) << 16
                const b_val = @as(u32, arr.elements_count) | (@as(u32, if (arr.has_spread) 1 else 0) << 16);
                break :blk self.addNode(.array_literal, loc, .{ .a = arr.elements_start, .b = b_val });
            },
            .object_literal => blk: {
                const obj = node.data.object;
                break :blk self.addNode(.object_literal, loc, .{ .a = obj.properties_start, .b = obj.properties_count });
            },
            .object_property => blk: {
                const p = node.data.property;
                // Pack: a = key, b = value(24) | is_computed(1) << 24 | is_shorthand(1) << 25
                const flags: u32 = (@as(u32, if (p.is_computed) 1 else 0) << 24) |
                    (@as(u32, if (p.is_shorthand) 1 else 0) << 25);
                const b_val = @as(u32, @as(u24, @truncate(p.value))) | flags;
                break :blk self.addNode(.object_property, loc, .{ .a = p.key, .b = b_val });
            },
            .object_spread => self.addNode(.object_spread, loc, .{ .a = node.data.opt_value orelse null_node, .b = 0 }),
            .spread => self.addNode(.spread, loc, .{ .a = node.data.opt_value orelse null_node, .b = 0 }),
            .yield_expr, .await_expr => self.addNode(node.tag, loc, .{ .a = node.data.opt_value orelse null_node, .b = 0 }),

            // --- Function expressions ---
            .function_expr, .arrow_function => blk: {
                const f = node.data.function;
                // Store in extra: [scope_id, name_atom, params_start, params_count, body, flags]
                const flags_u8: u8 = @bitCast(f.flags);
                const extra_start = try self.addExtra(&.{
                    f.scope_id,
                    f.name_atom,
                    f.params_start,
                    f.params_count,
                    f.body,
                    @as(u32, flags_u8),
                });
                break :blk self.addNode(node.tag, loc, .{ .a = extra_start, .b = 0 });
            },
            // Note: function_decl uses var_decl data (binding + init function)
            .function_decl => blk: {
                const v = node.data.var_decl;
                const binding_packed = (@as(u32, v.binding.scope_id) << 16) | v.binding.slot;
                const kind_and_binding_kind = @as(u32, @intFromEnum(v.kind)) |
                    (@as(u32, @intFromEnum(v.binding.kind)) << 2);
                const extra_start = try self.addExtra(&.{
                    binding_packed,
                    v.pattern,
                    v.init,
                    kind_and_binding_kind,
                });
                break :blk self.addNode(.function_decl, loc, .{ .a = extra_start, .b = 0 });
            },

            // --- Template literals ---
            .template_literal => blk: {
                const t = node.data.template;
                break :blk self.addNode(.template_literal, loc, .{
                    .a = t.parts_start,
                    .b = @as(u32, t.parts_count) | (@as(u32, @as(u16, @truncate(t.tag))) << 8),
                });
            },
            .template_part_string => self.addNode(.template_part_string, loc, .{ .a = node.data.string_idx, .b = 0 }),
            .template_part_expr => self.addNode(.template_part_expr, loc, .{ .a = node.data.opt_value orelse null_node, .b = 0 }),

            // --- Statements ---
            .var_decl => blk: {
                const v = node.data.var_decl;
                // Store in extra: [binding_packed, pattern, init, kind]
                const binding_packed = (@as(u32, v.binding.scope_id) << 16) | v.binding.slot;
                const kind_and_binding_kind = @as(u32, @intFromEnum(v.kind)) |
                    (@as(u32, @intFromEnum(v.binding.kind)) << 2);
                const extra_start = try self.addExtra(&.{
                    binding_packed,
                    v.pattern,
                    v.init,
                    kind_and_binding_kind,
                });
                break :blk self.addNode(.var_decl, loc, .{ .a = extra_start, .b = 0 });
            },
            .if_stmt => blk: {
                const i = node.data.if_stmt;
                const extra_start = try self.addExtra(&.{ i.condition, i.then_branch, i.else_branch });
                break :blk self.addNode(.if_stmt, loc, .{ .a = extra_start, .b = 0 });
            },
            .while_stmt, .do_while_stmt, .for_stmt => blk: {
                const l = node.data.loop;
                // Store in extra: [kind, init, condition, update, body]
                const extra_start = try self.addExtra(&.{
                    @intFromEnum(l.kind),
                    l.init,
                    l.condition,
                    l.update,
                    l.body,
                });
                break :blk self.addNode(node.tag, loc, .{ .a = extra_start, .b = 0 });
            },
            .for_of_stmt, .for_in_stmt => blk: {
                const f = node.data.for_iter;
                const binding_packed = (@as(u32, f.binding.scope_id) << 16) | f.binding.slot;
                const flags = (@as(u32, if (f.is_for_in) 1 else 0)) |
                    (@as(u32, if (f.is_const) 1 else 0) << 1) |
                    (@as(u32, @intFromEnum(f.binding.kind)) << 2);
                // Store: [flags, binding_packed, pattern, iterable, flags, body]
                const extra_start = try self.addExtra(&.{
                    flags,
                    binding_packed,
                    f.pattern,
                    f.iterable,
                    flags,
                    f.body,
                });
                break :blk self.addNode(node.tag, loc, .{ .a = extra_start, .b = 0 });
            },
            .switch_stmt => blk: {
                const s = node.data.switch_stmt;
                break :blk self.addNode(.switch_stmt, loc, .{
                    .a = s.discriminant,
                    .b = @as(u32, @as(u16, @truncate(s.cases_start))) | (@as(u32, s.cases_count) << 16),
                });
            },
            .case_clause => blk: {
                const c = node.data.case_clause;
                break :blk self.addNode(.case_clause, loc, .{
                    .a = c.test_expr,
                    .b = @as(u32, @as(u16, @truncate(c.body_start))) | (@as(u32, c.body_count) << 16),
                });
            },
            .return_stmt, .throw_stmt => self.addNode(node.tag, loc, .{ .a = node.data.opt_value orelse null_node, .b = 0 }),
            .break_stmt, .continue_stmt => self.addNode(node.tag, loc, .{ .a = node.data.opt_label orelse 0, .b = 0 }),
            .try_stmt => blk: {
                const t = node.data.try_stmt;
                const binding_packed = (@as(u32, t.catch_binding.scope_id) << 16) | t.catch_binding.slot;
                const extra_start = try self.addExtra(&.{
                    t.try_block,
                    binding_packed,
                    t.catch_block,
                    t.finally_block,
                });
                break :blk self.addNode(.try_stmt, loc, .{ .a = extra_start, .b = 0 });
            },
            .block, .program => blk: {
                const b = node.data.block;
                break :blk self.addNode(node.tag, loc, .{
                    .a = b.stmts_start,
                    .b = @as(u32, b.stmts_count) | (@as(u32, b.scope_id) << 16),
                });
            },
            .expr_stmt => self.addNode(.expr_stmt, loc, .{ .a = node.data.opt_value orelse null_node, .b = 0 }),
            .empty_stmt => self.addNode(.empty_stmt, loc, .{ .a = 0, .b = 0 }),
            .debugger_stmt => self.addNode(.debugger_stmt, loc, .{ .a = 0, .b = 0 }),

            // --- Patterns ---
            .pattern_element, .pattern_rest, .object_pattern, .array_pattern => blk: {
                if (node.tag == .object_pattern or node.tag == .array_pattern) {
                    const arr = node.data.array;
                    const b_val = @as(u32, arr.elements_count) | (@as(u32, if (arr.has_spread) 1 else 0) << 16);
                    break :blk self.addNode(node.tag, loc, .{ .a = arr.elements_start, .b = b_val });
                }
                const p = node.data.pattern_elem;
                const binding_packed = (@as(u32, p.binding.scope_id) << 16) | p.binding.slot |
                    (@as(u32, @intFromEnum(p.binding.kind)) << 29);
                const extra_start = try self.addExtra(&.{
                    @intFromEnum(p.kind),
                    binding_packed,
                    p.key,
                    p.key_atom,
                    p.default_value,
                });
                break :blk self.addNode(node.tag, loc, .{ .a = extra_start, .b = 0 });
            },

            // --- JSX ---
            .jsx_element, .jsx_fragment => blk: {
                const j = node.data.jsx_element;
                // Store in extra: [tag_atom, props_start, props_count, children_start, children_count | flags]
                const flags = (@as(u32, j.children_count) << 8) |
                    (@as(u32, if (j.is_component) 1 else 0)) |
                    (@as(u32, if (j.self_closing) 1 else 0) << 1);
                const extra_start = try self.addExtra(&.{
                    j.tag_atom,
                    j.props_start,
                    j.props_count,
                    j.children_start,
                    flags,
                });
                break :blk self.addNode(node.tag, loc, .{ .a = extra_start, .b = 0 });
            },
            .jsx_attribute => blk: {
                const a = node.data.jsx_attr;
                const b_val = @as(u32, @as(u24, @truncate(a.value))) | (@as(u32, if (a.is_spread) 1 else 0) << 24);
                break :blk self.addNode(.jsx_attribute, loc, .{ .a = a.name_atom, .b = b_val });
            },
            .jsx_spread_attribute => blk: {
                const a = node.data.jsx_attr;
                break :blk self.addNode(.jsx_spread_attribute, loc, .{ .a = a.value, .b = 0 });
            },
            .jsx_text => self.addNode(.jsx_text, loc, .{ .a = node.data.jsx_text, .b = 0 }),
            .jsx_expr_container => self.addNode(.jsx_expr_container, loc, .{ .a = node.data.opt_value orelse null_node, .b = 0 }),

            // --- Imports/Exports ---
            .import_decl => blk: {
                const i = node.data.import_decl;
                break :blk self.addNode(.import_decl, loc, .{
                    .a = i.module_idx,
                    .b = packListRef(i.specifiers_start, i.specifiers_count),
                });
            },
            .import_specifier => blk: {
                const i = node.data.import_spec;
                const binding_packed = (@as(u32, i.local_binding.scope_id & 0x1FFF) << 16) | i.local_binding.slot |
                    (@as(u32, @intFromEnum(i.local_binding.kind)) << 29);
                const extra_start = try self.addExtra(&.{
                    @intFromEnum(i.kind),
                    i.imported_atom,
                    binding_packed,
                });
                break :blk self.addNode(.import_specifier, loc, .{ .a = extra_start, .b = 0 });
            },
            .export_decl => blk: {
                const e = node.data.export_decl;
                const extra_start = try self.addExtra(&.{
                    @intFromEnum(e.kind),
                    e.declaration,
                    e.specifiers_start,
                    e.specifiers_count,
                    e.from_module_idx,
                });
                break :blk self.addNode(.export_decl, loc, .{ .a = extra_start, .b = 0 });
            },

            // --- Labeled ---
            .labeled_stmt => blk: {
                const l = node.data.labeled;
                break :blk self.addNode(.labeled_stmt, loc, .{ .a = l.label_atom, .b = l.body });
            },

            // --- Call variants ---
            .method_call => blk: {
                const c = node.data.call;
                const b_val = @as(u32, c.args_count) |
                    (@as(u32, @as(u16, @truncate(c.args_start))) << 8) |
                    (@as(u32, if (c.is_optional) 1 else 0) << 24);
                break :blk self.addNode(.method_call, loc, .{ .a = c.callee, .b = b_val });
            },
            .optional_call => blk: {
                const c = node.data.call;
                const b_val = @as(u32, c.args_count) | (@as(u32, @as(u16, @truncate(c.args_start))) << 8);
                break :blk self.addNode(.optional_call, loc, .{ .a = c.callee, .b = b_val });
            },

            // --- Object method/accessor ---
            .object_method, .object_getter, .object_setter => blk: {
                const p = node.data.property;
                const flags: u32 = (@as(u32, if (p.is_computed) 1 else 0) << 24);
                const b_val = @as(u32, @as(u24, @truncate(p.value))) | flags;
                break :blk self.addNode(node.tag, loc, .{ .a = p.key, .b = b_val });
            },

            // --- Sequence/Comma ---
            .sequence_expr, .comma_expr => blk: {
                const arr = node.data.array;
                break :blk self.addNode(node.tag, loc, .{
                    .a = arr.elements_start,
                    .b = @as(u32, arr.elements_count),
                });
            },

            // --- Pattern default ---
            .pattern_default => blk: {
                const p = node.data.pattern_elem;
                const binding_packed = (@as(u32, p.binding.scope_id & 0x1FFF) << 16) | p.binding.slot |
                    (@as(u32, @intFromEnum(p.binding.kind)) << 29);
                const extra_start = try self.addExtra(&.{
                    @intFromEnum(p.kind),
                    binding_packed,
                    p.key,
                    p.key_atom,
                    p.default_value,
                });
                break :blk self.addNode(.pattern_default, loc, .{ .a = extra_start, .b = 0 });
            },

            // --- Import/Export variants ---
            .import_default, .import_namespace => blk: {
                const i = node.data.import_spec;
                const binding_packed = (@as(u32, i.local_binding.scope_id & 0x1FFF) << 16) | i.local_binding.slot |
                    (@as(u32, @intFromEnum(i.local_binding.kind)) << 29);
                const extra_start = try self.addExtra(&.{
                    @intFromEnum(i.kind),
                    i.imported_atom,
                    binding_packed,
                });
                break :blk self.addNode(node.tag, loc, .{ .a = extra_start, .b = 0 });
            },
            .export_specifier => blk: {
                const e = node.data.export_decl;
                const extra_start = try self.addExtra(&.{
                    @intFromEnum(e.kind),
                    e.declaration,
                    e.specifiers_start,
                    e.specifiers_count,
                    e.from_module_idx,
                });
                break :blk self.addNode(.export_specifier, loc, .{ .a = extra_start, .b = 0 });
            },
            .export_default, .export_all => self.addNode(node.tag, loc, .{ .a = node.data.opt_value orelse null_node, .b = 0 }),

            // --- Internal markers (no data) ---
            .param_list, .arg_list, .stmt_list => self.addNode(node.tag, loc, .{ .a = 0, .b = 0 }),
        };
    }

    /// Get a Node struct from index (for compatibility during migration)
    /// Note: This reconstructs the Node from SoA storage, less efficient than direct access
    pub fn get(self: *const IRStore, idx: NodeIndex) ?Node {
        if (idx >= self.tags.items.len) return null;
        const tag = self.tags.items[idx];
        const loc = self.locs.items[idx];
        const d = self.data.items[idx];

        return Node{
            .tag = tag,
            .loc = loc,
            .data = switch (tag) {
                .lit_int => .{ .int_value = d.toInt() },
                .lit_float => .{ .float_idx = @truncate(d.a) },
                .lit_string => .{ .string_idx = @truncate(d.a) },
                .lit_bool => .{ .bool_value = d.a != 0 },
                .lit_null, .lit_undefined, .empty_stmt, .debugger_stmt => .{ .none = {} },
                .identifier => .{ .binding = self.getBinding(idx) },
                .binary_op => blk: {
                    const bin = d.toBinary();
                    break :blk .{ .binary = .{ .op = bin.op, .left = bin.left, .right = bin.right } };
                },
                .unary_op => .{ .unary = .{ .op = @enumFromInt(@as(u4, @truncate(d.a))), .operand = d.b } },
                .return_stmt,
                .throw_stmt,
                .expr_stmt,
                .spread,
                .object_spread,
                .yield_expr,
                .await_expr,
                .template_part_expr,
                .jsx_expr_container,
                => .{ .opt_value = if (d.a == null_node) null else d.a },
                .block, .program => .{ .block = .{
                    .stmts_start = d.a,
                    .stmts_count = @truncate(d.b),
                    .scope_id = @truncate(d.b >> 16),
                } },
                // For complex types, return minimal data - full reconstruction available via IrView
                else => .{ .none = {} },
            },
        };
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

test "IRStore basic operations" {
    var store = IRStore.init(std.testing.allocator);
    defer store.deinit();

    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    // Add integer literal
    const idx1 = try store.addLitInt(loc, 42);
    try std.testing.expectEqual(@as(NodeIndex, 0), idx1);
    try std.testing.expectEqual(NodeTag.lit_int, store.getTag(idx1));
    try std.testing.expectEqual(@as(i32, 42), store.getData(idx1).toInt());

    // Add binary expression
    const idx2 = try store.addLitInt(loc, 10);
    const idx3 = try store.addBinary(loc, .add, idx1, idx2);

    try std.testing.expectEqual(NodeTag.binary_op, store.getTag(idx3));
    const bin = store.getData(idx3).toBinary();
    try std.testing.expectEqual(BinaryOp.add, bin.op);
    try std.testing.expectEqual(idx1, bin.left);
    try std.testing.expectEqual(idx2, bin.right);
}

test "IRStore identifier binding" {
    var store = IRStore.init(std.testing.allocator);
    defer store.deinit();

    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    const binding = BindingRef{
        .scope_id = 5,
        .slot = 3,
        .kind = .upvalue,
    };

    const idx = try store.addIdentifier(loc, binding);
    const unpacked = store.getBinding(idx);

    try std.testing.expectEqual(@as(ScopeId, 5), unpacked.scope_id);
    try std.testing.expectEqual(@as(u16, 3), unpacked.slot);
    try std.testing.expectEqual(BindingRef.BindingKind.upvalue, unpacked.kind);
}

test "IRStore extra data" {
    var store = IRStore.init(std.testing.allocator);
    defer store.deinit();

    // Add some extra data (e.g., for function parameters)
    const extra_data = [_]u32{ 10, 20, 30, 40 };
    const start = try store.addExtra(&extra_data);

    try std.testing.expectEqual(@as(u32, 0), start);

    const retrieved = store.getExtra(start, 4);
    try std.testing.expectEqual(@as(u32, 10), retrieved[0]);
    try std.testing.expectEqual(@as(u32, 20), retrieved[1]);
    try std.testing.expectEqual(@as(u32, 30), retrieved[2]);
    try std.testing.expectEqual(@as(u32, 40), retrieved[3]);
}

test "IRStore memory efficiency" {
    // Verify DataPayload is exactly 8 bytes
    try std.testing.expectEqual(@as(usize, 8), @sizeOf(DataPayload));

    // NodeTag should be 1 byte
    try std.testing.expectEqual(@as(usize, 1), @sizeOf(NodeTag));
}

// ============================================================================
// Phase 8: IrView Abstraction Layer
// ============================================================================

/// Unified view for IR access - can be backed by NodeList (AoS) or IRStore (SoA).
/// This abstraction enables safe migration from NodeList to IRStore without
/// changing codegen code.
///
/// Usage:
/// ```zig
/// // With NodeList (current)
/// var view = IrView.fromNodeList(&nodes, &ir_constants);
///
/// // With IRStore (future)
/// var view = IrView.fromIRStore(&store, &ir_constants);
///
/// // Access is the same:
/// const tag = view.getTag(idx);
/// const binary = view.getBinary(idx);
/// ```
pub const IrView = struct {
    impl: Impl,
    ir_constants: *const ConstantPool,

    const Impl = union(enum) {
        node_list: *const NodeList,
        ir_store: *const IRStore,
    };

    /// Create view from NodeList (Array-of-Structs)
    pub fn fromNodeList(nodes: *const NodeList, constants: *const ConstantPool) IrView {
        return .{
            .impl = .{ .node_list = nodes },
            .ir_constants = constants,
        };
    }

    /// Create view from IRStore (Structure-of-Arrays)
    pub fn fromIRStore(store: *const IRStore, constants: *const ConstantPool) IrView {
        return .{
            .impl = .{ .ir_store = store },
            .ir_constants = constants,
        };
    }

    // ============ Core Accessors ============

    /// Get node tag
    pub fn getTag(self: IrView, idx: NodeIndex) ?NodeTag {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.tag else null,
            .ir_store => |ir| if (idx < ir.tags.items.len) ir.tags.items[idx] else null,
        };
    }

    /// Get node source location
    pub fn getLoc(self: IrView, idx: NodeIndex) ?SourceLocation {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.loc else null,
            .ir_store => |ir| if (idx < ir.locs.items.len) ir.locs.items[idx] else null,
        };
    }

    /// Check if node index is valid
    pub fn isValid(self: IrView, idx: NodeIndex) bool {
        if (idx == null_node) return false;
        return switch (self.impl) {
            .node_list => |nl| idx < nl.nodes.items.len,
            .ir_store => |ir| idx < ir.tags.items.len,
        };
    }

    /// Get total node count (useful for capacity pre-allocation)
    pub fn nodeCount(self: IrView) usize {
        return switch (self.impl) {
            .node_list => |nl| nl.nodes.items.len,
            .ir_store => |ir| ir.tags.items.len,
        };
    }

    /// Get index from list storage
    pub fn getListIndex(self: IrView, list_start: NodeIndex, offset: u16) NodeIndex {
        return switch (self.impl) {
            .node_list => |nl| nl.getListIndex(list_start, offset),
            .ir_store => |ir| ir.getListIndex(list_start, offset),
        };
    }

    // ============ Literal Accessors ============

    /// Get integer literal value
    pub fn getIntValue(self: IrView, idx: NodeIndex) ?i32 {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.int_value else null,
            .ir_store => |ir| if (idx < ir.data.items.len) ir.data.items[idx].toInt() else null,
        };
    }

    /// Get float literal index into constant pool
    pub fn getFloatIdx(self: IrView, idx: NodeIndex) ?u16 {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.float_idx else null,
            .ir_store => |ir| if (idx < ir.data.items.len) @truncate(ir.data.items[idx].a) else null,
        };
    }

    /// Get string literal index into constant pool
    pub fn getStringIdx(self: IrView, idx: NodeIndex) ?u16 {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.string_idx else null,
            .ir_store => |ir| if (idx < ir.data.items.len) @truncate(ir.data.items[idx].a) else null,
        };
    }

    /// Get boolean literal value
    pub fn getBoolValue(self: IrView, idx: NodeIndex) ?bool {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.bool_value else null,
            .ir_store => |ir| if (idx < ir.data.items.len) ir.data.items[idx].a != 0 else null,
        };
    }

    // ============ Expression Accessors ============

    /// Get identifier/binding reference
    pub fn getBinding(self: IrView, idx: NodeIndex) ?BindingRef {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.binding else null,
            .ir_store => |ir| if (idx < ir.data.items.len) ir.getBinding(idx) else null,
        };
    }

    /// Get binary expression data
    pub fn getBinary(self: IrView, idx: NodeIndex) ?Node.BinaryExpr {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.binary else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const bin = ir.data.items[idx].toBinary();
                break :blk .{
                    .op = bin.op,
                    .left = bin.left,
                    .right = bin.right,
                };
            },
        };
    }

    /// Get unary expression data
    pub fn getUnary(self: IrView, idx: NodeIndex) ?Node.UnaryExpr {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.unary else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                break :blk .{
                    .op = @enumFromInt(@as(u4, @truncate(d.a))),
                    .operand = d.b,
                };
            },
        };
    }

    /// Get ternary expression data
    pub fn getTernary(self: IrView, idx: NodeIndex) ?Node.TernaryExpr {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.ternary else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                // Ternary uses extra data: [condition, then, else]
                const extra_start = d.a;
                break :blk .{
                    .condition = ir.extra.items[extra_start],
                    .then_branch = ir.extra.items[extra_start + 1],
                    .else_branch = ir.extra.items[extra_start + 2],
                };
            },
        };
    }

    /// Get call expression data
    pub fn getCall(self: IrView, idx: NodeIndex) ?Node.CallExpr {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.call else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                // Call packed: a = callee, b[0:8] = args_count, b[8:24] = args_start, b[24] = is_optional
                break :blk .{
                    .callee = d.a,
                    .args_start = @truncate(d.b >> 8),
                    .args_count = @truncate(d.b),
                    .is_optional = (d.b >> 24) != 0,
                };
            },
        };
    }

    /// Get member access expression data
    pub fn getMember(self: IrView, idx: NodeIndex) ?Node.MemberExpr {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.member else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                const tag = ir.tags.items[idx];
                // Member packed formats differ by tag.
                switch (tag) {
                    .computed_access => {
                        // Pack: a = object, b = computed | is_optional << 24
                        break :blk .{
                            .object = d.a,
                            .property = 0,
                            .computed = @truncate(d.b),
                            .is_optional = (d.b >> 24) != 0,
                        };
                    },
                    else => {
                        // Member packed: a = object, b[0:16] = property, b[16:17] = is_optional
                        break :blk .{
                            .object = d.a,
                            .property = @truncate(d.b),
                            .computed = null_node,
                            .is_optional = (d.b >> 16) != 0,
                        };
                    },
                }
            },
        };
    }

    /// Get assignment expression data
    pub fn getAssignment(self: IrView, idx: NodeIndex) ?Node.AssignExpr {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.assignment else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                // Assignment: a = target, b[0:24] = value, b[24:32] = op (0 = none)
                const op_byte: u8 = @truncate(d.b >> 24);
                break :blk .{
                    .target = d.a,
                    .value = @truncate(d.b),
                    .op = if (op_byte == 0) null else @enumFromInt(op_byte - 1),
                };
            },
        };
    }

    /// Get array literal data
    pub fn getArray(self: IrView, idx: NodeIndex) ?Node.ArrayExpr {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.array else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                break :blk .{
                    .elements_start = d.a,
                    .elements_count = @truncate(d.b),
                    .has_spread = (d.b >> 16) != 0,
                };
            },
        };
    }

    /// Get object literal data
    pub fn getObject(self: IrView, idx: NodeIndex) ?Node.ObjectExpr {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.object else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                break :blk .{
                    .properties_start = d.a,
                    .properties_count = @truncate(d.b),
                };
            },
        };
    }

    /// Get property expression data
    pub fn getProperty(self: IrView, idx: NodeIndex) ?Node.PropertyExpr {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.property else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                // Property: a = key, b[0:24] = value, b[24] = is_computed, b[25] = is_shorthand
                break :blk .{
                    .key = d.a,
                    .value = @truncate(d.b),
                    .is_computed = (d.b >> 24) & 1 != 0,
                    .is_shorthand = (d.b >> 25) & 1 != 0,
                };
            },
        };
    }

    /// Get function expression data
    pub fn getFunction(self: IrView, idx: NodeIndex) ?Node.FunctionExpr {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.function else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                // Function uses extra data for full structure
                const extra_start = d.a;
                const extra = ir.extra.items;
                break :blk .{
                    .scope_id = @truncate(extra[extra_start]),
                    .name_atom = @truncate(extra[extra_start + 1]),
                    .params_start = extra[extra_start + 2],
                    .params_count = @truncate(extra[extra_start + 3]),
                    .body = extra[extra_start + 4],
                    .flags = @bitCast(@as(u8, @truncate(extra[extra_start + 5]))),
                };
            },
        };
    }

    /// Get template literal data
    pub fn getTemplate(self: IrView, idx: NodeIndex) ?Node.TemplateExpr {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.template else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                break :blk .{
                    .parts_start = d.a,
                    .parts_count = @truncate(d.b),
                    .tag = @truncate(d.b >> 8),
                };
            },
        };
    }

    // ============ Statement Accessors ============

    /// Get variable declaration data
    pub fn getVarDecl(self: IrView, idx: NodeIndex) ?Node.VarDecl {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.var_decl else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                const extra_start = d.a;
                const extra = ir.extra.items;
                // VarDecl in extra: [binding_packed, pattern, init, kind]
                const binding_packed = extra[extra_start];
                break :blk .{
                    .binding = .{
                        .scope_id = @truncate(binding_packed >> 16),
                        .slot = @truncate(binding_packed),
                        .kind = @enumFromInt(@as(u3, @truncate(extra[extra_start + 3] >> 2))),
                    },
                    .pattern = extra[extra_start + 1],
                    .init = extra[extra_start + 2],
                    .kind = @enumFromInt(@as(u2, @truncate(extra[extra_start + 3]))),
                };
            },
        };
    }

    /// Get if statement data
    pub fn getIfStmt(self: IrView, idx: NodeIndex) ?Node.IfStmt {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.if_stmt else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                const extra_start = d.a;
                const extra = ir.extra.items;
                break :blk .{
                    .condition = extra[extra_start],
                    .then_branch = extra[extra_start + 1],
                    .else_branch = extra[extra_start + 2],
                };
            },
        };
    }

    /// Get loop statement data
    pub fn getLoop(self: IrView, idx: NodeIndex) ?Node.LoopStmt {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.loop else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                const extra_start = d.a;
                const extra = ir.extra.items;
                break :blk .{
                    .kind = @enumFromInt(@as(u2, @truncate(extra[extra_start]))),
                    .init = extra[extra_start + 1],
                    .condition = extra[extra_start + 2],
                    .update = extra[extra_start + 3],
                    .body = extra[extra_start + 4],
                };
            },
        };
    }

    /// Get for-of/for-in statement data
    pub fn getForIter(self: IrView, idx: NodeIndex) ?Node.ForIterStmt {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.for_iter else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                const extra_start = d.a;
                const extra = ir.extra.items;
                const binding_packed = extra[extra_start + 1];
                const flags = extra[extra_start + 4];
                break :blk .{
                    .is_for_in = (flags & 1) != 0,
                    .binding = .{
                        .scope_id = @truncate(binding_packed >> 16),
                        .slot = @truncate(binding_packed),
                        .kind = @enumFromInt(@as(u3, @truncate(flags >> 2))),
                    },
                    .pattern = extra[extra_start + 2],
                    .iterable = extra[extra_start + 3],
                    .body = extra[extra_start + 5],
                    .is_const = (flags >> 1) & 1 != 0,
                };
            },
        };
    }

    /// Get switch statement data
    pub fn getSwitchStmt(self: IrView, idx: NodeIndex) ?Node.SwitchStmt {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.switch_stmt else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                break :blk .{
                    .discriminant = d.a,
                    .cases_start = @truncate(d.b),
                    .cases_count = @truncate(d.b >> 16),
                };
            },
        };
    }

    /// Get case clause data
    pub fn getCaseClause(self: IrView, idx: NodeIndex) ?Node.CaseClause {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.case_clause else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                break :blk .{
                    .test_expr = d.a,
                    .body_start = @truncate(d.b),
                    .body_count = @truncate(d.b >> 16),
                };
            },
        };
    }

    /// Get block data
    pub fn getBlock(self: IrView, idx: NodeIndex) ?Node.BlockData {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.block else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                break :blk .{
                    .stmts_start = d.a,
                    .stmts_count = @truncate(d.b),
                    .scope_id = @truncate(d.b >> 16),
                };
            },
        };
    }

    /// Get optional value (for return, expr_stmt, etc.)
    pub fn getOptValue(self: IrView, idx: NodeIndex) ?NodeIndex {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.opt_value else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const val = ir.data.items[idx].a;
                break :blk if (val == null_node) null else val;
            },
        };
    }

    // ============ JSX Accessors ============

    /// Get JSX element data
    pub fn getJsxElement(self: IrView, idx: NodeIndex) ?Node.JsxElement {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.jsx_element else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                const extra_start = d.a;
                const extra = ir.extra.items;
                const flags = extra[extra_start + 4];
                break :blk .{
                    .tag_atom = @truncate(extra[extra_start]),
                    .is_component = (flags & 1) != 0,
                    .props_start = extra[extra_start + 1],
                    .props_count = @truncate(extra[extra_start + 2]),
                    .children_start = extra[extra_start + 3],
                    .children_count = @truncate(extra[extra_start + 4] >> 8),
                    .self_closing = (flags >> 1) & 1 != 0,
                };
            },
        };
    }

    /// Get JSX attribute data
    pub fn getJsxAttr(self: IrView, idx: NodeIndex) ?Node.JsxAttr {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.jsx_attr else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                break :blk .{
                    .name_atom = @truncate(d.a),
                    .value = @truncate(d.b),
                    .is_spread = (d.b >> 24) != 0,
                };
            },
        };
    }

    /// Get JSX text index
    pub fn getJsxText(self: IrView, idx: NodeIndex) ?u16 {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.jsx_text else null,
            .ir_store => |ir| if (idx < ir.data.items.len) @truncate(ir.data.items[idx].a) else null,
        };
    }

    // ============ Module Accessors ============

    /// Get import declaration data
    /// Packing: a = module_idx, b = specifiers_start_low16 | (specifiers_count << 16)
    pub fn getImportDecl(self: IrView, idx: NodeIndex) ?Node.ImportDecl {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.import_decl else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                const list_ref = unpackListRef(d.b);
                break :blk .{
                    .module_idx = @truncate(d.a),
                    .specifiers_start = list_ref.start,
                    .specifiers_count = @truncate(list_ref.count),
                };
            },
        };
    }

    /// Get import specifier data
    /// Packing: a = extra_start -> extra = [kind, imported_atom, binding_packed]
    pub fn getImportSpec(self: IrView, idx: NodeIndex) ?Node.ImportSpec {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.import_spec else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                const extra_start = d.a;
                const extra = ir.extra.items;
                const binding_packed = extra[extra_start + 2];
                break :blk .{
                    .kind = @enumFromInt(@as(u2, @truncate(extra[extra_start]))),
                    .imported_atom = @truncate(extra[extra_start + 1]),
                    .local_binding = .{
                        .scope_id = @truncate((binding_packed >> 16) & 0x1FFF),
                        .slot = @truncate(binding_packed),
                        .kind = @enumFromInt(@as(u3, @truncate(binding_packed >> 29))),
                    },
                };
            },
        };
    }

    /// Get export declaration data
    /// Packing: a = extra_start -> extra = [kind, declaration, specifiers_start, specifiers_count, from_module_idx]
    pub fn getExportDecl(self: IrView, idx: NodeIndex) ?Node.ExportDecl {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.export_decl else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                const extra_start = d.a;
                const extra = ir.extra.items;
                break :blk .{
                    .kind = @enumFromInt(@as(u2, @truncate(extra[extra_start]))),
                    .declaration = extra[extra_start + 1],
                    .specifiers_start = extra[extra_start + 2],
                    .specifiers_count = @truncate(extra[extra_start + 3]),
                    .from_module_idx = @truncate(extra[extra_start + 4]),
                };
            },
        };
    }

    // ============ Pattern Accessors ============

    /// Get pattern element data
    pub fn getPatternElem(self: IrView, idx: NodeIndex) ?Node.PatternElem {
        return switch (self.impl) {
            .node_list => |nl| if (nl.get(idx)) |node| node.data.pattern_elem else null,
            .ir_store => |ir| blk: {
                if (idx >= ir.data.items.len) break :blk null;
                const d = ir.data.items[idx];
                const extra_start = d.a;
                const extra = ir.extra.items;
                const binding_packed = extra[extra_start + 1];
                break :blk .{
                    .kind = @enumFromInt(@as(u2, @truncate(extra[extra_start]))),
                    .binding = .{
                        .scope_id = @truncate((binding_packed >> 16) & 0x1FFF),
                        .slot = @truncate(binding_packed),
                        .kind = @enumFromInt(@as(u3, @truncate(binding_packed >> 29))),
                    },
                    .key = extra[extra_start + 2],
                    .key_atom = @truncate(extra[extra_start + 3]),
                    .default_value = extra[extra_start + 4],
                };
            },
        };
    }

    // ============ Constant Pool Access ============

    /// Get string from constant pool
    pub fn getString(self: IrView, str_idx: u16) ?[]const u8 {
        return self.ir_constants.getString(str_idx);
    }

    /// Get float from constant pool
    pub fn getFloat(self: IrView, float_idx: u16) ?f64 {
        return self.ir_constants.getFloat(float_idx);
    }
};

test "IrView NodeList basic operations" {
    const allocator = std.testing.allocator;

    var nodes = NodeList.init(allocator);
    defer nodes.deinit();

    var constants = ConstantPool.init(allocator);
    defer constants.deinit();

    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    // Add nodes
    const int_idx = try nodes.add(Node.litInt(loc, 42));
    const bool_idx = try nodes.add(Node.litBool(loc, true));
    const left = try nodes.add(Node.litInt(loc, 10));
    const right = try nodes.add(Node.litInt(loc, 20));
    const binary_idx = try nodes.add(Node.binaryOp(loc, .add, left, right));

    // Create view
    const view = IrView.fromNodeList(&nodes, &constants);

    // Test basic accessors
    try std.testing.expectEqual(NodeTag.lit_int, view.getTag(int_idx).?);
    try std.testing.expectEqual(@as(i32, 42), view.getIntValue(int_idx).?);

    try std.testing.expectEqual(NodeTag.lit_bool, view.getTag(bool_idx).?);
    try std.testing.expect(view.getBoolValue(bool_idx).?);

    // Test binary expression
    try std.testing.expectEqual(NodeTag.binary_op, view.getTag(binary_idx).?);
    const binary = view.getBinary(binary_idx).?;
    try std.testing.expectEqual(BinaryOp.add, binary.op);
    try std.testing.expectEqual(left, binary.left);
    try std.testing.expectEqual(right, binary.right);

    // Test validity
    try std.testing.expect(view.isValid(int_idx));
    try std.testing.expect(!view.isValid(null_node));
    try std.testing.expect(!view.isValid(999999));
}

test "IrView IRStore basic operations" {
    const allocator = std.testing.allocator;

    var store = IRStore.init(allocator);
    defer store.deinit();

    var constants = ConstantPool.init(allocator);
    defer constants.deinit();

    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    // Add nodes using IRStore
    const int_idx = try store.addLitInt(loc, 42);
    const left = try store.addLitInt(loc, 10);
    const right = try store.addLitInt(loc, 20);
    const binary_idx = try store.addBinary(loc, .add, left, right);

    // Create view
    const view = IrView.fromIRStore(&store, &constants);

    // Test basic accessors
    try std.testing.expectEqual(NodeTag.lit_int, view.getTag(int_idx).?);
    try std.testing.expectEqual(@as(i32, 42), view.getIntValue(int_idx).?);

    // Test binary expression
    try std.testing.expectEqual(NodeTag.binary_op, view.getTag(binary_idx).?);
    const binary = view.getBinary(binary_idx).?;
    try std.testing.expectEqual(BinaryOp.add, binary.op);
    try std.testing.expectEqual(left, binary.left);
    try std.testing.expectEqual(right, binary.right);

    // Test validity
    try std.testing.expect(view.isValid(int_idx));
    try std.testing.expect(!view.isValid(null_node));
}
