//! Bytecode Code Generator
//!
//! Walks the IR and emits bytecode, handling closures and upvalues.

const std = @import("std");
const ir = @import("ir.zig");
const scope_mod = @import("scope.zig");

// Import real types from parent module for integration
const bytecode = @import("../bytecode.zig");
const bytecode_opt = @import("../bytecode_opt.zig");
const value = @import("../value.zig");
const heap = @import("../heap.zig");
const string = @import("../string.zig");
const js_object = @import("../object.zig");
const context = @import("../context.zig");

// Re-export types used by this module
const JSValue = value.JSValue;
const Opcode = bytecode.Opcode;
const FunctionBytecode = bytecode.FunctionBytecode;
const UpvalueInfo = bytecode.UpvalueInfo;

const Node = ir.Node;
const NodeTag = ir.NodeTag;
const NodeIndex = ir.NodeIndex;
const NodeList = ir.NodeList;
const IRStore = ir.IRStore;
const IrView = ir.IrView;
const ConstantPool = ir.ConstantPool;
const BindingRef = ir.BindingRef;
const BinaryOp = ir.BinaryOp;
const UnaryOp = ir.UnaryOp;
const null_node = ir.null_node;

const ScopeAnalyzer = scope_mod.ScopeAnalyzer;
const Scope = scope_mod.Scope;
const ScopeId = scope_mod.ScopeId;
const Upvalue = scope_mod.Upvalue;

/// Label for jump patching
const Label = struct {
    offset: u32,
    resolved: bool,
};

/// Pending jump to patch
const PendingJump = struct {
    instruction_offset: u32, // Offset of the jump instruction
    target_label: u32, // Label ID to jump to
};

/// Break/continue target
const LoopContext = struct {
    break_label: u32,
    continue_label: u32,
};

/// Compile-time flag to enable peephole optimization
/// Reduces dispatch overhead by fusing common instruction sequences
pub const enable_peephole_opt = true;

/// Code generator state
pub const CodeGen = struct {
    allocator: std.mem.Allocator,
    ir: IrView,
    scopes: *ScopeAnalyzer,
    strings: ?*string.StringTable,
    atoms: ?*context.AtomTable,

    // Output
    code: std.ArrayList(u8),
    constants: std.ArrayList(JSValue),
    upvalue_info: std.ArrayList(UpvalueInfo),

    // State
    labels: std.ArrayList(Label),
    pending_jumps: std.ArrayList(PendingJump),
    loop_stack: std.ArrayList(LoopContext),
    current_scope: ScopeId,
    max_stack_depth: u16,
    current_stack_depth: u16,
    /// Next inline cache slot index (per function)
    /// When >= IC_CACHE_SIZE, falls back to non-IC opcodes
    ic_cache_idx: u16,

    /// Optimization statistics (accumulated across all functions)
    opt_stats: bytecode_opt.OptStats,

    /// Object literal shapes collected during compilation.
    /// Each entry is an array of atoms representing property names in declaration order.
    shapes: std.ArrayList([]const js_object.Atom),

    /// Map from shape content hash to shape index for deduplication.
    shape_dedup: std.AutoHashMapUnmanaged(u64, u16),

    /// Maximum inline cache slots per function (must match interpreter.IC_CACHE_SIZE)
    pub const IC_CACHE_SIZE: u16 = 256;

    pub fn init(
        allocator: std.mem.Allocator,
        nodes: *const NodeList,
        ir_constants: *const ConstantPool,
        scopes: *ScopeAnalyzer,
    ) CodeGen {
        return initWithStrings(allocator, nodes, ir_constants, scopes, null, null);
    }

    /// Initialize with IRStore (new SoA format)
    pub fn initWithIRStore(
        allocator: std.mem.Allocator,
        nodes: *const IRStore,
        ir_constants: *const ConstantPool,
        scopes: *ScopeAnalyzer,
        strings_table: ?*string.StringTable,
        atoms_table: ?*context.AtomTable,
    ) CodeGen {
        return .{
            .allocator = allocator,
            .ir = IrView.fromIRStore(nodes, ir_constants),
            .scopes = scopes,
            .strings = strings_table,
            .atoms = atoms_table,
            .code = std.ArrayList(u8).empty,
            .constants = std.ArrayList(JSValue).empty,
            .upvalue_info = std.ArrayList(UpvalueInfo).empty,
            .labels = std.ArrayList(Label).empty,
            .pending_jumps = std.ArrayList(PendingJump).empty,
            .loop_stack = std.ArrayList(LoopContext).empty,
            .current_scope = 0,
            .max_stack_depth = 0,
            .current_stack_depth = 0,
            .ic_cache_idx = 0,
            .opt_stats = .{},
            .shapes = .{},
            .shape_dedup = .{},
        };
    }

    pub fn initWithStrings(
        allocator: std.mem.Allocator,
        nodes: *const NodeList,
        ir_constants: *const ConstantPool,
        scopes: *ScopeAnalyzer,
        strings_table: ?*string.StringTable,
        atoms_table: ?*context.AtomTable,
    ) CodeGen {
        return .{
            .allocator = allocator,
            .ir = IrView.fromNodeList(nodes, ir_constants),
            .scopes = scopes,
            .strings = strings_table,
            .atoms = atoms_table,
            .code = std.ArrayList(u8).empty,
            .constants = std.ArrayList(JSValue).empty,
            .upvalue_info = std.ArrayList(UpvalueInfo).empty,
            .labels = std.ArrayList(Label).empty,
            .pending_jumps = std.ArrayList(PendingJump).empty,
            .loop_stack = std.ArrayList(LoopContext).empty,
            .current_scope = 0,
            .max_stack_depth = 0,
            .current_stack_depth = 0,
            .ic_cache_idx = 0,
            .opt_stats = .{},
            .shapes = .{},
            .shape_dedup = .{},
        };
    }

    /// Get accumulated optimization statistics
    pub fn getOptStats(self: *const CodeGen) bytecode_opt.OptStats {
        return self.opt_stats;
    }

    pub fn deinit(self: *CodeGen) void {
        // Constants may be retained by runtime-owned FunctionBytecode; don't free here.
        self.code.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        self.upvalue_info.deinit(self.allocator);
        self.labels.deinit(self.allocator);
        self.pending_jumps.deinit(self.allocator);
        self.loop_stack.deinit(self.allocator);

        // Free object literal shapes
        for (self.shapes.items) |shape| {
            self.allocator.free(shape);
        }
        self.shapes.deinit(self.allocator);
        self.shape_dedup.deinit(self.allocator);
    }

    /// Free heap-allocated objects stored in constants array (FunctionBytecode, Float64Box)
    fn freeConstantsContents(self: *CodeGen, constants: []const JSValue) void {
        for (constants) |val| {
            if (val.isExternPtr()) {
                const func_bc = val.toExternPtr(FunctionBytecode);
                // Recursively free nested constants
                self.freeConstantsContents(func_bc.constants);
                // Free the duped slices
                if (func_bc.code.len > 0) {
                    self.allocator.free(func_bc.code);
                }
                if (func_bc.constants.len > 0) {
                    self.allocator.free(func_bc.constants);
                }
                if (func_bc.upvalue_info.len > 0) {
                    self.allocator.free(func_bc.upvalue_info);
                }
                // Free the FunctionBytecode struct itself
                self.allocator.destroy(func_bc);
            } else if (val.isFloat64()) {
                // Free Float64Box allocations
                const float_box = val.toPtr(JSValue.Float64Box);
                self.allocator.destroy(float_box);
            }
            // Note: Strings are managed by StringTable, don't free them here
        }
    }

    /// Generate bytecode for the entire program
    pub fn generate(self: *CodeGen, root: NodeIndex) !FunctionBytecode {
        try self.emitNode(root);
        try self.emit(.ret_undefined);

        try self.resolveJumps();

        // Apply peephole optimization if enabled
        if (comptime enable_peephole_opt) {
            try self.applyPeepholeOpt();
        }

        return .{
            .header = .{ .flags = .{ .optimized = enable_peephole_opt } },
            .name_atom = 0,
            .arg_count = 0,
            .local_count = self.scopes.getLocalCount(0),
            .stack_size = self.max_stack_depth,
            .flags = .{},
            .upvalue_count = 0,
            .upvalue_info = &.{},
            .code = self.code.items,
            .constants = self.constants.items,
            .source_map = null,
        };
    }

    /// Generate bytecode for a function body
    pub fn generateFunction(self: *CodeGen, func_node: NodeIndex, scope_id: ScopeId) !FunctionBytecode {
        self.current_scope = scope_id;

        const func = self.ir.getFunction(func_node) orelse return error.InvalidNode;

        // Emit function body
        try self.emitNode(func.body);

        // Ensure function returns
        try self.emit(.ret_undefined);

        try self.resolveJumps();

        // Apply peephole optimization if enabled
        if (comptime enable_peephole_opt) {
            try self.applyPeepholeOpt();
        }

        // Get upvalue info from scope
        const scope = self.scopes.getScope(scope_id);
        var upvalue_info_list: std.ArrayList(UpvalueInfo) = .empty;
        errdefer upvalue_info_list.deinit(self.allocator);

        for (scope.upvalues.items) |uv| {
            try upvalue_info_list.append(self.allocator, .{
                .is_local = uv.is_direct,
                .index = uv.outer_slot,
            });
        }

        // Dupe to heap-owned slice to avoid dangling pointer after deinit
        const upvalue_copy = try self.allocator.dupe(UpvalueInfo, upvalue_info_list.items);
        upvalue_info_list.deinit(self.allocator);

        return .{
            .header = .{ .flags = .{ .optimized = enable_peephole_opt } },
            .name_atom = func.name_atom,
            .arg_count = func.params_count,
            .local_count = self.scopes.getLocalCount(scope_id),
            .stack_size = self.max_stack_depth,
            .flags = .{
                .is_generator = func.flags.is_generator,
                .is_async = func.flags.is_async,
                .has_rest = func.flags.has_rest_param,
            },
            .upvalue_count = @intCast(scope.upvalues.items.len),
            .upvalue_info = upvalue_copy,
            .code = self.code.items,
            .constants = self.constants.items,
            .source_map = null,
        };
    }

    /// Apply peephole optimization to the generated bytecode
    fn applyPeepholeOpt(self: *CodeGen) !void {
        var optimizer = bytecode_opt.BytecodeOptimizer.init(self.allocator);
        defer optimizer.deinit();

        // Run peephole optimization
        const stats = try optimizer.optimize(self.code.items);

        // Compact to remove NOPs
        const new_len = try optimizer.compact(self.code.items);

        // Resize the code array to the compacted length
        self.code.shrinkRetainingCapacity(new_len);

        // Accumulate statistics
        self.opt_stats.get_loc_add_count += stats.get_loc_add_count;
        self.opt_stats.get_loc_get_loc_add_count += stats.get_loc_get_loc_add_count;
        self.opt_stats.push_const_call_count += stats.push_const_call_count;
        self.opt_stats.get_field_call_count += stats.get_field_call_count;
        self.opt_stats.bytes_saved += stats.bytes_saved;
        self.opt_stats.dispatches_saved += stats.dispatches_saved;
    }

    // ============ Node Emission ============

    fn emitNode(self: *CodeGen, index: NodeIndex) anyerror!void {
        if (index == null_node) return;

        const tag = self.ir.getTag(index) orelse return;

        switch (tag) {
            // Literals
            .lit_int => try self.emitInteger(self.ir.getIntValue(index).?),
            .lit_float => try self.emitFloat(self.ir.getFloatIdx(index).?),
            .lit_string => try self.emitString(self.ir.getStringIdx(index).?),
            .lit_bool => try self.emitBool(self.ir.getBoolValue(index).?),
            .lit_null => try self.emit(.push_null),
            .lit_undefined => try self.emit(.push_undefined),

            // Identifiers
            .identifier => try self.emitIdentifier(self.ir.getBinding(index).?),

            // Expressions
            .binary_op => try self.emitBinaryOp(self.ir.getBinary(index).?),
            .unary_op => try self.emitUnaryOp(self.ir.getUnary(index).?),
            .ternary => try self.emitTernary(self.ir.getTernary(index).?),
            .call, .optional_call => try self.emitCall(self.ir.getCall(index).?),
            .member_access, .optional_chain => try self.emitMemberAccess(self.ir.getMember(index).?),
            .computed_access => try self.emitComputedAccess(self.ir.getMember(index).?),
            .assignment => try self.emitAssignment(self.ir.getAssignment(index).?),
            .array_literal => try self.emitArrayLiteral(self.ir.getArray(index).?),
            .object_literal => try self.emitObjectLiteral(self.ir.getObject(index).?),
            .function_expr, .arrow_function => try self.emitFunctionExpr(index, self.ir.getFunction(index).?),
            .template_literal => try self.emitTemplateLiteral(self.ir.getTemplate(index).?),

            // Statements
            .expr_stmt => {
                if (self.ir.getOptValue(index)) |expr| {
                    try self.emitNode(expr);
                    try self.emit(.drop);
                }
            },
            .var_decl, .function_decl => try self.emitVarDecl(self.ir.getVarDecl(index).?),
            .if_stmt => try self.emitIfStmt(self.ir.getIfStmt(index).?),
            .for_stmt => try self.emitForLoop(self.ir.getLoop(index).?),
            .for_of_stmt => try self.emitForIterLoop(self.ir.getForIter(index).?),
            .return_stmt => try self.emitReturn(self.ir.getOptValue(index)),
            .switch_stmt => try self.emitSwitch(self.ir.getSwitchStmt(index).?),
            .block, .program => try self.emitBlock(self.ir.getBlock(index).?),
            .empty_stmt, .debugger_stmt => {},

            // JSX - emit as h() calls
            .jsx_element, .jsx_fragment => try self.emitJsxElementByIndex(index),

            // JSX text content - emit as string constant
            .jsx_text => {
                const str_idx = self.ir.getJsxText(index).?;
                const str = self.ir.getString(str_idx) orelse "";
                const idx = try self.addStringConstant(str);
                try self.emitPushConst(idx);
                self.pushStack(1);
            },

            // JSX expression container - emit the inner expression
            .jsx_expr_container => {
                if (self.ir.getOptValue(index)) |inner_expr| {
                    if (inner_expr != null_node) {
                        try self.emitNode(inner_expr);
                    }
                }
            },

            else => {},
        }
    }

    // ============ Literal Emission ============

    fn emitInteger(self: *CodeGen, val: i32) !void {
        switch (val) {
            0 => try self.emit(.push_0),
            1 => try self.emit(.push_1),
            2 => try self.emit(.push_2),
            3 => try self.emit(.push_3),
            -128...-1, 4...127 => {
                try self.emit(.push_i8);
                try self.emitByte(@bitCast(@as(i8, @intCast(val))));
            },
            else => {
                const idx = try self.addConstant(JSValue.fromInt(val));
                try self.emitPushConst(idx);
            },
        }
        self.pushStack(1);
    }

    fn emitFloat(self: *CodeGen, float_idx: u16) !void {
        const f = self.ir.getFloat(float_idx) orelse 0.0;
        // Float64 needs heap allocation via Float64Box
        const float_box = try self.allocator.create(JSValue.Float64Box);
        float_box.* = .{
            .header = heap.MemBlockHeader.init(.float64, @sizeOf(JSValue.Float64Box)),
            ._pad = 0,
            .value = f,
        };
        const idx = try self.addConstant(JSValue.fromPtr(float_box));
        try self.emitPushConst(idx);
        self.pushStack(1);
    }

    fn emitString(self: *CodeGen, str_idx: u16) !void {
        const str = self.ir.getString(str_idx) orelse "";
        const idx = try self.addStringConstant(str);
        try self.emitPushConst(idx);
        self.pushStack(1);
    }

    fn emitBool(self: *CodeGen, val: bool) !void {
        try self.emit(if (val) .push_true else .push_false);
        self.pushStack(1);
    }

    // ============ Variable Access ============

    fn emitIdentifier(self: *CodeGen, binding: BindingRef) !void {
        switch (binding.kind) {
            .local, .argument => {
                switch (binding.slot) {
                    0 => try self.emit(.get_loc_0),
                    1 => try self.emit(.get_loc_1),
                    2 => try self.emit(.get_loc_2),
                    3 => try self.emit(.get_loc_3),
                    else => {
                        try self.emit(.get_loc);
                        try self.emitByte(@truncate(binding.slot)); // Local slots are u8 (checked at allocation)
                    },
                }
            },
            .upvalue => {
                try self.emit(.get_upvalue);
                try self.emitByte(@truncate(binding.slot)); // Upvalue slots are u8
            },
            .global => {
                try self.emit(.get_global);
                try self.emitU16(binding.slot);
            },
        }
        self.pushStack(1);
    }

    fn emitSetBinding(self: *CodeGen, binding: BindingRef) !void {
        switch (binding.kind) {
            .local, .argument => {
                switch (binding.slot) {
                    0 => try self.emit(.put_loc_0),
                    1 => try self.emit(.put_loc_1),
                    2 => try self.emit(.put_loc_2),
                    3 => try self.emit(.put_loc_3),
                    else => {
                        try self.emit(.put_loc);
                        try self.emitByte(@truncate(binding.slot)); // Local slots are u8
                    },
                }
            },
            .upvalue => {
                try self.emit(.put_upvalue);
                try self.emitByte(@truncate(binding.slot)); // Upvalue slots are u8
            },
            .global => {
                try self.emit(.put_global);
                try self.emitU16(binding.slot);
            },
        }
        self.popStack(1);
    }

    // ============ Expression Emission ============

    /// Try to get a constant integer value from a node
    fn tryGetConstantInt(self: *CodeGen, node_idx: NodeIndex) ?i32 {
        const tag = self.ir.getTag(node_idx) orelse return null;
        return switch (tag) {
            .lit_int => self.ir.getIntValue(node_idx),
            else => null,
        };
    }

    /// Try to fold a binary operation on two constant integers
    fn tryFoldIntBinaryOp(op: BinaryOp, a: i32, b: i32) ?i32 {
        return switch (op) {
            .add => blk: {
                const result = @addWithOverflow(a, b);
                break :blk if (result[1] == 0) result[0] else null;
            },
            .sub => blk: {
                const result = @subWithOverflow(a, b);
                break :blk if (result[1] == 0) result[0] else null;
            },
            .mul => blk: {
                const result = @mulWithOverflow(a, b);
                break :blk if (result[1] == 0) result[0] else null;
            },
            .mod => if (b != 0) @mod(a, b) else null,
            .bit_and => a & b,
            .bit_or => a | b,
            .bit_xor => a ^ b,
            .shl => blk: {
                const shift: u5 = @intCast(@as(u32, @bitCast(b)) & 31);
                break :blk a << shift;
            },
            .shr => blk: {
                const shift: u5 = @intCast(@as(u32, @bitCast(b)) & 31);
                break :blk a >> shift;
            },
            else => null, // Division, comparison, etc. not folded
        };
    }

    /// Try to emit a fused arithmetic-modulo opcode for pattern: (a op b) % divisor
    /// Returns error if pattern doesn't match
    fn tryEmitFusedArithMod(self: *CodeGen, left_expr: NodeIndex, divisor: i32) !void {
        const tag = self.ir.getTag(left_expr) orelse return error.PatternNotMatched;

        // Check if left expression is a binary add/sub/mul
        if (tag != .binary_op) return error.PatternNotMatched;

        const inner_binary = self.ir.getBinary(left_expr) orelse return error.PatternNotMatched;
        const fused_opcode: Opcode = switch (inner_binary.op) {
            .add => .add_mod,
            .sub => .sub_mod,
            .mul => .mul_mod,
            else => return error.PatternNotMatched,
        };

        // Emit the inner operands
        try self.emitNode(inner_binary.left);
        try self.emitNode(inner_binary.right);

        // Add divisor to constant pool
        const divisor_idx = try self.addConstant(JSValue.fromInt(divisor));

        // Emit the fused opcode with divisor constant index
        try self.emit(fused_opcode);
        try self.emitU16(divisor_idx);
        self.popStack(1); // Two operands -> one result
    }

    fn emitBinaryOp(self: *CodeGen, binary: Node.BinaryExpr) !void {
        // Short-circuit operators need special handling
        switch (binary.op) {
            .and_op => return self.emitShortCircuitAnd(binary),
            .or_op => return self.emitShortCircuitOr(binary),
            .nullish => return self.emitNullishCoalescing(binary),
            else => {},
        }

        // Try constant folding for integer operands
        if (self.tryGetConstantInt(binary.left)) |left_val| {
            if (self.tryGetConstantInt(binary.right)) |right_val| {
                if (tryFoldIntBinaryOp(binary.op, left_val, right_val)) |result| {
                    try self.emitInteger(result);
                    return;
                }
            }
        }

        // Pattern: (a op b) % constant - emit fused opcode
        if (binary.op == .mod) {
            if (self.tryGetConstantInt(binary.right)) |divisor| {
                if (divisor > 0) {
                    if (self.tryEmitFusedArithMod(binary.left, divisor)) |_| {
                        return;
                    } else |_| {
                        // Fused pattern didn't match, try simple mod_const
                        try self.emitNode(binary.left);
                        // Use inline i8 opcode for small divisors (1-127)
                        if (divisor <= 127) {
                            try self.emit(.mod_const_i8);
                            try self.emitByte(@intCast(divisor));
                        } else {
                            // Fall back to constant pool for larger divisors
                            const divisor_idx = try self.addConstant(JSValue.fromInt(divisor));
                            try self.emit(.mod_const);
                            try self.emitU16(divisor_idx);
                        }
                        return;
                    }
                }
            }
        }

        // Pattern: x >> 1 - emit optimized shr_1 opcode
        if (binary.op == .shr) {
            if (self.tryGetConstantInt(binary.right)) |shift_amt| {
                if (shift_amt == 1) {
                    try self.emitNode(binary.left);
                    try self.emit(.shr_1);
                    return;
                }
            }
        }

        // Pattern: x * 2 - emit optimized mul_2 opcode
        if (binary.op == .mul) {
            if (self.tryGetConstantInt(binary.right)) |val| {
                if (val == 2) {
                    try self.emitNode(binary.left);
                    try self.emit(.mul_2);
                    return;
                }
                // x * small_constant -> mul_const_i8
                if (val >= -128 and val <= 127) {
                    try self.emitNode(binary.left);
                    try self.emit(.mul_const_i8);
                    try self.emitByte(@bitCast(@as(i8, @intCast(val))));
                    return;
                }
            }
            // Also check left operand: 2 * x
            if (self.tryGetConstantInt(binary.left)) |val| {
                if (val == 2) {
                    try self.emitNode(binary.right);
                    try self.emit(.mul_2);
                    return;
                }
                // small_constant * x -> mul_const_i8
                if (val >= -128 and val <= 127) {
                    try self.emitNode(binary.right);
                    try self.emit(.mul_const_i8);
                    try self.emitByte(@bitCast(@as(i8, @intCast(val))));
                    return;
                }
            }
        }

        // Pattern: x + small_constant -> add_const_i8
        if (binary.op == .add) {
            if (self.tryGetConstantInt(binary.right)) |val| {
                if (val >= -128 and val <= 127) {
                    try self.emitNode(binary.left);
                    try self.emit(.add_const_i8);
                    try self.emitByte(@bitCast(@as(i8, @intCast(val))));
                    return;
                }
            }
            // small_constant + x -> add_const_i8 (commutative)
            if (self.tryGetConstantInt(binary.left)) |val| {
                if (val >= -128 and val <= 127) {
                    try self.emitNode(binary.right);
                    try self.emit(.add_const_i8);
                    try self.emitByte(@bitCast(@as(i8, @intCast(val))));
                    return;
                }
            }
        }

        // Pattern: x - small_constant -> sub_const_i8
        if (binary.op == .sub) {
            if (self.tryGetConstantInt(binary.right)) |val| {
                if (val >= -128 and val <= 127) {
                    try self.emitNode(binary.left);
                    try self.emit(.sub_const_i8);
                    try self.emitByte(@bitCast(@as(i8, @intCast(val))));
                    return;
                }
            }
        }

        // Pattern: x < small_constant -> lt_const_i8 (common in loop conditions)
        if (binary.op == .lt) {
            if (self.tryGetConstantInt(binary.right)) |val| {
                if (val >= -128 and val <= 127) {
                    try self.emitNode(binary.left);
                    try self.emit(.lt_const_i8);
                    try self.emitByte(@bitCast(@as(i8, @intCast(val))));
                    return;
                }
            }
        }

        // Pattern: x <= small_constant -> le_const_i8
        if (binary.op == .lte) {
            if (self.tryGetConstantInt(binary.right)) |val| {
                if (val >= -128 and val <= 127) {
                    try self.emitNode(binary.left);
                    try self.emit(.le_const_i8);
                    try self.emitByte(@bitCast(@as(i8, @intCast(val))));
                    return;
                }
            }
        }

        try self.emitNode(binary.left);
        try self.emitNode(binary.right);

        const opcode: Opcode = switch (binary.op) {
            .add => .add,
            .sub => .sub,
            .mul => .mul,
            .div => .div,
            .mod => .mod,
            .pow => .pow,
            .eq => .eq,
            .neq => .neq,
            .strict_eq => .strict_eq,
            .strict_neq => .strict_neq,
            .lt => .lt,
            .lte => .lte,
            .gt => .gt,
            .gte => .gte,
            .bit_and => .bit_and,
            .bit_or => .bit_or,
            .bit_xor => .bit_xor,
            .shl => .shl,
            .shr => .shr,
            .ushr => .ushr,
            .instanceof => .instanceof,
            else => .nop,
        };

        try self.emit(opcode);
        self.popStack(1); // Two operands -> one result
    }

    fn emitShortCircuitAnd(self: *CodeGen, binary: Node.BinaryExpr) !void {
        try self.emitNode(binary.left);
        try self.emit(.dup);
        self.pushStack(1);

        const false_label = try self.createLabel();
        try self.emitJump(.if_false, false_label);
        self.popStack(1);

        try self.emit(.drop);
        self.popStack(1);
        try self.emitNode(binary.right);

        try self.placeLabel(false_label);
    }

    fn emitShortCircuitOr(self: *CodeGen, binary: Node.BinaryExpr) !void {
        try self.emitNode(binary.left);
        try self.emit(.dup);
        self.pushStack(1);

        const true_label = try self.createLabel();
        try self.emitJump(.if_true, true_label);
        self.popStack(1);

        try self.emit(.drop);
        self.popStack(1);
        try self.emitNode(binary.right);

        try self.placeLabel(true_label);
    }

    fn emitNullishCoalescing(self: *CodeGen, binary: Node.BinaryExpr) !void {
        try self.emitNode(binary.left);
        try self.emit(.dup);
        self.pushStack(1);

        // Check if null or undefined
        try self.emit(.push_null);
        self.pushStack(1);
        try self.emit(.strict_eq);
        self.popStack(1);

        const not_null_label = try self.createLabel();
        try self.emitJump(.if_false, not_null_label);
        self.popStack(1);

        try self.emit(.drop);
        self.popStack(1);
        try self.emitNode(binary.right);

        try self.placeLabel(not_null_label);
    }

    fn emitUnaryOp(self: *CodeGen, unary: Node.UnaryExpr) !void {
        // Try constant folding for unary operations
        if (self.tryGetConstantInt(unary.operand)) |val| {
            const folded: ?i32 = switch (unary.op) {
                .neg => blk: {
                    // Negation can overflow for MIN_INT
                    if (val == std.math.minInt(i32)) break :blk null;
                    break :blk -val;
                },
                .bit_not => ~val,
                else => null,
            };
            if (folded) |result| {
                try self.emitInteger(result);
                return;
            }
        }

        try self.emitNode(unary.operand);

        const opcode: Opcode = switch (unary.op) {
            .neg => .neg,
            .not => .not,
            .bit_not => .bit_not,
            .typeof_op => .typeof,
            .void_op => .push_undefined,
            .delete_op => .nop, // delete not fully supported
        };

        try self.emit(opcode);
    }

    fn emitTernary(self: *CodeGen, ternary: Node.TernaryExpr) !void {
        try self.emitNode(ternary.condition);

        const else_label = try self.createLabel();
        const end_label = try self.createLabel();

        try self.emitJump(.if_false, else_label);
        self.popStack(1);

        try self.emitNode(ternary.then_branch);
        try self.emitJump(.goto, end_label);

        try self.placeLabel(else_label);
        try self.emitNode(ternary.else_branch);

        try self.placeLabel(end_label);
    }

    fn emitCall(self: *CodeGen, call: Node.CallExpr) !void {
        // Check if this is a method call (callee is member access)
        const callee_tag = self.ir.getTag(call.callee) orelse {
            try self.emitNode(call.callee);
            try self.emitCallArgs(call);
            try self.emit(.call);
            try self.emitByte(call.args_count);
            self.popStack(call.args_count);
            return;
        };

        const is_method = callee_tag == .member_access or callee_tag == .optional_chain;

        if (is_method) {
            const member = self.ir.getMember(call.callee).?;
            // Method call: obj.method(args)
            // Stack: [obj] -> [obj, obj] -> [obj, method] -> [obj, method, args...] -> [result]
            try self.emitNode(member.object);
            try self.emit(.dup); // Keep object as 'this'
            self.pushStack(1);
            try self.emitGetField(member.property);

            // Emit arguments
            try self.emitCallArgs(call);

            try self.emit(.call_method);
            try self.emitByte(call.args_count);

            // Pop object + method + args, push result (method call pops 'this' too)
            self.popStack(call.args_count + 1);
        } else {
            // Regular function call
            try self.emitNode(call.callee);

            // Emit arguments
            try self.emitCallArgs(call);

            try self.emit(.call);
            try self.emitByte(call.args_count);

            // Pop callee + args, push result
            self.popStack(call.args_count);
        }
    }

    fn emitCallArgs(self: *CodeGen, call: Node.CallExpr) !void {
        var i: u8 = 0;
        while (i < call.args_count) : (i += 1) {
            const arg_idx = self.ir.getListIndex(call.args_start, i);
            try self.emitNode(arg_idx);
        }
    }

    fn emitMemberAccess(self: *CodeGen, member: Node.MemberExpr) !void {
        try self.emitNode(member.object);
        try self.emitGetField(member.property);
    }

    fn emitComputedAccess(self: *CodeGen, member: Node.MemberExpr) !void {
        try self.emitNode(member.object);
        try self.emitNode(member.computed);
        try self.emit(.get_elem);
        self.popStack(1);
    }

    fn emitAssignment(self: *CodeGen, assign: Node.AssignExpr) !void {
        const target_tag = self.ir.getTag(assign.target) orelse return;

        if (assign.op) |op| {
            // Compound assignment: get current value first
            try self.emitNode(assign.target);
            try self.emitNode(assign.value);
            try self.emit(self.binaryOpToOpcode(op));
            self.popStack(1);
        } else {
            try self.emitNode(assign.value);
        }

        // Store to target
        switch (target_tag) {
            .identifier => {
                const binding = self.ir.getBinding(assign.target).?;
                try self.emit(.dup);
                self.pushStack(1);
                try self.emitSetBinding(binding);
            },
            .member_access => {
                const member = self.ir.getMember(assign.target).?;
                // Stack: value
                // Need: object, value
                try self.emitNode(member.object);
                try self.emit(.swap);
                try self.emit(.put_field_keep);
                try self.emitU16(member.property);
                self.popStack(1);
            },
            .computed_access => {
                const member = self.ir.getMember(assign.target).?;
                try self.emitNode(member.object);
                try self.emitNode(member.computed);
                try self.emit(.rot3);
                try self.emit(.put_elem);
                self.popStack(2);
            },
            else => {},
        }
    }

    fn binaryOpToOpcode(self: *CodeGen, op: BinaryOp) Opcode {
        _ = self;
        return switch (op) {
            .add => .add,
            .sub => .sub,
            .mul => .mul,
            .div => .div,
            .mod => .mod,
            .pow => .pow,
            .bit_and => .bit_and,
            .bit_or => .bit_or,
            .bit_xor => .bit_xor,
            .shl => .shl,
            .shr => .shr,
            .ushr => .ushr,
            else => .nop,
        };
    }

    fn emitArrayLiteral(self: *CodeGen, array: Node.ArrayExpr) !void {
        try self.emit(.new_array);
        try self.emitU16(array.elements_count);
        self.pushStack(1);

        var i: u16 = 0;
        while (i < array.elements_count) : (i += 1) {
            const elem_idx = self.ir.getListIndex(array.elements_start, i);

            // Duplicate array reference for put_elem
            try self.emit(.dup);
            self.pushStack(1);

            // Push index
            try self.emitSmallInt(@intCast(i));

            // Push element value
            if (elem_idx != null_node) {
                try self.emitNode(elem_idx);
            } else {
                try self.emit(.push_undefined);
                self.pushStack(1);
            }

            // Store element: arr[i] = value
            try self.emit(.put_elem);
            self.popStack(3); // pop arr, index, value
        }
        // Array remains on stack
    }

    /// Register a shape for object literal optimization.
    /// Returns the shape index for use in new_object_literal opcode.
    fn registerShape(self: *CodeGen, atoms: []const js_object.Atom) !u16 {
        // Hash the shape for deduplication
        var hasher = std.hash.Wyhash.init(0);
        for (atoms) |atom| {
            hasher.update(std.mem.asBytes(&@intFromEnum(atom)));
        }
        const hash = hasher.final();

        // Check for existing shape with same hash
        if (self.shape_dedup.get(hash)) |existing_idx| {
            return existing_idx;
        }

        // Register new shape
        const shape_idx: u16 = @intCast(self.shapes.items.len);
        const atoms_copy = try self.allocator.dupe(js_object.Atom, atoms);
        try self.shapes.append(self.allocator, atoms_copy);
        try self.shape_dedup.put(self.allocator, hash, shape_idx);
        return shape_idx;
    }

    fn emitObjectLiteral(self: *CodeGen, object: Node.ObjectExpr) !void {
        // Try to collect static string keys for shape pre-compilation
        var static_atoms: std.ArrayList(js_object.Atom) = .empty;
        defer static_atoms.deinit(self.allocator);

        var all_static = true;
        var i: u16 = 0;
        while (i < object.properties_count) : (i += 1) {
            const prop_idx = self.ir.getListIndex(object.properties_start, i);
            const prop_tag = self.ir.getTag(prop_idx) orelse {
                all_static = false;
                break;
            };

            if (prop_tag != .object_property) {
                all_static = false;
                break;
            }

            const prop = self.ir.getProperty(prop_idx).?;
            const key_tag = self.ir.getTag(prop.key) orelse {
                all_static = false;
                break;
            };

            // Only optimize string literal keys
            if (key_tag != .lit_string) {
                all_static = false;
                break;
            }

            const key_str_idx = self.ir.getStringIdx(prop.key).?;
            const key_str = self.ir.getString(key_str_idx) orelse "";

            // Intern atom
            const atom: js_object.Atom = if (js_object.lookupPredefinedAtom(key_str)) |a|
                a
            else if (self.atoms) |atoms|
                try atoms.intern(key_str)
            else
                @enumFromInt(js_object.Atom.FIRST_DYNAMIC + key_str_idx);

            try static_atoms.append(self.allocator, atom);
        }

        // Use optimized path if all keys are static strings and we have at least one property
        if (all_static and static_atoms.items.len > 0) {
            try self.emitPrecompiledObjectLiteral(object, static_atoms.items);
        } else {
            try self.emitDynamicObjectLiteral(object);
        }
    }

    /// Emit object literal using pre-compiled shape (O(1) class allocation).
    fn emitPrecompiledObjectLiteral(
        self: *CodeGen,
        object: Node.ObjectExpr,
        atoms: []const js_object.Atom,
    ) !void {
        // Register shape and get index
        const shape_idx = try self.registerShape(atoms);

        // Emit new_object_literal opcode: creates object with pre-built shape
        try self.emit(.new_object_literal);
        try self.emitU16(shape_idx);
        try self.emitU8(@intCast(atoms.len));
        self.pushStack(1);

        // Emit values and direct slot writes (in declaration order)
        var i: u16 = 0;
        while (i < object.properties_count) : (i += 1) {
            const prop_idx = self.ir.getListIndex(object.properties_start, i);
            const prop = self.ir.getProperty(prop_idx).?;

            try self.emit(.dup);
            self.pushStack(1);

            try self.emitNode(prop.value); // Push value

            try self.emit(.set_slot);
            try self.emitU8(@intCast(i)); // Slot index = property index
            self.popStack(2); // Pops value and object copy
        }
    }

    /// Emit object literal using dynamic property setting (original behavior).
    fn emitDynamicObjectLiteral(self: *CodeGen, object: Node.ObjectExpr) !void {
        try self.emit(.new_object);
        self.pushStack(1);

        var i: u16 = 0;
        while (i < object.properties_count) : (i += 1) {
            const prop_idx = self.ir.getListIndex(object.properties_start, i);
            const prop_tag = self.ir.getTag(prop_idx) orelse continue;

            if (prop_tag == .object_property) {
                const prop = self.ir.getProperty(prop_idx).?;

                try self.emit(.dup); // Duplicate object reference
                self.pushStack(1);

                // Get property key
                const key_tag = self.ir.getTag(prop.key) orelse continue;
                if (key_tag == .lit_string) {
                    const key_str_idx = self.ir.getStringIdx(prop.key).?;
                    // Key is a string constant, use put_field with atom
                    try self.emitNode(prop.value);
                    // Get string from IR constant pool and look up/intern atom
                    const key_str = self.ir.getString(key_str_idx) orelse "";
                    const atom_idx: u16 = if (js_object.lookupPredefinedAtom(key_str)) |atom|
                        @intCast(@intFromEnum(atom))
                    else if (self.atoms) |atoms|
                        @intCast(@intFromEnum(try atoms.intern(key_str)))
                    else
                        @intCast(js_object.Atom.FIRST_DYNAMIC + key_str_idx);
                    try self.emitPutField(atom_idx);
                    self.popStack(2);
                } else {
                    // Computed key
                    try self.emitNode(prop.key);
                    try self.emitNode(prop.value);
                    try self.emit(.put_elem);
                    self.popStack(3);
                }
            }
        }
    }

    fn emitFunctionExpr(self: *CodeGen, node_idx: NodeIndex, func: Node.FunctionExpr) !void {
        _ = node_idx;

        // Save current codegen state
        const saved_code = self.code;
        const saved_constants = self.constants;
        const saved_upvalue_info = self.upvalue_info;
        const saved_labels = self.labels;
        const saved_pending_jumps = self.pending_jumps;
        const saved_loop_stack = self.loop_stack;
        const saved_scope = self.current_scope;
        const saved_max_stack = self.max_stack_depth;
        const saved_current_stack = self.current_stack_depth;
        const saved_ic_cache_idx = self.ic_cache_idx;

        // Reset for function compilation
        self.code = std.ArrayList(u8).empty;
        self.constants = std.ArrayList(JSValue).empty;
        self.upvalue_info = std.ArrayList(UpvalueInfo).empty;
        self.labels = std.ArrayList(Label).empty;
        self.pending_jumps = std.ArrayList(PendingJump).empty;
        self.loop_stack = std.ArrayList(LoopContext).empty;
        self.current_scope = func.scope_id;
        self.max_stack_depth = 0;
        self.current_stack_depth = 0;
        self.ic_cache_idx = 0;

        // Compile function body
        try self.emitNode(func.body);
        try self.emit(.ret_undefined);
        try self.resolveJumps();

        // Get upvalue info from scope
        const scope = self.scopes.getScope(func.scope_id);
        var upvalue_info_list: std.ArrayList(UpvalueInfo) = .empty;
        for (scope.upvalues.items) |uv| {
            try upvalue_info_list.append(self.allocator, .{
                .is_local = uv.is_direct,
                .index = uv.outer_slot,
            });
        }

        // Create FunctionBytecode on heap with errdefer cleanup for error paths
        const func_bc = try self.allocator.create(FunctionBytecode);
        errdefer self.allocator.destroy(func_bc);

        const code_copy = try self.allocator.dupe(u8, self.code.items);
        errdefer self.allocator.free(code_copy);

        const consts_copy = try self.allocator.dupe(JSValue, self.constants.items);
        errdefer self.allocator.free(consts_copy);

        const upvalue_copy = try self.allocator.dupe(UpvalueInfo, upvalue_info_list.items);

        func_bc.* = .{
            .header = .{},
            .name_atom = func.name_atom,
            .arg_count = func.params_count,
            .local_count = self.scopes.getLocalCount(func.scope_id),
            .stack_size = self.max_stack_depth,
            .flags = .{
                .is_generator = func.flags.is_generator,
                .is_async = func.flags.is_async,
                .has_rest = func.flags.has_rest_param,
            },
            .upvalue_count = @intCast(scope.upvalues.items.len),
            .upvalue_info = upvalue_copy,
            .code = code_copy,
            .constants = consts_copy,
            .source_map = null,
        };

        // Clean up function-local state (code/constants are now owned by func_bc copies)
        self.code.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        self.upvalue_info.deinit(self.allocator);
        self.labels.deinit(self.allocator);
        self.pending_jumps.deinit(self.allocator);
        self.loop_stack.deinit(self.allocator);
        upvalue_info_list.deinit(self.allocator);

        // Restore parent state
        self.code = saved_code;
        self.constants = saved_constants;
        self.upvalue_info = saved_upvalue_info;
        self.labels = saved_labels;
        self.pending_jumps = saved_pending_jumps;
        self.loop_stack = saved_loop_stack;
        self.current_scope = saved_scope;
        self.max_stack_depth = saved_max_stack;
        self.current_stack_depth = saved_current_stack;
        self.ic_cache_idx = saved_ic_cache_idx;

        // Add function bytecode to parent constants and emit opcode
        const func_idx = try self.addConstant(JSValue.fromExternPtr(func_bc));
        const upvalue_count: u8 = @intCast(scope.upvalues.items.len);

        if (upvalue_count > 0) {
            try self.emit(.make_closure);
            try self.emitU16(func_idx);
            try self.emitByte(upvalue_count);
        } else {
            try self.emit(.make_function);
            try self.emitU16(func_idx);
        }
        self.pushStack(1);
    }

    fn emitTemplateLiteral(self: *CodeGen, template: Node.TemplateExpr) !void {
        // Emit first string part
        var i: u8 = 0;

        while (i < template.parts_count) : (i += 1) {
            const part_idx = self.ir.getListIndex(template.parts_start, i);
            const part_tag = self.ir.getTag(part_idx) orelse continue;

            if (part_tag == .template_part_string) {
                const str_idx = self.ir.getStringIdx(part_idx).?;
                try self.emitString(str_idx);
            } else if (part_tag == .template_part_expr) {
                if (self.ir.getOptValue(part_idx)) |expr| {
                    try self.emitNode(expr);
                    // Convert to string if needed (simplified)
                }
            }

            // Concatenate parts
            if (i > 0) {
                try self.emit(.add);
                self.popStack(1);
            }
        }

        if (template.parts_count == 0) {
            try self.emitString(0); // Empty string
        }
    }

    // ============ Statement Emission ============

    fn emitVarDecl(self: *CodeGen, decl: Node.VarDecl) !void {
        if (decl.init == null_node) return;

        // Check for destructuring pattern
        if (decl.pattern != null_node) {
            // Emit the source value
            try self.emitNode(decl.init);
            // Emit destructuring
            try self.emitDestructuringPattern(decl.pattern);
            // Drop the source value
            try self.emit(.drop);
            self.popStack(1);
        } else {
            // Simple variable declaration
            try self.emitNode(decl.init);
            try self.emitSetBinding(decl.binding);
        }
    }

    fn emitDestructuringPattern(self: *CodeGen, pattern: NodeIndex) anyerror!void {
        const pattern_tag = self.ir.getTag(pattern) orelse return;

        switch (pattern_tag) {
            .object_pattern => try self.emitObjectPattern(self.ir.getArray(pattern).?),
            .array_pattern => try self.emitArrayPattern(self.ir.getArray(pattern).?),
            else => {},
        }
    }

    fn emitObjectPattern(self: *CodeGen, array_data: Node.ArrayExpr) anyerror!void {
        // For each property in the pattern, extract from the source object
        var i: u16 = 0;
        while (i < array_data.elements_count) : (i += 1) {
            const elem_idx = self.ir.getListIndex(array_data.elements_start, i);
            const elem_tag = self.ir.getTag(elem_idx) orelse continue;
            if (elem_tag != .pattern_element) continue;

            const elem = self.ir.getPatternElem(elem_idx).?;

            switch (elem.kind) {
                .simple => {
                    // Stack: source_obj
                    try self.emit(.dup); // Duplicate source for next property
                    self.pushStack(1);

                    // Get the property value using the atom
                    try self.emitGetField(elem.key_atom);

                    // Handle default value if present
                    if (elem.default_value != null_node) {
                        try self.emitDefaultValue(elem.default_value);
                    }

                    // Store to binding
                    try self.emitSetBinding(elem.binding);
                },
                .object, .array => {
                    // Nested destructuring
                    try self.emit(.dup);
                    self.pushStack(1);

                    // Get the property value using the atom
                    try self.emitGetField(elem.key_atom);

                    // Handle default
                    if (elem.default_value != null_node) {
                        try self.emitDefaultValue(elem.default_value);
                    }

                    // Recursively destructure the nested pattern
                    if (elem.key != null_node) {
                        try self.emitDestructuringPattern(elem.key);
                    }

                    // Drop the nested value
                    try self.emit(.drop);
                    self.popStack(1);
                },
                .rest => {
                    // TODO: Implement rest element for objects
                    // Requires creating a new object with remaining properties
                },
            }
        }
    }

    fn emitArrayPattern(self: *CodeGen, array_data: Node.ArrayExpr) anyerror!void {
        // For each element in the pattern, extract by index from the source array
        var i: u16 = 0;
        var index: u8 = 0;
        while (i < array_data.elements_count) : (i += 1) {
            const elem_idx = self.ir.getListIndex(array_data.elements_start, i);
            const elem_tag = self.ir.getTag(elem_idx) orelse {
                index += 1; // Skip hole
                continue;
            };
            if (elem_tag != .pattern_element) {
                index += 1;
                continue;
            }

            const elem = self.ir.getPatternElem(elem_idx).?;

            switch (elem.kind) {
                .simple => {
                    // Stack: source_arr
                    try self.emit(.dup); // Duplicate source for next element
                    self.pushStack(1);

                    // Push array index
                    try self.emitSmallInt(index);

                    // Get element by index
                    try self.emit(.get_elem);
                    self.popStack(1); // get_elem pops index

                    // Handle default value if present
                    if (elem.default_value != null_node) {
                        try self.emitDefaultValue(elem.default_value);
                    }

                    // Store to binding
                    try self.emitSetBinding(elem.binding);
                },
                .object, .array => {
                    // Nested destructuring
                    try self.emit(.dup);
                    self.pushStack(1);

                    // Push array index
                    try self.emitSmallInt(index);

                    // Get element
                    try self.emit(.get_elem);
                    self.popStack(1);

                    // Handle default
                    if (elem.default_value != null_node) {
                        try self.emitDefaultValue(elem.default_value);
                    }

                    // Find the nested pattern node
                    // elem.key holds the nested pattern for arrays
                    if (elem.key != null_node) {
                        try self.emitDestructuringPattern(elem.key);
                    }

                    // Drop the nested value
                    try self.emit(.drop);
                    self.popStack(1);
                },
                .rest => {
                    // TODO: Implement rest element for arrays
                    // Requires slicing remaining elements into new array
                },
            }

            index += 1;
        }
    }

    fn emitSmallInt(self: *CodeGen, val: u8) !void {
        switch (val) {
            0 => try self.emit(.push_0),
            1 => try self.emit(.push_1),
            2 => try self.emit(.push_2),
            3 => try self.emit(.push_3),
            else => {
                try self.emit(.push_i8);
                try self.emitByte(val);
            },
        }
        self.pushStack(1);
    }

    fn emitDefaultValue(self: *CodeGen, default_value: NodeIndex) !void {
        // Pattern: if value === undefined, use default
        // Stack: value
        // After: value-or-default
        try self.emit(.dup);
        self.pushStack(1);
        try self.emit(.push_undefined);
        self.pushStack(1);
        try self.emit(.strict_eq);
        self.popStack(1);

        const else_label = try self.createLabel();
        const end_label = try self.createLabel();

        try self.emitJump(.if_false, else_label);
        self.popStack(1);

        // Value was undefined, use default
        try self.emit(.drop);
        self.popStack(1);
        try self.emitNode(default_value);
        try self.emitJump(.goto, end_label);

        try self.placeLabel(else_label);
        // Value was not undefined, keep it (already on stack)

        try self.placeLabel(end_label);
    }

    fn emitIfStmt(self: *CodeGen, if_stmt: Node.IfStmt) !void {
        try self.emitNode(if_stmt.condition);

        const else_label = try self.createLabel();

        if (if_stmt.else_branch != null_node) {
            const end_label = try self.createLabel();

            try self.emitJump(.if_false, else_label);
            self.popStack(1);

            try self.emitNode(if_stmt.then_branch);
            try self.emitJump(.goto, end_label);

            try self.placeLabel(else_label);
            try self.emitNode(if_stmt.else_branch);

            try self.placeLabel(end_label);
        } else {
            try self.emitJump(.if_false, else_label);
            self.popStack(1);
            try self.emitNode(if_stmt.then_branch);
            try self.placeLabel(else_label);
        }
    }

    fn emitForLoop(self: *CodeGen, loop: Node.LoopStmt) !void {
        // Init
        if (loop.init != null_node) {
            try self.emitNode(loop.init);
            if (self.ir.getTag(loop.init)) |init_tag| {
                if (init_tag != .var_decl) {
                    try self.emit(.drop);
                    self.popStack(1);
                }
            }
        }

        const loop_start = try self.createLabel();
        const loop_end = try self.createLabel();
        const continue_label = try self.createLabel();

        try self.loop_stack.append(self.allocator, .{
            .break_label = loop_end,
            .continue_label = continue_label,
        });

        try self.placeLabel(loop_start);

        // Condition
        if (loop.condition != null_node) {
            try self.emitNode(loop.condition);
            try self.emitJump(.if_false, loop_end);
            self.popStack(1);
        }

        // Body
        try self.emitNode(loop.body);

        try self.placeLabel(continue_label);

        // Update
        if (loop.update != null_node) {
            try self.emitNode(loop.update);
            try self.emit(.drop);
            self.popStack(1);
        }

        try self.emitJump(.goto, loop_start);
        try self.placeLabel(loop_end);

        _ = self.loop_stack.pop();
    }

    fn emitForIterLoop(self: *CodeGen, for_iter: Node.ForIterStmt) !void {
        // For-of loop using optimized for_of_next superinstruction
        // Stack layout: [iterable, index]
        // for_of_next combines: bounds check + element fetch + index increment

        const loop_start = try self.createLabel();
        const loop_end = try self.createLabel();
        const continue_label = try self.createLabel();

        try self.loop_stack.append(self.allocator, .{
            .break_label = loop_end,
            .continue_label = continue_label,
        });

        // Push iterable and initial index (0)
        try self.emitNode(for_iter.iterable);
        try self.emit(.push_0);
        self.pushStack(1);

        // Loop start
        // Stack: [iterable, index]
        try self.placeLabel(loop_start);

        // for_of_next: check bounds, push element, increment index
        // If index >= length, jumps to loop_end
        // Stack: [iterable, index] -> [iterable, index+1, element]
        const binding = for_iter.binding;
        if ((binding.kind == .local or binding.kind == .argument) and binding.slot <= 255) {
            // Fused opcode: for_of_next + put_loc (stores directly to local)
            try self.emit(.for_of_next_put_loc);
            try self.emitByte(@truncate(binding.slot));
            try self.emitI16Placeholder(loop_end);
            // No stack change - element goes directly to local
        } else {
            // Standard path: push element then store
            try self.emitJump(.for_of_next, loop_end);
            self.pushStack(1); // Element pushed on success
            try self.emitSetBinding(for_iter.binding);
        }
        // Stack: [iterable, index]

        // Execute loop body
        try self.emitNode(for_iter.body);

        // Continue label (for continue statements)
        try self.placeLabel(continue_label);

        // Jump back to loop start (index already incremented by for_of_next)
        try self.emitJump(.goto, loop_start);

        // Loop end - cleanup
        try self.placeLabel(loop_end);
        // Stack: [iterable, index]
        try self.emit(.drop); // drop index
        self.popStack(1);
        try self.emit(.drop); // drop iterable
        self.popStack(1);

        _ = self.loop_stack.pop();
    }

    fn emitReturn(self: *CodeGen, value_opt: ?NodeIndex) !void {
        if (value_opt) |val| {
            try self.emitNode(val);
            try self.emit(.ret);
        } else {
            try self.emit(.ret_undefined);
        }
    }

    fn emitSwitch(self: *CodeGen, switch_stmt: Node.SwitchStmt) !void {
        try self.emitNode(switch_stmt.discriminant);

        const end_label = try self.createLabel();
        var case_labels = std.ArrayList(u32).empty;
        defer case_labels.deinit(self.allocator);
        var default_label: ?u32 = null;

        // Create labels for each case
        var i: u8 = 0;
        while (i < switch_stmt.cases_count) : (i += 1) {
            const case_idx = self.ir.getListIndex(switch_stmt.cases_start, i);
            const label = try self.createLabel();
            try case_labels.append(self.allocator, label);

            const case_clause = self.ir.getCaseClause(case_idx) orelse continue;
            if (case_clause.test_expr == null_node) {
                default_label = label;
            }
        }

        // Emit comparisons
        i = 0;
        while (i < switch_stmt.cases_count) : (i += 1) {
            const case_idx = self.ir.getListIndex(switch_stmt.cases_start, i);
            const case_clause = self.ir.getCaseClause(case_idx) orelse continue;

            if (case_clause.test_expr != null_node) {
                try self.emit(.dup);
                self.pushStack(1);
                try self.emitNode(case_clause.test_expr);
                try self.emit(.strict_eq);
                self.popStack(1);
                try self.emitJump(.if_true, case_labels.items[i]);
                self.popStack(1);
            }
        }

        // Jump to default or end
        if (default_label) |lbl| {
            try self.emitJump(.goto, lbl);
        } else {
            try self.emitJump(.goto, end_label);
        }

        // Drop discriminant
        try self.emit(.drop);
        self.popStack(1);

        // Emit case bodies
        i = 0;
        while (i < switch_stmt.cases_count) : (i += 1) {
            const case_idx = self.ir.getListIndex(switch_stmt.cases_start, i);
            try self.placeLabel(case_labels.items[i]);

            const case_clause = self.ir.getCaseClause(case_idx) orelse continue;

            var j: u16 = 0;
            while (j < case_clause.body_count) : (j += 1) {
                const body_idx = self.ir.getListIndex(case_clause.body_start, j);
                try self.emitNode(body_idx);
            }
        }

        try self.placeLabel(end_label);
    }

    fn emitBlock(self: *CodeGen, block: Node.BlockData) !void {
        var i: u16 = 0;
        while (i < block.stmts_count) : (i += 1) {
            const stmt_idx = self.ir.getListIndex(block.stmts_start, i);
            try self.emitNode(stmt_idx);
        }
    }

    fn emitJsxElementByIndex(self: *CodeGen, index: NodeIndex) !void {
        // Emit JSX as h(tag, props, ...children) call
        const elem = self.ir.getJsxElement(index) orelse return;
        const tag = self.ir.getTag(index) orelse return;

        // Push h function reference (predefined atom)
        try self.emit(.get_global);
        try self.emitU16(@intFromEnum(js_object.Atom.h));
        self.pushStack(1);

        // Push tag name
        if (tag == .jsx_fragment) {
            // Fragment - use null or Fragment symbol
            try self.emit(.push_null);
            self.pushStack(1);
        } else if (elem.is_component) {
            // Component - push as identifier reference
            try self.emit(.get_global);
            try self.emitU16(elem.tag_atom);
            self.pushStack(1);
        } else {
            // HTML tag - push as string
            const tag_idx = try self.addStringConstant(self.ir.getString(elem.tag_atom) orelse "div");
            try self.emitPushConst(tag_idx);
            self.pushStack(1);
        }

        // Push props object
        if (elem.props_count > 0) {
            try self.emit(.new_object);
            self.pushStack(1);
            // Add props
            var i: u8 = 0;
            while (i < elem.props_count) : (i += 1) {
                const prop_idx = self.ir.getListIndex(elem.props_start, i);
                const prop_tag = self.ir.getTag(prop_idx) orelse continue;
                if (prop_tag == .jsx_attribute) {
                    const attr = self.ir.getJsxAttr(prop_idx) orelse continue;
                    try self.emit(.dup);
                    self.pushStack(1);
                    if (attr.value != null_node) {
                        try self.emitNode(attr.value);
                    } else {
                        try self.emit(.push_true);
                        self.pushStack(1);
                    }
                    try self.emitPutField(attr.name_atom);
                    self.popStack(2);
                }
            }
        } else {
            try self.emit(.push_null);
            self.pushStack(1);
        }

        // Push children
        var child_count: u8 = 0;
        if (elem.children_count > 0) {
            var i: u8 = 0;
            while (i < elem.children_count) : (i += 1) {
                const child_idx = self.ir.getListIndex(elem.children_start, i);
                try self.emitNode(child_idx);
                child_count += 1;
            }
        }

        // Call h(tag, props, ...children)
        try self.emit(.call);
        try self.emitByte(2 + child_count);
        self.popStack(2 + child_count);
    }

    // ============ Instruction Helpers ============

    fn emit(self: *CodeGen, opcode: Opcode) !void {
        try self.code.append(self.allocator, @intFromEnum(opcode));
    }

    fn emitByte(self: *CodeGen, b: u8) !void {
        try self.code.append(self.allocator, b);
    }

    fn emitU8(self: *CodeGen, val: u8) !void {
        try self.code.append(self.allocator, val);
    }

    fn emitU16(self: *CodeGen, val: u16) !void {
        // Little-endian: low byte first, then high byte
        try self.code.append(self.allocator, @truncate(val));
        try self.code.append(self.allocator, @truncate(val >> 8));
    }

    fn emitI16(self: *CodeGen, val: i16) !void {
        const unsigned: u16 = @bitCast(val);
        try self.emitU16(unsigned);
    }

    fn emitPushConst(self: *CodeGen, idx: u16) !void {
        try self.emit(.push_const);
        try self.emitU16(idx);
    }

    /// Emit get_field with inline cache if cache slots available
    /// Falls back to regular get_field when IC_CACHE_SIZE exceeded
    fn emitGetField(self: *CodeGen, atom_idx: u16) !void {
        if (self.ic_cache_idx < IC_CACHE_SIZE) {
            try self.emit(.get_field_ic);
            try self.emitU16(atom_idx);
            try self.emitU16(self.ic_cache_idx);
            self.ic_cache_idx += 1;
        } else {
            try self.emit(.get_field);
            try self.emitU16(atom_idx);
        }
    }

    /// Emit put_field with inline cache if cache slots available
    /// Falls back to regular put_field when IC_CACHE_SIZE exceeded
    fn emitPutField(self: *CodeGen, atom_idx: u16) !void {
        if (self.ic_cache_idx < IC_CACHE_SIZE) {
            try self.emit(.put_field_ic);
            try self.emitU16(atom_idx);
            try self.emitU16(self.ic_cache_idx);
            self.ic_cache_idx += 1;
        } else {
            try self.emit(.put_field);
            try self.emitU16(atom_idx);
        }
    }

    // ============ Jump Handling ============

    fn createLabel(self: *CodeGen) !u32 {
        const id = @as(u32, @intCast(self.labels.items.len));
        try self.labels.append(self.allocator, .{
            .offset = 0,
            .resolved = false,
        });
        return id;
    }

    fn placeLabel(self: *CodeGen, label_id: u32) !void {
        self.labels.items[label_id].offset = @intCast(self.code.items.len);
        self.labels.items[label_id].resolved = true;
    }

    fn emitJump(self: *CodeGen, opcode: Opcode, label_id: u32) !void {
        try self.emit(opcode);
        try self.pending_jumps.append(self.allocator, .{
            .instruction_offset = @intCast(self.code.items.len),
            .target_label = label_id,
        });
        try self.emitI16(0); // Placeholder
    }

    fn emitI16Placeholder(self: *CodeGen, label_id: u32) !void {
        try self.pending_jumps.append(self.allocator, .{
            .instruction_offset = @intCast(self.code.items.len),
            .target_label = label_id,
        });
        try self.emitI16(0);
    }

    fn resolveJumps(self: *CodeGen) !void {
        for (self.pending_jumps.items) |jump| {
            const label = self.labels.items[jump.target_label];
            if (!label.resolved) continue;

            const offset: i16 = @intCast(@as(i32, @intCast(label.offset)) - @as(i32, @intCast(jump.instruction_offset)) - 2);
            const unsigned: u16 = @bitCast(offset);

            // Write in little-endian to match interpreter's readI16
            self.code.items[jump.instruction_offset] = @truncate(unsigned);
            self.code.items[jump.instruction_offset + 1] = @truncate(unsigned >> 8);
        }
    }

    // ============ Constant Pool ============

    fn addConstant(self: *CodeGen, val: JSValue) !u16 {
        const idx = @as(u16, @intCast(self.constants.items.len));
        try self.constants.append(self.allocator, val);
        return idx;
    }

    fn addStringConstant(self: *CodeGen, str: []const u8) !u16 {
        if (self.strings) |strings| {
            // Use string table to create interned string
            const js_str = try strings.intern(str);
            return try self.addConstant(JSValue.fromPtr(js_str));
        } else {
            // Fallback: create string directly (for standalone testing)
            const js_str = try string.createString(self.allocator, str);
            return try self.addConstant(JSValue.fromPtr(js_str));
        }
    }

    fn addFunctionConstant(self: *CodeGen, node_idx: NodeIndex) !u16 {
        // Would compile function and add bytecode to constants
        _ = node_idx;
        return try self.addConstant(JSValue.undefined_val);
    }

    // ============ Stack Tracking ============

    fn pushStack(self: *CodeGen, count: u16) void {
        self.current_stack_depth += count;
        if (self.current_stack_depth > self.max_stack_depth) {
            self.max_stack_depth = self.current_stack_depth;
        }
    }

    fn popStack(self: *CodeGen, count: u16) void {
        self.current_stack_depth -|= count;
    }
};

// ============ Tests ============

test "basic codegen" {
    const allocator = std.testing.allocator;

    var nodes = NodeList.init(allocator);
    defer nodes.deinit();

    var constants = ConstantPool.init(allocator);
    defer constants.deinit();

    var scopes = ScopeAnalyzer.init(allocator);
    defer scopes.deinit();

    // Create a simple literal node
    const loc = ir.SourceLocation{ .line = 1, .column = 1, .offset = 0 };
    const lit_node = try nodes.add(Node.litInt(loc, 42));

    var gen = CodeGen.init(allocator, &nodes, &constants, &scopes);
    defer gen.deinit();

    const result = try gen.generate(lit_node);
    try std.testing.expect(result.code.len > 0);
}

test "binary op codegen" {
    const allocator = std.testing.allocator;

    var nodes = NodeList.init(allocator);
    defer nodes.deinit();

    var constants = ConstantPool.init(allocator);
    defer constants.deinit();

    var scopes = ScopeAnalyzer.init(allocator);
    defer scopes.deinit();

    const loc = ir.SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    // Create 1 + 2 - with constant folding, this should emit push_3 instead of push_1, push_2, add
    const left = try nodes.add(Node.litInt(loc, 1));
    const right = try nodes.add(Node.litInt(loc, 2));
    const add_node = try nodes.add(Node.binaryOp(loc, .add, left, right));

    var gen = CodeGen.init(allocator, &nodes, &constants, &scopes);
    defer gen.deinit();

    const result = try gen.generate(add_node);
    try std.testing.expect(result.code.len > 0);

    // Constant folding should eliminate the add opcode - 1+2 is folded to 3 at compile time
    // Should contain push_3 (the folded result), NOT add
    var found_add = false;
    var found_push_3 = false;
    for (result.code) |b| {
        if (b == @intFromEnum(Opcode.add)) {
            found_add = true;
        }
        if (b == @intFromEnum(Opcode.push_3)) {
            found_push_3 = true;
        }
    }
    // With constant folding, we should NOT have add opcode, but SHOULD have push_3
    try std.testing.expect(!found_add);
    try std.testing.expect(found_push_3);
}
