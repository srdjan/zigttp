//! Bytecode Code Generator
//!
//! Walks the IR and emits bytecode, handling closures and upvalues.

const std = @import("std");
const ir = @import("ir.zig");
const scope_mod = @import("scope.zig");

// Import real types from parent module for integration
const bytecode = @import("../bytecode.zig");
const value = @import("../value.zig");
const heap = @import("../heap.zig");
const string = @import("../string.zig");
const js_object = @import("../object.zig");

// Re-export types used by this module
const JSValue = value.JSValue;
const Opcode = bytecode.Opcode;
const FunctionBytecode = bytecode.FunctionBytecode;
const UpvalueInfo = bytecode.UpvalueInfo;

const Node = ir.Node;
const NodeTag = ir.NodeTag;
const NodeIndex = ir.NodeIndex;
const NodeList = ir.NodeList;
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

/// Code generator state
pub const CodeGen = struct {
    allocator: std.mem.Allocator,
    nodes: *const NodeList,
    ir_constants: *const ConstantPool,
    scopes: *ScopeAnalyzer,
    strings: ?*string.StringTable,

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

    pub fn init(
        allocator: std.mem.Allocator,
        nodes: *const NodeList,
        ir_constants: *const ConstantPool,
        scopes: *ScopeAnalyzer,
    ) CodeGen {
        return initWithStrings(allocator, nodes, ir_constants, scopes, null);
    }

    pub fn initWithStrings(
        allocator: std.mem.Allocator,
        nodes: *const NodeList,
        ir_constants: *const ConstantPool,
        scopes: *ScopeAnalyzer,
        strings_table: ?*string.StringTable,
    ) CodeGen {
        return .{
            .allocator = allocator,
            .nodes = nodes,
            .ir_constants = ir_constants,
            .scopes = scopes,
            .strings = strings_table,
            .code = std.ArrayList(u8).empty,
            .constants = std.ArrayList(JSValue).empty,
            .upvalue_info = std.ArrayList(UpvalueInfo).empty,
            .labels = std.ArrayList(Label).empty,
            .pending_jumps = std.ArrayList(PendingJump).empty,
            .loop_stack = std.ArrayList(LoopContext).empty,
            .current_scope = 0,
            .max_stack_depth = 0,
            .current_stack_depth = 0,
        };
    }

    pub fn deinit(self: *CodeGen) void {
        self.code.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        self.upvalue_info.deinit(self.allocator);
        self.labels.deinit(self.allocator);
        self.pending_jumps.deinit(self.allocator);
        self.loop_stack.deinit(self.allocator);
    }

    /// Generate bytecode for the entire program
    pub fn generate(self: *CodeGen, root: NodeIndex) !FunctionBytecode {
        try self.emitNode(root);
        try self.emit(.ret_undefined);

        try self.resolveJumps();

        return .{
            .header = .{},
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

        const node = self.nodes.get(func_node) orelse return error.InvalidNode;
        const func = node.data.function;

        // Emit function body
        try self.emitNode(func.body);

        // Ensure function returns
        try self.emit(.ret_undefined);

        try self.resolveJumps();

        // Get upvalue info from scope
        const scope = self.scopes.getScope(scope_id);
        var upvalue_info_list: std.ArrayList(UpvalueInfo) = .empty;
        defer upvalue_info_list.deinit(self.allocator);

        for (scope.upvalues.items) |uv| {
            try upvalue_info_list.append(self.allocator, .{
                .is_local = uv.is_direct,
                .index = uv.outer_slot,
            });
        }

        return .{
            .header = .{},
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
            .upvalue_info = upvalue_info_list.items,
            .code = self.code.items,
            .constants = self.constants.items,
            .source_map = null,
        };
    }

    // ============ Node Emission ============

    fn emitNode(self: *CodeGen, index: NodeIndex) anyerror!void {
        if (index == null_node) return;

        const node = self.nodes.get(index) orelse return;

        switch (node.tag) {
            // Literals
            .lit_int => try self.emitInteger(node.data.int_value),
            .lit_float => try self.emitFloat(node.data.float_idx),
            .lit_string => try self.emitString(node.data.string_idx),
            .lit_bool => try self.emitBool(node.data.bool_value),
            .lit_null => try self.emit(.push_null),
            .lit_undefined => try self.emit(.push_undefined),

            // Identifiers
            .identifier => try self.emitIdentifier(node.data.binding),
            .this_expr => try self.emit(.push_this),

            // Expressions
            .binary_op => try self.emitBinaryOp(node.data.binary),
            .unary_op => try self.emitUnaryOp(node.data.unary),
            .ternary => try self.emitTernary(node.data.ternary),
            .call, .optional_call => try self.emitCall(node.data.call),
            .new_expr => try self.emitNewExpr(node.data.call),
            .member_access, .optional_chain => try self.emitMemberAccess(node.data.member),
            .computed_access => try self.emitComputedAccess(node.data.member),
            .assignment => try self.emitAssignment(node.data.assignment),
            .array_literal => try self.emitArrayLiteral(node.data.array),
            .object_literal => try self.emitObjectLiteral(node.data.object),
            .function_expr, .arrow_function => try self.emitFunctionExpr(index, node.data.function),
            .template_literal => try self.emitTemplateLiteral(node.data.template),

            // Statements
            .expr_stmt => {
                if (node.data.opt_value) |expr| {
                    try self.emitNode(expr);
                    try self.emit(.drop);
                }
            },
            .var_decl, .function_decl => try self.emitVarDecl(node.data.var_decl),
            .if_stmt => try self.emitIfStmt(node.data.if_stmt),
            .while_stmt => try self.emitWhileLoop(node.data.loop),
            .do_while_stmt => try self.emitDoWhileLoop(node.data.loop),
            .for_stmt => try self.emitForLoop(node.data.loop),
            .for_of_stmt, .for_in_stmt => try self.emitForIterLoop(node.data.for_iter),
            .return_stmt => try self.emitReturn(node.data.opt_value),
            .break_stmt => try self.emitBreak(node.data.opt_label),
            .continue_stmt => try self.emitContinue(node.data.opt_label),
            .throw_stmt => try self.emitThrow(node.data.opt_value),
            .try_stmt => try self.emitTryStmt(node.data.try_stmt),
            .switch_stmt => try self.emitSwitch(node.data.switch_stmt),
            .block, .program => try self.emitBlock(node.data.block),
            .empty_stmt, .debugger_stmt => {},

            // JSX - emit as h() calls
            .jsx_element, .jsx_fragment => try self.emitJsxElement(node),

            // JSX text content - emit as string constant
            .jsx_text => {
                const str = self.ir_constants.getString(node.data.jsx_text) orelse "";
                const idx = try self.addStringConstant(str);
                try self.emitPushConst(idx);
                self.pushStack(1);
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
        const f = self.ir_constants.getFloat(float_idx) orelse 0.0;
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
        const str = self.ir_constants.getString(str_idx) orelse "";
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
                        try self.emitByte(binding.slot);
                    },
                }
            },
            .upvalue => {
                try self.emit(.get_upvalue);
                try self.emitByte(binding.slot);
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
                        try self.emitByte(binding.slot);
                    },
                }
            },
            .upvalue => {
                try self.emit(.put_upvalue);
                try self.emitByte(binding.slot);
            },
            .global => {
                try self.emit(.put_global);
                try self.emitU16(binding.slot);
            },
        }
        self.popStack(1);
    }

    // ============ Expression Emission ============

    fn emitBinaryOp(self: *CodeGen, binary: Node.BinaryExpr) !void {
        // Short-circuit operators need special handling
        switch (binary.op) {
            .and_op => return self.emitShortCircuitAnd(binary),
            .or_op => return self.emitShortCircuitOr(binary),
            .nullish => return self.emitNullishCoalescing(binary),
            else => {},
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
        switch (unary.op) {
            .pre_inc, .pre_dec, .post_inc, .post_dec => {
                // Handle increment/decrement
                return self.emitUpdateOp(unary);
            },
            else => {},
        }

        try self.emitNode(unary.operand);

        const opcode: Opcode = switch (unary.op) {
            .neg => .neg,
            .not => .not,
            .bit_not => .bit_not,
            .typeof_op => .typeof,
            else => .nop,
        };

        try self.emit(opcode);
    }

    fn emitUpdateOp(self: *CodeGen, unary: Node.UnaryExpr) !void {
        const operand_node = self.nodes.get(unary.operand) orelse return;

        if (operand_node.tag == .identifier) {
            const binding = operand_node.data.binding;

            // Get current value
            try self.emitIdentifier(binding);

            // For postfix, duplicate before modifying
            const is_postfix = unary.op == .post_inc or unary.op == .post_dec;
            if (is_postfix) {
                try self.emit(.dup);
                self.pushStack(1);
            }

            // Increment or decrement
            if (unary.op == .pre_inc or unary.op == .post_inc) {
                try self.emit(.inc);
            } else {
                try self.emit(.dec);
            }

            // Store back
            if (!is_postfix) {
                try self.emit(.dup);
                self.pushStack(1);
            }
            try self.emitSetBinding(binding);

            if (is_postfix) {
                try self.emit(.swap);
                try self.emit(.drop);
                self.popStack(1);
            }
        }
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
        const callee_node = self.nodes.get(call.callee) orelse {
            try self.emitNode(call.callee);
            try self.emitCallArgs(call);
            try self.emit(.call);
            try self.emitByte(call.args_count);
            self.popStack(call.args_count);
            return;
        };

        const is_method = callee_node.tag == .member_access or callee_node.tag == .optional_chain;

        if (is_method) {
            // Method call: obj.method(args)
            // Stack: [obj] -> [obj, obj] -> [obj, method] -> [obj, method, args...] -> [result]
            try self.emitNode(callee_node.data.member.object);
            try self.emit(.dup); // Keep object as 'this'
            self.pushStack(1);
            try self.emit(.get_field);
            try self.emitU16(callee_node.data.member.property);

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
            const arg_idx = self.nodes.getListIndex(call.args_start, i);
            try self.emitNode(arg_idx);
        }
    }

    fn emitNewExpr(self: *CodeGen, call: Node.CallExpr) !void {
        try self.emitNode(call.callee);

        var i: u8 = 0;
        while (i < call.args_count) : (i += 1) {
            const arg_idx = self.nodes.getListIndex(call.args_start, i);
            try self.emitNode(arg_idx);
        }

        try self.emit(.call_constructor);
        try self.emitByte(call.args_count);

        self.popStack(call.args_count);
    }

    fn emitMemberAccess(self: *CodeGen, member: Node.MemberExpr) !void {
        try self.emitNode(member.object);
        try self.emit(.get_field);
        try self.emitU16(member.property);
    }

    fn emitComputedAccess(self: *CodeGen, member: Node.MemberExpr) !void {
        try self.emitNode(member.object);
        try self.emitNode(member.computed);
        try self.emit(.get_elem);
        self.popStack(1);
    }

    fn emitAssignment(self: *CodeGen, assign: Node.AssignExpr) !void {
        const target_node = self.nodes.get(assign.target) orelse return;

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
        switch (target_node.tag) {
            .identifier => {
                try self.emit(.dup);
                self.pushStack(1);
                try self.emitSetBinding(target_node.data.binding);
            },
            .member_access => {
                // Stack: value
                // Need: object, value
                try self.emitNode(target_node.data.member.object);
                try self.emit(.swap);
                try self.emit(.put_field_keep);
                try self.emitU16(target_node.data.member.property);
                self.popStack(1);
            },
            .computed_access => {
                try self.emitNode(target_node.data.member.object);
                try self.emitNode(target_node.data.member.computed);
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
            const elem_idx = self.nodes.getListIndex(array.elements_start, i);

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

    fn emitObjectLiteral(self: *CodeGen, object: Node.ObjectExpr) !void {
        try self.emit(.new_object);
        self.pushStack(1);

        var i: u16 = 0;
        while (i < object.properties_count) : (i += 1) {
            const prop_idx = self.nodes.getListIndex(object.properties_start, i);
            const prop_node = self.nodes.get(prop_idx) orelse continue;

            if (prop_node.tag == .object_property) {
                const prop = prop_node.data.property;

                try self.emit(.dup); // Duplicate object reference
                self.pushStack(1);

                // Get property key
                const key_node = self.nodes.get(prop.key) orelse continue;
                if (key_node.tag == .lit_string) {
                    // Key is a string constant, use put_field
                    try self.emitNode(prop.value);
                    try self.emit(.put_field);
                    try self.emitU16(key_node.data.string_idx);
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

        // Create FunctionBytecode on heap
        const func_bc = try self.allocator.create(FunctionBytecode);
        const code_copy = try self.allocator.dupe(u8, self.code.items);
        const consts_copy = try self.allocator.dupe(JSValue, self.constants.items);
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

        // Add function bytecode to parent constants and emit opcode
        const func_idx = try self.addConstant(JSValue.fromPtr(func_bc));
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
            const part_idx = self.nodes.getListIndex(template.parts_start, i);
            const part = self.nodes.get(part_idx) orelse continue;

            if (part.tag == .template_part_string) {
                try self.emitString(part.data.string_idx);
            } else if (part.tag == .template_part_expr) {
                if (part.data.opt_value) |expr| {
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
        const pattern_node = self.nodes.get(pattern) orelse return;

        switch (pattern_node.tag) {
            .object_pattern => try self.emitObjectPattern(pattern_node.data.array),
            .array_pattern => try self.emitArrayPattern(pattern_node.data.array),
            else => {},
        }
    }

    fn emitObjectPattern(self: *CodeGen, array_data: Node.ArrayExpr) anyerror!void {
        // For each property in the pattern, extract from the source object
        var i: u16 = 0;
        while (i < array_data.elements_count) : (i += 1) {
            const elem_idx = self.nodes.getListIndex(array_data.elements_start, i);
            const elem_node = self.nodes.get(elem_idx) orelse continue;
            if (elem_node.tag != .pattern_element) continue;

            const elem = elem_node.data.pattern_elem;

            switch (elem.kind) {
                .simple => {
                    // Stack: source_obj
                    try self.emit(.dup); // Duplicate source for next property
                    self.pushStack(1);

                    // Get the property value using the atom
                    try self.emit(.get_field);
                    try self.emitU16(elem.key_atom);

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
                    try self.emit(.get_field);
                    try self.emitU16(elem.key_atom);

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
            const elem_idx = self.nodes.getListIndex(array_data.elements_start, i);
            const elem_node = self.nodes.get(elem_idx) orelse {
                index += 1; // Skip hole
                continue;
            };
            if (elem_node.tag != .pattern_element) {
                index += 1;
                continue;
            }

            const elem = elem_node.data.pattern_elem;

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

    fn emitWhileLoop(self: *CodeGen, loop: Node.LoopStmt) !void {
        const loop_start = try self.createLabel();
        const loop_end = try self.createLabel();

        try self.loop_stack.append(self.allocator, .{
            .break_label = loop_end,
            .continue_label = loop_start,
        });

        try self.placeLabel(loop_start);
        try self.emitNode(loop.condition);
        try self.emitJump(.if_false, loop_end);
        self.popStack(1);

        try self.emitNode(loop.body);
        try self.emitJump(.goto, loop_start);

        try self.placeLabel(loop_end);

        _ = self.loop_stack.pop();
    }

    fn emitDoWhileLoop(self: *CodeGen, loop: Node.LoopStmt) !void {
        const loop_start = try self.createLabel();
        const loop_end = try self.createLabel();
        const continue_label = try self.createLabel();

        try self.loop_stack.append(self.allocator, .{
            .break_label = loop_end,
            .continue_label = continue_label,
        });

        try self.placeLabel(loop_start);
        try self.emitNode(loop.body);

        try self.placeLabel(continue_label);
        try self.emitNode(loop.condition);
        try self.emitJump(.if_true, loop_start);
        self.popStack(1);

        try self.placeLabel(loop_end);

        _ = self.loop_stack.pop();
    }

    fn emitForLoop(self: *CodeGen, loop: Node.LoopStmt) !void {
        // Init
        if (loop.init != null_node) {
            try self.emitNode(loop.init);
            if (self.nodes.get(loop.init)) |init_node| {
                if (init_node.tag != .var_decl) {
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
        // Simplified for-of/for-in loop
        // Real implementation would use iterators
        const loop_start = try self.createLabel();
        const loop_end = try self.createLabel();

        try self.loop_stack.append(self.allocator, .{
            .break_label = loop_end,
            .continue_label = loop_start,
        });

        // Get iterator from iterable
        try self.emitNode(for_iter.iterable);
        // Would need iterator protocol here

        try self.placeLabel(loop_start);
        // Check iterator.next().done
        // Bind value to loop variable
        // Execute body

        try self.emitNode(for_iter.body);
        try self.emitJump(.goto, loop_start);

        try self.placeLabel(loop_end);
        try self.emit(.drop); // Drop iterator
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

    fn emitBreak(self: *CodeGen, label_opt: ?u16) !void {
        _ = label_opt;
        if (self.loop_stack.items.len > 0) {
            const ctx = self.loop_stack.items[self.loop_stack.items.len - 1];
            try self.emitJump(.goto, ctx.break_label);
        }
    }

    fn emitContinue(self: *CodeGen, label_opt: ?u16) !void {
        _ = label_opt;
        if (self.loop_stack.items.len > 0) {
            const ctx = self.loop_stack.items[self.loop_stack.items.len - 1];
            try self.emitJump(.goto, ctx.continue_label);
        }
    }

    fn emitThrow(self: *CodeGen, value_opt: ?NodeIndex) !void {
        if (value_opt) |val| {
            try self.emitNode(val);
        } else {
            try self.emit(.push_undefined);
            self.pushStack(1);
        }
        try self.emit(.@"throw");
        self.popStack(1);
    }

    fn emitTryStmt(self: *CodeGen, try_stmt: Node.TryStmt) !void {
        const catch_label = try self.createLabel();
        const finally_label = try self.createLabel();
        const end_label = try self.createLabel();

        // Push exception handler
        try self.emit(.push_catch);
        try self.emitI16Placeholder(catch_label);

        // Try block
        try self.emitNode(try_stmt.try_block);

        // Normal exit from try
        try self.emit(.pop_catch);
        if (try_stmt.finally_block != null_node) {
            try self.emitJump(.goto, finally_label);
        } else {
            try self.emitJump(.goto, end_label);
        }

        // Catch block
        try self.placeLabel(catch_label);
        if (try_stmt.catch_block != null_node) {
            if (try_stmt.catch_binding.slot != 255) {
                try self.emit(.get_exception);
                self.pushStack(1);
                try self.emitSetBinding(try_stmt.catch_binding);
            }
            try self.emitNode(try_stmt.catch_block);
        }

        // Finally block
        if (try_stmt.finally_block != null_node) {
            try self.placeLabel(finally_label);
            try self.emitNode(try_stmt.finally_block);
        }

        try self.placeLabel(end_label);
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
            const case_idx = self.nodes.getListIndex(switch_stmt.cases_start, i);
            const label = try self.createLabel();
            try case_labels.append(self.allocator, label);

            const case_node = self.nodes.get(case_idx) orelse continue;
            if (case_node.data.case_clause.test_expr == null_node) {
                default_label = label;
            }
        }

        // Emit comparisons
        i = 0;
        while (i < switch_stmt.cases_count) : (i += 1) {
            const case_idx = self.nodes.getListIndex(switch_stmt.cases_start, i);
            const case_node = self.nodes.get(case_idx) orelse continue;

            if (case_node.data.case_clause.test_expr != null_node) {
                try self.emit(.dup);
                self.pushStack(1);
                try self.emitNode(case_node.data.case_clause.test_expr);
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
            const case_idx = self.nodes.getListIndex(switch_stmt.cases_start, i);
            try self.placeLabel(case_labels.items[i]);

            const case_node = self.nodes.get(case_idx) orelse continue;

            var j: u16 = 0;
            while (j < case_node.data.case_clause.body_count) : (j += 1) {
                const body_idx = self.nodes.getListIndex(case_node.data.case_clause.body_start, j);
                try self.emitNode(body_idx);
            }
        }

        try self.placeLabel(end_label);
    }

    fn emitBlock(self: *CodeGen, block: Node.BlockData) !void {
        var i: u16 = 0;
        while (i < block.stmts_count) : (i += 1) {
            const stmt_idx = self.nodes.getListIndex(block.stmts_start, i);
            try self.emitNode(stmt_idx);
        }
    }

    fn emitJsxElement(self: *CodeGen, node: *const Node) !void {
        // Emit JSX as h(tag, props, ...children) call
        const elem = node.data.jsx_element;

        // Push h function reference (predefined atom)
        try self.emit(.get_global);
        try self.emitU16(@intFromEnum(js_object.Atom.h));
        self.pushStack(1);

        // Push tag name
        if (node.tag == .jsx_fragment) {
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
            const tag_idx = try self.addStringConstant(self.ir_constants.getString(elem.tag_atom) orelse "div");
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
                const prop_idx = self.nodes.getListIndex(elem.props_start, i);
                const prop_node = self.nodes.get(prop_idx) orelse continue;
                if (prop_node.tag == .jsx_attribute) {
                    try self.emit(.dup);
                    self.pushStack(1);
                    if (prop_node.data.jsx_attr.value != null_node) {
                        try self.emitNode(prop_node.data.jsx_attr.value);
                    } else {
                        try self.emit(.push_true);
                        self.pushStack(1);
                    }
                    try self.emit(.put_field);
                    try self.emitU16(prop_node.data.jsx_attr.name_atom);
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
                const child_idx = self.nodes.getListIndex(elem.children_start, i);
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

            self.code.items[jump.instruction_offset] = @truncate(unsigned >> 8);
            self.code.items[jump.instruction_offset + 1] = @truncate(unsigned);
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

    // Create 1 + 2
    const left = try nodes.add(Node.litInt(loc, 1));
    const right = try nodes.add(Node.litInt(loc, 2));
    const add_node = try nodes.add(Node.binaryOp(loc, .add, left, right));

    var gen = CodeGen.init(allocator, &nodes, &constants, &scopes);
    defer gen.deinit();

    const result = try gen.generate(add_node);
    try std.testing.expect(result.code.len > 0);

    // Should contain push_1, push_2, add
    var found_add = false;
    for (result.code) |b| {
        if (b == @intFromEnum(Opcode.add)) {
            found_add = true;
            break;
        }
    }
    try std.testing.expect(found_add);
}
