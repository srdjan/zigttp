//! Bytecode Code Generator
//!
//! Walks the IR and emits bytecode, handling closures and upvalues.

const std = @import("std");
const ir = @import("ir.zig");
const scope_mod = @import("scope.zig");

// When used as part of the full module, these will be imported from parent
// For standalone testing, we define stubs

const bytecode = struct {
    // JSValue stub - must be inside bytecode struct to avoid ambiguity
    pub const JSValue = struct {
        data: u64,

        pub fn initInt(val: i32) JSValue {
            return .{ .data = @bitCast(@as(i64, val)) };
        }

        pub fn initFloat(val: f64) JSValue {
            return .{ .data = @bitCast(val) };
        }

        pub fn initUndefined() JSValue {
            return .{ .data = 0 };
        }
    };
    pub const Opcode = enum(u8) {
        nop = 0x00,
        push_const = 0x01,
        push_0 = 0x02,
        push_1 = 0x03,
        push_2 = 0x04,
        push_3 = 0x05,
        push_i8 = 0x06,
        push_i16 = 0x07,
        push_null = 0x08,
        push_undefined = 0x09,
        push_true = 0x0A,
        push_false = 0x0B,
        dup = 0x0C,
        drop = 0x0D,
        swap = 0x0E,
        rot3 = 0x0F,
        get_loc = 0x10,
        put_loc = 0x11,
        get_loc_0 = 0x12,
        get_loc_1 = 0x13,
        get_loc_2 = 0x14,
        get_loc_3 = 0x15,
        put_loc_0 = 0x16,
        put_loc_1 = 0x17,
        put_loc_2 = 0x18,
        put_loc_3 = 0x19,
        halt = 0x1A,
        loop = 0x1B,
        add = 0x20,
        sub = 0x21,
        mul = 0x22,
        div = 0x23,
        mod = 0x24,
        pow = 0x25,
        neg = 0x26,
        inc = 0x27,
        dec = 0x28,
        bit_and = 0x30,
        bit_or = 0x31,
        bit_xor = 0x32,
        bit_not = 0x33,
        shl = 0x34,
        shr = 0x35,
        ushr = 0x36,
        lt = 0x40,
        lte = 0x41,
        gt = 0x42,
        gte = 0x43,
        eq = 0x44,
        neq = 0x45,
        strict_eq = 0x46,
        strict_neq = 0x47,
        not = 0x48,
        goto = 0x50,
        if_true = 0x51,
        if_false = 0x52,
        ret = 0x53,
        ret_undefined = 0x54,
        @"throw" = 0x55,
        call = 0x60,
        call_method = 0x61,
        call_constructor = 0x62,
        get_field = 0x70,
        put_field = 0x71,
        get_elem = 0x72,
        put_elem = 0x73,
        put_field_keep = 0x76,
        new_object = 0x80,
        new_array = 0x81,
        push_this = 0x82,
        get_global = 0x83,
        put_global = 0x84,
        make_function = 0x86,
        typeof = 0x90,
        instanceof = 0x91,
        push_catch = 0x92,
        pop_catch = 0x93,
        get_exception = 0x94,
        get_upvalue = 0xC0,
        put_upvalue = 0xC1,
        close_upvalue = 0xC2,
        make_closure = 0xC3,
        _,
    };

    pub const UpvalueInfo = struct {
        is_local: bool,
        index: u8,
    };

    pub const BytecodeHeader = struct {};

    pub const FunctionFlags = packed struct {
        is_strict: bool = true,
        is_generator: bool = false,
        is_async: bool = false,
        has_arguments: bool = false,
        has_rest: bool = false,
        _reserved: u3 = 0,
    };

    pub const FunctionBytecode = struct {
        header: BytecodeHeader,
        name_atom: u32,
        arg_count: u16,
        local_count: u8,
        stack_size: u16,
        flags: FunctionFlags,
        upvalue_count: u8,
        upvalue_info: []const UpvalueInfo,
        code: []const u8,
        constants: []const JSValue,
        source_map: ?[]const u8,
    };
};

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

const Opcode = bytecode.Opcode;
const FunctionBytecode = bytecode.FunctionBytecode;

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

    // Output
    code: std.ArrayList(u8),
    constants: std.ArrayList(bytecode.JSValue),
    upvalue_info: std.ArrayList(bytecode.UpvalueInfo),

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
        return .{
            .allocator = allocator,
            .nodes = nodes,
            .ir_constants = ir_constants,
            .scopes = scopes,
            .code = std.ArrayList(u8).empty,
            .constants = std.ArrayList(bytecode.JSValue).empty,
            .upvalue_info = std.ArrayList(bytecode.UpvalueInfo).empty,
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
        var upvalue_info_list: std.ArrayList(bytecode.UpvalueInfo) = .empty;
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
                const idx = try self.addConstant(bytecode.JSValue.initInt(val));
                try self.emitPushConst(idx);
            },
        }
        self.pushStack(1);
    }

    fn emitFloat(self: *CodeGen, float_idx: u16) !void {
        const f = self.ir_constants.getFloat(float_idx) orelse 0.0;
        const idx = try self.addConstant(bytecode.JSValue.initFloat(f));
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
        try self.emitNode(call.callee);

        // Emit arguments
        var i: u8 = 0;
        var arg_idx = call.args_start;
        while (i < call.args_count) : (i += 1) {
            try self.emitNode(arg_idx);
            arg_idx += 1;
        }

        try self.emit(.call);
        try self.emitByte(call.args_count);

        // Pop callee + args, push result
        self.popStack(call.args_count);
    }

    fn emitNewExpr(self: *CodeGen, call: Node.CallExpr) !void {
        try self.emitNode(call.callee);

        var i: u8 = 0;
        var arg_idx = call.args_start;
        while (i < call.args_count) : (i += 1) {
            try self.emitNode(arg_idx);
            arg_idx += 1;
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
        var elem_idx = array.elements_start;
        while (i < array.elements_count) : (i += 1) {
            if (elem_idx != null_node) {
                try self.emitNode(elem_idx);
            } else {
                try self.emit(.push_undefined);
                self.pushStack(1);
            }
            elem_idx += 1;
        }

        self.popStack(array.elements_count);
    }

    fn emitObjectLiteral(self: *CodeGen, object: Node.ObjectExpr) !void {
        try self.emit(.new_object);
        self.pushStack(1);

        var i: u16 = 0;
        var prop_idx = object.properties_start;
        while (i < object.properties_count) : (i += 1) {
            const prop_node = self.nodes.get(prop_idx) orelse {
                prop_idx += 1;
                continue;
            };

            if (prop_node.tag == .object_property) {
                const prop = prop_node.data.property;

                try self.emit(.dup); // Duplicate object reference
                self.pushStack(1);

                // Get property key
                const key_node = self.nodes.get(prop.key) orelse {
                    prop_idx += 1;
                    continue;
                };
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

            prop_idx += 1;
        }
    }

    fn emitFunctionExpr(self: *CodeGen, node_idx: NodeIndex, func: Node.FunctionExpr) !void {
        // Add function bytecode to constants and emit make_closure
        const func_idx = try self.addFunctionConstant(node_idx);

        const scope = self.scopes.getScope(func.scope_id);
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
        var part_idx = template.parts_start;

        while (i < template.parts_count) : (i += 1) {
            const part = self.nodes.get(part_idx) orelse {
                part_idx += 1;
                continue;
            };

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

            part_idx += 1;
        }

        if (template.parts_count == 0) {
            try self.emitString(0); // Empty string
        }
    }

    // ============ Statement Emission ============

    fn emitVarDecl(self: *CodeGen, decl: Node.VarDecl) !void {
        if (decl.init != null_node) {
            try self.emitNode(decl.init);
            try self.emitSetBinding(decl.binding);
        }
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
        var case_idx = switch_stmt.cases_start;
        while (i < switch_stmt.cases_count) : (i += 1) {
            const label = try self.createLabel();
            try case_labels.append(self.allocator, label);

            const case_node = self.nodes.get(case_idx) orelse {
                case_idx += 1;
                continue;
            };
            if (case_node.data.case_clause.test_expr == null_node) {
                default_label = label;
            }
            case_idx += 1;
        }

        // Emit comparisons
        i = 0;
        case_idx = switch_stmt.cases_start;
        while (i < switch_stmt.cases_count) : (i += 1) {
            const case_node = self.nodes.get(case_idx) orelse {
                case_idx += 1;
                continue;
            };

            if (case_node.data.case_clause.test_expr != null_node) {
                try self.emit(.dup);
                self.pushStack(1);
                try self.emitNode(case_node.data.case_clause.test_expr);
                try self.emit(.strict_eq);
                self.popStack(1);
                try self.emitJump(.if_true, case_labels.items[i]);
                self.popStack(1);
            }

            case_idx += 1;
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
        case_idx = switch_stmt.cases_start;
        while (i < switch_stmt.cases_count) : (i += 1) {
            try self.placeLabel(case_labels.items[i]);

            const case_node = self.nodes.get(case_idx) orelse {
                case_idx += 1;
                continue;
            };

            var body_idx = case_node.data.case_clause.body_start;
            var j: u16 = 0;
            while (j < case_node.data.case_clause.body_count) : (j += 1) {
                try self.emitNode(body_idx);
                body_idx += 1;
            }

            case_idx += 1;
        }

        try self.placeLabel(end_label);
    }

    fn emitBlock(self: *CodeGen, block: Node.BlockData) !void {
        var i: u16 = 0;
        var stmt_idx = block.stmts_start;
        while (i < block.stmts_count) : (i += 1) {
            try self.emitNode(stmt_idx);
            stmt_idx += 1;
        }
    }

    fn emitJsxElement(self: *CodeGen, node: *const Node) !void {
        // Emit JSX as h(tag, props, ...children) call
        const elem = node.data.jsx_element;

        // Push h function reference
        const h_atom = try self.addStringConstant("h");
        try self.emit(.get_global);
        try self.emitU16(h_atom);
        self.pushStack(1);

        // Push tag name
        if (elem.tag_atom == 0) {
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
            var prop_idx = elem.props_start;
            while (i < elem.props_count) : (i += 1) {
                const prop_node = self.nodes.get(prop_idx) orelse {
                    prop_idx += 1;
                    continue;
                };
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
                prop_idx += 1;
            }
        } else {
            try self.emit(.push_null);
            self.pushStack(1);
        }

        // Push children
        var child_count: u8 = 0;
        if (elem.children_count > 0) {
            var i: u8 = 0;
            var child_idx = elem.children_start;
            while (i < elem.children_count) : (i += 1) {
                try self.emitNode(child_idx);
                child_count += 1;
                child_idx += 1;
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
        try self.code.append(self.allocator, @truncate(val >> 8));
        try self.code.append(self.allocator, @truncate(val));
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

    fn addConstant(self: *CodeGen, val: bytecode.JSValue) !u16 {
        const idx = @as(u16, @intCast(self.constants.items.len));
        try self.constants.append(self.allocator, val);
        return idx;
    }

    fn addStringConstant(self: *CodeGen, str: []const u8) !u16 {
        // Simplified - would need proper string table integration
        _ = str;
        return try self.addConstant(bytecode.JSValue.initUndefined());
    }

    fn addFunctionConstant(self: *CodeGen, node_idx: NodeIndex) !u16 {
        // Would compile function and add bytecode to constants
        _ = node_idx;
        return try self.addConstant(bytecode.JSValue.initUndefined());
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
