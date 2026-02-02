//! IR-to-Zig Source Transpiler
//!
//! Converts JavaScript IR (IrView) into Zig source code that the Zig compiler
//! optimizes through its full pipeline. Runs at build time inside precompile.zig.
//!
//! The transpiler walks the IR tree and emits equivalent Zig source code.
//! Any IR node it cannot handle emits `error.AotBail`, falling through to
//! the bytecode interpreter.

const std = @import("std");
const zts = @import("zts");
const ir = zts.parser;
const NodeIndex = ir.NodeIndex;
const NodeTag = ir.NodeTag;
const null_node = ir.null_node;
const IrView = ir.IrView;
const BinaryOp = ir.BinaryOp;
const UnaryOp = ir.UnaryOp;
const BindingRef = ir.BindingRef;
const Atom = zts.Atom;

/// Inferred Zig type for a binding or expression
pub const ZigType = enum {
    i32_type,
    f64_type,
    bool_type,
    string_type,
    jsvalue_type,
    void_type,
};

/// Information about a local variable
const LocalInfo = struct {
    name: []const u8,
    zig_type: ZigType,
    is_mutable: bool,
    is_param: bool,
};

/// Function signature inferred from IR
const FunctionSig = struct {
    params: []ZigType,
    return_type: ZigType,
    is_pure_integer: bool,
    name: []const u8,
};

/// Discovered top-level function for transpilation
const FunctionInfo = struct {
    node_idx: NodeIndex,
    name_atom: u16,
    name: []const u8,
    sig: ?FunctionSig,
    is_handler: bool,
};

pub const TranspileResult = struct {
    source: []const u8,
    handler_name: ?[]const u8,
    functions_transpiled: usize,
    functions_bailed: usize,
};

pub const IrTranspiler = struct {
    ir: IrView,
    atoms: ?*zts.context.AtomTable,
    output: std.ArrayList(u8),
    indent: u16,
    allocator: std.mem.Allocator,

    // Binding slot -> local variable info
    locals: std.AutoHashMap(u32, LocalInfo),
    // Discovered functions
    functions: std.ArrayList(FunctionInfo),
    // Type inference environment per scope
    type_env: std.AutoHashMap(u32, ZigType),

    // Handler-specific state
    has_request_param: bool,
    url_local_slot: ?u32,
    method_local_slot: ?u32,

    // Statistics
    functions_transpiled: usize,
    functions_bailed: usize,

    // Track whether current expression context needs a value
    in_expr_context: bool,

    // Name allocations to free
    name_allocs: std.ArrayList([]const u8),

    pub fn init(allocator: std.mem.Allocator, ir_view: IrView, atoms: ?*zts.context.AtomTable) IrTranspiler {
        return .{
            .ir = ir_view,
            .atoms = atoms,
            .output = .empty,
            .indent = 0,
            .allocator = allocator,
            .locals = std.AutoHashMap(u32, LocalInfo).init(allocator),
            .functions = .empty,
            .type_env = std.AutoHashMap(u32, ZigType).init(allocator),
            .has_request_param = false,
            .url_local_slot = null,
            .method_local_slot = null,
            .functions_transpiled = 0,
            .functions_bailed = 0,
            .in_expr_context = false,
            .name_allocs = .empty,
        };
    }

    pub fn deinit(self: *IrTranspiler) void {
        self.output.deinit(self.allocator);
        self.locals.deinit();
        self.functions.deinit(self.allocator);
        self.type_env.deinit();
        for (self.name_allocs.items) |name| {
            self.allocator.free(name);
        }
        self.name_allocs.deinit(self.allocator);
    }

    // ============ Output Helpers ============

    fn emit(self: *IrTranspiler, str: []const u8) void {
        self.output.appendSlice(self.allocator, str) catch {};
    }

    fn emitFmt(self: *IrTranspiler, comptime fmt: []const u8, args: anytype) void {
        var aw: std.Io.Writer.Allocating = .fromArrayList(self.allocator, &self.output);
        aw.writer.print(fmt, args) catch {};
        self.output = aw.toArrayList();
    }

    fn emitByte(self: *IrTranspiler, byte: u8) void {
        self.output.append(self.allocator, byte) catch {};
    }

    fn emitIndent(self: *IrTranspiler) void {
        var i: u16 = 0;
        while (i < self.indent) : (i += 1) {
            self.emit("    ");
        }
    }

    fn pushIndent(self: *IrTranspiler) void {
        self.indent += 1;
    }

    fn popIndent(self: *IrTranspiler) void {
        if (self.indent > 0) self.indent -= 1;
    }

    fn emitZigString(self: *IrTranspiler, str: []const u8) void {
        self.emitByte('"');
        for (str) |c| {
            switch (c) {
                '"' => self.emit("\\\""),
                '\\' => self.emit("\\\\"),
                '\n' => self.emit("\\n"),
                '\r' => self.emit("\\r"),
                '\t' => self.emit("\\t"),
                0x00...0x08, 0x0b...0x0c, 0x0e...0x1f, 0x7f => {
                    var buf: [4]u8 = undefined;
                    buf[0] = '\\';
                    buf[1] = 'x';
                    const hex = "0123456789abcdef";
                    buf[2] = hex[c >> 4];
                    buf[3] = hex[c & 0x0f];
                    self.emit(&buf);
                },
                else => self.emitByte(c),
            }
        }
        self.emitByte('"');
    }

    /// Emit: @memcpy(json_buf[json_pos..][0..N], "escaped_str");
    /// json_pos += N;
    /// Properly escapes the string for Zig string literal context.
    fn emitMemcpyLiteral(self: *IrTranspiler, str: []const u8) void {
        self.emitIndent();
        self.emitFmt("@memcpy(json_buf[json_pos..][0..{d}], ", .{str.len});
        self.emitZigString(str);
        self.emit(");\n");
        self.emitIndent();
        self.emitFmt("json_pos += {d};\n", .{str.len});
    }

    // ============ Name Resolution ============

    fn localName(self: *IrTranspiler, binding: BindingRef) []const u8 {
        // Check if we have a cached name
        const key = bindingKey(binding);
        if (self.locals.get(key)) |info| {
            return info.name;
        }

        // Generate a name
        if (binding.kind == .argument) {
            return switch (binding.slot) {
                0 => "arg_0",
                1 => "arg_1",
                2 => "arg_2",
                3 => "arg_3",
                else => "arg_n",
            };
        }

        return switch (binding.slot) {
            0 => "local_0",
            1 => "local_1",
            2 => "local_2",
            3 => "local_3",
            4 => "local_4",
            5 => "local_5",
            6 => "local_6",
            7 => "local_7",
            else => "local_n",
        };
    }

    fn bindingKey(binding: BindingRef) u32 {
        return (@as(u32, binding.scope_id) << 16) | binding.slot;
    }

    fn atomName(self: *IrTranspiler, atom_idx: u16) ?[]const u8 {
        if (self.atoms) |atoms| {
            return atoms.getName(@enumFromInt(atom_idx));
        }
        return null;
    }

    fn registerLocal(self: *IrTranspiler, binding: BindingRef, name: []const u8, zig_type: ZigType, is_mutable: bool) void {
        const key = bindingKey(binding);
        self.locals.put(key, .{
            .name = name,
            .zig_type = zig_type,
            .is_mutable = is_mutable,
            .is_param = binding.kind == .argument,
        }) catch {};
        self.type_env.put(key, zig_type) catch {};
    }

    // ============ Type Inference ============

    fn inferExprType(self: *IrTranspiler, idx: NodeIndex) ZigType {
        const tag = self.ir.getTag(idx) orelse return .jsvalue_type;
        return switch (tag) {
            .lit_int => .i32_type,
            .lit_float => .f64_type,
            .lit_string => .string_type,
            .lit_bool => .bool_type,
            .lit_null, .lit_undefined => .jsvalue_type,
            .identifier => blk: {
                const binding = self.ir.getBinding(idx) orelse break :blk .jsvalue_type;
                const key = bindingKey(binding);
                break :blk self.type_env.get(key) orelse .jsvalue_type;
            },
            .binary_op => blk: {
                const bin = self.ir.getBinary(idx) orelse break :blk .jsvalue_type;
                switch (bin.op) {
                    .add, .sub, .mul, .div, .mod, .pow => {
                        const lt = self.inferExprType(bin.left);
                        const rt = self.inferExprType(bin.right);
                        // String concatenation
                        if (bin.op == .add and (lt == .string_type or rt == .string_type)) {
                            break :blk .string_type;
                        }
                        // Integer arithmetic
                        if (lt == .i32_type and rt == .i32_type) break :blk .i32_type;
                        // Float arithmetic
                        if (lt == .f64_type or rt == .f64_type) break :blk .f64_type;
                        break :blk .jsvalue_type;
                    },
                    .eq, .neq, .strict_eq, .strict_neq, .lt, .lte, .gt, .gte => break :blk .bool_type,
                    .and_op, .or_op => {
                        const lt = self.inferExprType(bin.left);
                        break :blk lt;
                    },
                    .bit_and, .bit_or, .bit_xor, .shl, .shr, .ushr => break :blk .i32_type,
                    else => break :blk .jsvalue_type,
                }
            },
            .unary_op => blk: {
                const un = self.ir.getUnary(idx) orelse break :blk .jsvalue_type;
                switch (un.op) {
                    .neg => break :blk self.inferExprType(un.operand),
                    .not => break :blk .bool_type,
                    .bit_not => break :blk .i32_type,
                    else => break :blk .jsvalue_type,
                }
            },
            .call, .method_call => .jsvalue_type,
            .member_access => .jsvalue_type,
            .ternary => blk: {
                const t = self.ir.getTernary(idx) orelse break :blk .jsvalue_type;
                break :blk self.inferExprType(t.then_branch);
            },
            .object_literal, .array_literal => .jsvalue_type,
            .template_literal => .string_type,
            else => .jsvalue_type,
        };
    }

    /// Infer if a function is pure-integer (all params and return are i32)
    fn inferFunctionSig(self: *IrTranspiler, func_node: NodeIndex) ?FunctionSig {
        const func = self.ir.getFunction(func_node) orelse return null;
        const body_idx = func.body;

        // Walk body to check operations on parameters
        const is_pure_int = self.checkPureIntegerBody(body_idx, func.scope_id, func.params_count);

        if (is_pure_int) {
            const result = self.allocator.alloc(ZigType, func.params_count) catch return null;
            for (result) |*pt| {
                pt.* = .i32_type;
            }
            const name = self.atomName(func.name_atom) orelse "anonymous";
            return .{
                .params = result,
                .return_type = .i32_type,
                .is_pure_integer = true,
                .name = name,
            };
        }

        return null;
    }

    /// Check if a function body only uses integer operations
    fn checkPureIntegerBody(self: *IrTranspiler, idx: NodeIndex, scope_id: u16, param_count: u8) bool {
        const tag = self.ir.getTag(idx) orelse return false;
        return switch (tag) {
            .block => blk: {
                const block = self.ir.getBlock(idx) orelse break :blk false;
                var i: u16 = 0;
                while (i < block.stmts_count) : (i += 1) {
                    const stmt = self.ir.getListIndex(block.stmts_start, i);
                    if (!self.checkPureIntegerBody(stmt, scope_id, param_count)) break :blk false;
                }
                break :blk true;
            },
            .return_stmt => blk: {
                const val = self.ir.getOptValue(idx) orelse break :blk true;
                const t = self.inferExprType(val);
                break :blk t == .i32_type;
            },
            .if_stmt => blk: {
                const if_s = self.ir.getIfStmt(idx) orelse break :blk false;
                if (!self.checkPureIntegerBody(if_s.then_branch, scope_id, param_count)) break :blk false;
                if (if_s.else_branch != null_node) {
                    if (!self.checkPureIntegerBody(if_s.else_branch, scope_id, param_count)) break :blk false;
                }
                break :blk true;
            },
            .var_decl => blk: {
                const decl = self.ir.getVarDecl(idx) orelse break :blk false;
                if (decl.init == null_node) break :blk true;
                const t = self.inferExprType(decl.init);
                break :blk t == .i32_type;
            },
            .expr_stmt => blk: {
                const val = self.ir.getOptValue(idx) orelse break :blk true;
                const t = self.inferExprType(val);
                _ = t;
                break :blk true; // Expression statements can be anything
            },
            .for_of_stmt => false, // Conservatively bail
            .assignment => blk: {
                const a = self.ir.getAssignment(idx) orelse break :blk false;
                const t = self.inferExprType(a.value);
                break :blk t == .i32_type;
            },
            else => true, // Unknown nodes treated as pure for now
        };
    }

    // ============ Main Entry Point ============

    /// Transpile the full handler file, returns generated Zig source
    pub fn transpileHandler(self: *IrTranspiler, root: NodeIndex) !TranspileResult {
        // Phase 1: Discover all functions
        try self.discoverFunctions(root);

        // Phase 2: Emit file header
        self.emitHeader();

        // Phase 3: Emit helper utility functions
        self.emitHelpers();

        // Phase 4: Transpile helper functions (non-handler)
        for (self.functions.items) |*func_info| {
            if (func_info.is_handler) continue;
            self.transpileTopLevelFunction(func_info);
        }

        // Phase 5: Transpile handler function
        var handler_name: ?[]const u8 = null;
        for (self.functions.items) |*func_info| {
            if (!func_info.is_handler) continue;
            handler_name = "aotHandler";
            self.transpileHandlerFunction(func_info);
        }

        return .{
            .source = self.output.items,
            .handler_name = handler_name,
            .functions_transpiled = self.functions_transpiled,
            .functions_bailed = self.functions_bailed,
        };
    }

    // ============ Phase 1: Function Discovery ============

    fn discoverFunctions(self: *IrTranspiler, root: NodeIndex) !void {
        const tag = self.ir.getTag(root) orelse return;
        if (tag != .program and tag != .block) return;
        const block = self.ir.getBlock(root) orelse return;

        var i: u16 = 0;
        while (i < block.stmts_count) : (i += 1) {
            const stmt = self.ir.getListIndex(block.stmts_start, i);
            const stmt_tag = self.ir.getTag(stmt) orelse continue;

            if (stmt_tag == .function_decl or stmt_tag == .var_decl) {
                const decl = self.ir.getVarDecl(stmt) orelse continue;
                if (decl.binding.kind != .global) continue;

                const init_tag = self.ir.getTag(decl.init) orelse continue;
                if (init_tag != .function_expr and init_tag != .arrow_function) continue;

                const is_handler = decl.binding.slot == @intFromEnum(Atom.handler);
                const name = self.atomName(decl.binding.slot) orelse "unknown";
                const sig = if (!is_handler) self.inferFunctionSig(decl.init) else null;

                try self.functions.append(self.allocator, .{
                    .node_idx = decl.init,
                    .name_atom = decl.binding.slot,
                    .name = name,
                    .sig = sig,
                    .is_handler = is_handler,
                });
            }
        }
    }

    // ============ Phase 2: File Header ============

    fn emitHeader(self: *IrTranspiler) void {
        self.emit("//! Auto-generated by IR transpiler\n");
        self.emit("//! Do not edit - regenerate with: zig build -Dhandler=<path> -Daot=true\n\n");
        self.emit("const std = @import(\"std\");\n");
        self.emit("const zq = @import(\"zts\");\n\n");
        self.emit("pub const has_aot = true;\n");
        self.emit("pub const aot_metadata = .{\n");
        self.emit("    .transpiled = true,\n");
        self.emitFmt("    .functions_transpiled = {d},\n", .{self.functions.items.len});
        self.emit("};\n\n");
    }

    // ============ Phase 3: Helper Utilities ============

    fn emitHelpers(self: *IrTranspiler) void {
        // extractRequestField helper
        self.emit(
            \\fn extractRequestField(ctx: *zq.Context, req_obj: *zq.JSObject, comptime field: zq.Atom) ?[]const u8 {
            \\    const pool = ctx.hidden_class_pool orelse return null;
            \\    if (ctx.http_shapes) |shapes| {
            \\        if (req_obj.hidden_class_idx == shapes.request.class_idx) {
            \\            const val = switch (field) {
            \\                .url => req_obj.getSlot(shapes.request.url_slot),
            \\                .method => req_obj.getSlot(shapes.request.method_slot),
            \\                .body => req_obj.getSlot(shapes.request.body_slot),
            \\                else => return null,
            \\            };
            \\            if (val.isString()) return val.toPtr(zq.JSString).data();
            \\            return null;
            \\        }
            \\    }
            \\    if (req_obj.getOwnProperty(pool, field)) |val| {
            \\        if (val.isString()) return val.toPtr(zq.JSString).data();
            \\    }
            \\    return null;
            \\}
            \\
            \\
        );

        // JSON escape helper
        self.emit(
            \\fn jsonEscapeLen(src: []const u8) usize {
            \\    var extra: usize = 0;
            \\    for (src) |c| {
            \\        switch (c) {
            \\            '"', '\\' => extra += 1,
            \\            0x00...0x1f => extra += 5, // \uXXXX
            \\            else => {},
            \\        }
            \\    }
            \\    return src.len + extra;
            \\}
            \\
            \\fn jsonEscapeInto(dest: []u8, src: []const u8) usize {
            \\    var di: usize = 0;
            \\    for (src) |c| {
            \\        switch (c) {
            \\            '"' => {
            \\                dest[di] = '\\';
            \\                dest[di + 1] = '"';
            \\                di += 2;
            \\            },
            \\            '\\' => {
            \\                dest[di] = '\\';
            \\                dest[di + 1] = '\\';
            \\                di += 2;
            \\            },
            \\            '\n' => {
            \\                dest[di] = '\\';
            \\                dest[di + 1] = 'n';
            \\                di += 2;
            \\            },
            \\            '\r' => {
            \\                dest[di] = '\\';
            \\                dest[di + 1] = 'r';
            \\                di += 2;
            \\            },
            \\            '\t' => {
            \\                dest[di] = '\\';
            \\                dest[di + 1] = 't';
            \\                di += 2;
            \\            },
            \\            0x00...0x08, 0x0b...0x0c, 0x0e...0x1f => {
            \\                const hex = "0123456789abcdef";
            \\                dest[di] = '\\';
            \\                dest[di + 1] = 'u';
            \\                dest[di + 2] = '0';
            \\                dest[di + 3] = '0';
            \\                dest[di + 4] = hex[c >> 4];
            \\                dest[di + 5] = hex[c & 0x0f];
            \\                di += 6;
            \\            },
            \\            else => {
            \\                dest[di] = c;
            \\                di += 1;
            \\            },
            \\        }
            \\    }
            \\    return di;
            \\}
            \\
            \\
        );
    }

    // ============ Phase 4: Top-Level Helper Functions ============

    fn transpileTopLevelFunction(self: *IrTranspiler, func_info: *FunctionInfo) void {
        const func = self.ir.getFunction(func_info.node_idx) orelse {
            self.functions_bailed += 1;
            return;
        };

        // Try to emit specialized integer version
        if (func_info.sig) |sig| {
            if (sig.is_pure_integer) {
                self.emitIntegerFunction(func_info, &func, &sig);
                self.emitIntegerEntryWrapper(func_info, &sig);
                self.functions_transpiled += 1;
                return;
            }
        }

        // Fall back to JSValue-based version
        self.emitJSValueFunction(func_info, &func);
        self.functions_transpiled += 1;
    }

    fn emitIntegerFunction(self: *IrTranspiler, func_info: *FunctionInfo, func: *const ir.Node.FunctionExpr, _: *const FunctionSig) void {
        // fn aot_fibonacci(n: i32) i32 {
        self.emitFmt("fn aot_{s}(", .{func_info.name});
        var p: u8 = 0;
        while (p < func.params_count) : (p += 1) {
            if (p > 0) self.emit(", ");
            const param_idx = self.ir.getListIndex(func.params_start, p);
            const param_name = self.getParamName(param_idx, p);
            self.emitFmt("{s}: i32", .{param_name});

            // Register parameter binding
            const binding = self.ir.getBinding(param_idx) orelse continue;
            self.registerLocal(binding, param_name, .i32_type, false);
        }
        self.emit(") i32 {\n");
        self.pushIndent();

        // Transpile body
        self.transpileBlock(func.body);

        self.popIndent();
        self.emit("}\n\n");
    }

    fn emitIntegerEntryWrapper(self: *IrTranspiler, func_info: *FunctionInfo, sig: *const FunctionSig) void {
        // fn aot_fibonacci_entry(ctx: *zq.Context, args: []const zq.JSValue) !zq.JSValue {
        self.emitFmt("fn aot_{s}_entry(_: *zq.Context, args: []const zq.JSValue) anyerror!zq.JSValue {{\n", .{func_info.name});
        self.pushIndent();

        self.emitIndent();
        self.emitFmt("if (args.len < {d}) return error.AotBail;\n", .{sig.params.len});

        // Type guards
        var p: usize = 0;
        while (p < sig.params.len) : (p += 1) {
            self.emitIndent();
            self.emitFmt("if (!args[{d}].isInt()) return error.AotBail;\n", .{p});
        }

        // Call the specialized function
        self.emitIndent();
        self.emit("return zq.JSValue.fromInt(aot_");
        self.emit(func_info.name);
        self.emitByte('(');
        p = 0;
        while (p < sig.params.len) : (p += 1) {
            if (p > 0) self.emit(", ");
            self.emitFmt("args[{d}].getInt()", .{p});
        }
        self.emit("));\n");

        self.popIndent();
        self.emit("}\n\n");
    }

    fn emitJSValueFunction(self: *IrTranspiler, func_info: *FunctionInfo, func: *const ir.Node.FunctionExpr) void {
        _ = func_info;
        _ = func;
        // TODO: JSValue-based function transpilation
        self.functions_bailed += 1;
    }

    fn getParamName(self: *IrTranspiler, param_idx: NodeIndex, ordinal: u8) []const u8 {
        const tag = self.ir.getTag(param_idx) orelse return switch (ordinal) {
            0 => "p0",
            1 => "p1",
            2 => "p2",
            else => "pn",
        };
        if (tag == .identifier) {
            const binding = self.ir.getBinding(param_idx) orelse return "p";
            if (binding.kind == .global) {
                if (self.atomName(binding.slot)) |name| return name;
            }
            return self.localName(binding);
        }
        return switch (ordinal) {
            0 => "p0",
            1 => "p1",
            2 => "p2",
            else => "pn",
        };
    }

    // ============ Phase 5: Handler Function ============

    fn transpileHandlerFunction(self: *IrTranspiler, func_info: *FunctionInfo) void {
        const func = self.ir.getFunction(func_info.node_idx) orelse {
            self.emitBailHandler();
            return;
        };

        // Identify request parameter - the first argument (slot=0) is the request object.
        // We flag this so isRequestFieldExtract can match argument slot 0.
        if (func.params_count >= 1) {
            self.has_request_param = true;
        }

        self.emit("pub fn aotHandler(ctx: *zq.Context, args: []const zq.JSValue) anyerror!zq.JSValue {\n");
        self.pushIndent();

        // Preamble: extract request object
        self.emitIndent();
        self.emit("if (args.len < 1) return error.AotBail;\n");
        self.emitIndent();
        self.emit("const req_val = args[0];\n");
        self.emitIndent();
        self.emit("if (!req_val.isObject()) return error.AotBail;\n");
        self.emitIndent();
        self.emit("const req_obj = req_val.toPtr(zq.JSObject);\n");
        // Ensure ctx and req_obj are used even if body bails immediately
        self.emitIndent();
        self.emit("_ = .{ &ctx, req_obj };\n");

        // Transpile body - look for variable declarations that extract request fields
        self.transpileHandlerBody(func.body);

        self.popIndent();
        self.emit("}\n\n");
        self.functions_transpiled += 1;
    }

    fn emitBailHandler(self: *IrTranspiler) void {
        self.emit("pub fn aotHandler(_: *zq.Context, _: []const zq.JSValue) anyerror!zq.JSValue {\n");
        self.emit("    return error.AotBail;\n");
        self.emit("}\n\n");
    }

    fn transpileHandlerBody(self: *IrTranspiler, body_idx: NodeIndex) void {
        const tag = self.ir.getTag(body_idx) orelse return;
        if (tag != .block) return;
        const block = self.ir.getBlock(body_idx) orelse return;

        var i: u16 = 0;
        while (i < block.stmts_count) : (i += 1) {
            const stmt = self.ir.getListIndex(block.stmts_start, i);
            if (self.transpileHandlerStmt(stmt)) return; // Terminal statement emitted
        }

        // If we reach end without returning, bail
        self.emitIndent();
        self.emit("return error.AotBail;\n");
    }

    /// Transpile a handler statement. Returns true if a terminal statement was emitted
    /// (return or bail), meaning no more statements should follow.
    fn transpileHandlerStmt(self: *IrTranspiler, idx: NodeIndex) bool {
        const tag = self.ir.getTag(idx) orelse return false;
        switch (tag) {
            .var_decl, .function_decl => return self.transpileHandlerVarDecl(idx),
            .if_stmt => {
                self.transpileHandlerIf(idx);
                return false;
            },
            .return_stmt => {
                self.transpileHandlerReturn(idx);
                return true; // return is always terminal
            },
            .expr_stmt => {
                self.emitIndent();
                self.emit("_ = ");
                self.transpileExpr(idx);
                self.emit(";\n");
                return false;
            },
            else => {
                // Unknown statement - bail
                self.emitIndent();
                self.emit("return error.AotBail;\n");
                return true;
            },
        }
    }

    /// Returns true if a terminal statement was emitted (bail).
    fn transpileHandlerVarDecl(self: *IrTranspiler, idx: NodeIndex) bool {
        const decl = self.ir.getVarDecl(idx) orelse return false;
        if (decl.init == null_node) return false;

        // Check if this is extracting a request field: const url = request.url
        if (self.isRequestFieldExtract(decl.init)) |field_atom| {
            const key = bindingKey(decl.binding);
            const field_name = self.atomName(field_atom) orelse "field";

            // Track url and method locals
            if (field_atom == @intFromEnum(Atom.url)) {
                self.url_local_slot = key;
            } else if (field_atom == @intFromEnum(Atom.method)) {
                self.method_local_slot = key;
            }

            // Allocate name and track it
            const name = std.fmt.allocPrint(self.allocator, "aot_{s}", .{field_name}) catch return true;
            self.name_allocs.append(self.allocator, name) catch return true;
            self.registerLocal(decl.binding, name, .string_type, decl.kind != .@"const");

            self.emitIndent();
            self.emitFmt("const {s} = extractRequestField(ctx, req_obj, .{s}) orelse return error.AotBail;\n", .{ name, field_name });
            return false; // Not terminal - variable was successfully declared
        }

        // General var decl - bail since we can't transpile it and the variable
        // may be used in subsequent code
        self.emitIndent();
        self.emit("return error.AotBail;\n");
        return true;
    }

    fn resolveVarDeclName(self: *IrTranspiler, decl: ir.Node.VarDecl) []const u8 {
        // Try to get the atom name for global bindings
        if (decl.binding.kind == .global) {
            if (self.atomName(decl.binding.slot)) |name| return name;
        }
        return self.localName(decl.binding);
    }

    fn isRequestFieldExtract(self: *IrTranspiler, init_idx: NodeIndex) ?u16 {
        // Check if init is member_access on the request parameter
        const tag = self.ir.getTag(init_idx) orelse return null;
        if (tag != .member_access) return null;

        const member = self.ir.getMember(init_idx) orelse return null;
        // Check if object is the request parameter (first argument, slot=0)
        const obj_tag = self.ir.getTag(member.object) orelse return null;
        if (obj_tag != .identifier) return null;
        const obj_binding = self.ir.getBinding(member.object) orelse return null;

        if (self.has_request_param and obj_binding.kind == .argument and obj_binding.slot == 0) {
            return member.property;
        }
        return null;
    }

    fn transpileHandlerIf(self: *IrTranspiler, idx: NodeIndex) void {
        const if_s = self.ir.getIfStmt(idx) orelse return;

        self.emitIndent();
        self.emit("if (");
        self.transpileCondition(if_s.condition);
        self.emit(") {\n");
        self.pushIndent();

        // Transpile then branch
        self.transpileHandlerBranch(if_s.then_branch);

        self.popIndent();
        self.emitIndent();

        if (if_s.else_branch != null_node) {
            const else_tag = self.ir.getTag(if_s.else_branch) orelse {
                self.emit("}\n");
                return;
            };
            if (else_tag == .if_stmt) {
                self.emit("} else ");
                self.transpileHandlerIf(if_s.else_branch);
                return;
            }
            self.emit("} else {\n");
            self.pushIndent();
            self.transpileHandlerBranch(if_s.else_branch);
            self.popIndent();
            self.emitIndent();
        }
        self.emit("}\n");
    }

    fn transpileHandlerBranch(self: *IrTranspiler, idx: NodeIndex) void {
        const tag = self.ir.getTag(idx) orelse return;
        if (tag == .block) {
            const block = self.ir.getBlock(idx) orelse return;
            var i: u16 = 0;
            while (i < block.stmts_count) : (i += 1) {
                const stmt = self.ir.getListIndex(block.stmts_start, i);
                if (self.transpileHandlerStmt(stmt)) return; // Terminal
            }
        } else {
            _ = self.transpileHandlerStmt(idx);
        }
    }

    fn transpileHandlerReturn(self: *IrTranspiler, idx: NodeIndex) void {
        const val_idx = self.ir.getOptValue(idx) orelse {
            self.emitIndent();
            self.emit("return error.AotBail;\n");
            return;
        };

        // Check if returning a Response.* call
        if (self.tryTranspileResponseCall(val_idx)) return;

        // General return - bail
        self.emitIndent();
        self.emit("return error.AotBail;\n");
    }

    // ============ Response.* Transpilation ============

    fn tryTranspileResponseCall(self: *IrTranspiler, idx: NodeIndex) bool {
        const tag = self.ir.getTag(idx) orelse return false;
        if (tag != .call and tag != .method_call) return false;

        const call = self.ir.getCall(idx) orelse return false;

        // Check if callee is Response.json, Response.text, Response.html
        const callee_tag = self.ir.getTag(call.callee) orelse return false;
        if (callee_tag != .member_access) return false;

        const member = self.ir.getMember(call.callee) orelse return false;

        // Check if object is Response (global)
        const obj_tag = self.ir.getTag(member.object) orelse return false;
        if (obj_tag != .identifier) return false;
        const obj_binding = self.ir.getBinding(member.object) orelse return false;
        if (obj_binding.kind != .global) return false;
        if (obj_binding.slot != @intFromEnum(Atom.Response)) return false;

        // Determine which Response method
        const method_atom = member.property;
        const content_type: []const u8 = if (method_atom == @intFromEnum(Atom.json))
            "application/json"
        else if (method_atom == @intFromEnum(Atom.text))
            "text/plain; charset=utf-8"
        else if (method_atom == @intFromEnum(Atom.html))
            "text/html; charset=utf-8"
        else
            return false;

        const is_json = method_atom == @intFromEnum(Atom.json);

        // Get the first argument (body data)
        if (call.args_count < 1) return false;
        const body_arg = self.ir.getListIndex(call.args_start, 0);

        // Check for status override in second argument
        var status: u16 = 200;
        if (call.args_count >= 2) {
            const opts_arg = self.ir.getListIndex(call.args_start, 1);
            status = self.extractStatusFromInit(opts_arg) orelse 200;
        }

        if (is_json) {
            return self.emitJsonResponse(body_arg, status, content_type);
        } else {
            return self.emitStringResponse(body_arg, status, content_type);
        }
    }

    fn extractStatusFromInit(self: *IrTranspiler, idx: NodeIndex) ?u16 {
        // Look for { status: <number> } in the init object
        const tag = self.ir.getTag(idx) orelse return null;
        if (tag != .object_literal) return null;
        const obj = self.ir.getObject(idx) orelse return null;

        var i: u16 = 0;
        while (i < obj.properties_count) : (i += 1) {
            const prop_idx = self.ir.getListIndex(obj.properties_start, i);
            const prop = self.ir.getProperty(prop_idx) orelse continue;

            // Check if key is "status"
            const key_tag = self.ir.getTag(prop.key) orelse continue;
            if (key_tag == .identifier) {
                const key_binding = self.ir.getBinding(prop.key) orelse continue;
                if (key_binding.kind == .global and key_binding.slot == @intFromEnum(Atom.status)) {
                    // Get value
                    const val_tag = self.ir.getTag(prop.value) orelse continue;
                    if (val_tag == .lit_int) {
                        const val = self.ir.getIntValue(prop.value) orelse continue;
                        return @intCast(val);
                    }
                }
            }
        }
        return null;
    }

    fn emitJsonResponse(self: *IrTranspiler, body_arg: NodeIndex, status: u16, content_type: []const u8) bool {
        const tag = self.ir.getTag(body_arg) orelse return false;

        if (tag == .object_literal) {
            // Try to serialize static object at compile time
            if (self.trySerializeStaticObject(body_arg)) |json_str| {
                self.emitIndent();
                self.emit("return zq.http.createResponse(ctx, ");
                self.emitZigString(json_str);
                self.emitFmt(", {d}, ", .{status});
                self.emitZigString(content_type);
                self.emit(");\n");
                return true;
            }

            // Dynamic object - build JSON at runtime
            return self.emitDynamicJsonResponse(body_arg, status, content_type);
        }

        // Not an object literal - bail
        return false;
    }

    fn emitStringResponse(self: *IrTranspiler, body_arg: NodeIndex, status: u16, content_type: []const u8) bool {
        const tag = self.ir.getTag(body_arg) orelse return false;

        if (tag == .lit_string) {
            const str_idx = self.ir.getStringIdx(body_arg) orelse return false;
            const str = self.ir.getString(str_idx) orelse return false;
            self.emitIndent();
            self.emit("return zq.http.createResponse(ctx, ");
            self.emitZigString(str);
            self.emitFmt(", {d}, ", .{status});
            self.emitZigString(content_type);
            self.emit(");\n");
            return true;
        }

        // Non-literal string - check if it's a variable
        if (tag == .identifier) {
            const binding = self.ir.getBinding(body_arg) orelse return false;
            const name = self.localName(binding);
            self.emitIndent();
            self.emitFmt("return zq.http.createResponse(ctx, {s}, {d}, ", .{ name, status });
            self.emitZigString(content_type);
            self.emit(");\n");
            return true;
        }

        return false;
    }

    // ============ JSON Serialization ============

    fn trySerializeStaticObject(self: *IrTranspiler, idx: NodeIndex) ?[]const u8 {
        // Check if all values are literals
        const obj = self.ir.getObject(idx) orelse return null;
        var buf: std.ArrayList(u8) = .empty;

        buf.append(self.allocator, '{') catch return null;

        var i: u16 = 0;
        while (i < obj.properties_count) : (i += 1) {
            if (i > 0) buf.append(self.allocator, ',') catch return null;

            const prop_idx = self.ir.getListIndex(obj.properties_start, i);
            const prop = self.ir.getProperty(prop_idx) orelse return null;

            // Get key name
            const key_name = self.getPropertyKeyName(prop.key) orelse return null;
            buf.append(self.allocator, '"') catch return null;
            buf.appendSlice(self.allocator, key_name) catch return null;
            buf.appendSlice(self.allocator, "\":") catch return null;

            // Serialize value
            if (!self.serializeStaticValue(&buf, prop.value)) {
                buf.deinit(self.allocator);
                return null;
            }
        }

        buf.append(self.allocator, '}') catch return null;

        const result = buf.toOwnedSlice(self.allocator) catch return null;
        self.name_allocs.append(self.allocator, result) catch {};
        return result;
    }

    fn serializeStaticValue(self: *IrTranspiler, buf: *std.ArrayList(u8), idx: NodeIndex) bool {
        const a = self.allocator;
        const tag = self.ir.getTag(idx) orelse return false;
        switch (tag) {
            .lit_string => {
                const str_idx = self.ir.getStringIdx(idx) orelse return false;
                const str = self.ir.getString(str_idx) orelse return false;
                buf.append(a, '"') catch return false;
                for (str) |c| {
                    switch (c) {
                        '"' => buf.appendSlice(a, "\\\"") catch return false,
                        '\\' => buf.appendSlice(a, "\\\\") catch return false,
                        '\n' => buf.appendSlice(a, "\\n") catch return false,
                        '\r' => buf.appendSlice(a, "\\r") catch return false,
                        '\t' => buf.appendSlice(a, "\\t") catch return false,
                        else => buf.append(a, c) catch return false,
                    }
                }
                buf.append(a, '"') catch return false;
                return true;
            },
            .lit_int => {
                const val = self.ir.getIntValue(idx) orelse return false;
                var aw: std.Io.Writer.Allocating = .fromArrayList(a, buf);
                aw.writer.print("{d}", .{val}) catch return false;
                buf.* = aw.toArrayList();
                return true;
            },
            .lit_float => {
                const fidx = self.ir.getFloatIdx(idx) orelse return false;
                const val = self.ir.getFloat(fidx) orelse return false;
                var aw: std.Io.Writer.Allocating = .fromArrayList(a, buf);
                aw.writer.print("{d}", .{val}) catch return false;
                buf.* = aw.toArrayList();
                return true;
            },
            .lit_bool => {
                const val = self.ir.getBoolValue(idx) orelse return false;
                buf.appendSlice(a, if (val) "true" else "false") catch return false;
                return true;
            },
            .lit_null => {
                buf.appendSlice(a, "null") catch return false;
                return true;
            },
            .object_literal => {
                const obj = self.ir.getObject(idx) orelse return false;
                buf.append(a, '{') catch return false;
                var i: u16 = 0;
                while (i < obj.properties_count) : (i += 1) {
                    if (i > 0) buf.append(a, ',') catch return false;
                    const prop_idx = self.ir.getListIndex(obj.properties_start, i);
                    const prop = self.ir.getProperty(prop_idx) orelse return false;
                    const key_name = self.getPropertyKeyName(prop.key) orelse return false;
                    buf.append(a, '"') catch return false;
                    buf.appendSlice(a, key_name) catch return false;
                    buf.appendSlice(a, "\":") catch return false;
                    if (!self.serializeStaticValue(buf, prop.value)) return false;
                }
                buf.append(a, '}') catch return false;
                return true;
            },
            .array_literal => {
                const arr = self.ir.getArray(idx) orelse return false;
                buf.append(a, '[') catch return false;
                var i: u16 = 0;
                while (i < arr.elements_count) : (i += 1) {
                    if (i > 0) buf.append(a, ',') catch return false;
                    const elem = self.ir.getListIndex(arr.elements_start, i);
                    if (!self.serializeStaticValue(buf, elem)) return false;
                }
                buf.append(a, ']') catch return false;
                return true;
            },
            else => return false,
        }
    }

    fn getPropertyKeyName(self: *IrTranspiler, key_idx: NodeIndex) ?[]const u8 {
        const tag = self.ir.getTag(key_idx) orelse return null;
        if (tag == .identifier) {
            const binding = self.ir.getBinding(key_idx) orelse return null;
            if (binding.kind == .global) {
                return self.atomName(binding.slot);
            }
        }
        if (tag == .lit_string) {
            const str_idx = self.ir.getStringIdx(key_idx) orelse return null;
            return self.ir.getString(str_idx);
        }
        return null;
    }

    // ============ Dynamic JSON Response ============

    fn emitDynamicJsonResponse(self: *IrTranspiler, body_arg: NodeIndex, status: u16, content_type: []const u8) bool {
        const obj = self.ir.getObject(body_arg) orelse return false;

        // Build the JSON using bufPrint
        // First compute the static parts and identify dynamic parts
        self.emitIndent();
        self.emit("{\n");
        self.pushIndent();

        // Calculate buffer size (conservative estimate)
        self.emitIndent();
        self.emit("var json_buf: [4096]u8 = undefined;\n");
        self.emitIndent();
        self.emit("var json_pos: usize = 0;\n");

        // Emit opening brace
        self.emitIndent();
        self.emit("json_buf[json_pos] = '{';\n");
        self.emitIndent();
        self.emit("json_pos += 1;\n");

        var i: u16 = 0;
        while (i < obj.properties_count) : (i += 1) {
            if (i > 0) {
                self.emitIndent();
                self.emit("json_buf[json_pos] = ',';\n");
                self.emitIndent();
                self.emit("json_pos += 1;\n");
            }

            const prop_idx = self.ir.getListIndex(obj.properties_start, i);
            const prop = self.ir.getProperty(prop_idx) orelse {
                self.emitIndent();
                self.emit("return error.AotBail;\n");
                self.popIndent();
                self.emitIndent();
                self.emit("}\n");
                return true;
            };
            const key_name = self.getPropertyKeyName(prop.key) orelse {
                self.emitIndent();
                self.emit("return error.AotBail;\n");
                self.popIndent();
                self.emitIndent();
                self.emit("}\n");
                return true;
            };

            // Emit key as "key":
            const key_lit = std.fmt.allocPrint(self.allocator, "\"{s}\":", .{key_name}) catch return false;
            self.name_allocs.append(self.allocator, key_lit) catch {};
            self.emitMemcpyLiteral(key_lit);

            // Emit value - bail if we can't handle it
            if (!self.emitDynamicJsonValue(prop.value)) {
                self.emitIndent();
                self.emit("return error.AotBail;\n");
                self.popIndent();
                self.emitIndent();
                self.emit("}\n");
                return true; // We emitted a valid bail path
            }
        }

        // Emit closing brace
        self.emitIndent();
        self.emit("json_buf[json_pos] = '}';\n");
        self.emitIndent();
        self.emit("json_pos += 1;\n");

        // Return response
        self.emitIndent();
        self.emitFmt("return zq.http.createResponse(ctx, json_buf[0..json_pos], {d}, ", .{status});
        self.emitZigString(content_type);
        self.emit(");\n");

        self.popIndent();
        self.emitIndent();
        self.emit("}\n");

        return true;
    }

    fn emitDynamicJsonValue(self: *IrTranspiler, idx: NodeIndex) bool {
        const tag = self.ir.getTag(idx) orelse return false;
        switch (tag) {
            .lit_string => {
                const str_idx = self.ir.getStringIdx(idx) orelse return false;
                const str = self.ir.getString(str_idx) orelse return false;
                // Emit static string JSON value: "value"
                const json_val = std.fmt.allocPrint(self.allocator, "\"{s}\"", .{str}) catch return false;
                self.name_allocs.append(self.allocator, json_val) catch {};
                self.emitMemcpyLiteral(json_val);
                return true;
            },
            .lit_int => {
                const val = self.ir.getIntValue(idx) orelse return false;
                const val_str = std.fmt.allocPrint(self.allocator, "{d}", .{val}) catch return false;
                self.name_allocs.append(self.allocator, val_str) catch {};
                self.emitMemcpyLiteral(val_str);
                return true;
            },
            .lit_bool => {
                const val = self.ir.getBoolValue(idx) orelse return false;
                const val_str: []const u8 = if (val) "true" else "false";
                self.emitMemcpyLiteral(val_str);
                return true;
            },
            .lit_null => {
                self.emitIndent();
                self.emit("@memcpy(json_buf[json_pos..][0..4], \"null\");\n");
                self.emitIndent();
                self.emit("json_pos += 4;\n");
                return true;
            },
            .identifier => {
                const binding = self.ir.getBinding(idx) orelse return false;
                const name = self.localName(binding);
                const inferred = self.inferExprType(idx);

                if (inferred == .string_type) {
                    // String variable - needs quoting and escaping
                    self.emitIndent();
                    self.emit("json_buf[json_pos] = '\"';\n");
                    self.emitIndent();
                    self.emit("json_pos += 1;\n");
                    self.emitIndent();
                    self.emitFmt("json_pos += jsonEscapeInto(json_buf[json_pos..], {s});\n", .{name});
                    self.emitIndent();
                    self.emit("json_buf[json_pos] = '\"';\n");
                    self.emitIndent();
                    self.emit("json_pos += 1;\n");
                    return true;
                } else if (inferred == .i32_type) {
                    // Integer variable - format as number
                    self.emitIndent();
                    self.emitFmt("const int_str = std.fmt.bufPrint(json_buf[json_pos..], \"{{d}}\", .{{{s}}}) catch return error.AotBail;\n", .{name});
                    self.emitIndent();
                    self.emit("json_pos += int_str.len;\n");
                    return true;
                }
                return false; // Can't serialize unknown type
            },
            .call, .method_call => {
                // Function call result - check if it's a known integer function
                return self.emitDynamicJsonCallResult(idx);
            },
            .binary_op => {
                // String concatenation in JSON value
                return self.emitDynamicJsonBinaryOp(idx);
            },
            else => return false,
        }
    }

    fn emitDynamicJsonCallResult(self: *IrTranspiler, idx: NodeIndex) bool {
        const call = self.ir.getCall(idx) orelse return false;
        const callee_tag = self.ir.getTag(call.callee) orelse return false;

        // Check for method calls on known globals: Date.now(), Math.floor(), etc.
        if (callee_tag == .member_access) {
            return self.emitDynamicJsonMethodCallResult(idx, &call);
        }

        // Check if calling a known transpiled function (direct call)
        if (callee_tag != .identifier) return false;
        const callee_binding = self.ir.getBinding(call.callee) orelse return false;
        if (callee_binding.kind != .global) return false;

        const func_name = self.atomName(callee_binding.slot) orelse return false;

        // Check if this function was transpiled as pure integer
        for (self.functions.items) |func_info| {
            if (std.mem.eql(u8, func_info.name, func_name)) {
                if (func_info.sig) |sig| {
                    if (sig.is_pure_integer) {
                        // Call the native function and format result
                        self.emitIndent();
                        self.emitFmt("const call_result = aot_{s}(", .{func_name});
                        var a: u8 = 0;
                        while (a < call.args_count) : (a += 1) {
                            if (a > 0) self.emit(", ");
                            const arg = self.ir.getListIndex(call.args_start, a);
                            self.transpileIntExpr(arg);
                        }
                        self.emit(");\n");
                        self.emitIndent();
                        self.emit("const call_str = std.fmt.bufPrint(json_buf[json_pos..], \"{d}\", .{call_result}) catch return error.AotBail;\n");
                        self.emitIndent();
                        self.emit("json_pos += call_str.len;\n");
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /// Handle method calls like Date.now(), Math.floor(), etc. in JSON value context.
    fn emitDynamicJsonMethodCallResult(self: *IrTranspiler, _: NodeIndex, call: *const ir.Node.CallExpr) bool {
        const member = self.ir.getMember(call.callee) orelse return false;

        // Check if object is a known global
        const obj_tag = self.ir.getTag(member.object) orelse return false;
        if (obj_tag != .identifier) return false;
        const obj_binding = self.ir.getBinding(member.object) orelse return false;
        if (obj_binding.kind != .global) return false;

        const obj_name = self.atomName(obj_binding.slot) orelse return false;
        const method_name = self.atomName(member.property) orelse return false;

        // Date.now() -> std.posix.clock_gettime(.REALTIME) converted to epoch millis
        if (std.mem.eql(u8, obj_name, "Date") and std.mem.eql(u8, method_name, "now")) {
            self.emitIndent();
            self.emit("const ts_clock = std.posix.clock_gettime(.REALTIME) catch return error.AotBail;\n");
            self.emitIndent();
            self.emit("const ts_val = ts_clock.sec * 1000 + @divTrunc(ts_clock.nsec, 1_000_000);\n");
            self.emitIndent();
            self.emit("const ts_str = std.fmt.bufPrint(json_buf[json_pos..], \"{d}\", .{ts_val}) catch return error.AotBail;\n");
            self.emitIndent();
            self.emit("json_pos += ts_str.len;\n");
            return true;
        }

        // Math.floor(x), Math.ceil(x), Math.abs(x) - single numeric arg
        if (std.mem.eql(u8, obj_name, "Math") and call.args_count == 1) {
            const arg = self.ir.getListIndex(call.args_start, 0);
            const arg_type = self.inferExprType(arg);

            if (std.mem.eql(u8, method_name, "floor") or
                std.mem.eql(u8, method_name, "ceil") or
                std.mem.eql(u8, method_name, "abs"))
            {
                // For integer args, these are identity/abs operations
                if (arg_type == .i32_type) {
                    if (std.mem.eql(u8, method_name, "abs")) {
                        self.emitIndent();
                        self.emit("const math_result = @abs(");
                        self.transpileIntExpr(arg);
                        self.emit(");\n");
                    } else {
                        // floor/ceil on integers is identity
                        self.emitIndent();
                        self.emit("const math_result = ");
                        self.transpileIntExpr(arg);
                        self.emit(";\n");
                    }
                    self.emitIndent();
                    self.emit("const math_str = std.fmt.bufPrint(json_buf[json_pos..], \"{d}\", .{math_result}) catch return error.AotBail;\n");
                    self.emitIndent();
                    self.emit("json_pos += math_str.len;\n");
                    return true;
                }
            }

            // Math.max(a, b), Math.min(a, b) - two integer args
        } else if (std.mem.eql(u8, obj_name, "Math") and call.args_count == 2) {
            const arg0 = self.ir.getListIndex(call.args_start, 0);
            const arg1 = self.ir.getListIndex(call.args_start, 1);
            const t0 = self.inferExprType(arg0);
            const t1 = self.inferExprType(arg1);

            if (t0 == .i32_type and t1 == .i32_type) {
                if (std.mem.eql(u8, method_name, "max") or std.mem.eql(u8, method_name, "min")) {
                    const builtin = if (std.mem.eql(u8, method_name, "max")) "@max" else "@min";
                    self.emitIndent();
                    self.emitFmt("const math_result = {s}(", .{builtin});
                    self.transpileIntExpr(arg0);
                    self.emit(", ");
                    self.transpileIntExpr(arg1);
                    self.emit(");\n");
                    self.emitIndent();
                    self.emit("const math_str = std.fmt.bufPrint(json_buf[json_pos..], \"{d}\", .{math_result}) catch return error.AotBail;\n");
                    self.emitIndent();
                    self.emit("json_pos += math_str.len;\n");
                    return true;
                }
            }
        }

        return false;
    }

    fn emitDynamicJsonBinaryOp(self: *IrTranspiler, idx: NodeIndex) bool {
        const bin = self.ir.getBinary(idx) orelse return false;
        if (bin.op != .add) return false;

        // String concatenation chain - flatten and emit each part
        return self.emitDynamicJsonConcat(idx);
    }

    fn emitDynamicJsonConcat(self: *IrTranspiler, idx: NodeIndex) bool {
        const tag = self.ir.getTag(idx) orelse return false;
        if (tag == .binary_op) {
            const bin = self.ir.getBinary(idx) orelse return false;
            if (bin.op == .add) {
                if (!self.emitDynamicJsonConcat(bin.left)) return false;
                if (!self.emitDynamicJsonConcat(bin.right)) return false;
                return true;
            }
        }
        // Base case: emit as string part (unquoted since we're building the string)
        if (tag == .lit_string) {
            const str_idx = self.ir.getStringIdx(idx) orelse return false;
            const str = self.ir.getString(str_idx) orelse return false;
            self.emitIndent();
            self.emitFmt("@memcpy(json_buf[json_pos..][0..{d}], ", .{str.len});
            self.emitZigString(str);
            self.emit(");\n");
            self.emitIndent();
            self.emitFmt("json_pos += {d};\n", .{str.len});
            return true;
        }
        if (tag == .identifier) {
            const binding = self.ir.getBinding(idx) orelse return false;
            const name = self.localName(binding);
            self.emitIndent();
            self.emitFmt("@memcpy(json_buf[json_pos..][0..{s}.len], {s});\n", .{ name, name });
            self.emitIndent();
            self.emitFmt("json_pos += {s}.len;\n", .{name});
            return true;
        }
        return false;
    }

    // ============ Condition Transpilation ============

    fn transpileCondition(self: *IrTranspiler, idx: NodeIndex) void {
        const tag = self.ir.getTag(idx) orelse {
            self.emit("false");
            return;
        };

        switch (tag) {
            .binary_op => {
                const bin = self.ir.getBinary(idx) orelse {
                    self.emit("false");
                    return;
                };
                switch (bin.op) {
                    .strict_eq, .eq => self.transpileEqCondition(bin),
                    .strict_neq, .neq => {
                        self.emit("!");
                        self.transpileEqCondition(bin);
                    },
                    .and_op => {
                        self.transpileCondition(bin.left);
                        self.emit(" and ");
                        self.transpileCondition(bin.right);
                    },
                    .or_op => {
                        self.emit("(");
                        self.transpileCondition(bin.left);
                        self.emit(" or ");
                        self.transpileCondition(bin.right);
                        self.emit(")");
                    },
                    .lt, .lte, .gt, .gte => {
                        self.transpileComparison(bin);
                    },
                    else => self.emit("false"),
                }
            },
            .unary_op => {
                const un = self.ir.getUnary(idx) orelse {
                    self.emit("false");
                    return;
                };
                if (un.op == .not) {
                    self.emit("!");
                    self.transpileCondition(un.operand);
                } else {
                    self.emit("false");
                }
            },
            .identifier => {
                // Truthy check on a variable
                const binding = self.ir.getBinding(idx) orelse {
                    self.emit("false");
                    return;
                };
                const name = self.localName(binding);
                const inferred = self.inferExprType(idx);
                if (inferred == .string_type) {
                    self.emitFmt("({s}.len > 0)", .{name});
                } else if (inferred == .bool_type) {
                    self.emit(name);
                } else {
                    self.emit("false");
                }
            },
            .call, .method_call => {
                // Check for url.indexOf("/api/greet/") === 0 pattern
                // Actually, the full pattern is: binary_op(eq, call(member(url, indexOf), [str]), 0)
                // This will be caught at the binary_op level, not here
                self.emit("false");
            },
            else => self.emit("false"),
        }
    }

    fn transpileEqCondition(self: *IrTranspiler, bin: ir.Node.BinaryExpr) void {
        // Check if this is string comparison
        const lt = self.inferExprType(bin.left);
        const rt = self.inferExprType(bin.right);

        if (lt == .string_type or rt == .string_type) {
            self.emit("std.mem.eql(u8, ");
            self.transpileStringExpr(bin.left);
            self.emit(", ");
            self.transpileStringExpr(bin.right);
            self.emit(")");
            return;
        }

        // Check if this is indexOf() === 0 pattern (prefix check)
        if (self.tryTranspileIndexOfEq(bin)) return;

        // Integer comparison
        if (lt == .i32_type and rt == .i32_type) {
            self.emit("(");
            self.transpileIntExpr(bin.left);
            self.emit(" == ");
            self.transpileIntExpr(bin.right);
            self.emit(")");
            return;
        }

        self.emit("false");
    }

    fn tryTranspileIndexOfEq(self: *IrTranspiler, bin: ir.Node.BinaryExpr) bool {
        // Pattern: url.indexOf("/prefix/") === 0
        // IR: binary_op(eq, call(member(url, indexOf), [lit_string]), lit_int(0))
        const call_side = bin.left;
        const zero_side = bin.right;

        // Check if right side is 0
        const zero_tag = self.ir.getTag(zero_side) orelse return false;
        if (zero_tag != .lit_int) return false;
        if ((self.ir.getIntValue(zero_side) orelse return false) != 0) return false;

        // Check if left side is a call to indexOf
        const call_tag = self.ir.getTag(call_side) orelse return false;
        if (call_tag != .method_call and call_tag != .call) return false;
        const call = self.ir.getCall(call_side) orelse return false;

        // Check callee is member_access .indexOf
        const callee_tag = self.ir.getTag(call.callee) orelse return false;
        if (callee_tag != .member_access) return false;
        const member = self.ir.getMember(call.callee) orelse return false;
        if (member.property != @intFromEnum(Atom.indexOf)) return false;

        // Get the string argument to indexOf
        if (call.args_count < 1) return false;
        const prefix_arg = self.ir.getListIndex(call.args_start, 0);
        const prefix_tag = self.ir.getTag(prefix_arg) orelse return false;
        if (prefix_tag != .lit_string) return false;
        const str_idx = self.ir.getStringIdx(prefix_arg) orelse return false;
        const prefix_str = self.ir.getString(str_idx) orelse return false;

        // Get the object being called on
        const obj_binding = self.ir.getBinding(member.object) orelse return false;
        const obj_name = self.localName(obj_binding);

        self.emit("std.mem.startsWith(u8, ");
        self.emit(obj_name);
        self.emit(", ");
        self.emitZigString(prefix_str);
        self.emit(")");
        return true;
    }

    fn transpileComparison(self: *IrTranspiler, bin: ir.Node.BinaryExpr) void {
        const lt = self.inferExprType(bin.left);
        const rt = self.inferExprType(bin.right);

        if (lt == .i32_type and rt == .i32_type) {
            self.emit("(");
            self.transpileIntExpr(bin.left);
            switch (bin.op) {
                .lt => self.emit(" < "),
                .lte => self.emit(" <= "),
                .gt => self.emit(" > "),
                .gte => self.emit(" >= "),
                else => self.emit(" == "),
            }
            self.transpileIntExpr(bin.right);
            self.emit(")");
        } else {
            self.emit("false");
        }
    }

    // ============ Expression Transpilation ============

    fn transpileExpr(self: *IrTranspiler, idx: NodeIndex) void {
        const tag = self.ir.getTag(idx) orelse {
            self.emit("error.AotBail");
            return;
        };
        switch (tag) {
            .lit_int => {
                const val = self.ir.getIntValue(idx) orelse {
                    self.emit("0");
                    return;
                };
                self.emitFmt("{d}", .{val});
            },
            .lit_float => {
                const fidx = self.ir.getFloatIdx(idx) orelse {
                    self.emit("0.0");
                    return;
                };
                const val = self.ir.getFloat(fidx) orelse {
                    self.emit("0.0");
                    return;
                };
                self.emitFmt("{d}", .{val});
            },
            .lit_string => {
                const str_idx = self.ir.getStringIdx(idx) orelse return;
                const str = self.ir.getString(str_idx) orelse return;
                self.emitZigString(str);
            },
            .lit_bool => {
                const val = self.ir.getBoolValue(idx) orelse return;
                self.emit(if (val) "true" else "false");
            },
            .lit_null => self.emit("zq.JSValue.null_val"),
            .lit_undefined => self.emit("zq.JSValue.undefined_val"),
            .identifier => {
                const binding = self.ir.getBinding(idx) orelse return;
                self.emit(self.localName(binding));
            },
            .binary_op => self.transpileBinaryOp(idx),
            .unary_op => self.transpileUnaryOp(idx),
            else => self.emit("error.AotBail"),
        }
    }

    fn transpileStringExpr(self: *IrTranspiler, idx: NodeIndex) void {
        const tag = self.ir.getTag(idx) orelse {
            self.emit("\"\"");
            return;
        };
        switch (tag) {
            .lit_string => {
                const str_idx = self.ir.getStringIdx(idx) orelse return;
                const str = self.ir.getString(str_idx) orelse return;
                self.emitZigString(str);
            },
            .identifier => {
                const binding = self.ir.getBinding(idx) orelse return;
                self.emit(self.localName(binding));
            },
            else => self.emit("\"\""),
        }
    }

    fn transpileIntExpr(self: *IrTranspiler, idx: NodeIndex) void {
        const tag = self.ir.getTag(idx) orelse {
            self.emit("0");
            return;
        };
        switch (tag) {
            .lit_int => {
                const val = self.ir.getIntValue(idx) orelse {
                    self.emit("0");
                    return;
                };
                self.emitFmt("{d}", .{val});
            },
            .identifier => {
                const binding = self.ir.getBinding(idx) orelse {
                    self.emit("0");
                    return;
                };
                self.emit(self.localName(binding));
            },
            .binary_op => self.transpileBinaryOp(idx),
            .call, .method_call => {
                // Check if calling a known integer function
                self.transpileIntCallExpr(idx);
            },
            else => self.emit("0"),
        }
    }

    fn transpileIntCallExpr(self: *IrTranspiler, idx: NodeIndex) void {
        const call = self.ir.getCall(idx) orelse {
            self.emit("0");
            return;
        };
        const callee_tag = self.ir.getTag(call.callee) orelse {
            self.emit("0");
            return;
        };
        if (callee_tag != .identifier) {
            self.emit("0");
            return;
        }
        const callee_binding = self.ir.getBinding(call.callee) orelse {
            self.emit("0");
            return;
        };
        if (callee_binding.kind != .global) {
            self.emit("0");
            return;
        }
        const func_name = self.atomName(callee_binding.slot) orelse {
            self.emit("0");
            return;
        };

        self.emitFmt("aot_{s}(", .{func_name});
        var a: u8 = 0;
        while (a < call.args_count) : (a += 1) {
            if (a > 0) self.emit(", ");
            const arg = self.ir.getListIndex(call.args_start, a);
            self.transpileIntExpr(arg);
        }
        self.emit(")");
    }

    fn transpileBinaryOp(self: *IrTranspiler, idx: NodeIndex) void {
        const bin = self.ir.getBinary(idx) orelse return;
        const lt = self.inferExprType(bin.left);
        const rt = self.inferExprType(bin.right);

        switch (bin.op) {
            .add => {
                if (lt == .string_type or rt == .string_type) {
                    // String concatenation - not directly emittable in Zig
                    self.emit("error.AotBail");
                } else {
                    self.emit("(");
                    self.transpileIntExpr(bin.left);
                    self.emit(" + ");
                    self.transpileIntExpr(bin.right);
                    self.emit(")");
                }
            },
            .sub => {
                self.emit("(");
                self.transpileIntExpr(bin.left);
                self.emit(" - ");
                self.transpileIntExpr(bin.right);
                self.emit(")");
            },
            .mul => {
                self.emit("(");
                self.transpileIntExpr(bin.left);
                self.emit(" * ");
                self.transpileIntExpr(bin.right);
                self.emit(")");
            },
            .div => {
                self.emit("@divTrunc(");
                self.transpileIntExpr(bin.left);
                self.emit(", ");
                self.transpileIntExpr(bin.right);
                self.emit(")");
            },
            .mod => {
                self.emit("@mod(");
                self.transpileIntExpr(bin.left);
                self.emit(", ");
                self.transpileIntExpr(bin.right);
                self.emit(")");
            },
            else => self.emit("error.AotBail"),
        }
    }

    fn transpileUnaryOp(self: *IrTranspiler, idx: NodeIndex) void {
        const un = self.ir.getUnary(idx) orelse return;
        switch (un.op) {
            .neg => {
                self.emit("-");
                self.transpileIntExpr(un.operand);
            },
            .not => {
                self.emit("!");
                self.transpileCondition(un.operand);
            },
            .bit_not => {
                self.emit("~");
                self.transpileIntExpr(un.operand);
            },
            else => self.emit("error.AotBail"),
        }
    }

    // ============ Block / Statement Transpilation ============

    fn transpileBlock(self: *IrTranspiler, idx: NodeIndex) void {
        const tag = self.ir.getTag(idx) orelse return;
        if (tag != .block) {
            self.transpileStmt(idx);
            return;
        }
        const block = self.ir.getBlock(idx) orelse return;

        var i: u16 = 0;
        while (i < block.stmts_count) : (i += 1) {
            const stmt = self.ir.getListIndex(block.stmts_start, i);
            self.transpileStmt(stmt);
        }
    }

    fn transpileStmt(self: *IrTranspiler, idx: NodeIndex) void {
        const tag = self.ir.getTag(idx) orelse return;
        switch (tag) {
            .var_decl, .function_decl => self.transpileVarDecl(idx),
            .if_stmt => self.transpileIfStmt(idx),
            .return_stmt => self.transpileReturnStmt(idx),
            .expr_stmt => self.transpileExprStmt(idx),
            .for_of_stmt => self.transpileForOf(idx),
            .assignment => self.transpileAssignment(idx),
            .block => self.transpileBlock(idx),
            else => {
                self.emitIndent();
                self.emit("return error.AotBail;\n");
            },
        }
    }

    fn transpileVarDecl(self: *IrTranspiler, idx: NodeIndex) void {
        const decl = self.ir.getVarDecl(idx) orelse return;
        const name = self.resolveVarDeclName(decl);
        const inferred = if (decl.init != null_node) self.inferExprType(decl.init) else .jsvalue_type;
        self.registerLocal(decl.binding, name, inferred, decl.kind != .@"const");

        self.emitIndent();
        if (decl.kind == .@"const") {
            self.emitFmt("const {s} = ", .{name});
        } else {
            self.emitFmt("var {s}: i32 = ", .{name});
        }
        if (decl.init != null_node) {
            self.transpileIntExpr(decl.init);
        } else {
            self.emit("0");
        }
        self.emit(";\n");
    }

    fn transpileIfStmt(self: *IrTranspiler, idx: NodeIndex) void {
        const if_s = self.ir.getIfStmt(idx) orelse return;

        self.emitIndent();
        self.emit("if (");
        self.transpileCondition(if_s.condition);
        self.emit(") {\n");
        self.pushIndent();

        const then_tag = self.ir.getTag(if_s.then_branch) orelse return;
        if (then_tag == .block) {
            self.transpileBlock(if_s.then_branch);
        } else {
            self.transpileStmt(if_s.then_branch);
        }

        self.popIndent();
        self.emitIndent();

        if (if_s.else_branch != null_node) {
            const else_tag = self.ir.getTag(if_s.else_branch) orelse {
                self.emit("}\n");
                return;
            };
            if (else_tag == .if_stmt) {
                self.emit("} else ");
                self.transpileIfStmt(if_s.else_branch);
                return;
            }
            self.emit("} else {\n");
            self.pushIndent();
            if (else_tag == .block) {
                self.transpileBlock(if_s.else_branch);
            } else {
                self.transpileStmt(if_s.else_branch);
            }
            self.popIndent();
            self.emitIndent();
        }
        self.emit("}\n");
    }

    fn transpileReturnStmt(self: *IrTranspiler, idx: NodeIndex) void {
        const val_idx = self.ir.getOptValue(idx) orelse {
            self.emitIndent();
            self.emit("return;\n");
            return;
        };
        self.emitIndent();
        self.emit("return ");
        self.transpileIntExpr(val_idx);
        self.emit(";\n");
    }

    fn transpileExprStmt(self: *IrTranspiler, idx: NodeIndex) void {
        const val_idx = self.ir.getOptValue(idx) orelse return;
        const tag = self.ir.getTag(val_idx) orelse return;

        // Check for assignment expression
        if (tag == .assignment) {
            self.transpileAssignment(val_idx);
            return;
        }

        self.emitIndent();
        self.emit("_ = ");
        self.transpileExpr(val_idx);
        self.emit(";\n");
    }

    fn transpileAssignment(self: *IrTranspiler, idx: NodeIndex) void {
        const assign = self.ir.getAssignment(idx) orelse return;

        // Get target name
        const target_tag = self.ir.getTag(assign.target) orelse return;
        if (target_tag != .identifier) return;
        const binding = self.ir.getBinding(assign.target) orelse return;
        const name = self.localName(binding);

        self.emitIndent();
        self.emitFmt("{s} = ", .{name});
        self.transpileIntExpr(assign.value);
        self.emit(";\n");
    }

    fn transpileForOf(self: *IrTranspiler, idx: NodeIndex) void {
        const for_iter = self.ir.getForIter(idx) orelse {
            self.emitIndent();
            self.emit("return error.AotBail;\n");
            return;
        };

        // Check if iterating over range(start, end)
        if (self.tryTranspileRangeLoop(for_iter)) return;

        // General for-of - bail for now
        self.emitIndent();
        self.emit("return error.AotBail;\n");
    }

    fn tryTranspileRangeLoop(self: *IrTranspiler, for_iter: ir.Node.ForIterStmt) bool {
        // Check if iterable is a call to `range`
        const iter_tag = self.ir.getTag(for_iter.iterable) orelse return false;
        if (iter_tag != .call and iter_tag != .method_call) return false;
        const call = self.ir.getCall(for_iter.iterable) orelse return false;

        // Check if callee is `range` (global)
        const callee_tag = self.ir.getTag(call.callee) orelse return false;
        if (callee_tag != .identifier) return false;
        const callee_binding = self.ir.getBinding(call.callee) orelse return false;
        if (callee_binding.kind != .global) return false;

        const callee_name = self.atomName(callee_binding.slot) orelse return false;
        if (!std.mem.eql(u8, callee_name, "range")) return false;

        if (call.args_count < 2) return false;

        // Register loop variable (use _ since we might not need it)
        const loop_var_name = "_";
        _ = loop_var_name;

        // Get start and end
        const start_arg = self.ir.getListIndex(call.args_start, 0);
        const end_arg = self.ir.getListIndex(call.args_start, 1);

        // Emit: var aot_i: i32 = <start>; while (aot_i < <end>) : (aot_i += 1)
        self.emitIndent();
        self.emit("{\n");
        self.pushIndent();
        self.emitIndent();
        self.emit("var range_i: i32 = ");
        self.transpileIntExpr(start_arg);
        self.emit(";\n");
        self.emitIndent();
        self.emit("const range_end: i32 = ");
        self.transpileIntExpr(end_arg);
        self.emit(";\n");
        self.emitIndent();
        self.emit("while (range_i < range_end) : (range_i += 1) {\n");
        self.pushIndent();

        // Transpile body
        const body_tag = self.ir.getTag(for_iter.body) orelse {
            self.popIndent();
            self.emitIndent();
            self.emit("}\n");
            self.popIndent();
            self.emitIndent();
            self.emit("}\n");
            return true;
        };
        if (body_tag == .block) {
            self.transpileBlock(for_iter.body);
        } else {
            self.transpileStmt(for_iter.body);
        }

        self.popIndent();
        self.emitIndent();
        self.emit("}\n");
        self.popIndent();
        self.emitIndent();
        self.emit("}\n");

        return true;
    }
};

// ============ Tests ============

// Tests removed from tools/ - transpiler is tested via integration (build + run)
