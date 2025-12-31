//! JavaScript/JSX Parser
//!
//! Pratt parser producing IR nodes with integrated scope analysis.
//! Supports ES5 + limited ES6 (arrow functions, template literals, classes).

const std = @import("std");
const tokenizer_mod = @import("tokenizer.zig");
const ir = @import("ir.zig");
const scope_mod = @import("scope.zig");
const error_mod = @import("error.zig");
const token_mod = @import("token.zig");
const object = @import("../object.zig");
const context = @import("../context.zig");

const Tokenizer = tokenizer_mod.Tokenizer;
const TokenizerState = tokenizer_mod.TokenizerState;
const Token = token_mod.Token;
const TokenType = token_mod.TokenType;
const SourceLocation = token_mod.SourceLocation;

const Node = ir.Node;
const NodeTag = ir.NodeTag;
const NodeIndex = ir.NodeIndex;
const NodeList = ir.NodeList;
const ConstantPool = ir.ConstantPool;
const BindingRef = ir.BindingRef;
const BinaryOp = ir.BinaryOp;
const UnaryOp = ir.UnaryOp;
const FunctionFlags = ir.FunctionFlags;
const null_node = ir.null_node;

const ScopeAnalyzer = scope_mod.ScopeAnalyzer;
const ScopeKind = scope_mod.ScopeKind;
const Binding = scope_mod.Binding;

const ErrorList = error_mod.ErrorList;
const ErrorBuilder = error_mod.ErrorBuilder;

/// Parser errors
pub const ParseErr = error{
    UnexpectedToken,
    TooManyLocals,
    OutOfMemory,
};

/// Operator precedence levels (higher = binds tighter)
const Precedence = enum(u8) {
    none = 0,
    comma = 1, // ,
    assignment = 2, // = += -= etc
    ternary = 3, // ?:
    nullish = 4, // ??
    or_op = 5, // ||
    and_op = 6, // &&
    bit_or = 7, // |
    bit_xor = 8, // ^
    bit_and = 9, // &
    equality = 10, // == != === !==
    comparison = 11, // < > <= >= in instanceof
    shift = 12, // << >> >>>
    additive = 13, // + -
    multiplicative = 14, // * / %
    exponent = 15, // **
    unary = 16, // ! ~ - + typeof void delete
    postfix = 17, // ++ --
    call = 18, // () [] .
    primary = 19,
};

/// Parser state
pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokenizer: Tokenizer,
    source: []const u8,

    // Output
    nodes: NodeList,
    constants: ConstantPool,
    scopes: ScopeAnalyzer,
    errors: ErrorList,

    // Current state
    current: Token,
    previous: Token,
    had_error: bool,

    // Optional atom table for interning identifiers/properties
    atoms: ?*context.AtomTable,

    // Context flags
    in_loop: bool,
    in_switch: bool,
    in_function: bool,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Parser {
        var parser = Parser{
            .allocator = allocator,
            .tokenizer = Tokenizer.init(source),
            .source = source,
            .nodes = NodeList.init(allocator),
            .constants = ConstantPool.init(allocator),
            .scopes = ScopeAnalyzer.init(allocator),
            .errors = ErrorList.init(allocator, source),
            .current = undefined,
            .previous = undefined,
            .had_error = false,
            .atoms = null,
            .in_loop = false,
            .in_switch = false,
            .in_function = false,
        };
        // Prime the parser with first token
        parser.advance();
        return parser;
    }

    pub fn deinit(self: *Parser) void {
        self.nodes.deinit();
        self.constants.deinit();
        self.scopes.deinit();
        self.errors.deinit();
    }

    pub fn setAtomTable(self: *Parser, atoms: *context.AtomTable) void {
        self.atoms = atoms;
    }

    /// Parse the entire program
    pub fn parse(self: *Parser) anyerror!NodeIndex {
        var stmts: std.ArrayList(NodeIndex) = .empty;
        defer stmts.deinit(self.allocator);

        while (!self.check(.eof)) {
            if (self.parseStatement()) |stmt| {
                try stmts.append(self.allocator, stmt);
            } else |err| {
                // If there are recorded errors, fail immediately
                if (self.errors.hasErrors()) {
                    return err;
                }
                // Otherwise try to recover for better error messages
                self.synchronize();
            }
        }

        // If any errors were recorded during parsing, fail
        if (self.errors.hasErrors()) {
            return error.ParseError;
        }

        // Create program node
        const stmts_start = try self.addStmtList(stmts.items);
        return try self.nodes.add(.{
            .tag = .program,
            .loc = .{ .line = 1, .column = 1, .offset = 0 },
            .data = .{ .block = .{
                .stmts_start = stmts_start,
                .stmts_count = @intCast(stmts.items.len),
                .scope_id = 0,
            } },
        });
    }

    // ============ Statement Parsing ============

    fn parseStatement(self: *Parser) anyerror!NodeIndex {
        return switch (self.current.type) {
            .kw_var, .kw_let, .kw_const => self.parseVarDeclaration(),
            .kw_function => self.parseFunctionDeclaration(),
            .kw_if => self.parseIfStatement(),
            .kw_while => self.parseWhileStatement(),
            .kw_do => self.parseDoWhileStatement(),
            .kw_for => self.parseForStatement(),
            .kw_return => self.parseReturnStatement(),
            .kw_break => self.parseBreakStatement(),
            .kw_continue => self.parseContinueStatement(),
            .kw_throw => self.parseThrowStatement(),
            .kw_try => self.parseTryStatement(),
            .kw_switch => self.parseSwitchStatement(),
            .kw_class => self.parseClassDeclaration(),
            .lbrace => self.parseBlock(),
            .semicolon => self.parseEmptyStatement(),
            .kw_debugger => self.parseDebuggerStatement(),
            else => self.parseExpressionStatement(),
        };
    }

    fn parseVarDeclaration(self: *Parser) anyerror!NodeIndex {
        // Reject 'var' - only 'let' and 'const' are supported
        if (self.current.type == .kw_var) {
            self.errors.addErrorAt(.unsupported_feature, self.current, "'var' is not supported; use 'let' or 'const' instead");
            self.had_error = true;
            return error.ParseError;
        }

        const kind: Node.VarDecl.VarKind = switch (self.current.type) {
            .kw_let => .let,
            .kw_const => .@"const",
            else => unreachable,
        };
        const loc = self.current.location();
        self.advance();

        // Check for destructuring pattern
        if (self.check(.lbrace)) {
            return self.parseDestructuringDecl(loc, kind, .object);
        } else if (self.check(.lbracket)) {
            return self.parseDestructuringDecl(loc, kind, .array);
        }

        // Simple identifier binding
        const name = try self.expectIdentifier("variable name");
        const name_atom = try self.addAtom(name.text(self.source));

        // Declare in scope
        const binding = self.scopes.declareBinding(
            name.text(self.source),
            name_atom,
            .variable,
            kind == .@"const",
        ) catch {
            self.errorAtCurrent("too many local variables");
            return error.TooManyLocals;
        };

        // Parse initializer
        var init_node: NodeIndex = null_node;
        if (self.match(.assign)) {
            init_node = try self.parseExpression(.assignment);
        } else if (kind == .@"const") {
            self.errorAtCurrent("const declarations must have an initializer");
        }

        try self.expectSemicolon();

        return try self.nodes.add(.{
            .tag = .var_decl,
            .loc = loc,
            .data = .{ .var_decl = .{
                .binding = binding,
                .pattern = null_node,
                .init = init_node,
                .kind = kind,
            } },
        });
    }

    const PatternKind = enum { object, array };

    fn parseDestructuringDecl(self: *Parser, loc: SourceLocation, kind: Node.VarDecl.VarKind, pattern_kind: PatternKind) anyerror!NodeIndex {
        // Parse the pattern
        const pattern = switch (pattern_kind) {
            .object => try self.parseObjectPattern(),
            .array => try self.parseArrayPattern(),
        };

        // Require initializer for destructuring
        try self.expect(.assign, "'=' after destructuring pattern");
        const init_node = try self.parseExpression(.assignment);

        try self.expectSemicolon();

        // Create a special binding for destructuring (slot 255 = pattern)
        const dummy_binding = BindingRef{
            .scope_id = 0,
            .slot = 255,
            .kind = .local,
        };

        return try self.nodes.add(.{
            .tag = .var_decl,
            .loc = loc,
            .data = .{ .var_decl = .{
                .binding = dummy_binding,
                .pattern = pattern,
                .init = init_node,
                .kind = kind,
            } },
        });
    }

    fn parseObjectPattern(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume '{'

        var elements = std.ArrayList(NodeIndex).empty;
        defer elements.deinit(self.allocator);

        while (!self.check(.rbrace) and !self.check(.eof)) {
            const elem = try self.parseObjectPatternElement();
            try elements.append(self.allocator, elem);

            if (!self.match(.comma)) break;
        }

        try self.expect(.rbrace, "'}' to close object pattern");

        const elements_start = try self.addNodeList(elements.items);

        return try self.nodes.add(.{
            .tag = .object_pattern,
            .loc = loc,
            .data = .{ .array = .{
                .elements_start = elements_start,
                .elements_count = @intCast(elements.items.len),
                .has_spread = false,
            } },
        });
    }

    fn parseObjectPatternElement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();

        // Check for rest element: ...rest
        if (self.match(.spread)) {
            const name = try self.expectIdentifier("identifier after '...'");
            const name_atom = try self.addAtom(name.text(self.source));

            const binding = self.scopes.declareBinding(
                name.text(self.source),
                name_atom,
                .variable,
                false,
            ) catch {
                self.errorAtCurrent("too many local variables");
                return error.TooManyLocals;
            };

            return try self.nodes.add(.{
                .tag = .pattern_rest,
                .loc = loc,
                .data = .{ .pattern_elem = .{
                    .kind = .rest,
                    .binding = binding,
                    .key = null_node,
                    .key_atom = 0,
                    .default_value = null_node,
                } },
            });
        }

        // Property name (could be renamed: { name: localName })
        const key_name = try self.expectIdentifier("property name in object pattern");
        // Use addString for consistency with object literals (which also use string constants for keys)
        const key_str_idx = try self.addString(key_name.text(self.source));
        const key_atom_for_binding = try self.addAtom(key_name.text(self.source));

        var local_name = key_name;
        var local_atom = key_atom_for_binding;

        // Check for rename: { name: localName }
        if (self.match(.colon)) {
            // Check for nested pattern
            if (self.check(.lbrace)) {
                const nested = try self.parseObjectPattern();

                return try self.nodes.add(.{
                    .tag = .pattern_element,
                    .loc = loc,
                    .data = .{ .pattern_elem = .{
                        .kind = .object,
                        .binding = .{ .scope_id = 0, .slot = 255, .kind = .local },
                        .key = nested, // Nested pattern
                        .key_atom = key_str_idx, // Use string constant index for get_field
                        .default_value = null_node,
                    } },
                });
            } else if (self.check(.lbracket)) {
                const nested = try self.parseArrayPattern();

                return try self.nodes.add(.{
                    .tag = .pattern_element,
                    .loc = loc,
                    .data = .{ .pattern_elem = .{
                        .kind = .array,
                        .binding = .{ .scope_id = 0, .slot = 255, .kind = .local },
                        .key = nested, // Nested pattern
                        .key_atom = key_str_idx, // Use string constant index for get_field
                        .default_value = null_node,
                    } },
                });
            }

            // Simple rename
            local_name = try self.expectIdentifier("local name after ':'");
            local_atom = try self.addAtom(local_name.text(self.source));
        }

        // Declare the local binding
        const binding = self.scopes.declareBinding(
            local_name.text(self.source),
            local_atom,
            .variable,
            false,
        ) catch {
            self.errorAtCurrent("too many local variables");
            return error.TooManyLocals;
        };

        // Check for default value: { x = 10 }
        var default_value: NodeIndex = null_node;
        if (self.match(.assign)) {
            default_value = try self.parseExpression(.assignment);
        }

        return try self.nodes.add(.{
            .tag = .pattern_element,
            .loc = loc,
            .data = .{ .pattern_elem = .{
                .kind = .simple,
                .binding = binding,
                .key = null_node,
                .key_atom = key_str_idx, // Use string constant index for get_field
                .default_value = default_value,
            } },
        });
    }

    fn parseArrayPattern(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume '['

        var elements = std.ArrayList(NodeIndex).empty;
        defer elements.deinit(self.allocator);

        while (!self.check(.rbracket) and !self.check(.eof)) {
            // Handle holes: [a, , b]
            if (self.check(.comma)) {
                // Hole - push null_node
                try elements.append(self.allocator, null_node);
                self.advance();
                continue;
            }

            const elem = try self.parseArrayPatternElement();
            try elements.append(self.allocator, elem);

            if (!self.match(.comma)) break;
        }

        try self.expect(.rbracket, "']' to close array pattern");

        const elements_start = try self.addNodeList(elements.items);

        return try self.nodes.add(.{
            .tag = .array_pattern,
            .loc = loc,
            .data = .{ .array = .{
                .elements_start = elements_start,
                .elements_count = @intCast(elements.items.len),
                .has_spread = false,
            } },
        });
    }

    fn parseArrayPatternElement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();

        // Check for rest element: ...rest
        if (self.match(.spread)) {
            const name = try self.expectIdentifier("identifier after '...'");
            const name_atom = try self.addAtom(name.text(self.source));

            const binding = self.scopes.declareBinding(
                name.text(self.source),
                name_atom,
                .variable,
                false,
            ) catch {
                self.errorAtCurrent("too many local variables");
                return error.TooManyLocals;
            };

            return try self.nodes.add(.{
                .tag = .pattern_rest,
                .loc = loc,
                .data = .{ .pattern_elem = .{
                    .kind = .rest,
                    .binding = binding,
                    .key = null_node,
                    .key_atom = 0,
                    .default_value = null_node,
                } },
            });
        }

        // Check for nested patterns
        if (self.check(.lbrace)) {
            const nested = try self.parseObjectPattern();
            return try self.nodes.add(.{
                .tag = .pattern_element,
                .loc = loc,
                .data = .{ .pattern_elem = .{
                    .kind = .object,
                    .binding = .{ .scope_id = 0, .slot = 255, .kind = .local },
                    .key = nested,
                    .key_atom = 0,
                    .default_value = null_node,
                } },
            });
        } else if (self.check(.lbracket)) {
            const nested = try self.parseArrayPattern();
            return try self.nodes.add(.{
                .tag = .pattern_element,
                .loc = loc,
                .data = .{ .pattern_elem = .{
                    .kind = .array,
                    .binding = .{ .scope_id = 0, .slot = 255, .kind = .local },
                    .key = nested,
                    .key_atom = 0,
                    .default_value = null_node,
                } },
            });
        }

        // Simple identifier
        const name = try self.expectIdentifier("identifier in array pattern");
        const name_atom = try self.addAtom(name.text(self.source));

        const binding = self.scopes.declareBinding(
            name.text(self.source),
            name_atom,
            .variable,
            false,
        ) catch {
            self.errorAtCurrent("too many local variables");
            return error.TooManyLocals;
        };

        // Check for default value: [x = 10]
        var default_value: NodeIndex = null_node;
        if (self.match(.assign)) {
            default_value = try self.parseExpression(.assignment);
        }

        return try self.nodes.add(.{
            .tag = .pattern_element,
            .loc = loc,
            .data = .{ .pattern_elem = .{
                .kind = .simple,
                .binding = binding,
                .key = null_node,
                .key_atom = 0,
                .default_value = default_value,
            } },
        });
    }

    fn parseFunctionDeclaration(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'function'

        var flags = FunctionFlags{};

        // Check for generator
        if (self.match(.star)) {
            flags.is_generator = true;
        }

        // Function name
        const name = try self.expectIdentifier("function name");
        const name_atom = try self.addAtom(name.text(self.source));

        // Declare function in current scope (hoisted)
        const binding = self.scopes.declareBinding(
            name.text(self.source),
            name_atom,
            .function,
            false,
        ) catch {
            self.errorAtCurrent("too many local variables");
            return error.TooManyLocals;
        };

        // Parse function body
        const func_node = try self.parseFunctionBody(name_atom, flags);

        // Create function declaration node
        // Note: function declarations use .let for binding (no var support)
        return try self.nodes.add(.{
            .tag = .function_decl,
            .loc = loc,
            .data = .{ .var_decl = .{
                .binding = binding,
                .pattern = null_node,
                .init = func_node,
                .kind = .let,
            } },
        });
    }

    fn parseFunctionBody(self: *Parser, name_atom: u16, flags: FunctionFlags) anyerror!NodeIndex {
        const loc = self.current.location();

        // Enter function scope
        const scope_id = try self.scopes.pushScope(.function);
        const was_in_function = self.in_function;
        self.in_function = true;

        // Parse parameters
        try self.expect(.lparen, "'('");
        var params = std.ArrayList(NodeIndex).empty;
        defer params.deinit(self.allocator);

        var param_flags = flags;

        if (!self.check(.rparen)) {
            while (true) {
                // Check for rest parameter
                if (self.match(.spread)) {
                    param_flags.has_rest_param = true;
                }

                const param_name = try self.expectIdentifier("parameter name");
                const param_atom = try self.addAtom(param_name.text(self.source));

                const param_binding = self.scopes.declareBinding(
                    param_name.text(self.source),
                    param_atom,
                    .parameter,
                    false,
                ) catch {
                    self.errorAtCurrent("too many parameters");
                    return error.TooManyLocals;
                };

                // Check for default value
                var default_value: NodeIndex = null_node;
                if (self.match(.assign)) {
                    param_flags.has_default_params = true;
                    default_value = try self.parseExpression(.assignment);
                }

                const param_node = try self.nodes.add(.{
                    .tag = .pattern_element,
                    .loc = param_name.location(),
                    .data = .{ .pattern_elem = .{
                        .kind = if (param_flags.has_rest_param) .rest else .simple,
                        .binding = param_binding,
                        .key = null_node,
                        .key_atom = 0,
                        .default_value = default_value,
                    } },
                });
                try params.append(self.allocator, param_node);

                if (param_flags.has_rest_param) break; // Rest must be last
                if (!self.match(.comma)) break;
                if (self.check(.rparen)) break; // Trailing comma
            }
        }
        try self.expect(.rparen, "')'");

        // Parse body
        const body = try self.parseBlock();

        self.in_function = was_in_function;
        self.scopes.popScope();

        // Create function expression node
        const params_start = if (params.items.len > 0)
            try self.addNodeList(params.items)
        else
            null_node;

        return try self.nodes.add(.{
            .tag = .function_expr,
            .loc = loc,
            .data = .{ .function = .{
                .scope_id = scope_id,
                .name_atom = name_atom,
                .params_start = params_start,
                .params_count = @intCast(params.items.len),
                .body = body,
                .flags = param_flags,
            } },
        });
    }

    fn parseIfStatement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'if'

        try self.expect(.lparen, "'('");
        const condition = try self.parseExpression(.none);
        try self.expect(.rparen, "')'");

        const then_branch = try self.parseStatement();

        var else_branch: NodeIndex = null_node;
        if (self.match(.kw_else)) {
            else_branch = try self.parseStatement();
        }

        return try self.nodes.add(.{
            .tag = .if_stmt,
            .loc = loc,
            .data = .{ .if_stmt = .{
                .condition = condition,
                .then_branch = then_branch,
                .else_branch = else_branch,
            } },
        });
    }

    fn parseWhileStatement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'while'

        try self.expect(.lparen, "'('");
        const condition = try self.parseExpression(.none);
        try self.expect(.rparen, "')'");

        const was_in_loop = self.in_loop;
        self.in_loop = true;
        const body = try self.parseStatement();
        self.in_loop = was_in_loop;

        return try self.nodes.add(.{
            .tag = .while_stmt,
            .loc = loc,
            .data = .{ .loop = .{
                .kind = .while_loop,
                .init = null_node,
                .condition = condition,
                .update = null_node,
                .body = body,
            } },
        });
    }

    fn parseDoWhileStatement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'do'

        const was_in_loop = self.in_loop;
        self.in_loop = true;
        const body = try self.parseStatement();
        self.in_loop = was_in_loop;

        try self.expect(.kw_while, "'while'");
        try self.expect(.lparen, "'('");
        const condition = try self.parseExpression(.none);
        try self.expect(.rparen, "')'");
        try self.expectSemicolon();

        return try self.nodes.add(.{
            .tag = .do_while_stmt,
            .loc = loc,
            .data = .{ .loop = .{
                .kind = .do_while,
                .init = null_node,
                .condition = condition,
                .update = null_node,
                .body = body,
            } },
        });
    }

    fn parseForStatement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'for'

        try self.expect(.lparen, "'('");

        // Enter loop scope for let/const declarations
        _ = try self.scopes.pushScope(.for_loop);

        // Parse initializer
        var init_node: NodeIndex = null_node;
        var is_for_in = false;
        var is_for_of = false;
        var is_const = false;

        if (!self.check(.semicolon)) {
            // Reject 'var' in for loops - only 'let' and 'const' are supported
            if (self.check(.kw_var)) {
                self.errors.addErrorAt(.unsupported_feature, self.current, "'var' is not supported; use 'let' or 'const' instead");
                self.had_error = true;
                return error.ParseError;
            }

            if (self.check(.kw_let) or self.check(.kw_const)) {
                is_const = self.current.type == .kw_const;
                const kind: Node.VarDecl.VarKind = switch (self.current.type) {
                    .kw_let => .let,
                    .kw_const => .@"const",
                    else => unreachable,
                };
                self.advance();

                const name = try self.expectIdentifier("variable name");
                const name_atom = try self.addAtom(name.text(self.source));
                const binding = self.scopes.declareBinding(
                    name.text(self.source),
                    name_atom,
                    .variable,
                    is_const,
                ) catch return error.TooManyLocals;

                // Check for for-in/for-of
                if (self.check(.kw_in)) {
                    is_for_in = true;
                    self.advance();
                    const iterable = try self.parseExpression(.none);
                    try self.expect(.rparen, "')'");

                    const was_in_loop = self.in_loop;
                    self.in_loop = true;
                    const body = try self.parseStatement();
                    self.in_loop = was_in_loop;

                    self.scopes.popScope();

                    return try self.nodes.add(.{
                        .tag = .for_in_stmt,
                        .loc = loc,
                        .data = .{ .for_iter = .{
                            .is_for_in = true,
                            .binding = binding,
                            .pattern = null_node,
                            .iterable = iterable,
                            .body = body,
                            .is_const = is_const,
                        } },
                    });
                } else if (self.check(.kw_of)) {
                    is_for_of = true;
                    self.advance();
                    const iterable = try self.parseExpression(.none);
                    try self.expect(.rparen, "')'");

                    const was_in_loop = self.in_loop;
                    self.in_loop = true;
                    const body = try self.parseStatement();
                    self.in_loop = was_in_loop;

                    self.scopes.popScope();

                    return try self.nodes.add(.{
                        .tag = .for_of_stmt,
                        .loc = loc,
                        .data = .{ .for_iter = .{
                            .is_for_in = false,
                            .binding = binding,
                            .pattern = null_node,
                            .iterable = iterable,
                            .body = body,
                            .is_const = is_const,
                        } },
                    });
                }

                // Regular for loop with var declaration
                var var_init: NodeIndex = null_node;
                if (self.match(.assign)) {
                    var_init = try self.parseExpression(.assignment);
                }

                init_node = try self.nodes.add(.{
                    .tag = .var_decl,
                    .loc = name.location(),
                    .data = .{ .var_decl = .{
                        .binding = binding,
                        .pattern = null_node,
                        .init = var_init,
                        .kind = kind,
                    } },
                });
            } else {
                init_node = try self.parseExpression(.none);
            }
        }

        if (!is_for_in and !is_for_of) {
            try self.expect(.semicolon, "';'");

            // Condition
            var condition: NodeIndex = null_node;
            if (!self.check(.semicolon)) {
                condition = try self.parseExpression(.none);
            }
            try self.expect(.semicolon, "';'");

            // Update
            var update: NodeIndex = null_node;
            if (!self.check(.rparen)) {
                update = try self.parseExpression(.none);
            }
            try self.expect(.rparen, "')'");

            const was_in_loop = self.in_loop;
            self.in_loop = true;
            const body = try self.parseStatement();
            self.in_loop = was_in_loop;

            self.scopes.popScope();

            return try self.nodes.add(.{
                .tag = .for_stmt,
                .loc = loc,
                .data = .{ .loop = .{
                    .kind = .for_loop,
                    .init = init_node,
                    .condition = condition,
                    .update = update,
                    .body = body,
                } },
            });
        }

        self.scopes.popScope();
        return error.UnexpectedToken;
    }

    fn parseReturnStatement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'return'

        if (!self.in_function) {
            self.errorAt(loc, "'return' outside of function");
        }

        var value: ?NodeIndex = null;
        if (!self.check(.semicolon) and !self.check(.rbrace) and !self.check(.eof)) {
            value = try self.parseExpression(.none);
        }
        try self.expectSemicolon();

        return try self.nodes.add(.{
            .tag = .return_stmt,
            .loc = loc,
            .data = .{ .opt_value = value },
        });
    }

    fn parseBreakStatement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'break'

        if (!self.in_loop and !self.in_switch) {
            self.errorAt(loc, "'break' outside of loop or switch");
        }

        // Optional label
        var label: ?u16 = null;
        if (self.check(.identifier)) {
            label = try self.addAtom(self.current.text(self.source));
            self.advance();
        }
        try self.expectSemicolon();

        return try self.nodes.add(.{
            .tag = .break_stmt,
            .loc = loc,
            .data = .{ .opt_label = label },
        });
    }

    fn parseContinueStatement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'continue'

        if (!self.in_loop) {
            self.errorAt(loc, "'continue' outside of loop");
        }

        var label: ?u16 = null;
        if (self.check(.identifier)) {
            label = try self.addAtom(self.current.text(self.source));
            self.advance();
        }
        try self.expectSemicolon();

        return try self.nodes.add(.{
            .tag = .continue_stmt,
            .loc = loc,
            .data = .{ .opt_label = label },
        });
    }

    fn parseThrowStatement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'throw'

        const value = try self.parseExpression(.none);
        try self.expectSemicolon();

        return try self.nodes.add(.{
            .tag = .throw_stmt,
            .loc = loc,
            .data = .{ .opt_value = value },
        });
    }

    fn parseTryStatement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'try'

        const try_block = try self.parseBlock();

        var catch_binding = BindingRef{ .scope_id = 0, .slot = 255, .kind = .local };
        var catch_block: NodeIndex = null_node;
        var finally_block: NodeIndex = null_node;

        if (self.match(.kw_catch)) {
            _ = try self.scopes.pushScope(.catch_block);

            if (self.match(.lparen)) {
                const catch_name = try self.expectIdentifier("catch parameter");
                const catch_atom = try self.addAtom(catch_name.text(self.source));
                catch_binding = self.scopes.declareBinding(
                    catch_name.text(self.source),
                    catch_atom,
                    .catch_param,
                    false,
                ) catch return error.TooManyLocals;
                try self.expect(.rparen, "')'");
            }

            catch_block = try self.parseBlock();
            self.scopes.popScope();
        }

        if (self.match(.kw_finally)) {
            finally_block = try self.parseBlock();
        }

        if (catch_block == null_node and finally_block == null_node) {
            self.errorAt(loc, "try statement requires catch or finally");
        }

        return try self.nodes.add(.{
            .tag = .try_stmt,
            .loc = loc,
            .data = .{ .try_stmt = .{
                .try_block = try_block,
                .catch_binding = catch_binding,
                .catch_block = catch_block,
                .finally_block = finally_block,
            } },
        });
    }

    fn parseSwitchStatement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'switch'

        try self.expect(.lparen, "'('");
        const discriminant = try self.parseExpression(.none);
        try self.expect(.rparen, "')'");
        try self.expect(.lbrace, "'{'");

        var cases = std.ArrayList(NodeIndex).empty;
        defer cases.deinit(self.allocator);

        const was_in_switch = self.in_switch;
        self.in_switch = true;

        while (!self.check(.rbrace) and !self.check(.eof)) {
            const case_loc = self.current.location();
            var test_expr: NodeIndex = null_node;

            if (self.match(.kw_case)) {
                test_expr = try self.parseExpression(.none);
            } else if (self.match(.kw_default)) {
                // default case
            } else {
                self.errorAtCurrent("expected 'case' or 'default'");
                break;
            }
            try self.expect(.colon, "':'");

            // Parse case body statements
            var body_stmts = std.ArrayList(NodeIndex).empty;
            defer body_stmts.deinit(self.allocator);

            while (!self.check(.kw_case) and !self.check(.kw_default) and
                !self.check(.rbrace) and !self.check(.eof))
            {
                if (self.parseStatement()) |stmt| {
                    try body_stmts.append(self.allocator, stmt);
                } else |_| {
                    self.synchronize();
                }
            }

            const body_start = if (body_stmts.items.len > 0)
                try self.addStmtList(body_stmts.items)
            else
                null_node;

            const case_node = try self.nodes.add(.{
                .tag = .case_clause,
                .loc = case_loc,
                .data = .{ .case_clause = .{
                    .test_expr = test_expr,
                    .body_start = body_start,
                    .body_count = @intCast(body_stmts.items.len),
                } },
            });
            try cases.append(self.allocator, case_node);
        }

        self.in_switch = was_in_switch;
        try self.expect(.rbrace, "'}'");

        const cases_start = if (cases.items.len > 0)
            try self.addNodeList(cases.items)
        else
            null_node;

        return try self.nodes.add(.{
            .tag = .switch_stmt,
            .loc = loc,
            .data = .{ .switch_stmt = .{
                .discriminant = discriminant,
                .cases_start = cases_start,
                .cases_count = @intCast(cases.items.len),
            } },
        });
    }

    fn parseClassDeclaration(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'class'

        const name = try self.expectIdentifier("class name");
        const name_atom = try self.addAtom(name.text(self.source));

        // Declare class in current scope
        const binding = self.scopes.declareBinding(
            name.text(self.source),
            name_atom,
            .class,
            true,
        ) catch return error.TooManyLocals;

        // Check for extends
        var super_class: NodeIndex = null_node;
        if (self.match(.kw_extends)) {
            super_class = try self.parseExpression(.none);
        }

        // Enter class scope
        const scope_id = try self.scopes.pushScope(.class_body);

        try self.expect(.lbrace, "'{'");

        // Parse class body
        var members = std.ArrayList(NodeIndex).empty;
        defer members.deinit(self.allocator);

        while (!self.check(.rbrace) and !self.check(.eof)) {
            var is_static = false;
            if (self.match(.kw_static)) {
                is_static = true;
            }

            const member_loc = self.current.location();
            var member_kind: Node.ClassMember.MemberKind = .method;

            // Check for getter/setter
            if (self.check(.kw_get) and self.peekIsPropertyName()) {
                self.advance();
                member_kind = .getter;
            } else if (self.check(.kw_set) and self.peekIsPropertyName()) {
                self.advance();
                member_kind = .setter;
            }

            // Property name
            var key_node: NodeIndex = undefined;
            var is_computed = false;

            if (self.match(.lbracket)) {
                is_computed = true;
                key_node = try self.parseExpression(.none);
                try self.expect(.rbracket, "']'");
            } else if (self.check(.identifier) or self.check(.string_literal) or self.check(.number)) {
                const key_tok = self.current;
                self.advance();
                const key_idx = try self.addString(key_tok.text(self.source));
                key_node = try self.nodes.add(Node.litString(key_tok.location(), key_idx));
            } else {
                self.errorAtCurrent("expected property name");
                break;
            }

            // Method body
            var value: NodeIndex = undefined;
            if (self.check(.lparen)) {
                value = try self.parseFunctionBody(0, .{
                    .is_method = true,
                    .is_getter = member_kind == .getter,
                    .is_setter = member_kind == .setter,
                });
            } else if (self.match(.assign)) {
                member_kind = .field;
                value = try self.parseExpression(.assignment);
                try self.expectSemicolon();
            } else if (self.check(.semicolon) or self.check(.rbrace)) {
                member_kind = .field;
                value = null_node;
                _ = self.match(.semicolon);
            } else {
                self.errorAtCurrent("expected method body or field initializer");
                break;
            }

            const member_node = try self.nodes.add(.{
                .tag = .class_method,
                .loc = member_loc,
                .data = .{ .class_member = .{
                    .key = key_node,
                    .value = value,
                    .kind = member_kind,
                    .is_static = is_static,
                    .is_computed = is_computed,
                } },
            });
            try members.append(self.allocator, member_node);
        }

        try self.expect(.rbrace, "'}'");
        self.scopes.popScope();

        const members_start = if (members.items.len > 0)
            try self.addNodeList(members.items)
        else
            null_node;

        return try self.nodes.add(.{
            .tag = .class_decl,
            .loc = loc,
            .data = .{ .class_decl = .{
                .scope_id = scope_id,
                .name_atom = name_atom,
                .super_class = super_class,
                .members_start = members_start,
                .members_count = @intCast(members.items.len),
                .binding = binding,
            } },
        });
    }

    fn parseBlock(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        try self.expect(.lbrace, "'{'");

        const scope_id = try self.scopes.pushScope(.block);

        var stmts = std.ArrayList(NodeIndex).empty;
        defer stmts.deinit(self.allocator);

        while (!self.check(.rbrace) and !self.check(.eof)) {
            if (self.parseStatement()) |stmt| {
                try stmts.append(self.allocator, stmt);
            } else |_| {
                self.synchronize();
            }
        }

        try self.expect(.rbrace, "'}'");
        self.scopes.popScope();

        const stmts_start = if (stmts.items.len > 0)
            try self.addStmtList(stmts.items)
        else
            null_node;

        return try self.nodes.add(.{
            .tag = .block,
            .loc = loc,
            .data = .{ .block = .{
                .stmts_start = stmts_start,
                .stmts_count = @intCast(stmts.items.len),
                .scope_id = scope_id,
            } },
        });
    }

    fn parseEmptyStatement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume ';'
        return try self.nodes.add(.{
            .tag = .empty_stmt,
            .loc = loc,
            .data = .{ .none = {} },
        });
    }

    fn parseDebuggerStatement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'debugger'
        try self.expectSemicolon();
        return try self.nodes.add(.{
            .tag = .debugger_stmt,
            .loc = loc,
            .data = .{ .none = {} },
        });
    }

    fn parseExpressionStatement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        const expr = try self.parseExpression(.none);
        try self.expectSemicolon();
        return try self.nodes.add(.{
            .tag = .expr_stmt,
            .loc = loc,
            .data = .{ .opt_value = expr },
        });
    }

    // ============ Expression Parsing (Pratt) ============

    fn parseExpression(self: *Parser, min_prec: Precedence) anyerror!NodeIndex {
        // Check for arrow function
        if (self.isArrowFunction()) {
            return self.parseArrowFunction();
        }

        var left = try self.parsePrefixExpr();

        while (true) {
            const prec = self.getInfixPrecedence(self.current.type);
            if (@intFromEnum(prec) <= @intFromEnum(min_prec)) break;

            left = try self.parseInfixExpr(left, prec);
        }

        return left;
    }

    fn parsePrefixExpr(self: *Parser) anyerror!NodeIndex {
        return switch (self.current.type) {
            // Literals
            .number => self.parseNumber(),
            .string_literal => self.parseString(),
            .true_lit => self.parseBoolLiteral(true),
            .false_lit => self.parseBoolLiteral(false),
            .null_lit => self.parseNullLiteral(),
            .undefined_lit => self.parseUndefinedLiteral(),
            .regex_literal => self.parseRegexLiteral(),

            // Template literals
            .template_literal, .template_head => self.parseTemplateLiteral(),

            // Identifiers and keywords
            .identifier => self.parseIdentifier(),
            .kw_this => self.parseThis(),
            .kw_super => self.parseSuper(),
            .kw_new => self.parseNewExpression(),
            .kw_function => self.parseFunctionExpression(),
            .kw_class => self.parseClassExpression(),
            .kw_async => self.parseAsyncExpression(),
            .kw_yield => self.parseYieldExpression(),
            .kw_await => self.parseAwaitExpression(),
            .kw_typeof => self.parseUnaryKeyword(.typeof_op),
            .kw_void => self.parseUnaryKeyword(.void_op),
            .kw_delete => self.parseUnaryKeyword(.delete_op),

            // Grouping and array/object literals
            .lparen => self.parseGroupedOrArrowParams(),
            .lbracket => self.parseArrayLiteral(),
            .lbrace => self.parseObjectLiteral(),

            // Unary operators
            .bang => self.parseUnaryOp(.not),
            .tilde => self.parseUnaryOp(.bit_not),
            .plus => self.parseUnaryOp(.neg), // Actually +, but often optimized away
            .minus => self.parseUnaryOp(.neg),
            // Prefix increment/decrement - not supported
            .plus_plus => {
                self.errors.addErrorAt(.unsupported_feature, self.current, "'++x' is not supported; use 'x = x + 1' instead");
                self.had_error = true;
                return error.ParseError;
            },
            .minus_minus => {
                self.errors.addErrorAt(.unsupported_feature, self.current, "'--x' is not supported; use 'x = x - 1' instead");
                self.had_error = true;
                return error.ParseError;
            },

            // JSX
            .lt => if (self.tokenizer.jsx_mode) self.parseJsxElement() else error.UnexpectedToken,

            else => {
                self.errorAtCurrent("expected expression");
                return error.UnexpectedToken;
            },
        };
    }

    fn parseInfixExpr(self: *Parser, left: NodeIndex, prec: Precedence) anyerror!NodeIndex {
        const op_tok = self.current;
        const loc = op_tok.location();

        return switch (op_tok.type) {
            // Binary operators
            .plus,
            .minus,
            .star,
            .slash,
            .percent,
            .star_star,
            .eq,
            .ne,
            .eq_eq,
            .ne_ne,
            .lt,
            .le,
            .gt,
            .ge,
            .ampersand,
            .pipe,
            .caret,
            .lt_lt,
            .gt_gt,
            .gt_gt_gt,
            .ampersand_ampersand,
            .pipe_pipe,
            .question_question,
            .kw_in,
            .kw_instanceof,
            => {
                self.advance();
                const right_prec = if (op_tok.type == .star_star)
                    @as(Precedence, @enumFromInt(@intFromEnum(prec) - 1)) // Right associative
                else
                    prec;
                const right = try self.parseExpression(right_prec);
                return try self.nodes.add(Node.binaryOp(loc, self.tokenToBinaryOp(op_tok.type), left, right));
            },

            // Assignment
            .assign,
            .plus_assign,
            .minus_assign,
            .star_assign,
            .slash_assign,
            .percent_assign,
            .ampersand_assign,
            .pipe_assign,
            .caret_assign,
            .lt_lt_assign,
            .gt_gt_assign,
            .gt_gt_gt_assign,
            .star_star_assign,
            .ampersand_ampersand_assign,
            .pipe_pipe_assign,
            .question_question_assign,
            => {
                self.advance();
                const right = try self.parseExpression(@enumFromInt(@intFromEnum(prec) - 1));
                const op: ?BinaryOp = switch (op_tok.type) {
                    .assign => null,
                    .plus_assign => .add,
                    .minus_assign => .sub,
                    .star_assign => .mul,
                    .slash_assign => .div,
                    .percent_assign => .mod,
                    .ampersand_assign => .bit_and,
                    .pipe_assign => .bit_or,
                    .caret_assign => .bit_xor,
                    .lt_lt_assign => .shl,
                    .gt_gt_assign => .shr,
                    .gt_gt_gt_assign => .ushr,
                    .star_star_assign => .pow,
                    .ampersand_ampersand_assign => .and_op,
                    .pipe_pipe_assign => .or_op,
                    .question_question_assign => .nullish,
                    else => unreachable,
                };
                return try self.nodes.add(.{
                    .tag = .assignment,
                    .loc = loc,
                    .data = .{ .assignment = .{
                        .target = left,
                        .value = right,
                        .op = op,
                    } },
                });
            },

            // Ternary
            .question => {
                self.advance();
                const then_expr = try self.parseExpression(.assignment);
                try self.expect(.colon, "':'");
                const else_expr = try self.parseExpression(.assignment);
                return try self.nodes.add(.{
                    .tag = .ternary,
                    .loc = loc,
                    .data = .{ .ternary = .{
                        .condition = left,
                        .then_branch = then_expr,
                        .else_branch = else_expr,
                    } },
                });
            },

            // Member access
            .dot => {
                self.advance();
                const prop = try self.expectIdentifier("property name");
                const prop_atom = try self.addAtom(prop.text(self.source));
                return try self.nodes.add(.{
                    .tag = .member_access,
                    .loc = loc,
                    .data = .{ .member = .{
                        .object = left,
                        .property = prop_atom,
                        .computed = null_node,
                        .is_optional = false,
                    } },
                });
            },

            // Computed member access
            .lbracket => {
                self.advance();
                const index = try self.parseExpression(.none);
                try self.expect(.rbracket, "']'");
                return try self.nodes.add(.{
                    .tag = .computed_access,
                    .loc = loc,
                    .data = .{ .member = .{
                        .object = left,
                        .property = 0,
                        .computed = index,
                        .is_optional = false,
                    } },
                });
            },

            // Optional chaining
            .question_dot => {
                self.advance();
                if (self.match(.lbracket)) {
                    const index = try self.parseExpression(.none);
                    try self.expect(.rbracket, "']'");
                    return try self.nodes.add(.{
                        .tag = .optional_chain,
                        .loc = loc,
                        .data = .{ .member = .{
                            .object = left,
                            .property = 0,
                            .computed = index,
                            .is_optional = true,
                        } },
                    });
                } else if (self.match(.lparen)) {
                    // Optional call
                    return self.parseCallArgs(left, loc, true);
                } else {
                    const prop = try self.expectIdentifier("property name");
                    const prop_atom = try self.addAtom(prop.text(self.source));
                    return try self.nodes.add(.{
                        .tag = .optional_chain,
                        .loc = loc,
                        .data = .{ .member = .{
                            .object = left,
                            .property = prop_atom,
                            .computed = null_node,
                            .is_optional = true,
                        } },
                    });
                }
            },

            // Function call
            .lparen => {
                self.advance();
                return self.parseCallArgs(left, loc, false);
            },

            // Postfix operators - not supported
            .plus_plus => {
                self.errors.addErrorAt(.unsupported_feature, self.current, "'x++' is not supported; use 'x = x + 1' instead");
                self.had_error = true;
                return error.ParseError;
            },
            .minus_minus => {
                self.errors.addErrorAt(.unsupported_feature, self.current, "'x--' is not supported; use 'x = x - 1' instead");
                self.had_error = true;
                return error.ParseError;
            },

            else => left,
        };
    }

    fn parseCallArgs(self: *Parser, callee: NodeIndex, loc: SourceLocation, is_optional: bool) anyerror!NodeIndex {
        var args = std.ArrayList(NodeIndex).empty;
        defer args.deinit(self.allocator);

        if (!self.check(.rparen)) {
            while (true) {
                if (self.match(.spread)) {
                    const spread_expr = try self.parseExpression(.assignment);
                    const spread_node = try self.nodes.add(.{
                        .tag = .spread,
                        .loc = loc,
                        .data = .{ .unary = .{ .op = .neg, .operand = spread_expr } },
                    });
                    try args.append(self.allocator, spread_node);
                } else {
                    const arg = try self.parseExpression(.assignment);
                    try args.append(self.allocator, arg);
                }
                if (!self.match(.comma)) break;
                if (self.check(.rparen)) break;
            }
        }
        try self.expect(.rparen, "')'");

        const args_start = if (args.items.len > 0)
            try self.addNodeList(args.items)
        else
            null_node;

        return try self.nodes.add(.{
            .tag = if (is_optional) .optional_call else .call,
            .loc = loc,
            .data = .{ .call = .{
                .callee = callee,
                .args_start = args_start,
                .args_count = @intCast(args.items.len),
                .is_optional = is_optional,
            } },
        });
    }

    // ============ Literal Parsing ============

    fn parseNumber(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        const text = self.current.text(self.source);
        self.advance();

        // Try to parse as integer first
        if (std.fmt.parseInt(i32, text, 0)) |int_val| {
            return try self.nodes.add(Node.litInt(loc, int_val));
        } else |_| {}

        // Parse as float
        const float_val = std.fmt.parseFloat(f64, text) catch 0.0;
        const float_idx = try self.constants.addFloat(float_val);
        return try self.nodes.add(.{
            .tag = .lit_float,
            .loc = loc,
            .data = .{ .float_idx = float_idx },
        });
    }

    fn parseString(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        const text = self.current.text(self.source);
        self.advance();

        // Strip quotes
        const content = if (text.len >= 2) text[1 .. text.len - 1] else "";
        const str_idx = try self.addString(content);
        return try self.nodes.add(Node.litString(loc, str_idx));
    }

    fn parseBoolLiteral(self: *Parser, value: bool) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance();
        return try self.nodes.add(Node.litBool(loc, value));
    }

    fn parseNullLiteral(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance();
        return try self.nodes.add(Node.litNull(loc));
    }

    fn parseUndefinedLiteral(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance();
        return try self.nodes.add(Node.litUndefined(loc));
    }

    fn parseRegexLiteral(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        const text = self.current.text(self.source);
        self.advance();

        // Parse /pattern/flags
        var pattern_end: usize = 1;
        var in_class = false;
        while (pattern_end < text.len) {
            const c = text[pattern_end];
            if (c == '\\' and pattern_end + 1 < text.len) {
                pattern_end += 2;
            } else if (c == '[') {
                in_class = true;
                pattern_end += 1;
            } else if (c == ']' and in_class) {
                in_class = false;
                pattern_end += 1;
            } else if (c == '/' and !in_class) {
                break;
            } else {
                pattern_end += 1;
            }
        }

        const pattern = text[1..pattern_end];
        const flags = if (pattern_end + 1 < text.len) text[pattern_end + 1 ..] else "";

        const pattern_idx = try self.addString(pattern);
        const flags_idx = try self.addString(flags);

        return try self.nodes.add(.{
            .tag = .lit_regex,
            .loc = loc,
            .data = .{ .regex = .{
                .pattern_idx = pattern_idx,
                .flags_idx = flags_idx,
            } },
        });
    }

    fn parseTemplateLiteral(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();

        if (self.current.type == .template_literal) {
            // Simple template with no interpolation
            const text = self.current.text(self.source);
            self.advance();
            const content = if (text.len >= 2) text[1 .. text.len - 1] else "";
            const str_idx = try self.addString(content);
            return try self.nodes.add(Node.litString(loc, str_idx));
        }

        // Template with interpolation
        var parts = std.ArrayList(NodeIndex).empty;
        defer parts.deinit(self.allocator);

        // template_head: `string${
        var text = self.current.text(self.source);
        self.advance();
        var content = if (text.len >= 3) text[1 .. text.len - 2] else "";
        var str_idx = try self.addString(content);
        var str_node = try self.nodes.add(.{
            .tag = .template_part_string,
            .loc = loc,
            .data = .{ .string_idx = str_idx },
        });
        try parts.append(self.allocator, str_node);

        while (self.current.type != .template_tail and self.current.type != .eof) {
            // Expression
            const expr = try self.parseExpression(.none);
            const expr_node = try self.nodes.add(.{
                .tag = .template_part_expr,
                .loc = loc,
                .data = .{ .opt_value = expr },
            });
            try parts.append(self.allocator, expr_node);

            if (self.current.type == .template_middle) {
                text = self.current.text(self.source);
                self.advance();
                content = if (text.len >= 3) text[1 .. text.len - 2] else "";
                str_idx = try self.addString(content);
                str_node = try self.nodes.add(.{
                    .tag = .template_part_string,
                    .loc = loc,
                    .data = .{ .string_idx = str_idx },
                });
                try parts.append(self.allocator, str_node);
            } else if (self.current.type == .template_tail) {
                break;
            } else {
                self.errorAtCurrent("expected template continuation");
                break;
            }
        }

        // template_tail: }string`
        if (self.current.type == .template_tail) {
            text = self.current.text(self.source);
            self.advance();
            content = if (text.len >= 2) text[1 .. text.len - 1] else "";
            str_idx = try self.addString(content);
            str_node = try self.nodes.add(.{
                .tag = .template_part_string,
                .loc = loc,
                .data = .{ .string_idx = str_idx },
            });
            try parts.append(self.allocator, str_node);
        }

        const parts_start = try self.addNodeList(parts.items);
        return try self.nodes.add(.{
            .tag = .template_literal,
            .loc = loc,
            .data = .{ .template = .{
                .parts_start = parts_start,
                .parts_count = @intCast(parts.items.len),
                .tag = null_node,
            } },
        });
    }

    fn parseIdentifier(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        const name = self.current.text(self.source);
        self.advance();

        const name_atom = try self.addAtom(name);
        const binding = self.scopes.resolveBinding(name, name_atom);

        return try self.nodes.add(Node.identifier(loc, binding));
    }

    fn parseThis(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance();
        return try self.nodes.add(Node.thisExpr(loc));
    }

    fn parseSuper(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance();

        // super must be followed by . or (
        if (!self.check(.dot) and !self.check(.lparen) and !self.check(.lbracket)) {
            self.errorAt(loc, "'super' keyword unexpected here");
        }

        return try self.nodes.add(.{
            .tag = .identifier,
            .loc = loc,
            .data = .{ .binding = BindingRef.global(0) }, // Special handling in codegen
        });
    }

    fn parseUnaryOp(self: *Parser, op: UnaryOp) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance();
        const operand = try self.parseExpression(.unary);
        return try self.nodes.add(Node.unaryOp(loc, op, operand));
    }

    fn parseUnaryKeyword(self: *Parser, op: UnaryOp) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance();
        const operand = try self.parseExpression(.unary);
        return try self.nodes.add(Node.unaryOp(loc, op, operand));
    }

    fn parseNewExpression(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'new'

        // Parse constructor expression
        var callee = try self.parsePrefixExpr();

        // Parse member access chain
        while (self.check(.dot) or self.check(.lbracket)) {
            if (self.match(.dot)) {
                const prop = try self.expectIdentifier("property name");
                const prop_atom = try self.addAtom(prop.text(self.source));
                callee = try self.nodes.add(.{
                    .tag = .member_access,
                    .loc = loc,
                    .data = .{ .member = .{
                        .object = callee,
                        .property = prop_atom,
                        .computed = null_node,
                        .is_optional = false,
                    } },
                });
            } else if (self.match(.lbracket)) {
                const index = try self.parseExpression(.none);
                try self.expect(.rbracket, "']'");
                callee = try self.nodes.add(.{
                    .tag = .computed_access,
                    .loc = loc,
                    .data = .{ .member = .{
                        .object = callee,
                        .property = 0,
                        .computed = index,
                        .is_optional = false,
                    } },
                });
            }
        }

        // Parse arguments if present
        var args = std.ArrayList(NodeIndex).empty;
        defer args.deinit(self.allocator);

        if (self.match(.lparen)) {
            if (!self.check(.rparen)) {
                while (true) {
                    const arg = try self.parseExpression(.assignment);
                    try args.append(self.allocator, arg);
                    if (!self.match(.comma)) break;
                    if (self.check(.rparen)) break;
                }
            }
            try self.expect(.rparen, "')'");
        }

        const args_start = if (args.items.len > 0)
            try self.addNodeList(args.items)
        else
            null_node;

        return try self.nodes.add(.{
            .tag = .new_expr,
            .loc = loc,
            .data = .{ .call = .{
                .callee = callee,
                .args_start = args_start,
                .args_count = @intCast(args.items.len),
                .is_optional = false,
            } },
        });
    }

    fn parseFunctionExpression(self: *Parser) anyerror!NodeIndex {
        self.advance(); // consume 'function'

        var flags = FunctionFlags{};

        // Check for generator
        if (self.match(.star)) {
            flags.is_generator = true;
        }

        // Optional name
        var name_atom: u16 = 0;
        if (self.check(.identifier)) {
            const name = self.current;
            self.advance();
            name_atom = try self.addAtom(name.text(self.source));
        }

        return try self.parseFunctionBody(name_atom, flags);
    }

    fn parseClassExpression(self: *Parser) anyerror!NodeIndex {
        // Similar to parseClassDeclaration but returns expression
        const loc = self.current.location();
        self.advance(); // consume 'class'

        var name_atom: u16 = 0;
        if (self.check(.identifier)) {
            const name = self.current;
            self.advance();
            name_atom = try self.addAtom(name.text(self.source));
        }

        var super_class: NodeIndex = null_node;
        if (self.match(.kw_extends)) {
            super_class = try self.parseExpression(.none);
        }

        const scope_id = try self.scopes.pushScope(.class_body);

        try self.expect(.lbrace, "'{'");

        var members = std.ArrayList(NodeIndex).empty;
        defer members.deinit(self.allocator);

        while (!self.check(.rbrace) and !self.check(.eof)) {
            var is_static = false;
            if (self.match(.kw_static)) {
                is_static = true;
            }

            const member_loc = self.current.location();
            var member_kind: Node.ClassMember.MemberKind = .method;

            if (self.check(.kw_get) and self.peekIsPropertyName()) {
                self.advance();
                member_kind = .getter;
            } else if (self.check(.kw_set) and self.peekIsPropertyName()) {
                self.advance();
                member_kind = .setter;
            }

            var key_node: NodeIndex = undefined;
            var is_computed = false;

            if (self.match(.lbracket)) {
                is_computed = true;
                key_node = try self.parseExpression(.none);
                try self.expect(.rbracket, "']'");
            } else if (self.check(.identifier) or self.check(.string_literal) or self.check(.number)) {
                const key_tok = self.current;
                self.advance();
                const key_idx = try self.addString(key_tok.text(self.source));
                key_node = try self.nodes.add(Node.litString(key_tok.location(), key_idx));
            } else {
                self.errorAtCurrent("expected property name");
                break;
            }

            var value: NodeIndex = undefined;
            if (self.check(.lparen)) {
                value = try self.parseFunctionBody(0, .{
                    .is_method = true,
                    .is_getter = member_kind == .getter,
                    .is_setter = member_kind == .setter,
                });
            } else if (self.match(.assign)) {
                member_kind = .field;
                value = try self.parseExpression(.assignment);
                try self.expectSemicolon();
            } else {
                member_kind = .field;
                value = null_node;
                _ = self.match(.semicolon);
            }

            const member_node = try self.nodes.add(.{
                .tag = .class_method,
                .loc = member_loc,
                .data = .{ .class_member = .{
                    .key = key_node,
                    .value = value,
                    .kind = member_kind,
                    .is_static = is_static,
                    .is_computed = is_computed,
                } },
            });
            try members.append(self.allocator, member_node);
        }

        try self.expect(.rbrace, "'}'");
        self.scopes.popScope();

        const members_start = if (members.items.len > 0)
            try self.addNodeList(members.items)
        else
            null_node;

        return try self.nodes.add(.{
            .tag = .class_decl,
            .loc = loc,
            .data = .{ .class_decl = .{
                .scope_id = scope_id,
                .name_atom = name_atom,
                .super_class = super_class,
                .members_start = members_start,
                .members_count = @intCast(members.items.len),
                .binding = BindingRef{ .scope_id = 0, .slot = 0, .kind = .local },
            } },
        });
    }

    fn parseAsyncExpression(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'async'

        if (self.check(.kw_function)) {
            self.advance();
            var flags = FunctionFlags{ .is_async = true };
            if (self.match(.star)) {
                flags.is_generator = true;
            }
            var name_atom: u16 = 0;
            if (self.check(.identifier)) {
                const name = self.current;
                self.advance();
                name_atom = try self.addAtom(name.text(self.source));
            }
            return try self.parseFunctionBody(name_atom, flags);
        }

        // async arrow function
        if (self.isArrowFunction()) {
            return self.parseArrowFunction();
        }

        // Just 'async' as identifier
        const name_atom = try self.addAtom("async");
        const binding = self.scopes.resolveBinding("async", name_atom);
        return try self.nodes.add(Node.identifier(loc, binding));
    }

    fn parseYieldExpression(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'yield'

        var value: NodeIndex = null_node;
        if (!self.check(.semicolon) and !self.check(.rbrace) and
            !self.check(.rparen) and !self.check(.rbracket) and
            !self.check(.comma) and !self.check(.colon) and !self.check(.eof))
        {
            // Check for yield*
            _ = self.match(.star);
            value = try self.parseExpression(.assignment);
        }

        return try self.nodes.add(.{
            .tag = .yield_expr,
            .loc = loc,
            .data = .{ .opt_value = value },
        });
    }

    fn parseAwaitExpression(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume 'await'

        const operand = try self.parseExpression(.unary);
        return try self.nodes.add(.{
            .tag = .await_expr,
            .loc = loc,
            .data = .{ .opt_value = operand },
        });
    }

    fn parseGroupedOrArrowParams(self: *Parser) anyerror!NodeIndex {
        // Could be (expr) or arrow function params
        self.advance(); // consume '('

        if (self.check(.rparen)) {
            // () => ... empty params arrow function handled by isArrowFunction
            self.advance();
            self.errorAtCurrent("unexpected ')'");
            return error.UnexpectedToken;
        }

        const expr = try self.parseExpression(.none);
        try self.expect(.rparen, "')'");

        return expr;
    }

    fn parseArrayLiteral(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume '['

        var elements = std.ArrayList(NodeIndex).empty;
        defer elements.deinit(self.allocator);
        var has_spread = false;

        if (!self.check(.rbracket)) {
            while (true) {
                if (self.check(.comma)) {
                    // Elision (hole in array)
                    try elements.append(self.allocator, null_node);
                } else if (self.match(.spread)) {
                    has_spread = true;
                    const spread_expr = try self.parseExpression(.assignment);
                    const spread_node = try self.nodes.add(.{
                        .tag = .spread,
                        .loc = loc,
                        .data = .{ .unary = .{ .op = .neg, .operand = spread_expr } },
                    });
                    try elements.append(self.allocator, spread_node);
                } else {
                    const elem = try self.parseExpression(.assignment);
                    try elements.append(self.allocator, elem);
                }
                if (!self.match(.comma)) break;
                if (self.check(.rbracket)) break;
            }
        }
        try self.expect(.rbracket, "']'");

        const elements_start = if (elements.items.len > 0)
            try self.addNodeList(elements.items)
        else
            null_node;

        return try self.nodes.add(.{
            .tag = .array_literal,
            .loc = loc,
            .data = .{ .array = .{
                .elements_start = elements_start,
                .elements_count = @intCast(elements.items.len),
                .has_spread = has_spread,
            } },
        });
    }

    fn parseObjectLiteral(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume '{'

        var properties = std.ArrayList(NodeIndex).empty;
        defer properties.deinit(self.allocator);

        if (!self.check(.rbrace)) {
            while (true) {
                const prop_loc = self.current.location();

                // Check for spread
                if (self.match(.spread)) {
                    const spread_expr = try self.parseExpression(.assignment);
                    const spread_node = try self.nodes.add(.{
                        .tag = .object_spread,
                        .loc = prop_loc,
                        .data = .{ .opt_value = spread_expr },
                    });
                    try properties.append(self.allocator, spread_node);
                } else {
                    // Check for getter/setter
                    var prop_kind: NodeTag = .object_property;
                    if (self.check(.kw_get) and self.peekIsPropertyName()) {
                        self.advance();
                        prop_kind = .object_getter;
                    } else if (self.check(.kw_set) and self.peekIsPropertyName()) {
                        self.advance();
                        prop_kind = .object_setter;
                    }

                    // Property key
                    var key_node: NodeIndex = undefined;
                    var is_computed = false;
                    var is_shorthand = false;

                    if (self.match(.lbracket)) {
                        is_computed = true;
                        key_node = try self.parseExpression(.none);
                        try self.expect(.rbracket, "']'");
                    } else if (self.check(.identifier)) {
                        const key = self.current;
                        self.advance();

                        // Check for shorthand { x } or method { x() {} }
                        if (prop_kind == .object_property and
                            !self.check(.colon) and !self.check(.lparen))
                        {
                            is_shorthand = true;
                            const key_idx = try self.addString(key.text(self.source));
                            key_node = try self.nodes.add(Node.litString(key.location(), key_idx));

                            // Shorthand - value is identifier reference
                            const name_atom = try self.addAtom(key.text(self.source));
                            const binding = self.scopes.resolveBinding(key.text(self.source), name_atom);
                            const value_node = try self.nodes.add(Node.identifier(key.location(), binding));

                            const prop_node = try self.nodes.add(.{
                                .tag = .object_property,
                                .loc = prop_loc,
                                .data = .{ .property = .{
                                    .key = key_node,
                                    .value = value_node,
                                    .is_computed = false,
                                    .is_shorthand = true,
                                } },
                            });
                            try properties.append(self.allocator, prop_node);

                            if (!self.match(.comma)) break;
                            if (self.check(.rbrace)) break;
                            continue;
                        }

                        const key_idx = try self.addString(key.text(self.source));
                        key_node = try self.nodes.add(Node.litString(key.location(), key_idx));
                    } else if (self.check(.string_literal) or self.check(.number)) {
                        const key = self.current;
                        self.advance();
                        const text = key.text(self.source);
                        const content = if (key.type == .string_literal and text.len >= 2)
                            text[1 .. text.len - 1]
                        else
                            text;
                        const key_idx = try self.addString(content);
                        key_node = try self.nodes.add(Node.litString(key.location(), key_idx));
                    } else if (self.isKeyword(self.current.type)) {
                        // JavaScript allows reserved keywords as property names
                        // e.g., {class: 'foo', if: 'bar'}
                        const key = self.current;
                        self.advance();
                        const key_idx = try self.addString(key.text(self.source));
                        key_node = try self.nodes.add(Node.litString(key.location(), key_idx));
                    } else {
                        self.errorAtCurrent("expected property name");
                        break;
                    }

                    // Method shorthand or getter/setter
                    if (self.check(.lparen)) {
                        const method_flags = FunctionFlags{
                            .is_method = true,
                            .is_getter = prop_kind == .object_getter,
                            .is_setter = prop_kind == .object_setter,
                        };
                        const method_body = try self.parseFunctionBody(0, method_flags);

                        const tag: NodeTag = switch (prop_kind) {
                            .object_getter => .object_getter,
                            .object_setter => .object_setter,
                            else => .object_method,
                        };

                        const prop_node = try self.nodes.add(.{
                            .tag = tag,
                            .loc = prop_loc,
                            .data = .{ .property = .{
                                .key = key_node,
                                .value = method_body,
                                .is_computed = is_computed,
                                .is_shorthand = false,
                            } },
                        });
                        try properties.append(self.allocator, prop_node);
                    } else {
                        // Regular property
                        try self.expect(.colon, "':'");
                        const value = try self.parseExpression(.assignment);

                        const prop_node = try self.nodes.add(.{
                            .tag = .object_property,
                            .loc = prop_loc,
                            .data = .{ .property = .{
                                .key = key_node,
                                .value = value,
                                .is_computed = is_computed,
                                .is_shorthand = is_shorthand,
                            } },
                        });
                        try properties.append(self.allocator, prop_node);
                    }
                }

                if (!self.match(.comma)) break;
                if (self.check(.rbrace)) break;
            }
        }
        try self.expect(.rbrace, "'}'");

        const props_start = if (properties.items.len > 0)
            try self.addNodeList(properties.items)
        else
            null_node;

        return try self.nodes.add(.{
            .tag = .object_literal,
            .loc = loc,
            .data = .{ .object = .{
                .properties_start = props_start,
                .properties_count = @intCast(properties.items.len),
            } },
        });
    }

    // ============ Arrow Function Detection ============

    fn isArrowFunction(self: *Parser) bool {
        // Simple cases
        if (self.check(.identifier)) {
            // x => ...
            const state = self.tokenizer.saveState();
            const tok1 = self.tokenizer.next();
            const tok2 = self.tokenizer.next();
            self.tokenizer.restoreState(state);
            return tok1.type == .identifier and tok2.type == .arrow;
        }

        if (self.check(.lparen)) {
            // (...) => ...
            const state = self.tokenizer.saveState();
            var depth: u32 = 0;
            var tok = self.tokenizer.next();

            // Skip past parentheses
            while (tok.type != .eof) {
                if (tok.type == .lparen) depth += 1;
                if (tok.type == .rparen) {
                    if (depth == 0) break; // Already at top level
                    depth -= 1;
                    if (depth == 0) break;
                }
                tok = self.tokenizer.next();
            }

            tok = self.tokenizer.next();
            self.tokenizer.restoreState(state);
            return tok.type == .arrow;
        }

        return false;
    }

    fn parseArrowFunction(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        var flags = FunctionFlags{ .is_arrow = true };

        // Enter function scope
        const scope_id = try self.scopes.pushScope(.function);
        const was_in_function = self.in_function;
        self.in_function = true;

        // Parse parameters
        var params = std.ArrayList(NodeIndex).empty;
        defer params.deinit(self.allocator);

        if (self.check(.identifier)) {
            // Single parameter: x => ...
            const param_name = self.current;
            self.advance();
            const param_atom = try self.addAtom(param_name.text(self.source));
            const param_binding = self.scopes.declareBinding(
                param_name.text(self.source),
                param_atom,
                .parameter,
                false,
            ) catch return error.TooManyLocals;

            const param_node = try self.nodes.add(.{
                .tag = .pattern_element,
                .loc = param_name.location(),
                .data = .{ .pattern_elem = .{
                    .kind = .simple,
                    .binding = param_binding,
                    .key = null_node,
                    .key_atom = 0,
                    .default_value = null_node,
                } },
            });
            try params.append(self.allocator, param_node);
        } else {
            // Parenthesized parameters
            try self.expect(.lparen, "'('");

            if (!self.check(.rparen)) {
                while (true) {
                    if (self.match(.spread)) {
                        flags.has_rest_param = true;
                    }

                    const param_name = try self.expectIdentifier("parameter name");
                    const param_atom = try self.addAtom(param_name.text(self.source));
                    const param_binding = self.scopes.declareBinding(
                        param_name.text(self.source),
                        param_atom,
                        .parameter,
                        false,
                    ) catch return error.TooManyLocals;

                    var default_value: NodeIndex = null_node;
                    if (self.match(.assign)) {
                        flags.has_default_params = true;
                        default_value = try self.parseExpression(.assignment);
                    }

                    const param_node = try self.nodes.add(.{
                        .tag = .pattern_element,
                        .loc = param_name.location(),
                        .data = .{ .pattern_elem = .{
                            .kind = if (flags.has_rest_param) .rest else .simple,
                            .binding = param_binding,
                            .key = null_node,
                            .key_atom = 0,
                            .default_value = default_value,
                        } },
                    });
                    try params.append(self.allocator, param_node);

                    if (flags.has_rest_param) break;
                    if (!self.match(.comma)) break;
                    if (self.check(.rparen)) break;
                }
            }
            try self.expect(.rparen, "')'");
        }

        try self.expect(.arrow, "'=>'");

        // Parse body
        var body: NodeIndex = undefined;
        if (self.check(.lbrace)) {
            body = try self.parseBlock();
        } else {
            // Expression body
            const expr = try self.parseExpression(.assignment);
            body = try self.nodes.add(.{
                .tag = .return_stmt,
                .loc = loc,
                .data = .{ .opt_value = expr },
            });
        }

        self.in_function = was_in_function;
        self.scopes.popScope();

        const params_start = if (params.items.len > 0)
            try self.addNodeList(params.items)
        else
            null_node;

        return try self.nodes.add(.{
            .tag = .arrow_function,
            .loc = loc,
            .data = .{ .function = .{
                .scope_id = scope_id,
                .name_atom = 0,
                .params_start = params_start,
                .params_count = @intCast(params.items.len),
                .body = body,
                .flags = flags,
            } },
        });
    }

    // ============ JSX Parsing ============

    fn peekJsxClosingTag(self: *Parser) bool {
        // Look ahead from the tokenizer position (right after '<') to see if this is a closing tag.
        // Avoid tokenizer.next() here because it can misclassify `</` as a regex literal.
        var i: usize = self.tokenizer.pos;
        while (i < self.source.len) : (i += 1) {
            switch (self.source[i]) {
                ' ', '\t', '\r', '\n' => continue,
                else => return self.source[i] == '/',
            }
        }
        return false;
    }

    fn parseJsxElement(self: *Parser) anyerror!NodeIndex {
        const loc = self.current.location();
        self.advance(); // consume '<'

        // Check for fragment <>
        if (self.check(.gt)) {
            self.advance();
            return self.parseJsxFragment(loc);
        }

        // Tag name
        var tag_name: []const u8 = "";
        var is_component = false;

        if (self.check(.identifier)) {
            tag_name = self.current.text(self.source);
            is_component = tag_name.len > 0 and tag_name[0] >= 'A' and tag_name[0] <= 'Z';
            self.advance();

            // Handle member expressions: <Foo.Bar />
            while (self.match(.dot)) {
                if (self.check(.identifier)) {
                    self.advance();
                    // Append to tag name (simplified)
                } else {
                    self.errorAtCurrent("expected identifier after '.'");
                    break;
                }
            }
        } else {
            self.errorAtCurrent("expected JSX tag name");
            return error.UnexpectedToken;
        }

        const tag_atom = if (is_component)
            try self.addAtom(tag_name)
        else
            try self.addString(tag_name);

        // Parse attributes
        var props = std.ArrayList(NodeIndex).empty;
        defer props.deinit(self.allocator);

        while (!self.check(.gt) and !self.check(.slash) and !self.check(.eof)) {
            const attr_loc = self.current.location();

            if (self.match(.lbrace)) {
                // Spread attribute: {...props}
                if (self.match(.spread)) {
                    const spread_expr = try self.parseExpression(.assignment);
                    try self.expect(.rbrace, "'}'");

                    const attr_node = try self.nodes.add(.{
                        .tag = .jsx_spread_attribute,
                        .loc = attr_loc,
                        .data = .{ .jsx_attr = .{
                            .name_atom = 0,
                            .value = spread_expr,
                            .is_spread = true,
                        } },
                    });
                    try props.append(self.allocator, attr_node);
                } else {
                    self.errorAtCurrent("expected '...' in JSX spread");
                }
                continue;
            }

            // JSX attribute names can be identifiers or keywords (like "class", "for")
            // and can contain hyphens (like "hx-post", "data-value", "aria-label")
            if (!self.check(.identifier) and !self.isKeyword(self.current.type)) break;

            // Build hyphenated attribute name: start with first identifier
            const attr_name_start: u32 = self.current.start;
            var attr_name_end: u32 = self.current.start + self.current.len;
            self.advance();

            // Continue consuming hyphens and identifiers (allows hx-on--after-request pattern)
            // Handle both single minus (-) and minus_minus (--)
            while (self.check(.minus) or self.check(.minus_minus)) {
                // Include the hyphen(s) in the name
                attr_name_end = self.current.start + self.current.len;
                self.advance(); // consume - or --

                // If followed by identifier/keyword, include it too
                if (self.check(.identifier) or self.isKeyword(self.current.type)) {
                    attr_name_end = self.current.start + self.current.len;
                    self.advance();
                }
                // If not followed by identifier (e.g., another hyphen or =), loop continues
            }

            const attr_name_text = self.source[attr_name_start..attr_name_end];
            const attr_atom = try self.addAtom(attr_name_text);

            var attr_value: NodeIndex = null_node;
            if (self.match(.assign)) {
                if (self.check(.string_literal)) {
                    attr_value = try self.parseString();
                } else if (self.match(.lbrace)) {
                    attr_value = try self.parseExpression(.none);
                    try self.expect(.rbrace, "'}'");
                } else {
                    self.errorAtCurrent("expected attribute value");
                }
            }
            // else: boolean attribute like `disabled`

            const attr_node = try self.nodes.add(.{
                .tag = .jsx_attribute,
                .loc = attr_loc,
                .data = .{ .jsx_attr = .{
                    .name_atom = attr_atom,
                    .value = attr_value,
                    .is_spread = false,
                } },
            });
            try props.append(self.allocator, attr_node);
        }

        // Self-closing or children
        var children = std.ArrayList(NodeIndex).empty;
        defer children.deinit(self.allocator);
        var self_closing = false;

        if (self.match(.slash)) {
            self_closing = true;
            try self.expect(.gt, "'>'");
        } else {
            try self.expect(.gt, "'>'");

            // Parse children
            while (!self.check(.eof)) {
                // Check for closing tag
                if (self.check(.lt)) {
                    if (self.peekJsxClosingTag()) {
                        // Closing tag
                        // Force slash tokenization (avoid regex literal)
                        self.tokenizer.can_be_regex = false;
                        self.advance(); // <
                        try self.expect(.slash, "'/'");

                        var close_tok: ?Token = null;
                        if (self.check(.identifier)) {
                            close_tok = self.current;
                            self.advance();
                        }
                        if (close_tok) |tok| {
                            const close_name = tok.text(self.source);
                            if (!std.mem.eql(u8, close_name, tag_name)) {
                                self.errors.addErrorAt(.mismatched_jsx_tag, tok, "mismatched JSX closing tag");
                            }
                        }

                        try self.expect(.gt, "'>'");
                        break;
                    }

                    // Nested element
                    const child = try self.parseJsxElement();
                    try children.append(self.allocator, child);
                } else if (self.match(.lbrace)) {
                    // Expression child
                    const expr = try self.parseExpression(.none);
                    try self.expect(.rbrace, "'}'");
                    const child = try self.nodes.add(.{
                        .tag = .jsx_expr_container,
                        .loc = loc,
                        .data = .{ .opt_value = expr },
                    });
                    try children.append(self.allocator, child);
                } else {
                    // Raw text content (may include punctuation/whitespace)
                    const text_start: usize = @intCast(self.current.start);
                    var text_end: usize = text_start + self.current.len;

                    while (true) {
                        const state = self.tokenizer.saveState();
                        const next = self.tokenizer.next();
                        self.tokenizer.restoreState(state);

                        if (next.type == .lt or next.type == .lbrace or next.type == .eof) {
                            text_end = @intCast(next.start);
                            break;
                        }

                        // Consume the next token and extend the text range
                        self.advance();
                        text_end = @intCast(self.current.start + self.current.len);
                    }

                    const raw = self.source[text_start..@min(text_end, self.source.len)];
                    const trimmed = std.mem.trim(u8, raw, " \t\n\r");
                    if (trimmed.len > 0) {
                        const text_idx = try self.addString(trimmed);
                        const child = try self.nodes.add(.{
                            .tag = .jsx_text,
                            .loc = loc,
                            .data = .{ .jsx_text = text_idx },
                        });
                        try children.append(self.allocator, child);
                    }

                    // Move to the next token (<, {, or eof) for the outer loop
                    self.advance();
                }
            }
        }

        const props_start = if (props.items.len > 0)
            try self.addNodeList(props.items)
        else
            null_node;

        const children_start = if (children.items.len > 0)
            try self.addNodeList(children.items)
        else
            null_node;

        return try self.nodes.add(.{
            .tag = .jsx_element,
            .loc = loc,
            .data = .{ .jsx_element = .{
                .tag_atom = tag_atom,
                .is_component = is_component,
                .props_start = props_start,
                .props_count = @intCast(props.items.len),
                .children_start = children_start,
                .children_count = @intCast(children.items.len),
                .self_closing = self_closing,
            } },
        });
    }

    fn parseJsxFragment(self: *Parser, loc: SourceLocation) anyerror!NodeIndex {
        var children = std.ArrayList(NodeIndex).empty;
        defer children.deinit(self.allocator);

        while (!self.check(.eof)) {
            // Check for closing fragment </>
            if (self.check(.lt)) {
                if (self.peekJsxClosingTag()) {
                    self.tokenizer.can_be_regex = false;
                    self.advance(); // <
                    try self.expect(.slash, "'/'");
                    try self.expect(.gt, "'>'");
                    break;
                }

                const child = try self.parseJsxElement();
                try children.append(self.allocator, child);
            } else if (self.match(.lbrace)) {
                const expr = try self.parseExpression(.none);
                try self.expect(.rbrace, "'}'");
                const child = try self.nodes.add(.{
                    .tag = .jsx_expr_container,
                    .loc = loc,
                    .data = .{ .opt_value = expr },
                });
                try children.append(self.allocator, child);
            } else {
                // Raw text content
                const text_start: usize = @intCast(self.current.start);
                var text_end: usize = text_start + self.current.len;

                while (true) {
                    const state = self.tokenizer.saveState();
                    const next = self.tokenizer.next();
                    self.tokenizer.restoreState(state);

                    if (next.type == .lt or next.type == .lbrace or next.type == .eof) {
                        text_end = @intCast(next.start);
                        break;
                    }

                    self.advance();
                    text_end = @intCast(self.current.start + self.current.len);
                }

                const raw = self.source[text_start..@min(text_end, self.source.len)];
                const trimmed = std.mem.trim(u8, raw, " \t\n\r");
                if (trimmed.len > 0) {
                    const text_idx = try self.addString(trimmed);
                    const child = try self.nodes.add(.{
                        .tag = .jsx_text,
                        .loc = loc,
                        .data = .{ .jsx_text = text_idx },
                    });
                    try children.append(self.allocator, child);
                }

                self.advance();
            }
        }

        const children_start = if (children.items.len > 0)
            try self.addNodeList(children.items)
        else
            null_node;

        return try self.nodes.add(.{
            .tag = .jsx_fragment,
            .loc = loc,
            .data = .{ .jsx_element = .{
                .tag_atom = 0,
                .is_component = false,
                .props_start = null_node,
                .props_count = 0,
                .children_start = children_start,
                .children_count = @intCast(children.items.len),
                .self_closing = false,
            } },
        });
    }

    // ============ Utility Functions ============

    fn advance(self: *Parser) void {
        self.previous = self.current;
        self.current = self.tokenizer.next();
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        return self.current.type == token_type;
    }

    fn match(self: *Parser, token_type: TokenType) bool {
        if (!self.check(token_type)) return false;
        self.advance();
        return true;
    }

    fn expect(self: *Parser, token_type: TokenType, message: []const u8) !void {
        if (self.check(token_type)) {
            self.advance();
            return;
        }
        self.errors.addExpectedError(self.current, message);
        return error.UnexpectedToken;
    }

    fn expectSemicolon(self: *Parser) !void {
        if (self.match(.semicolon)) return;
        // ASI: accept implicit semicolon after } or at newline
        if (self.previous.type == .rbrace) return;
        if (self.check(.rbrace) or self.check(.eof)) return;
        // For simplicity, just accept
    }

    fn expectIdentifier(self: *Parser, ctx_label: []const u8) !Token {
        if (self.check(.identifier)) {
            const tok = self.current;
            self.advance();
            return tok;
        }
        self.errors.addExpectedError(self.current, ctx_label);
        return error.UnexpectedToken;
    }

    fn peekIsPropertyName(self: *Parser) bool {
        const state = self.tokenizer.saveState();
        const tok = self.tokenizer.next();
        self.tokenizer.restoreState(state);
        return tok.type == .identifier or tok.type == .string_literal or
            tok.type == .number or tok.type == .lbracket or self.isKeyword(tok.type);
    }

    /// Check if token type is a keyword (can be used as property name)
    fn isKeyword(self: *Parser, token_type: TokenType) bool {
        _ = self;
        return switch (token_type) {
            // JavaScript keywords that can be used as property names
            .kw_break, .kw_case, .kw_catch, .kw_continue, .kw_debugger,
            .kw_default, .kw_delete, .kw_do, .kw_else, .kw_finally,
            .kw_for, .kw_function, .kw_if, .kw_in, .kw_instanceof,
            .kw_new, .kw_return, .kw_switch, .kw_this, .kw_throw,
            .kw_try, .kw_typeof, .kw_var, .kw_void, .kw_while,
            .kw_with, .kw_class, .kw_const, .kw_export, .kw_extends,
            .kw_import, .kw_super, .kw_let, .kw_static, .kw_yield,
            .kw_async, .kw_await, .kw_get, .kw_set, .kw_of, .kw_from,
            .kw_as,
            // Literals that look like keywords
            .true_lit, .false_lit, .null_lit,
            => true,
            else => false,
        };
    }

    fn getInfixPrecedence(self: *Parser, token_type: TokenType) Precedence {
        _ = self;
        return switch (token_type) {
            .comma => .comma,
            .assign, .plus_assign, .minus_assign, .star_assign, .slash_assign, .percent_assign, .ampersand_assign, .pipe_assign, .caret_assign, .lt_lt_assign, .gt_gt_assign, .gt_gt_gt_assign, .star_star_assign, .ampersand_ampersand_assign, .pipe_pipe_assign, .question_question_assign => .assignment,
            .question => .ternary,
            .question_question => .nullish,
            .pipe_pipe => .or_op,
            .ampersand_ampersand => .and_op,
            .pipe => .bit_or,
            .caret => .bit_xor,
            .ampersand => .bit_and,
            .eq, .ne, .eq_eq, .ne_ne => .equality,
            .lt, .le, .gt, .ge, .kw_in, .kw_instanceof => .comparison,
            .lt_lt, .gt_gt, .gt_gt_gt => .shift,
            .plus, .minus => .additive,
            .star, .slash, .percent => .multiplicative,
            .star_star => .exponent,
            .lparen, .lbracket, .dot, .question_dot => .call,
            .plus_plus, .minus_minus => .postfix,
            else => .none,
        };
    }

    fn tokenToBinaryOp(self: *Parser, token_type: TokenType) BinaryOp {
        _ = self;
        return switch (token_type) {
            .plus => .add,
            .minus => .sub,
            .star => .mul,
            .slash => .div,
            .percent => .mod,
            .star_star => .pow,
            .eq => .eq,
            .ne => .neq,
            .eq_eq => .strict_eq,
            .ne_ne => .strict_neq,
            .lt => .lt,
            .le => .lte,
            .gt => .gt,
            .ge => .gte,
            .ampersand => .bit_and,
            .pipe => .bit_or,
            .caret => .bit_xor,
            .lt_lt => .shl,
            .gt_gt => .shr,
            .gt_gt_gt => .ushr,
            .ampersand_ampersand => .and_op,
            .pipe_pipe => .or_op,
            .question_question => .nullish,
            .kw_in => .in_op,
            .kw_instanceof => .instanceof,
            else => .add,
        };
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) void {
        self.errors.addErrorAt(.unexpected_token, self.current, message);
        self.had_error = true;
    }

    fn errorAt(self: *Parser, loc: SourceLocation, message: []const u8) void {
        self.errors.addError(.unexpected_token, loc, message);
        self.had_error = true;
    }

    fn synchronize(self: *Parser) void {
        self.errors.enterPanicMode();

        while (!self.check(.eof)) {
            if (self.previous.type == .semicolon) {
                self.errors.exitPanicMode();
                return;
            }

            switch (self.current.type) {
                .kw_class, .kw_function, .kw_var, .kw_let, .kw_const, .kw_for, .kw_if, .kw_while, .kw_return, .kw_try => {
                    self.errors.exitPanicMode();
                    return;
                },
                else => {},
            }

            self.advance();
        }
        self.errors.exitPanicMode();
    }

    // ============ Node List Helpers ============

    fn addNodeList(self: *Parser, indices: []const NodeIndex) anyerror!NodeIndex {
        // Store the list of indices in the node list's index storage
        return try self.nodes.addIndexList(indices);
    }

    fn addStmtList(self: *Parser, stmts: []const NodeIndex) anyerror!NodeIndex {
        return self.addNodeList(stmts);
    }

    fn addString(self: *Parser, str: []const u8) !u16 {
        return try self.constants.addString(str);
    }

    fn addAtom(self: *Parser, name: []const u8) !u16 {
        // Check predefined atoms first (e.g., "handler" -> 159)
        if (object.lookupPredefinedAtom(name)) |atom| {
            return @truncate(@intFromEnum(atom));
        }
        // Intern dynamic atoms if atom table is available
        if (self.atoms) |atoms| {
            const atom = try atoms.intern(name);
            return @truncate(@intFromEnum(atom));
        }
        // Fall back to string constant pool for dynamic atoms (standalone parser)
        return try self.constants.addString(name);
    }

    // ============ Public API ============

    pub fn enableJsx(self: *Parser) void {
        self.tokenizer.enableJsx();
    }

    pub fn hasErrors(self: *const Parser) bool {
        return self.errors.hasErrors();
    }

    pub fn getErrors(self: *const Parser) []const error_mod.ParseError {
        return self.errors.getErrors();
    }
};

// ============ Tests ============

test "parse simple expression" {
    var parser = Parser.init(std.testing.allocator, "1 + 2;");
    defer parser.deinit();

    const result = parser.parse() catch {
        try std.testing.expect(false);
        return;
    };

    try std.testing.expect(result != null_node);
    try std.testing.expect(!parser.hasErrors());
}

test "parse variable declaration" {
    var parser = Parser.init(std.testing.allocator, "let x = 42;");
    defer parser.deinit();

    const result = parser.parse() catch {
        try std.testing.expect(false);
        return;
    };

    try std.testing.expect(result != null_node);
    try std.testing.expect(!parser.hasErrors());
}

test "parse function declaration" {
    var parser = Parser.init(std.testing.allocator,
        \\function add(a, b) {
        \\  return a + b;
        \\}
    );
    defer parser.deinit();

    const result = parser.parse() catch {
        try std.testing.expect(false);
        return;
    };

    try std.testing.expect(result != null_node);
    try std.testing.expect(!parser.hasErrors());
}

test "parse arrow function" {
    var parser = Parser.init(std.testing.allocator, "const f = (x) => x * 2;");
    defer parser.deinit();

    const result = parser.parse() catch {
        try std.testing.expect(false);
        return;
    };

    try std.testing.expect(result != null_node);
    try std.testing.expect(!parser.hasErrors());
}

test "parse template literal" {
    var parser = Parser.init(std.testing.allocator, "const s = `hello ${name}!`;");
    defer parser.deinit();

    const result = parser.parse() catch {
        try std.testing.expect(false);
        return;
    };

    try std.testing.expect(result != null_node);
    try std.testing.expect(!parser.hasErrors());
}

test "parse object literal" {
    var parser = Parser.init(std.testing.allocator, "const obj = { x: 1, y: 2 };");
    defer parser.deinit();

    const result = parser.parse() catch {
        try std.testing.expect(false);
        return;
    };

    try std.testing.expect(result != null_node);
    try std.testing.expect(!parser.hasErrors());
}

test "parse class" {
    var parser = Parser.init(std.testing.allocator,
        \\class Foo {
        \\  constructor(x) {
        \\    this.x = x;
        \\  }
        \\  getX() {
        \\    return this.x;
        \\  }
        \\}
    );
    defer parser.deinit();

    const result = parser.parse() catch {
        try std.testing.expect(false);
        return;
    };

    try std.testing.expect(result != null_node);
    try std.testing.expect(!parser.hasErrors());
}

test "parse closure creates upvalue" {
    var parser = Parser.init(std.testing.allocator,
        \\function outer(x) {
        \\  return function inner() {
        \\    return x;
        \\  };
        \\}
    );
    defer parser.deinit();

    const result = parser.parse() catch {
        try std.testing.expect(false);
        return;
    };

    try std.testing.expect(result != null_node);
    try std.testing.expect(!parser.hasErrors());

    // Verify upvalue was created in one of the inner function scopes
    // Scope 0 = global, scope 1 = outer function, scope 2 = outer block, scope 3 = inner function
    var found_upvalue = false;
    for (parser.scopes.scopes.items) |scope| {
        if (scope.upvalues.items.len > 0) {
            found_upvalue = true;
            break;
        }
    }
    try std.testing.expect(found_upvalue);
}

test "parse object destructuring" {
    const allocator = std.testing.allocator;
    const source =
        \\const { name, age } = obj;
        \\const { x: localX, y = 10 } = point;
    ;

    var parser = Parser.init(allocator, source);
    defer parser.deinit();

    const result = parser.parse() catch {
        try std.testing.expect(false);
        return;
    };

    try std.testing.expect(result != null_node);
    try std.testing.expect(!parser.hasErrors());
}

test "parse array destructuring" {
    const allocator = std.testing.allocator;
    const source =
        \\const [a, b, c] = arr;
        \\let [x, , y] = [1, 2, 3];
    ;

    var parser = Parser.init(allocator, source);
    defer parser.deinit();

    const result = parser.parse() catch {
        try std.testing.expect(false);
        return;
    };

    try std.testing.expect(result != null_node);
    try std.testing.expect(!parser.hasErrors());
}
