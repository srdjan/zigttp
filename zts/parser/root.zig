//! New Parser Module - Two-pass architecture with scope-aware IR
//!
//! This module provides a modern JavaScript parser with:
//! - Proper closure/upvalue support for nested functions
//! - Full template literal interpolation
//! - Unified JSX parsing (no separate preprocessing)
//! - Better error messages with source locations
//!
//! Usage:
//!   const parser = @import("parser/root.zig");
//!
//!   // Parse source code (new API)
//!   var p = parser.JsParser.init(allocator, source);
//!   defer p.deinit();
//!   const ast = try p.parse();
//!
//!   // Or use legacy API for compatibility with zruntime.zig:
//!   var p = parser.Parser.init(allocator, source, &strings, &atoms);
//!   defer p.deinit();
//!   const bytecode = try p.parse();
//!   // Access: p.max_local_count, p.constants.items

const std = @import("std");

// Import from parent for compatibility types
const bytecode = @import("../bytecode.zig");
const value = @import("../value.zig");
const string = @import("../string.zig");
const context = @import("../context.zig");

// Re-export all parser components
pub const Token = @import("token.zig").Token;
pub const TokenType = @import("token.zig").TokenType;
pub const SourceLocation = @import("token.zig").SourceLocation;

pub const Tokenizer = @import("tokenizer.zig").Tokenizer;

pub const Node = @import("ir.zig").Node;
pub const NodeTag = @import("ir.zig").NodeTag;
pub const NodeIndex = @import("ir.zig").NodeIndex;
pub const NodeList = @import("ir.zig").NodeList;
pub const ConstantPool = @import("ir.zig").ConstantPool;
pub const BinaryOp = @import("ir.zig").BinaryOp;
pub const UnaryOp = @import("ir.zig").UnaryOp;

pub const ScopeAnalyzer = @import("scope.zig").ScopeAnalyzer;
pub const Scope = @import("scope.zig").Scope;
pub const Binding = @import("scope.zig").Binding;
pub const Upvalue = @import("scope.zig").Upvalue;

pub const ErrorList = @import("error.zig").ErrorList;
pub const ErrorBuilder = @import("error.zig").ErrorBuilder;
pub const ParseError = @import("error.zig").ParseError;
pub const ErrorKind = @import("error.zig").ErrorKind;

pub const JsParser = @import("parse.zig").Parser;
pub const CodeGen = @import("codegen.zig").CodeGen;

/// Parse options
pub const ParseOptions = struct {
    jsx_enabled: bool = false,
    module_mode: bool = false,
    strict_mode: bool = true,
};

/// Parse result containing bytecode and any errors
pub const ParseResult = struct {
    nodes: NodeList,
    constants: ConstantPool,
    scopes: ScopeAnalyzer,
    errors: ErrorList,
    root: NodeIndex,

    pub fn deinit(self: *ParseResult) void {
        self.nodes.deinit();
        self.constants.deinit();
        self.scopes.deinit();
        self.errors.deinit();
    }

    pub fn hasErrors(self: *const ParseResult) bool {
        return self.errors.hasErrors();
    }
};

/// High-level parse function
pub fn parse(
    allocator: std.mem.Allocator,
    source: []const u8,
    options: ParseOptions,
) !ParseResult {
    _ = options; // TODO: Use options

    var p = JsParser.init(allocator, source);
    const root = p.parse() catch {
        return ParseResult{
            .nodes = p.nodes,
            .constants = p.constants,
            .scopes = p.scopes,
            .errors = p.errors,
            .root = @import("ir.zig").null_node,
        };
    };

    return ParseResult{
        .nodes = p.nodes,
        .constants = p.constants,
        .scopes = p.scopes,
        .errors = p.errors,
        .root = root,
    };
}

// ============================================================================
// Legacy Parser API - Compatible with zruntime.zig
// ============================================================================

/// Legacy Parser wrapper that provides the old API for zruntime.zig compatibility
/// Usage:
///   var p = Parser.init(allocator, source, &strings, &atoms);
///   defer p.deinit();
///   const bytecode = try p.parse();
///   // Access: p.max_local_count, p.constants.items
pub const Parser = struct {
    allocator: std.mem.Allocator,
    source: []const u8,

    // Internal parser and codegen state
    js_parser: JsParser,
    code_gen: ?CodeGen,

    // Output fields for zruntime.zig compatibility
    max_local_count: u8,
    constants: ConstantsList,

    // Strings/atoms kept for API compatibility (not used by new parser)
    strings: *string.StringTable,
    atoms: ?*context.AtomTable,

    /// Wrapper for constants to provide .items interface
    pub const ConstantsList = struct {
        items: []const value.JSValue,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        source: []const u8,
        strings: *string.StringTable,
        atoms: ?*context.AtomTable,
    ) Parser {
        var p = Parser{
            .allocator = allocator,
            .source = source,
            .js_parser = JsParser.init(allocator, source),
            .code_gen = null,
            .max_local_count = 0,
            .constants = .{ .items = &.{} },
            .strings = strings,
            .atoms = atoms,
        };
        if (atoms) |atom_table| {
            p.js_parser.setAtomTable(atom_table);
        }
        return p;
    }

    /// Enable JSX parsing mode
    pub fn enableJsx(self: *Parser) void {
        self.js_parser.tokenizer.enableJsx();
    }

    pub fn deinit(self: *Parser) void {
        if (self.code_gen) |*cg| {
            cg.deinit();
        }
        self.js_parser.deinit();
    }

    /// Parse and generate bytecode, returns bytecode slice
    pub fn parse(self: *Parser) ![]const u8 {
        // Parse to IR
        const root = try self.js_parser.parse();

        // Generate bytecode with string table and atoms for proper string/property handling
        self.code_gen = CodeGen.initWithStrings(
            self.allocator,
            &self.js_parser.nodes,
            &self.js_parser.constants,
            &self.js_parser.scopes,
            self.strings,
            self.atoms,
        );

        const func_bc = try self.code_gen.?.generate(root);

        // Update compatibility fields
        self.max_local_count = @intCast(func_bc.local_count);
        self.constants = .{ .items = func_bc.constants };

        return func_bc.code;
    }
};

test "root module imports" {
    // Just verify imports work
    _ = Token;
    _ = Tokenizer;
    _ = Node;
    _ = JsParser;
    _ = CodeGen;
}

test "legacy Parser API" {
    const allocator = std.testing.allocator;
    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    var p = Parser.init(allocator, "let x = 1;", &strings, null);
    defer p.deinit();

    const bytecode_data = try p.parse();
    try std.testing.expect(bytecode_data.len > 0);
    // Global variables don't need local slots - they use put_global
    // max_local_count is 0 for top-level code with only global vars
}

test "JSX parsing with enableJsx" {
    const allocator = std.testing.allocator;
    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    // Simple JSX element - use lowercase to avoid component detection
    var p = Parser.init(allocator, "let x = <div>hello</div>;", &strings, null);
    defer p.deinit();

    p.enableJsx();
    const bytecode_data = try p.parse();
    try std.testing.expect(bytecode_data.len > 0);

    // Check constants contain the text - find by content
    const p_constants = &p.js_parser.constants;
    var found_div = false;
    var found_hello = false;
    for (p_constants.strings.items) |s| {
        if (std.mem.eql(u8, s, "div")) found_div = true;
        if (std.mem.eql(u8, s, "hello")) found_hello = true;
    }
    try std.testing.expect(found_div);
    try std.testing.expect(found_hello);
}

test "JSX rendering integration" {
    const allocator = std.testing.allocator;
    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    // Parse and compile JSX
    var p = Parser.init(allocator, "let x = <div>hello</div>;", &strings, null);
    defer p.deinit();

    p.enableJsx();
    const bytecode_data = try p.parse();

    // Should not have parsing errors
    try std.testing.expect(!p.js_parser.errors.hasErrors());

    // Check that bytecode contains expected opcodes (not just ret_undefined)
    try std.testing.expect(bytecode_data.len > 5);

    // Check nodes include jsx_element
    var found_jsx_element = false;
    for (p.js_parser.nodes.nodes.items) |node| {
        if (node.tag == .jsx_element) found_jsx_element = true;
    }
    try std.testing.expect(found_jsx_element);
}

test "JSX parsing preserves text with punctuation" {
    const allocator = std.testing.allocator;
    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    const source = "let x = <div><span>GET /api/health</span> - ok</div>;";
    var p = Parser.init(allocator, source, &strings, null);
    defer p.deinit();
    p.enableJsx();

    _ = try p.parse();
    try std.testing.expect(!p.js_parser.errors.hasErrors());

    var found_text = false;
    for (p.js_parser.constants.strings.items) |s| {
        if (std.mem.eql(u8, s, "GET /api/health")) found_text = true;
    }
    try std.testing.expect(found_text);
}

test "JSX parsing reports malformed JSX" {
    const allocator = std.testing.allocator;
    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    const source = "let x = <div><span></div>;";
    var p = Parser.init(allocator, source, &strings, null);
    defer p.deinit();
    p.enableJsx();

    _ = p.parse() catch {};
    try std.testing.expect(p.js_parser.errors.hasErrors());
}

test "var keyword is rejected with helpful error" {
    const allocator = std.testing.allocator;
    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    const source = "var x = 1;";
    var p = Parser.init(allocator, source, &strings, null);
    defer p.deinit();

    _ = p.parse() catch {};

    // Should have an error
    try std.testing.expect(p.js_parser.errors.hasErrors());

    // Error should mention 'var' and suggest 'let' or 'const'
    const errors = p.js_parser.errors.getErrors();
    try std.testing.expect(errors.len > 0);
    const err = errors[0];
    try std.testing.expect(std.mem.indexOf(u8, err.message, "var") != null);
    try std.testing.expect(std.mem.indexOf(u8, err.message, "let") != null);
}

test "postfix increment is rejected with helpful error" {
    const allocator = std.testing.allocator;
    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    const source = "let x = 0; x++;";
    var p = Parser.init(allocator, source, &strings, null);
    defer p.deinit();

    _ = p.parse() catch {};

    // Should have an error about postfix increment
    try std.testing.expect(p.js_parser.errors.hasErrors());
    const errors = p.js_parser.errors.getErrors();
    try std.testing.expect(errors.len > 0);
    const err = errors[0];
    // Message should mention x++ and suggest x = x + 1
    try std.testing.expect(std.mem.indexOf(u8, err.message, "x++") != null);
    try std.testing.expect(std.mem.indexOf(u8, err.message, "x = x + 1") != null);
}

test "prefix increment is rejected with helpful error" {
    const allocator = std.testing.allocator;
    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    const source = "let x = 0; ++x;";
    var p = Parser.init(allocator, source, &strings, null);
    defer p.deinit();

    _ = p.parse() catch {};

    // Should have an error about prefix increment
    try std.testing.expect(p.js_parser.errors.hasErrors());
    const errors = p.js_parser.errors.getErrors();
    try std.testing.expect(errors.len > 0);
    const err = errors[0];
    // Message should mention ++x and suggest x = x + 1
    try std.testing.expect(std.mem.indexOf(u8, err.message, "++x") != null);
    try std.testing.expect(std.mem.indexOf(u8, err.message, "x = x + 1") != null);
}
