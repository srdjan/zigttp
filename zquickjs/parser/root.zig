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
//!   // Parse source code
//!   var p = parser.JsParser.init(allocator, source);
//!   defer p.deinit();
//!
//!   const ast = try p.parse();
//!
//!   // Generate bytecode
//!   var gen = parser.CodeGen.init(allocator, &ast.nodes, &ast.constants, &ast.scopes);
//!   defer gen.deinit();
//!
//!   const bytecode = try gen.generate(ast.root);

const std = @import("std");

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
    const root = p.parseProgram() catch {
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

test "root module imports" {
    // Just verify imports work
    _ = Token;
    _ = Tokenizer;
    _ = Node;
    _ = JsParser;
    _ = CodeGen;
}
