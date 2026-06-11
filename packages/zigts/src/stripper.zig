//! TypeScript/TSX Type Stripper
//!
//! Removes TypeScript type annotations from source code, producing
//! JavaScript that the zts parser can handle. Preserves line/column
//! positions by replacing stripped spans with spaces.
//!
//! Supported:
//! - type/interface declarations (stripped entirely)
//! - import type / export type (stripped entirely)
//! - Variable/param/return annotations (: Type)
//! - as / satisfies assertions
//! - Generic parameters on functions/types
//!
//! Unsupported (errors):
//! - angle-bracket assertions in TSX (<T>expr)
//! - `any` type annotations
//!
//! Note: enum, namespace, implements, decorators, access modifiers, class,
//! and abstract class are all handled by the parser for consistent error
//! messages across .ts and .js files (see zts/parser/parse.zig)

const std = @import("std");
const builtin = @import("builtin");
const comptime_eval = @import("comptime.zig");
const type_map_mod = @import("type_map.zig");
pub const TypeMap = type_map_mod.TypeMap;
pub const TypeMapEntry = type_map_mod.TypeMapEntry;
pub const TypeMapKind = type_map_mod.TypeMapKind;

pub const StripError = error{
    UnsupportedAngleBracketAssertion,
    UnsupportedAnyType,
    UnclosedTypeAnnotation,
    UnclosedGeneric,
    UnterminatedString,
    UnterminatedComment,
    OutOfMemory,
    ComptimeEvaluationFailed,
    /// 'as' type assertion is not supported
    UnsupportedAsAssertion,
    /// 'satisfies' type assertion is not supported
    UnsupportedSatisfiesAssertion,
};

/// Kind of unsupported-TypeScript construct rejected by the stripper. Each
/// kind maps to a fixed remediation message; `json_diagnostics` projects it
/// onto a ZTS error code so `--json` consumers see a structured diagnostic.
pub const StripDiagnosticKind = enum {
    any_type,
    as_assertion,
    satisfies_assertion,

    pub fn message(self: StripDiagnosticKind) []const u8 {
        return switch (self) {
            .any_type => "'any' type is not supported; use specific types (string, number, object) or union types instead",
            .as_assertion => "'as' type assertion is not supported; use type-safe patterns instead",
            .satisfies_assertion => "'satisfies' type assertion is not supported; use type-safe patterns instead",
        };
    }
};

/// Structured location for an unsupported-TypeScript rejection. Populated
/// through `StripOptions.diagnostic_out` so `--json` consumers and the
/// `zigts expert` agent see the same line/column the std.log path reports,
/// rather than a bare `error.StripFailed`.
pub const StripDiagnostic = struct {
    line: u32,
    column: u32,
    kind: StripDiagnosticKind,
};

pub const StripResult = struct {
    code: []const u8,
    allocator: std.mem.Allocator,
    /// Type annotations extracted during stripping.
    type_map: TypeMap,
    /// Recovered type-assertion diagnostics, populated only under
    /// `StripOptions.collect_all_diagnostics`. Owned by this result. Empty in
    /// the default abort-on-first-rejection mode (those still surface as a
    /// `StripError` instead).
    diagnostics: []const StripDiagnostic = &.{},

    pub fn deinit(self: *StripResult) void {
        @constCast(&self.type_map).deinit(self.allocator);
        self.allocator.free(self.code);
        if (self.diagnostics.len > 0) self.allocator.free(self.diagnostics);
    }
};

/// Environment for comptime evaluation
pub const ComptimeEnv = struct {
    /// Environment variables for Env.* lookups
    env_vars: ?*const std.StringHashMap([]const u8) = null,
    /// Build metadata
    build_time: ?[]const u8 = null,
    git_commit: ?[]const u8 = null,
    version: ?[]const u8 = null,
};

pub const StripOptions = struct {
    /// TSX mode: disallow angle-bracket assertions, preserve JSX
    tsx_mode: bool = false,
    /// Enable comptime() expression evaluation
    enable_comptime: bool = false,
    /// Environment for comptime evaluation
    comptime_env: ?ComptimeEnv = null,
    /// Emit unsupported-feature diagnostics to std.log
    /// Disabled by default in tests to avoid expected-error log failures.
    report_errors: bool = !builtin.is_test,
    /// When set, the stripper writes the line/column/kind of the first
    /// unsupported-feature rejection here before returning the StripError.
    /// Left null for OOM and other location-free failures.
    diagnostic_out: ?*?StripDiagnostic = null,
    /// When true, type-assertion rejections (`as` / `satisfies` / `any`) do
    /// NOT abort the strip: each site is recorded into `StripResult.diagnostics`
    /// and stripping continues, so a single pass surfaces EVERY offending site
    /// instead of just the first. The analysis path (`runCheckOnly`,
    /// `edit-simulate`) opts in so the agent fixes all casts in one round-trip;
    /// compile/verify/runtime paths leave it off so an `as` still fails hard.
    /// The recovered code is diagnostics-only and must not be parsed as-is.
    collect_all_diagnostics: bool = false,
};

/// Strip TypeScript types from source code
pub fn strip(allocator: std.mem.Allocator, source: []const u8, options: StripOptions) StripError!StripResult {
    var stripper = Stripper.init(allocator, source, options);
    errdefer stripper.output.deinit(allocator);
    errdefer stripper.diagnostics.deinit(allocator);
    defer stripper.brace_stack.deinit(allocator);
    return stripper.strip();
}

// ============================================================================
// Stripper
// ============================================================================

const Stripper = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    pos: usize,
    output: std.ArrayList(u8),

    // Configuration
    tsx_mode: bool,
    enable_comptime: bool,
    comptime_env: ?ComptimeEnv,
    report_errors: bool,
    diagnostic_out: ?*?StripDiagnostic,
    collect_all_diagnostics: bool,
    /// All recorded type-assertion diagnostics. In abort mode only the first is
    /// recorded before the StripError; in collect mode every site lands here.
    diagnostics: std.ArrayListUnmanaged(StripDiagnostic),

    // State
    line: u32,
    col: u32,

    // Context tracking for smart colon handling
    // When true, colons are for expressions (object literals), not types
    in_expression: bool,
    // True when inside an import declaration (import { ... } from "...";)
    // Prevents 'as' from being treated as a type assertion (it's import aliasing)
    in_import: bool,
    // Stack of `in_expression` values captured at each `{`; popped on the
    // matching `}`. Without this, the closing `}` of a property-value
    // object literal would clobber the parent context's `in_expression`
    // to false, causing the next sibling property colon to be mis-treated
    // as a type annotation.
    brace_stack: std.ArrayListUnmanaged(bool),
    // True between a `const`/`let`/`var` keyword and the binding it introduces.
    // A `{` seen while this holds opens a destructuring pattern, where property
    // colons are renames (`{ a: localName }`), never type annotations.
    expect_binding: bool,

    // TypeMap: records type annotations for downstream type checking
    type_map: TypeMap,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, source: []const u8, options: StripOptions) Self {
        return .{
            .allocator = allocator,
            .source = source,
            .pos = 0,
            .output = .empty,
            .tsx_mode = options.tsx_mode,
            .enable_comptime = options.enable_comptime,
            .comptime_env = options.comptime_env,
            .report_errors = options.report_errors,
            .diagnostic_out = options.diagnostic_out,
            .collect_all_diagnostics = options.collect_all_diagnostics,
            .diagnostics = .empty,
            .line = 1,
            .col = 1,
            .in_expression = false,
            .in_import = false,
            .brace_stack = .empty,
            .expect_binding = false,
            .type_map = TypeMap.init(source),
        };
    }

    pub fn strip(self: *Self) StripError!StripResult {
        while (self.pos < self.source.len) {
            // Check for unsupported constructs at statement boundaries
            if (self.isAtStatementStart()) {
                if (try self.tryStripTypeDeclaration()) continue;
                if (try self.tryStripImportType()) continue;
                if (try self.tryStripExportType()) continue;
            }

            // Process next token
            try self.processToken();
        }

        const code = self.output.toOwnedSlice(self.allocator) catch return StripError.OutOfMemory;
        errdefer self.allocator.free(code);
        const diags = self.diagnostics.toOwnedSlice(self.allocator) catch return StripError.OutOfMemory;
        return StripResult{
            .code = code,
            .allocator = self.allocator,
            .type_map = self.type_map,
            .diagnostics = diags,
        };
    }

    // ========================================================================
    // Token Processing
    // ========================================================================

    fn processToken(self: *Self) StripError!void {
        if (self.pos >= self.source.len) return;

        const start = self.pos;
        const c = self.source[self.pos];

        // Whitespace - pass through
        if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
            if (c == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
            self.output.appendSlice(self.allocator, self.source[start..self.pos]) catch return StripError.OutOfMemory;
            return;
        }

        // Comments - pass through
        if (c == '/' and self.pos + 1 < self.source.len) {
            const next = self.source[self.pos + 1];
            if (next == '/') {
                self.skipLineComment();
                self.output.appendSlice(self.allocator, self.source[start..self.pos]) catch return StripError.OutOfMemory;
                return;
            }
            if (next == '*') {
                try self.skipBlockComment();
                self.output.appendSlice(self.allocator, self.source[start..self.pos]) catch return StripError.OutOfMemory;
                return;
            }
        }

        // Strings and template literals - pass through
        if (c == '"' or c == '\'' or c == '`') {
            try self.skipString(c);
            self.output.appendSlice(self.allocator, self.source[start..self.pos]) catch return StripError.OutOfMemory;
            return;
        }

        // Numbers - pass through
        if (std.ascii.isDigit(c) or (c == '.' and self.pos + 1 < self.source.len and std.ascii.isDigit(self.source[self.pos + 1]))) {
            self.skipNumber();
            self.output.appendSlice(self.allocator, self.source[start..self.pos]) catch return StripError.OutOfMemory;
            return;
        }

        // Identifiers
        if (isIdentifierStart(c)) {
            const ident = self.scanIdentifier();

            // Any identifier other than const/let/var ends a pending binding
            // position; the keyword branch below re-arms it when applicable.
            self.expect_binding = false;

            // Check for comptime() expression
            if (self.enable_comptime and std.mem.eql(u8, ident, "comptime")) {
                if (try self.tryEvaluateComptime(start)) return;
            }

            // Check for 'as' or 'satisfies' after expression
            // Skip 'as' inside import specifiers (import { x as y } is aliasing, not assertion)
            if (std.mem.eql(u8, ident, "as") and !self.in_import) {
                if (try self.tryStripAsAssertion(start)) return;
            }
            if (std.mem.eql(u8, ident, "satisfies")) {
                if (try self.tryStripSatisfiesAssertion(start)) return;
            }

            // Check for function declaration - handle generics inline
            if (std.mem.eql(u8, ident, "function")) {
                self.output.appendSlice(self.allocator, ident) catch return StripError.OutOfMemory;
                try self.handleFunctionDeclaration();
                return;
            }

            // Track import context: 'as' inside import specifiers is aliasing, not assertion
            if (std.mem.eql(u8, ident, "import")) {
                self.in_import = true;
            }

            // Keywords that start expressions
            if (std.mem.eql(u8, ident, "return") or std.mem.eql(u8, ident, "throw") or
                std.mem.eql(u8, ident, "new") or std.mem.eql(u8, ident, "typeof") or
                std.mem.eql(u8, ident, "delete") or std.mem.eql(u8, ident, "void") or
                std.mem.eql(u8, ident, "await") or std.mem.eql(u8, ident, "yield") or
                std.mem.eql(u8, ident, "when"))
            {
                self.in_expression = true;
            }
            // Keywords that end expressions
            else if (std.mem.eql(u8, ident, "let") or std.mem.eql(u8, ident, "const") or
                std.mem.eql(u8, ident, "var"))
            {
                self.in_expression = false;
                self.expect_binding = true;
            }

            self.output.appendSlice(self.allocator, ident) catch return StripError.OutOfMemory;
            return;
        }

        // Colon - might be type annotation
        if (c == ':') {
            if (try self.tryStripColonAnnotation()) return;
            self.pos += 1;
            self.col += 1;
            self.output.append(self.allocator, ':') catch return StripError.OutOfMemory;
            return;
        }

        // Angle bracket - might be generic params
        if (c == '<') {
            if (try self.tryStripGenericParams()) return;
            self.pos += 1;
            self.col += 1;
            self.output.append(self.allocator, '<') catch return StripError.OutOfMemory;
            return;
        }

        // Check for arrow function: = (...): Type => or = <T>(...): Type =>
        if (c == '=') {
            self.pos += 1;
            self.col += 1;
            self.output.append(self.allocator, c) catch return StripError.OutOfMemory;

            // Copy whitespace
            const ws_start = self.pos;
            self.skipWhitespaceTracked();
            self.output.appendSlice(self.allocator, self.source[ws_start..self.pos]) catch return StripError.OutOfMemory;

            // Check for generic arrow: = <T>(...)
            if (self.pos < self.source.len and self.source[self.pos] == '<' and !self.tsx_mode) {
                if (self.looksLikeGenericArrow()) {
                    const generic_start = self.pos;
                    if (self.skipBalancedAngles()) {
                        // Record generic params (content inside angle brackets)
                        self.recordTypeAnnotation(.generic_params, generic_start + 1, self.pos - 1, 0, 0);
                        self.blankSpan(generic_start, self.pos);
                        // Copy whitespace after generics
                        const ws2_start = self.pos;
                        self.skipWhitespaceTracked();
                        self.output.appendSlice(self.allocator, self.source[ws2_start..self.pos]) catch return StripError.OutOfMemory;
                    }
                }
            }

            // Check for arrow function params
            if (self.pos < self.source.len and self.source[self.pos] == '(') {
                if (self.looksLikeArrowFunction()) {
                    try self.handleArrowFunction();
                }
            }
            self.in_expression = true;
            return;
        }

        // Arrow function in expression position (call argument, array element,
        // default value, etc.). The `=`-anchored path above only catches
        // `= (params) =>`; a typed arrow passed as an argument, e.g.
        // `nums.toSorted((a: number, b: number) => a - b)`, reaches here as a
        // bare `(` and its param-type colons must be stripped or the parser
        // rejects them (ENG-15). The strict detector requires a confirming
        // `=>` so a ternary branch like `cond ? (a) : (b)` is not mistaken for
        // an arrow.
        if (c == '(' and self.looksLikeArrowFunctionStrict()) {
            try self.handleArrowFunction();
            self.in_expression = true;
            return;
        }

        // Track expression context based on punctuators
        switch (c) {
            ';' => {
                self.in_expression = false; // Statement end
                self.in_import = false; // Import statement ended
            },
            '{' => {
                // Remember the expression context the brace was opened in;
                // the matching `}` restores it.
                self.brace_stack.append(self.allocator, self.in_expression) catch return StripError.OutOfMemory;
                // Inside the brace, keep current context: a `{` after `=`, `(`, `,`,
                // `[`, or `:` is a value-position object literal where property
                // colons stay expressions; a `{` after `)` or a statement is a
                // block where labels and `let`/`const` again drive the flag.
                // Exception: a `{` right after `const`/`let`/`var` opens a
                // destructuring pattern whose property colons are renames, so
                // colons inside must stay in expression context.
                if (self.expect_binding) {
                    self.in_expression = true;
                }
            },
            '}' => {
                if (self.brace_stack.pop()) |prev| {
                    self.in_expression = prev;
                } else {
                    // Unbalanced `}` - keep prior behaviour so error paths
                    // still flow through the parser.
                    self.in_expression = false;
                }
            },
            '(' => self.in_expression = true, // Contents of parens is expression
            ')' => {}, // Keep context
            '[' => self.in_expression = true, // Array literal
            ']' => {}, // Keep context
            else => {},
        }

        // Any punctuator ends a pending binding position (the `{` case above
        // has already consumed the flag for destructuring patterns).
        self.expect_binding = false;

        // Other punctuators - pass through
        self.pos += 1;
        self.col += 1;
        self.output.append(self.allocator, c) catch return StripError.OutOfMemory;
    }

    // ========================================================================
    // Function Declaration Handling (for generics)
    // ========================================================================

    fn handleFunctionDeclaration(self: *Self) StripError!void {
        // We've already output "function"
        // Now handle: [name]<generics>(params): returnType { ... }

        // Copy whitespace
        const ws_start = self.pos;
        self.skipWhitespaceTracked();
        self.output.appendSlice(self.allocator, self.source[ws_start..self.pos]) catch return StripError.OutOfMemory;

        // Copy function name (optional for function expressions)
        var fn_name_start: usize = 0;
        var fn_name_end: usize = 0;
        if (self.pos < self.source.len and isIdentifierStart(self.source[self.pos])) {
            fn_name_start = self.pos;
            const name = self.scanIdentifier();
            fn_name_end = self.pos;
            self.output.appendSlice(self.allocator, name) catch return StripError.OutOfMemory;
        }

        // Check for generic params <T, U>
        if (self.pos < self.source.len and self.source[self.pos] == '<') {
            const generic_start = self.pos;
            if (self.skipBalancedAngles()) {
                // Record generic params in TypeMap (inside the angle brackets)
                self.recordTypeAnnotation(.generic_params, generic_start + 1, self.pos - 1, fn_name_start, fn_name_end);
                // Blank the generic params
                self.blankSpan(generic_start, self.pos);
            }
        }

        // Handle parameter list with type stripping
        // Preserve whitespace before (
        const ws2_start = self.pos;
        self.skipWhitespaceTracked();
        self.output.appendSlice(self.allocator, self.source[ws2_start..self.pos]) catch return StripError.OutOfMemory;

        if (self.pos < self.source.len and self.source[self.pos] == '(') {
            try self.handleFunctionParams();
        }

        // Handle return type annotation
        const ws3_start = self.pos;
        self.skipWhitespaceTracked();

        if (self.pos < self.source.len and self.source[self.pos] == ':') {
            self.pos += 1;
            self.col += 1;
            self.skipWhitespaceTracked();

            if (self.looksLikeTypeStart()) {
                const ret_type_start = self.pos;
                try self.skipTypeExpressionUntilDelimiter(&[_]u8{ '{', ';', '=' }, true);
                const ret_type_end = self.pos;
                // Skip whitespace after type
                self.skipWhitespaceTracked();
                const kind = classifyReturnType(self.source[ret_type_start..ret_type_end]);
                self.recordTypeAnnotation(kind, ret_type_start, ret_type_end, fn_name_start, fn_name_end);
                // Blank from ws3_start (includes whitespace before ':') to preserve output length.
                self.blankSpan(ws3_start, self.pos);
            }
        } else {
            // No return type - output the whitespace we skipped
            self.output.appendSlice(self.allocator, self.source[ws3_start..self.pos]) catch return StripError.OutOfMemory;
        }

        // Body will be handled by normal token processing
        self.in_expression = false;
    }

    fn handleFunctionParams(self: *Self) StripError!void {
        // We're at '('
        self.output.append(self.allocator, '(') catch return StripError.OutOfMemory;
        self.pos += 1;
        self.col += 1;

        var paren_depth: u16 = 1;
        // Track the last identifier position for param name recording
        var last_ident_start: usize = 0;
        var last_ident_end: usize = 0;

        while (self.pos < self.source.len and paren_depth > 0) {
            const c = self.source[self.pos];

            if (c == '(') {
                paren_depth += 1;
                self.output.append(self.allocator, c) catch return StripError.OutOfMemory;
                self.pos += 1;
                self.col += 1;
                continue;
            }

            if (c == ')') {
                paren_depth -= 1;
                self.output.append(self.allocator, c) catch return StripError.OutOfMemory;
                self.pos += 1;
                self.col += 1;
                continue;
            }

            // Track identifiers at depth 1 for param name recording.
            // Only record the FIRST identifier in each parameter (before the colon).
            if (isIdentifierStart(c) and paren_depth == 1 and last_ident_start == 0) {
                last_ident_start = self.pos;
                // Peek ahead to find end of identifier without consuming
                var peek = self.pos;
                while (peek < self.source.len and isIdentifierContinue(self.source[peek])) {
                    peek += 1;
                }
                last_ident_end = peek;
            }

            // Reset ident tracking on comma (new param)
            if (c == ',' and paren_depth == 1) {
                last_ident_start = 0;
                last_ident_end = 0;
            }

            // Optional parameter marker in TypeScript: `name?: Type`
            if (c == '?' and paren_depth == 1) {
                const question_pos = self.pos;
                self.pos += 1;
                self.col += 1;
                self.skipWhitespaceTracked();
                if (self.pos < self.source.len and self.source[self.pos] == ':') {
                    self.blankSpan(question_pos, self.pos);
                    continue;
                }
                self.pos = question_pos;
                self.col -= 1;
            }

            // Strip type annotations in params
            if (c == ':' and paren_depth == 1) {
                const colon_pos = self.pos;
                self.pos += 1;
                self.col += 1;
                self.skipWhitespaceTracked();

                if (self.looksLikeTypeStart()) {
                    // Check for banned 'any' type
                    try self.checkForAnyType();
                    const type_start = self.pos;
                    // Skip to , or ) at depth 1
                    try self.skipParamType();
                    // Trim trailing whitespace from type text
                    const type_end = trimTrailingWs(self.source, type_start, self.pos);
                    // Record param annotation
                    self.recordTypeAnnotation(.param_annotation, type_start, type_end, last_ident_start, last_ident_end);
                    self.blankSpan(colon_pos, self.pos);
                    continue;
                } else {
                    // Not a type, output the colon
                    self.pos = colon_pos;
                }
            }

            // Handle strings
            if (c == '"' or c == '\'' or c == '`') {
                const start = self.pos;
                try self.skipString(c);
                self.output.appendSlice(self.allocator, self.source[start..self.pos]) catch return StripError.OutOfMemory;
                continue;
            }

            // Handle whitespace
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
                if (c == '\n') {
                    self.line += 1;
                    self.col = 1;
                } else {
                    self.col += 1;
                }
                self.output.append(self.allocator, c) catch return StripError.OutOfMemory;
                self.pos += 1;
                continue;
            }

            // Pass through everything else
            self.output.append(self.allocator, c) catch return StripError.OutOfMemory;
            self.pos += 1;
            self.col += 1;
        }
    }

    fn handleArrowFunction(self: *Self) StripError!void {
        // We're at '(' after '='
        // Handle: (params): ReturnType => body
        try self.handleFunctionParams();

        // Handle optional return type annotation
        const ws_start = self.pos;
        self.skipWhitespaceTracked();

        if (self.pos < self.source.len and self.source[self.pos] == ':') {
            self.pos += 1;
            self.col += 1;
            self.skipWhitespaceTracked();

            if (self.looksLikeTypeStart()) {
                const ret_type_start = self.pos;
                // Skip to => or {
                try self.skipTypeExpressionUntilDelimiter(&[_]u8{'{'}, true);
                const ret_type_end = self.pos;
                // Check for =>
                self.skipWhitespaceTracked();
                if (self.pos + 1 < self.source.len and
                    self.source[self.pos] == '=' and self.source[self.pos + 1] == '>')
                {
                    const arrow_kind = classifyReturnType(self.source[ret_type_start..ret_type_end]);
                    self.recordTypeAnnotation(arrow_kind, ret_type_start, ret_type_end, 0, 0);
                    // Blank just the return type, not the =>
                    self.blankSpan(ws_start, self.pos);
                    return;
                }
            }
        }
        // If no return type, output the whitespace we skipped
        self.output.appendSlice(self.allocator, self.source[ws_start..self.pos]) catch return StripError.OutOfMemory;
    }

    fn looksLikeArrowFunction(self: *Self) bool {
        // Scan ahead to check if this is (params) => or (params): Type =>
        // vs just a parenthesized expression like (foo as number)
        const saved_pos = self.pos;
        const saved_line = self.line;
        const saved_col = self.col;
        defer {
            self.pos = saved_pos;
            self.line = saved_line;
            self.col = saved_col;
        }

        // Skip the opening paren
        self.pos += 1;
        var paren_depth: u16 = 1;

        // Scan to find matching )
        while (self.pos < self.source.len and paren_depth > 0) {
            const c = self.source[self.pos];
            if (c == '(') paren_depth += 1;
            if (c == ')') paren_depth -= 1;
            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch return false;
                continue;
            }
            self.pos += 1;
        }

        // Skip whitespace after )
        while (self.pos < self.source.len and (self.source[self.pos] == ' ' or
            self.source[self.pos] == '\t' or self.source[self.pos] == '\n' or
            self.source[self.pos] == '\r'))
        {
            self.pos += 1;
        }

        // Check for : (return type) or => (arrow)
        if (self.pos >= self.source.len) return false;

        if (self.source[self.pos] == ':') {
            // Has return type annotation - likely arrow function
            return true;
        }

        if (self.pos + 1 < self.source.len and
            self.source[self.pos] == '=' and self.source[self.pos + 1] == '>')
        {
            // Direct arrow - is arrow function
            return true;
        }

        return false;
    }

    /// Like `looksLikeArrowFunction`, but when the params are followed by a
    /// `:` return-type it REQUIRES a confirming `=>` after the type. Used in
    /// expression position (any bare `(`), where the looser bare-`:` heuristic
    /// would misread a ternary branch `cond ? (a) : (b)` as an arrow. Errs
    /// toward false (no strip) on anything ambiguous, so it never corrupts a
    /// non-arrow.
    fn looksLikeArrowFunctionStrict(self: *Self) bool {
        const saved_pos = self.pos;
        const saved_line = self.line;
        const saved_col = self.col;
        defer {
            self.pos = saved_pos;
            self.line = saved_line;
            self.col = saved_col;
        }

        // Scan the parenthesized group to its matching `)`.
        self.pos += 1;
        var paren_depth: u16 = 1;
        while (self.pos < self.source.len and paren_depth > 0) {
            const c = self.source[self.pos];
            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch return false;
                continue;
            }
            if (c == '(') paren_depth += 1;
            if (c == ')') paren_depth -= 1;
            self.pos += 1;
        }
        if (paren_depth != 0) return false;

        // Whitespace after `)`.
        while (self.pos < self.source.len and (self.source[self.pos] == ' ' or
            self.source[self.pos] == '\t' or self.source[self.pos] == '\n' or
            self.source[self.pos] == '\r')) self.pos += 1;
        if (self.pos >= self.source.len) return false;

        // Direct arrow.
        if (self.pos + 1 < self.source.len and self.source[self.pos] == '=' and self.source[self.pos + 1] == '>') {
            return true;
        }

        // Return-type annotation: scan the type at bracket-depth 0 and only
        // accept if a `=>` follows. A depth-0 close bracket or `,`/`;` means we
        // left the construct without reaching an arrow (e.g. the ternary case).
        if (self.source[self.pos] == ':') {
            self.pos += 1;
            var depth: i32 = 0;
            while (self.pos < self.source.len) {
                const c = self.source[self.pos];
                if (c == '"' or c == '\'' or c == '`') {
                    self.skipString(c) catch return false;
                    continue;
                }
                if (c == '(' or c == '[' or c == '{' or c == '<') {
                    depth += 1;
                } else if (c == ')' or c == ']' or c == '}') {
                    if (depth == 0) return false;
                    depth -= 1;
                } else if (c == '>') {
                    if (depth > 0) depth -= 1;
                } else if (depth == 0) {
                    if (c == '=' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '>') return true;
                    if (c == ',' or c == ';') return false;
                }
                self.pos += 1;
            }
            return false;
        }

        return false;
    }

    fn looksLikeGenericArrow(self: *Self) bool {
        // Check if <...> is followed by ( for arrow function. This is a pure
        // probe: skipBalancedAngles advances self.line/col across newlines, so
        // restore all of pos/line/col (not just pos) to keep it side-effect-free.
        const saved_pos = self.pos;
        const saved_line = self.line;
        const saved_col = self.col;
        defer {
            self.pos = saved_pos;
            self.line = saved_line;
            self.col = saved_col;
        }

        // Skip <...>
        if (!self.skipBalancedAngles()) return false;

        // Skip whitespace
        while (self.pos < self.source.len and (self.source[self.pos] == ' ' or
            self.source[self.pos] == '\t' or self.source[self.pos] == '\n'))
        {
            self.pos += 1;
        }

        // Should be followed by (
        return self.pos < self.source.len and self.source[self.pos] == '(';
    }

    fn skipParamType(self: *Self) StripError!void {
        // Skip type expression in parameter until , or )
        var paren_depth: u16 = 0;
        var angle_depth: u16 = 0;
        var bracket_depth: u16 = 0;
        var brace_depth: u16 = 0;

        while (self.pos < self.source.len) {
            const c = self.source[self.pos];

            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch {};
                continue;
            }

            // Comments inside the annotation: skip wholesale so their
            // content cannot unbalance the depth counters below.
            if (c == '/' and self.pos + 1 < self.source.len) {
                const next = self.source[self.pos + 1];
                if (next == '/') {
                    self.skipLineComment();
                    continue;
                }
                if (next == '*') {
                    try self.skipBlockComment();
                    continue;
                }
            }

            // Check identifiers for banned types
            if (isIdentifierStart(c)) {
                try self.checkForAnyType();
            }

            if (c == '(') {
                paren_depth += 1;
            } else if (c == ')') {
                if (paren_depth == 0) return; // End of params
                paren_depth -= 1;
            } else if (c == '<') {
                angle_depth += 1;
            } else if (c == '>') {
                if (angle_depth > 0) angle_depth -= 1;
            } else if (c == '[') {
                bracket_depth += 1;
            } else if (c == ']') {
                if (bracket_depth > 0) bracket_depth -= 1;
            } else if (c == '{') {
                brace_depth += 1;
            } else if (c == '}') {
                if (brace_depth > 0) brace_depth -= 1;
            } else if (c == ',' and paren_depth == 0 and angle_depth == 0 and bracket_depth == 0 and brace_depth == 0) {
                return; // Next param
            } else if (c == '=' and
                paren_depth == 0 and
                angle_depth == 0 and
                !(self.pos + 1 < self.source.len and self.source[self.pos + 1] == '>'))
            {
                return; // Default value
            }

            if (c == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    // ========================================================================
    // Type/Interface Declaration Stripping (Phase 2)
    // ========================================================================

    fn tryStripTypeDeclaration(self: *Self) StripError!bool {
        return self.stripTypeOrInterfaceBody(self.pos, self.line, self.col);
    }

    /// Shared body for stripping [distinct] type/interface declarations.
    /// `span_start` marks where blanking begins (before any `export` prefix).
    fn stripTypeOrInterfaceBody(self: *Self, span_start: usize, span_start_line: u32, span_start_col: u32) StripError!bool {
        // Check for 'distinct', 'type', or 'interface' keyword
        var is_distinct = false;
        const first_kw = self.peekKeyword();
        if (first_kw == null) {
            self.pos = span_start;
            self.line = span_start_line;
            self.col = span_start_col;
            return false;
        }

        if (std.mem.eql(u8, first_kw.?, "distinct")) {
            is_distinct = true;
            self.pos += first_kw.?.len;
            self.col += @intCast(first_kw.?.len);
            self.skipWhitespaceTracked();
        }

        const keyword = if (is_distinct) self.peekKeyword() else first_kw;
        if (keyword == null) {
            self.pos = span_start;
            self.line = span_start_line;
            self.col = span_start_col;
            return false;
        }

        const is_type = std.mem.eql(u8, keyword.?, "type");
        const is_interface = std.mem.eql(u8, keyword.?, "interface");

        if ((!is_type and !is_interface) or (is_distinct and is_interface)) {
            self.pos = span_start;
            self.line = span_start_line;
            self.col = span_start_col;
            return false;
        }

        // Skip the keyword
        self.pos += keyword.?.len;
        self.col += @intCast(keyword.?.len);
        self.skipWhitespaceTracked();

        // Must be followed by identifier
        if (!self.peekIdentifierStart()) {
            self.pos = span_start;
            self.line = span_start_line;
            self.col = span_start_col;
            return false;
        }

        const name_start = self.pos;
        _ = self.scanIdentifier();
        const name_end = self.pos;
        self.skipWhitespaceTracked();

        // Skip optional generic params <T, U>
        var generic_start: usize = 0;
        var generic_end: usize = 0;
        if (self.pos < self.source.len and self.source[self.pos] == '<') {
            generic_start = self.pos;
            if (!self.skipBalancedAngles()) {
                self.pos = span_start;
                self.line = span_start_line;
                self.col = span_start_col;
                return false;
            }
            generic_end = self.pos;
            self.skipWhitespaceTracked();
        }

        // For interface: skip optional 'extends' clause
        if (is_interface) {
            const extends_kw = self.peekKeyword();
            if (extends_kw != null and std.mem.eql(u8, extends_kw.?, "extends")) {
                self.pos += 7; // "extends"
                self.col += 7;
                self.skipWhitespaceTracked();
                try self.skipTypeExpressionUntilDelimiter(&[_]u8{ '{', ';' }, false);
                self.skipWhitespaceTracked();
            }
        }

        var type_body_start: usize = 0;
        var type_body_end: usize = 0;

        if (is_type and self.pos < self.source.len and self.source[self.pos] == '=') {
            self.pos += 1;
            self.col += 1;
            self.skipWhitespaceTracked();
            type_body_start = self.pos;
            try self.skipTypeExpressionUntilDelimiter(&[_]u8{ ';', '\n' }, false);
            type_body_end = self.pos;
        }

        if (is_interface and self.pos < self.source.len and self.source[self.pos] == '{') {
            type_body_start = self.pos;
            self.skipBalancedBraces();
            type_body_end = self.pos;
        }

        self.skipWhitespaceTracked();
        if (self.pos < self.source.len and self.source[self.pos] == ';') {
            self.pos += 1;
            self.col += 1;
        }

        const kind: TypeMapKind = if (is_distinct) .distinct_type else if (is_type) .type_alias else .interface_decl;
        self.recordTypeAnnotation(kind, type_body_start, type_body_end, name_start, name_end);
        if (generic_start != 0) {
            self.recordTypeAnnotation(.generic_params, generic_start, generic_end, name_start, name_end);
        }

        self.blankSpan(span_start, self.pos);
        return true;
    }

    // ========================================================================
    // Import/Export Type Stripping (Phase 3)
    // ========================================================================

    fn tryStripImportType(self: *Self) StripError!bool {
        const saved_pos = self.pos;
        const saved_line = self.line;
        const saved_col = self.col;

        const keyword = self.peekKeyword();
        if (keyword == null or !std.mem.eql(u8, keyword.?, "import")) return false;

        self.pos += 6; // "import"
        self.col += 6;
        self.skipWhitespaceTracked();

        const type_kw = self.peekKeyword();
        if (type_kw == null or !std.mem.eql(u8, type_kw.?, "type")) {
            self.pos = saved_pos;
            self.line = saved_line;
            self.col = saved_col;
            return false;
        }

        // Skip to end of statement
        self.skipToStatementEnd();
        self.blankSpan(saved_pos, self.pos);
        return true;
    }

    fn tryStripExportType(self: *Self) StripError!bool {
        const saved_pos = self.pos;
        const saved_line = self.line;
        const saved_col = self.col;

        const keyword = self.peekKeyword();
        if (keyword == null or !std.mem.eql(u8, keyword.?, "export")) return false;

        self.pos += 6; // "export"
        self.col += 6;
        self.skipWhitespaceTracked();

        // Handle `export type { Foo }` re-exports before delegating.
        const peek_kw = self.peekKeyword();
        if (peek_kw != null and std.mem.eql(u8, peek_kw.?, "type")) {
            var scan = self.pos + 4;
            while (scan < self.source.len and (self.source[scan] == ' ' or self.source[scan] == '\t')) : (scan += 1) {}
            if (scan < self.source.len and self.source[scan] == '{') {
                self.pos += 4;
                self.col += 4;
                self.skipWhitespaceTracked();
                self.skipToStatementEnd();
                self.blankSpan(saved_pos, self.pos);
                return true;
            }
        }

        return self.stripTypeOrInterfaceBody(saved_pos, saved_line, saved_col);
    }

    // ========================================================================
    // Annotation Stripping (Phase 4)
    // ========================================================================

    /// Detect match arm separator: `when { ... }:`
    fn isMatchArmColon(self: *Self) bool {
        const items = self.output.items;
        if (items.len == 0) return false;

        var p = skipBackwardWs(items, items.len);
        if (p == 0 or items[p - 1] != '}') return false;
        p -= 1;

        var depth: u32 = 1;
        while (p > 0 and depth > 0) {
            p -= 1;
            if (items[p] == '}') {
                depth += 1;
            } else if (items[p] == '{') {
                depth -= 1;
            }
        }
        if (depth != 0) return false;

        p = skipBackwardWs(items, p);
        if (p < 4) return false;
        if (!std.mem.eql(u8, items[p - 4 .. p], "when")) return false;
        // "when" must not be part of a longer identifier
        if (p > 4 and isIdentifierContinue(items[p - 5])) return false;

        return true;
    }

    fn skipBackwardWs(items: []const u8, start: usize) usize {
        var p = start;
        while (p > 0 and (items[p - 1] == ' ' or items[p - 1] == '\t' or items[p - 1] == '\n' or items[p - 1] == '\r')) {
            p -= 1;
        }
        return p;
    }

    /// Check if we're at a label colon (identifier at statement start followed by colon).
    fn isLabelColon(self: *Self) bool {
        const items = self.output.items;
        if (items.len == 0) return false;

        // Find the identifier that precedes the colon
        var ident_start = items.len;

        // Skip back through identifier characters
        while (ident_start > 0 and isIdentifierContinue(items[ident_start - 1])) {
            ident_start -= 1;
        }

        // Must have at least one identifier character and start with identifier start char
        if (ident_start >= items.len) return false;
        if (!isIdentifierStart(items[ident_start])) return false;

        // Check if we're at statement start (before the identifier)
        if (ident_start == 0) return true; // Start of file

        // Look at the character before the identifier (skip whitespace)
        var check_pos = ident_start - 1;
        while (check_pos > 0 and (items[check_pos] == ' ' or items[check_pos] == '\t')) {
            check_pos -= 1;
        }

        const before = items[check_pos];
        return before == ';' or before == '{' or before == '}' or before == '\n';
    }

    fn tryStripColonAnnotation(self: *Self) StripError!bool {
        // We're at ':'
        // Skip if we're in expression context (object literals, arrays, etc.)
        if (self.in_expression) {
            return false;
        }

        // Match arm separators: when { ... }: body
        // The } before the colon sets in_expression=false, so we must detect this pattern
        // by scanning backward for a matching when { ... } sequence.
        if (self.isMatchArmColon()) {
            return false;
        }

        // Check if this is a label (identifier at statement start followed by colon)
        // Labels look like: public: foo(); or loop: for (...) {...}
        if (self.isLabelColon()) {
            return false;
        }

        const colon_pos = self.pos;
        const colon_col = self.col;
        const colon_line = self.line;

        self.pos += 1;
        self.col += 1;

        // Skip whitespace after colon
        self.skipWhitespaceTracked();

        // Check if this looks like a type annotation
        // Type annotations are followed by type syntax, not expression syntax
        if (!self.looksLikeTypeStart()) {
            self.pos = colon_pos;
            self.col = colon_col;
            // Restore the line too: skipWhitespaceTracked above may have crossed
            // a newline, and leaving self.line advanced drifts every later
            // source position (proof cards, diagnostics) by those lines.
            self.line = colon_line;
            return false;
        }

        // Check for banned 'any' type
        try self.checkForAnyType();

        // This looks like a type annotation - skip the type
        const type_start = self.pos;
        try self.skipTypeExpressionUntilDelimiter(
            &[_]u8{ ',', ')', ';', '=', '{', '}' },
            false,
        );
        // Trim trailing whitespace from type text
        const type_end = trimTrailingWs(self.source, type_start, self.pos);

        // Check for arrow function
        self.skipWhitespaceTracked();
        if (self.pos + 1 < self.source.len and
            self.source[self.pos] == '=' and self.source[self.pos + 1] == '>')
        {
            // Stop before =>
        }

        // Find the identifier name before the colon in original source
        const name_range = self.findIdentifierBefore(colon_pos);
        self.recordTypeAnnotation(.var_annotation, type_start, type_end, name_range[0], name_range[1]);

        // Blank from colon to current position
        self.blankSpan(colon_pos, self.pos);
        return true;
    }

    fn looksLikeTypeStart(self: *Self) bool {
        if (self.pos >= self.source.len) return false;
        const c = self.source[self.pos];

        // Type starts with: identifier, {, (, [, <, 'string', "string", `template`, typeof, keyof
        if (isIdentifierStart(c)) return true;
        if (c == '{' or c == '(' or c == '[' or c == '<') return true;
        if (c == '\'' or c == '"' or c == '`') return true;

        return false;
    }

    // ========================================================================
    // Assertion Stripping (Phase 5)
    // ========================================================================

    /// Shared tail of the `as`/`satisfies` assertion handlers: record the
    /// rejection, then either recover (collect mode: drop the asserted type and
    /// keep scanning so EVERY later site is reported too - output is
    /// diagnostics-only) or abort with `err`.
    fn rejectTypeAssertion(self: *Self, kind: StripDiagnosticKind, err: StripError) StripError!bool {
        if (self.report_errors) {
            std.log.err("{}:{}: {s}", .{ self.line, self.col, kind.message() });
        }
        self.recordDiagnostic(kind);
        if (self.collect_all_diagnostics) {
            self.skipTypeExpressionUntilDelimiter(&[_]u8{ ';', ',', ')', ']', '}' }, false) catch {};
            return true;
        }
        return err;
    }

    fn tryStripAsAssertion(self: *Self, _: usize) StripError!bool {
        // We just scanned 'as' - check if it's an assertion
        self.skipWhitespaceTracked();
        if (!self.looksLikeTypeStart()) return false;
        return self.rejectTypeAssertion(.as_assertion, StripError.UnsupportedAsAssertion);
    }

    fn tryStripSatisfiesAssertion(self: *Self, _: usize) StripError!bool {
        // We just scanned 'satisfies'
        self.skipWhitespaceTracked();
        if (!self.looksLikeTypeStart()) return false;
        return self.rejectTypeAssertion(.satisfies_assertion, StripError.UnsupportedSatisfiesAssertion);
    }

    // ========================================================================
    // Comptime Expression Evaluation
    // ========================================================================

    fn tryEvaluateComptime(self: *Self, keyword_start: usize) StripError!bool {
        // We just scanned 'comptime', now expect (
        const entry_col = self.col;
        const entry_line = self.line;
        self.skipWhitespaceTracked();

        if (self.pos >= self.source.len or self.source[self.pos] != '(') {
            // Not a comptime() call, restore and treat as regular identifier.
            // Restore col/line too: skipWhitespaceTracked may have crossed a
            // newline, and leaving them advanced drifts later source positions.
            self.pos = keyword_start + 8; // "comptime".len
            self.col = entry_col;
            self.line = entry_line;
            return false;
        }

        // Find the matching closing paren
        self.pos += 1; // skip (
        self.col += 1;

        var paren_depth: u16 = 1;
        const expr_start = self.pos;

        while (self.pos < self.source.len and paren_depth > 0) {
            const c = self.source[self.pos];

            // Handle strings
            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch return StripError.ComptimeEvaluationFailed;
                continue;
            }

            if (c == '(') {
                paren_depth += 1;
            } else if (c == ')') {
                paren_depth -= 1;
            }

            if (c == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }

        if (paren_depth != 0) {
            return StripError.ComptimeEvaluationFailed;
        }

        // expr_end is one before the closing paren
        const expr_end = self.pos - 1;
        const expr = self.source[expr_start..expr_end];

        // Evaluate the expression
        var evaluator = comptime_eval.ComptimeEvaluator.init(self.allocator, expr, self.line, self.col);

        // Set up environment if provided
        if (self.comptime_env) |env| {
            evaluator.env = env.env_vars;
            evaluator.build_time = env.build_time;
            evaluator.git_commit = env.git_commit;
            evaluator.version = env.version;
        }

        const result = evaluator.evaluate() catch return StripError.ComptimeEvaluationFailed;
        defer result.deinit(self.allocator);

        // Emit the literal value
        const literal = comptime_eval.emitLiteral(self.allocator, result) catch return StripError.OutOfMemory;
        defer self.allocator.free(literal);

        // Output the literal
        self.output.appendSlice(self.allocator, literal) catch return StripError.OutOfMemory;

        // Pad with spaces to preserve line/column positions
        // The total span is from keyword_start to self.pos (end of closing paren)
        const total_span = self.pos - keyword_start;
        const literal_len = literal.len;

        if (total_span > literal_len) {
            const padding = total_span - literal_len;
            for (0..padding) |_| {
                self.output.append(self.allocator, ' ') catch return StripError.OutOfMemory;
            }
        }

        return true;
    }

    // ========================================================================
    // Generic Parameter Stripping (Phase 6)
    // ========================================================================

    fn tryStripGenericParams(self: *Self) StripError!bool {
        // We're at '<'
        // This could be: generic params, comparison, JSX

        if (self.tsx_mode) {
            // In TSX, need to distinguish from JSX
            if (self.looksLikeJsx()) {
                return false;
            }
        }

        // Check if this is a comparison operator
        if (self.looksLikeComparison()) {
            // Explicit type arguments on a call (`f<number>(x)`) also sit
            // right after an expression. Per the TypeScript disambiguation
            // rule, a balanced `<...>` of type syntax whose closing `>` lands
            // directly on `(` is a type-argument list, not a comparison; only
            // then fall through to the strip below.
            if (!self.looksLikeCallTypeArguments()) return false;
        }

        // Try to parse as generic params. The probe below advances pos via
        // skipBalancedAngles/skipWhitespaceTracked, both of which also advance
        // self.line/self.col across newlines. If the probe fails we must rewind
        // ALL of pos, line, and col - rewinding pos alone drifts the line
        // counter, which corrupts the byte/line positions recorded for later
        // type annotations. In TSX this fires on every multi-line JSX closing
        // tag (`</body>` etc.): looksLikeJsx() rejects the `</` so we reach here,
        // probe across the trailing newline, then bail - leaving self.line ahead
        // and breaking declared-Spec extraction on the handler that follows.
        const start = self.pos;
        const start_line = self.line;
        const start_col = self.col;
        if (!self.skipBalancedAngles()) {
            self.pos = start;
            self.line = start_line;
            self.col = start_col;
            return false;
        }

        // Generic params are typically followed by ( or extends
        self.skipWhitespaceTracked();
        if (self.pos < self.source.len) {
            const next = self.source[self.pos];
            if (next == '(' or next == '{') {
                // Record generic params (content inside angle brackets)
                self.recordTypeAnnotation(.generic_params, start + 1, self.pos - 1, 0, 0);
                // Looks like generic function - blank the params
                self.blankSpan(start, self.pos);
                return true;
            }

            // Check for 'extends' (in type context)
            const kw = self.peekKeyword();
            if (kw != null and std.mem.eql(u8, kw.?, "extends")) {
                self.recordTypeAnnotation(.generic_params, start + 1, self.pos - 1, 0, 0);
                self.blankSpan(start, self.pos);
                return true;
            }
        }

        // Not generic params, restore position AND line/col tracking.
        self.pos = start;
        self.line = start_line;
        self.col = start_col;
        return false;
    }

    fn looksLikeJsx(self: *Self) bool {
        if (self.pos + 1 >= self.source.len) return false;
        const next = self.source[self.pos + 1];
        // JSX: <div, <MyComponent, <>
        if (next == '>') return true; // Fragment
        if (std.ascii.isAlphabetic(next) or next == '_') {
            // Could be JSX tag - check if lowercase (element) or uppercase (component)
            return true;
        }
        return false;
    }

    fn looksLikeComparison(self: *Self) bool {
        // Look at what comes before - if it's an expression end, this is comparison
        // This is a heuristic - we look at the output buffer
        if (self.output.items.len == 0) return false;

        const last = self.output.items[self.output.items.len - 1];
        // If last char is identifier/number end, or ), ], this could be comparison
        if (std.ascii.isAlphanumeric(last) or last == ')' or last == ']' or last == '_' or last == '$') {
            // Could be comparison like `a < b` or `foo() < bar`
            // Need more context - for now, be conservative
            return true;
        }
        return false;
    }

    /// Probe from a `<` that follows an expression: returns true when the
    /// balanced `<...>` reads as an explicit type-argument list on a call,
    /// i.e. its closing `>` is directly followed by `(` and the content stays
    /// inside type-argument grammar. Anything a type-argument list cannot
    /// contain (`&&`, `||`, arithmetic, a top-level `?`/`:`/`;`, an unbalanced
    /// closer) means comparison, so the probe rejects and the source passes
    /// through unchanged. Side-effect-free: pos/line/col are restored.
    fn looksLikeCallTypeArguments(self: *Self) bool {
        const saved_pos = self.pos;
        const saved_line = self.line;
        const saved_col = self.col;
        defer {
            self.pos = saved_pos;
            self.line = saved_line;
            self.col = saved_col;
        }

        self.pos += 1;
        self.col += 1;
        var angle_depth: u16 = 1;
        var paren_depth: u16 = 0;
        var bracket_depth: u16 = 0;
        var brace_depth: u16 = 0;

        while (self.pos < self.source.len) {
            const c = self.source[self.pos];

            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch return false;
                continue;
            }
            if (c == '/' and self.pos + 1 < self.source.len) {
                const next = self.source[self.pos + 1];
                if (next == '/') {
                    self.skipLineComment();
                    continue;
                }
                if (next == '*') {
                    self.skipBlockComment() catch return false;
                    continue;
                }
            }

            if (c == '>') {
                if (angle_depth == 1) {
                    return paren_depth == 0 and bracket_depth == 0 and brace_depth == 0 and
                        self.pos + 1 < self.source.len and self.source[self.pos + 1] == '(';
                }
                angle_depth -= 1;
            } else if (c == '<') {
                angle_depth += 1;
            } else if (c == '(') {
                paren_depth += 1;
            } else if (c == ')') {
                if (paren_depth == 0) return false;
                paren_depth -= 1;
            } else if (c == '[') {
                bracket_depth += 1;
            } else if (c == ']') {
                if (bracket_depth == 0) return false;
                bracket_depth -= 1;
            } else if (c == '{') {
                brace_depth += 1;
            } else if (c == '}') {
                if (brace_depth == 0) return false;
                brace_depth -= 1;
            } else if (c == '=') {
                // Only `=>` of a function type; a bare `=` cannot appear in
                // type arguments. Consume both so the `>` does not close the
                // angle scan.
                if (self.pos + 1 >= self.source.len or self.source[self.pos + 1] != '>') return false;
                self.pos += 2;
                self.col += 2;
                continue;
            } else if (c == '|' or c == '&') {
                // Union/intersection are single; doubled is a logical operator.
                if (self.pos + 1 < self.source.len and self.source[self.pos + 1] == c) return false;
            } else if (c == '?' or c == ':') {
                // Valid only nested (function-type params, object/tuple
                // members); at the top level these read as ternary syntax.
                if (paren_depth == 0 and bracket_depth == 0 and brace_depth == 0) return false;
            } else if (c == ';') {
                if (brace_depth == 0) return false;
            } else if (c == '-') {
                // Negative numeric literal type; anything else is arithmetic.
                if (self.pos + 1 >= self.source.len or !std.ascii.isDigit(self.source[self.pos + 1])) return false;
            } else if (c == ' ' or c == '\t' or c == '\n' or c == '\r' or c == ',' or c == '.') {
                // Allowed separators.
            } else if (!isIdentifierContinue(c)) {
                return false;
            }

            if (c == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
        return false;
    }

    // ========================================================================
    // Error Checking (Phase 8)
    // ========================================================================

    // ========================================================================
    // Banned Type Detection
    // ========================================================================

    /// Record the location of an unsupported-feature rejection for callers
    /// that requested structured diagnostics. A no-op when `diagnostic_out`
    /// is null. The matching `StripError` is still returned by the caller.
    fn recordDiagnostic(self: *Self, kind: StripDiagnosticKind) void {
        const d: StripDiagnostic = .{ .line = self.line, .column = self.col, .kind = kind };
        if (self.diagnostic_out) |out| out.* = d;
        // Best-effort: an OOM here will resurface at the next allocating step.
        self.diagnostics.append(self.allocator, d) catch {};
    }

    fn checkForAnyType(self: *Self) StripError!void {
        if (self.pos >= self.source.len) return;
        if (!isIdentifierStart(self.source[self.pos])) return;

        const kw = self.peekKeyword();
        if (kw != null and std.mem.eql(u8, kw.?, "any")) {
            // Word boundary: next char after "any" must not continue the identifier
            const after = self.pos + 3;
            if (after >= self.source.len or !isIdentifierContinue(self.source[after])) {
                if (self.report_errors) std.log.err("{}:{}: {s}", .{ self.line, self.col, StripDiagnosticKind.any_type.message() });
                self.recordDiagnostic(.any_type);
                if (self.collect_all_diagnostics) return; // recorded; keep scanning for more
                return StripError.UnsupportedAnyType;
            }
        }
    }

    // ========================================================================
    // Helper Functions
    // ========================================================================

    fn isAtStatementStart(self: *Self) bool {
        // Simplified check - at start of file or after statement-ending punctuation
        if (self.output.items.len == 0) return true;
        const last = self.output.items[self.output.items.len - 1];
        return last == ';' or last == '{' or last == '}' or last == '\n';
    }

    fn blankSpan(self: *Self, start: usize, end: usize) void {
        // Replace with spaces, preserving newlines
        for (self.source[start..end]) |c| {
            if (c == '\n') {
                self.output.append(self.allocator, '\n') catch {};
            } else {
                self.output.append(self.allocator, ' ') catch {};
            }
        }
    }

    /// Record a type annotation in the TypeMap.
    fn recordTypeAnnotation(
        self: *Self,
        kind: TypeMapKind,
        type_start: usize,
        type_end: usize,
        name_start: usize,
        name_end: usize,
    ) void {
        self.type_map.addEntry(self.allocator, .{
            .kind = kind,
            .source_start = @intCast(type_start),
            .source_end = @intCast(type_end),
            .context_line = self.line,
            .context_col = self.col,
            .name_start = @intCast(name_start),
            .name_end = @intCast(name_end),
        }) catch {};
    }

    /// Scan backwards in the original source to find the identifier before a given position.
    /// Returns [start, end] byte offsets. Returns [0, 0] if no identifier found.
    fn findIdentifierBefore(self: *const Self, pos: usize) [2]usize {
        if (pos == 0) return .{ 0, 0 };
        var end = pos;
        // Skip whitespace backwards
        while (end > 0 and (self.source[end - 1] == ' ' or self.source[end - 1] == '\t')) {
            end -= 1;
        }
        // Now end points past the last identifier char (or non-identifier)
        var start = end;
        while (start > 0 and isIdentifierContinue(self.source[start - 1])) {
            start -= 1;
        }
        if (start >= end) return .{ 0, 0 };
        if (!isIdentifierStart(self.source[start])) return .{ 0, 0 };
        return .{ start, end };
    }

    fn skipWhitespaceTracked(self: *Self) void {
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == ' ' or c == '\t') {
                self.pos += 1;
                self.col += 1;
            } else if (c == '\n') {
                self.pos += 1;
                self.line += 1;
                self.col = 1;
            } else if (c == '\r') {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn skipToStatementEnd(self: *Self) void {
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == ';') {
                self.pos += 1;
                self.col += 1;
                return;
            }
            if (c == '\n') {
                return; // Don't consume newline
            }
            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch {};
                continue;
            }
            self.pos += 1;
            self.col += 1;
        }
    }

    fn skipTypeExpressionUntilDelimiter(
        self: *Self,
        delimiters: []const u8,
        stop_before_arrow: bool,
    ) StripError!void {
        var paren_depth: u16 = 0;
        var bracket_depth: u16 = 0;
        var angle_depth: u16 = 0;
        var brace_depth: u16 = 0;

        while (self.pos < self.source.len) {
            const c = self.source[self.pos];

            // Handle strings
            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch {};
                continue;
            }

            // Comments inside the annotation: skip wholesale so their
            // content cannot match a delimiter or unbalance the depths below.
            if (c == '/' and self.pos + 1 < self.source.len) {
                const next = self.source[self.pos + 1];
                if (next == '/') {
                    self.skipLineComment();
                    continue;
                }
                if (next == '*') {
                    try self.skipBlockComment();
                    continue;
                }
            }

            // Check identifiers for banned types (catches nested 'any' in Record<string, any> etc.)
            if (isIdentifierStart(c)) {
                if (!(brace_depth > 0 and self.isObjectTypeMemberKey(self.pos))) {
                    try self.checkForAnyType();
                }
            }

            // Check delimiters at depth 0 FIRST (before tracking)
            if (paren_depth == 0 and bracket_depth == 0 and angle_depth == 0 and brace_depth == 0) {
                for (delimiters) |d| {
                    if (c == d) {
                        if (c == '{' and self.isObjectTypeBraceStart(self.pos)) break;
                        return;
                    }
                }
                // Check for arrow
                if (stop_before_arrow and c == '=' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '>') {
                    return; // Stop before =>
                }
            }

            // Track nesting
            if (c == '(') {
                paren_depth += 1;
            } else if (c == ')') {
                if (paren_depth > 0) paren_depth -= 1;
            } else if (c == '[') {
                bracket_depth += 1;
            } else if (c == ']') {
                if (bracket_depth > 0) bracket_depth -= 1;
            } else if (c == '<') {
                angle_depth += 1;
            } else if (c == '>') {
                if (angle_depth > 0) angle_depth -= 1;
            } else if (c == '{') {
                brace_depth += 1;
            } else if (c == '}') {
                if (brace_depth > 0) brace_depth -= 1;
            }

            if (c == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    fn isObjectTypeBraceStart(self: *const Self, pos: usize) bool {
        const prev = self.previousSignificantChar(pos) orelse return true;
        return switch (prev) {
            ':', '|', '&', '(', '[', '<', ',', '=' => true,
            else => false,
        };
    }

    fn isObjectTypeMemberKey(self: *const Self, pos: usize) bool {
        var i = pos;
        if (i >= self.source.len or !isIdentifierStart(self.source[i])) return false;

        while (i < self.source.len and isIdentifierContinue(self.source[i])) {
            i += 1;
        }
        i = self.skipInlineWhitespaceFrom(i);

        if (i < self.source.len and self.source[i] == '?') {
            i += 1;
            i = self.skipInlineWhitespaceFrom(i);
        }

        return i < self.source.len and (self.source[i] == ':' or self.source[i] == '(');
    }

    fn skipInlineWhitespaceFrom(self: *const Self, pos: usize) usize {
        var i = pos;
        while (i < self.source.len and (self.source[i] == ' ' or self.source[i] == '\t')) {
            i += 1;
        }
        return i;
    }

    fn previousSignificantChar(self: *const Self, pos: usize) ?u8 {
        var i = pos;
        while (i > 0) {
            i -= 1;
            return switch (self.source[i]) {
                ' ', '\t', '\n', '\r' => continue,
                else => self.source[i],
            };
        }
        return null;
    }

    fn skipBalancedAngles(self: *Self) bool {
        if (self.pos >= self.source.len or self.source[self.pos] != '<') return false;

        self.pos += 1;
        self.col += 1;
        var depth: u16 = 1;

        while (self.pos < self.source.len and depth > 0) {
            const c = self.source[self.pos];

            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch return false;
                continue;
            }

            // Comments inside the generic: skip wholesale so a `<` or `>`
            // in the comment text cannot unbalance the depth count.
            if (c == '/' and self.pos + 1 < self.source.len) {
                const next = self.source[self.pos + 1];
                if (next == '/') {
                    self.skipLineComment();
                    continue;
                }
                if (next == '*') {
                    self.skipBlockComment() catch return false;
                    continue;
                }
            }

            if (c == '<') {
                depth += 1;
            } else if (c == '>') {
                depth -= 1;
            } else if (c == '(' or c == '[' or c == '{') {
                // These must be balanced within the generic
                self.skipMatchingBracket(c);
                continue;
            }

            if (c == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }

        return depth == 0;
    }

    fn skipBalancedBraces(self: *Self) void {
        if (self.pos >= self.source.len or self.source[self.pos] != '{') return;

        self.pos += 1;
        self.col += 1;
        var depth: u16 = 1;

        while (self.pos < self.source.len and depth > 0) {
            const c = self.source[self.pos];

            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch {};
                continue;
            }

            if (c == '{') {
                depth += 1;
            } else if (c == '}') {
                depth -= 1;
            }

            if (c == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    fn skipMatchingBracket(self: *Self, open: u8) void {
        const close: u8 = switch (open) {
            '(' => ')',
            '[' => ']',
            '{' => '}',
            else => return,
        };

        self.pos += 1;
        self.col += 1;
        var depth: u16 = 1;

        while (self.pos < self.source.len and depth > 0) {
            const c = self.source[self.pos];

            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch {};
                continue;
            }

            if (c == open) {
                depth += 1;
            } else if (c == close) {
                depth -= 1;
            }

            if (c == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    fn peekKeyword(self: *Self) ?[]const u8 {
        if (self.pos >= self.source.len) return null;
        if (!isIdentifierStart(self.source[self.pos])) return null;

        var end = self.pos;
        while (end < self.source.len and isIdentifierContinue(self.source[end])) {
            end += 1;
        }
        return self.source[self.pos..end];
    }

    fn peekIdentifierStart(self: *Self) bool {
        if (self.pos >= self.source.len) return false;
        return isIdentifierStart(self.source[self.pos]);
    }

    fn scanIdentifier(self: *Self) []const u8 {
        const start = self.pos;
        while (self.pos < self.source.len and isIdentifierContinue(self.source[self.pos])) {
            self.pos += 1;
            self.col += 1;
        }
        return self.source[start..self.pos];
    }

    fn skipString(self: *Self, quote: u8) StripError!void {
        self.pos += 1;
        self.col += 1;

        if (quote == '`') {
            // Template literal
            while (self.pos < self.source.len) {
                const c = self.source[self.pos];
                if (c == '`') {
                    self.pos += 1;
                    self.col += 1;
                    return;
                }
                if (c == '\\' and self.pos + 1 < self.source.len) {
                    self.pos += 2;
                    self.col += 2;
                    continue;
                }
                if (c == '$' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '{') {
                    self.pos += 2;
                    self.col += 2;
                    var depth: usize = 1;
                    while (self.pos < self.source.len and depth > 0) {
                        const ic = self.source[self.pos];
                        // Strings and nested templates inside the interpolation
                        // may contain unbalanced braces; delegate to skipString
                        // (recursively, so nested `${...}` is handled too) so a
                        // `}` inside a string does not close the interpolation
                        // early. Also honor backslash escapes.
                        if (ic == '"' or ic == '\'' or ic == '`') {
                            self.skipString(ic) catch {};
                            continue;
                        }
                        if (ic == '\\' and self.pos + 1 < self.source.len) {
                            self.pos += 2;
                            self.col += 2;
                            continue;
                        }
                        if (ic == '{') depth += 1;
                        if (ic == '}') depth -= 1;
                        if (ic == '\n') {
                            self.line += 1;
                            self.col = 1;
                        } else {
                            self.col += 1;
                        }
                        self.pos += 1;
                    }
                    continue;
                }
                if (c == '\n') {
                    self.line += 1;
                    self.col = 1;
                } else {
                    self.col += 1;
                }
                self.pos += 1;
            }
        } else {
            // Regular string
            while (self.pos < self.source.len) {
                const c = self.source[self.pos];
                if (c == quote) {
                    self.pos += 1;
                    self.col += 1;
                    return;
                }
                if (c == '\\' and self.pos + 1 < self.source.len) {
                    self.pos += 2;
                    self.col += 2;
                    continue;
                }
                if (c == '\n') {
                    return StripError.UnterminatedString;
                }
                self.pos += 1;
                self.col += 1;
            }
        }
    }

    fn skipLineComment(self: *Self) void {
        while (self.pos < self.source.len and self.source[self.pos] != '\n') {
            self.pos += 1;
            self.col += 1;
        }
    }

    fn skipBlockComment(self: *Self) StripError!void {
        self.pos += 2; // skip /*
        self.col += 2;

        while (self.pos + 1 < self.source.len) {
            if (self.source[self.pos] == '*' and self.source[self.pos + 1] == '/') {
                self.pos += 2;
                self.col += 2;
                return;
            }
            if (self.source[self.pos] == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }

        return StripError.UnterminatedComment;
    }

    fn skipNumber(self: *Self) void {
        if (self.pos >= self.source.len) return;

        // Handle 0x, 0b, 0o prefixes
        if (self.source[self.pos] == '0' and self.pos + 1 < self.source.len) {
            const next = self.source[self.pos + 1];
            if (next == 'x' or next == 'X') {
                self.pos += 2;
                self.col += 2;
                while (self.pos < self.source.len and std.ascii.isHex(self.source[self.pos])) {
                    self.pos += 1;
                    self.col += 1;
                }
                return;
            }
            if (next == 'b' or next == 'B') {
                self.pos += 2;
                self.col += 2;
                while (self.pos < self.source.len and (self.source[self.pos] == '0' or self.source[self.pos] == '1')) {
                    self.pos += 1;
                    self.col += 1;
                }
                return;
            }
            if (next == 'o' or next == 'O') {
                self.pos += 2;
                self.col += 2;
                while (self.pos < self.source.len and self.source[self.pos] >= '0' and self.source[self.pos] <= '7') {
                    self.pos += 1;
                    self.col += 1;
                }
                return;
            }
        }

        // Decimal / float
        while (self.pos < self.source.len and std.ascii.isDigit(self.source[self.pos])) {
            self.pos += 1;
            self.col += 1;
        }
        if (self.pos < self.source.len and self.source[self.pos] == '.') {
            self.pos += 1;
            self.col += 1;
            while (self.pos < self.source.len and std.ascii.isDigit(self.source[self.pos])) {
                self.pos += 1;
                self.col += 1;
            }
        }
        // Exponent
        if (self.pos < self.source.len and (self.source[self.pos] == 'e' or self.source[self.pos] == 'E')) {
            self.pos += 1;
            self.col += 1;
            if (self.pos < self.source.len and (self.source[self.pos] == '+' or self.source[self.pos] == '-')) {
                self.pos += 1;
                self.col += 1;
            }
            while (self.pos < self.source.len and std.ascii.isDigit(self.source[self.pos])) {
                self.pos += 1;
                self.col += 1;
            }
        }
    }

    fn isIdentifierStart(c: u8) bool {
        return std.ascii.isAlphabetic(c) or c == '_' or c == '$';
    }

    fn isIdentifierContinue(c: u8) bool {
        return std.ascii.isAlphanumeric(c) or c == '_' or c == '$';
    }
};

/// Trim trailing whitespace from a source range, returning the new end position.
/// Classify return type text as a type guard ("x is T") or plain return annotation.
fn classifyReturnType(text: []const u8) TypeMapKind {
    return if (std.mem.indexOf(u8, text, " is ") != null) .type_guard_annotation else .return_annotation;
}

fn trimTrailingWs(source: []const u8, start: usize, end: usize) usize {
    var pos = end;
    while (pos > start and (source[pos - 1] == ' ' or source[pos - 1] == '\t')) {
        pos -= 1;
    }
    return pos;
}

// ============================================================================
// Tests
// ============================================================================

test "passthrough plain js" {
    const source = "let x = 1;\nfunction f() { return x + 1; }";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings(source, result.code);
}

test "passthrough with strings" {
    const source = "let s = \"hello world\";\nlet t = 'single';";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings(source, result.code);
}

test "ENG-15: typed arrow params are stripped in call-argument position" {
    // The `=`-anchored path strips `const f = (a: number) => ...`; this guards
    // the bare-`(` expression position (arrow passed as a call argument), which
    // previously left the `: number` in place and broke the parser.
    const source = "const r = ns.toSorted((a: number, b: number) => a - b);";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "=> a - b") != null);
}

test "ENG-15: a ternary with parenthesized branches is not mistaken for an arrow" {
    // `cond ? (a) : (b)` has a `:` after `)` but no `=>`; the strict detector
    // must leave it untouched (no false-positive type stripping).
    const source = "const x = c ? (1) : (2);";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings(source, result.code);
}

test "passthrough with comments" {
    const source = "// line comment\nlet x = 1; /* block */";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings(source, result.code);
}

test "type alias stripped" {
    const result = try strip(std.testing.allocator, "type X = number;", .{});
    defer @constCast(&result).deinit();
    // Should be all spaces/newlines
    const trimmed = std.mem.trim(u8, result.code, " \n\r\t");
    try std.testing.expectEqual(@as(usize, 0), trimmed.len);
}

test "interface stripped" {
    const result = try strip(std.testing.allocator, "interface Foo { x: number; }", .{});
    defer @constCast(&result).deinit();
    const trimmed = std.mem.trim(u8, result.code, " \n\r\t");
    try std.testing.expectEqual(@as(usize, 0), trimmed.len);
}

test "import type stripped" {
    const result = try strip(std.testing.allocator, "import type { Foo } from \"./types\";", .{});
    defer @constCast(&result).deinit();
    const trimmed = std.mem.trim(u8, result.code, " \n\r\t");
    try std.testing.expectEqual(@as(usize, 0), trimmed.len);
}

test "import type Spec from zigttp:types stripped" {
    // Author-declared proof obligations live behind `import type { Spec }
    // from "zigttp:types"`. Confirm the synthetic module path strips
    // identically to any other type-only import.
    const result = try strip(std.testing.allocator, "import type { Spec } from \"zigttp:types\";", .{});
    defer @constCast(&result).deinit();
    const trimmed = std.mem.trim(u8, result.code, " \n\r\t");
    try std.testing.expectEqual(@as(usize, 0), trimmed.len);
}

test "export type stripped" {
    const result = try strip(std.testing.allocator, "export type { Foo };", .{});
    defer @constCast(&result).deinit();
    const trimmed = std.mem.trim(u8, result.code, " \n\r\t");
    try std.testing.expectEqual(@as(usize, 0), trimmed.len);
}

test "exported type alias declaration stripped" {
    const source =
        \\export type Result<T, E> = {
        \\  ok: boolean;
        \\  value?: T;
        \\  error?: E;
        \\};
    ;
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    const trimmed = std.mem.trim(u8, result.code, " \n\r\t");
    try std.testing.expectEqual(@as(usize, 0), trimmed.len);
}

test "exported interface declaration stripped" {
    const source =
        \\export interface AppCapabilities {
        \\  taskRepo: Repository<Task>;
        \\  clock: Clock;
        \\}
    ;
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    const trimmed = std.mem.trim(u8, result.code, " \n\r\t");
    try std.testing.expectEqual(@as(usize, 0), trimmed.len);
}

// NOTE: enum, namespace, decorator, implements, and access modifier detection
// has been moved to the parser (Stage 4). The stripper now passes these through
// so the parser can produce consistent error messages for both .ts and .js files.

test "enum passes through to parser" {
    var result = try strip(std.testing.allocator, "enum Color { Red, Blue }", .{});
    defer result.deinit();
    try std.testing.expect(result.code.len > 0);
}

test "namespace passes through to parser" {
    var result = try strip(std.testing.allocator, "namespace N { }", .{});
    defer result.deinit();
    try std.testing.expect(result.code.len > 0);
}

test "decorator passes through to parser" {
    var result = try strip(std.testing.allocator, "@sealed class X {}", .{});
    defer result.deinit();
    try std.testing.expect(result.code.len > 0);
}

test "any type annotation errors" {
    const result = strip(std.testing.allocator, "const x: any = 5;", .{});
    try std.testing.expectError(StripError.UnsupportedAnyType, result);
}

test "diagnostic_out captures any-type rejection location" {
    var diag: ?StripDiagnostic = null;
    const result = strip(std.testing.allocator, "const x: any = 5;", .{ .diagnostic_out = &diag });
    try std.testing.expectError(StripError.UnsupportedAnyType, result);
    try std.testing.expect(diag != null);
    try std.testing.expectEqual(StripDiagnosticKind.any_type, diag.?.kind);
    try std.testing.expectEqual(@as(u32, 1), diag.?.line);
    try std.testing.expectEqual(@as(u32, 10), diag.?.column);
}

test "diagnostic_out captures as-assertion rejection" {
    var diag: ?StripDiagnostic = null;
    const result = strip(std.testing.allocator, "const x = value as number;", .{ .diagnostic_out = &diag });
    try std.testing.expectError(StripError.UnsupportedAsAssertion, result);
    try std.testing.expect(diag != null);
    try std.testing.expectEqual(StripDiagnosticKind.as_assertion, diag.?.kind);
}

test "diagnostic_out captures satisfies-assertion rejection" {
    var diag: ?StripDiagnostic = null;
    const result = strip(std.testing.allocator, "const c = { p: 1 } satisfies Config;", .{ .diagnostic_out = &diag });
    try std.testing.expectError(StripError.UnsupportedSatisfiesAssertion, result);
    try std.testing.expect(diag != null);
    try std.testing.expectEqual(StripDiagnosticKind.satisfies_assertion, diag.?.kind);
}

test "diagnostic_out left null on a clean strip" {
    var diag: ?StripDiagnostic = null;
    var result = try strip(std.testing.allocator, "const x: number = 5;", .{ .diagnostic_out = &diag });
    defer result.deinit();
    try std.testing.expect(diag == null);
}

test "any function param errors" {
    const result = strip(std.testing.allocator, "function f(x: any) { return x; }", .{});
    try std.testing.expectError(StripError.UnsupportedAnyType, result);
}

test "any return type errors" {
    const result = strip(std.testing.allocator, "function f(): any { return 1; }", .{});
    try std.testing.expectError(StripError.UnsupportedAnyType, result);
}

test "any in object return type errors" {
    const result = strip(std.testing.allocator, "function f(): { ok: any } { return { ok: true }; }", .{});
    try std.testing.expectError(StripError.UnsupportedAnyType, result);
}

test "any object return type member key allowed" {
    const source = "function f(): { any: string; optional?: number } { return { any: \"x\", optional: 1 }; }";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "any: string") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "optional?: number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "return { any: \"x\", optional: 1 }") != null);
}

test "any as assertion errors" {
    // 'as' is now always rejected, so UnsupportedAsAssertion fires before any check
    const result = strip(std.testing.allocator, "const x = value as any;", .{});
    try std.testing.expectError(StripError.UnsupportedAsAssertion, result);
}

test "any in generic errors" {
    const result = strip(std.testing.allocator, "const x: Record<string, any> = {};", .{});
    try std.testing.expectError(StripError.UnsupportedAnyType, result);
}

test "any array errors" {
    const result = strip(std.testing.allocator, "const x: any[] = [];", .{});
    try std.testing.expectError(StripError.UnsupportedAnyType, result);
}

test "any variable name allowed" {
    var result = try strip(std.testing.allocator, "const any = 5;", .{});
    defer result.deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "const any") != null);
}

test "anything identifier allowed" {
    var result = try strip(std.testing.allocator, "let x: anything = 5;", .{});
    defer result.deinit();
}

// Stage 3/4: class keyword passes through stripper to be caught by parser
test "class passes through to parser" {
    var result = try strip(std.testing.allocator, "class Foo { }", .{});
    defer result.deinit();
    // Class should pass through (stripped to maintain positions)
    // Parser will catch it with helpful error message
    try std.testing.expect(result.code.len > 0);
    // Verify it contains some whitespace (class keyword was blanked)
    try std.testing.expect(std.mem.indexOfScalar(u8, result.code, ' ') != null or
        std.mem.indexOfScalar(u8, result.code, '{') != null);
}

test "public label allowed" {
    var result = try strip(std.testing.allocator, "public: foo();", .{});
    defer result.deinit();
    try std.testing.expectEqualStrings("public: foo();", result.code);
}

test "line preservation" {
    const input = "type X = number;\nlet x = 1;\nconsole.log(x);";
    const result = try strip(std.testing.allocator, input, .{});
    defer @constCast(&result).deinit();

    // Count newlines - should be preserved
    var input_newlines: usize = 0;
    var output_newlines: usize = 0;
    for (input) |c| {
        if (c == '\n') input_newlines += 1;
    }
    for (result.code) |c| {
        if (c == '\n') output_newlines += 1;
    }
    try std.testing.expectEqual(input_newlines, output_newlines);
}

test "type with usage preserved" {
    const input = "type User = { id: number };\nlet u = { id: 1 };";
    const result = try strip(std.testing.allocator, input, .{});
    defer @constCast(&result).deinit();

    // Second line should be preserved
    try std.testing.expect(std.mem.indexOf(u8, result.code, "let u = { id: 1 };") != null);
}

// ============================================================================
// Phase 4: Annotation Tests
// ============================================================================

test "variable annotation stripped" {
    const result = try strip(std.testing.allocator, "let x: number = 1;", .{});
    defer @constCast(&result).deinit();
    // Should strip ": number" but keep the rest
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "let x") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "= 1;") != null);
}

test "function params annotation stripped" {
    const result = try strip(std.testing.allocator, "function add(a: number, b: number) { return a + b; }", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "function add(a") != null);
}

test "optional function param annotation stripped" {
    const result = try strip(std.testing.allocator, "function greet(name?: string) { return name; }", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "name?") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": string") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "function greet(name") != null);
}

test "function-typed param annotation stripped" {
    const result = try strip(
        std.testing.allocator,
        "function use(loadState: (id: string) => string | undefined) { return loadState(\"1\"); }",
        .{},
    );
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "loadState:") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "=> string") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "function use(") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "return loadState(\"1\")") != null);
}

test "function return type stripped" {
    const result = try strip(std.testing.allocator, "function add(a, b): number { return a + b; }", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "): number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "function add(a, b)") != null);
}

test "function return type with space before colon preserves output length" {
    const src = "function f(x) : string { return x; }";
    const result = try strip(std.testing.allocator, src, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqual(src.len, result.code.len);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": string") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "function f(x)") != null);
}

test "function object return type stripped" {
    const source = "function make(): { ok: boolean } { return { ok: true }; }";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "boolean") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "return { ok: true }") != null);
}

test "function nested object return type stripped" {
    const source = "function make(): { ok: boolean; meta: { count: number } } { return { ok: true, meta: { count: 1 } }; }";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "boolean") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "return { ok: true, meta: { count: 1 } }") != null);
}

test "function union and intersection object return types stripped" {
    const source =
        \\function fromUnion(): Response | { ok: boolean } { return { ok: true }; }
        \\function fromIntersection(): Response & { ok: boolean } { return { ok: true }; }
    ;
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "boolean") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "return { ok: true }") != null);
}

test "arrow function annotation stripped" {
    const result = try strip(std.testing.allocator, "const f = (x: string): string => x.trim();", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": string") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "const f = (x") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "=> x.trim()") != null);
}

test "arrow function return type with space before colon preserves output length" {
    const src = "const f = (x) : string => x;";
    const result = try strip(std.testing.allocator, src, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqual(src.len, result.code.len);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "=> x") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": string") == null);
}

test "arrow function object return type stripped" {
    const source = "const make = (): { ok: boolean } => ({ ok: true });";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "boolean") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "=> ({ ok: true })") != null);
}

// ============================================================================
// Phase 5: Assertion Tests
// ============================================================================

test "as assertion rejected" {
    const result = strip(std.testing.allocator, "let n = (foo as number) + 1;", .{});
    try std.testing.expectError(StripError.UnsupportedAsAssertion, result);
}

test "satisfies assertion rejected" {
    const result = strip(std.testing.allocator, "const cfg = { port: 8080 } satisfies Config;", .{});
    try std.testing.expectError(StripError.UnsupportedSatisfiesAssertion, result);
}

test "collect_all_diagnostics: every as/satisfies site reported in one pass" {
    const src =
        \\const a = (x as string);
        \\const b = (y as number);
        \\const c = (z satisfies Config);
    ;
    var result = try strip(std.testing.allocator, src, .{ .collect_all_diagnostics = true });
    defer @constCast(&result).deinit();

    // All three rejections are reported; none aborts the strip.
    try std.testing.expectEqual(@as(usize, 3), result.diagnostics.len);
    var as_count: usize = 0;
    var sat_count: usize = 0;
    for (result.diagnostics) |d| switch (d.kind) {
        .as_assertion => as_count += 1,
        .satisfies_assertion => sat_count += 1,
        else => {},
    };
    try std.testing.expectEqual(@as(usize, 2), as_count);
    try std.testing.expectEqual(@as(usize, 1), sat_count);
}

test "collect_all_diagnostics off: first as still aborts (default unchanged)" {
    const result = strip(std.testing.allocator, "const a = (x as string); const b = (y as number);", .{});
    try std.testing.expectError(StripError.UnsupportedAsAssertion, result);
}

// ============================================================================
// Phase 6: Generic Tests
// ============================================================================

test "generic function stripped" {
    const result = try strip(std.testing.allocator, "function id<T>(x: T): T { return x; }", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "<T>") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": T") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "function id") != null);
}

test "generic type alias stripped" {
    const result = try strip(std.testing.allocator, "type Box<T> = { value: T };", .{});
    defer @constCast(&result).deinit();
    const trimmed = std.mem.trim(u8, result.code, " \n\r\t");
    try std.testing.expectEqual(@as(usize, 0), trimmed.len);
}

test "function type alias stripped" {
    const result = try strip(std.testing.allocator, "type HandlerFn = (req: Request) => Response;", .{});
    defer @constCast(&result).deinit();
    const trimmed = std.mem.trim(u8, result.code, " \n\r\t");
    try std.testing.expectEqual(@as(usize, 0), trimmed.len);
}

test "full handler example" {
    const source =
        \\// TypeScript handler example
        \\
        \\type RequestData = {
        \\    name: string;
        \\    count: number;
        \\};
        \\
        \\interface ResponseData {
        \\    message: string;
        \\    timestamp: number;
        \\}
        \\
        \\function processData(data: RequestData): ResponseData {
        \\    return {
        \\        message: "Hello, " + data.name,
        \\        timestamp: Date.now()
        \\    };
        \\}
        \\
        \\function handler(req: Request): Response {
        \\    const data: RequestData = { name: "World", count: 42 };
        \\    const result: ResponseData = processData(data);
        \\    return Response.json(result);
        \\}
        \\
    ;

    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();

    // Should not contain type annotations
    try std.testing.expect(std.mem.indexOf(u8, result.code, "type RequestData") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "interface ResponseData") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": RequestData") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": ResponseData") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, " as RequestData") == null);

    // Should contain the function bodies
    try std.testing.expect(std.mem.indexOf(u8, result.code, "function processData") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "function handler") != null);
}

test "tsx mode preserves jsx" {
    const source =
        \\function App(props: Props): JSX.Element {
        \\    const name: string = "World";
        \\    return <div class="test">{name}</div>;
        \\}
    ;

    const result = try strip(std.testing.allocator, source, .{ .tsx_mode = true });
    defer @constCast(&result).deinit();

    // JSX should be preserved
    try std.testing.expect(std.mem.indexOf(u8, result.code, "<div class=\"test\">") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "</div>") != null);

    // Types should be stripped
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": Props") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": string") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": JSX.Element") == null);
}

test "tsx mode handles fragments" {
    const source =
        \\function List(items: string[]): JSX.Element {
        \\    return <><span>a</span><span>b</span></>;
        \\}
    ;

    const result = try strip(std.testing.allocator, source, .{ .tsx_mode = true });
    defer @constCast(&result).deinit();

    // Fragment syntax preserved
    try std.testing.expect(std.mem.indexOf(u8, result.code, "<>") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "</>") != null);
}

test "multi-line JSX does not drift line tracking for later annotations" {
    // Regression: a TSX closing tag like `</body>` is not recognized as JSX by
    // looksLikeJsx() (the char after `<` is `/`), so it falls into the
    // generic-params probe. That probe crosses the trailing newline via
    // skipBalancedAngles/skipWhitespaceTracked and previously restored only
    // `pos` on the not-generic path, leaving `self.line` advanced. The drift
    // corrupted the recorded line of every annotation after the JSX, which broke
    // declared-Spec extraction on the handler (getFnSigByLoc missed, so all
    // specs activated). `handler` is on line 10; its return annotation must be
    // recorded there.
    const source =
        \\function Page(): JSX.Element {
        \\    return (
        \\        <html>
        \\            <body>
        \\                <h1>x</h1>
        \\            </body>
        \\        </html>
        \\    );
        \\}
        \\function handler(req: Request): Response {
        \\    return Response.text("ok");
        \\}
    ;

    const result = try strip(std.testing.allocator, source, .{ .tsx_mode = true });
    defer @constCast(&result).deinit();

    var found = false;
    for (result.type_map.entries.items) |e| {
        if (e.kind != .return_annotation) continue;
        if (e.name_end <= e.name_start) continue;
        if (std.mem.eql(u8, source[e.name_start..e.name_end], "handler")) {
            try std.testing.expectEqual(@as(u32, 10), e.context_line);
            found = true;
        }
    }
    try std.testing.expect(found);
}

// ============================================================================
// Comptime Tests
// ============================================================================

test "comptime disabled by default" {
    // When enable_comptime is false, comptime() is passed through as-is
    const result = try strip(std.testing.allocator, "const x = comptime(1 + 2);", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "comptime(1 + 2)") != null);
}

test "comptime simple arithmetic" {
    const result = try strip(std.testing.allocator, "const x = comptime(1 + 2 * 3);", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "7") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "comptime") == null);
}

test "comptime Math.PI" {
    const result = try strip(std.testing.allocator, "const pi = comptime(Math.PI);", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "3.14159") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "comptime") == null);
}

test "comptime string" {
    const result = try strip(std.testing.allocator, "const s = comptime(\"hello\");", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "\"hello\"") != null);
}

test "comptime array" {
    const result = try strip(std.testing.allocator, "const arr = comptime([1, 2, 3]);", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "[1,2,3]") != null);
}

test "comptime object" {
    const result = try strip(std.testing.allocator, "const cfg = comptime({ a: 1, b: 2 });", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "({a:1,b:2})") != null);
}

test "comptime ternary" {
    const result = try strip(std.testing.allocator, "const v = comptime(true ? 1 : 0);", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "1") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "true") == null);
}

test "comptime Math.max" {
    const result = try strip(std.testing.allocator, "const m = comptime(Math.max(1, 5, 3));", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "5") != null);
}

test "comptime hash" {
    const result = try strip(std.testing.allocator, "const h = comptime(hash(\"test\"));", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    // Should have an 8-char hex string
    try std.testing.expect(std.mem.indexOf(u8, result.code, "\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "hash") == null);
}

test "comptime with types" {
    // Both type stripping and comptime should work together
    const source = "const x: number = comptime(1 + 2);";
    const result = try strip(std.testing.allocator, source, .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "3") != null);
}

test "comptime identifier without parens passes through" {
    // The identifier 'comptime' without () should be passed through
    const result = try strip(std.testing.allocator, "const comptime = 5;", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "const comptime = 5;") != null);
}

// ============================================================================
// Type Annotation and TypeMap Tests
// ============================================================================

test "type annotations stripped" {
    const result = try strip(std.testing.allocator, "function f(x: number): string { return x; }", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": string") == null);
}

test "as identifier (not assertion) passes" {
    // 'as' used as identifier, not type assertion
    const result = try strip(std.testing.allocator, "import { sha256 as hash } from \"zigttp:crypto\";", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "as hash") != null);
}

// ---------------------------------------------------------------------------
// TypeMap extraction tests
// ---------------------------------------------------------------------------

test "TypeMap records variable annotation" {
    const source = "const x: number = 42;";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();

    const tm = result.type_map;
    try std.testing.expectEqual(@as(usize, 1), tm.count());
    const entry = tm.entries.items[0];
    try std.testing.expectEqual(TypeMapKind.var_annotation, entry.kind);
    try std.testing.expectEqualStrings("number", tm.getTypeText(entry));
    try std.testing.expectEqualStrings("x", tm.getNameText(entry).?);
}

test "TypeMap records type alias" {
    const source = "type Config = { port: number };";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();

    const tm = result.type_map;
    var found_alias = false;
    for (tm.entries.items) |entry| {
        if (entry.kind == .type_alias) {
            found_alias = true;
            try std.testing.expectEqualStrings("Config", tm.getNameText(entry).?);
            const type_text = tm.getTypeText(entry);
            try std.testing.expect(std.mem.indexOf(u8, type_text, "port") != null);
        }
    }
    try std.testing.expect(found_alias);
}

test "TypeMap records interface" {
    const source = "interface CacheFx { get(key: string): string | null; }";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();

    const tm = result.type_map;
    var found_iface = false;
    for (tm.entries.items) |entry| {
        if (entry.kind == .interface_decl) {
            found_iface = true;
            try std.testing.expectEqualStrings("CacheFx", tm.getNameText(entry).?);
        }
    }
    try std.testing.expect(found_iface);
}

test "TypeMap records function return type" {
    const source = "function handler(req: Request): Response { }";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();

    const tm = result.type_map;
    var found_return = false;
    var found_param = false;
    for (tm.entries.items) |entry| {
        if (entry.kind == .return_annotation) {
            found_return = true;
            const type_text = tm.getTypeText(entry);
            try std.testing.expect(std.mem.indexOf(u8, type_text, "Response") != null);
        }
        if (entry.kind == .param_annotation) {
            found_param = true;
            try std.testing.expectEqualStrings("req", tm.getNameText(entry).?);
            const type_text = tm.getTypeText(entry);
            try std.testing.expect(std.mem.indexOf(u8, type_text, "Request") != null);
        }
    }
    try std.testing.expect(found_return);
    try std.testing.expect(found_param);
}

test "TypeMap records generic params" {
    const source = "function identity<T>(x: T): T { return x; }";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();

    const tm = result.type_map;
    var found_generic = false;
    for (tm.entries.items) |entry| {
        if (entry.kind == .generic_params) {
            found_generic = true;
            const type_text = tm.getTypeText(entry);
            try std.testing.expect(std.mem.indexOf(u8, type_text, "T") != null);
        }
    }
    try std.testing.expect(found_generic);
}

test "TypeMap records arrow function return type" {
    const source = "const f = (x: number): string => x.toString();";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();

    const tm = result.type_map;
    var found_return = false;
    for (tm.entries.items) |entry| {
        if (entry.kind == .return_annotation) {
            found_return = true;
            const type_text = tm.getTypeText(entry);
            try std.testing.expect(std.mem.indexOf(u8, type_text, "string") != null);
        }
    }
    try std.testing.expect(found_return);
}

test "TypeMap records object return type" {
    const source = "function make(): { ok: boolean } { return { ok: true }; }";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();

    const tm = result.type_map;
    var found_return = false;
    for (tm.entries.items) |entry| {
        if (entry.kind == .return_annotation) {
            found_return = true;
            const type_text = std.mem.trim(u8, tm.getTypeText(entry), " \n\r\t");
            try std.testing.expectEqualStrings("{ ok: boolean }", type_text);
        }
    }
    try std.testing.expect(found_return);
}

test "type guard annotation detected" {
    const allocator = std.testing.allocator;
    const source = "function isString(x: unknown): x is string { return typeof x === 'string'; }";
    var result = try strip(allocator, source, .{});
    defer result.deinit();

    const tm = &result.type_map;
    var found_guard = false;
    for (tm.entries.items) |entry| {
        if (entry.kind == .type_guard_annotation) {
            found_guard = true;
            const type_text = tm.getTypeText(entry);
            try std.testing.expect(std.mem.indexOf(u8, type_text, "x is string") != null);
        }
    }
    try std.testing.expect(found_guard);
}

test "distinct type declaration stripped" {
    const allocator = std.testing.allocator;
    const source = "distinct type UserId = string;";
    var result = try strip(allocator, source, .{});
    defer result.deinit();

    const tm = &result.type_map;
    var found_distinct = false;
    for (tm.entries.items) |entry| {
        if (entry.kind == .distinct_type) {
            found_distinct = true;
            const name = tm.getNameText(entry) orelse "";
            try std.testing.expectEqualStrings("UserId", name);
            const type_text = tm.getTypeText(entry);
            try std.testing.expectEqualStrings("string", type_text);
        }
    }
    try std.testing.expect(found_distinct);
}

test "sibling object-literal properties survive after nested object value" {
    // Regression: the closing `}` of `a: { b: 1 }` used to clobber the
    // expression flag, causing the next sibling colon (`c:`) to be
    // mis-stripped as a type annotation. With the brace stack the flag
    // is restored to its pre-`{` value on every `}`.
    const source =
        \\export const x = {
        \\  a: { b: 1 },
        \\  c: { d: 1, e: { f: 2 } }
        \\};
    ;
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings(source, result.code);
}

test "array of objects preserves sibling property colons" {
    const source =
        \\const arr = [
        \\  { method: "GET", path: "/aaa" },
        \\  { method: "POST", path: "/bbb" }
        \\];
    ;
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings(source, result.code);
}

test "destructuring pattern rename colons survive stripping" {
    // Regression: a `{` right after `const`/`let`/`var` opens a destructuring
    // pattern whose `c: renamed` is a rename, not a `: Type` annotation. The
    // stripper used to leave `in_expression` false inside the pattern, so every
    // rename colon after the first element was blanked as a type annotation,
    // leaving the renamed binding undeclared.
    const source =
        \\const { b, c: renamed } = obj;
        \\let { x: first, y: second } = pt;
    ;
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings(source, result.code);
}

test "type annotation after destructuring pattern still strips" {
    // Must NOT regress: the `: { a: number }` after the pattern is a real
    // type annotation on the binding and must be blanked.
    const source = "const { a }: { a: number } = obj;";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "{ a }") != null);
}

test "type annotation with object-typed value still strips" {
    // Must NOT regress: `: { foo: string }` on a declaration is a real
    // type annotation and the whole `: {...}` span must be blanked.
    const source = "const x: { foo: string } = { foo: \"y\" };";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    // The annotation is gone; the value object literal survives.
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": {") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "= { foo:") != null);
}

test "export distinct type stripped" {
    const allocator = std.testing.allocator;
    const source = "export distinct type UserId = string;";
    var result = try strip(allocator, source, .{});
    defer result.deinit();

    const trimmed = std.mem.trim(u8, result.code, " \n\r\t");
    try std.testing.expectEqual(@as(usize, 0), trimmed.len);

    const tm = &result.type_map;
    var found_distinct = false;
    for (tm.entries.items) |entry| {
        if (entry.kind == .distinct_type) {
            found_distinct = true;
            const name = tm.getNameText(entry) orelse "";
            try std.testing.expectEqualStrings("UserId", name);
        }
    }
    try std.testing.expect(found_distinct);
}

// ============================================================================
// Explicit Type Arguments on Calls
// ============================================================================

test "explicit type argument on call stripped" {
    // `f<number>(x)` must not survive as chained comparisons (f < number) > (x).
    const result = try strip(std.testing.allocator, "const r = f<number>(x);", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "<") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "f") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "(x);") != null);
}

test "several explicit type arguments on call stripped" {
    const result = try strip(std.testing.allocator, "const r = g<string, number>(a, b);", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "<") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "string") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "(a, b);") != null);
}

test "nested generic type arguments on call stripped" {
    const result = try strip(std.testing.allocator, "const r = h<Array<number>>(x);", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "<") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "Array") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "(x);") != null);
}

test "string literal type argument with angle bracket inside stripped" {
    const result = try strip(std.testing.allocator, "const r = f<\"a>b\">(x);", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "<") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "a>b") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "(x);") != null);
}

test "explicit type argument on method call stripped" {
    const result = try strip(std.testing.allocator, "const r = obj.pick<string>(key);", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "<") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "obj.pick") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "(key);") != null);
}

test "comparison chain is not treated as type arguments" {
    const source = "const r = a < b > c;";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings(source, result.code);
}

test "comparison with logical operator is not treated as type arguments" {
    // `i<j && k>(l)` reads as (i < j) && (k > (l)); `&&` cannot appear in a
    // type-argument list, so the probe must reject it.
    const source = "const ok = i<j && k>(l);";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings(source, result.code);
}

test "ternary between comparisons is not treated as type arguments" {
    // `t ? a<b : c>(d)` is (t ? (a < b) : (c > (d))); a top-level `:` cannot
    // appear in a type-argument list.
    const source = "const v = t ? a<b : c>(d);";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings(source, result.code);
}

test "comparison whose right operand is not a call is unchanged" {
    const source = "const r = x<y>z;";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings(source, result.code);
}

test "block comment inside call type arguments stripped" {
    const result = try strip(std.testing.allocator, "const r = f</* c: a<b */ number>(x);", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "/*") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "(x);") != null);
}

// ============================================================================
// Comments Inside Type Annotations
// ============================================================================

test "block comment inside variable annotation stripped" {
    // The `<` and `:` inside the comment must not derail the type skipper.
    const result = try strip(std.testing.allocator, "let x: number /* note: a<b */ = 2;", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "/*") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "let x") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "= 2;") != null);
}

test "block comment with brace inside variable annotation stripped" {
    const result = try strip(std.testing.allocator, "let x: number /* { */ = 1;", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "{") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "= 1;") != null);
}

test "line comment inside variable annotation stripped" {
    const source = "let x: number // note: a<b\n  = 1;";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "//") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "= 1;") != null);
}

test "block comment inside param annotation stripped" {
    const source = "function f(a: number /* c: x<y */, b: string) { return b; }";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": string") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ", b") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "return b;") != null);
}

test "block comment inside generic annotation stripped" {
    const result = try strip(std.testing.allocator, "let x: Array</* a<b */ number> = [];", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "Array") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "/*") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "= [];") != null);
}
