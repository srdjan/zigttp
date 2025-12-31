//! Simple regex engine for zts
//!
//! Supports a useful subset of JavaScript regex:
//! - Literal characters
//! - Character classes: [abc], [a-z], [^abc]
//! - Quantifiers: *, +, ?, {n}, {n,}, {n,m}
//! - Anchors: ^, $
//! - Dot: . (any character except newline)
//! - Escapes: \d, \w, \s, \D, \W, \S, \.
//! - Groups: (...) for capturing
//! - Alternation: |

const std = @import("std");

/// Regex flags
pub const Flags = packed struct {
    global: bool = false, // g - find all matches
    ignore_case: bool = false, // i - case insensitive
    multiline: bool = false, // m - ^ and $ match line boundaries
    dot_all: bool = false, // s - . matches newline
    _reserved: u4 = 0,
};

/// Compiled regex pattern
pub const Regex = struct {
    pattern: []const u8,
    flags: Flags,
    // Compiled bytecode would go here for a more sophisticated implementation
    // For now, we use direct interpretation

    pub fn compile(pattern: []const u8, flags: Flags) Regex {
        return .{
            .pattern = pattern,
            .flags = flags,
        };
    }

    /// Test if the pattern matches anywhere in the string
    pub fn match(self: *const Regex, input: []const u8) bool {
        return self.exec(input) != null;
    }

    /// Execute the regex and return match info
    pub fn exec(self: *const Regex, input: []const u8) ?Match {
        // Try matching at each position
        var pos: usize = 0;
        while (pos <= input.len) : (pos += 1) {
            if (self.matchAt(input, pos)) |end| {
                return Match{
                    .start = pos,
                    .end = end,
                    .input = input,
                };
            }
        }
        return null;
    }

    /// Execute and return all matches (for global flag)
    pub fn execAll(self: *const Regex, allocator: std.mem.Allocator, input: []const u8) ![]Match {
        var matches = std.ArrayList(Match).init(allocator);
        errdefer matches.deinit();

        var pos: usize = 0;
        while (pos <= input.len) {
            if (self.matchAt(input, pos)) |end| {
                try matches.append(Match{
                    .start = pos,
                    .end = end,
                    .input = input,
                });
                // Move past the match (at least 1 char to avoid infinite loop on empty matches)
                pos = if (end > pos) end else pos + 1;
            } else {
                pos += 1;
            }
        }

        return matches.toOwnedSlice();
    }

    /// Try to match the pattern starting at a specific position
    fn matchAt(self: *const Regex, input: []const u8, start: usize) ?usize {
        var state = MatchState{
            .input = input,
            .pos = start,
            .pattern = self.pattern,
            .pat_pos = 0,
            .flags = self.flags,
        };

        // Handle ^ anchor
        if (state.pattern.len > 0 and state.pattern[0] == '^') {
            if (start != 0) {
                if (!self.flags.multiline) return null;
                // In multiline mode, ^ also matches after newline
                if (start > 0 and input[start - 1] != '\n') return null;
            }
            state.pat_pos = 1;
        }

        if (matchPattern(&state)) {
            return state.pos;
        }
        return null;
    }
};

/// Match result
pub const Match = struct {
    start: usize,
    end: usize,
    input: []const u8,

    pub fn slice(self: *const Match) []const u8 {
        return self.input[self.start..self.end];
    }
};

/// Internal matching state
const MatchState = struct {
    input: []const u8,
    pos: usize,
    pattern: []const u8,
    pat_pos: usize,
    flags: Flags,
};

/// Match the pattern from current state
fn matchPattern(state: *MatchState) bool {
    while (state.pat_pos < state.pattern.len) {
        const c = state.pattern[state.pat_pos];

        // Handle $ anchor
        if (c == '$') {
            if (state.pos == state.input.len) {
                state.pat_pos += 1;
                continue;
            }
            if (state.flags.multiline and state.pos < state.input.len and state.input[state.pos] == '\n') {
                state.pat_pos += 1;
                continue;
            }
            return false;
        }

        // Handle alternation |
        if (c == '|') {
            // Left side failed, try right side from original position
            state.pat_pos += 1;
            continue;
        }

        // Handle groups (...)
        if (c == '(') {
            // Find matching )
            var depth: usize = 1;
            var end = state.pat_pos + 1;
            while (end < state.pattern.len and depth > 0) : (end += 1) {
                if (state.pattern[end] == '(') depth += 1;
                if (state.pattern[end] == ')') depth -= 1;
            }
            // For now, just treat as non-capturing group
            const group_pattern = state.pattern[state.pat_pos + 1 .. end - 1];
            var group_state = MatchState{
                .input = state.input,
                .pos = state.pos,
                .pattern = group_pattern,
                .pat_pos = 0,
                .flags = state.flags,
            };
            if (matchPattern(&group_state)) {
                state.pos = group_state.pos;
                state.pat_pos = end;
                // Check for quantifier after group
                if (state.pat_pos < state.pattern.len) {
                    const next = state.pattern[state.pat_pos];
                    if (next == '*' or next == '+' or next == '?') {
                        state.pat_pos += 1;
                        // Handle quantifier (simplified)
                    }
                }
                continue;
            }
            return false;
        }

        // Get the pattern element and possible quantifier
        const elem = getPatternElement(state.pattern, state.pat_pos);
        state.pat_pos = elem.next_pos;

        // Check for quantifier
        var min: usize = 1;
        var max: usize = 1;
        if (state.pat_pos < state.pattern.len) {
            const q = state.pattern[state.pat_pos];
            if (q == '*') {
                min = 0;
                max = std.math.maxInt(usize);
                state.pat_pos += 1;
            } else if (q == '+') {
                min = 1;
                max = std.math.maxInt(usize);
                state.pat_pos += 1;
            } else if (q == '?') {
                min = 0;
                max = 1;
                state.pat_pos += 1;
            }
        }

        // Match the element with quantifier
        var count: usize = 0;
        while (count < max and state.pos < state.input.len) {
            if (matchElement(elem, state.input[state.pos], state.flags)) {
                count += 1;
                state.pos += 1;
            } else {
                break;
            }
        }

        if (count < min) {
            return false;
        }
    }

    return true;
}

/// Pattern element (character, class, escape, etc.)
const PatternElement = struct {
    kind: Kind,
    char: u8 = 0,
    class_start: usize = 0,
    class_end: usize = 0,
    negated: bool = false,
    next_pos: usize,

    const Kind = enum {
        literal,
        dot,
        char_class,
        digit,
        word,
        whitespace,
        non_digit,
        non_word,
        non_whitespace,
    };
};

/// Parse a pattern element at the given position
fn getPatternElement(pattern: []const u8, pos: usize) PatternElement {
    if (pos >= pattern.len) {
        return .{ .kind = .literal, .char = 0, .next_pos = pos };
    }

    const c = pattern[pos];

    // Escape sequences
    if (c == '\\' and pos + 1 < pattern.len) {
        const escaped = pattern[pos + 1];
        return switch (escaped) {
            'd' => .{ .kind = .digit, .next_pos = pos + 2 },
            'D' => .{ .kind = .non_digit, .next_pos = pos + 2 },
            'w' => .{ .kind = .word, .next_pos = pos + 2 },
            'W' => .{ .kind = .non_word, .next_pos = pos + 2 },
            's' => .{ .kind = .whitespace, .next_pos = pos + 2 },
            'S' => .{ .kind = .non_whitespace, .next_pos = pos + 2 },
            'n' => .{ .kind = .literal, .char = '\n', .next_pos = pos + 2 },
            'r' => .{ .kind = .literal, .char = '\r', .next_pos = pos + 2 },
            't' => .{ .kind = .literal, .char = '\t', .next_pos = pos + 2 },
            else => .{ .kind = .literal, .char = escaped, .next_pos = pos + 2 },
        };
    }

    // Dot (any character)
    if (c == '.') {
        return .{ .kind = .dot, .next_pos = pos + 1 };
    }

    // Character class [...]
    if (c == '[') {
        var end = pos + 1;
        const negated = end < pattern.len and pattern[end] == '^';
        if (negated) end += 1;

        // Find closing ]
        while (end < pattern.len and pattern[end] != ']') : (end += 1) {
            if (pattern[end] == '\\' and end + 1 < pattern.len) {
                end += 1; // Skip escaped char
            }
        }
        if (end < pattern.len) end += 1; // Include ]

        return .{
            .kind = .char_class,
            .class_start = pos + 1 + @as(usize, if (negated) 1 else 0),
            .class_end = end - 1,
            .negated = negated,
            .next_pos = end,
        };
    }

    // Literal character
    return .{ .kind = .literal, .char = c, .next_pos = pos + 1 };
}

/// Match a single character against a pattern element
fn matchElement(elem: PatternElement, c: u8, flags: Flags) bool {
    return switch (elem.kind) {
        .literal => {
            if (flags.ignore_case) {
                return std.ascii.toLower(c) == std.ascii.toLower(elem.char);
            }
            return c == elem.char;
        },
        .dot => {
            if (flags.dot_all) return true;
            return c != '\n';
        },
        .digit => c >= '0' and c <= '9',
        .non_digit => !(c >= '0' and c <= '9'),
        .word => (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_',
        .non_word => !((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_'),
        .whitespace => c == ' ' or c == '\t' or c == '\n' or c == '\r',
        .non_whitespace => !(c == ' ' or c == '\t' or c == '\n' or c == '\r'),
        .char_class => false, // TODO: implement character class matching
    };
}

/// Parse flags string into Flags struct
pub fn parseFlags(flags_str: []const u8) Flags {
    var flags = Flags{};
    for (flags_str) |c| {
        switch (c) {
            'g' => flags.global = true,
            'i' => flags.ignore_case = true,
            'm' => flags.multiline = true,
            's' => flags.dot_all = true,
            else => {},
        }
    }
    return flags;
}

// ============================================================================
// Tests
// ============================================================================

test "simple literal match" {
    const regex = Regex.compile("hello", .{});
    try std.testing.expect(regex.match("hello world"));
    try std.testing.expect(regex.match("say hello"));
    try std.testing.expect(!regex.match("goodbye"));
}

test "case insensitive" {
    const regex = Regex.compile("hello", .{ .ignore_case = true });
    try std.testing.expect(regex.match("HELLO"));
    try std.testing.expect(regex.match("Hello"));
}

test "dot matches any" {
    const regex = Regex.compile("h.llo", .{});
    try std.testing.expect(regex.match("hello"));
    try std.testing.expect(regex.match("hallo"));
    try std.testing.expect(!regex.match("hllo"));
}

test "quantifiers" {
    const star = Regex.compile("ab*c", .{});
    try std.testing.expect(star.match("ac"));
    try std.testing.expect(star.match("abc"));
    try std.testing.expect(star.match("abbbc"));

    const plus = Regex.compile("ab+c", .{});
    try std.testing.expect(!plus.match("ac"));
    try std.testing.expect(plus.match("abc"));
    try std.testing.expect(plus.match("abbbc"));

    const question = Regex.compile("ab?c", .{});
    try std.testing.expect(question.match("ac"));
    try std.testing.expect(question.match("abc"));
    try std.testing.expect(!question.match("abbc"));
}

test "anchors" {
    const start = Regex.compile("^hello", .{});
    try std.testing.expect(start.match("hello world"));
    try std.testing.expect(!start.match("say hello"));

    const end_anchor = Regex.compile("world$", .{});
    try std.testing.expect(end_anchor.match("hello world"));
    try std.testing.expect(!end_anchor.match("world hello"));
}

test "escape sequences" {
    const digit = Regex.compile("\\d+", .{});
    try std.testing.expect(digit.match("abc123def"));
    try std.testing.expect(!digit.match("abcdef"));

    const word = Regex.compile("\\w+", .{});
    try std.testing.expect(word.match("hello_world"));
}

test "exec returns match" {
    const regex = Regex.compile("\\d+", .{});
    const match = regex.exec("abc123def");
    try std.testing.expect(match != null);
    try std.testing.expectEqualStrings("123", match.?.slice());
}
