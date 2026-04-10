//! Static registry of all diagnostic rules.
//!
//! Derived at comptime from the handler_verifier.DiagnosticKind,
//! handler_policy ViolationKind, and property_diagnostics.ViolationKind
//! enums so the registry cannot drift from the actual checkers.
//!
//! Used by `zigts describe-rule`, `zigts search`, and policy hash assertions.

const std = @import("std");
const handler_verifier = @import("handler_verifier.zig");
const handler_policy = @import("handler_policy.zig");
const property_diagnostics = @import("property_diagnostics.zig");

pub const RuleCategory = enum {
    verifier,
    policy,
    property,

    pub fn label(self: RuleCategory) []const u8 {
        return switch (self) {
            .verifier => "verifier",
            .policy => "policy",
            .property => "property",
        };
    }
};

pub const RuleEntry = struct {
    name: []const u8,
    code: []const u8,
    category: RuleCategory,
    description: []const u8,
    example: ?[]const u8,
    help: []const u8,
};

// ---------------------------------------------------------------------------
// Verifier rules (ZTS3xx) - derived from handler_verifier.DiagnosticKind
// ---------------------------------------------------------------------------

const verifier_meta = [_]struct {
    kind: handler_verifier.DiagnosticKind,
    code: []const u8,
    description: []const u8,
    example: ?[]const u8,
    help: []const u8,
}{
    .{
        .kind = .missing_return_else,
        .code = "ZTS300",
        .description = "An if/else branch does not return a Response.",
        .example = "if (cond) { return Response.json({ok: true}); } // else missing",
        .help = "Add a return statement to the else branch.",
    },
    .{
        .kind = .missing_return_default,
        .code = "ZTS301",
        .description = "A match expression lacks a default arm that returns a Response.",
        .example = "match (x) { 'a' => Response.text('a') } // no default",
        .help = "Add a default arm: _ => Response.text('not found', {status: 404})",
    },
    .{
        .kind = .missing_return_path,
        .code = "ZTS302",
        .description = "A code path through the handler does not return a Response.",
        .example = null,
        .help = "Ensure every code path returns a Response.",
    },
    .{
        .kind = .unchecked_result_value,
        .code = "ZTS303",
        .description = "result.value is accessed without checking result.ok first.",
        .example = "const r = jwtVerify(token, secret); return Response.json(r.value);",
        .help = "Check result.ok before accessing result.value.",
    },
    .{
        .kind = .unreachable_after_return,
        .code = "ZTS304",
        .description = "Code appears after an unconditional return statement.",
        .example = "return Response.json({ok: true}); const x = 1;",
        .help = "Remove unreachable code after the return.",
    },
    .{
        .kind = .unused_variable,
        .code = "ZTS305",
        .description = "A variable is declared but never referenced.",
        .example = "const unused = 42;",
        .help = "Remove the unused variable or prefix with _.",
    },
    .{
        .kind = .unused_import,
        .code = "ZTS306",
        .description = "An import binding is never used.",
        .example = "import { sha256 } from 'zigttp:crypto'; // sha256 never used",
        .help = "Remove the unused import.",
    },
    .{
        .kind = .non_exhaustive_match,
        .code = "ZTS307",
        .description = "A match expression does not cover all possible values.",
        .example = "match (method) { 'GET' => ... } // missing POST, etc.",
        .help = "Add a default arm or cover all cases.",
    },
    .{
        .kind = .unchecked_optional_use,
        .code = "ZTS308",
        .description = "An optional value is used without narrowing (e.g. undefined check).",
        .example = "const val = obj.name; return Response.text(val); // val may be undefined",
        .help = "Check val !== undefined before using it.",
    },
    .{
        .kind = .unchecked_optional_access,
        .code = "ZTS309",
        .description = "A property on an optional value is accessed without narrowing.",
        .example = "const user = getUser(); return Response.text(user.name); // user may be undefined",
        .help = "Check the value is defined before accessing properties.",
    },
    .{
        .kind = .module_scope_mutation,
        .code = "ZTS310",
        .description = "Handler body mutates a module-scope variable, breaking request isolation.",
        .example = "let count = 0; function handler(req) { count = count + 1; ... }",
        .help = "Move mutable state inside the handler or use zigttp:cache.",
    },
};

// ---------------------------------------------------------------------------
// Policy rules (POL0xx) - derived from handler_policy combinations
// ---------------------------------------------------------------------------

const policy_meta = [_]struct {
    name: []const u8,
    code: []const u8,
    description: []const u8,
    help: []const u8,
}{
    .{
        .name = "env_literal_not_allowed",
        .code = "POL001",
        .description = "Handler accesses an env var not in the policy allow-list.",
        .help = "Add the env var to the policy file or remove the access.",
    },
    .{
        .name = "env_dynamic_not_allowed",
        .code = "POL002",
        .description = "Handler uses dynamic (computed) env access when policy requires static-only.",
        .help = "Use literal env var names instead of computed access.",
    },
    .{
        .name = "egress_literal_not_allowed",
        .code = "POL003",
        .description = "Handler calls an outbound host not in the policy allow-list.",
        .help = "Add the host to the policy file or remove the fetch call.",
    },
    .{
        .name = "egress_dynamic_not_allowed",
        .code = "POL004",
        .description = "Handler uses dynamic (computed) outbound URLs when policy requires static-only.",
        .help = "Use literal host names instead of computed URLs.",
    },
    .{
        .name = "cache_literal_not_allowed",
        .code = "POL005",
        .description = "Handler accesses a cache namespace not in the policy allow-list.",
        .help = "Add the namespace to the policy file or remove the cache access.",
    },
    .{
        .name = "cache_dynamic_not_allowed",
        .code = "POL006",
        .description = "Handler uses dynamic (computed) cache namespace when policy requires static-only.",
        .help = "Use literal cache namespace names.",
    },
    .{
        .name = "sql_literal_not_allowed",
        .code = "POL007",
        .description = "Handler executes a SQL query name not in the policy allow-list.",
        .help = "Add the query name to the policy file.",
    },
    .{
        .name = "sql_dynamic_not_allowed",
        .code = "POL008",
        .description = "Handler uses dynamic (computed) SQL query names when policy requires static-only.",
        .help = "Use literal query names.",
    },
};

// ---------------------------------------------------------------------------
// Property rules (PROP0x) - derived from property_diagnostics.ViolationKind
// ---------------------------------------------------------------------------

const property_meta = [_]struct {
    kind: property_diagnostics.ViolationKind,
    code: []const u8,
    description: []const u8,
    help: []const u8,
}{
    .{
        .kind = .fault_uncovered,
        .code = "PROP01",
        .description = "A critical I/O call (auth/validation) has a failure path that returns 2xx.",
        .help = "Handle the failure case and return an appropriate error status.",
    },
    .{
        .kind = .injection_unsafe,
        .code = "PROP02",
        .description = "Unvalidated user input reaches a sensitive sink (fetchSync, Response.html).",
        .help = "Validate or sanitize user input before passing to sinks.",
    },
    .{
        .kind = .secret_leakage,
        .code = "PROP03",
        .description = "Secret env var data flows to a response body, log, or egress URL.",
        .help = "Do not include secret values in responses or logs.",
    },
    .{
        .kind = .credential_leakage,
        .code = "PROP04",
        .description = "Credential data (auth token, JWT) flows to a response body or log.",
        .help = "Do not echo credentials back in responses.",
    },
    .{
        .kind = .result_unsafe,
        .code = "PROP05",
        .description = "result.value accessed without checking result.ok first.",
        .help = "Check result.ok before accessing result.value.",
    },
    .{
        .kind = .optional_unchecked,
        .code = "PROP06",
        .description = "Optional value used without narrowing (undefined check).",
        .help = "Check the value is defined before using it.",
    },
};

// ---------------------------------------------------------------------------
// Unified rule table (comptime-assembled)
// ---------------------------------------------------------------------------

fn verifierName(kind: handler_verifier.DiagnosticKind) []const u8 {
    return @tagName(kind);
}

fn propertyName(kind: property_diagnostics.ViolationKind) []const u8 {
    return @tagName(kind);
}

const total_count = verifier_meta.len + policy_meta.len + property_meta.len;

pub const all_rules: [total_count]RuleEntry = blk: {
    var rules: [total_count]RuleEntry = undefined;
    var i: usize = 0;

    for (verifier_meta) |v| {
        rules[i] = .{
            .name = verifierName(v.kind),
            .code = v.code,
            .category = .verifier,
            .description = v.description,
            .example = v.example,
            .help = v.help,
        };
        i += 1;
    }

    for (policy_meta) |p| {
        rules[i] = .{
            .name = p.name,
            .code = p.code,
            .category = .policy,
            .description = p.description,
            .example = null,
            .help = p.help,
        };
        i += 1;
    }

    for (property_meta) |p| {
        rules[i] = .{
            .name = propertyName(p.kind),
            .code = p.code,
            .category = .property,
            .description = p.description,
            .example = null,
            .help = p.help,
        };
        i += 1;
    }

    break :blk rules;
};

// ---------------------------------------------------------------------------
// Lookup functions
// ---------------------------------------------------------------------------

pub fn findByName(name: []const u8) ?*const RuleEntry {
    for (&all_rules) |*rule| {
        if (std.mem.eql(u8, rule.name, name)) return rule;
    }
    return null;
}

pub fn findByCode(code: []const u8) ?*const RuleEntry {
    for (&all_rules) |*rule| {
        if (std.mem.eql(u8, rule.code, code)) return rule;
    }
    return null;
}

pub const SearchResults = struct {
    buf: [total_count]usize = undefined,
    len: usize = 0,

    pub fn constSlice(self: *const SearchResults) []const usize {
        return self.buf[0..self.len];
    }
};

/// Returns indices into all_rules where `keyword` is a substring of name,
/// description, or help. Stack-allocated, no heap.
pub fn search(keyword: []const u8) SearchResults {
    var results = SearchResults{};

    for (&all_rules, 0..) |*rule, idx| {
        if (containsSubstring(rule.name, keyword) or
            containsSubstring(rule.description, keyword) or
            containsSubstring(rule.help, keyword))
        {
            results.buf[results.len] = idx;
            results.len += 1;
        }
    }
    return results;
}

fn containsSubstring(haystack: []const u8, needle: []const u8) bool {
    if (needle.len == 0) return true;
    if (needle.len > haystack.len) return false;
    return std.mem.indexOf(u8, haystack, needle) != null;
}

// ---------------------------------------------------------------------------
// Policy hash (SHA-256 of sorted rule metadata)
// ---------------------------------------------------------------------------

/// Deterministic SHA-256 hash of all rule metadata.
/// Changes if and only if a rule is added, removed, or its metadata changes.
/// Cannot be comptime due to SHA-256 branch budget; cached on first call.
var cached_hash: ?[64]u8 = null;

pub fn policyHash() [64]u8 {
    if (cached_hash) |h| return h;
    cached_hash = computePolicyHash();
    return cached_hash.?;
}

fn computePolicyHash() [64]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});

    for (&all_rules) |*rule| {
        hasher.update(rule.name);
        hasher.update("\x00");
        hasher.update(rule.code);
        hasher.update("\x00");
        hasher.update(rule.description);
        hasher.update("\x00");
        hasher.update(rule.help);
        hasher.update("\x01");
    }

    const digest = hasher.finalResult();
    return std.fmt.bytesToHex(digest, .lower);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "all_rules has expected count" {
    try std.testing.expectEqual(total_count, all_rules.len);
    // Sanity: at least 11 verifier + 8 policy + 6 property = 25
    try std.testing.expect(all_rules.len >= 25);
}

test "findByName returns entry" {
    const entry = findByName("unchecked_result_value");
    try std.testing.expect(entry != null);
    try std.testing.expectEqualStrings("ZTS303", entry.?.code);
    try std.testing.expectEqual(RuleCategory.verifier, entry.?.category);
}

test "findByCode returns entry" {
    const entry = findByCode("ZTS303");
    try std.testing.expect(entry != null);
    try std.testing.expectEqualStrings("unchecked_result_value", entry.?.name);
}

test "findByName unknown returns null" {
    try std.testing.expect(findByName("nonexistent_rule") == null);
}

test "findByCode unknown returns null" {
    try std.testing.expect(findByCode("ZTS999") == null);
}

test "search matches substring" {
    const results = search("result");
    try std.testing.expect(results.len >= 2);
}

test "search no matches" {
    const results = search("xyznonexistent");
    try std.testing.expectEqual(@as(usize, 0), results.len);
}

test "policyHash is deterministic and 64 hex chars" {
    const hash1 = policyHash();
    const hash2 = policyHash();
    try std.testing.expectEqualStrings(&hash1, &hash2);
    try std.testing.expectEqual(@as(usize, 64), hash1.len);
}

test "all verifier rules have ZTS3xx codes" {
    for (&all_rules) |*rule| {
        if (rule.category == .verifier) {
            try std.testing.expect(std.mem.startsWith(u8, rule.code, "ZTS3"));
        }
    }
}

test "all policy rules have POL codes" {
    for (&all_rules) |*rule| {
        if (rule.category == .policy) {
            try std.testing.expect(std.mem.startsWith(u8, rule.code, "POL"));
        }
    }
}

test "all property rules have PROP codes" {
    for (&all_rules) |*rule| {
        if (rule.category == .property) {
            try std.testing.expect(std.mem.startsWith(u8, rule.code, "PROP"));
        }
    }
}
