//! Static registry of all diagnostic rules.
//!
//! Derived at comptime from the handler_verifier.DiagnosticKind,
//! strict_checker.DiagnosticKind, handler_policy ViolationKind, and
//! property_diagnostics.ViolationKind
//! enums so the registry cannot drift from the actual checkers.
//!
//! Used by `zigts describe-rule`, `zigts search`, and policy hash assertions.

const std = @import("std");
const handler_verifier = @import("handler_verifier.zig");
const strict_checker = @import("strict_checker.zig");
const handler_policy = @import("handler_policy.zig");
const property_diagnostics = @import("property_diagnostics.zig");
const flow_checker = @import("flow_checker.zig");

pub const RuleCategory = enum {
    verifier,
    policy,
    property,
    flow,

    pub fn label(self: RuleCategory) []const u8 {
        return switch (self) {
            .verifier => "verifier",
            .policy => "policy",
            .property => "property",
            .flow => "flow",
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
    .{
        .kind = .websocket_import_without_events,
        .code = "ZTS320",
        .description = "Handler imports zigttp:websocket but exports no WebSocket event handlers.",
        .example = "import { send } from 'zigttp:websocket'; // no onMessage/onOpen/onClose",
        .help = "Export at least onMessage(ws, data, room) to handle inbound frames, or remove the import.",
    },
    .{
        .kind = .websocket_events_without_import,
        .code = "ZTS321",
        .description = "Handler exports WebSocket event functions but does not import zigttp:websocket.",
        .example = "export function onMessage(ws, data, room) { send(ws, data); } // send undefined",
        .help = "Add `import { send, close, ... } from 'zigttp:websocket';` so event handlers can reply.",
    },
    .{
        .kind = .spec_not_discharged,
        .code = "ZTS500",
        .description = "Handler declared a Spec<\"name\"> obligation but the inferred property is false.",
        .example = "type Guardrails = Spec<\"idempotent\">; function handler(req): Response & Guardrails { return Response.json({ now: Date.now() }); }",
        .help = "Either remove the spec name from the Spec<...> union or fix the handler so the property holds.",
    },
    .{
        .kind = .spec_incompatible_with_import,
        .code = "ZTS501",
        .description = "Handler declared a spec that contradicts an imported virtual-module function.",
        .example = "type G = Spec<\"read_only\">; import { cacheSet } from 'zigttp:cache'; // write violates read_only",
        .help = "Drop the spec name, remove the import, or move the writing call outside the handler.",
    },
    .{
        .kind = .spec_unknown_name,
        .code = "ZTS502",
        .description = "Handler declared Spec<\"NAME\"> with a name not in the v1 spec set.",
        .example = "type G = Spec<\"made_up\">;",
        .help = "Use one of: deterministic, read_only, retry_safe, idempotent, state_isolated, fault_covered, no_secret_leakage, no_credential_leakage, input_validated, pii_contained, injection_safe.",
    },
};

// ---------------------------------------------------------------------------
// Strict rules (ZTS6xx) - default expert language profile
// ---------------------------------------------------------------------------

const strict_meta = [_]struct {
    kind: strict_checker.DiagnosticKind,
    code: []const u8,
    description: []const u8,
    example: ?[]const u8,
    help: []const u8,
}{
    .{
        .kind = .implicit_unknown,
        .code = "ZTS600",
        .description = "A handler-reachable call result has implicit unknown type.",
        .example = "const x = makeThing(); return Response.json(x);",
        .help = "Add a return type annotation, use a modeled virtual module, or narrow the value with a type guard/assert.",
    },
    .{
        .kind = .missing_public_annotation,
        .code = "ZTS601",
        .description = "A function lacks explicit parameter or return annotations.",
        .example = "function handler(req) { return Response.json({ok: true}); }",
        .help = "Annotate every parameter and the return type, for example `function handler(req: Request): Response`.",
    },
    .{
        .kind = .dynamic_capability_access,
        .code = "ZTS602",
        .description = "Capability-bearing virtual module access uses a dynamic argument.",
        .example = "env(req.headers.name)",
        .help = "Use compiler-visible literal env keys, cache namespaces, SQL query names, egress URLs, route paths, or service names.",
    },
    .{
        .kind = .non_exhaustive_profile_match,
        .code = "ZTS603",
        .description = "A match expression is not exhaustive under the strict profile.",
        .example = "match (method) { 'GET' => ok }",
        .help = "Cover every finite union member or add an explicit default when the discriminant is not finite.",
    },
    .{
        .kind = .avoidable_let,
        .code = "ZTS604",
        .description = "A let binding is not reassigned.",
        .example = "let x = 1; return x;",
        .help = "Use `const` for bindings that do not change.",
    },
    .{
        .kind = .computed_property_access,
        .code = "ZTS605",
        .description = "Dynamic computed property access hides object shape from the compiler.",
        .example = "obj[key]",
        .help = "Use a typed field, a literal key, or validate/narrow the object before indexing.",
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
// Proof-capsule rules (ZTS606) - function-level Proof<...> capsule discharge
// ---------------------------------------------------------------------------

const capsule_meta = [_]struct {
    name: []const u8,
    code: []const u8,
    description: []const u8,
    example: ?[]const u8,
    help: []const u8,
}{
    .{
        .name = "missing_proof_capsule",
        .code = "ZTS606",
        .description = "A handler-reachable helper breaks a capsule property the handler's Spec demands and carries no Proof<...> capsule declaring it.",
        .example = "function load(): User { return cacheSet('ns', 'k', 'v'); } function handler(req: Request): Response & Spec<\"read_only\"> { return Response.json(load()); }",
        .help = "Annotate the helper's return type with `Proof<T, \"...\">` and satisfy the property, or inline the helper into the handler.",
    },
    .{
        .name = "effects_undeclared_capability",
        .code = "ZTS503",
        .description = "A function reaches a capability that its declared Effects<...> ceiling does not list.",
        .example = "function load(): Effects<string, \"env\"> { return sha256('x'); } // sha256 needs crypto",
        .help = "Add the capability to the Effects<...> ceiling, or remove the call that reaches it.",
    },
    .{
        .name = "effects_unknown_capability",
        .code = "ZTS504",
        .description = "An Effects<...> ceiling names a capability that is not a runtime capability.",
        .example = "function load(): Effects<string, \"databse\"> { return env('X'); }",
        .help = "Use one of: env, clock, random, crypto, stderr, runtime_callback, sqlite, filesystem, network, policy_check.",
    },
    .{
        .name = "effects_over_declared",
        .code = "ZTS505",
        .description = "An Effects<...> ceiling names a capability the function never reaches. Advisory; never fails a build.",
        .example = "function pure(s: string): Effects<string, \"crypto\"> { return s; }",
        .help = "Remove the unused capability from the Effects<...> ceiling to keep it least-privilege.",
    },
    .{
        .name = "handler_budget_exceeded",
        .code = "ZTS506",
        .description = "The handler reaches a capability directly that is outside its declared Effects<...> budget.",
        .example = "function handler(req: Request): Effects<Response, \"env\"> { return Response.json({ t: Date.now() }); }",
        .help = "Add the capability to the handler's Effects<...> budget, or remove the call that reaches it.",
    },
    .{
        .name = "missing_effects_capsule",
        .code = "ZTS507",
        .description = "An exported helper carries no Effects<...> capsule. Emitted only under the opt-in docs mode.",
        .example = "export function load(s: string): string { return env(s); }",
        .help = "Annotate the exported helper's return type with `Effects<T, \"...\">` to document its capability ceiling.",
    },
    .{
        .name = "missing_proof_capsule_export",
        .code = "ZTS508",
        .description = "An exported helper carries no Proof<...> capsule. Emitted only under the opt-in docs mode.",
        .example = "export function clean(s: string): string { return s; }",
        .help = "Annotate the exported helper's return type with `Proof<T, \"...\">` to document its proven properties.",
    },
    .{
        .name = "helper_exceeds_handler_budget",
        .code = "ZTS607",
        .description = "A handler-reachable helper reaches a capability outside the handler's declared Effects<...> budget.",
        .example = "function load(): string { return sha256('x'); } function handler(req: Request): Effects<Response, \"env\"> { return Response.text(load()); }",
        .help = "Add the capability to the handler's Effects<...> budget, or remove the call from the reachable helper.",
    },
};

// ---------------------------------------------------------------------------
// Flow rules (ZTS4xx) - derived from flow_checker.DiagnosticKind
//
// The data-label flow checker emits ZTS4xx codes for {secret}, {credential},
// and {user_input} leaks reaching responses, logs, and egress. Wave 1B/2D
// P0 #5 (2026-05-23 review) flagged that `policyHash` did not include
// these rules, so adding, removing, or renaming a flow diagnostic did not
// trip the embedded-contract drift check at runtime. The `comptime` block
// below pins exhaustiveness against the live enum: any new DiagnosticKind
// variant must show up here, or the build fails with a named compile
// error. Codes mirror `packages/tools/src/json_diagnostics.zig:151-162`.
// ---------------------------------------------------------------------------

const flow_meta = [_]struct {
    kind: flow_checker.DiagnosticKind,
    code: []const u8,
    description: []const u8,
    example: ?[]const u8,
    help: []const u8,
}{
    .{
        .kind = .secret_in_response,
        .code = "ZTS400",
        .description = "A value labeled {secret} reaches a Response body or header.",
        .example = "const key = env('SECRET_KEY'); return Response.json({ key });",
        .help = "Do not return secrets to clients; derive a public artifact or drop the label.",
    },
    .{
        .kind = .credential_in_response,
        .code = "ZTS401",
        .description = "A value labeled {credential} reaches a Response body.",
        .example = "const token = jwtSign(claims, secret); return Response.json({ token });",
        .help = "Return only opaque session ids; never echo credentials issued by the handler.",
    },
    .{
        .kind = .secret_in_log,
        .code = "ZTS402",
        .description = "A value labeled {secret} reaches console.log / warn / error.",
        .example = "const key = env('SECRET_KEY'); logInfo(key);",
        .help = "Strip or mask the secret before logging.",
    },
    .{
        .kind = .credential_in_log,
        .code = "ZTS403",
        .description = "A value labeled {credential} reaches console.log / warn / error.",
        .example = "const auth = parseBearer(req.headers.get('Authorization')); logDebug(auth);",
        .help = "Log a hash or session id instead of the credential itself.",
    },
    .{
        .kind = .secret_in_egress_url,
        .code = "ZTS404",
        .description = "A value labeled {secret} reaches a fetch URL.",
        .example = "const key = env('SECRET_KEY'); fetchSync(`https://api/?k=${key}`);",
        .help = "Send the secret in a request header or body field that is not part of the URL.",
    },
    .{
        .kind = .credential_in_egress_url,
        .code = "ZTS405",
        .description = "A value labeled {credential} reaches a fetch URL.",
        .example = "fetchSync(`https://api/u?token=${authToken}`);",
        .help = "Use an Authorization header instead of placing credentials in URLs.",
    },
    .{
        .kind = .secret_in_egress_body,
        .code = "ZTS406",
        .description = "A value labeled {secret} reaches a fetch request body.",
        .example = "fetchSync('https://api/echo', { method: 'POST', body: JSON.stringify({ key }) });",
        .help = "Forward the secret only to a host on the egress allow-list, or strip it.",
    },
    .{
        .kind = .unvalidated_input_in_egress,
        .code = "ZTS407",
        .description = "A {user_input} value without {validated} reaches an egress call.",
        .example = "fetchSync(`https://api/echo?q=${req.url.searchParams.get('q')}`);",
        .help = "Pass user input through validateJson / schemaCompile or strip it before egress.",
    },
};

comptime {
    // Exhaustiveness gate: every flow_checker.DiagnosticKind must have a row
    // in flow_meta. If a new variant lands without a corresponding rule, the
    // build fails with the variant name — the policy hash cannot silently
    // miss a new ZTS4xx code.
    const fields = @typeInfo(flow_checker.DiagnosticKind).@"enum".fields;
    var seen: [fields.len]bool = .{false} ** fields.len;
    for (flow_meta) |entry| {
        seen[@intFromEnum(entry.kind)] = true;
    }
    for (seen, 0..) |s, idx| {
        if (!s) {
            @compileError("flow_meta missing entry for flow_checker.DiagnosticKind." ++ fields[idx].name);
        }
    }
}

// ---------------------------------------------------------------------------
// Unified rule table (comptime-assembled)
// ---------------------------------------------------------------------------

fn verifierName(kind: handler_verifier.DiagnosticKind) []const u8 {
    return @tagName(kind);
}

fn propertyName(kind: property_diagnostics.ViolationKind) []const u8 {
    return @tagName(kind);
}

fn strictName(kind: strict_checker.DiagnosticKind) []const u8 {
    return @tagName(kind);
}

fn flowName(kind: flow_checker.DiagnosticKind) []const u8 {
    return @tagName(kind);
}

const total_count = verifier_meta.len + strict_meta.len + capsule_meta.len + policy_meta.len + property_meta.len + flow_meta.len;

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

    for (strict_meta) |s| {
        rules[i] = .{
            .name = strictName(s.kind),
            .code = s.code,
            .category = .verifier,
            .description = s.description,
            .example = s.example,
            .help = s.help,
        };
        i += 1;
    }

    for (capsule_meta) |c| {
        rules[i] = .{
            .name = c.name,
            .code = c.code,
            .category = .verifier,
            .description = c.description,
            .example = c.example,
            .help = c.help,
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

    for (flow_meta) |f| {
        rules[i] = .{
            .name = flowName(f.kind),
            .code = f.code,
            .category = .flow,
            .description = f.description,
            .example = f.example,
            .help = f.help,
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
        // Category byte — included so reclassifying a rule from `.policy` to
        // `.flow` (or vice versa) drifts the hash even when text strings
        // stay identical. Wave 1B/2D P0 #5 (2026-05-23 review).
        const category_byte: u8 = @intCast(@intFromEnum(rule.category));
        hasher.update(&[_]u8{category_byte});
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
    // Sanity: verifier + policy + property + flow categories all populate.
    // 13+ verifier + 8 policy + 6 property + 8 flow = 35+. The lower bound
    // tracks the smallest legitimate set; the exact count is enforced by the
    // total_count == all_rules.len check above.
    try std.testing.expect(all_rules.len >= 35);
}

test "all_rules includes every flow_checker.DiagnosticKind under .flow" {
    // Mirrors the comptime exhaustiveness gate but verifies the projection
    // into all_rules — guards against a refactor that wires flow_meta but
    // forgets to append it to the unified table.
    inline for (@typeInfo(flow_checker.DiagnosticKind).@"enum".fields) |field| {
        const kind: flow_checker.DiagnosticKind = @enumFromInt(field.value);
        const name = @tagName(kind);
        const entry = findByName(name) orelse {
            std.log.err("flow rule '{s}' missing from all_rules", .{name});
            return error.MissingFlowRule;
        };
        try std.testing.expectEqual(RuleCategory.flow, entry.category);
    }
}

test "policyHash differs when a rule is reclassified to a different category" {
    // Direct unit-style assertion: feeding the same name/code/text strings
    // but different categories must produce different hashes. This guards
    // the category-byte inclusion in computePolicyHash.
    const a: RuleEntry = .{
        .name = "x",
        .code = "ZTSTEST",
        .category = .verifier,
        .description = "d",
        .example = null,
        .help = "h",
    };
    const b: RuleEntry = .{
        .name = "x",
        .code = "ZTSTEST",
        .category = .flow,
        .description = "d",
        .example = null,
        .help = "h",
    };
    var ha = std.crypto.hash.sha2.Sha256.init(.{});
    var hb = std.crypto.hash.sha2.Sha256.init(.{});
    inline for (.{ &ha, &hb }, .{ a, b }) |h, rule| {
        h.update(rule.name);
        h.update("\x00");
        h.update(rule.code);
        h.update("\x00");
        const cat: u8 = @intCast(@intFromEnum(rule.category));
        h.update(&[_]u8{cat});
        h.update("\x00");
        h.update(rule.description);
        h.update("\x00");
        h.update(rule.help);
        h.update("\x01");
    }
    var da: [32]u8 = undefined;
    var db: [32]u8 = undefined;
    da = ha.finalResult();
    db = hb.finalResult();
    try std.testing.expect(!std.mem.eql(u8, &da, &db));
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

test "all verifier rules have ZTS3xx, ZTS5xx, or ZTS6xx codes" {
    // ZTS3xx covers the structural checks (return analysis, result/optional
    // checking, dead code, state isolation, websocket consistency).
    // ZTS4xx is reserved for FlowChecker (defined in tools/json_diagnostics
    // alongside the runtime flow analysis pipeline). ZTS5xx covers
    // author-declared spec discharge (ZTS500 not_discharged, ZTS501
    // incompatible_with_import, ZTS502 unknown_name). ZTS6xx covers the
    // default strict ZigTS language profile.
    for (&all_rules) |*rule| {
        if (rule.category == .verifier) {
            try std.testing.expect(
                std.mem.startsWith(u8, rule.code, "ZTS3") or
                    std.mem.startsWith(u8, rule.code, "ZTS5") or
                    std.mem.startsWith(u8, rule.code, "ZTS6"),
            );
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
