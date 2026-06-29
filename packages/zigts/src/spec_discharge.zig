//! Spec discharge: takes a contract's declared specs, classified
//! HandlerProperties, and module imports, and emits the per-spec diagnostics
//! that the verifier surfaces as ZTS500 / ZTS501 / ZTS502.
//!
//! Lives next to (not inside) handler_verifier so it can run AFTER the
//! contract is fully built. The verifier runs on the IR before properties
//! are classified; spec discharge needs the classified output, the declared
//! spec set extracted from the handler return type, and the import list to
//! catch contradictions.

const std = @import("std");
const contract_types = @import("contract_types.zig");
const module_binding = @import("module_binding.zig");

const HandlerProperties = contract_types.HandlerProperties;
const PropertyCause = contract_types.PropertyCause;
pub const SpecDiagnostic = contract_types.SpecDiagnostic;

/// The v1 spec name set. Each entry maps to a boolean field on
/// HandlerProperties. Names outside this set produce ZTS502.
pub const v1_specs = [_]V1Spec{
    .{ .name = "deterministic", .field = .deterministic, .cause_only = true },
    .{ .name = "read_only", .field = .read_only, .cause_only = true },
    .{ .name = "retry_safe", .field = .retry_safe, .cause_only = true },
    .{ .name = "idempotent", .field = .idempotent, .cause_only = true },
    .{ .name = "state_isolated", .field = .state_isolated, .cause_only = true },
    .{ .name = "fault_covered", .field = .fault_covered, .cause_only = true },
    .{ .name = "pure", .field = .pure, .cause_only = true },
    .{ .name = "stateless", .field = .stateless, .cause_only = true },
    .{ .name = "result_safe", .field = .result_safe, .cause_only = true },
    .{ .name = "optional_safe", .field = .optional_safe, .cause_only = true },
    .{ .name = "no_secret_leakage", .field = .no_secret_leakage, .cause_only = false },
    .{ .name = "no_credential_leakage", .field = .no_credential_leakage, .cause_only = false },
    .{ .name = "input_validated", .field = .input_validated, .cause_only = false },
    .{ .name = "pii_contained", .field = .pii_contained, .cause_only = false },
    .{ .name = "injection_safe", .field = .injection_safe, .cause_only = false },
    .{ .name = "canonical", .field = .canonical, .cause_only = true },
};

pub const V1Spec = struct {
    name: []const u8,
    field: PropertyField,
    /// `true` for specs whose failure path produces only a PropertyCause (no
    /// counterexample witness). The HUD renders these with a per-property
    /// `Try: ...` suggestion to keep failures actionable.
    cause_only: bool,
};

pub const PropertyField = enum {
    deterministic,
    read_only,
    retry_safe,
    idempotent,
    state_isolated,
    fault_covered,
    pure,
    stateless,
    result_safe,
    optional_safe,
    no_secret_leakage,
    no_credential_leakage,
    input_validated,
    pii_contained,
    injection_safe,
    canonical,

    pub fn lookup(self: PropertyField, properties: HandlerProperties) bool {
        return switch (self) {
            .deterministic => properties.deterministic,
            .read_only => properties.read_only,
            .retry_safe => properties.retry_safe,
            .idempotent => properties.idempotent,
            .state_isolated => properties.state_isolated,
            .fault_covered => properties.fault_covered,
            .pure => properties.pure,
            .stateless => properties.stateless,
            .result_safe => properties.result_safe,
            .optional_safe => properties.optional_safe,
            .no_secret_leakage => properties.no_secret_leakage,
            .no_credential_leakage => properties.no_credential_leakage,
            .input_validated => properties.input_validated,
            .pii_contained => properties.pii_contained,
            .injection_safe => properties.injection_safe,
            .canonical => properties.canonical,
        };
    }
};

/// True when `name` is a v1 spec whose failure has no flow-style witness -
/// the verdict is a structural classifier output. Witness corpus seeding
/// for these flows through `witness_corpus.synthesizeStructural`; the
/// flow-rich half populates automatically through `flow_checker`.
pub fn isCauseOnly(name: []const u8) bool {
    for (v1_specs) |s| {
        if (s.cause_only and std.mem.eql(u8, s.name, name)) return true;
    }
    return false;
}

/// Per-property HUD suggestion strings. Mandatory for cause-only specs so
/// the proof card never renders an anemic single-line failure.
pub fn suggestionFor(name: []const u8) ?[]const u8 {
    if (std.mem.eql(u8, name, "deterministic")) {
        return "remove Date.now() / Math.random() or move the call inside a `durable.step`.";
    }
    if (std.mem.eql(u8, name, "read_only")) {
        return "remove writing calls to zigttp:cache / zigttp:sql, or drop `read_only` from your Spec set.";
    }
    if (std.mem.eql(u8, name, "retry_safe")) {
        return "wrap writes in `durable.step` so retried invocations replay deterministically.";
    }
    if (std.mem.eql(u8, name, "idempotent")) {
        return "remove non-deterministic operations or move writes inside a `durable.step`.";
    }
    if (std.mem.eql(u8, name, "state_isolated")) {
        return "move module-scope mutations into the handler body or behind zigttp:cache.";
    }
    if (std.mem.eql(u8, name, "fault_covered")) {
        return "add an explicit failure path for every critical I/O call site.";
    }
    if (std.mem.eql(u8, name, "pure")) {
        return "remove side effects (virtual-module calls, Date.now, Math.random) from the handler body.";
    }
    if (std.mem.eql(u8, name, "stateless")) {
        return "move module-scope mutable state into the handler body or behind zigttp:cache.";
    }
    if (std.mem.eql(u8, name, "result_safe")) {
        return "match every Result before unwrapping (no `.value` on an unchecked Result).";
    }
    if (std.mem.eql(u8, name, "optional_safe")) {
        return "guard every optional access with `?? fallback` or an explicit `if (x != undefined)` check.";
    }
    if (std.mem.eql(u8, name, "canonical")) {
        return "run `zigts normalize <file> --write` to rewrite the handler into Canonical Normal Form, or fix the flagged ZTS6xx constructs by hand.";
    }
    return null;
}

/// ZTS501 suggestion: actionable copy for `read_only` colliding with a
/// stateful module import. The current check is import-level, so keeping the
/// same module specifier will continue to fail even if only read functions are
/// used.
pub fn suggestionForIncompatible(spec_name: []const u8, module: []const u8) ?[]const u8 {
    if (std.mem.eql(u8, spec_name, "read_only")) {
        if (std.mem.eql(u8, module, "zigttp:cache")) {
            return "drop `read_only` from your Spec set, or remove the zigttp:cache import from this handler.";
        }
        if (std.mem.eql(u8, module, "zigttp:sql")) {
            return "drop `read_only` from your Spec set, or remove the zigttp:sql import from this handler.";
        }
    }
    return null;
}

/// v1 spec names, derived from `v1_specs` so the two cannot drift.
pub const v1_spec_names = blk: {
    var names: [v1_specs.len][]const u8 = undefined;
    for (v1_specs, 0..) |s, i| names[i] = s.name;
    break :blk names;
};

/// Nearest-match suggestion: when `name` is within Levenshtein distance 2 of
/// a candidate, return an owned `did you mean ...` string, else null. The
/// distance-2 cap keeps further misses from being more noise than signal.
fn nearestSuggestion(
    allocator: std.mem.Allocator,
    name: []const u8,
    candidates: []const []const u8,
) !?[]const u8 {
    var best_name: ?[]const u8 = null;
    var best_dist: usize = std.math.maxInt(usize);
    for (candidates) |cand| {
        const d = levenshtein(name, cand);
        if (d < best_dist) {
            best_dist = d;
            best_name = cand;
        }
    }
    if (best_name == null or best_dist > 2) return null;
    return try std.fmt.allocPrint(allocator, "did you mean `{s}`?", .{best_name.?});
}

/// ZTS502 suggestion for an unknown handler spec name. Caller frees.
pub fn suggestionForUnknown(allocator: std.mem.Allocator, name: []const u8) !?[]const u8 {
    return nearestSuggestion(allocator, name, &v1_spec_names);
}

/// Iterative Levenshtein distance with a single row buffer. Bounded
/// O(len(a) * len(b)) time and O(min(len(a), len(b))) auxiliary stack.
fn levenshtein(a: []const u8, b: []const u8) usize {
    if (a.len == 0) return b.len;
    if (b.len == 0) return a.len;
    // Keep the shorter on the columns to bound the stack buffer.
    const short = if (a.len <= b.len) a else b;
    const long = if (a.len <= b.len) b else a;

    var prev: [64]usize = undefined;
    if (short.len + 1 > prev.len) {
        // For pathologically long spec names fall back to len; the caller
        // caps at distance 2 anyway, so a saturated value just suppresses
        // the suggestion.
        return std.math.maxInt(usize);
    }
    for (0..short.len + 1) |i| prev[i] = i;

    for (long, 1..) |lc, i| {
        var prev_diag = prev[0];
        prev[0] = i;
        for (short, 1..) |sc, j| {
            const tmp = prev[j];
            const cost: usize = if (lc == sc) 0 else 1;
            prev[j] = @min(
                @min(prev[j] + 1, prev[j - 1] + 1),
                prev_diag + cost,
            );
            prev_diag = tmp;
        }
    }
    return prev[short.len];
}

const incompatible_modules_for_read_only = [_][]const u8{
    "zigttp:cache",
    "zigttp:sql",
};

/// True when any imported module forbids the `read_only` property at the import
/// level (the same set ZTS501 keys on). Used to keep the ZTS500 suggestion from
/// recommending a property the import would then reject, and by the edit-simulate
/// HUD to show the agent read_only as not-declarable for such handlers.
pub fn readOnlyForbiddenByImport(modules: []const []const u8) bool {
    for (incompatible_modules_for_read_only) |m| {
        if (containsString(modules, m)) return true;
    }
    return false;
}

/// Compute the set of diagnostics for a handler given its declared specs,
/// classified properties, and imported modules. The caller owns the
/// returned ArrayList and is responsible for calling `SpecDiagnostic.deinit`
/// on each entry before deinit-ing the list.
pub fn dischargeSpecs(
    allocator: std.mem.Allocator,
    declared: []const []const u8,
    properties: ?HandlerProperties,
    modules: []const []const u8,
    /// True when `declared` is the implicit default profile (the handler
    /// declared no `Spec<...>`). Every emitted ZTS500/ZTS501 then carries the
    /// "declare a narrow Spec" remedy instead of the per-property hint, and is
    /// flagged so downstream surfaces stop claiming a spec was "declared".
    implicit_default: bool,
) !std.ArrayList(SpecDiagnostic) {
    var out: std.ArrayList(SpecDiagnostic) = .empty;
    errdefer {
        for (out.items) |*d| @constCast(d).deinit(allocator);
        out.deinit(allocator);
    }

    // For the implicit default profile, every unsatisfiable property would
    // otherwise emit its own ZTS500 - a fan-out of byte-identical diagnostics
    // (same code/line/column/message) differing only in spec_name. The agent
    // re-feeds each one to the model every retry, inflating the veto signal.
    // Instead, collect the undischarged property names here and emit a SINGLE
    // collapsed ZTS500 after the loop whose suggestion names the specific
    // undischarged set plus the minimal Spec to declare. (ZTS502 cannot fire
    // in this mode - `declared` is the full v1 name set - so unknown-name and
    // collapsed-ZTS500 do not interleave.)
    var undischarged: std.ArrayList([]const u8) = .empty;
    defer undischarged.deinit(allocator);

    for (declared) |name| {
        const spec = lookupV1(name);

        if (spec == null) {
            const spec_name = try allocator.dupe(u8, name);
            errdefer allocator.free(spec_name);
            const suggestion = try suggestionForUnknown(allocator, name);
            errdefer if (suggestion) |s| allocator.free(s);
            try out.append(allocator, .{
                .kind = .unknown_name,
                .spec_name = spec_name,
                .suggestion = suggestion,
            });
            continue;
        }

        // ZTS501: read_only contradicts zigttp:cache or zigttp:sql imports.
        // Other v1 specs do not have hard import-level contradictions in v1.
        // When ZTS501 fires for a spec, skip ZTS500 for the same name -
        // the contradiction is the actionable error and the agent should
        // not try to repair the property until the import or the spec is
        // resolved.
        var emitted_incompat = false;
        if (spec.?.field == .read_only) {
            for (incompatible_modules_for_read_only) |module_name| {
                if (containsString(modules, module_name)) {
                    const spec_name = try allocator.dupe(u8, name);
                    errdefer allocator.free(spec_name);
                    const owned_module = try allocator.dupe(u8, module_name);
                    errdefer allocator.free(owned_module);
                    // The cache/sql contradiction is the real cause here, so
                    // the per-import remedy is accurate even under the implicit
                    // default profile.
                    const suggestion = if (suggestionForIncompatible(name, module_name)) |s|
                        try allocator.dupe(u8, s)
                    else
                        null;
                    errdefer if (suggestion) |s| allocator.free(s);
                    try out.append(allocator, .{
                        .kind = .incompatible_with_import,
                        .spec_name = spec_name,
                        .incompatible_module = owned_module,
                        .suggestion = suggestion,
                        .implicit_default = implicit_default,
                    });
                    emitted_incompat = true;
                    break;
                }
            }
        }
        if (emitted_incompat) continue;

        // ZTS500: discharge against the classified property, only if the
        // properties block is present (build-time classifier ran). When
        // properties is null we record a "not_discharged" with no cause -
        // that is still actionable: it tells the user the classifier did
        // not run for this build.
        const undischarged_here = blk: {
            const props = properties orelse break :blk true;
            break :blk !spec.?.field.lookup(props);
        };
        if (!undischarged_here) continue;

        if (implicit_default) {
            // Defer to the single collapsed diagnostic below.
            try undischarged.append(allocator, spec.?.name);
        } else {
            try appendNotDischarged(allocator, &out, name);
        }
    }

    // Emit the one collapsed ZTS500 for the implicit default profile. Its
    // spec_name carries the joined undischarged set so the JSON surface lists
    // the real properties, and its suggestion names them plus the minimal Spec.
    if (implicit_default and undischarged.items.len > 0) {
        const spec_name = try std.mem.join(allocator, ", ", undischarged.items);
        errdefer allocator.free(spec_name);
        const suggestion = try implicitDefaultSuggestion(allocator, properties, modules, undischarged.items);
        errdefer allocator.free(suggestion);
        try out.append(allocator, .{
            .kind = .not_discharged,
            .spec_name = spec_name,
            .suggestion = suggestion,
            .implicit_default = true,
        });
    }

    return out;
}

/// Append a per-spec ZTS500 for an explicitly declared Spec that the handler
/// proof did not discharge. The implicit-default profile does not use this -
/// it accumulates undischarged names and emits one collapsed diagnostic.
fn appendNotDischarged(
    allocator: std.mem.Allocator,
    out: *std.ArrayList(SpecDiagnostic),
    name: []const u8,
) !void {
    const spec_name = try allocator.dupe(u8, name);
    errdefer allocator.free(spec_name);
    const suggestion = if (suggestionFor(name)) |s|
        try allocator.dupe(u8, s)
    else
        null;
    errdefer if (suggestion) |s| allocator.free(s);
    try out.append(allocator, .{
        .kind = .not_discharged,
        .spec_name = spec_name,
        .suggestion = suggestion,
        .implicit_default = false,
    });
}

/// Build the actionable remedy shown for the single collapsed ZTS500 emitted
/// when the handler declares no `Spec<...>` (the IMPLICIT default profile).
/// Data-driven from `undischarged`, the exact set of default-profile properties
/// this handler does NOT hold: it names those properties, then offers the
/// minimal Spec to declare - the default-profile properties the handler DOES
/// hold. Caller owns the result.
fn implicitDefaultSuggestion(
    allocator: std.mem.Allocator,
    properties: ?HandlerProperties,
    modules: []const []const u8,
    undischarged: []const []const u8,
) ![]u8 {
    // The properties the handler still holds form the minimal Spec the author
    // can declare and have pass immediately. Read them straight off the
    // classified `properties` so we never recommend one the handler fails.
    // Without classified properties, fall back to the v1 profile minus the
    // undischarged set.
    const read_only_forbidden = readOnlyForbiddenByImport(modules);
    var holds: std.ArrayList(u8) = .empty;
    defer holds.deinit(allocator);
    var holds_count: usize = 0;
    for (v1_spec_names) |cn| {
        // Never suggest a property an imported module forbids at the import
        // level. The classifier marks read_only true for a SQL SELECT (it does
        // not write), but a zigttp:sql / zigttp:cache import means declaring
        // read_only trips ZTS501 - the exact contradiction (ZTS500 says "add
        // read_only", ZTS501 says "drop it") this suggestion must not create.
        if (read_only_forbidden and std.mem.eql(u8, cn, "read_only")) continue;
        const cn_holds = if (properties) |props| blk: {
            const spec = lookupV1(cn) orelse break :blk false;
            break :blk spec.field.lookup(props);
        } else !containsString(undischarged, cn);
        if (!cn_holds) continue;
        if (holds_count > 0) try holds.appendSlice(allocator, " | ");
        try holds.appendSlice(allocator, "\"");
        try holds.appendSlice(allocator, cn);
        try holds.appendSlice(allocator, "\"");
        holds_count += 1;
    }
    if (holds_count == 0) try holds.appendSlice(allocator, "\"injection_safe\"");

    // The undischarged properties, named so the author sees exactly what the
    // default profile demanded that this handler cannot prove.
    const missing = try std.mem.join(allocator, ", ", undischarged);
    defer allocator.free(missing);

    return std.fmt.allocPrint(
        allocator,
        "this handler declares no Spec<...>, so the compiler must prove the full " ++
            "default profile, but it does not hold: {s}. Declare a narrow Spec on the " ++
            "return type with only the properties it holds, e.g. " ++
            "`function handler(req: Request): Response & Spec<{s}>`.",
        .{ missing, holds.items },
    );
}

fn lookupV1(name: []const u8) ?V1Spec {
    for (v1_specs) |spec| {
        if (std.mem.eql(u8, spec.name, name)) return spec;
    }
    return null;
}

fn containsString(list: []const []const u8, target: []const u8) bool {
    for (list) |s| {
        if (std.mem.eql(u8, s, target)) return true;
    }
    return false;
}

// ---------------------------------------------------------------------------
// Proof-capsule discharge
//
// Function-level proof capsules (`Proof<T, S>`) discharge a smaller property
// set than handler `Spec<...>`: the four keystone properties whose facts come
// from per-function effect inference and path-return analysis. Capsule
// discharge reuses the SpecDiagnostic shape (ZTS500 / ZTS502) so helper
// failures flow through the same diagnostic channel as handler Spec failures.
// ZTS501 (import contradiction) has no capsule analogue: the per-function
// EffectRow knows precisely whether a helper writes, so the coarse
// import-level heuristic is unnecessary.
// ---------------------------------------------------------------------------

/// The v1 capsule property set. A keystone-scoped subset; flow properties
/// (no_secret_leakage, injection_safe, ...) join in a later slice.
pub const CapsuleProperty = enum {
    total,
    pure,
    read_only,
    deterministic,

    pub fn fromName(name: []const u8) ?CapsuleProperty {
        return std.meta.stringToEnum(CapsuleProperty, name);
    }
};

/// Capsule property names, derived from the enum so the two cannot drift.
pub const capsule_property_names = blk: {
    const fields = @typeInfo(CapsuleProperty).@"enum".fields;
    var names: [fields.len][]const u8 = undefined;
    for (fields, 0..) |f, i| names[i] = f.name;
    break :blk names;
};

/// Facts the compiler proved about one function, used to discharge its
/// declared capsule. A recursive function is treated as unproven for every
/// property: slice 1 does not certify specs at a recursive fixed point.
pub const CapsuleFacts = struct {
    total: bool = false,
    pure: bool = false,
    read_only: bool = false,
    deterministic: bool = false,
    recursive: bool = false,

    pub fn holds(self: CapsuleFacts, prop: CapsuleProperty) bool {
        if (self.recursive) return false;
        return switch (prop) {
            .total => self.total,
            .pure => self.pure,
            .read_only => self.read_only,
            .deterministic => self.deterministic,
        };
    }
};

fn capsuleSuggestionFor(prop: CapsuleProperty) []const u8 {
    return switch (prop) {
        .total => "return a value on every path so the capsule's `total` property holds.",
        .pure => "remove module calls and Date.now()/Math.random() so the helper stays pure.",
        .read_only => "remove writing module calls (zigttp:cache / zigttp:sql) and egress from the helper.",
        .deterministic => "remove Date.now() / Math.random() from the helper.",
    };
}

const recursive_capsule_suggestion =
    "recursive functions are not yet supported for capsule discharge; inline or remove the recursion.";

/// ZTS502 suggestion for an unknown capsule property name. Caller frees.
fn suggestionForUnknownCapsule(allocator: std.mem.Allocator, name: []const u8) !?[]const u8 {
    return nearestSuggestion(allocator, name, &capsule_property_names);
}

/// Discharge a helper's declared capsule properties against the facts the
/// compiler proved about it. Emits ZTS500 (not discharged) and ZTS502
/// (unknown name). The caller owns the returned list and must call
/// `SpecDiagnostic.deinit` on each entry before deinit-ing the list.
pub fn dischargeCapsule(
    allocator: std.mem.Allocator,
    declared: []const []const u8,
    facts: CapsuleFacts,
) !std.ArrayList(SpecDiagnostic) {
    var out: std.ArrayList(SpecDiagnostic) = .empty;
    errdefer {
        for (out.items) |*d| @constCast(d).deinit(allocator);
        out.deinit(allocator);
    }

    for (declared) |name| {
        const prop = CapsuleProperty.fromName(name) orelse {
            const spec_name = try allocator.dupe(u8, name);
            errdefer allocator.free(spec_name);
            const suggestion = try suggestionForUnknownCapsule(allocator, name);
            errdefer if (suggestion) |s| allocator.free(s);
            try out.append(allocator, .{
                .kind = .unknown_name,
                .spec_name = spec_name,
                .suggestion = suggestion,
            });
            continue;
        };

        if (facts.holds(prop)) continue;

        const spec_name = try allocator.dupe(u8, name);
        errdefer allocator.free(spec_name);
        const text = if (facts.recursive) recursive_capsule_suggestion else capsuleSuggestionFor(prop);
        const suggestion = try allocator.dupe(u8, text);
        errdefer allocator.free(suggestion);
        try out.append(allocator, .{
            .kind = .not_discharged,
            .spec_name = spec_name,
            .suggestion = suggestion,
        });
    }

    return out;
}

// ---------------------------------------------------------------------------
// Effects-capsule discharge
//
// Function-level capability capsules (`Effects<T, S>`) declare a ceiling on
// the inferred effect row: the function may reach at most the capabilities in
// `S`. Discharge is the inverse direction of `Proof` / `Spec`. A proof
// property is checked `declared => proven`; an effect ceiling is checked
// `inferred is a subset of declared`. Reaching a capability outside the
// ceiling is an error (ZTS503); naming an unknown capability is an error
// (ZTS504); naming a capability the function never reaches is a warning
// (ZTS505).
// ---------------------------------------------------------------------------

/// Capability vocabulary for `Effects<...>`: the exact `ModuleCapability`
/// enum field names, derived so the two cannot drift.
pub const effect_capability_names = blk: {
    const fields = @typeInfo(module_binding.ModuleCapability).@"enum".fields;
    var names: [fields.len][]const u8 = undefined;
    for (fields, 0..) |f, i| names[i] = f.name;
    break :blk names;
};

/// The inferred capability set type, shared with effect_inference.
pub const CapabilitySet = std.EnumSet(module_binding.ModuleCapability);

/// ZTS504 suggestion for an unknown capability name. Caller frees.
pub fn suggestionForUnknownCapability(allocator: std.mem.Allocator, name: []const u8) !?[]const u8 {
    return nearestSuggestion(allocator, name, &effect_capability_names);
}

/// Discharge a function's declared `Effects<...>` ceiling against the
/// capability set effect inference computed for it. Emits ZTS503 (a reached
/// capability is outside the ceiling), ZTS504 (unknown capability name), and
/// ZTS505 (a declared capability is never reached). The caller owns the
/// returned list and must `deinit` each entry before deinit-ing the list.
pub fn dischargeEffects(
    allocator: std.mem.Allocator,
    declared: []const []const u8,
    inferred: CapabilitySet,
) !std.ArrayList(SpecDiagnostic) {
    var out: std.ArrayList(SpecDiagnostic) = .empty;
    errdefer {
        for (out.items) |*d| @constCast(d).deinit(allocator);
        out.deinit(allocator);
    }

    // No `Effects<...>` annotation means no declared ceiling, so no check.
    // The capsule is opt-in: a function the author never annotated must not
    // be flagged for the capabilities it reaches.
    if (declared.len == 0) return out;

    // Parse the declared ceiling. An unknown name produces ZTS504 and is
    // dropped from the ceiling so it neither widens nor narrows it.
    var ceiling = CapabilitySet.initEmpty();
    for (declared) |name| {
        const cap = std.meta.stringToEnum(module_binding.ModuleCapability, name) orelse {
            const spec_name = try allocator.dupe(u8, name);
            errdefer allocator.free(spec_name);
            const suggestion = try nearestSuggestion(allocator, name, &effect_capability_names);
            errdefer if (suggestion) |s| allocator.free(s);
            try out.append(allocator, .{
                .kind = .effect_unknown_capability,
                .spec_name = spec_name,
                .suggestion = suggestion,
            });
            continue;
        };
        ceiling.insert(cap);
    }

    // ZTS503: every inferred capability must be inside the ceiling.
    var inferred_it = inferred.iterator();
    while (inferred_it.next()) |cap| {
        if (ceiling.contains(cap)) continue;
        const spec_name = try allocator.dupe(u8, @tagName(cap));
        errdefer allocator.free(spec_name);
        const suggestion = try std.fmt.allocPrint(
            allocator,
            "add `{s}` to the Effects<...> ceiling, or remove the call that reaches it.",
            .{@tagName(cap)},
        );
        errdefer allocator.free(suggestion);
        try out.append(allocator, .{
            .kind = .effect_undeclared,
            .spec_name = spec_name,
            .suggestion = suggestion,
        });
    }

    // ZTS505 (warning): a declared capability the function never reaches.
    for (declared) |name| {
        const cap = std.meta.stringToEnum(module_binding.ModuleCapability, name) orelse continue;
        if (inferred.contains(cap)) continue;
        const spec_name = try allocator.dupe(u8, name);
        errdefer allocator.free(spec_name);
        const suggestion = try allocator.dupe(
            u8,
            "the function never reaches this capability; remove it to tighten the ceiling.",
        );
        errdefer allocator.free(suggestion);
        try out.append(allocator, .{
            .kind = .effect_over_declared,
            .spec_name = spec_name,
            .suggestion = suggestion,
        });
    }

    return out;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "dischargeSpecs all-discharged returns empty" {
    const allocator = std.testing.allocator;

    const props = HandlerProperties{
        .pure = true,
        .read_only = true,
        .stateless = true,
        .retry_safe = true,
        .deterministic = true,
        .has_egress = false,
        .idempotent = true,
        .state_isolated = true,
        .fault_covered = true,
        .injection_safe = true,
        // remaining flow-derived defaults stay at their optimistic true.
    };
    const declared: [3][]const u8 = .{ "idempotent", "deterministic", "state_isolated" };
    const modules: [0][]const u8 = .{};

    var diags = try dischargeSpecs(allocator, &declared, props, &modules, false);
    defer {
        for (diags.items) |*d| @constCast(d).deinit(allocator);
        diags.deinit(allocator);
    }
    try std.testing.expectEqual(@as(usize, 0), diags.items.len);
}

test "dischargeSpecs unmet idempotent emits ZTS401 not_discharged" {
    const allocator = std.testing.allocator;

    const props = HandlerProperties{
        .pure = true,
        .read_only = true,
        .stateless = true,
        .retry_safe = true,
        .deterministic = false, // intentionally unmet
        .has_egress = false,
        .idempotent = false,
        .state_isolated = true,
        .fault_covered = true,
        .injection_safe = true,
    };
    const declared: [1][]const u8 = .{"idempotent"};
    const modules: [0][]const u8 = .{};

    var diags = try dischargeSpecs(allocator, &declared, props, &modules, false);
    defer {
        for (diags.items) |*d| @constCast(d).deinit(allocator);
        diags.deinit(allocator);
    }
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    try std.testing.expectEqual(SpecDiagnostic.Kind.not_discharged, diags.items[0].kind);
    try std.testing.expectEqualStrings("idempotent", diags.items[0].spec_name);
    try std.testing.expect(diags.items[0].suggestion != null);
}

test "dischargeSpecs read_only with cache import emits ZTS402" {
    const allocator = std.testing.allocator;

    const props = HandlerProperties{
        .pure = false,
        .read_only = false,
        .stateless = false,
        .retry_safe = false,
        .deterministic = true,
        .has_egress = false,
    };
    const declared: [1][]const u8 = .{"read_only"};
    const modules: [1][]const u8 = .{"zigttp:cache"};

    var diags = try dischargeSpecs(allocator, &declared, props, &modules, false);
    defer {
        for (diags.items) |*d| @constCast(d).deinit(allocator);
        diags.deinit(allocator);
    }
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    try std.testing.expectEqual(SpecDiagnostic.Kind.incompatible_with_import, diags.items[0].kind);
    try std.testing.expectEqualStrings("read_only", diags.items[0].spec_name);
    try std.testing.expectEqualStrings("zigttp:cache", diags.items[0].incompatible_module.?);
}

test "dischargeSpecs unknown name emits ZTS403" {
    const allocator = std.testing.allocator;

    const props = HandlerProperties{
        .pure = true,
        .read_only = true,
        .stateless = true,
        .retry_safe = true,
        .deterministic = true,
        .has_egress = false,
        .idempotent = true,
        .state_isolated = true,
        .fault_covered = true,
        .injection_safe = true,
    };
    const declared: [2][]const u8 = .{ "made_up_name", "idempotent" };
    const modules: [0][]const u8 = .{};

    var diags = try dischargeSpecs(allocator, &declared, props, &modules, false);
    defer {
        for (diags.items) |*d| @constCast(d).deinit(allocator);
        diags.deinit(allocator);
    }
    // Only the unknown name fails; idempotent is true so no ZTS401.
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    try std.testing.expectEqual(SpecDiagnostic.Kind.unknown_name, diags.items[0].kind);
    try std.testing.expectEqualStrings("made_up_name", diags.items[0].spec_name);
}

test "dischargeSpecs implicit default profile gives the narrow-Spec remedy" {
    const allocator = std.testing.allocator;
    // A cache-writing handler: read_only/retry_safe/idempotent/pure are false,
    // but deterministic/injection_safe/state_isolated hold.
    const props = HandlerProperties{
        .pure = false,
        .read_only = false,
        .stateless = false,
        .retry_safe = false,
        .deterministic = true,
        .has_egress = false,
        .idempotent = false,
        .state_isolated = true,
        .fault_covered = false,
        .injection_safe = true,
    };
    const modules: [1][]const u8 = .{"zigttp:cache"};

    // The implicit path passes the full v1 set as `declared` with the flag.
    var diags = try dischargeSpecs(allocator, &v1_spec_names, props, &modules, true);
    defer {
        for (diags.items) |*d| @constCast(d).deinit(allocator);
        diags.deinit(allocator);
    }

    try std.testing.expect(diags.items.len > 0);
    var saw_remedy = false;
    for (diags.items) |d| {
        try std.testing.expect(d.implicit_default);
        const s = d.suggestion orelse continue;
        if (std.mem.indexOf(u8, s, "declares no Spec") != null and
            std.mem.indexOf(u8, s, "Response & Spec<") != null)
        {
            saw_remedy = true;
            // The example lists held properties and never an unheld one.
            try std.testing.expect(std.mem.indexOf(u8, s, "\"deterministic\"") != null);
            try std.testing.expect(std.mem.indexOf(u8, s, "\"injection_safe\"") != null);
            try std.testing.expect(std.mem.indexOf(u8, s, "\"read_only\"") == null);
            try std.testing.expect(std.mem.indexOf(u8, s, "\"idempotent\"") == null);
        }
    }
    try std.testing.expect(saw_remedy);
}

test "dischargeSpecs implicit default omits import-forbidden read_only from the suggestion" {
    const allocator = std.testing.allocator;
    // A SQL SELECT handler: the classifier marks read_only TRUE (it does not
    // write), but the zigttp:sql import forbids declaring read_only (ZTS501).
    // The ZTS500 suggestion must not list read_only, or it contradicts ZTS501 -
    // the spiral that made stateful handlers hard to land on the first draft.
    const props = HandlerProperties{
        .pure = false,
        .read_only = true, // classifier: a SELECT does not write
        .stateless = true,
        .retry_safe = true,
        .deterministic = true,
        .has_egress = false,
        .idempotent = true,
        .state_isolated = true,
        .fault_covered = false, // forces a ZTS500 so the suggestion is emitted
        .injection_safe = true,
    };
    const modules: [1][]const u8 = .{"zigttp:sql"};

    var diags = try dischargeSpecs(allocator, &v1_spec_names, props, &modules, true);
    defer {
        for (diags.items) |*d| @constCast(d).deinit(allocator);
        diags.deinit(allocator);
    }

    var saw_zts500 = false;
    var saw_zts501 = false;
    for (diags.items) |d| {
        switch (d.kind) {
            .not_discharged => {
                saw_zts500 = true;
                const s = d.suggestion orelse continue;
                // Omits read_only despite the classifier holding it (sql forbids it).
                try std.testing.expect(std.mem.indexOf(u8, s, "\"read_only\"") == null);
                // Still lists properties the handler genuinely holds.
                try std.testing.expect(std.mem.indexOf(u8, s, "\"deterministic\"") != null);
            },
            .incompatible_with_import => saw_zts501 = true,
            else => {},
        }
    }
    try std.testing.expect(saw_zts500);
    try std.testing.expect(saw_zts501); // read_only vs sql is still flagged separately
}

test "dischargeSpecs Spec-less pure handler collapses to one ZTS500 naming real props" {
    // EXP-3/EXP-4 regression. A trivial pure/read-only Spec-less handler holds
    // pure/read_only/deterministic and every flow default, but the derived
    // properties (idempotent, fault_covered, result_safe, optional_safe,
    // canonical) default false. Previously the implicit default profile emitted
    // one byte-identical ZTS500 per undischarged property (the fan-out the agent
    // re-fed to the model) and every one carried the misleading
    // "...cache/sql cannot hold" suggestion. Now: exactly one collapsed ZTS500,
    // whose suggestion names the actual undischarged set, not cache/sql.
    const allocator = std.testing.allocator;
    const props = HandlerProperties{
        .pure = true,
        .read_only = true,
        .stateless = true,
        .retry_safe = true,
        .deterministic = true,
        .has_egress = false,
        // idempotent/fault_covered/result_safe/optional_safe/canonical default
        // false; the flow + injection + state-isolation defaults are true.
    };
    const modules: [0][]const u8 = .{};

    var diags = try dischargeSpecs(allocator, &v1_spec_names, props, &modules, true);
    defer {
        for (diags.items) |*d| @constCast(d).deinit(allocator);
        diags.deinit(allocator);
    }

    // Exactly one ZTS500 (no per-property fan-out), no ZTS501/ZTS502.
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    const d = diags.items[0];
    try std.testing.expectEqual(SpecDiagnostic.Kind.not_discharged, d.kind);
    try std.testing.expect(d.implicit_default);
    try std.testing.expectEqualStrings("ZTS500", d.kind.code());

    const s = d.suggestion orelse return error.MissingSuggestion;
    // Names the real undischarged properties for THIS handler.
    try std.testing.expect(std.mem.indexOf(u8, s, "fault_covered") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "result_safe") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "optional_safe") != null);
    // Never the misdirecting cache/sql sentence on a handler that writes neither.
    try std.testing.expect(std.mem.indexOf(u8, s, "cache/zigttp:sql cannot hold") == null);
    // The minimal Spec it offers lists held properties, not the unheld ones.
    try std.testing.expect(std.mem.indexOf(u8, s, "Response & Spec<") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"pure\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"fault_covered\"") == null);
}

test "dischargeEffects ceiling matching the inferred row returns empty" {
    const allocator = std.testing.allocator;
    var inferred = CapabilitySet.initEmpty();
    inferred.insert(.env);
    inferred.insert(.clock);
    const declared: [2][]const u8 = .{ "env", "clock" };

    var diags = try dischargeEffects(allocator, &declared, inferred);
    defer freeDiags(allocator, &diags);
    try std.testing.expectEqual(@as(usize, 0), diags.items.len);
}

test "dischargeEffects reached capability outside ceiling emits ZTS503" {
    const allocator = std.testing.allocator;
    var inferred = CapabilitySet.initEmpty();
    inferred.insert(.env);
    inferred.insert(.crypto);
    const declared: [1][]const u8 = .{"env"};

    var diags = try dischargeEffects(allocator, &declared, inferred);
    defer freeDiags(allocator, &diags);
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    try std.testing.expectEqual(SpecDiagnostic.Kind.effect_undeclared, diags.items[0].kind);
    try std.testing.expectEqualStrings("crypto", diags.items[0].spec_name);
}

test "dischargeEffects unknown capability name emits ZTS504" {
    const allocator = std.testing.allocator;
    var inferred = CapabilitySet.initEmpty();
    inferred.insert(.env);
    const declared: [2][]const u8 = .{ "env", "databse" };

    var diags = try dischargeEffects(allocator, &declared, inferred);
    defer freeDiags(allocator, &diags);
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    try std.testing.expectEqual(SpecDiagnostic.Kind.effect_unknown_capability, diags.items[0].kind);
    try std.testing.expectEqualStrings("databse", diags.items[0].spec_name);
}

test "dischargeEffects over-declared capability emits ZTS505 warning" {
    const allocator = std.testing.allocator;
    var inferred = CapabilitySet.initEmpty();
    inferred.insert(.env);
    const declared: [2][]const u8 = .{ "env", "crypto" };

    var diags = try dischargeEffects(allocator, &declared, inferred);
    defer freeDiags(allocator, &diags);
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    try std.testing.expectEqual(SpecDiagnostic.Kind.effect_over_declared, diags.items[0].kind);
    try std.testing.expectEqualStrings("crypto", diags.items[0].spec_name);
    try std.testing.expectEqual(SpecDiagnostic.Severity.warn, diags.items[0].kind.severity());
}

test "dischargeSpecs ZTS402 suppresses ZTS401 for the same spec" {
    const allocator = std.testing.allocator;

    const props = HandlerProperties{
        .pure = false,
        .read_only = false, // would otherwise trip ZTS401
        .stateless = false,
        .retry_safe = false,
        .deterministic = true,
        .has_egress = false,
    };
    const declared: [1][]const u8 = .{"read_only"};
    const modules: [1][]const u8 = .{"zigttp:cache"};

    var diags = try dischargeSpecs(allocator, &declared, props, &modules, false);
    defer {
        for (diags.items) |*d| @constCast(d).deinit(allocator);
        diags.deinit(allocator);
    }
    // Only ZTS402 fires; ZTS401 is suppressed because the contradiction
    // is the actionable error and the agent should resolve that first.
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    try std.testing.expectEqual(SpecDiagnostic.Kind.incompatible_with_import, diags.items[0].kind);
}

test "suggestionFor covers all cause-only specs" {
    for (v1_specs) |spec| {
        if (spec.cause_only) {
            try std.testing.expect(suggestionFor(spec.name) != null);
        }
    }
}

test "dischargeSpecs recognises pure/stateless/result_safe/optional_safe" {
    // Regression guard for the v1_specs/HandlerProperties drift fix.
    // Before this change, dischargeSpecs would emit ZTS502 unknown_name
    // for every name below — yet ratchet treated them as real
    // obligations, and the README told authors to write `Spec<"pure">`.
    // Now each name resolves to its HandlerProperties field and follows
    // the standard ZTS500 not_discharged path when the property is false.
    const allocator = std.testing.allocator;

    const props_all_false = HandlerProperties{
        .pure = false,
        .read_only = false,
        .stateless = false,
        .retry_safe = false,
        .deterministic = false,
        .has_egress = false,
        .idempotent = false,
        .state_isolated = false,
        .fault_covered = false,
        .injection_safe = false,
        .result_safe = false,
        .optional_safe = false,
    };
    const declared: [4][]const u8 = .{ "pure", "stateless", "result_safe", "optional_safe" };
    const modules: [0][]const u8 = .{};

    var diags = try dischargeSpecs(allocator, &declared, props_all_false, &modules, false);
    defer {
        for (diags.items) |*d| @constCast(d).deinit(allocator);
        diags.deinit(allocator);
    }

    // Four declared names, four not_discharged diagnostics — no unknown_name.
    try std.testing.expectEqual(@as(usize, 4), diags.items.len);
    for (diags.items) |d| {
        try std.testing.expectEqual(SpecDiagnostic.Kind.not_discharged, d.kind);
        try std.testing.expect(d.suggestion != null);
    }
}

test "dischargeSpecs ZTS502 unknown name carries a nearest-match suggestion" {
    const allocator = std.testing.allocator;

    const props = HandlerProperties{
        .pure = true,
        .read_only = true,
        .stateless = true,
        .retry_safe = true,
        .deterministic = true,
        .has_egress = false,
    };
    // Typo of `idempotent`. Levenshtein distance 2 (transposed o-t).
    const declared: [1][]const u8 = .{"idemptoent"};
    const modules: [0][]const u8 = .{};

    var diags = try dischargeSpecs(allocator, &declared, props, &modules, false);
    defer {
        for (diags.items) |*d| @constCast(d).deinit(allocator);
        diags.deinit(allocator);
    }
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    try std.testing.expectEqual(SpecDiagnostic.Kind.unknown_name, diags.items[0].kind);
    try std.testing.expect(diags.items[0].suggestion != null);
    try std.testing.expect(std.mem.indexOf(u8, diags.items[0].suggestion.?, "idempotent") != null);
}

test "dischargeSpecs ZTS502 with far-off name yields no suggestion" {
    const allocator = std.testing.allocator;

    const props = HandlerProperties{
        .pure = true,
        .read_only = true,
        .stateless = true,
        .retry_safe = true,
        .deterministic = true,
        .has_egress = false,
    };
    const declared: [1][]const u8 = .{"completely_unrelated_name"};
    const modules: [0][]const u8 = .{};

    var diags = try dischargeSpecs(allocator, &declared, props, &modules, false);
    defer {
        for (diags.items) |*d| @constCast(d).deinit(allocator);
        diags.deinit(allocator);
    }
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    try std.testing.expect(diags.items[0].suggestion == null);
}

test "dischargeSpecs ZTS501 incompatible carries an actionable suggestion" {
    const allocator = std.testing.allocator;

    const props = HandlerProperties{
        .pure = false,
        .read_only = false,
        .stateless = false,
        .retry_safe = false,
        .deterministic = true,
        .has_egress = false,
    };
    const declared: [1][]const u8 = .{"read_only"};
    const modules: [1][]const u8 = .{"zigttp:sql"};

    var diags = try dischargeSpecs(allocator, &declared, props, &modules, false);
    defer {
        for (diags.items) |*d| @constCast(d).deinit(allocator);
        diags.deinit(allocator);
    }
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    try std.testing.expectEqual(SpecDiagnostic.Kind.incompatible_with_import, diags.items[0].kind);
    try std.testing.expect(diags.items[0].suggestion != null);
    try std.testing.expect(std.mem.indexOf(u8, diags.items[0].suggestion.?, "zigttp:sql") != null);
    try std.testing.expect(std.mem.indexOf(u8, diags.items[0].suggestion.?, "remove") != null);
    try std.testing.expect(std.mem.indexOf(u8, diags.items[0].suggestion.?, "reads") == null);
}

test "levenshtein distance basics" {
    try std.testing.expectEqual(@as(usize, 0), levenshtein("abc", "abc"));
    try std.testing.expectEqual(@as(usize, 1), levenshtein("abc", "abd"));
    try std.testing.expectEqual(@as(usize, 1), levenshtein("abc", "ab"));
    try std.testing.expectEqual(@as(usize, 3), levenshtein("", "abc"));
    try std.testing.expectEqual(@as(usize, 2), levenshtein("idemptoent", "idempotent"));
}

fn freeDiags(allocator: std.mem.Allocator, diags: *std.ArrayList(SpecDiagnostic)) void {
    for (diags.items) |*d| @constCast(d).deinit(allocator);
    diags.deinit(allocator);
}

test "dischargeCapsule all-proven returns empty" {
    const allocator = std.testing.allocator;
    const facts = CapsuleFacts{ .total = true, .pure = true, .read_only = true, .deterministic = true };
    const declared: [3][]const u8 = .{ "total", "pure", "deterministic" };
    var diags = try dischargeCapsule(allocator, &declared, facts);
    defer freeDiags(allocator, &diags);
    try std.testing.expectEqual(@as(usize, 0), diags.items.len);
}

test "dischargeCapsule unmet property emits ZTS500" {
    const allocator = std.testing.allocator;
    const facts = CapsuleFacts{ .total = true, .pure = false, .read_only = true, .deterministic = true };
    const declared: [1][]const u8 = .{"pure"};
    var diags = try dischargeCapsule(allocator, &declared, facts);
    defer freeDiags(allocator, &diags);
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    try std.testing.expectEqual(SpecDiagnostic.Kind.not_discharged, diags.items[0].kind);
    try std.testing.expectEqualStrings("pure", diags.items[0].spec_name);
    try std.testing.expect(diags.items[0].suggestion != null);
}

test "dischargeCapsule unknown name emits ZTS502 with suggestion" {
    const allocator = std.testing.allocator;
    const facts = CapsuleFacts{ .total = true, .pure = true, .read_only = true, .deterministic = true };
    const declared: [1][]const u8 = .{"read_onyl"};
    var diags = try dischargeCapsule(allocator, &declared, facts);
    defer freeDiags(allocator, &diags);
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    try std.testing.expectEqual(SpecDiagnostic.Kind.unknown_name, diags.items[0].kind);
    try std.testing.expect(diags.items[0].suggestion != null);
    try std.testing.expect(std.mem.indexOf(u8, diags.items[0].suggestion.?, "read_only") != null);
}

test "dischargeCapsule recursive function fails every declared property" {
    const allocator = std.testing.allocator;
    const facts = CapsuleFacts{
        .total = true,
        .pure = true,
        .read_only = true,
        .deterministic = true,
        .recursive = true,
    };
    const declared: [1][]const u8 = .{"pure"};
    var diags = try dischargeCapsule(allocator, &declared, facts);
    defer freeDiags(allocator, &diags);
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    try std.testing.expectEqual(SpecDiagnostic.Kind.not_discharged, diags.items[0].kind);
    try std.testing.expect(std.mem.indexOf(u8, diags.items[0].suggestion.?, "recursive") != null);
}
