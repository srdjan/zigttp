//! Deploy Proof Review
//!
//! Projects what the compiler proved about a handler into a small, persistable
//! `ReviewFacts` struct, derives a `ReviewDelta` against the previous deploy's
//! facts, and renders a single concise card the user reads before any upload
//! starts. Vocabulary is deliberately aligned with `live_reload.formatUpgradeDiff`:
//! `safe` / `safe_with_additions` / `breaking`. Vocabulary alignment matters
//! because a developer iterating with `zigttp dev --watch --prove` should see
//! the same words at deploy time without context-switching.
//!
//! Persistence rule: ReviewFacts contains only contract-derived identifiers.
//! No env values, no tokens, no credentials, no PII ever round-trips through
//! the persisted `last_review_facts` snapshot.
//!
//! Ownership: `ReviewFacts.fromProvenFacts` allocates owned copies of every
//! string so the result outlives the source contract. `deriveDelta` produces
//! outer slices whose elements are borrowed from the inputs; the delta's
//! deinit only frees the outer slices. `renderReviewCard` writes to a
//! borrowed `std.Io.Writer` and does not allocate.

const std = @import("std");
const json_util = @import("json_util.zig");
const zigts_cli = @import("zigts_cli");

const ProvenFacts = zigts_cli.deploy_manifest.ProvenFacts;

// Verdict mirrors `upgrade_verifier.UpgradeVerdict` so the deploy-time card
// uses the same words the watch-loop diff already trains the user on. We do
// not import that enum directly: the deploy review derives its verdict from
// a lightweight `ReviewDelta` rather than a full HandlerContract pair, and
// keeping a local enum lets the persisted facts stay simple.
pub const Verdict = enum {
    safe,
    safe_with_additions,
    breaking,

    pub fn toString(self: Verdict) []const u8 {
        return switch (self) {
            .safe => "safe",
            .safe_with_additions => "safe_with_additions",
            .breaking => "breaking",
        };
    }
};

/// Mirrors `contract_diff.ProofLevel`. Duplicated locally so review.zig stays
/// independent of zigts contract types and so persisted JSON uses simple
/// stable strings rather than re-exporting the engine enum.
pub const ProofLevel = enum {
    complete,
    partial,
    none,

    pub fn toString(self: ProofLevel) []const u8 {
        return switch (self) {
            .complete => "complete",
            .partial => "partial",
            .none => "none",
        };
    }

    pub fn fromString(s: []const u8) ProofLevel {
        if (std.mem.eql(u8, s, "complete")) return .complete;
        if (std.mem.eql(u8, s, "partial")) return .partial;
        return .none;
    }

    /// Higher rank means stronger proof. Used to detect downgrades.
    fn rank(self: ProofLevel) u8 {
        return switch (self) {
            .complete => 2,
            .partial => 1,
            .none => 0,
        };
    }
};

pub const Route = struct {
    pattern: []const u8,
    is_prefix: bool,

    fn lessThan(_: void, a: Route, b: Route) bool {
        const cmp = std.mem.order(u8, a.pattern, b.pattern);
        if (cmp != .eq) return cmp == .lt;
        return @intFromBool(a.is_prefix) < @intFromBool(b.is_prefix);
    }

    fn eql(a: Route, b: Route) bool {
        return a.is_prefix == b.is_prefix and std.mem.eql(u8, a.pattern, b.pattern);
    }
};

/// One declared spec name plus its current discharge state. `name` is
/// owned. The HUD renders failure state as `[FAIL]`; the ledger preserves
/// the same so historical entries can be diffed without re-running the
/// verifier.
pub const SpecState = struct {
    name: []const u8,
    discharged: bool,
    /// Author-facing error code (ZTS500/501/502) when the spec failed to
    /// discharge. Borrowed in transient builders, owned after
    /// `dupeSortedDedupedSpecs`.
    diagnostic_code: ?[]const u8 = null,
    /// One-line explanation of why the spec failed (e.g. "property not
    /// discharged", "contradicts import of zigttp:cache"). Owned when set.
    diagnostic_message: ?[]const u8 = null,
    /// Source line in the handler that caused the demotion. Only populated
    /// for ZTS500 when the verifier captured a `PropertyCause`.
    source_line: ?u32 = null,
    /// Source column matching `source_line`.
    source_column: ?u16 = null,
    /// The construct that demoted the property (e.g. "Date.now()"). Owned
    /// when set.
    source_snippet: ?[]const u8 = null,

    fn lessThan(_: void, a: SpecState, b: SpecState) bool {
        return std.mem.order(u8, a.name, b.name) == .lt;
    }
};

// Properties carries the property booleans that show up in the contract.
// Defaults match what extractProvenFacts uses when contract.properties is null,
// so a missing-field parse round-trips the same shape an absent properties
// block would have produced.
pub const Properties = struct {
    retry_safe: bool = false,
    read_only: bool = false,
    injection_safe: bool = true,
    idempotent: bool = false,
    state_isolated: bool = true,
    no_secret_leakage: bool = true,
    no_credential_leakage: bool = true,
    input_validated: bool = true,
    pii_contained: bool = true,
    results_safe: bool = false,
    fault_covered: bool = false,
    /// True when the handler has no `Date.now()` or `Math.random()` calls.
    /// First-class in the live-reload HUD so authors see the canonical
    /// `+deterministic`/`-deterministic` flip while editing.
    deterministic: bool = true,
};

pub const PropertyMeta = struct {
    field: []const u8,
    json_key: []const u8,
    label: []const u8,
};

pub const property_metas = [_]PropertyMeta{
    .{ .field = "retry_safe", .json_key = "retrySafe", .label = "retry-safe" },
    .{ .field = "read_only", .json_key = "readOnly", .label = "read-only" },
    .{ .field = "deterministic", .json_key = "deterministic", .label = "deterministic" },
    .{ .field = "injection_safe", .json_key = "injectionSafe", .label = "injection-safe" },
    .{ .field = "idempotent", .json_key = "idempotent", .label = "idempotent" },
    .{ .field = "state_isolated", .json_key = "stateIsolated", .label = "state-isolated" },
    .{ .field = "no_secret_leakage", .json_key = "noSecretLeakage", .label = "no-secret-leakage" },
    .{ .field = "no_credential_leakage", .json_key = "noCredentialLeakage", .label = "no-credential-leakage" },
    .{ .field = "input_validated", .json_key = "inputValidated", .label = "input-validated" },
    .{ .field = "pii_contained", .json_key = "piiContained", .label = "pii-contained" },
    .{ .field = "results_safe", .json_key = "resultsSafe", .label = "results-safe" },
    .{ .field = "fault_covered", .json_key = "faultCovered", .label = "fault-covered" },
};

/// Maps each proof property to the substrate restrictions that earn it,
/// plus a short tagline. Mirrored by the JS `TRADE_TABLE` in
/// `packages/runtime/src/studio.zig` for the browser lens; keep both in
/// sync when adding properties or changing labels. Restriction names are
/// the human strings from `packages/tools/src/json_diagnostics.zig`.
pub const TradeRow = struct {
    property_field: []const u8,
    restrictions: []const []const u8,
    earned: []const u8,
};

pub const proof_to_restrictions = [_]TradeRow{
    .{
        .property_field = "deterministic",
        .restrictions = &.{ "async/await", "while", "do...while", "for(;;)" },
        .earned = "deterministic, replayable, AI-refactorable",
    },
    .{
        .property_field = "retry_safe",
        .restrictions = &.{ "try/catch", "throw" },
        .earned = "Result-narrowed, exhaustive paths, no hidden control flow",
    },
    .{
        .property_field = "state_isolated",
        .restrictions = &.{ "class", "this", "++", "--" },
        .earned = "explicit data flow, no shared mutable receivers",
    },
    .{
        .property_field = "read_only",
        .restrictions = &.{ "delete", "++", "--" },
        .earned = "shape-stable property access, no hidden writes",
    },
    .{
        .property_field = "input_validated",
        .restrictions = &.{"regex"},
        .earned = "schema-checkable validation, no opaque accept sets",
    },
    .{
        .property_field = "injection_safe",
        .restrictions = &.{},
        .earned = "flow analysis tracks user-input into sinks",
    },
    .{
        .property_field = "idempotent",
        .restrictions = &.{},
        .earned = "earned by analysis; retries are safe",
    },
    .{
        .property_field = "no_secret_leakage",
        .restrictions = &.{},
        .earned = "flow analysis tracks secret labels to sinks",
    },
    .{
        .property_field = "no_credential_leakage",
        .restrictions = &.{},
        .earned = "flow analysis tracks credential labels to sinks",
    },
    .{
        .property_field = "pii_contained",
        .restrictions = &.{},
        .earned = "PII never reaches egress without an explicit boundary",
    },
    .{
        .property_field = "results_safe",
        .restrictions = &.{},
        .earned = "all paths return a Response or Result.err",
    },
    .{
        .property_field = "fault_covered",
        .restrictions = &.{},
        .earned = "every failure path has a witness or test",
    },
};

// ReviewFacts is the persisted-and-rendered projection of a contract. Strings
// are owned and freed in deinit. Routes are owned (pattern bytes duped).
pub const ReviewFacts = struct {
    contract_sha: []const u8,
    proof_level: ProofLevel,
    env_keys: []const []const u8,
    egress_hosts: []const []const u8,
    cache_namespaces: []const []const u8,
    routes: []const Route,
    capabilities: []const []const u8,
    properties: Properties,
    /// Active specs from the compiled contract. Each entry carries the name
    /// plus its current discharge state. The HUD renders failed entries as
    /// `[FAIL]` with a per-property suggestion.
    declared_specs: []const SpecState = &.{},
    /// Flattened intent + behavior summary, denormalised from the
    /// contract so the diff and review card can call out "intent added"
    /// or "path count changed" without re-walking the contract.
    intent_assertion_count: usize = 0,
    intent_dynamic: bool = false,
    behavior_path_count: usize = 0,
    /// Subset of `behavior_path_count`; invariant: `<= behavior_path_count`.
    failure_path_count: usize = 0,

    /// Build owned facts from the same ProvenFacts the deploy already extracts
    /// (so we cannot drift from what `buildProofLabels` and `extractProvenFacts`
    /// have already canonicalised). Capability strings come from
    /// `contract.capabilities.slice()` in the caller; we accept them as a
    /// borrowed list to keep this module free of zigts contract types.
    /// Active specs come from `contract.declared_specs.items` and the
    /// matching not-discharged set is derived by the caller (the contract
    /// already classifies them via spec_discharge).
    pub fn fromProvenFacts(
        allocator: std.mem.Allocator,
        facts: *const ProvenFacts,
        capabilities: []const []const u8,
        contract_sha: []const u8,
        declared_specs: []const SpecState,
    ) !ReviewFacts {
        const env_keys = try dupeSortedDedupedStrings(allocator, facts.env_vars);
        errdefer freeStringList(allocator, env_keys);
        const egress_hosts = try dupeSortedDedupedStrings(allocator, facts.egress_hosts);
        errdefer freeStringList(allocator, egress_hosts);
        const cache_namespaces = try dupeSortedDedupedStrings(allocator, facts.cache_namespaces);
        errdefer freeStringList(allocator, cache_namespaces);
        const caps = try dupeSortedDedupedStrings(allocator, capabilities);
        errdefer freeStringList(allocator, caps);

        const routes = try dupeSortedDedupedRoutes(allocator, facts.routes);
        errdefer freeRouteList(allocator, routes);

        const specs = try dupeSortedDedupedSpecs(allocator, declared_specs);
        errdefer freeSpecList(allocator, specs);

        const sha = try allocator.dupe(u8, contract_sha);
        errdefer allocator.free(sha);

        return .{
            .contract_sha = sha,
            .proof_level = ProofLevel.fromString(facts.proof_level.toString()),
            .env_keys = env_keys,
            .egress_hosts = egress_hosts,
            .cache_namespaces = cache_namespaces,
            .routes = routes,
            .capabilities = caps,
            .properties = .{
                .retry_safe = facts.retry_safe,
                .read_only = facts.read_only,
                .injection_safe = facts.injection_safe,
                .idempotent = facts.idempotent,
                .state_isolated = facts.state_isolated,
                .deterministic = facts.deterministic,
                .no_secret_leakage = facts.no_secret_leakage,
                .no_credential_leakage = facts.no_credential_leakage,
                .input_validated = facts.input_validated,
                .pii_contained = facts.pii_contained,
                .results_safe = facts.results_safe,
                .fault_covered = facts.fault_covered,
            },
            .declared_specs = specs,
        };
    }

    pub fn deinit(self: *ReviewFacts, allocator: std.mem.Allocator) void {
        allocator.free(self.contract_sha);
        freeStringList(allocator, self.env_keys);
        freeStringList(allocator, self.egress_hosts);
        freeStringList(allocator, self.cache_namespaces);
        freeStringList(allocator, self.capabilities);
        freeRouteList(allocator, self.routes);
        freeSpecList(allocator, self.declared_specs);
    }

    pub const IntentSummary = struct {
        assertion_count: usize = 0,
        dynamic: bool = false,
        behavior_paths: usize = 0,
        failure_paths: usize = 0,
    };

    /// Enrich a freshly-built `ReviewFacts` with the intent and behavior
    /// summary fields. Call after `fromProvenFacts` from a site that has
    /// the `HandlerContract`; callers without it can skip - the defaults
    /// preserve existing rendering.
    pub fn setIntentSummary(self: *ReviewFacts, s: IntentSummary) void {
        std.debug.assert(s.failure_paths <= s.behavior_paths);
        self.intent_assertion_count = s.assertion_count;
        self.intent_dynamic = s.dynamic;
        self.behavior_path_count = s.behavior_paths;
        self.failure_path_count = s.failure_paths;
    }

    pub fn writeJson(self: *const ReviewFacts, json: *std.json.Stringify) !void {
        try json.beginObject();
        try json.objectField("contractSha");
        try json.write(self.contract_sha);
        try json.objectField("proofLevel");
        try json.write(self.proof_level.toString());
        try json.objectField("envKeys");
        try writeStringArray(json, self.env_keys);
        try json.objectField("egressHosts");
        try writeStringArray(json, self.egress_hosts);
        try json.objectField("cacheNamespaces");
        try writeStringArray(json, self.cache_namespaces);
        try json.objectField("capabilities");
        try writeStringArray(json, self.capabilities);
        try json.objectField("routes");
        try json.beginArray();
        for (self.routes) |r| {
            try json.beginObject();
            try json.objectField("pattern");
            try json.write(r.pattern);
            try json.objectField("isPrefix");
            try json.write(r.is_prefix);
            try json.endObject();
        }
        try json.endArray();
        try json.objectField("properties");
        try json.beginObject();
        inline for (property_metas) |meta| {
            try json.objectField(meta.json_key);
            try json.write(@field(self.properties, meta.field));
        }
        try json.endObject();
        try json.objectField("declaredSpecs");
        try json.beginArray();
        for (self.declared_specs) |s| {
            try json.beginObject();
            try json.objectField("name");
            try json.write(s.name);
            try json.objectField("discharged");
            try json.write(s.discharged);
            if (s.diagnostic_code) |c| {
                try json.objectField("diagnosticCode");
                try json.write(c);
            }
            if (s.diagnostic_message) |m| {
                try json.objectField("diagnosticMessage");
                try json.write(m);
            }
            if (s.source_line) |line| {
                try json.objectField("sourceLine");
                try json.write(line);
            }
            if (s.source_column) |col| {
                try json.objectField("sourceColumn");
                try json.write(col);
            }
            if (s.source_snippet) |sn| {
                try json.objectField("sourceSnippet");
                try json.write(sn);
            }
            try json.endObject();
        }
        try json.endArray();
        try json.objectField("intentAssertionCount");
        try json.write(self.intent_assertion_count);
        try json.objectField("intentDynamic");
        try json.write(self.intent_dynamic);
        try json.objectField("behaviorPathCount");
        try json.write(self.behavior_path_count);
        try json.objectField("failurePathCount");
        try json.write(self.failure_path_count);
        try json.endObject();
    }

    /// Backward-compatible parser: missing arrays default to empty, missing
    /// strings default to "none"/"" so older deploy-state.json entries still
    /// load. Per the v1 plan, an entry without `lastReviewFacts` is treated
    /// by the caller as "no baseline" rather than parsed via this function.
    pub fn parseJson(allocator: std.mem.Allocator, obj: std.json.ObjectMap) !ReviewFacts {
        const sha_value: []const u8 = json_util.getString(obj, "contractSha") orelse "";
        const sha = try allocator.dupe(u8, sha_value);
        errdefer allocator.free(sha);

        const level = ProofLevel.fromString(json_util.getString(obj, "proofLevel") orelse "none");

        const env_keys = try parseStringArray(allocator, obj, "envKeys");
        errdefer freeStringList(allocator, env_keys);
        const egress_hosts = try parseStringArray(allocator, obj, "egressHosts");
        errdefer freeStringList(allocator, egress_hosts);
        const cache_namespaces = try parseStringArray(allocator, obj, "cacheNamespaces");
        errdefer freeStringList(allocator, cache_namespaces);
        const capabilities = try parseStringArray(allocator, obj, "capabilities");
        errdefer freeStringList(allocator, capabilities);

        const routes = try parseRouteArray(allocator, obj);
        errdefer freeRouteList(allocator, routes);

        var props = Properties{};
        if (obj.get("properties")) |props_value| {
            if (props_value == .object) {
                inline for (property_metas) |meta| {
                    if (props_value.object.get(meta.json_key)) |v| {
                        if (v == .bool) @field(props, meta.field) = v.bool;
                    }
                }
            }
        }

        const declared_specs = try parseSpecArray(allocator, obj);
        errdefer freeSpecList(allocator, declared_specs);

        // Read back the intent-summary fields writeJson emits; without this they
        // silently reset to 0/false on every round-trip through deploy-state.json.
        const intent_assertion_count: usize = @intCast(@max(0, json_util.getI64(obj, "intentAssertionCount") orelse 0));
        const behavior_path_count: usize = @intCast(@max(0, json_util.getI64(obj, "behaviorPathCount") orelse 0));
        const failure_path_count: usize = @intCast(@max(0, json_util.getI64(obj, "failurePathCount") orelse 0));
        const intent_dynamic = if (obj.get("intentDynamic")) |v| (v == .bool and v.bool) else false;

        return .{
            .contract_sha = sha,
            .proof_level = level,
            .env_keys = env_keys,
            .egress_hosts = egress_hosts,
            .cache_namespaces = cache_namespaces,
            .routes = routes,
            .capabilities = capabilities,
            .properties = props,
            .declared_specs = declared_specs,
            .intent_assertion_count = intent_assertion_count,
            .intent_dynamic = intent_dynamic,
            .behavior_path_count = behavior_path_count,
            .failure_path_count = failure_path_count,
        };
    }
};

pub const PropertyChange = struct {
    /// Borrowed pointer into property_metas; outlives any DeployReview built
    /// in the same process. Render code must not free.
    name: []const u8,
    label: []const u8,
    old_value: bool,
    new_value: bool,
};

pub const ProofLevelChange = struct {
    old: ProofLevel,
    new: ProofLevel,
};

// ReviewDelta holds outer slices that the delta owns; each element references
// strings owned by the input ReviewFacts (so deinit only frees the outer
// slices). Routes are also borrowed. Field defaults express the "empty
// delta" so callers (especially classify-style unit tests) can build a
// delta varying just the field of interest.
pub const ReviewDelta = struct {
    added_env: []const []const u8 = &.{},
    removed_env: []const []const u8 = &.{},
    added_egress: []const []const u8 = &.{},
    removed_egress: []const []const u8 = &.{},
    added_cache: []const []const u8 = &.{},
    removed_cache: []const []const u8 = &.{},
    added_routes: []const Route = &.{},
    removed_routes: []const Route = &.{},
    added_capabilities: []const []const u8 = &.{},
    removed_capabilities: []const []const u8 = &.{},
    proof_level_change: ?ProofLevelChange = null,
    promoted_properties: []const PropertyChange = &.{},
    demoted_properties: []const PropertyChange = &.{},

    pub fn isEmpty(self: *const ReviewDelta) bool {
        return self.added_env.len == 0 and self.removed_env.len == 0 and
            self.added_egress.len == 0 and self.removed_egress.len == 0 and
            self.added_cache.len == 0 and self.removed_cache.len == 0 and
            self.added_routes.len == 0 and self.removed_routes.len == 0 and
            self.added_capabilities.len == 0 and self.removed_capabilities.len == 0 and
            self.proof_level_change == null and
            self.promoted_properties.len == 0 and self.demoted_properties.len == 0;
    }

    /// Breaking signals: any removal, any property demotion, any proof-level
    /// downgrade. Rationale matches the watch-loop verdict: the dev who saw
    /// `breaking` while editing should see the same word at deploy.
    pub fn hasBreaking(self: *const ReviewDelta) bool {
        if (self.removed_env.len > 0) return true;
        if (self.removed_egress.len > 0) return true;
        if (self.removed_cache.len > 0) return true;
        if (self.removed_routes.len > 0) return true;
        if (self.removed_capabilities.len > 0) return true;
        if (self.demoted_properties.len > 0) return true;
        if (self.proof_level_change) |change| {
            if (change.old.rank() > change.new.rank()) return true;
        }
        return false;
    }

    pub fn deinit(self: *ReviewDelta, allocator: std.mem.Allocator) void {
        allocator.free(self.added_env);
        allocator.free(self.removed_env);
        allocator.free(self.added_egress);
        allocator.free(self.removed_egress);
        allocator.free(self.added_cache);
        allocator.free(self.removed_cache);
        allocator.free(self.added_routes);
        allocator.free(self.removed_routes);
        allocator.free(self.added_capabilities);
        allocator.free(self.removed_capabilities);
        allocator.free(self.promoted_properties);
        allocator.free(self.demoted_properties);
    }
};

/// Derive a delta of `current` against `baseline`. With no baseline the delta
/// is empty by construction so `classify` returns `.safe` for first deploys;
/// the renderer separately surfaces "baseline: none" so that case is still
/// distinguishable in output.
pub fn deriveDelta(
    allocator: std.mem.Allocator,
    current: *const ReviewFacts,
    baseline: ?*const ReviewFacts,
) !ReviewDelta {
    if (baseline == null) return .{};
    const b = baseline.?;

    const added_env = try setDiff(allocator, current.env_keys, b.env_keys);
    errdefer allocator.free(added_env);
    const removed_env = try setDiff(allocator, b.env_keys, current.env_keys);
    errdefer allocator.free(removed_env);

    const added_egress = try setDiff(allocator, current.egress_hosts, b.egress_hosts);
    errdefer allocator.free(added_egress);
    const removed_egress = try setDiff(allocator, b.egress_hosts, current.egress_hosts);
    errdefer allocator.free(removed_egress);

    const added_cache = try setDiff(allocator, current.cache_namespaces, b.cache_namespaces);
    errdefer allocator.free(added_cache);
    const removed_cache = try setDiff(allocator, b.cache_namespaces, current.cache_namespaces);
    errdefer allocator.free(removed_cache);

    const added_caps = try setDiff(allocator, current.capabilities, b.capabilities);
    errdefer allocator.free(added_caps);
    const removed_caps = try setDiff(allocator, b.capabilities, current.capabilities);
    errdefer allocator.free(removed_caps);

    const added_routes = try routeDiff(allocator, current.routes, b.routes);
    errdefer allocator.free(added_routes);
    const removed_routes = try routeDiff(allocator, b.routes, current.routes);
    errdefer allocator.free(removed_routes);

    var promoted: std.ArrayList(PropertyChange) = .empty;
    errdefer promoted.deinit(allocator);
    var demoted: std.ArrayList(PropertyChange) = .empty;
    errdefer demoted.deinit(allocator);
    inline for (property_metas) |meta| {
        const old_v = @field(b.properties, meta.field);
        const new_v = @field(current.properties, meta.field);
        if (old_v != new_v) {
            const change = PropertyChange{
                .name = meta.field,
                .label = meta.label,
                .old_value = old_v,
                .new_value = new_v,
            };
            if (!old_v and new_v) {
                try promoted.append(allocator, change);
            } else {
                try demoted.append(allocator, change);
            }
        }
    }

    const level_change: ?ProofLevelChange = if (current.proof_level == b.proof_level)
        null
    else
        .{ .old = b.proof_level, .new = current.proof_level };

    // Convert both lists into owned slices before assembling the struct, with an
    // errdefer covering the window between the two conversions: once promoted is
    // toOwnedSlice'd its ArrayList errdefer above no longer frees the returned
    // slice, so a failing demoted conversion would otherwise leak it.
    const promoted_slice = try promoted.toOwnedSlice(allocator);
    errdefer allocator.free(promoted_slice);
    const demoted_slice = try demoted.toOwnedSlice(allocator);

    return .{
        .added_env = added_env,
        .removed_env = removed_env,
        .added_egress = added_egress,
        .removed_egress = removed_egress,
        .added_cache = added_cache,
        .removed_cache = removed_cache,
        .added_routes = added_routes,
        .removed_routes = removed_routes,
        .added_capabilities = added_caps,
        .removed_capabilities = removed_caps,
        .proof_level_change = level_change,
        .promoted_properties = promoted_slice,
        .demoted_properties = demoted_slice,
    };
}

pub fn classify(delta: *const ReviewDelta) Verdict {
    if (delta.hasBreaking()) return .breaking;
    if (delta.isEmpty()) return .safe;
    return .safe_with_additions;
}

pub const DriftStatus = struct {
    reason: []const u8, // borrowed
};

pub const PlanRequiredSummary = struct {
    plan_id: []const u8,
    review_url: ?[]const u8,
    covered_reasons: []const []const u8,
    uncovered_reasons: []const []const u8,
};

// DeployReview owns its `current` and `delta`. `baseline` is borrowed: it
// lives in the caller's deploy state (or in a test fixture) and outlives
// the render call. The drift and plan_required substructures borrow strings
// from caller-owned data and must outlive the render call only.
pub const DeployReview = struct {
    handler_path: []const u8, // borrowed
    service_name: []const u8, // borrowed
    region: []const u8, // borrowed
    current: ReviewFacts,
    baseline: ?*const ReviewFacts = null,
    delta: ReviewDelta,
    drift: ?DriftStatus = null,
    plan_required: ?PlanRequiredSummary = null,

    pub const InitParams = struct {
        handler_path: []const u8,
        service_name: []const u8,
        region: []const u8,
        facts: *const ProvenFacts,
        capability_names: []const []const u8,
        contract_sha: []const u8,
        /// Author-declared specs for the handler, paired with their current
        /// discharge state. Defaults to empty for first-deploy callers and
        /// for handlers without a `Spec<...>` annotation. ReviewFacts dupes
        /// the names into its own ownership.
        declared_specs: []const SpecState = &.{},
        baseline: ?*const ReviewFacts = null,
        drift: ?DriftStatus = null,
        plan_required: ?PlanRequiredSummary = null,
    };

    /// Build a DeployReview from caller inputs. The labeled-block + errdefer
    /// here are load-bearing: a struct-literal that called `deriveDelta`
    /// inline would leak `current`'s allocations if `deriveDelta` errored,
    /// because the caller's `defer review.deinit` is only installed once
    /// init returns. A function-scope errdefer would instead double-free on
    /// a later error after ownership transferred into the returned struct.
    pub fn init(allocator: std.mem.Allocator, params: InitParams) !DeployReview {
        return build: {
            var current = try ReviewFacts.fromProvenFacts(
                allocator,
                params.facts,
                params.capability_names,
                params.contract_sha,
                params.declared_specs,
            );
            errdefer current.deinit(allocator);
            const delta = try deriveDelta(allocator, &current, params.baseline);
            break :build DeployReview{
                .handler_path = params.handler_path,
                .service_name = params.service_name,
                .region = params.region,
                .current = current,
                .baseline = params.baseline,
                .delta = delta,
                .drift = params.drift,
                .plan_required = params.plan_required,
            };
        };
    }

    pub fn verdict(self: *const DeployReview) Verdict {
        return classify(&self.delta);
    }

    pub fn deinit(self: *DeployReview, allocator: std.mem.Allocator) void {
        self.current.deinit(allocator);
        self.delta.deinit(allocator);
    }
};

pub const RenderOptions = struct {
    /// Reserved for a follow-up; v1 ships with no ANSI to keep snapshot tests
    /// trivial. The flag exists so deploy.zig can detect TTYs today and gate
    /// future color additions without an API churn.
    use_color: bool = false,
};

/// ProofCard is the source-agnostic projection a renderer needs. It borrows
/// every field from longer-lived data (a DeployReview, a ledger entry, a
/// live-reload session) so the same card can drive deploy, `zigttp proofs
/// show`, the live HUD, and HTML/SVG export. No allocations, no ownership.
///
/// Fields are intentionally a strict subset of DeployReview plus a few that
/// callers without a DeployReview (proofs ledger replay, live HUD) can fill
/// directly. `verdict` is computed from `delta` to avoid a third source of
/// truth on top of `classify` and `DeployReview.verdict`.
/// Why a property regressed: source location plus the construct that caused
/// it (e.g. `Date.now()`). Only the live-reload path populates this; deploy
/// reviews and ledger replays leave the slice empty because they have no
/// access to the source file at render time. Borrowed entries; the caller
/// (live_reload) keeps the underlying contract alive across the render call.
pub const PropertyCauseEntry = struct {
    /// Field name from `property_metas` (e.g. "deterministic"). Borrowed.
    field: []const u8,
    line: u32,
    column: u16,
    snippet: []const u8,
};

/// All `[]const u8` fields are borrowed: the live-reload session that builds
/// the preview keeps the contract, `property_metas`, and the suggestion
/// catalog alive across the render call.
pub const CounterexamplePreview = struct {
    field: []const u8,
    label: []const u8,
    line: u32,
    column: u16,
    snippet: []const u8,
    handler_path: []const u8,
    suggestion: ?[]const u8 = null,
    failing_request: ?FailingRequest = null,
    previous_response: ?ReplayResponse = null,
    current_response: ?ReplayResponse = null,

    pub const FailingRequest = struct {
        method: []const u8,
        url: []const u8,
        has_auth_header: bool = false,
        body: ?[]const u8 = null,
    };

    pub const ReplayResponse = struct {
        status: u16,
        body: []const u8,
        /// Mutually exclusive with a populated `body` carrying a successful
        /// response payload.
        error_text: ?[]const u8 = null,
    };
};

pub const ProofCard = struct {
    handler_path: []const u8,
    service_name: []const u8,
    region: []const u8,
    current: *const ReviewFacts,
    baseline: ?*const ReviewFacts = null,
    delta: *const ReviewDelta,
    drift: ?DriftStatus = null,
    plan_required: ?PlanRequiredSummary = null,
    /// Optional per-property "Why" entries for the live HUD. The renderer
    /// shows a Why row for every demoted_property whose field matches an
    /// entry here. Empty (default) outside the live-reload path.
    property_causes: []const PropertyCauseEntry = &.{},
    counterexample: ?CounterexamplePreview = null,

    pub fn verdict(self: *const ProofCard) Verdict {
        return classify(self.delta);
    }

    /// Build a card view of an existing DeployReview. The card borrows every
    /// pointer from `review`, so the review must outlive the card.
    pub fn fromDeployReview(review: *const DeployReview) ProofCard {
        return .{
            .handler_path = review.handler_path,
            .service_name = review.service_name,
            .region = review.region,
            .current = &review.current,
            .baseline = review.baseline,
            .delta = &review.delta,
            .drift = review.drift,
            .plan_required = review.plan_required,
        };
    }
};

/// Render the proof card as plaintext. The TUI writer in `proof_card_tui.zig`
/// consumes the same `ProofCard` to populate frame cells. Output is
/// deterministic for the same inputs so snapshot tests stay stable. Caller
/// flushes the writer.
pub fn writeProofCardPlaintext(card: *const ProofCard, writer: *std.Io.Writer) !void {
    try writer.writeAll("Proof review:\n");
    try writeKv(writer, "  Handler:    ", card.handler_path);
    try writeKv(writer, "  Service:    ", card.service_name);
    try writeKv(writer, "  Region:     ", card.region);
    try writeKv(writer, "  Contract:   ", card.current.contract_sha);
    try writeKv(writer, "  Proof:      ", card.current.proof_level.toString());

    try writer.writeAll("  Proven:     ");
    var any_proven = false;
    inline for (property_metas) |meta| {
        if (@field(card.current.properties, meta.field)) {
            if (any_proven) try writer.writeAll(", ");
            try writer.writeAll(meta.label);
            any_proven = true;
        }
    }
    if (!any_proven) try writer.writeAll("(none)");
    try writer.writeAll("\n");

    try writer.print("  Surface:    {d} route(s), {d} env, {d} egress, {d} cache, {d} cap(s)\n", .{
        card.current.routes.len,
        card.current.env_keys.len,
        card.current.egress_hosts.len,
        card.current.cache_namespaces.len,
        card.current.capabilities.len,
    });

    if (card.baseline == null) {
        try writer.writeAll("  Baseline:   none (first deploy)\n");
    } else {
        try writeKv(writer, "  Baseline:   ", card.baseline.?.contract_sha);
        try renderDelta(writer, card.delta);
    }

    try writeKv(writer, "  Verdict:    ", card.verdict().toString());

    if (card.drift) |d| {
        try writeKv(writer, "  Drift:      ", d.reason);
        try writer.writeAll("              re-run with --confirm to apply\n");
    }
    if (card.plan_required) |plan| {
        try writer.writeAll("  Review:     capability review required\n");
        try writeKv(writer, "  Plan ID:    ", plan.plan_id);
        if (plan.review_url) |url| try writeKv(writer, "  URL:        ", url);
        for (plan.covered_reasons) |r| {
            try writer.writeAll("    already granted: ");
            try writer.writeAll(r);
            try writer.writeAll("\n");
        }
        for (plan.uncovered_reasons) |r| {
            try writer.writeAll("    needs review:    ");
            try writer.writeAll(r);
            try writer.writeAll("\n");
        }
        try writer.writeAll("              approve the plan, then re-run\n");
    }
    if (card.drift == null and card.plan_required == null) {
        try writer.writeAll("  Blockers:   none\n");
    }
}

/// Convenience wrapper for callers that already hold a `DeployReview`.
pub fn renderReviewCard(
    allocator: std.mem.Allocator,
    review: *const DeployReview,
    writer: *std.Io.Writer,
    opts: RenderOptions,
) !void {
    _ = allocator;
    _ = opts;
    const card = ProofCard.fromDeployReview(review);
    try writeProofCardPlaintext(&card, writer);
}

fn renderDelta(writer: *std.Io.Writer, delta: *const ReviewDelta) !void {
    if (delta.isEmpty()) {
        try writer.writeAll("  Changes:    (none)\n");
        return;
    }
    try renderSetChange(writer, "Routes", delta.added_routes.len, delta.removed_routes.len);
    try renderSetChange(writer, "Env keys", delta.added_env.len, delta.removed_env.len);
    try renderSetChange(writer, "Egress", delta.added_egress.len, delta.removed_egress.len);
    try renderSetChange(writer, "Caches", delta.added_cache.len, delta.removed_cache.len);
    try renderSetChange(writer, "Caps", delta.added_capabilities.len, delta.removed_capabilities.len);

    for (delta.added_routes) |r| {
        try writer.writeAll("    + route ");
        try writer.writeAll(r.pattern);
        if (r.is_prefix) try writer.writeAll(" (prefix)");
        try writer.writeAll("\n");
    }
    for (delta.removed_routes) |r| {
        try writer.writeAll("    - route ");
        try writer.writeAll(r.pattern);
        if (r.is_prefix) try writer.writeAll(" (prefix)");
        try writer.writeAll("\n");
    }
    for (delta.added_env) |k| try writeBulletLine(writer, "    + env ", k);
    for (delta.removed_env) |k| try writeBulletLine(writer, "    - env ", k);
    for (delta.added_egress) |h| try writeBulletLine(writer, "    + egress ", h);
    for (delta.removed_egress) |h| try writeBulletLine(writer, "    - egress ", h);
    for (delta.added_cache) |n| try writeBulletLine(writer, "    + cache ", n);
    for (delta.removed_cache) |n| try writeBulletLine(writer, "    - cache ", n);
    for (delta.added_capabilities) |c| try writeBulletLine(writer, "    + cap ", c);
    for (delta.removed_capabilities) |c| try writeBulletLine(writer, "    - cap ", c);

    for (delta.promoted_properties) |p| try writeBulletLine(writer, "    + property ", p.label);
    for (delta.demoted_properties) |p| try writeBulletLine(writer, "    - property ", p.label);

    if (delta.proof_level_change) |change| {
        try writer.writeAll("    proof: ");
        try writer.writeAll(change.old.toString());
        try writer.writeAll(" -> ");
        try writer.writeAll(change.new.toString());
        try writer.writeAll("\n");
    }
}

fn renderSetChange(writer: *std.Io.Writer, name: []const u8, added: usize, removed: usize) !void {
    if (added == 0 and removed == 0) return;
    try writer.print("  {s:<10}  +{d} added, -{d} removed\n", .{ name, added, removed });
}

fn writeKv(writer: *std.Io.Writer, prefix: []const u8, value: []const u8) !void {
    try writer.writeAll(prefix);
    try writer.writeAll(value);
    try writer.writeAll("\n");
}

fn writeBulletLine(writer: *std.Io.Writer, prefix: []const u8, value: []const u8) !void {
    try writer.writeAll(prefix);
    try writer.writeAll(value);
    try writer.writeAll("\n");
}

// ---------------------------------------------------------------------------
// String / route helpers
// ---------------------------------------------------------------------------

fn dupeSortedDedupedStrings(
    allocator: std.mem.Allocator,
    items: []const []const u8,
) ![]const []const u8 {
    if (items.len == 0) {
        return try allocator.alloc([]const u8, 0);
    }
    var temp = try allocator.alloc([]const u8, items.len);
    defer allocator.free(temp);
    for (items, 0..) |s, i| temp[i] = s;
    std.mem.sort([]const u8, temp, {}, lessThanSlice);

    var out: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (out.items) |s| allocator.free(s);
        out.deinit(allocator);
    }
    var prev: ?[]const u8 = null;
    for (temp) |s| {
        if (prev) |p| {
            if (std.mem.eql(u8, p, s)) continue;
        }
        const owned = try allocator.dupe(u8, s);
        errdefer allocator.free(owned);
        try out.append(allocator, owned);
        prev = s;
    }
    return try out.toOwnedSlice(allocator);
}

fn dupeSortedDedupedRoutes(
    allocator: std.mem.Allocator,
    items: []const zigts_cli.deploy_manifest.ProvenRoute,
) ![]const Route {
    if (items.len == 0) {
        return try allocator.alloc(Route, 0);
    }
    var temp = try allocator.alloc(Route, items.len);
    defer allocator.free(temp);
    for (items, 0..) |r, i| temp[i] = .{ .pattern = r.pattern, .is_prefix = r.is_prefix };
    std.mem.sort(Route, temp, {}, Route.lessThan);

    var out: std.ArrayList(Route) = .empty;
    errdefer {
        for (out.items) |r| allocator.free(r.pattern);
        out.deinit(allocator);
    }
    var prev: ?Route = null;
    for (temp) |r| {
        if (prev) |p| {
            if (Route.eql(p, r)) continue;
        }
        const pattern = try allocator.dupe(u8, r.pattern);
        errdefer allocator.free(pattern);
        try out.append(allocator, .{ .pattern = pattern, .is_prefix = r.is_prefix });
        prev = r;
    }
    return try out.toOwnedSlice(allocator);
}

fn lessThanSlice(_: void, a: []const u8, b: []const u8) bool {
    return std.mem.order(u8, a, b) == .lt;
}

fn freeStringList(allocator: std.mem.Allocator, items: []const []const u8) void {
    for (items) |s| allocator.free(s);
    allocator.free(items);
}

fn freeRouteList(allocator: std.mem.Allocator, items: []const Route) void {
    for (items) |r| allocator.free(r.pattern);
    allocator.free(items);
}

fn dupeSortedDedupedSpecs(
    allocator: std.mem.Allocator,
    items: []const SpecState,
) ![]const SpecState {
    if (items.len == 0) {
        return try allocator.alloc(SpecState, 0);
    }
    const temp = try allocator.alloc(SpecState, items.len);
    defer allocator.free(temp);
    @memcpy(temp, items);
    std.mem.sort(SpecState, temp, {}, SpecState.lessThan);

    var out: std.ArrayList(SpecState) = .empty;
    errdefer {
        for (out.items) |s| allocator.free(s.name);
        out.deinit(allocator);
    }
    var prev_name: ?[]const u8 = null;
    for (temp) |s| {
        if (prev_name) |p| {
            if (std.mem.eql(u8, p, s.name)) continue;
        }
        const name = try allocator.dupe(u8, s.name);
        errdefer allocator.free(name);
        const code = if (s.diagnostic_code) |c| try allocator.dupe(u8, c) else null;
        errdefer if (code) |c| allocator.free(c);
        const msg = if (s.diagnostic_message) |m| try allocator.dupe(u8, m) else null;
        errdefer if (msg) |m| allocator.free(m);
        const snippet = if (s.source_snippet) |sn| try allocator.dupe(u8, sn) else null;
        errdefer if (snippet) |sn| allocator.free(sn);
        try out.append(allocator, .{
            .name = name,
            .discharged = s.discharged,
            .diagnostic_code = code,
            .diagnostic_message = msg,
            .source_line = s.source_line,
            .source_column = s.source_column,
            .source_snippet = snippet,
        });
        prev_name = s.name;
    }
    return try out.toOwnedSlice(allocator);
}

fn freeSpecList(allocator: std.mem.Allocator, items: []const SpecState) void {
    for (items) |s| {
        allocator.free(s.name);
        if (s.diagnostic_code) |c| allocator.free(c);
        if (s.diagnostic_message) |m| allocator.free(m);
        if (s.source_snippet) |sn| allocator.free(sn);
    }
    allocator.free(items);
}

fn writeStringArray(json: *std.json.Stringify, items: []const []const u8) !void {
    try json.beginArray();
    for (items) |s| try json.write(s);
    try json.endArray();
}

fn parseStringArray(
    allocator: std.mem.Allocator,
    obj: std.json.ObjectMap,
    key: []const u8,
) ![]const []const u8 {
    const value = obj.get(key) orelse return try allocator.alloc([]const u8, 0);
    if (value != .array) return error.InvalidReviewFacts;
    const out = try allocator.alloc([]const u8, value.array.items.len);
    errdefer allocator.free(out);
    var n: usize = 0;
    errdefer for (out[0..n]) |s| allocator.free(s);
    for (value.array.items) |item| {
        if (item != .string) return error.InvalidReviewFacts;
        out[n] = try allocator.dupe(u8, item.string);
        n += 1;
    }
    return out;
}

fn parseSpecArray(allocator: std.mem.Allocator, obj: std.json.ObjectMap) ![]const SpecState {
    const value = obj.get("declaredSpecs") orelse return try allocator.alloc(SpecState, 0);
    if (value != .array) return error.InvalidReviewFacts;
    const out = try allocator.alloc(SpecState, value.array.items.len);
    errdefer allocator.free(out);
    var n: usize = 0;
    errdefer for (out[0..n]) |s| {
        allocator.free(s.name);
        if (s.diagnostic_code) |c| allocator.free(c);
        if (s.diagnostic_message) |m| allocator.free(m);
        if (s.source_snippet) |sn| allocator.free(sn);
    };
    for (value.array.items) |item| {
        if (item != .object) return error.InvalidReviewFacts;
        const name_str = json_util.getString(item.object, "name") orelse return error.InvalidReviewFacts;
        const discharged = blk: {
            const v = item.object.get("discharged") orelse break :blk false;
            if (v != .bool) return error.InvalidReviewFacts;
            break :blk v.bool;
        };
        const code: ?[]const u8 = if (json_util.getString(item.object, "diagnosticCode")) |s|
            try allocator.dupe(u8, s)
        else
            null;
        errdefer if (code) |c| allocator.free(c);
        const message: ?[]const u8 = if (json_util.getString(item.object, "diagnosticMessage")) |s|
            try allocator.dupe(u8, s)
        else
            null;
        errdefer if (message) |m| allocator.free(m);
        const source_line: ?u32 = blk: {
            const v = item.object.get("sourceLine") orelse break :blk null;
            if (v != .integer) break :blk null;
            if (v.integer < 0 or v.integer > std.math.maxInt(u32)) break :blk null;
            break :blk @intCast(v.integer);
        };
        const source_column: ?u16 = blk: {
            const v = item.object.get("sourceColumn") orelse break :blk null;
            if (v != .integer) break :blk null;
            if (v.integer < 0 or v.integer > std.math.maxInt(u16)) break :blk null;
            break :blk @intCast(v.integer);
        };
        const snippet: ?[]const u8 = if (json_util.getString(item.object, "sourceSnippet")) |s|
            try allocator.dupe(u8, s)
        else
            null;
        errdefer if (snippet) |sn| allocator.free(sn);
        out[n] = .{
            .name = try allocator.dupe(u8, name_str),
            .discharged = discharged,
            .diagnostic_code = code,
            .diagnostic_message = message,
            .source_line = source_line,
            .source_column = source_column,
            .source_snippet = snippet,
        };
        n += 1;
    }
    return out;
}

fn parseRouteArray(allocator: std.mem.Allocator, obj: std.json.ObjectMap) ![]const Route {
    const value = obj.get("routes") orelse return try allocator.alloc(Route, 0);
    if (value != .array) return error.InvalidReviewFacts;
    const out = try allocator.alloc(Route, value.array.items.len);
    errdefer allocator.free(out);
    var n: usize = 0;
    errdefer for (out[0..n]) |r| allocator.free(r.pattern);
    for (value.array.items) |item| {
        if (item != .object) return error.InvalidReviewFacts;
        const pattern_str = json_util.getString(item.object, "pattern") orelse return error.InvalidReviewFacts;
        const is_prefix = blk: {
            const v = item.object.get("isPrefix") orelse break :blk false;
            if (v != .bool) return error.InvalidReviewFacts;
            break :blk v.bool;
        };
        out[n] = .{
            .pattern = try allocator.dupe(u8, pattern_str),
            .is_prefix = is_prefix,
        };
        n += 1;
    }
    return out;
}

fn setDiff(
    allocator: std.mem.Allocator,
    a: []const []const u8,
    b: []const []const u8,
) ![]const []const u8 {
    var out: std.ArrayList([]const u8) = .empty;
    errdefer out.deinit(allocator);
    for (a) |s| {
        if (!containsString(b, s)) try out.append(allocator, s);
    }
    return try out.toOwnedSlice(allocator);
}

fn routeDiff(
    allocator: std.mem.Allocator,
    a: []const Route,
    b: []const Route,
) ![]const Route {
    var out: std.ArrayList(Route) = .empty;
    errdefer out.deinit(allocator);
    for (a) |r| {
        if (!containsRoute(b, r)) try out.append(allocator, r);
    }
    return try out.toOwnedSlice(allocator);
}

fn containsString(haystack: []const []const u8, needle: []const u8) bool {
    for (haystack) |s| {
        if (std.mem.eql(u8, s, needle)) return true;
    }
    return false;
}

fn containsRoute(haystack: []const Route, needle: Route) bool {
    for (haystack) |r| {
        if (Route.eql(r, needle)) return true;
    }
    return false;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "classify: empty delta returns safe" {
    const delta: ReviewDelta = .{};
    try std.testing.expectEqual(Verdict.safe, classify(&delta));
}

test "classify: only additions returns safe_with_additions" {
    const added = [_][]const u8{"NEW_KEY"};
    const delta = ReviewDelta{ .added_env = &added };
    try std.testing.expectEqual(Verdict.safe_with_additions, classify(&delta));
}

test "classify: any removal is breaking" {
    const removed = [_][]const u8{"GONE_KEY"};
    const delta = ReviewDelta{ .removed_env = &removed };
    try std.testing.expectEqual(Verdict.breaking, classify(&delta));
}

test "classify: property demotion is breaking" {
    const demoted = [_]PropertyChange{.{
        .name = "results_safe",
        .label = "results-safe",
        .old_value = true,
        .new_value = false,
    }};
    const delta = ReviewDelta{ .demoted_properties = &demoted };
    try std.testing.expectEqual(Verdict.breaking, classify(&delta));
}

test "classify: proof level downgrade is breaking" {
    const delta = ReviewDelta{
        .proof_level_change = .{ .old = .complete, .new = .partial },
    };
    try std.testing.expectEqual(Verdict.breaking, classify(&delta));
}

test "classify: proof level upgrade is safe_with_additions" {
    const delta = ReviewDelta{
        .proof_level_change = .{ .old = .partial, .new = .complete },
    };
    try std.testing.expectEqual(Verdict.safe_with_additions, classify(&delta));
}

test "deriveDelta: no baseline yields empty delta" {
    const allocator = std.testing.allocator;
    const current = ReviewFacts{
        .contract_sha = "sha-current",
        .proof_level = .complete,
        .env_keys = &[_][]const u8{"PORT"},
        .egress_hosts = &.{},
        .cache_namespaces = &.{},
        .routes = &.{},
        .capabilities = &.{},
        .properties = .{},
    };
    var delta = try deriveDelta(allocator, &current, null);
    defer delta.deinit(allocator);
    try std.testing.expect(delta.isEmpty());
    try std.testing.expectEqual(Verdict.safe, classify(&delta));
}

test "deriveDelta: detects added env, removed route, demoted property" {
    const allocator = std.testing.allocator;
    var baseline = ReviewFacts{
        .contract_sha = "sha-old",
        .proof_level = .complete,
        .env_keys = try allocator.dupe([]const u8, &[_][]const u8{"PORT"}),
        .egress_hosts = &.{},
        .cache_namespaces = &.{},
        .routes = blk: {
            var arr = try allocator.alloc(Route, 1);
            arr[0] = .{ .pattern = try allocator.dupe(u8, "/api/v1/users"), .is_prefix = false };
            break :blk arr;
        },
        .capabilities = &.{},
        .properties = .{ .results_safe = true },
    };
    defer {
        allocator.free(baseline.env_keys);
        for (baseline.routes) |r| allocator.free(r.pattern);
        allocator.free(baseline.routes);
    }

    var current = ReviewFacts{
        .contract_sha = "sha-new",
        .proof_level = .complete,
        .env_keys = try allocator.dupe([]const u8, &[_][]const u8{ "PORT", "DB_URL" }),
        .egress_hosts = &.{},
        .cache_namespaces = &.{},
        .routes = try allocator.alloc(Route, 0),
        .capabilities = &.{},
        .properties = .{ .results_safe = false },
    };
    defer {
        allocator.free(current.env_keys);
        allocator.free(current.routes);
    }

    var delta = try deriveDelta(allocator, &current, &baseline);
    defer delta.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 1), delta.added_env.len);
    try std.testing.expectEqualStrings("DB_URL", delta.added_env[0]);
    try std.testing.expectEqual(@as(usize, 1), delta.removed_routes.len);
    try std.testing.expectEqualStrings("/api/v1/users", delta.removed_routes[0].pattern);
    try std.testing.expectEqual(@as(usize, 1), delta.demoted_properties.len);
    try std.testing.expectEqualStrings("results_safe", delta.demoted_properties[0].name);
    try std.testing.expectEqual(Verdict.breaking, classify(&delta));
}

test "fromProvenFacts: projects every field from ProvenFacts" {
    const allocator = std.testing.allocator;
    const env_in = [_][]const u8{ "PORT", "DB_URL" };
    const egress_in = [_][]const u8{"api.example.com"};
    const cache_in = [_][]const u8{"sessions"};
    const caps_in = [_][]const u8{ "network", "clock" };
    const ProvenRoute = zigts_cli.deploy_manifest.ProvenRoute;
    const routes_in = [_]ProvenRoute{
        .{ .pattern = "/users", .is_prefix = true },
        .{ .pattern = "/healthz", .is_prefix = false },
    };

    const facts = ProvenFacts{
        .handler_name = "demo",
        .handler_path = "src/handler.ts",
        .env_vars = &env_in,
        .env_proven = true,
        .egress_hosts = &egress_in,
        .egress_proven = true,
        .cache_namespaces = &cache_in,
        .cache_proven = true,
        .routes = &routes_in,
        .proof_level = .complete,
        .checks_passed = &.{},
        .retry_safe = true,
        .read_only = true,
        .injection_safe = false,
        .idempotent = true,
        .state_isolated = false,
        .no_secret_leakage = false,
        .no_credential_leakage = false,
        .input_validated = false,
        .pii_contained = false,
        .results_safe = true,
        .fault_covered = true,
    };

    var review = try ReviewFacts.fromProvenFacts(allocator, &facts, &caps_in, "sha-xyz", &.{});
    defer review.deinit(allocator);

    try std.testing.expectEqualStrings("sha-xyz", review.contract_sha);
    try std.testing.expectEqual(ProofLevel.complete, review.proof_level);

    // env_keys are sorted+deduped (DB_URL < PORT lexicographically).
    try std.testing.expectEqual(@as(usize, 2), review.env_keys.len);
    try std.testing.expectEqualStrings("DB_URL", review.env_keys[0]);
    try std.testing.expectEqualStrings("PORT", review.env_keys[1]);

    try std.testing.expectEqual(@as(usize, 1), review.egress_hosts.len);
    try std.testing.expectEqualStrings("api.example.com", review.egress_hosts[0]);

    try std.testing.expectEqual(@as(usize, 1), review.cache_namespaces.len);
    try std.testing.expectEqualStrings("sessions", review.cache_namespaces[0]);

    // capabilities sorted: clock < network.
    try std.testing.expectEqual(@as(usize, 2), review.capabilities.len);
    try std.testing.expectEqualStrings("clock", review.capabilities[0]);
    try std.testing.expectEqualStrings("network", review.capabilities[1]);

    // routes sorted by pattern: /healthz < /users.
    try std.testing.expectEqual(@as(usize, 2), review.routes.len);
    try std.testing.expectEqualStrings("/healthz", review.routes[0].pattern);
    try std.testing.expect(!review.routes[0].is_prefix);
    try std.testing.expectEqualStrings("/users", review.routes[1].pattern);
    try std.testing.expect(review.routes[1].is_prefix);

    // Property booleans projected verbatim.
    try std.testing.expect(review.properties.retry_safe);
    try std.testing.expect(review.properties.read_only);
    try std.testing.expect(!review.properties.injection_safe);
    try std.testing.expect(review.properties.idempotent);
    try std.testing.expect(!review.properties.state_isolated);
    try std.testing.expect(!review.properties.no_secret_leakage);
    try std.testing.expect(!review.properties.no_credential_leakage);
    try std.testing.expect(!review.properties.input_validated);
    try std.testing.expect(!review.properties.pii_contained);
    try std.testing.expect(review.properties.results_safe);
    try std.testing.expect(review.properties.fault_covered);
}

test "fromProvenFacts: dedupes duplicate env keys" {
    const allocator = std.testing.allocator;
    const env_in = [_][]const u8{ "PORT", "DB_URL", "PORT" };
    const facts = ProvenFacts{
        .handler_name = "demo",
        .handler_path = "src/handler.ts",
        .env_vars = &env_in,
        .env_proven = true,
        .egress_hosts = &.{},
        .egress_proven = true,
        .cache_namespaces = &.{},
        .cache_proven = true,
        .routes = &.{},
        .proof_level = .none,
        .checks_passed = &.{},
    };

    var review = try ReviewFacts.fromProvenFacts(allocator, &facts, &.{}, "sha-1", &.{});
    defer review.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 2), review.env_keys.len);
    try std.testing.expectEqualStrings("DB_URL", review.env_keys[0]);
    try std.testing.expectEqualStrings("PORT", review.env_keys[1]);
}

test "ReviewFacts: writeJson then parseJson round trips" {
    const allocator = std.testing.allocator;
    var facts = ReviewFacts{
        .contract_sha = try allocator.dupe(u8, "sha-1"),
        .proof_level = .complete,
        .env_keys = blk: {
            var arr = try allocator.alloc([]const u8, 2);
            arr[0] = try allocator.dupe(u8, "DB_URL");
            arr[1] = try allocator.dupe(u8, "PORT");
            break :blk arr;
        },
        .egress_hosts = blk: {
            var arr = try allocator.alloc([]const u8, 1);
            arr[0] = try allocator.dupe(u8, "api.example.com");
            break :blk arr;
        },
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = blk: {
            var arr = try allocator.alloc(Route, 1);
            arr[0] = .{ .pattern = try allocator.dupe(u8, "/users"), .is_prefix = true };
            break :blk arr;
        },
        .capabilities = blk: {
            var arr = try allocator.alloc([]const u8, 1);
            arr[0] = try allocator.dupe(u8, "network");
            break :blk arr;
        },
        .properties = .{ .results_safe = true, .read_only = true },
    };
    defer facts.deinit(allocator);

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try facts.writeJson(&json);

    const bytes = aw.writer.buffered();
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, bytes, .{});
    defer parsed.deinit();
    try std.testing.expect(parsed.value == .object);

    var round = try ReviewFacts.parseJson(allocator, parsed.value.object);
    defer round.deinit(allocator);

    try std.testing.expectEqualStrings(facts.contract_sha, round.contract_sha);
    try std.testing.expectEqual(facts.proof_level, round.proof_level);
    try std.testing.expectEqual(@as(usize, 2), round.env_keys.len);
    try std.testing.expectEqualStrings("DB_URL", round.env_keys[0]);
    try std.testing.expectEqualStrings("PORT", round.env_keys[1]);
    try std.testing.expectEqual(@as(usize, 1), round.egress_hosts.len);
    try std.testing.expectEqualStrings("api.example.com", round.egress_hosts[0]);
    try std.testing.expectEqual(@as(usize, 1), round.routes.len);
    try std.testing.expectEqualStrings("/users", round.routes[0].pattern);
    try std.testing.expect(round.routes[0].is_prefix);
    try std.testing.expectEqual(@as(usize, 1), round.capabilities.len);
    try std.testing.expectEqualStrings("network", round.capabilities[0]);
    try std.testing.expect(round.properties.results_safe);
    try std.testing.expect(round.properties.read_only);
    try std.testing.expect(!round.properties.retry_safe);
}

test "ReviewFacts.setIntentSummary surfaces intent fields in JSON" {
    const allocator = std.testing.allocator;
    var facts = ReviewFacts{
        .contract_sha = try allocator.dupe(u8, "sha-intent"),
        .proof_level = .complete,
        .env_keys = try allocator.alloc([]const u8, 0),
        .egress_hosts = try allocator.alloc([]const u8, 0),
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = try allocator.alloc(Route, 0),
        .capabilities = try allocator.alloc([]const u8, 0),
        .properties = .{},
    };
    defer facts.deinit(allocator);
    facts.setIntentSummary(.{ .assertion_count = 3, .dynamic = false, .behavior_paths = 7, .failure_paths = 2 });

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try facts.writeJson(&json);
    const bytes = aw.writer.buffered();

    try std.testing.expect(std.mem.indexOf(u8, bytes, "\"intentAssertionCount\":3") != null);
    try std.testing.expect(std.mem.indexOf(u8, bytes, "\"intentDynamic\":false") != null);
    try std.testing.expect(std.mem.indexOf(u8, bytes, "\"behaviorPathCount\":7") != null);
    try std.testing.expect(std.mem.indexOf(u8, bytes, "\"failurePathCount\":2") != null);
}

test "ReviewFacts intent fields default to zero in JSON" {
    const allocator = std.testing.allocator;
    var facts = ReviewFacts{
        .contract_sha = try allocator.dupe(u8, "sha-default"),
        .proof_level = .none,
        .env_keys = try allocator.alloc([]const u8, 0),
        .egress_hosts = try allocator.alloc([]const u8, 0),
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = try allocator.alloc(Route, 0),
        .capabilities = try allocator.alloc([]const u8, 0),
        .properties = .{},
    };
    defer facts.deinit(allocator);

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try facts.writeJson(&json);
    const bytes = aw.writer.buffered();

    try std.testing.expect(std.mem.indexOf(u8, bytes, "\"intentAssertionCount\":0") != null);
    try std.testing.expect(std.mem.indexOf(u8, bytes, "\"intentDynamic\":false") != null);
}

test "ReviewFacts.parseJson tolerates missing optional fields" {
    const allocator = std.testing.allocator;
    const input = "{\"contractSha\":\"sha-x\"}";
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, input, .{});
    defer parsed.deinit();
    var facts = try ReviewFacts.parseJson(allocator, parsed.value.object);
    defer facts.deinit(allocator);
    try std.testing.expectEqualStrings("sha-x", facts.contract_sha);
    try std.testing.expectEqual(ProofLevel.none, facts.proof_level);
    try std.testing.expectEqual(@as(usize, 0), facts.env_keys.len);
    try std.testing.expectEqual(@as(usize, 0), facts.routes.len);
    try std.testing.expect(!facts.properties.results_safe);
    try std.testing.expect(facts.properties.injection_safe); // default true
}

test "renderReviewCard: first deploy shows baseline none and no blockers" {
    const allocator = std.testing.allocator;
    var current = ReviewFacts{
        .contract_sha = try allocator.dupe(u8, "sha-1"),
        .proof_level = .complete,
        .env_keys = blk: {
            var arr = try allocator.alloc([]const u8, 1);
            arr[0] = try allocator.dupe(u8, "PORT");
            break :blk arr;
        },
        .egress_hosts = try allocator.alloc([]const u8, 0),
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = try allocator.alloc(Route, 0),
        .capabilities = try allocator.alloc([]const u8, 0),
        .properties = .{ .results_safe = true },
    };
    var delta = try deriveDelta(allocator, &current, null);
    var review = DeployReview{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "us-east",
        .current = current,
        .baseline = null,
        .delta = delta,
    };
    defer review.deinit(allocator);
    _ = &delta;

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try renderReviewCard(allocator, &review, &aw.writer, .{});

    const out = aw.writer.buffered();
    try std.testing.expect(std.mem.indexOf(u8, out, "Proof review:") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Handler:    src/handler.ts") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Service:    demo") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Proof:      complete") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "results-safe") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Baseline:   none (first deploy)") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Verdict:    safe") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Blockers:   none") != null);
}

test "renderReviewCard: drift section is included when drift is set" {
    const allocator = std.testing.allocator;
    var current = ReviewFacts{
        .contract_sha = try allocator.dupe(u8, "sha-2"),
        .proof_level = .complete,
        .env_keys = try allocator.alloc([]const u8, 0),
        .egress_hosts = try allocator.alloc([]const u8, 0),
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = try allocator.alloc(Route, 0),
        .capabilities = try allocator.alloc([]const u8, 0),
        .properties = .{},
    };
    var delta = try deriveDelta(allocator, &current, null);
    var review = DeployReview{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "us-east",
        .current = current,
        .delta = delta,
        .drift = .{ .reason = "scope changed" },
    };
    defer review.deinit(allocator);
    _ = &delta;

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try renderReviewCard(allocator, &review, &aw.writer, .{});
    const out = aw.writer.buffered();
    try std.testing.expect(std.mem.indexOf(u8, out, "Drift:      scope changed") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "--confirm") != null);
}

test "renderReviewCard: additive deploy shows added route and env" {
    const allocator = std.testing.allocator;
    var baseline = ReviewFacts{
        .contract_sha = try allocator.dupe(u8, "sha-old"),
        .proof_level = .complete,
        .env_keys = blk: {
            var arr = try allocator.alloc([]const u8, 1);
            arr[0] = try allocator.dupe(u8, "PORT");
            break :blk arr;
        },
        .egress_hosts = try allocator.alloc([]const u8, 0),
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = try allocator.alloc(Route, 0),
        .capabilities = try allocator.alloc([]const u8, 0),
        .properties = .{},
    };
    defer baseline.deinit(allocator);
    var current = ReviewFacts{
        .contract_sha = try allocator.dupe(u8, "sha-new"),
        .proof_level = .complete,
        .env_keys = blk: {
            var arr = try allocator.alloc([]const u8, 2);
            arr[0] = try allocator.dupe(u8, "DB_URL");
            arr[1] = try allocator.dupe(u8, "PORT");
            break :blk arr;
        },
        .egress_hosts = try allocator.alloc([]const u8, 0),
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = blk: {
            var arr = try allocator.alloc(Route, 1);
            arr[0] = .{ .pattern = try allocator.dupe(u8, "/healthz"), .is_prefix = false };
            break :blk arr;
        },
        .capabilities = try allocator.alloc([]const u8, 0),
        .properties = .{},
    };
    var review = DeployReview{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "us-east",
        .current = current,
        .baseline = &baseline,
        .delta = try deriveDelta(allocator, &current, &baseline),
    };
    defer review.deinit(allocator);

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try renderReviewCard(allocator, &review, &aw.writer, .{});
    const out = aw.writer.buffered();
    try std.testing.expect(std.mem.indexOf(u8, out, "Baseline:   sha-old") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "+ env DB_URL") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "+ route /healthz") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Verdict:    safe_with_additions") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Blockers:   none") != null);
}

test "renderReviewCard: demoted property is breaking with bullet" {
    const allocator = std.testing.allocator;
    var baseline = ReviewFacts{
        .contract_sha = try allocator.dupe(u8, "sha-old"),
        .proof_level = .complete,
        .env_keys = try allocator.alloc([]const u8, 0),
        .egress_hosts = try allocator.alloc([]const u8, 0),
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = try allocator.alloc(Route, 0),
        .capabilities = try allocator.alloc([]const u8, 0),
        .properties = .{ .read_only = true, .results_safe = true },
    };
    defer baseline.deinit(allocator);
    var current = ReviewFacts{
        .contract_sha = try allocator.dupe(u8, "sha-new"),
        .proof_level = .complete,
        .env_keys = try allocator.alloc([]const u8, 0),
        .egress_hosts = try allocator.alloc([]const u8, 0),
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = try allocator.alloc(Route, 0),
        .capabilities = try allocator.alloc([]const u8, 0),
        .properties = .{ .read_only = false, .results_safe = true },
    };
    var review = DeployReview{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "us-east",
        .current = current,
        .baseline = &baseline,
        .delta = try deriveDelta(allocator, &current, &baseline),
    };
    defer review.deinit(allocator);

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try renderReviewCard(allocator, &review, &aw.writer, .{});
    const out = aw.writer.buffered();
    try std.testing.expect(std.mem.indexOf(u8, out, "- property read-only") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Verdict:    breaking") != null);
}

test "renderReviewCard: promoted property renders + bullet" {
    const allocator = std.testing.allocator;
    var baseline = ReviewFacts{
        .contract_sha = try allocator.dupe(u8, "sha-old"),
        .proof_level = .complete,
        .env_keys = try allocator.alloc([]const u8, 0),
        .egress_hosts = try allocator.alloc([]const u8, 0),
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = try allocator.alloc(Route, 0),
        .capabilities = try allocator.alloc([]const u8, 0),
        .properties = .{ .read_only = false },
    };
    defer baseline.deinit(allocator);
    var current = ReviewFacts{
        .contract_sha = try allocator.dupe(u8, "sha-new"),
        .proof_level = .complete,
        .env_keys = try allocator.alloc([]const u8, 0),
        .egress_hosts = try allocator.alloc([]const u8, 0),
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = try allocator.alloc(Route, 0),
        .capabilities = try allocator.alloc([]const u8, 0),
        .properties = .{ .read_only = true },
    };
    var review = DeployReview{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "us-east",
        .current = current,
        .baseline = &baseline,
        .delta = try deriveDelta(allocator, &current, &baseline),
    };
    defer review.deinit(allocator);

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try renderReviewCard(allocator, &review, &aw.writer, .{});
    const out = aw.writer.buffered();
    try std.testing.expect(std.mem.indexOf(u8, out, "+ property read-only") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Verdict:    safe_with_additions") != null);
}

test "renderReviewCard: plan_required section lists reasons" {
    const allocator = std.testing.allocator;
    var current = ReviewFacts{
        .contract_sha = try allocator.dupe(u8, "sha-3"),
        .proof_level = .complete,
        .env_keys = try allocator.alloc([]const u8, 0),
        .egress_hosts = try allocator.alloc([]const u8, 0),
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = try allocator.alloc(Route, 0),
        .capabilities = try allocator.alloc([]const u8, 0),
        .properties = .{},
    };
    var delta = try deriveDelta(allocator, &current, null);
    const covered = [_][]const u8{"network egress to api.x"};
    const uncovered = [_][]const u8{"crypto capability"};
    var review = DeployReview{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "us-east",
        .current = current,
        .delta = delta,
        .plan_required = .{
            .plan_id = "plan-1",
            .review_url = "https://control/deploy/plans/plan-1",
            .covered_reasons = &covered,
            .uncovered_reasons = &uncovered,
        },
    };
    defer review.deinit(allocator);
    _ = &delta;

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try renderReviewCard(allocator, &review, &aw.writer, .{});
    const out = aw.writer.buffered();
    try std.testing.expect(std.mem.indexOf(u8, out, "capability review required") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Plan ID:    plan-1") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "URL:        https://control/deploy/plans/plan-1") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "already granted: network egress to api.x") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "needs review:    crypto capability") != null);
}

test "writeProofCardPlaintext: renders a card built without a DeployReview" {
    const allocator = std.testing.allocator;
    var current = ReviewFacts{
        .contract_sha = try allocator.dupe(u8, "sha-card"),
        .proof_level = .complete,
        .env_keys = blk: {
            var arr = try allocator.alloc([]const u8, 1);
            arr[0] = try allocator.dupe(u8, "PORT");
            break :blk arr;
        },
        .egress_hosts = try allocator.alloc([]const u8, 0),
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = try allocator.alloc(Route, 0),
        .capabilities = try allocator.alloc([]const u8, 0),
        .properties = .{ .read_only = true },
    };
    defer current.deinit(allocator);
    var delta = try deriveDelta(allocator, &current, null);
    defer delta.deinit(allocator);

    const card = ProofCard{
        .handler_path = "src/handler.ts",
        .service_name = "demo",
        .region = "us-east",
        .current = &current,
        .baseline = null,
        .delta = &delta,
    };

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try writeProofCardPlaintext(&card, &aw.writer);
    const out = aw.writer.buffered();

    try std.testing.expect(std.mem.indexOf(u8, out, "Contract:   sha-card") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "read-only") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Baseline:   none (first deploy)") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Verdict:    safe") != null);
}
