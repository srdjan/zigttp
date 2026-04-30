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
    .{ .name = "no_secret_leakage", .field = .no_secret_leakage, .cause_only = false },
    .{ .name = "no_credential_leakage", .field = .no_credential_leakage, .cause_only = false },
    .{ .name = "input_validated", .field = .input_validated, .cause_only = false },
    .{ .name = "pii_contained", .field = .pii_contained, .cause_only = false },
    .{ .name = "injection_safe", .field = .injection_safe, .cause_only = false },
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
    no_secret_leakage,
    no_credential_leakage,
    input_validated,
    pii_contained,
    injection_safe,

    pub fn lookup(self: PropertyField, properties: HandlerProperties) bool {
        return switch (self) {
            .deterministic => properties.deterministic,
            .read_only => properties.read_only,
            .retry_safe => properties.retry_safe,
            .idempotent => properties.idempotent,
            .state_isolated => properties.state_isolated,
            .fault_covered => properties.fault_covered,
            .no_secret_leakage => properties.no_secret_leakage,
            .no_credential_leakage => properties.no_credential_leakage,
            .input_validated => properties.input_validated,
            .pii_contained => properties.pii_contained,
            .injection_safe => properties.injection_safe,
        };
    }
};

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
    return null;
}

const incompatible_modules_for_read_only = [_][]const u8{
    "zigttp:cache",
    "zigttp:sql",
};

/// Compute the set of diagnostics for a handler given its declared specs,
/// classified properties, and imported modules. The caller owns the
/// returned ArrayList and is responsible for calling `SpecDiagnostic.deinit`
/// on each entry before deinit-ing the list.
pub fn dischargeSpecs(
    allocator: std.mem.Allocator,
    declared: []const []const u8,
    properties: ?HandlerProperties,
    modules: []const []const u8,
) !std.ArrayList(SpecDiagnostic) {
    var out: std.ArrayList(SpecDiagnostic) = .empty;
    errdefer {
        for (out.items) |*d| @constCast(d).deinit(allocator);
        out.deinit(allocator);
    }

    for (declared) |name| {
        const spec = lookupV1(name);

        if (spec == null) {
            try out.append(allocator, .{
                .kind = .unknown_name,
                .spec_name = try allocator.dupe(u8, name),
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
                    try out.append(allocator, .{
                        .kind = .incompatible_with_import,
                        .spec_name = try allocator.dupe(u8, name),
                        .incompatible_module = try allocator.dupe(u8, module_name),
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
        const props = properties orelse {
            try appendNotDischarged(allocator, &out, name);
            continue;
        };

        if (!spec.?.field.lookup(props)) {
            try appendNotDischarged(allocator, &out, name);
        }
    }

    return out;
}

fn appendNotDischarged(
    allocator: std.mem.Allocator,
    out: *std.ArrayList(SpecDiagnostic),
    name: []const u8,
) !void {
    const spec_name = try allocator.dupe(u8, name);
    errdefer allocator.free(spec_name);
    const suggestion = if (suggestionFor(name)) |s| try allocator.dupe(u8, s) else null;
    errdefer if (suggestion) |s| allocator.free(s);
    try out.append(allocator, .{
        .kind = .not_discharged,
        .spec_name = spec_name,
        .suggestion = suggestion,
    });
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

    var diags = try dischargeSpecs(allocator, &declared, props, &modules);
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

    var diags = try dischargeSpecs(allocator, &declared, props, &modules);
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

    var diags = try dischargeSpecs(allocator, &declared, props, &modules);
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

    var diags = try dischargeSpecs(allocator, &declared, props, &modules);
    defer {
        for (diags.items) |*d| @constCast(d).deinit(allocator);
        diags.deinit(allocator);
    }
    // Only the unknown name fails; idempotent is true so no ZTS401.
    try std.testing.expectEqual(@as(usize, 1), diags.items.len);
    try std.testing.expectEqual(SpecDiagnostic.Kind.unknown_name, diags.items[0].kind);
    try std.testing.expectEqualStrings("made_up_name", diags.items[0].spec_name);
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

    var diags = try dischargeSpecs(allocator, &declared, props, &modules);
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
