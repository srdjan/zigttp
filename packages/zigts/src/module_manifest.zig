//! Proof-carrying virtual module manifest parsing and validation.
//!
//! The manifest is the stable semantic surface for module metadata. Native
//! Zig bindings still provide the executable implementation today, but this
//! parser lets tools validate the proof metadata structurally instead of
//! scraping JSON with string searches.

const std = @import("std");
const mb = @import("module_binding.zig");

pub const ManifestError = error{
    InvalidJson,
    InvalidManifest,
    UnsupportedSchemaVersion,
    InvalidSpecifier,
    InvalidBackend,
    InvalidStateModel,
    InvalidCapability,
    InvalidExport,
    InvalidEffect,
    InvalidReturnKind,
    InvalidFailureSeverity,
    InvalidDataLabel,
    InvalidContractCategory,
    InvalidContractTransform,
    InvalidContractFlag,
    InvalidLaw,
    DuplicateExport,
} || std.mem.Allocator.Error;

pub const Backend = enum {
    native_zig,
    wasm_component,
};

pub const StateModel = enum {
    none,
    instance,
    shared,
};

/// Manifest-derived contract extraction rule. Mirrors
/// `module_binding.ContractExtraction` but owns its `extension_category`
/// string instead of borrowing a comptime constant.
pub const ContractExtractionRule = struct {
    arg_position: u8 = 0,
    category: mb.ContractCategory,
    transform: ?mb.ContractTransform = null,
    flag_only: bool = false,
    /// Owned. Non-null only when `category == .extension_specific`.
    extension_category: ?[]u8 = null,

    pub fn deinit(self: *ContractExtractionRule, allocator: std.mem.Allocator) void {
        if (self.extension_category) |slice| allocator.free(slice);
        self.extension_category = null;
    }
};

/// One entry in a manifest's `requiredCapabilities` list.
///
/// Partners that need a capability outside the closed `ModuleCapability`
/// enum can still declare it in the manifest by writing an object form:
///
/// ```json
/// "requiredCapabilities": [
///   { "name": "llm_egress", "inherits": "network" },
///   "clock"
/// ]
/// ```
///
/// `inherits` resolves to the existing enum tag that the manifest validator
/// and audit fence enforce against. `name` is the partner-declared semantic
/// label; it is preserved for tooling (extension-status) but not used for
/// enforcement, since the runtime capability fence is structural and only
/// understands the closed enum. Bare-string entries (the original form)
/// have `partner_name == null`.
pub const CapabilityDeclaration = struct {
    effective: mb.ModuleCapability,
    /// Owned. Non-null when the manifest used the object form.
    partner_name: ?[]u8 = null,

    pub fn deinit(self: *CapabilityDeclaration, allocator: std.mem.Allocator) void {
        if (self.partner_name) |slice| allocator.free(slice);
        self.partner_name = null;
    }
};

pub const Export = struct {
    name: []const u8,
    effect: mb.EffectClass,
    returns: mb.ReturnKind,
    failure_severity: mb.FailureSeverity,
    traceable: bool,
    return_labels: mb.LabelSet,
    contract_extractions: std.ArrayList(ContractExtractionRule),

    pub fn deinit(self: *Export, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        for (self.contract_extractions.items) |*rule| rule.deinit(allocator);
        self.contract_extractions.deinit(allocator);
    }
};

pub const Manifest = struct {
    schema_version: u32,
    specifier: []const u8,
    backend: ?Backend,
    state_model: ?StateModel,
    required_capabilities: std.ArrayList(CapabilityDeclaration),
    exports: std.ArrayList(Export),
    /// Optional partner-declared top-level section name. When set, the
    /// contract writer mirrors this extension's category buckets into a
    /// `<name>` block at the top of contract.json, giving partners parity
    /// with built-ins like `cache` and `durable`. Owned by the manifest;
    /// null when the partner is content with the per-specifier namespace
    /// under `extensions.<specifier>`.
    contract_section: ?[]u8 = null,

    pub fn deinit(self: *Manifest, allocator: std.mem.Allocator) void {
        allocator.free(self.specifier);
        for (self.required_capabilities.items) |*decl| decl.deinit(allocator);
        self.required_capabilities.deinit(allocator);
        for (self.exports.items) |*exp| exp.deinit(allocator);
        self.exports.deinit(allocator);
        if (self.contract_section) |slice| allocator.free(slice);
    }
};

pub fn parse(allocator: std.mem.Allocator, bytes: []const u8) ManifestError!Manifest {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, bytes, .{}) catch return error.InvalidJson;
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidManifest;
    const root = parsed.value.object;

    const schema_version = try parseSchemaVersion(root.get("schemaVersion") orelse root.get("schema_version"));
    if (schema_version != 1) return error.UnsupportedSchemaVersion;

    const specifier = try stringField(root, "specifier");
    if (!validSpecifier(specifier)) return error.InvalidSpecifier;

    var manifest = Manifest{
        .schema_version = schema_version,
        .specifier = try allocator.dupe(u8, specifier),
        .backend = null,
        .state_model = null,
        .required_capabilities = .empty,
        .exports = .empty,
        .contract_section = null,
    };
    errdefer manifest.deinit(allocator);

    if (optionalStringField(root, "contractSection") orelse optionalStringField(root, "contract_section")) |raw| {
        if (!validContractSectionName(raw)) return error.InvalidContractFlag;
        if (reservedContractSectionName(raw)) return error.InvalidContractFlag;
        manifest.contract_section = try allocator.dupe(u8, raw);
    }

    if (optionalStringField(root, "backend")) |backend_raw| {
        manifest.backend = parseBackend(backend_raw) orelse return error.InvalidBackend;
    }
    if (optionalStringField(root, "stateModel") orelse optionalStringField(root, "state_model")) |state_raw| {
        manifest.state_model = parseStateModel(state_raw) orelse return error.InvalidStateModel;
    }

    if (root.get("requiredCapabilities") orelse root.get("required_capabilities")) |caps_value| {
        if (caps_value != .array) return error.InvalidCapability;
        for (caps_value.array.items) |item| {
            const decl = try parseCapabilityDeclaration(allocator, item);
            if (containsCapability(manifest.required_capabilities.items, decl.effective)) {
                var owned = decl;
                owned.deinit(allocator);
                return error.InvalidCapability;
            }
            manifest.required_capabilities.append(allocator, decl) catch |err| {
                var owned = decl;
                owned.deinit(allocator);
                return err;
            };
        }
    }

    const exports_value = root.get("exports") orelse return error.InvalidManifest;
    if (exports_value != .array) return error.InvalidExport;
    for (exports_value.array.items) |item| {
        const exp = try parseExport(allocator, item);
        if (containsExport(manifest.exports.items, exp.name)) {
            var owned = exp;
            owned.deinit(allocator);
            return error.DuplicateExport;
        }
        manifest.exports.append(allocator, exp) catch |err| {
            var owned = exp;
            owned.deinit(allocator);
            return err;
        };
    }

    return manifest;
}

pub fn registryHashFromBindings(comptime bindings: []const mb.ModuleBinding) [64]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    inline for (bindings) |binding| {
        hasher.update(binding.specifier);
        hasher.update("\x00");
        hasher.update(binding.name);
        hasher.update("\x00");
        inline for (binding.required_capabilities) |cap| {
            hasher.update(@tagName(cap));
            hasher.update(",");
        }
        hasher.update("\x00");
        inline for (binding.exports) |exp| {
            hasher.update(exp.name);
            hasher.update(":");
            hasher.update(@tagName(exp.effect));
            hasher.update(":");
            hasher.update(@tagName(exp.returns));
            hasher.update(":");
            hasher.update(@tagName(exp.failure_severity));
            hasher.update(":");
            hasher.update(if (exp.traceable) "trace" else "no-trace");
            hasher.update("\x00");
        }
        hasher.update("\n");
    }
    var digest: [32]u8 = undefined;
    hasher.final(&digest);
    return std.fmt.bytesToHex(digest, .lower);
}

fn parseExport(allocator: std.mem.Allocator, value: std.json.Value) ManifestError!Export {
    if (value != .object) return error.InvalidExport;
    const obj = value.object;

    const name = try stringField(obj, "name");
    if (name.len == 0) return error.InvalidExport;

    const effect_raw = optionalStringField(obj, "effect") orelse "read";
    const returns_raw = optionalStringField(obj, "returns") orelse "unknown";
    const failure_raw = optionalStringField(obj, "failureSeverity") orelse optionalStringField(obj, "failure_severity") orelse "none";
    const name_owned = try allocator.dupe(u8, name);
    errdefer allocator.free(name_owned);

    var exp = Export{
        .name = name_owned,
        .effect = std.meta.stringToEnum(mb.EffectClass, effect_raw) orelse return error.InvalidEffect,
        .returns = std.meta.stringToEnum(mb.ReturnKind, returns_raw) orelse return error.InvalidReturnKind,
        .failure_severity = std.meta.stringToEnum(mb.FailureSeverity, failure_raw) orelse return error.InvalidFailureSeverity,
        .traceable = boolField(obj, "traceable") orelse true,
        .return_labels = .{},
        .contract_extractions = .empty,
    };
    errdefer exp.deinit(allocator);

    if (obj.get("returnLabels") orelse obj.get("return_labels")) |labels_value| {
        exp.return_labels = try parseLabels(labels_value);
    }
    if (obj.get("params") orelse obj.get("paramTypes") orelse obj.get("param_types")) |params_value| {
        try validateReturnKindArray(params_value);
    }
    if (obj.get("contractExtractions") orelse obj.get("contract_extractions")) |extractions_value| {
        try parseContractExtractions(allocator, extractions_value, &exp.contract_extractions);
    }
    if (obj.get("contractFlags") orelse obj.get("contract_flags")) |flags_value| {
        try validateContractFlags(flags_value);
    }
    if (obj.get("laws")) |laws_value| {
        try validateLaws(laws_value);
    }

    return exp;
}

fn parseSchemaVersion(value_opt: ?std.json.Value) ManifestError!u32 {
    const value = value_opt orelse return error.InvalidManifest;
    if (value != .integer or value.integer < 0 or value.integer > std.math.maxInt(u32)) return error.InvalidManifest;
    return @intCast(value.integer);
}

fn stringField(obj: anytype, key: []const u8) ManifestError![]const u8 {
    const value = obj.get(key) orelse return error.InvalidManifest;
    if (value != .string) return error.InvalidManifest;
    return value.string;
}

fn optionalStringField(obj: anytype, key: []const u8) ?[]const u8 {
    const value = obj.get(key) orelse return null;
    if (value != .string) return null;
    return value.string;
}

fn boolField(obj: anytype, key: []const u8) ?bool {
    const value = obj.get(key) orelse return null;
    if (value != .bool) return null;
    return value.bool;
}

fn validSpecifier(specifier: []const u8) bool {
    return std.mem.startsWith(u8, specifier, "zigttp:") or
        std.mem.startsWith(u8, specifier, "zigttp-ext:");
}

fn parseBackend(raw: []const u8) ?Backend {
    if (std.mem.eql(u8, raw, "native-zig") or std.mem.eql(u8, raw, "native_zig")) return .native_zig;
    if (std.mem.eql(u8, raw, "wasm-component") or std.mem.eql(u8, raw, "wasm_component")) return .wasm_component;
    return null;
}

fn parseStateModel(raw: []const u8) ?StateModel {
    return std.meta.stringToEnum(StateModel, raw);
}

fn parseLabels(value: std.json.Value) ManifestError!mb.LabelSet {
    if (value != .array) return error.InvalidDataLabel;
    var labels: mb.LabelSet = .{};
    for (value.array.items) |item| {
        if (item != .string) return error.InvalidDataLabel;
        const label = std.meta.stringToEnum(mb.DataLabel, item.string) orelse return error.InvalidDataLabel;
        labels = mb.LabelSet.merge(labels, mb.LabelSet.fromLabel(label));
    }
    return labels;
}

fn validateReturnKindArray(value: std.json.Value) ManifestError!void {
    if (value != .array) return error.InvalidReturnKind;
    for (value.array.items) |item| {
        if (item != .string) return error.InvalidReturnKind;
        _ = std.meta.stringToEnum(mb.ReturnKind, item.string) orelse return error.InvalidReturnKind;
    }
}

fn parseContractExtractions(
    allocator: std.mem.Allocator,
    value: std.json.Value,
    out: *std.ArrayList(ContractExtractionRule),
) ManifestError!void {
    if (value != .array) return error.InvalidContractCategory;
    for (value.array.items) |item| {
        if (item != .object) return error.InvalidContractCategory;
        const obj = item.object;

        const category_raw = try stringField(obj, "category");
        const category = std.meta.stringToEnum(mb.ContractCategory, category_raw) orelse return error.InvalidContractCategory;

        var rule = ContractExtractionRule{ .category = category };
        errdefer rule.deinit(allocator);

        if (obj.get("argPosition") orelse obj.get("arg_position")) |arg| {
            if (arg != .integer or arg.integer < 0 or arg.integer > std.math.maxInt(u8)) return error.InvalidContractCategory;
            rule.arg_position = @intCast(arg.integer);
        }
        if (optionalStringField(obj, "transform")) |transform_raw| {
            rule.transform = std.meta.stringToEnum(mb.ContractTransform, transform_raw) orelse return error.InvalidContractTransform;
        }
        if (obj.get("flagOnly") orelse obj.get("flag_only")) |flag| {
            if (flag != .bool) return error.InvalidContractCategory;
            rule.flag_only = flag.bool;
        }

        if (category == .extension_specific) {
            const tag = optionalStringField(obj, "extensionCategory") orelse optionalStringField(obj, "extension_category") orelse return error.InvalidContractCategory;
            if (tag.len == 0) return error.InvalidContractCategory;
            if (!validExtensionCategory(tag)) return error.InvalidContractCategory;
            rule.extension_category = try allocator.dupe(u8, tag);
        } else {
            if (obj.get("extensionCategory") != null or obj.get("extension_category") != null) {
                return error.InvalidContractCategory;
            }
        }

        try out.append(allocator, rule);
    }
}

fn validExtensionCategory(tag: []const u8) bool {
    if (tag.len > 64) return false;
    for (tag) |c| {
        const is_lower = c >= 'a' and c <= 'z';
        const is_upper = c >= 'A' and c <= 'Z';
        const is_digit = c >= '0' and c <= '9';
        if (!(is_lower or is_upper or is_digit or c == '_' or c == '-')) return false;
    }
    return true;
}

fn validateContractFlags(value: std.json.Value) ManifestError!void {
    if (value != .array and value != .object) return error.InvalidContractFlag;
    if (value == .array) {
        for (value.array.items) |item| {
            if (item != .string) return error.InvalidContractFlag;
            try validateContractFlagName(item.string);
        }
        return;
    }

    var iter = value.object.iterator();
    while (iter.next()) |entry| {
        try validateContractFlagName(entry.key_ptr.*);
        if (entry.value_ptr.* != .bool) return error.InvalidContractFlag;
    }
}

fn validateContractFlagName(raw: []const u8) ManifestError!void {
    const valid =
        std.mem.eql(u8, raw, "sets_scope_used") or
        std.mem.eql(u8, raw, "sets_durable_used") or
        std.mem.eql(u8, raw, "sets_durable_timers") or
        std.mem.eql(u8, raw, "sets_bearer_auth") or
        std.mem.eql(u8, raw, "sets_jwt_auth") or
        std.mem.eql(u8, raw, "setsScopeUsed") or
        std.mem.eql(u8, raw, "setsDurableUsed") or
        std.mem.eql(u8, raw, "setsDurableTimers") or
        std.mem.eql(u8, raw, "setsBearerAuth") or
        std.mem.eql(u8, raw, "setsJwtAuth");
    if (!valid) return error.InvalidContractFlag;
}

fn validateLaws(value: std.json.Value) ManifestError!void {
    if (value != .array) return error.InvalidLaw;
    for (value.array.items) |item| {
        if (item == .string) {
            _ = std.meta.stringToEnum(mb.LawKind, item.string) orelse return error.InvalidLaw;
            continue;
        }
        if (item != .object) return error.InvalidLaw;
        const obj = item.object;
        if (obj.get("inverseOf") orelse obj.get("inverse_of")) |inverse| {
            if (inverse != .string or inverse.string.len == 0) return error.InvalidLaw;
            continue;
        }

        const absorbing = obj.get("absorbing") orelse return error.InvalidLaw;
        if (absorbing != .object) return error.InvalidLaw;
        const absorbing_obj = absorbing.object;
        if (absorbing_obj.get("argPosition") orelse absorbing_obj.get("arg_position")) |arg| {
            if (arg != .integer or arg.integer < 0 or arg.integer > std.math.maxInt(u8)) return error.InvalidLaw;
        }
        const shape = try stringField(absorbing_obj, "argumentShape");
        const residue = try stringField(absorbing_obj, "residue");
        _ = std.meta.stringToEnum(mb.AbsorbingPattern.ArgumentShape, shape) orelse return error.InvalidLaw;
        _ = std.meta.stringToEnum(mb.AbsorbingPattern.Residue, residue) orelse return error.InvalidLaw;
    }
}

fn containsCapability(items: []const CapabilityDeclaration, needle: mb.ModuleCapability) bool {
    for (items) |item| {
        if (item.effective == needle) return true;
    }
    return false;
}

fn parseCapabilityDeclaration(
    allocator: std.mem.Allocator,
    value: std.json.Value,
) ManifestError!CapabilityDeclaration {
    switch (value) {
        .string => |raw| {
            const cap = std.meta.stringToEnum(mb.ModuleCapability, raw) orelse return error.InvalidCapability;
            return .{ .effective = cap };
        },
        .object => |obj| {
            const partner_raw = stringField(obj, "name") catch return error.InvalidCapability;
            if (!validPartnerCapabilityName(partner_raw)) return error.InvalidCapability;
            // The closed enum tag (the structurally-enforced capability) lives
            // under `inherits`. We accept `inheritsCapability` as a more
            // explicit alias for partners who already use camelCase.
            const inherits_raw = (optionalStringField(obj, "inherits") orelse optionalStringField(obj, "inheritsCapability")) orelse return error.InvalidCapability;
            const inherits = std.meta.stringToEnum(mb.ModuleCapability, inherits_raw) orelse return error.InvalidCapability;
            // Reject partners that re-declare a name already shipped as a
            // built-in enum tag; the bare-string form is the right surface
            // for those.
            if (std.meta.stringToEnum(mb.ModuleCapability, partner_raw) != null) return error.InvalidCapability;
            const owned = try allocator.dupe(u8, partner_raw);
            return .{ .effective = inherits, .partner_name = owned };
        },
        else => return error.InvalidCapability,
    }
}

/// Reserved built-in section names. Partners cannot pick these for their
/// `contractSection`; the writer would emit duplicate top-level keys
/// otherwise. The list is intentionally inclusive of every key the JSON
/// writer emits at the top level, not just the existing module bindings,
/// so partners cannot collide with `egress`, `properties`, etc.
const reserved_contract_sections = [_][]const u8{
    "handler",             "version",       "routes",
    "modules",             "sandbox",       "functions",
    "env",                 "egress",        "serviceCalls",
    "cache",               "sql",           "durable",
    "scope",               "api",           "verification",
    "websocket",           "aot",           "faultCoverage",
    "rateLimiting",        "properties",    "behaviors",
    "behaviorsExhaustive", "declaredSpecs", "specDiagnostics",
    "extensions",
};

fn reservedContractSectionName(raw: []const u8) bool {
    for (reserved_contract_sections) |reserved| {
        if (std.mem.eql(u8, reserved, raw)) return true;
    }
    return false;
}

fn validContractSectionName(raw: []const u8) bool {
    if (raw.len == 0 or raw.len > 64) return false;
    for (raw, 0..) |c, i| {
        const is_lower = c >= 'a' and c <= 'z';
        const is_upper = c >= 'A' and c <= 'Z';
        const is_digit = c >= '0' and c <= '9';
        if (i == 0 and !(is_lower or is_upper)) return false;
        if (!(is_lower or is_upper or is_digit or c == '_' or c == '-')) return false;
    }
    return true;
}

/// Partner capability names use the same syntactic rules as
/// `validExtensionCategory`: 1..64 chars from `[A-Za-z0-9_-]`. This keeps
/// the namespace audit-friendly (greppable, line-stable) and matches the
/// shape partners already use elsewhere in the manifest.
fn validPartnerCapabilityName(name: []const u8) bool {
    if (name.len == 0 or name.len > 64) return false;
    for (name) |c| {
        const is_lower = c >= 'a' and c <= 'z';
        const is_upper = c >= 'A' and c <= 'Z';
        const is_digit = c >= '0' and c <= '9';
        if (!(is_lower or is_upper or is_digit or c == '_' or c == '-')) return false;
    }
    return true;
}

fn containsExport(items: []const Export, needle: []const u8) bool {
    for (items) |item| {
        if (std.mem.eql(u8, item.name, needle)) return true;
    }
    return false;
}

test "parse manifest accepts demo extension metadata" {
    const json =
        \\{
        \\  "schemaVersion": 1,
        \\  "specifier": "zigttp-ext:math",
        \\  "backend": "native-zig",
        \\  "stateModel": "none",
        \\  "requiredCapabilities": ["clock"],
        \\  "exports": [
        \\    { "name": "double", "params": ["number"], "returns": "number", "effect": "read" }
        \\  ]
        \\}
    ;
    var manifest = try parse(std.testing.allocator, json);
    defer manifest.deinit(std.testing.allocator);

    try std.testing.expectEqualStrings("zigttp-ext:math", manifest.specifier);
    try std.testing.expectEqual(Backend.native_zig, manifest.backend.?);
    try std.testing.expectEqual(StateModel.none, manifest.state_model.?);
    try std.testing.expectEqual(@as(usize, 1), manifest.required_capabilities.items.len);
    try std.testing.expectEqual(mb.ModuleCapability.clock, manifest.required_capabilities.items[0].effective);
    try std.testing.expect(manifest.required_capabilities.items[0].partner_name == null);
    try std.testing.expectEqualStrings("double", manifest.exports.items[0].name);
}

test "parse manifest accepts partner-declared capability that inherits an enum tag" {
    const json =
        \\{
        \\  "schemaVersion": 1,
        \\  "specifier": "zigttp-ext:llm",
        \\  "requiredCapabilities": [
        \\    { "name": "llm_egress", "inherits": "network" },
        \\    "clock"
        \\  ],
        \\  "exports": [{ "name": "complete" }]
        \\}
    ;
    var manifest = try parse(std.testing.allocator, json);
    defer manifest.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 2), manifest.required_capabilities.items.len);
    try std.testing.expectEqual(mb.ModuleCapability.network, manifest.required_capabilities.items[0].effective);
    try std.testing.expectEqualStrings("llm_egress", manifest.required_capabilities.items[0].partner_name orelse return error.TestExpectedName);
    try std.testing.expectEqual(mb.ModuleCapability.clock, manifest.required_capabilities.items[1].effective);
    try std.testing.expect(manifest.required_capabilities.items[1].partner_name == null);
}

test "parse manifest rejects partner capability missing inherits" {
    const json =
        \\{
        \\  "schemaVersion": 1,
        \\  "specifier": "zigttp-ext:llm",
        \\  "requiredCapabilities": [{ "name": "llm_egress" }],
        \\  "exports": [{ "name": "complete" }]
        \\}
    ;
    try std.testing.expectError(error.InvalidCapability, parse(std.testing.allocator, json));
}

test "parse manifest rejects partner capability whose name collides with a built-in" {
    const json =
        \\{
        \\  "schemaVersion": 1,
        \\  "specifier": "zigttp-ext:llm",
        \\  "requiredCapabilities": [{ "name": "network", "inherits": "network" }],
        \\  "exports": [{ "name": "complete" }]
        \\}
    ;
    try std.testing.expectError(error.InvalidCapability, parse(std.testing.allocator, json));
}

test "parse manifest captures contractSection when set" {
    const json =
        \\{
        \\  "schemaVersion": 1,
        \\  "specifier": "zigttp-ext:stripe",
        \\  "contractSection": "stripe",
        \\  "exports": [{ "name": "chargeCard" }]
        \\}
    ;
    var manifest = try parse(std.testing.allocator, json);
    defer manifest.deinit(std.testing.allocator);

    try std.testing.expectEqualStrings("stripe", manifest.contract_section orelse return error.TestExpectedSection);
}

test "parse manifest rejects contractSection that collides with a built-in" {
    const reserved = [_][]const u8{ "cache", "functions", "modules", "websocket", "aot" };
    for (reserved) |section| {
        const json = try std.fmt.allocPrint(
            std.testing.allocator,
            \\{{
            \\  "schemaVersion": 1,
            \\  "specifier": "zigttp-ext:bad",
            \\  "contractSection": "{s}",
            \\  "exports": [{{ "name": "x" }}]
            \\}}
        ,
            .{section},
        );
        defer std.testing.allocator.free(json);
        try std.testing.expectError(error.InvalidContractFlag, parse(std.testing.allocator, json));
    }
}

test "parse manifest rejects duplicate exports within module" {
    const json =
        \\{
        \\  "schemaVersion": 1,
        \\  "specifier": "zigttp-ext:dupe",
        \\  "exports": [
        \\    { "name": "same" },
        \\    { "name": "same" }
        \\  ]
        \\}
    ;
    try std.testing.expectError(error.DuplicateExport, parse(std.testing.allocator, json));
}
