//! Session-scoped registry of partner virtual-module manifests.
//!
//! Built-in modules live in a comptime array (`builtin_modules.all`). Third-
//! party modules ship their proof metadata as JSON `zttp-module.json` files
//! and are registered into a `Registry` at compile-session start (typically
//! from `--module-manifest <path>` flags).
//!
//! The contract builder consults the registry after the built-in lookup
//! misses, so partner manifests participate in handler proof extraction
//! without monorepo edits.

const std = @import("std");
const module_manifest = @import("module_manifest.zig");

pub const Manifest = module_manifest.Manifest;
pub const Export = module_manifest.Export;
pub const ContractExtractionRule = module_manifest.ContractExtractionRule;

pub const RegistryError = error{
    DuplicateSpecifier,
    DuplicateContractSection,
} || std.mem.Allocator.Error;

pub const Registry = struct {
    allocator: std.mem.Allocator,
    manifests: std.ArrayList(Manifest),

    pub fn init(allocator: std.mem.Allocator) Registry {
        return .{
            .allocator = allocator,
            .manifests = .empty,
        };
    }

    pub fn deinit(self: *Registry) void {
        for (self.manifests.items) |*manifest| manifest.deinit(self.allocator);
        self.manifests.deinit(self.allocator);
    }

    /// Take ownership of `manifest`. On `DuplicateSpecifier`, the caller
    /// retains ownership of the rejected manifest and must deinit it.
    pub fn register(self: *Registry, manifest: Manifest) RegistryError!void {
        for (self.manifests.items) |existing| {
            if (std.mem.eql(u8, existing.specifier, manifest.specifier)) {
                return error.DuplicateSpecifier;
            }
            if (manifest.contract_section) |incoming| {
                if (existing.contract_section) |prior| {
                    if (std.mem.eql(u8, incoming, prior)) {
                        return error.DuplicateContractSection;
                    }
                }
            }
        }
        try self.manifests.append(self.allocator, manifest);
    }

    pub fn fromSpecifier(self: *const Registry, specifier: []const u8) ?*const Manifest {
        for (self.manifests.items) |*manifest| {
            if (std.mem.eql(u8, manifest.specifier, specifier)) return manifest;
        }
        return null;
    }

    pub fn findExport(
        self: *const Registry,
        specifier: []const u8,
        name: []const u8,
    ) ?*const Export {
        const manifest = self.fromSpecifier(specifier) orelse return null;
        for (manifest.exports.items) |*exp| {
            if (std.mem.eql(u8, exp.name, name)) return exp;
        }
        return null;
    }

    pub fn count(self: *const Registry) usize {
        return self.manifests.items.len;
    }
};

test "registry register and lookup round-trip" {
    const allocator = std.testing.allocator;

    var registry = Registry.init(allocator);
    defer registry.deinit();

    const json =
        \\{
        \\  "schemaVersion": 1,
        \\  "specifier": "zttp-ext:stripe",
        \\  "backend": "native-zig",
        \\  "requiredCapabilities": ["network"],
        \\  "exports": [
        \\    {
        \\      "name": "chargeCard",
        \\      "effect": "write",
        \\      "returns": "result",
        \\      "contractExtractions": [
        \\        { "category": "extension_specific", "extensionCategory": "payment_gateway", "argPosition": 0 }
        \\      ]
        \\    }
        \\  ]
        \\}
    ;
    var manifest = try module_manifest.parse(allocator, json);
    errdefer manifest.deinit(allocator);

    try registry.register(manifest);

    try std.testing.expectEqual(@as(usize, 1), registry.count());

    const looked_up = registry.fromSpecifier("zttp-ext:stripe") orelse return error.TestExpectedManifest;
    try std.testing.expectEqualStrings("zttp-ext:stripe", looked_up.specifier);

    const exp = registry.findExport("zttp-ext:stripe", "chargeCard") orelse return error.TestExpectedExport;
    try std.testing.expectEqualStrings("chargeCard", exp.name);
    try std.testing.expectEqual(@as(usize, 1), exp.contract_extractions.items.len);

    const rule = exp.contract_extractions.items[0];
    try std.testing.expectEqual(@as(u8, 0), rule.arg_position);
    try std.testing.expect(rule.category == .extension_specific);
    try std.testing.expectEqualStrings("payment_gateway", rule.extension_category orelse return error.TestExpectedTag);
}

test "registry rejects duplicate contract sections" {
    const allocator = std.testing.allocator;

    var registry = Registry.init(allocator);
    defer registry.deinit();

    const json_a =
        \\{
        \\  "schemaVersion": 1,
        \\  "specifier": "zttp-ext:a",
        \\  "contractSection": "shared",
        \\  "exports": [{ "name": "x" }]
        \\}
    ;
    const json_b =
        \\{
        \\  "schemaVersion": 1,
        \\  "specifier": "zttp-ext:b",
        \\  "contractSection": "shared",
        \\  "exports": [{ "name": "x" }]
        \\}
    ;
    var first = try module_manifest.parse(allocator, json_a);
    errdefer first.deinit(allocator);
    try registry.register(first);

    var second = try module_manifest.parse(allocator, json_b);
    defer second.deinit(allocator);
    try std.testing.expectError(error.DuplicateContractSection, registry.register(second));
}

test "registry rejects duplicate specifiers" {
    const allocator = std.testing.allocator;

    var registry = Registry.init(allocator);
    defer registry.deinit();

    const json =
        \\{
        \\  "schemaVersion": 1,
        \\  "specifier": "zttp-ext:dup",
        \\  "exports": [{ "name": "a" }]
        \\}
    ;
    var first = try module_manifest.parse(allocator, json);
    errdefer first.deinit(allocator);
    try registry.register(first);

    var second = try module_manifest.parse(allocator, json);
    defer second.deinit(allocator);
    try std.testing.expectError(error.DuplicateSpecifier, registry.register(second));
}

test "registry fromSpecifier returns null on miss" {
    const allocator = std.testing.allocator;

    var registry = Registry.init(allocator);
    defer registry.deinit();

    try std.testing.expect(registry.fromSpecifier("zttp-ext:nope") == null);
    try std.testing.expect(registry.findExport("zttp-ext:nope", "x") == null);
}
