//! Curated compile-time model registry. Capability metadata describes what a
//! model can support; request policy describes what zigttp intentionally asks
//! for. Selection is exact and provider-aware.

const std = @import("std");

pub const Provider = enum {
    anthropic,
    openai,
};

pub const Capabilities = struct {
    context_window_tokens: u32,
    max_output_tokens: u32,
};

pub const RequestPolicy = struct {
    max_output_tokens: u32,
};

pub const Model = struct {
    provider: Provider,
    id: []const u8,
    display_name: []const u8,
    capabilities: Capabilities,
    request_policy: RequestPolicy,
    is_default: bool = false,
};

pub const registry = [_]Model{
    .{
        .provider = .anthropic,
        .id = "claude-opus-4-8",
        .display_name = "Claude Opus 4.8",
        .capabilities = .{ .context_window_tokens = 200_000, .max_output_tokens = 32_000 },
        .request_policy = .{ .max_output_tokens = 32_000 },
    },
    .{
        .provider = .anthropic,
        .id = "claude-sonnet-5",
        .display_name = "Claude Sonnet 5",
        .capabilities = .{ .context_window_tokens = 200_000, .max_output_tokens = 64_000 },
        .request_policy = .{ .max_output_tokens = 64_000 },
    },
    .{
        .provider = .anthropic,
        .id = "claude-sonnet-4-6",
        .display_name = "Claude Sonnet 4.6",
        .capabilities = .{ .context_window_tokens = 200_000, .max_output_tokens = 64_000 },
        .request_policy = .{ .max_output_tokens = 64_000 },
        .is_default = true,
    },
    .{
        .provider = .anthropic,
        .id = "claude-haiku-4-5-20251001",
        .display_name = "Claude Haiku 4.5",
        .capabilities = .{ .context_window_tokens = 200_000, .max_output_tokens = 8_192 },
        .request_policy = .{ .max_output_tokens = 8_192 },
    },
    .{
        .provider = .openai,
        .id = "gpt-4o-mini",
        .display_name = "GPT-4o mini",
        .capabilities = .{ .context_window_tokens = 128_000, .max_output_tokens = 16_384 },
        .request_policy = .{ .max_output_tokens = 8_192 },
        .is_default = true,
    },
};

comptime {
    for (registry, 0..) |model, i| {
        if (model.request_policy.max_output_tokens > model.capabilities.max_output_tokens) {
            @compileError("model request policy exceeds output capability");
        }
        for (registry[i + 1 ..]) |other| {
            if (std.mem.eql(u8, model.id, other.id)) {
                @compileError("duplicate model id in registry");
            }
        }
    }
}

pub fn findById(id: []const u8) ?*const Model {
    for (&registry) |*model| {
        if (std.mem.eql(u8, model.id, id)) return model;
    }
    return null;
}

pub const SelectionError = error{
    UnknownModel,
    ProviderMismatch,
};

pub fn resolveForProvider(provider: Provider, id: []const u8) SelectionError!*const Model {
    const model = findById(id) orelse return error.UnknownModel;
    if (model.provider != provider) return error.ProviderMismatch;
    return model;
}

pub fn defaultForProvider(provider: Provider) *const Model {
    var found: ?*const Model = null;
    var iterator = iterateProvider(provider);
    while (iterator.next()) |model| {
        if (!model.is_default) continue;
        std.debug.assert(found == null);
        found = model;
    }
    return found orelse unreachable;
}

pub const ProviderIterator = struct {
    provider: Provider,
    index: usize = 0,

    pub fn next(self: *ProviderIterator) ?*const Model {
        while (self.index < registry.len) {
            const index = self.index;
            self.index += 1;
            if (registry[index].provider == self.provider) return &registry[index];
        }
        return null;
    }
};

pub fn iterateProvider(provider: Provider) ProviderIterator {
    return .{ .provider = provider };
}

test "registry ids and defaults are unique and provider-exact" {
    inline for (std.meta.tags(Provider)) |provider| {
        var default_count: usize = 0;
        var iterator = iterateProvider(provider);
        while (iterator.next()) |model| {
            try std.testing.expectEqual(provider, model.provider);
            try std.testing.expect(model.request_policy.max_output_tokens <= model.capabilities.max_output_tokens);
            if (model.is_default) default_count += 1;
        }
        try std.testing.expectEqual(@as(usize, 1), default_count);
        try std.testing.expectEqual(provider, defaultForProvider(provider).provider);
    }
}

test "gpt-4o-mini capability and request policy remain distinct" {
    const model = try resolveForProvider(.openai, "gpt-4o-mini");
    try std.testing.expectEqual(@as(u32, 128_000), model.capabilities.context_window_tokens);
    try std.testing.expectEqual(@as(u32, 16_384), model.capabilities.max_output_tokens);
    try std.testing.expectEqual(@as(u32, 8_192), model.request_policy.max_output_tokens);
    try std.testing.expect(findById("gpt-4o-mini-search-preview") == null);
}

test "Anthropic request policies preserve curated model budgets" {
    const expected = [_]struct { id: []const u8, max_output_tokens: u32 }{
        .{ .id = "claude-opus-4-8", .max_output_tokens = 32_000 },
        .{ .id = "claude-sonnet-5", .max_output_tokens = 64_000 },
        .{ .id = "claude-sonnet-4-6", .max_output_tokens = 64_000 },
        .{ .id = "claude-haiku-4-5-20251001", .max_output_tokens = 8_192 },
    };
    for (expected) |entry| {
        const model = try resolveForProvider(.anthropic, entry.id);
        try std.testing.expectEqual(entry.max_output_tokens, model.request_policy.max_output_tokens);
    }
}

test "provider resolution distinguishes unknown and cross-provider ids" {
    try std.testing.expectError(error.UnknownModel, resolveForProvider(.openai, "gpt-4o-min"));
    try std.testing.expectError(error.ProviderMismatch, resolveForProvider(.anthropic, "gpt-4o-mini"));
}
