const std = @import("std");

pub const Provider = enum {
    northflank,

    pub fn fromString(raw: []const u8) ?Provider {
        if (std.mem.eql(u8, raw, "northflank")) return .northflank;
        return null;
    }

    pub fn toString(self: Provider) []const u8 {
        return @tagName(self);
    }
};

pub const Arch = enum {
    amd64,
    arm64,

    pub fn fromString(raw: []const u8) ?Arch {
        if (std.mem.eql(u8, raw, "amd64")) return .amd64;
        if (std.mem.eql(u8, raw, "arm64")) return .arm64;
        return null;
    }

    pub fn toString(self: Arch) []const u8 {
        return @tagName(self);
    }

    pub fn targetTriple(self: Arch) []const u8 {
        return switch (self) {
            .amd64 => "x86_64-linux-musl",
            .arm64 => "aarch64-linux-musl",
        };
    }

    pub fn ociArchitecture(self: Arch) []const u8 {
        return switch (self) {
            .amd64 => "amd64",
            .arm64 => "arm64",
        };
    }
};

pub const EnvVar = struct {
    key: []const u8,
    value: []const u8,
};

pub const ProviderAction = enum {
    create,
    update,
    replace_requires_confirm,

    pub fn toString(self: ProviderAction) []const u8 {
        return @tagName(self);
    }
};

pub const DeployResult = struct {
    provider: Provider,
    name: []const u8,
    service_id: []const u8,
    deployment_id: ?[]const u8,
    url: ?[]const u8,
    status: []const u8,
    image_digest_ref: []const u8,
};
