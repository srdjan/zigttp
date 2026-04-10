const std = @import("std");

pub fn build(b: *std.Build) void {
    // Accept standard options (required by dependency resolution)
    _ = b.standardTargetOptions(.{});
    _ = b.standardOptimizeOption(.{});
    // Runtime sources are consumed directly by the root build.zig
    // via runtime_dep.path("src/..."). No modules are exported
    // because the root orchestrates executable and test construction.
}
