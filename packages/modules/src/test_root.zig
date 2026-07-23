const std = @import("std");
const modules = @import("root.zig");
const test_shim = @import("zttp-sdk-test-shim");

test {
    std.testing.refAllDecls(modules);
    std.testing.refAllDecls(test_shim);
}
