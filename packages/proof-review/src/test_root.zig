const std = @import("std");

// Force-link each proof-review file so its `test` blocks run. Skipping the
// `refAllDecls`/recursive variant on purpose — some files in this
// subtree reference platform APIs that are stable to call at runtime but trip
// the test compiler when every declaration is touched at compile time. Listing
// each file by hand keeps the test surface explicit.
comptime {
    _ = @import("io_util.zig");
    _ = @import("json_util.zig");
    _ = @import("printer.zig");
    _ = @import("review.zig");
    _ = @import("spec_diagnostic.zig");
    _ = @import("state.zig");
    _ = @import("types.zig");
}
