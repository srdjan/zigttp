const std = @import("std");

// Force-link each deploy file so its `test` blocks run. Skipping the
// `refAllDecls`/recursive variant on purpose — some files in this
// subtree (auth.zig, control_plane.zig, autodetect.zig) reference
// platform APIs that are stable to call at runtime but trip the test
// compiler when every declaration is touched at compile time. Listing
// each file by hand lets us include only the ones whose tests
// compile on every supported target.
comptime {
    _ = @import("printer.zig");
    _ = @import("review.zig");
    _ = @import("northflank_adapter.zig");
    _ = @import("http.zig");
    _ = @import("state.zig");
    _ = @import("oci/layer.zig");
    _ = @import("oci/image.zig");
    _ = @import("oci/registry.zig");
    _ = @import("oci/tar.zig");
}
