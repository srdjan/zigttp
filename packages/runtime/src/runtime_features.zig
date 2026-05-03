const options = @import("runtime_feature_options");

pub const studio = if (options.enable_studio)
    @import("studio.zig")
else
    @import("studio_stub.zig");

pub const live_reload = if (options.enable_live_reload)
    @import("live_reload.zig")
else
    @import("live_reload_stub.zig");
