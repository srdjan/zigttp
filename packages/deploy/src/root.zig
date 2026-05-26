//! zigttp-deploy — local self-contained binary deploy + hosted control-plane
//! orchestration. Depends only on zigts and zigts_cli (the precompile/build
//! pipeline that produces the deployable artifact). The runtime package
//! imports this module to wire `zigttp deploy` and the studio/proofs UI;
//! the runtime itself does not consume any of these files at request time.

pub const auth = @import("auth.zig");
pub const autodetect = @import("autodetect.zig");
pub const builder = @import("builder.zig");
pub const config = @import("config.zig");
pub const control_plane = @import("control_plane.zig");
pub const first_run = @import("first_run.zig");
pub const http = @import("http.zig");
pub const io_util = @import("io_util.zig");
pub const json_util = @import("json_util.zig");
pub const northflank_adapter = @import("northflank_adapter.zig");
pub const printer = @import("printer.zig");
pub const review = @import("review.zig");
pub const spec_diagnostic = @import("spec_diagnostic.zig");
pub const state = @import("state.zig");
pub const types = @import("types.zig");

pub const oci = struct {
    pub const config = @import("oci/config.zig");
    pub const image = @import("oci/image.zig");
    pub const layer = @import("oci/layer.zig");
    pub const manifest = @import("oci/manifest.zig");
    pub const registry = @import("oci/registry.zig");
    pub const tar = @import("oci/tar.zig");
};
