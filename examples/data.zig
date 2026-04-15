//! Embedded canonical zigts handler examples consumed by the pi package's
//! `expert_persona` bundle builder. Lives at examples/ level so the module
//! root's path subtree covers the example subdirectories; `@embedFile` can
//! reach them without escaping Zig 0.16's module-path sandbox.
//!
//! Exposed as the `zigts_expert_examples` named module from the root
//! `build.zig` and imported into the pi package's build.

pub const basic_handler: []const u8 = @embedFile("handler/handler.ts");
pub const routing_router: []const u8 = @embedFile("routing/router.ts");
pub const system_users: []const u8 = @embedFile("system/users.ts");
