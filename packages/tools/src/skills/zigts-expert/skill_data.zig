//! Embedded zigts-expert catalog. Skill prose plus vendored canonical
//! handler examples, bundled as a single named module so the persona
//! builder has one embedding seam to consume.
//!
//! The `examples/` subtree is a frozen snapshot mirrored from the repo-root
//! `/examples/` directory. Bundling (vs. reaching through the module-path
//! sandbox) keeps the persona reproducible: the model's few-shot examples
//! do not silently drift when someone edits an in-flight repo example.
//!
//! Consumed by `packages/pi/src/expert_persona.zig` via the `zigts_expert_skill`
//! named module exposed from `packages/tools/build.zig`.

pub const skill_md: []const u8 = @embedFile("SKILL.md");
pub const virtual_modules_md: []const u8 = @embedFile("references/virtual-modules.md");
pub const testing_replay_md: []const u8 = @embedFile("references/testing-replay.md");
pub const jsx_patterns_md: []const u8 = @embedFile("references/jsx-patterns.md");

pub const basic_handler: []const u8 = @embedFile("examples/handler/handler.ts");
pub const routing_router: []const u8 = @embedFile("examples/routing/router.ts");
pub const system_users: []const u8 = @embedFile("examples/system/users.ts");
