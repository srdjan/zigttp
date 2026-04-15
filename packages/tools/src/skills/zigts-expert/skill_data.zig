//! Embedded zigts-expert skill content. This file lives inside the skill
//! directory so `@embedFile` reaches siblings directly - a module rooted
//! higher up would escape Zig 0.16's module-path sandbox when trying to
//! embed these markdown files.
//!
//! Consumed by `packages/pi/src/expert_persona.zig` via the `zigts_expert_skill`
//! named module exposed from `packages/tools/build.zig`.

pub const skill_md: []const u8 = @embedFile("SKILL.md");
pub const virtual_modules_md: []const u8 = @embedFile("references/virtual-modules.md");
pub const testing_replay_md: []const u8 = @embedFile("references/testing-replay.md");
pub const jsx_patterns_md: []const u8 = @embedFile("references/jsx-patterns.md");
