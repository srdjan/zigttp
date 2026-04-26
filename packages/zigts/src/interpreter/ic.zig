//! Polymorphic Inline Cache (PIC) for property access optimization, plus
//! the tunables (ZTS_PIC_MEGA_RECOVERY_WINDOW, ZTS_PIC_DISABLE_RECOVERY) that
//! govern megamorphic recovery behavior.
//!
//! PIC layout (PolymorphicInlineCache, PICEntry, PIC_ENTRIES) is consumed
//! directly by the JIT (jit/baseline.zig), so changes here must keep the
//! struct layout stable.

const std = @import("std");
const object = @import("../object.zig");
const type_feedback = @import("../type_feedback.zig");

var pic_mega_recovery_window_cache: ?u16 = null;
var pic_recovery_disabled_cache: ?bool = null;

/// Number of consecutive monomorphic observations after a PIC goes megamorphic
/// before it is allowed to reset and re-specialize on the dominant shape.
/// Overridable via ZTS_PIC_MEGA_RECOVERY_WINDOW.
pub fn getPicMegaRecoveryWindow() u16 {
    if (pic_mega_recovery_window_cache) |cached| return cached;
    const default_window: u16 = 32;
    if (std.c.getenv("ZTS_PIC_MEGA_RECOVERY_WINDOW")) |raw_ptr| {
        const raw = std.mem.sliceTo(raw_ptr, 0);
        const parsed = std.fmt.parseUnsigned(u16, raw, 10) catch default_window;
        pic_mega_recovery_window_cache = if (parsed == 0) default_window else parsed;
    } else {
        pic_mega_recovery_window_cache = default_window;
    }
    return pic_mega_recovery_window_cache.?;
}

/// When ZTS_PIC_DISABLE_RECOVERY is set, PICs stay megamorphic permanently
/// (legacy behavior) instead of attempting recovery after a stable shape window.
pub fn isPicRecoveryDisabled() bool {
    if (pic_recovery_disabled_cache) |cached| return cached;
    const disabled = std.c.getenv("ZTS_PIC_DISABLE_RECOVERY") != null;
    pic_recovery_disabled_cache = disabled;
    return disabled;
}

/// Single entry in a polymorphic inline cache
/// Stores hidden class index and slot offset for one observed shape
pub const PICEntry = struct {
    hidden_class_idx: object.HiddenClassIndex,
    slot_offset: u16,
};

/// Number of entries in a polymorphic inline cache
/// 8 entries provides good coverage for common polymorphic patterns
/// while keeping memory overhead reasonable per IC site
pub const PIC_ENTRIES = 8;

/// When true, cap the number of distinct shapes the PIC caches at
/// `type_feedback.MAX_POLYMORPHIC_SHAPES` so that PIC and the upstream
/// type-feedback layer agree on what counts as megamorphic. The underlying
/// `entries` array stays at PIC_ENTRIES to preserve memory layout and the
/// JIT's inline PIC_CHECK_COUNT assumptions. Flip to false for reversion.
pub const pic_entries_tracks_feedback = true;

/// Effective cap on distinct shapes the PIC will cache before going megamorphic.
pub const effective_pic_cap: u8 = if (pic_entries_tracks_feedback)
    type_feedback.MAX_POLYMORPHIC_SHAPES
else
    PIC_ENTRIES;

/// Polymorphic Inline Cache for property access optimization
/// Caches up to `effective_pic_cap` (hidden_class, slot_offset) pairs per access site
/// Falls back to megamorphic mode when more shapes are observed
pub const PolymorphicInlineCache = struct {
    /// Cached entries (only first `count` are valid)
    /// Initialize all entries with invalid hidden class to prevent JIT false matches
    /// on uninitialized memory (JIT checks first PIC_CHECK_COUNT entries inline)
    entries: [PIC_ENTRIES]PICEntry = [_]PICEntry{.{ .hidden_class_idx = .none, .slot_offset = 0 }} ** PIC_ENTRIES,
    /// Number of valid entries (0..effective_pic_cap)
    count: u8 = 0,
    /// Index of most recently hit entry
    last_hit: u8 = 0,
    /// Megamorphic flag: true when > effective_pic_cap shapes observed
    /// When megamorphic, skip caching unless a recovery window elapses with a single shape
    megamorphic: bool = false,
    /// Shape being watched for megamorphic recovery
    recovery_candidate: object.HiddenClassIndex = .none,
    /// Consecutive observations of `recovery_candidate` while megamorphic
    consec_same_shape: u16 = 0,
    /// Latched when update() performed a megamorphic->monomorphic recovery
    /// Callers read and reset it to increment PerfStats.mega_recoveries.
    just_recovered: bool = false,

    /// Lookup hidden class in cache, return slot offset if found
    pub inline fn lookup(self: *PolymorphicInlineCache, hidden_class_idx: object.HiddenClassIndex) ?u16 {
        if (self.count == 0) return null;
        if (self.last_hit < self.count) {
            const entry = self.entries[self.last_hit];
            if (entry.hidden_class_idx == hidden_class_idx) {
                return entry.slot_offset;
            }
        }
        for (self.entries[0..self.count], 0..) |entry, idx| {
            if (entry.hidden_class_idx == hidden_class_idx) {
                self.last_hit = @intCast(idx);
                return entry.slot_offset;
            }
        }
        return null;
    }

    /// Update cache with new (hidden_class_idx, slot_offset) pair
    /// Returns true if the shape is now cached, false if still rejected as megamorphic.
    /// If recovery triggers, `just_recovered` is set so callers can observe it.
    pub inline fn update(self: *PolymorphicInlineCache, hidden_class_idx: object.HiddenClassIndex, slot_offset: u16) bool {
        if (self.megamorphic) {
            if (isPicRecoveryDisabled()) return false;
            if (self.recovery_candidate == hidden_class_idx and hidden_class_idx != .none) {
                self.consec_same_shape +|= 1;
                if (self.consec_same_shape >= getPicMegaRecoveryWindow()) {
                    self.entries[0] = .{ .hidden_class_idx = hidden_class_idx, .slot_offset = slot_offset };
                    self.count = 1;
                    self.last_hit = 0;
                    self.megamorphic = false;
                    self.consec_same_shape = 0;
                    self.recovery_candidate = .none;
                    self.just_recovered = true;
                    return true;
                }
            } else {
                self.recovery_candidate = hidden_class_idx;
                self.consec_same_shape = 1;
            }
            return false;
        }

        for (self.entries[0..self.count], 0..) |*entry, idx| {
            if (entry.hidden_class_idx == hidden_class_idx) {
                entry.slot_offset = slot_offset;
                self.last_hit = @intCast(idx);
                return true;
            }
        }

        if (self.count < effective_pic_cap) {
            self.entries[self.count] = .{
                .hidden_class_idx = hidden_class_idx,
                .slot_offset = slot_offset,
            };
            self.last_hit = self.count;
            self.count += 1;
            return true;
        }

        self.megamorphic = true;
        self.recovery_candidate = .none;
        self.consec_same_shape = 0;
        return false;
    }

    /// Reset cache to initial state (for debugging/testing)
    pub fn reset(self: *PolymorphicInlineCache) void {
        self.count = 0;
        self.last_hit = 0;
        self.megamorphic = false;
        self.recovery_candidate = .none;
        self.consec_same_shape = 0;
        self.just_recovered = false;
    }
};

/// Maximum number of inline cache slots per compilation unit.
/// Each get_field_ic/put_field_ic instruction references a cache index.
/// Must match codegen.IC_CACHE_SIZE. IC indices are globally unique across
/// all functions in a file, so this must be large enough for the total
/// property access count across all functions.
pub const IC_CACHE_SIZE = 512;
