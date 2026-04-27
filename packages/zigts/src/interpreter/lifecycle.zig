//! Interpreter lifecycle helpers: construction, profiling-counter reset,
//! perf-stats snapshot, deopt accounting, and PIC update bookkeeping.

const context = @import("../context.zig");
const object = @import("../object.zig");
const interpreter = @import("../interpreter.zig");
const Interpreter = interpreter.Interpreter;
const PerfStats = interpreter.PerfStats;
const PolymorphicInlineCache = interpreter.PolymorphicInlineCache;
const IC_CACHE_SIZE = interpreter.IC_CACHE_SIZE;
const perf = @import("perf.zig");

const empty_code: [0]u8 = .{};

pub fn init(ctx: *context.Context) Interpreter {
    return .{
        .ctx = ctx,
        .pc = @ptrCast(&empty_code),
        .code_end = @ptrCast(&empty_code),
        .constants = &.{},
        .current_func = null,
        .current_closure = null,
        .open_upvalues = null,
        .state_stack = undefined,
        .state_depth = 0,
        .pic_cache = [_]PolymorphicInlineCache{.{}} ** IC_CACHE_SIZE,
    };
}

pub fn resetProfilingCounters(self: *Interpreter) void {
    self.backedge_count = 0;
    self.pic_hits = 0;
    self.pic_misses = 0;
    self.deopt_count = 0;
    self.mega_recoveries = 0;
    self.tier_promotions = [_]u32{0} ** perf.tier_count;
    self.promotion_attempted = 0;
    self.promotion_succeeded = 0;
    self.promotion_rejected_deopt_storm = 0;
    self.opcode_histogram = [_]u32{0} ** 256;
}

/// Update a PIC and observe whether it performed a megamorphic recovery,
/// bumping PerfStats.mega_recoveries when it does.
pub inline fn updatePic(
    self: *Interpreter,
    pic: *PolymorphicInlineCache,
    hidden_class_idx: object.HiddenClassIndex,
    slot_offset: u16,
) void {
    _ = pic.update(hidden_class_idx, slot_offset);
    if (pic.just_recovered) {
        pic.just_recovered = false;
        self.mega_recoveries +%= 1;
    }
}

pub fn snapshotPerfStats(self: *const Interpreter) PerfStats {
    return .{
        .backedge_count = self.backedge_count,
        .pic_hits = self.pic_hits,
        .pic_misses = self.pic_misses,
        .deopt_count = self.deopt_count,
        .mega_recoveries = self.mega_recoveries,
        .tier_promotions = self.tier_promotions,
        .promotion_attempted = self.promotion_attempted,
        .promotion_succeeded = self.promotion_succeeded,
        .promotion_rejected_deopt_storm = self.promotion_rejected_deopt_storm,
        .opcode_histogram_enabled = perf.enable_opcode_histogram,
        .opcode_histogram_nonzero = if (perf.enable_opcode_histogram)
            perf.countNonZeroHistogramEntries(self.opcode_histogram[0..])
        else
            0,
        .opcode_histogram = self.opcode_histogram,
    };
}

pub fn recordDeopt(self: *Interpreter) void {
    self.deopt_count +%= 1;
}
