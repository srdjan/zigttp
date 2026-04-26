//! Interpreter performance counters and opcode histogram.

const std = @import("std");
const bytecode = @import("../bytecode.zig");
const build_options = @import("build_options");

pub const tier_count = std.meta.fields(bytecode.CompilationTier).len;

pub const enable_opcode_histogram = build_options.perf_histogram;

pub const PerfStats = struct {
    backedge_count: u32 = 0,
    pic_hits: u32 = 0,
    pic_misses: u32 = 0,
    deopt_count: u32 = 0,
    mega_recoveries: u32 = 0,
    tier_promotions: [tier_count]u32 = [_]u32{0} ** tier_count,
    promotion_attempted: u32 = 0,
    promotion_succeeded: u32 = 0,
    promotion_rejected_deopt_storm: u32 = 0,
    opcode_histogram_enabled: bool = enable_opcode_histogram,
    opcode_histogram_nonzero: u32 = 0,
    opcode_histogram: [256]u32 = [_]u32{0} ** 256,
};

pub fn countNonZeroHistogramEntries(histogram: []const u32) u32 {
    var count: u32 = 0;
    for (histogram) |entry| {
        if (entry > 0) count +|= 1;
    }
    return count;
}
