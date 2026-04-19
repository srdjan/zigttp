//! zigttp:time - Date/time formatting and arithmetic
//!
//! Exports:
//!   formatIso(epochMs: number) -> string
//!     Formats as ISO 8601 UTC string: "2024-03-15T14:30:00.000Z"
//!
//!   formatHttp(epochMs: number) -> string
//!     Formats as RFC 2822 HTTP date: "Fri, 15 Mar 2024 14:30:00 GMT"
//!
//!   parseIso(str: string) -> number | undefined
//!     Parses ISO 8601 string to epoch milliseconds. Returns undefined on invalid input.
//!
//!   addSeconds(epochMs: number, seconds: number) -> number
//!     Adds seconds to a timestamp. Negative values subtract.

const std = @import("std");
const context = @import("../../context.zig");
const value = @import("../../value.zig");
const util = @import("../internal/util.zig");
const mb = @import("../../module_binding.zig");

const epoch = std.time.epoch;
const builtins_helpers = @import("../../builtins/helpers.zig");

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:time",
    .name = "time",
    .exports = &.{
        // These functions format/parse a caller-provided epoch number;
        // they do not read the clock themselves (there is no `now`
        // helper here - handlers get the current time via Date.now from
        // the JS engine). Pure in the strict sense.
        .{ .name = "formatIso", .func = formatIsoNative, .arg_count = 1, .returns = .string, .param_types = &.{.number}, .laws = &.{.pure} },
        .{ .name = "formatHttp", .func = formatHttpNative, .arg_count = 1, .returns = .string, .param_types = &.{.number}, .laws = &.{.pure} },
        .{ .name = "parseIso", .func = parseIsoNative, .arg_count = 1, .returns = .number, .param_types = &.{.string}, .failure_severity = .expected, .laws = &.{.pure} },
        .{ .name = "addSeconds", .func = addSecondsNative, .arg_count = 2, .returns = .number, .param_types = &.{ .number, .number }, .laws = &.{.pure} },
    },
};

pub const exports = binding.toModuleExports();

// -------------------------------------------------------------------------
// Date component extraction from epoch milliseconds
// -------------------------------------------------------------------------

const DateComponents = struct {
    year: u16,
    month: u4, // 1-12
    day: u5, // 1-31
    hour: u5,
    minute: u6,
    second: u6,
    ms: u16,
    day_of_week: u3, // 0=Sun, 1=Mon, ..., 6=Sat
};

fn epochMsToComponents(epoch_ms: i64) DateComponents {
    const ms_remainder: u16 = @intCast(@mod(@as(u64, @intCast(@max(0, epoch_ms))), 1000));
    const total_secs: u64 = @intCast(@divTrunc(@as(u64, @intCast(@max(0, epoch_ms))), 1000));

    const es = epoch.EpochSeconds{ .secs = total_secs };
    const epoch_day = es.getEpochDay();
    const day_secs = es.getDaySeconds();
    const year_day = epoch_day.calculateYearDay();
    const month_day = year_day.calculateMonthDay();

    // Day of week: epoch day 0 = 1970-01-01 = Thursday (4)
    // 0=Sun, 1=Mon, ..., 4=Thu, 5=Fri, 6=Sat
    const dow: u3 = @intCast(@mod(epoch_day.day + 4, 7));

    return .{
        .year = @intCast(year_day.year),
        .month = @intFromEnum(month_day.month),
        .day = month_day.day_index + 1,
        .hour = day_secs.getHoursIntoDay(),
        .minute = day_secs.getMinutesIntoHour(),
        .second = day_secs.getSecondsIntoMinute(),
        .ms = ms_remainder,
        .day_of_week = dow,
    };
}

// -------------------------------------------------------------------------
// formatIso: "2024-03-15T14:30:00.000Z"
// -------------------------------------------------------------------------

fn formatIsoNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    if (args.len == 0) return value.JSValue.undefined_val;
    const epoch_ms = extractEpochMs(args[0]) orelse return value.JSValue.undefined_val;

    const c = epochMsToComponents(epoch_ms);
    var buf: [24]u8 = undefined; // "2024-03-15T14:30:00.000Z"
    const s = std.fmt.bufPrint(&buf, "{d:0>4}-{d:0>2}-{d:0>2}T{d:0>2}:{d:0>2}:{d:0>2}.{d:0>3}Z", .{
        c.year, c.month, c.day, c.hour, c.minute, c.second, c.ms,
    }) catch return value.JSValue.undefined_val;

    return ctx.createString(s) catch value.JSValue.undefined_val;
}

// -------------------------------------------------------------------------
// formatHttp: "Fri, 15 Mar 2024 14:30:00 GMT"
// -------------------------------------------------------------------------

const DAY_NAMES = [_][]const u8{ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
const MONTH_NAMES = [_][]const u8{ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

fn formatHttpNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    if (args.len == 0) return value.JSValue.undefined_val;
    const epoch_ms = extractEpochMs(args[0]) orelse return value.JSValue.undefined_val;

    const c = epochMsToComponents(epoch_ms);
    var buf: [32]u8 = undefined; // "Thu, 01 Jan 1970 00:00:00 GMT"
    const day_name = if (c.day_of_week < 7) DAY_NAMES[c.day_of_week] else "???";
    const month_name = if (c.month >= 1 and c.month <= 12) MONTH_NAMES[c.month - 1] else "???";

    const s = std.fmt.bufPrint(&buf, "{s}, {d:0>2} {s} {d:0>4} {d:0>2}:{d:0>2}:{d:0>2} GMT", .{
        day_name, c.day, month_name, c.year, c.hour, c.minute, c.second,
    }) catch return value.JSValue.undefined_val;

    return ctx.createString(s) catch value.JSValue.undefined_val;
}

// -------------------------------------------------------------------------
// parseIso: "2024-03-15T14:30:00.000Z" -> epoch ms
// -------------------------------------------------------------------------

fn parseIsoNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    if (args.len == 0) return value.JSValue.undefined_val;
    const input = util.extractString(args[0]) orelse return value.JSValue.undefined_val;

    const ms = parseIsoString(input) orelse return value.JSValue.undefined_val;
    return builtins_helpers.allocFloat(ctx, @floatFromInt(ms));
}

fn parseIsoString(s: []const u8) ?i64 {
    // Minimum: "YYYY-MM-DD" (10 chars)
    if (s.len < 10) return null;

    const year = parseInt(u16, s[0..4]) orelse return null;
    if (s[4] != '-') return null;
    const month = parseInt(u8, s[5..7]) orelse return null;
    if (s[7] != '-') return null;
    const day = parseInt(u8, s[8..10]) orelse return null;

    if (month < 1 or month > 12 or day < 1 or day > 31) return null;

    var hour: u8 = 0;
    var minute: u8 = 0;
    var second: u8 = 0;
    var ms: u16 = 0;

    if (s.len > 10) {
        if (s[10] != 'T' and s[10] != 't') return null;
        if (s.len < 16) return null;
        hour = parseInt(u8, s[11..13]) orelse return null;
        if (s[13] != ':') return null;
        minute = parseInt(u8, s[14..16]) orelse return null;

        if (s.len > 16 and s[16] == ':') {
            if (s.len < 19) return null;
            second = parseInt(u8, s[17..19]) orelse return null;

            if (s.len > 19 and s[19] == '.') {
                // Parse fractional seconds (1-3 digits)
                var end: usize = 20;
                while (end < s.len and std.ascii.isDigit(s[end])) : (end += 1) {}
                const frac_str = s[20..end];
                if (frac_str.len >= 3) {
                    ms = parseInt(u16, frac_str[0..3]) orelse 0;
                } else if (frac_str.len == 2) {
                    ms = (parseInt(u16, frac_str) orelse 0) * 10;
                } else if (frac_str.len == 1) {
                    ms = (parseInt(u16, frac_str) orelse 0) * 100;
                }
            }
        }
    }

    // Convert to epoch ms using day calculation
    const days = yearMonthDayToEpochDays(year, month, day) orelse return null;
    const total_seconds = @as(i64, days) * 86400 + @as(i64, hour) * 3600 + @as(i64, minute) * 60 + @as(i64, second);
    return total_seconds * 1000 + @as(i64, ms);
}

fn yearMonthDayToEpochDays(year: u16, month: u8, day: u8) ?i64 {
    if (year < 1970) return null;

    // O(1) leap year count: leaps in [1970, year)
    const leapsBefore = leapYearsBefore(year);
    const leaps1970 = leapYearsBefore(1970);
    const y: i64 = @as(i64, year) - 1970;
    var total_days: i64 = y * 365 + @as(i64, leapsBefore) - @as(i64, leaps1970);

    // Add months
    const month_days = [_]u8{ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    var m: u8 = 1;
    while (m < month) : (m += 1) {
        total_days += month_days[m - 1];
        if (m == 2 and isLeapYear(year)) total_days += 1;
    }

    total_days += day - 1;
    return total_days;
}

/// Count leap years before a given year (from year 0).
fn leapYearsBefore(year: u16) u16 {
    const y = year - 1;
    return y / 4 - y / 100 + y / 400;
}

fn isLeapYear(year: u16) bool {
    return (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0);
}

// -------------------------------------------------------------------------
// addSeconds
// -------------------------------------------------------------------------

fn addSecondsNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    if (args.len < 2) return value.JSValue.undefined_val;

    const epoch_ms = extractEpochMs(args[0]) orelse return value.JSValue.undefined_val;
    const seconds_f = util.extractFloat(args[1]) orelse return value.JSValue.undefined_val;
    const delta_ms: i64 = @intFromFloat(seconds_f * 1000.0);

    return builtins_helpers.allocFloat(ctx, @floatFromInt(epoch_ms + delta_ms));
}

// -------------------------------------------------------------------------
// Helpers
// -------------------------------------------------------------------------

fn extractEpochMs(val: value.JSValue) ?i64 {
    if (val.isInt()) {
        return @as(i64, val.getInt());
    }
    if (val.isFloat64()) {
        const f = val.getFloat64();
        if (std.math.isNan(f) or std.math.isInf(f)) return null;
        return @intFromFloat(f);
    }
    return null;
}

fn parseInt(comptime T: type, s: []const u8) ?T {
    return std.fmt.parseInt(T, s, 10) catch null;
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

test "epochMsToComponents: Unix epoch" {
    const c = epochMsToComponents(0);
    try std.testing.expectEqual(@as(u16, 1970), c.year);
    try std.testing.expectEqual(@as(u4, 1), c.month);
    try std.testing.expectEqual(@as(u5, 1), c.day);
    try std.testing.expectEqual(@as(u5, 0), c.hour);
    try std.testing.expectEqual(@as(u6, 0), c.minute);
    try std.testing.expectEqual(@as(u6, 0), c.second);
    try std.testing.expectEqual(@as(u3, 4), c.day_of_week); // Thursday
}

test "epochMsToComponents: known date" {
    // 2024-03-15T14:30:00.123Z = 1710513000123
    const c = epochMsToComponents(1710513000123);
    try std.testing.expectEqual(@as(u16, 2024), c.year);
    try std.testing.expectEqual(@as(u4, 3), c.month);
    try std.testing.expectEqual(@as(u5, 15), c.day);
    try std.testing.expectEqual(@as(u5, 14), c.hour);
    try std.testing.expectEqual(@as(u6, 30), c.minute);
    try std.testing.expectEqual(@as(u6, 0), c.second);
    try std.testing.expectEqual(@as(u16, 123), c.ms);
    try std.testing.expectEqual(@as(u3, 5), c.day_of_week); // Friday
}

test "parseIsoString: full ISO 8601" {
    const ms = parseIsoString("2024-03-15T14:30:00.000Z").?;
    try std.testing.expectEqual(@as(i64, 1710513000000), ms);
}

test "parseIsoString: date only" {
    const ms = parseIsoString("1970-01-01").?;
    try std.testing.expectEqual(@as(i64, 0), ms);
}

test "parseIsoString: with milliseconds" {
    const ms = parseIsoString("2024-03-15T14:30:00.123Z").?;
    try std.testing.expectEqual(@as(i64, 1710513000123), ms);
}

test "parseIsoString: invalid" {
    try std.testing.expect(parseIsoString("not-a-date") == null);
    try std.testing.expect(parseIsoString("2024") == null);
    try std.testing.expect(parseIsoString("") == null);
}

test "isLeapYear" {
    try std.testing.expect(isLeapYear(2000));
    try std.testing.expect(isLeapYear(2024));
    try std.testing.expect(!isLeapYear(1900));
    try std.testing.expect(!isLeapYear(2023));
}

test "yearMonthDayToEpochDays" {
    try std.testing.expectEqual(@as(i64, 0), yearMonthDayToEpochDays(1970, 1, 1).?);
    try std.testing.expectEqual(@as(i64, 365), yearMonthDayToEpochDays(1971, 1, 1).?);
}
