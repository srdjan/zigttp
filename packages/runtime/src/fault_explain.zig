//! fault_explain.zig — turns a runtime handler fault into a proof-explained
//! message.
//!
//! When a handler faults at runtime, zttp does not just return a bare 500: it
//! names the proof chip(s) that guard the fault's class and checks them against
//! what the handler actually proved. The precise attribution uses the *unproven*
//! guarding chip as the predicted cause; only when every guarding chip was proven
//! and the handler still faulted is it a (possible) soundness incident — the
//! prover said it could not happen.
//!
//! This module is the pure core: fault class -> guarding chips -> diagnosis ->
//! message. Threading the fault class and the proof facts to each 500 site lives
//! in `zruntime.zig`/`server.zig`.

const std = @import("std");

/// A runtime fault class, coarser than the interpreter's `InterpreterError`
/// set: only classes a proof chip currently guards are distinguished.
pub const FaultClass = enum {
    /// A value was used in a way its type forbids — `TypeError`/`NotCallable`
    /// from dereferencing or calling an un-narrowed optional, or reading
    /// `.value` off an unchecked `Result`. Guarded by `optional_safe`/`result_safe`.
    type_fault,
    /// The handler returned something other than a Response. Guarded by
    /// `exhaustive_returns`.
    non_response_return,
    /// Any other fault with no chip mapping yet — stays an ordinary 500.
    other,
};

/// The proof facts a fault site needs to attribute a fault. A neutral input so
/// this module couples to neither `contract_runtime` nor `runtime_config`;
/// callers populate it from whichever proof view they hold.
pub const Proof = struct {
    optional_safe: bool = false,
    result_safe: bool = false,
    exhaustive_returns: bool = false,
};

/// The proof chips that guard a fault class, in priority order. Static storage.
pub fn guardingChips(fault: FaultClass) []const []const u8 {
    return switch (fault) {
        .type_fault => &.{ "optional_safe", "result_safe" },
        .non_response_return => &.{"exhaustive_returns"},
        .other => &.{},
    };
}

pub const Verdict = enum { predicted, soundness_incident, unmapped };

/// A fault attributed to specific chips. `chips[0..chip_count]` are the chips
/// the message names: the *unproven* guarding chips for `.predicted` (the likely
/// cause), or all guarding chips for `.soundness_incident` (every one was proven).
pub const Diagnosis = struct {
    verdict: Verdict,
    fault: FaultClass,
    chips: [2][]const u8 = .{ "", "" },
    chip_count: u8 = 0,

    pub fn namedChips(self: *const Diagnosis) []const []const u8 {
        return self.chips[0..self.chip_count];
    }
};

/// Attribute a fault to the guarding chip(s) that best explain it. The unproven
/// guarding chips are the predicted cause; if every guarding chip was proven the
/// fault contradicts a complete proof and is a possible soundness incident.
pub fn diagnose(proof: Proof, fault: FaultClass) Diagnosis {
    const guards = guardingChips(fault);
    if (guards.len == 0) return .{ .verdict = .unmapped, .fault = fault };

    var d = Diagnosis{ .verdict = .predicted, .fault = fault };
    for (guards) |chip| {
        if (!isProven(proof, chip)) {
            d.chips[d.chip_count] = chip;
            d.chip_count += 1;
        }
    }
    if (d.chip_count == 0) {
        // Every guarding chip was proven, yet the handler faulted.
        d.verdict = .soundness_incident;
        for (guards) |chip| {
            d.chips[d.chip_count] = chip;
            d.chip_count += 1;
        }
    }
    return d;
}

fn isProven(proof: Proof, chip: []const u8) bool {
    if (std.mem.eql(u8, chip, "optional_safe")) return proof.optional_safe;
    if (std.mem.eql(u8, chip, "result_safe")) return proof.result_safe;
    if (std.mem.eql(u8, chip, "exhaustive_returns")) return proof.exhaustive_returns;
    return false;
}

/// Format a proof-explained fault message into `buf`. Bounded and
/// allocation-free; a 256-byte `buf` is always sufficient.
pub fn formatMessage(buf: []u8, diag: Diagnosis) []const u8 {
    if (diag.verdict == .unmapped) return "Internal Server Error";

    var chip_buf: [96]u8 = undefined;
    const chips = joinChips(&chip_buf, diag.namedChips());

    const cause: []const u8 = switch (diag.fault) {
        .type_fault => "A value was used without narrowing its type.",
        .non_response_return => "The handler returned something other than a Response.",
        .other => "",
    };

    return switch (diag.verdict) {
        .predicted => std.fmt.bufPrint(
            buf,
            "This response path was not proven {s}. {s}",
            .{ chips, cause },
        ) catch "Internal Server Error",
        .soundness_incident => std.fmt.bufPrint(
            buf,
            "Possible soundness incident: the handler faulted on a path proven {s}. " ++
                "This should be impossible — please report it.",
            .{chips},
        ) catch "Internal Server Error",
        .unmapped => "Internal Server Error",
    };
}

fn joinChips(buf: []u8, chips: []const []const u8) []const u8 {
    var len: usize = 0;
    for (chips, 0..) |chip, i| {
        if (i > 0) {
            const sep = std.fmt.bufPrint(buf[len..], "/", .{}) catch break;
            len += sep.len;
        }
        const w = std.fmt.bufPrint(buf[len..], "{s}", .{chip}) catch break;
        len += w.len;
    }
    return buf[0..len];
}

test "guardingChips names the chips that cover each fault class" {
    try std.testing.expectEqual(@as(usize, 2), guardingChips(.type_fault).len);
    try std.testing.expectEqualStrings("optional_safe", guardingChips(.type_fault)[0]);
    try std.testing.expectEqualStrings("result_safe", guardingChips(.type_fault)[1]);
    try std.testing.expectEqualStrings("exhaustive_returns", guardingChips(.non_response_return)[0]);
    try std.testing.expectEqual(@as(usize, 0), guardingChips(.other).len);
}

test "diagnose: nothing proven => predicted, names all guarding chips" {
    const d = diagnose(.{}, .type_fault);
    try std.testing.expectEqual(Verdict.predicted, d.verdict);
    try std.testing.expectEqual(@as(u8, 2), d.chip_count);
    try std.testing.expectEqualStrings("optional_safe", d.chips[0]);
    try std.testing.expectEqualStrings("result_safe", d.chips[1]);
}

test "diagnose: one chip proven => predicted names only the UNPROVEN chip (precise)" {
    const d = diagnose(.{ .optional_safe = true }, .type_fault);
    try std.testing.expectEqual(Verdict.predicted, d.verdict);
    try std.testing.expectEqual(@as(u8, 1), d.chip_count);
    try std.testing.expectEqualStrings("result_safe", d.chips[0]);
}

test "diagnose: all guarding chips proven but faulted => soundness incident" {
    const d = diagnose(.{ .optional_safe = true, .result_safe = true }, .type_fault);
    try std.testing.expectEqual(Verdict.soundness_incident, d.verdict);
    try std.testing.expectEqual(@as(u8, 2), d.chip_count);
}

test "diagnose: non-Response return maps to exhaustive_returns" {
    const predicted = diagnose(.{}, .non_response_return);
    try std.testing.expectEqual(Verdict.predicted, predicted.verdict);
    try std.testing.expectEqualStrings("exhaustive_returns", predicted.chips[0]);

    const incident = diagnose(.{ .exhaustive_returns = true }, .non_response_return);
    try std.testing.expectEqual(Verdict.soundness_incident, incident.verdict);
}

test "diagnose: unmapped fault class is never an incident" {
    try std.testing.expectEqual(Verdict.unmapped, diagnose(.{}, .other).verdict);
}

test "formatMessage: predicted names the unproven chip and the cause" {
    var buf: [256]u8 = undefined;
    const msg = formatMessage(&buf, diagnose(.{ .optional_safe = true }, .type_fault));
    try std.testing.expect(std.mem.indexOf(u8, msg, "not proven result_safe") != null);
    try std.testing.expect(std.mem.indexOf(u8, msg, "optional_safe") == null); // proven, not named
    try std.testing.expect(std.mem.indexOf(u8, msg, "narrowing its type") != null);
}

test "formatMessage: soundness incident is loud and lists the proven chips" {
    var buf: [256]u8 = undefined;
    const msg = formatMessage(&buf, diagnose(.{ .optional_safe = true, .result_safe = true }, .type_fault));
    try std.testing.expect(std.mem.indexOf(u8, msg, "Possible soundness incident") != null);
    try std.testing.expect(std.mem.indexOf(u8, msg, "optional_safe/result_safe") != null);
}

test "formatMessage: unmapped falls back to a generic 500 message" {
    var buf: [256]u8 = undefined;
    try std.testing.expectEqualStrings("Internal Server Error", formatMessage(&buf, diagnose(.{}, .other)));
}
