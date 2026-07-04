//! fault_explain.zig — turns a runtime handler fault into a proof-explained
//! message.
//!
//! When a handler faults at runtime, zigttp does not just return a bare 500: it
//! names the proof chip(s) that guard the fault's class and checks them against
//! the handler's proven contract. A fault on a path that *claimed* the guarding
//! chip is a soundness incident (the prover said it could not happen); a fault
//! on an unproven path was predicted (the `[-]` pill already flagged it).
//!
//! This module is the pure core: fault class -> guarding chips -> verdict, plus
//! message formatting. Threading the fault class from the interpreter to the
//! 500 site, and reading the contract there, lives in `zruntime.zig`/`server.zig`.

const std = @import("std");
const Properties = @import("contract_runtime.zig").Properties;

/// A runtime fault class, coarser than the interpreter's `InterpreterError`
/// set: only classes a proof chip currently guards are distinguished. Everything
/// else is `.other`, which stays an ordinary unexplained 500.
pub const FaultClass = enum {
    /// A value was used in a way its type forbids — `TypeError`/`NotCallable`
    /// from dereferencing or calling an un-narrowed optional, or reading
    /// `.value` off an unchecked `Result`. Guarded by `optional_safe`/`result_safe`.
    type_fault,
    /// Any other interpreter fault with no chip mapping yet.
    other,
};

/// The proof chips that guard a fault class, in priority order. Static storage.
pub fn guardingChips(fault: FaultClass) []const []const u8 {
    return switch (fault) {
        .type_fault => &.{ "optional_safe", "result_safe" },
        .other => &.{},
    };
}

/// Was the fault predicted by an unproven chip, or does it contradict a proven
/// one (a soundness incident)? `.unmapped` when no chip covers the fault class.
pub const Verdict = enum { predicted, soundness_incident, unmapped };

/// Classify a fault against the handler's proven properties. A guarding chip
/// proven `true` while the handler still faulted in that class is a soundness
/// incident; otherwise the fault was predicted by the unproven chip.
pub fn classify(props: Properties, fault: FaultClass) Verdict {
    return switch (fault) {
        .type_fault => if (props.optional_safe or props.result_safe)
            .soundness_incident
        else
            .predicted,
        .other => .unmapped,
    };
}

/// Format a proof-explained fault message into `buf`. Bounded and
/// allocation-free; `buf` of 256 bytes is always sufficient.
pub fn formatMessage(buf: []u8, fault: FaultClass, verdict: Verdict) []const u8 {
    const chips = guardingChips(fault);
    return switch (verdict) {
        .soundness_incident => std.fmt.bufPrint(
            buf,
            "Soundness incident: handler faulted (type error) on a path proven {s}/{s}. " ++
                "This should be impossible — please report it.",
            .{ chipOrEmpty(chips, 0), chipOrEmpty(chips, 1) },
        ) catch fallback(verdict),
        .predicted => std.fmt.bufPrint(
            buf,
            "Type error: this response path was not proven {s}/{s}. " ++
                "A value was used without narrowing its type.",
            .{ chipOrEmpty(chips, 0), chipOrEmpty(chips, 1) },
        ) catch fallback(verdict),
        .unmapped => fallback(verdict),
    };
}

fn chipOrEmpty(chips: []const []const u8, i: usize) []const u8 {
    return if (i < chips.len) chips[i] else "";
}

fn fallback(verdict: Verdict) []const u8 {
    _ = verdict;
    return "Internal Server Error";
}

test "guardingChips names the chips that cover a type fault" {
    const chips = guardingChips(.type_fault);
    try std.testing.expectEqual(@as(usize, 2), chips.len);
    try std.testing.expectEqualStrings("optional_safe", chips[0]);
    try std.testing.expectEqualStrings("result_safe", chips[1]);
    try std.testing.expectEqual(@as(usize, 0), guardingChips(.other).len);
}

test "classify: unproven guarding chip => predicted" {
    const props = Properties{}; // all false
    try std.testing.expectEqual(Verdict.predicted, classify(props, .type_fault));
}

test "classify: proven optional_safe but faulted => soundness incident" {
    var props = Properties{};
    props.optional_safe = true;
    try std.testing.expectEqual(Verdict.soundness_incident, classify(props, .type_fault));
}

test "classify: proven result_safe but faulted => soundness incident" {
    var props = Properties{};
    props.result_safe = true;
    try std.testing.expectEqual(Verdict.soundness_incident, classify(props, .type_fault));
}

test "classify: unmapped fault class is never an incident" {
    var props = Properties{};
    props.optional_safe = true;
    try std.testing.expectEqual(Verdict.unmapped, classify(props, .other));
}

test "formatMessage: predicted names both guarding chips" {
    var buf: [256]u8 = undefined;
    const msg = formatMessage(&buf, .type_fault, .predicted);
    try std.testing.expect(std.mem.indexOf(u8, msg, "optional_safe") != null);
    try std.testing.expect(std.mem.indexOf(u8, msg, "result_safe") != null);
    try std.testing.expect(std.mem.indexOf(u8, msg, "not proven") != null);
}

test "formatMessage: soundness incident is loud and specific" {
    var buf: [256]u8 = undefined;
    const msg = formatMessage(&buf, .type_fault, .soundness_incident);
    try std.testing.expect(std.mem.indexOf(u8, msg, "Soundness incident") != null);
    try std.testing.expect(std.mem.indexOf(u8, msg, "optional_safe") != null);
}

test "formatMessage: unmapped falls back to a generic 500 message" {
    var buf: [256]u8 = undefined;
    const msg = formatMessage(&buf, .other, .unmapped);
    try std.testing.expectEqualStrings("Internal Server Error", msg);
}
