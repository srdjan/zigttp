//! Compiler-native repair plan primitives.
//!
//! This module does not edit source. It turns verifier and flow/property
//! failures into small, structured edit intents that an agent can apply, then
//! re-check through the existing compiler veto.

const std = @import("std");
const counterexample = @import("counterexample.zig");
const flow_checker = @import("flow_checker.zig");
const handler_verifier = @import("handler_verifier.zig");

pub const SourceSpan = counterexample.SourceSpan;

pub const RepairKind = enum {
    check_result_before_value,
    narrow_optional_before_use,
    add_fallback_response,
    redact_sensitive_sink,
    validate_before_egress,

    pub fn asString(self: RepairKind) []const u8 {
        return @tagName(self);
    }
};

pub const EditIntentKind = enum {
    insert_guard_before_line,
    replace_sink_expression,
    add_trailing_return,
    validate_before_sink,

    pub fn asString(self: EditIntentKind) []const u8 {
        return @tagName(self);
    }
};

pub const EditIntent = struct {
    kind: EditIntentKind,
    line: u32,
    column: u32,
    template: []const u8,
};

pub const Plan = struct {
    kind: RepairKind,
    target: SourceSpan,
    behavioral_change: bool,
    summary: []const u8,
    edit_intent: EditIntent,

    pub fn closesProperty(self: Plan, tag: counterexample.PropertyTag) bool {
        return switch (self.kind) {
            .redact_sensitive_sink => tag == .no_secret_leakage or tag == .no_credential_leakage,
            .validate_before_egress => tag == .injection_safe,
            else => false,
        };
    }
};

pub fn fromVerifierDiagnostic(
    diag: handler_verifier.Diagnostic,
    target: SourceSpan,
) ?Plan {
    return switch (diag.kind) {
        .unchecked_result_value => .{
            .kind = .check_result_before_value,
            .target = target,
            .behavioral_change = true,
            .summary = "Add an explicit non-2xx early return before reading result.value.",
            .edit_intent = .{
                .kind = .insert_guard_before_line,
                .line = target.line,
                .column = target.column,
                .template = "if (!result.ok) return Response.json({ error: result.error }, { status: 400 });",
            },
        },
        .unchecked_optional_use, .unchecked_optional_access => .{
            .kind = .narrow_optional_before_use,
            .target = target,
            .behavioral_change = true,
            .summary = "Narrow the optional value, provide a fallback, or return before using it.",
            .edit_intent = .{
                .kind = .insert_guard_before_line,
                .line = target.line,
                .column = target.column,
                .template = "if (value === undefined) return Response.json({ error: \"missing value\" }, { status: 400 });",
            },
        },
        .missing_return_path, .missing_return_else, .missing_return_default => .{
            .kind = .add_fallback_response,
            .target = target,
            .behavioral_change = true,
            .summary = "Add a fallback Response so every handler path returns.",
            .edit_intent = .{
                .kind = .add_trailing_return,
                .line = target.line,
                .column = target.column,
                .template = "return Response.text(\"Not Found\", { status: 404 });",
            },
        },
        else => null,
    };
}

pub fn fromFlowDiagnostic(
    diag: flow_checker.Diagnostic,
    tag: counterexample.PropertyTag,
    target: SourceSpan,
) ?Plan {
    _ = diag;
    return switch (tag) {
        .no_secret_leakage, .no_credential_leakage => .{
            .kind = .redact_sensitive_sink,
            .target = target,
            .behavioral_change = true,
            .summary = "Return before the witnessed sink so the labelled value is unreachable on this path.",
            // v1 apply only supports line insertions, so we close the sink
            // path with an early return rather than mutating the sink
            // argument. The property (no secret reaches the response) is
            // achieved; the handler's behaviour on the witnessed request
            // changes to a 500. Callers that want the original response
            // shape back should follow up with a semantic repair once
            // replace_sink_expression is apply-supported.
            .edit_intent = .{
                .kind = .insert_guard_before_line,
                .line = target.line,
                .column = target.column,
                .template = "return Response.json({ error: \"redacted to preserve no_secret_leakage\" }, { status: 500 });",
            },
        },
        .injection_safe => .{
            .kind = .validate_before_egress,
            .target = target,
            .behavioral_change = true,
            .summary = "Validate or coerce user input before it reaches the sensitive sink.",
            .edit_intent = .{
                .kind = .validate_before_sink,
                .line = target.line,
                .column = target.column,
                .template = "Decode and validate the request data, then pass only the validated value to this sink.",
            },
        },
        .input_validated, .pii_contained => null,
    };
}

test "verifier diagnostics map to narrow repair plans" {
    const diag = handler_verifier.Diagnostic{
        .severity = .err,
        .kind = .unchecked_result_value,
        .node = 0,
        .message = "result.value accessed without checking result.ok first",
        .help = null,
    };
    const plan = fromVerifierDiagnostic(diag, .{ .line = 12, .column = 9 }) orelse return error.MissingPlan;
    try std.testing.expectEqual(RepairKind.check_result_before_value, plan.kind);
    try std.testing.expectEqual(EditIntentKind.insert_guard_before_line, plan.edit_intent.kind);
    try std.testing.expect(plan.behavioral_change);
}

test "flow diagnostics map to property repair plans" {
    const diag = flow_checker.Diagnostic{
        .severity = .err,
        .kind = .secret_in_response,
        .node = 0,
        .message = "secret reaches response",
        .help = null,
    };
    const plan = fromFlowDiagnostic(diag, .no_secret_leakage, .{ .line = 7, .column = 17 }) orelse return error.MissingPlan;
    try std.testing.expectEqual(RepairKind.redact_sensitive_sink, plan.kind);
    try std.testing.expectEqual(EditIntentKind.insert_guard_before_line, plan.edit_intent.kind);
    try std.testing.expect(std.mem.indexOf(u8, plan.edit_intent.template, "return Response") != null);
    try std.testing.expect(plan.closesProperty(.no_secret_leakage));
}
