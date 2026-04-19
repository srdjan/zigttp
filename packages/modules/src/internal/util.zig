//! Shared helpers for virtual modules. Only helpers with added value
//! live here; callers import `zigttp-sdk` directly for SDK primitives.

const std = @import("std");
const sdk = @import("zigttp-sdk");

/// Throw a TypeError with the given message.
pub fn throwTypeError(handle: *sdk.ModuleHandle, message: []const u8) sdk.JSValue {
    return sdk.throwError(handle, "TypeError", message);
}

/// Throw a CapabilityPolicyError carrying the denied category and subject.
pub fn throwCapabilityPolicyError(
    handle: *sdk.ModuleHandle,
    category: []const u8,
    subject: []const u8,
) sdk.JSValue {
    var buf: [256]u8 = undefined;
    const written = std.fmt.bufPrint(
        &buf,
        "{s} '{s}' is not allowed by capability policy",
        .{ category, subject },
    ) catch return sdk.throwError(handle, "CapabilityPolicyError", "capability denied by policy");
    return sdk.throwError(handle, "CapabilityPolicyError", written);
}
