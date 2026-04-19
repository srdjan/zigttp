//! Shared utilities for virtual modules in the peer package.
//!
//! Thin adaptations of the SDK's handle-bound operations. Modules
//! should import these helpers instead of repeating the boilerplate
//! for TypeError/CapabilityPolicyError construction.

const std = @import("std");
const sdk = @import("zigttp-sdk");

pub const extractString = sdk.extractString;
pub const extractInt = sdk.extractInt;
pub const extractFloat = sdk.extractFloat;
pub const createString = sdk.createString;
pub const getAllocator = sdk.getAllocator;
pub const createObject = sdk.createObject;
pub const objectSet = sdk.objectSet;
pub const objectGet = sdk.objectGet;
pub const resultOk = sdk.resultOk;
pub const resultErr = sdk.resultErr;
pub const resultErrValue = sdk.resultErrValue;
pub const resultErrs = sdk.resultErrs;
pub const throwError = sdk.throwError;

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
    // Allocate the message on a small fixed stack buffer; if it overflows
    // fall back to a static message so the error still surfaces.
    var buf: [256]u8 = undefined;
    const written = std.fmt.bufPrint(
        &buf,
        "{s} '{s}' is not allowed by capability policy",
        .{ category, subject },
    ) catch return sdk.throwError(handle, "CapabilityPolicyError", "capability denied by policy");
    return sdk.throwError(handle, "CapabilityPolicyError", written);
}

test "throwTypeError compiles" {
    // Cannot exercise the handle-bound path without a live runtime; this
    // test just asserts the function signatures compile against the SDK.
    const handle: *sdk.ModuleHandle = @ptrFromInt(0x1);
    _ = handle;
    _ = throwTypeError;
    _ = throwCapabilityPolicyError;
}
