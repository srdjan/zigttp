const std = @import("std");
const sdk = @import("zigttp-sdk");
const test_shim = @import("zigttp-sdk-test-shim");

test {
    std.testing.refAllDecls(sdk);
    std.testing.refAllDecls(test_shim);
}

test "numberFromF64 folds safe-integer floats to int tag" {
    try std.testing.expect(sdk.numberFromF64(0.0).isInt());
    try std.testing.expectEqual(@as(i32, 0), sdk.numberFromF64(0.0).getInt());
    try std.testing.expectEqual(@as(i32, 42), sdk.numberFromF64(42.0).getInt());
    try std.testing.expectEqual(@as(i32, -1), sdk.numberFromF64(-1.0).getInt());
    try std.testing.expectEqual(@as(i32, 2147483647), sdk.numberFromF64(2147483647.0).getInt());
    try std.testing.expectEqual(@as(i32, -2147483648), sdk.numberFromF64(-2147483648.0).getInt());
}

test "numberFromF64 keeps non-integer and out-of-range as float" {
    try std.testing.expect(!sdk.numberFromF64(0.5).isInt());
    try std.testing.expectEqual(@as(f64, 0.5), sdk.numberFromF64(0.5).toFloat().?);
    try std.testing.expect(!sdk.numberFromF64(2147483648.0).isInt());
    try std.testing.expect(!sdk.numberFromF64(-2147483649.0).isInt());
    try std.testing.expect(!sdk.numberFromF64(std.math.inf(f64)).isInt());
    try std.testing.expect(!sdk.numberFromF64(-std.math.inf(f64)).isInt());
    try std.testing.expect(!sdk.numberFromF64(std.math.nan(f64)).isInt());
}

test "extractInt accepts int tag and whole-number floats" {
    try std.testing.expectEqual(@as(?i32, 7), sdk.extractInt(sdk.JSValue.fromInt(7)));
    try std.testing.expectEqual(@as(?i32, 7), sdk.extractInt(sdk.JSValue.fromFloat(7.0)));
    try std.testing.expectEqual(@as(?i32, -3), sdk.extractInt(sdk.JSValue.fromFloat(-3.0)));
    try std.testing.expectEqual(@as(?i32, null), sdk.extractInt(sdk.JSValue.fromFloat(7.5)));
    try std.testing.expectEqual(@as(?i32, null), sdk.extractInt(sdk.JSValue.undefined_val));
}
