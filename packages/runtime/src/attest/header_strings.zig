//! Proof receipt response headers.
//!
//! Precomputes the two response header values exactly once, at runtime
//! startup, so the per-request cost is one `writer.print` per header:
//!
//!   Zttp-Proofs: pure, read_only, injection_safe, ...
//!   Zttp-Attest: <compact JWS>
//!
//! Field set and emission order are fixed (`chip_field_names` below). Only
//! `true` chips appear; the empty list signals "do not emit Zttp-Proofs."
//! `has_egress` and `max_io_depth` are intentionally excluded: the header
//! advertises positive proof claims to external consumers, not facts about
//! the handler's I/O surface.

const std = @import("std");

/// Wire names for the two HTTP headers slice 1 emits. Single source of truth
/// so the writer (`server.zig:appendAttestationHeaders`) and the reader
/// (`verify_cli.zig:fetchAttestHeader`) can't drift on capitalization.
pub const header_name_proofs: []const u8 = "Zttp-Proofs";
pub const header_name_attest: []const u8 = "Zttp-Attest";

pub const chip_field_names = [_][]const u8{
    "pure",
    "read_only",
    "stateless",
    "retry_safe",
    "deterministic",
    "no_secret_leakage",
    "no_credential_leakage",
    "input_validated",
    "pii_contained",
    "injection_safe",
    "idempotent",
    "state_isolated",
    "fault_covered",
    "result_safe",
    "optional_safe",
};

pub const HeaderStrings = struct {
    /// Empty slice means the runtime should not emit `Zttp-Proofs` at all
    /// (no chips were proven). `attest_value` is always present when this
    /// struct is built.
    proofs_value: []const u8,
    attest_value: []const u8,

    pub fn deinit(self: *HeaderStrings, allocator: std.mem.Allocator) void {
        if (self.proofs_value.len > 0) allocator.free(self.proofs_value);
        allocator.free(self.attest_value);
    }
};

/// `props` must be a struct with `bool` fields matching `chip_field_names`.
/// Missing fields are treated as `false`, so older `Properties` shapes stay
/// compatible.
pub fn build(
    allocator: std.mem.Allocator,
    props: anytype,
    jws_compact: []const u8,
) !HeaderStrings {
    const proofs_value = try formatProofChips(allocator, props);
    errdefer if (proofs_value.len > 0) allocator.free(proofs_value);

    const attest_value = try allocator.dupe(u8, jws_compact);

    return .{
        .proofs_value = proofs_value,
        .attest_value = attest_value,
    };
}

/// Comma-and-space-joined list of `true` chip field names in
/// `chip_field_names` order. Empty slice when no chip is proven. Exposed so
/// the compile-time signer can put the same string into signed claims that
/// the runtime later emits as a header.
pub fn formatProofChips(allocator: std.mem.Allocator, props: anytype) ![]const u8 {
    var total_len: usize = 0;
    var count: usize = 0;
    inline for (chip_field_names) |name| {
        if (readBoolField(props, name)) {
            total_len += name.len;
            count += 1;
        }
    }
    if (count == 0) return "";
    total_len += (count - 1) * 2; // ", " separators

    var buf = try allocator.alloc(u8, total_len);
    var pos: usize = 0;
    var emitted: usize = 0;
    inline for (chip_field_names) |name| {
        if (readBoolField(props, name)) {
            if (emitted > 0) {
                buf[pos] = ',';
                buf[pos + 1] = ' ';
                pos += 2;
            }
            @memcpy(buf[pos .. pos + name.len], name);
            pos += name.len;
            emitted += 1;
        }
    }
    return buf;
}

fn readBoolField(props: anytype, comptime name: []const u8) bool {
    const T = @TypeOf(props);
    if (!@hasField(T, name)) return false;
    return @field(props, name);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const TestProps = struct {
    pure: bool = false,
    read_only: bool = false,
    stateless: bool = false,
    retry_safe: bool = false,
    deterministic: bool = false,
    no_secret_leakage: bool = false,
    no_credential_leakage: bool = false,
    input_validated: bool = false,
    pii_contained: bool = false,
    injection_safe: bool = false,
    idempotent: bool = false,
    state_isolated: bool = false,
    fault_covered: bool = false,
    result_safe: bool = false,
    optional_safe: bool = false,
    // Fields header_strings deliberately ignores:
    has_egress: bool = false,
    max_io_depth: ?u32 = null,
};

test "all-false props produce an empty proofs value" {
    const allocator = std.testing.allocator;
    var hs = try build(allocator, TestProps{}, "h.p.s");
    defer hs.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 0), hs.proofs_value.len);
    try std.testing.expectEqualStrings("h.p.s", hs.attest_value);
}

test "single true chip emits exactly that name" {
    const allocator = std.testing.allocator;
    var hs = try build(allocator, TestProps{ .injection_safe = true }, "jws");
    defer hs.deinit(allocator);

    try std.testing.expectEqualStrings("injection_safe", hs.proofs_value);
}

test "multiple chips join with comma-space in declared order" {
    const allocator = std.testing.allocator;
    var hs = try build(
        allocator,
        TestProps{
            .pure = true,
            .read_only = true,
            .injection_safe = true,
            .no_secret_leakage = true,
            .deterministic = true,
        },
        "jws",
    );
    defer hs.deinit(allocator);

    try std.testing.expectEqualStrings(
        "pure, read_only, deterministic, no_secret_leakage, injection_safe",
        hs.proofs_value,
    );
}

test "has_egress is excluded from the public header" {
    const allocator = std.testing.allocator;
    var hs = try build(
        allocator,
        TestProps{ .has_egress = true, .pure = true },
        "jws",
    );
    defer hs.deinit(allocator);

    try std.testing.expectEqualStrings("pure", hs.proofs_value);
}

test "structs missing chip fields treat them as false" {
    const allocator = std.testing.allocator;
    const Partial = struct {
        pure: bool = true,
        injection_safe: bool = true,
    };
    var hs = try build(allocator, Partial{}, "jws");
    defer hs.deinit(allocator);

    try std.testing.expectEqualStrings("pure, injection_safe", hs.proofs_value);
}

test "attest_value is an owned copy, not an alias" {
    const allocator = std.testing.allocator;
    var jws_buf: [16]u8 = .{ 'a', 'b', 'c', '.', 'd', 'e', 'f', '.', 'g', 'h', 'i', 0, 0, 0, 0, 0 };
    var hs = try build(allocator, TestProps{}, jws_buf[0..11]);
    defer hs.deinit(allocator);

    @memset(jws_buf[0..11], 'X');
    try std.testing.expectEqualStrings("abc.def.ghi", hs.attest_value);
}

test "chip_field_names matches the public contract length" {
    try std.testing.expectEqual(@as(usize, 15), chip_field_names.len);
}
