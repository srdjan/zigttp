//! ARM64 (AArch64) Instruction Emitter
//!
//! Generates ARM64 machine code for Apple Silicon and other AArch64 platforms.
//! All ARM64 instructions are 32 bits (fixed width).
//!
//! Apple ARM64 ABI:
//! - x0-x7: argument/return registers
//! - x8: indirect result location (struct returns)
//! - x9-x15: caller-saved temporaries
//! - x16-x17: intra-procedure-call scratch (avoid)
//! - x18: platform register (reserved on Apple)
//! - x19-x28: callee-saved
//! - x29 (fp): frame pointer
//! - x30 (lr): link register
//! - sp: stack pointer (separate from x31)

const std = @import("std");

/// ARM64 64-bit registers (x0-x30, sp)
/// Note: encoding 31 is SP in load/store and add/sub immediate forms, and XZR in many others.
pub const Register = enum(u5) {
    x0 = 0,
    x1 = 1,
    x2 = 2,
    x3 = 3,
    x4 = 4,
    x5 = 5,
    x6 = 6,
    x7 = 7,
    x8 = 8,
    x9 = 9,
    x10 = 10,
    x11 = 11,
    x12 = 12,
    x13 = 13,
    x14 = 14,
    x15 = 15,
    x16 = 16,
    x17 = 17,
    x18 = 18,
    x19 = 19,
    x20 = 20,
    x21 = 21,
    x22 = 22,
    x23 = 23,
    x24 = 24,
    x25 = 25,
    x26 = 26,
    x27 = 27,
    x28 = 28,
    x29 = 29, // fp (frame pointer)
    x30 = 30, // lr (link register)
    sp = 31, // stack pointer (only valid for specific encodings)

    pub fn encode(self: Register) u5 {
        return @intFromEnum(self);
    }
};

/// Register aliases for clarity
pub const Regs = struct {
    // Argument/return registers
    pub const arg0 = Register.x0;
    pub const arg1 = Register.x1;
    pub const arg2 = Register.x2;
    pub const arg3 = Register.x3;
    pub const ret = Register.x0;

    // Frame/link registers
    pub const fp = Register.x29;
    pub const lr = Register.x30;

    // Callee-saved registers we use
    pub const ctx = Register.x19; // Context pointer (preserved)
    pub const js_sp = Register.x20; // JS stack pointer (preserved)

    // Caller-saved scratch registers
    pub const tmp0 = Register.x9;
    pub const tmp1 = Register.x10;
    pub const tmp2 = Register.x11;
    pub const tmp3 = Register.x12;
    pub const tmp4 = Register.x13;
};

/// Condition codes for conditional branches
pub const Condition = enum(u4) {
    eq = 0b0000, // Equal (Z=1)
    ne = 0b0001, // Not equal (Z=0)
    cs = 0b0010, // Carry set / unsigned >= (C=1)
    cc = 0b0011, // Carry clear / unsigned < (C=0)
    mi = 0b0100, // Minus/negative (N=1)
    pl = 0b0101, // Plus/positive or zero (N=0)
    vs = 0b0110, // Overflow (V=1)
    vc = 0b0111, // No overflow (V=0)
    hi = 0b1000, // Unsigned higher (C=1 && Z=0)
    ls = 0b1001, // Unsigned lower or same (C=0 || Z=1)
    ge = 0b1010, // Signed >= (N=V)
    lt = 0b1011, // Signed < (N!=V)
    gt = 0b1100, // Signed > (Z=0 && N=V)
    le = 0b1101, // Signed <= (Z=1 || N!=V)
    al = 0b1110, // Always
    nv = 0b1111, // Never (reserved)

    // Aliases
    pub const hs = Condition.cs; // unsigned >=
    pub const lo = Condition.cc; // unsigned <
};

/// ARM64 instruction emitter
pub const Arm64Emitter = struct {
    buffer: std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Arm64Emitter {
        return .{
            .buffer = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Arm64Emitter) void {
        self.buffer.deinit(self.allocator);
    }

    pub fn getCode(self: *const Arm64Emitter) []const u8 {
        return self.buffer.items;
    }

    pub fn offset(self: *const Arm64Emitter) usize {
        return self.buffer.items.len;
    }

    /// Emit a 32-bit instruction (little-endian)
    fn emit32(self: *Arm64Emitter, inst: u32) !void {
        try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(inst)));
    }

    /// Reserve space for later patching
    pub fn reserve(self: *Arm64Emitter, n: usize) !usize {
        const off = self.offset();
        try self.buffer.appendNTimes(self.allocator, 0, n);
        return off;
    }

    /// Patch a 32-bit instruction at the given offset
    pub fn patch32(self: *Arm64Emitter, off: usize, inst: u32) void {
        const bytes = @as([4]u8, @bitCast(inst));
        @memcpy(self.buffer.items[off..][0..4], &bytes);
    }

    // ========================================
    // Data movement instructions
    // ========================================

    /// MOV (register) - ORR Xd, XZR, Xm
    pub fn movRegReg(self: *Arm64Emitter, dst: Register, src: Register) !void {
        // ORR Xd, XZR, Xm: 10101010000 Rm 000000 11111 Rd
        const inst: u32 = 0xAA0003E0 |
            (@as(u32, src.encode()) << 16) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// MOV (immediate) - MOVZ followed by MOVK instructions for 64-bit
    pub fn movRegImm64(self: *Arm64Emitter, dst: Register, imm: u64) !void {
        // For small immediates, use single MOVZ
        if (imm <= 0xFFFF) {
            try self.movz(dst, @truncate(imm), 0);
            return;
        }

        // Build 64-bit value with MOVZ + MOVK sequence
        const hw0: u16 = @truncate(imm);
        const hw1: u16 = @truncate(imm >> 16);
        const hw2: u16 = @truncate(imm >> 32);
        const hw3: u16 = @truncate(imm >> 48);

        // Find first non-zero halfword for MOVZ
        if (hw0 != 0 or (hw1 == 0 and hw2 == 0 and hw3 == 0)) {
            try self.movz(dst, hw0, 0);
            if (hw1 != 0) try self.movk(dst, hw1, 1);
            if (hw2 != 0) try self.movk(dst, hw2, 2);
            if (hw3 != 0) try self.movk(dst, hw3, 3);
        } else if (hw1 != 0) {
            try self.movz(dst, hw1, 1);
            if (hw0 != 0) try self.movk(dst, hw0, 0);
            if (hw2 != 0) try self.movk(dst, hw2, 2);
            if (hw3 != 0) try self.movk(dst, hw3, 3);
        } else if (hw2 != 0) {
            try self.movz(dst, hw2, 2);
            if (hw0 != 0) try self.movk(dst, hw0, 0);
            if (hw1 != 0) try self.movk(dst, hw1, 1);
            if (hw3 != 0) try self.movk(dst, hw3, 3);
        } else {
            try self.movz(dst, hw3, 3);
            if (hw0 != 0) try self.movk(dst, hw0, 0);
            if (hw1 != 0) try self.movk(dst, hw1, 1);
            if (hw2 != 0) try self.movk(dst, hw2, 2);
        }
    }

    /// MOVZ Xd, #imm16, LSL #shift*16
    fn movz(self: *Arm64Emitter, dst: Register, imm16: u16, shift: u2) !void {
        // MOVZ: 1 10 100101 hw imm16 Rd
        const inst: u32 = 0xD2800000 |
            (@as(u32, shift) << 21) |
            (@as(u32, imm16) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// MOVK Xd, #imm16, LSL #shift*16
    fn movk(self: *Arm64Emitter, dst: Register, imm16: u16, shift: u2) !void {
        // MOVK: 1 11 100101 hw imm16 Rd
        const inst: u32 = 0xF2800000 |
            (@as(u32, shift) << 21) |
            (@as(u32, imm16) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// LDR Xt, [Xn, #imm] (unsigned offset, scaled by 8)
    pub fn ldrImm(self: *Arm64Emitter, dst: Register, base: Register, offset_bytes: i32) !void {
        if (offset_bytes >= 0 and offset_bytes <= 32760 and @mod(offset_bytes, 8) == 0) {
            // Unsigned offset form: LDR Xt, [Xn, #pimm]
            const pimm: u12 = @intCast(@divExact(offset_bytes, 8));
            const inst: u32 = 0xF9400000 |
                (@as(u32, pimm) << 10) |
                (@as(u32, base.encode()) << 5) |
                @as(u32, dst.encode());
            try self.emit32(inst);
        } else if (offset_bytes >= -256 and offset_bytes <= 255) {
            // Unscaled immediate: LDUR Xt, [Xn, #simm9]
            const simm9: u9 = @bitCast(@as(i9, @intCast(offset_bytes)));
            const inst: u32 = 0xF8400000 |
                (@as(u32, simm9) << 12) |
                (@as(u32, base.encode()) << 5) |
                @as(u32, dst.encode());
            try self.emit32(inst);
        } else {
            // Need to compute address in temp register
            return error.OffsetTooLarge;
        }
    }

    /// LDR Wt, [Xn, #imm] (32-bit load, zero-extends to 64-bit, scaled by 4)
    /// Used for loading hidden_class_idx and other u32 values
    pub fn ldrImmW(self: *Arm64Emitter, dst: Register, base: Register, offset_bytes: i32) !void {
        if (offset_bytes >= 0 and offset_bytes <= 16380 and @mod(offset_bytes, 4) == 0) {
            // Unsigned offset form: LDR Wt, [Xn, #pimm]
            const pimm: u12 = @intCast(@divExact(offset_bytes, 4));
            const inst: u32 = 0xB9400000 | // size=10, opc=01 for 32-bit load
                (@as(u32, pimm) << 10) |
                (@as(u32, base.encode()) << 5) |
                @as(u32, dst.encode());
            try self.emit32(inst);
        } else if (offset_bytes >= -256 and offset_bytes <= 255) {
            // Unscaled immediate: LDUR Wt, [Xn, #simm9]
            const simm9: u9 = @bitCast(@as(i9, @intCast(offset_bytes)));
            const inst: u32 = 0xB8400000 |
                (@as(u32, simm9) << 12) |
                (@as(u32, base.encode()) << 5) |
                @as(u32, dst.encode());
            try self.emit32(inst);
        } else {
            return error.OffsetTooLarge;
        }
    }

    /// LDRH Xt, [Xn, #imm] (16-bit load, zero-extends to 64-bit, scaled by 2)
    /// Used for loading slot_offset from PIC entries
    pub fn ldrhImm(self: *Arm64Emitter, dst: Register, base: Register, offset_bytes: i32) !void {
        if (offset_bytes >= 0 and offset_bytes <= 8190 and @mod(offset_bytes, 2) == 0) {
            // Unsigned offset form: LDRH Wt, [Xn, #pimm]
            const pimm: u12 = @intCast(@divExact(offset_bytes, 2));
            const inst: u32 = 0x79400000 | // size=01, opc=01 for 16-bit load
                (@as(u32, pimm) << 10) |
                (@as(u32, base.encode()) << 5) |
                @as(u32, dst.encode());
            try self.emit32(inst);
        } else if (offset_bytes >= -256 and offset_bytes <= 255) {
            // Unscaled immediate: LDURH Wt, [Xn, #simm9]
            const simm9: u9 = @bitCast(@as(i9, @intCast(offset_bytes)));
            const inst: u32 = 0x78400000 |
                (@as(u32, simm9) << 12) |
                (@as(u32, base.encode()) << 5) |
                @as(u32, dst.encode());
            try self.emit32(inst);
        } else {
            return error.OffsetTooLarge;
        }
    }

    /// STR Xt, [Xn, #imm] (unsigned offset, scaled by 8)
    pub fn strImm(self: *Arm64Emitter, src: Register, base: Register, offset_bytes: i32) !void {
        if (offset_bytes >= 0 and offset_bytes <= 32760 and @mod(offset_bytes, 8) == 0) {
            const pimm: u12 = @intCast(@divExact(offset_bytes, 8));
            const inst: u32 = 0xF9000000 |
                (@as(u32, pimm) << 10) |
                (@as(u32, base.encode()) << 5) |
                @as(u32, src.encode());
            try self.emit32(inst);
        } else if (offset_bytes >= -256 and offset_bytes <= 255) {
            const simm9: u9 = @bitCast(@as(i9, @intCast(offset_bytes)));
            const inst: u32 = 0xF8000000 |
                (@as(u32, simm9) << 12) |
                (@as(u32, base.encode()) << 5) |
                @as(u32, src.encode());
            try self.emit32(inst);
        } else {
            return error.OffsetTooLarge;
        }
    }

    /// LDR Xt, [Xn], #imm (post-index)
    pub fn ldrPostIndex(self: *Arm64Emitter, dst: Register, base: Register, offset_bytes: i9) !void {
        const simm9: u9 = @bitCast(offset_bytes);
        const inst: u32 = 0xF8400400 |
            (@as(u32, simm9) << 12) |
            (@as(u32, base.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// STR Xt, [Xn, #imm]! (pre-index)
    pub fn strPreIndex(self: *Arm64Emitter, src: Register, base: Register, offset_bytes: i9) !void {
        const simm9: u9 = @bitCast(offset_bytes);
        const inst: u32 = 0xF8000C00 |
            (@as(u32, simm9) << 12) |
            (@as(u32, base.encode()) << 5) |
            @as(u32, src.encode());
        try self.emit32(inst);
    }

    /// STP Xt1, Xt2, [Xn, #imm]! (pre-index, for prologue)
    pub fn stpPreIndex(self: *Arm64Emitter, rt1: Register, rt2: Register, base: Register, offset_bytes: i32) !void {
        if (@mod(offset_bytes, 8) != 0 or offset_bytes < -512 or offset_bytes > 504) {
            return error.OffsetTooLarge;
        }
        const imm7: u7 = @bitCast(@as(i7, @intCast(@divExact(offset_bytes, 8))));
        const inst: u32 = 0xA9800000 |
            (@as(u32, imm7) << 15) |
            (@as(u32, rt2.encode()) << 10) |
            (@as(u32, base.encode()) << 5) |
            @as(u32, rt1.encode());
        try self.emit32(inst);
    }

    /// LDP Xt1, Xt2, [Xn], #imm (post-index, for epilogue)
    pub fn ldpPostIndex(self: *Arm64Emitter, rt1: Register, rt2: Register, base: Register, offset_bytes: i32) !void {
        if (@mod(offset_bytes, 8) != 0 or offset_bytes < -512 or offset_bytes > 504) {
            return error.OffsetTooLarge;
        }
        const imm7: u7 = @bitCast(@as(i7, @intCast(@divExact(offset_bytes, 8))));
        const inst: u32 = 0xA8C00000 |
            (@as(u32, imm7) << 15) |
            (@as(u32, rt2.encode()) << 10) |
            (@as(u32, base.encode()) << 5) |
            @as(u32, rt1.encode());
        try self.emit32(inst);
    }

    /// STP Xt1, Xt2, [Xn, #imm] (signed offset, no writeback)
    pub fn stpOffset(self: *Arm64Emitter, rt1: Register, rt2: Register, base: Register, offset_bytes: i32) !void {
        if (@mod(offset_bytes, 8) != 0 or offset_bytes < -512 or offset_bytes > 504) {
            return error.OffsetTooLarge;
        }
        const imm7: u7 = @bitCast(@as(i7, @intCast(@divExact(offset_bytes, 8))));
        const inst: u32 = 0xA9000000 |
            (@as(u32, imm7) << 15) |
            (@as(u32, rt2.encode()) << 10) |
            (@as(u32, base.encode()) << 5) |
            @as(u32, rt1.encode());
        try self.emit32(inst);
    }

    /// LDP Xt1, Xt2, [Xn, #imm] (signed offset, no writeback)
    pub fn ldpOffset(self: *Arm64Emitter, rt1: Register, rt2: Register, base: Register, offset_bytes: i32) !void {
        if (@mod(offset_bytes, 8) != 0 or offset_bytes < -512 or offset_bytes > 504) {
            return error.OffsetTooLarge;
        }
        const imm7: u7 = @bitCast(@as(i7, @intCast(@divExact(offset_bytes, 8))));
        const inst: u32 = 0xA9400000 |
            (@as(u32, imm7) << 15) |
            (@as(u32, rt2.encode()) << 10) |
            (@as(u32, base.encode()) << 5) |
            @as(u32, rt1.encode());
        try self.emit32(inst);
    }

    // ========================================
    // Arithmetic instructions
    // ========================================

    /// ADD Xd, Xn, Xm
    pub fn addRegReg(self: *Arm64Emitter, dst: Register, src1: Register, src2: Register) !void {
        const inst: u32 = 0x8B000000 |
            (@as(u32, src2.encode()) << 16) |
            (@as(u32, src1.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// ADD Xd, Xn, Xm, LSL #shift
    pub fn addRegRegShift(self: *Arm64Emitter, dst: Register, src1: Register, src2: Register, shift: u6) !void {
        const inst: u32 = 0x8B000000 |
            (@as(u32, shift) << 10) |
            (@as(u32, src2.encode()) << 16) |
            (@as(u32, src1.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// ADDS Xd, Xn, Xm (sets flags)
    pub fn addsRegReg(self: *Arm64Emitter, dst: Register, src1: Register, src2: Register) !void {
        const inst: u32 = 0xAB000000 |
            (@as(u32, src2.encode()) << 16) |
            (@as(u32, src1.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// ADD Xd, Xn, #imm12
    pub fn addRegImm12(self: *Arm64Emitter, dst: Register, src: Register, imm12: u12) !void {
        const inst: u32 = 0x91000000 |
            (@as(u32, imm12) << 10) |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// SUB Xd, Xn, Xm
    pub fn subRegReg(self: *Arm64Emitter, dst: Register, src1: Register, src2: Register) !void {
        const inst: u32 = 0xCB000000 |
            (@as(u32, src2.encode()) << 16) |
            (@as(u32, src1.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// SUBS Xd, Xn, Xm (sets flags)
    pub fn subsRegReg(self: *Arm64Emitter, dst: Register, src1: Register, src2: Register) !void {
        const inst: u32 = 0xEB000000 |
            (@as(u32, src2.encode()) << 16) |
            (@as(u32, src1.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// SUB Xd, Xn, #imm12
    pub fn subRegImm12(self: *Arm64Emitter, dst: Register, src: Register, imm12: u12) !void {
        const inst: u32 = 0xD1000000 |
            (@as(u32, imm12) << 10) |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// MUL Xd, Xn, Xm (alias for MADD Xd, Xn, Xm, XZR)
    pub fn mulRegReg(self: *Arm64Emitter, dst: Register, src1: Register, src2: Register) !void {
        const inst: u32 = 0x9B007C00 |
            (@as(u32, src2.encode()) << 16) |
            (@as(u32, src1.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// NEG Xd, Xm (alias for SUB Xd, XZR, Xm)
    pub fn negReg(self: *Arm64Emitter, dst: Register, src: Register) !void {
        const inst: u32 = 0xCB0003E0 |
            (@as(u32, src.encode()) << 16) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// NEGS Xd, Xm (alias for SUBS Xd, XZR, Xm)
    pub fn negsReg(self: *Arm64Emitter, dst: Register, src: Register) !void {
        const inst: u32 = 0xEB0003E0 |
            (@as(u32, src.encode()) << 16) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// ASR Xd, Xn, #imm (arithmetic shift right by immediate)
    pub fn asrRegImm(self: *Arm64Emitter, dst: Register, src: Register, imm: u6) !void {
        // SBFM Xd, Xn, #shift, #63 (ASR is an alias)
        // sf=1, opc=00, N=1, immr=shift, imms=63
        const inst: u32 = 0x9340FC00 |
            (@as(u32, imm) << 16) |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// LSL Xd, Xn, #imm (logical shift left by immediate)
    pub fn lslRegImm(self: *Arm64Emitter, dst: Register, src: Register, imm: u6) !void {
        // UBFM Xd, Xn, #(-shift MOD 64), #(63-shift) (LSL is an alias)
        // For 64-bit: sf=1, opc=10, N=1
        const shift = @as(u6, @intCast((@as(u7, 64) - @as(u7, imm)) & 63));
        const imms: u6 = 63 - imm;
        const inst: u32 = 0xD3400000 |
            (@as(u32, shift) << 16) |
            (@as(u32, imms) << 10) |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// LSR Xd, Xn, #imm (logical shift right by immediate)
    pub fn lsrRegImm(self: *Arm64Emitter, dst: Register, src: Register, imm: u6) !void {
        // UBFM Xd, Xn, #shift, #63 (LSR is an alias)
        const inst: u32 = 0xD340FC00 |
            (@as(u32, imm) << 16) |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// LSL Xd, Xn, Xm (logical shift left by register)
    pub fn lslRegReg(self: *Arm64Emitter, dst: Register, src: Register, shift_reg: Register) !void {
        // LSLV Xd, Xn, Xm
        const inst: u32 = 0x9AC02000 |
            (@as(u32, shift_reg.encode()) << 16) |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// ASR Xd, Xn, Xm (arithmetic shift right by register)
    pub fn asrRegReg(self: *Arm64Emitter, dst: Register, src: Register, shift_reg: Register) !void {
        // ASRV Xd, Xn, Xm
        const inst: u32 = 0x9AC02800 |
            (@as(u32, shift_reg.encode()) << 16) |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// LSR Xd, Xn, Xm (logical shift right by register)
    pub fn lsrRegReg(self: *Arm64Emitter, dst: Register, src: Register, shift_reg: Register) !void {
        // LSRV Xd, Xn, Xm
        const inst: u32 = 0x9AC02400 |
            (@as(u32, shift_reg.encode()) << 16) |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// AND Xd, Xn, #mask (AND with immediate)
    /// Note: ARM64 AND immediate uses a bitmask encoding, this only supports low-bit masks
    /// like 0x1, 0x3, 0x7, 0xF, 0x1F.
    pub fn andRegImm(self: *Arm64Emitter, dst: Register, src: Register, mask: u6) !void {
        std.debug.assert(mask != 0);
        const mask_u7: u7 = mask;
        std.debug.assert((mask_u7 & (mask_u7 + 1)) == 0);
        const ones: u6 = @intCast(@popCount(mask));
        const imms: u6 = ones - 1;
        const inst: u32 = 0x92400000 |
            (@as(u32, imms) << 10) |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    // ========================================
    // Comparison instructions
    // ========================================

    /// CMP Xn, Xm (alias for SUBS XZR, Xn, Xm)
    pub fn cmpRegReg(self: *Arm64Emitter, rn: Register, rm: Register) !void {
        const inst: u32 = 0xEB00001F |
            (@as(u32, rm.encode()) << 16) |
            (@as(u32, rn.encode()) << 5);
        try self.emit32(inst);
    }

    /// CMP Xn, #imm12
    pub fn cmpRegImm12(self: *Arm64Emitter, rn: Register, imm12: u12) !void {
        const inst: u32 = 0xF100001F |
            (@as(u32, imm12) << 10) |
            (@as(u32, rn.encode()) << 5);
        try self.emit32(inst);
    }

    /// CSEL Xd, Xn, Xm, cond (conditional select)
    /// If cond is true, Xd = Xn; else Xd = Xm
    pub fn csel(self: *Arm64Emitter, dst: Register, src_true: Register, src_false: Register, cond: Condition) !void {
        const inst: u32 = 0x9A800000 |
            (@as(u32, src_false.encode()) << 16) |
            (@as(u32, @intFromEnum(cond)) << 12) |
            (@as(u32, src_true.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    // ========================================
    // Bitwise instructions
    // ========================================

    /// AND Xd, Xn, Xm
    pub fn andRegReg(self: *Arm64Emitter, dst: Register, src1: Register, src2: Register) !void {
        const inst: u32 = 0x8A000000 |
            (@as(u32, src2.encode()) << 16) |
            (@as(u32, src1.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// ORR Xd, Xn, Xm
    pub fn orrRegReg(self: *Arm64Emitter, dst: Register, src1: Register, src2: Register) !void {
        const inst: u32 = 0xAA000000 |
            (@as(u32, src2.encode()) << 16) |
            (@as(u32, src1.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// EOR Xd, Xn, Xm
    pub fn eorRegReg(self: *Arm64Emitter, dst: Register, src1: Register, src2: Register) !void {
        const inst: u32 = 0xCA000000 |
            (@as(u32, src2.encode()) << 16) |
            (@as(u32, src1.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// MVN Xd, Xm (alias for ORN Xd, XZR, Xm)
    pub fn mvnReg(self: *Arm64Emitter, dst: Register, src: Register) !void {
        const inst: u32 = 0xAA2003E0 |
            (@as(u32, src.encode()) << 16) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    // ========================================
    // Control flow
    // ========================================

    /// B label (PC-relative, +/- 128MB)
    pub fn b(self: *Arm64Emitter, offset_bytes: i32) !void {
        if (@mod(offset_bytes, 4) != 0) return error.UnalignedBranch;
        const imm26: i26 = @intCast(@divExact(offset_bytes, 4));
        const inst: u32 = 0x14000000 | @as(u32, @as(u26, @bitCast(imm26)));
        try self.emit32(inst);
    }

    /// B.cond label (PC-relative, +/- 1MB)
    pub fn bcond(self: *Arm64Emitter, cond: Condition, offset_bytes: i32) !void {
        if (@mod(offset_bytes, 4) != 0) return error.UnalignedBranch;
        const imm19: i19 = @intCast(@divExact(offset_bytes, 4));
        const inst: u32 = 0x54000000 |
            (@as(u32, @as(u19, @bitCast(imm19))) << 5) |
            @as(u32, @intFromEnum(cond));
        try self.emit32(inst);
    }

    /// BL label (branch with link, for calls)
    pub fn bl(self: *Arm64Emitter, offset_bytes: i32) !void {
        if (@mod(offset_bytes, 4) != 0) return error.UnalignedBranch;
        const imm26: i26 = @intCast(@divExact(offset_bytes, 4));
        const inst: u32 = 0x94000000 | @as(u32, @as(u26, @bitCast(imm26)));
        try self.emit32(inst);
    }

    /// BLR Xn (indirect call)
    pub fn blr(self: *Arm64Emitter, target: Register) !void {
        const inst: u32 = 0xD63F0000 | (@as(u32, target.encode()) << 5);
        try self.emit32(inst);
    }

    /// RET (return via x30/lr)
    pub fn ret(self: *Arm64Emitter) !void {
        try self.emit32(0xD65F03C0); // RET x30
    }

    /// RET Xn (return via specified register)
    pub fn retReg(self: *Arm64Emitter, target: Register) !void {
        const inst: u32 = 0xD65F0000 | (@as(u32, target.encode()) << 5);
        try self.emit32(inst);
    }

    // ========================================
    // Stack operations (using STP/LDP)
    // ========================================

    /// Push a single register (SUB sp, sp, #16; STR Xn, [sp])
    pub fn pushReg(self: *Arm64Emitter, reg: Register) !void {
        // STR Xn, [sp, #-16]!
        try self.strPreIndex(reg, Register.x29, -16);
    }

    /// Pop a single register (LDR Xn, [sp], #16)
    pub fn popReg(self: *Arm64Emitter, reg: Register) !void {
        try self.ldrPostIndex(reg, Register.x29, 16);
    }

    // ========================================
    // Floating-Point Arithmetic (Scalar Double)
    // ========================================

    /// FADD Dd, Dn, Dm (scalar double-precision add)
    /// Encoding: 0 0 0 11110 01 1 Rm 001010 Rn Rd
    pub fn faddDouble(self: *Arm64Emitter, dst: Register, src1: Register, src2: Register) !void {
        const inst: u32 = 0x1E602800 |
            (@as(u32, src2.encode()) << 16) |
            (@as(u32, src1.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// FSUB Dd, Dn, Dm (scalar double-precision subtract)
    /// Encoding: 0 0 0 11110 01 1 Rm 001110 Rn Rd
    pub fn fsubDouble(self: *Arm64Emitter, dst: Register, src1: Register, src2: Register) !void {
        const inst: u32 = 0x1E603800 |
            (@as(u32, src2.encode()) << 16) |
            (@as(u32, src1.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// FMUL Dd, Dn, Dm (scalar double-precision multiply)
    /// Encoding: 0 0 0 11110 01 1 Rm 000010 Rn Rd
    pub fn fmulDouble(self: *Arm64Emitter, dst: Register, src1: Register, src2: Register) !void {
        const inst: u32 = 0x1E600800 |
            (@as(u32, src2.encode()) << 16) |
            (@as(u32, src1.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// FDIV Dd, Dn, Dm (scalar double-precision divide)
    /// Encoding: 0 0 0 11110 01 1 Rm 000110 Rn Rd
    pub fn fdivDouble(self: *Arm64Emitter, dst: Register, src1: Register, src2: Register) !void {
        const inst: u32 = 0x1E601800 |
            (@as(u32, src2.encode()) << 16) |
            (@as(u32, src1.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// FMIN Dd, Dn, Dm (scalar double-precision minimum)
    /// Encoding: 0 0 0 11110 01 1 Rm 010110 Rn Rd
    pub fn fminDouble(self: *Arm64Emitter, dst: Register, src1: Register, src2: Register) !void {
        const inst: u32 = 0x1E605800 |
            (@as(u32, src2.encode()) << 16) |
            (@as(u32, src1.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// FMAX Dd, Dn, Dm (scalar double-precision maximum)
    /// Encoding: 0 0 0 11110 01 1 Rm 010010 Rn Rd
    pub fn fmaxDouble(self: *Arm64Emitter, dst: Register, src1: Register, src2: Register) !void {
        const inst: u32 = 0x1E604800 |
            (@as(u32, src2.encode()) << 16) |
            (@as(u32, src1.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// FMOV Dd, Xn (move 64-bit GP register to double FP register)
    /// Encoding: 1 0 0 11110 01 1 00111 000000 Rn Rd
    pub fn fmovDoubleFromGpr(self: *Arm64Emitter, dst: Register, src: Register) !void {
        const inst: u32 = 0x9E670000 |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// FMOV Xd, Dn (move double FP register to 64-bit GP register)
    /// Encoding: 1 0 0 11110 01 1 00110 000000 Rn Rd
    pub fn fmovGprFromDouble(self: *Arm64Emitter, dst: Register, src: Register) !void {
        const inst: u32 = 0x9E660000 |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// SCVTF Dd, Xn (convert signed 64-bit integer to double)
    /// Encoding: 1 0 0 11110 01 1 00010 000000 Rn Rd
    pub fn scvtfDoubleFromGpr(self: *Arm64Emitter, dst: Register, src: Register) !void {
        const inst: u32 = 0x9E620000 |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// FCVTZS Xd, Dn (convert double to signed 64-bit integer, round toward zero)
    /// Encoding: 1 0 0 11110 01 1 11000 000000 Rn Rd
    pub fn fcvtzsGprFromDouble(self: *Arm64Emitter, dst: Register, src: Register) !void {
        const inst: u32 = 0x9E780000 |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// FCVT Sd, Dn (convert double to single)
    /// Encoding: 0 0 0 11110 01 1 00100 10000 Rn Rd
    pub fn fcvtSingleFromDouble(self: *Arm64Emitter, dst: Register, src: Register) !void {
        const inst: u32 = 0x1E624000 |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// FCVT Dd, Sn (convert single to double)
    /// Encoding: 0 0 0 11110 00 1 00101 10000 Rn Rd
    pub fn fcvtDoubleFromSingle(self: *Arm64Emitter, dst: Register, src: Register) !void {
        const inst: u32 = 0x1E22C000 |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// FMOV Sd, Wn (move 32-bit GP register to single FP register)
    /// Encoding: 0 0 0 11110 00 1 00111 000000 Rn Rd
    pub fn fmovSingleFromGpr32(self: *Arm64Emitter, dst: Register, src: Register) !void {
        const inst: u32 = 0x1E270000 |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// FMOV Wd, Sn (move single FP register to 32-bit GP register)
    /// Encoding: 0 0 0 11110 00 1 00110 000000 Rn Rd
    pub fn fmovGpr32FromSingle(self: *Arm64Emitter, dst: Register, src: Register) !void {
        const inst: u32 = 0x1E260000 |
            (@as(u32, src.encode()) << 5) |
            @as(u32, dst.encode());
        try self.emit32(inst);
    }

    /// FCMP Dn, Dm (compare two double FP registers, sets NZCV flags)
    /// Encoding: 0 0 0 11110 01 1 Rm 00 1000 Rn 00 000
    pub fn fcmpDouble(self: *Arm64Emitter, rn: Register, rm: Register) !void {
        const inst: u32 = 0x1E602000 |
            (@as(u32, rm.encode()) << 16) |
            (@as(u32, rn.encode()) << 5);
        try self.emit32(inst);
    }

    // ========================================
    // Miscellaneous
    // ========================================

    /// NOP
    pub fn nop(self: *Arm64Emitter) !void {
        try self.emit32(0xD503201F);
    }

    /// BRK #imm16 (breakpoint)
    pub fn brk(self: *Arm64Emitter, imm16: u16) !void {
        const inst: u32 = 0xD4200000 | (@as(u32, imm16) << 5);
        try self.emit32(inst);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "Arm64Emitter: mov reg, reg" {
    const testing = std.testing;
    var emitter = Arm64Emitter.init(testing.allocator);
    defer emitter.deinit();

    try emitter.movRegReg(.x0, .x1);
    const code = emitter.getCode();

    try testing.expectEqual(@as(usize, 4), code.len);
    // MOV X0, X1 = ORR X0, XZR, X1 = 0xAA0103E0
    try testing.expectEqual(@as(u32, 0xAA0103E0), @as(u32, @bitCast(code[0..4].*)));
}

test "Arm64Emitter: mov reg, imm16" {
    const testing = std.testing;
    var emitter = Arm64Emitter.init(testing.allocator);
    defer emitter.deinit();

    try emitter.movRegImm64(.x0, 42);
    const code = emitter.getCode();

    try testing.expectEqual(@as(usize, 4), code.len);
    // MOVZ X0, #42 = 0xD2800540
    try testing.expectEqual(@as(u32, 0xD2800540), @as(u32, @bitCast(code[0..4].*)));
}

test "Arm64Emitter: mov reg, imm64" {
    const testing = std.testing;
    var emitter = Arm64Emitter.init(testing.allocator);
    defer emitter.deinit();

    try emitter.movRegImm64(.x0, 0x123456789ABCDEF0);
    const code = emitter.getCode();

    // Should generate MOVZ + 3 MOVK instructions = 16 bytes
    try testing.expectEqual(@as(usize, 16), code.len);
}

test "Arm64Emitter: add and sub" {
    const testing = std.testing;
    var emitter = Arm64Emitter.init(testing.allocator);
    defer emitter.deinit();

    try emitter.addRegReg(.x0, .x1, .x2);
    try emitter.subRegReg(.x3, .x4, .x5);
    const code = emitter.getCode();

    try testing.expectEqual(@as(usize, 8), code.len);
}

test "Arm64Emitter: branch and return" {
    const testing = std.testing;
    var emitter = Arm64Emitter.init(testing.allocator);
    defer emitter.deinit();

    try emitter.b(0);
    try emitter.bcond(.eq, 8);
    try emitter.ret();
    const code = emitter.getCode();

    try testing.expectEqual(@as(usize, 12), code.len);
    // RET = 0xD65F03C0
    try testing.expectEqual(@as(u32, 0xD65F03C0), @as(u32, @bitCast(code[8..12].*)));
}

test "Arm64Emitter: load and store" {
    const testing = std.testing;
    var emitter = Arm64Emitter.init(testing.allocator);
    defer emitter.deinit();

    try emitter.ldrImm(.x0, .x1, 0);
    try emitter.strImm(.x2, .x3, 8);
    const code = emitter.getCode();

    try testing.expectEqual(@as(usize, 8), code.len);
}

test "Arm64Emitter: stp and ldp" {
    const testing = std.testing;
    var emitter = Arm64Emitter.init(testing.allocator);
    defer emitter.deinit();

    // Standard prologue pattern
    try emitter.stpPreIndex(.x29, .x30, .sp, -16);
    // Standard epilogue pattern
    try emitter.ldpPostIndex(.x29, .x30, .sp, 16);

    const code = emitter.getCode();
    try testing.expectEqual(@as(usize, 8), code.len);
}

test "Arm64Emitter: floating-point arithmetic" {
    const testing = std.testing;
    var emitter = Arm64Emitter.init(testing.allocator);
    defer emitter.deinit();

    // Using x0-x3 as FP register aliases (d0-d3)
    // FADD D0, D1, D2
    try emitter.faddDouble(.x0, .x1, .x2);
    // FSUB D0, D1, D2
    try emitter.fsubDouble(.x0, .x1, .x2);
    // FMUL D0, D1, D2
    try emitter.fmulDouble(.x0, .x1, .x2);
    // FDIV D0, D1, D2
    try emitter.fdivDouble(.x0, .x1, .x2);

    const code = emitter.getCode();
    try testing.expectEqual(@as(usize, 16), code.len);

    // FADD D0, D1, D2: 0x1E622820
    try testing.expectEqual(@as(u32, 0x1E622820), @as(u32, @bitCast(code[0..4].*)));
    // FSUB D0, D1, D2: 0x1E623820
    try testing.expectEqual(@as(u32, 0x1E623820), @as(u32, @bitCast(code[4..8].*)));
    // FMUL D0, D1, D2: 0x1E620820
    try testing.expectEqual(@as(u32, 0x1E620820), @as(u32, @bitCast(code[8..12].*)));
    // FDIV D0, D1, D2: 0x1E621820
    try testing.expectEqual(@as(u32, 0x1E621820), @as(u32, @bitCast(code[12..16].*)));
}
