//! x86-64 Code Emitter for JIT Compilation
//!
//! Provides low-level x86-64 instruction encoding for the baseline JIT compiler.
//! Uses System V AMD64 ABI calling convention (used on macOS and Linux).
//!
//! Register allocation strategy:
//! - rdi: Context* (first argument, preserved across calls)
//! - rbx: JS stack pointer (callee-saved)
//! - rax, rcx, rdx, r8-r11: scratch registers
//! - rbp, r12-r15: callee-saved (available for locals)

const std = @import("std");

/// x86-64 general-purpose registers
pub const Register = enum(u4) {
    rax = 0,
    rcx = 1,
    rdx = 2,
    rbx = 3,
    rsp = 4,
    rbp = 5,
    rsi = 6,
    rdi = 7,
    r8 = 8,
    r9 = 9,
    r10 = 10,
    r11 = 11,
    r12 = 12,
    r13 = 13,
    r14 = 14,
    r15 = 15,

    /// Returns true if this is an extended register (r8-r15)
    pub fn isExtended(self: Register) bool {
        return @intFromEnum(self) >= 8;
    }

    /// Returns the low 3 bits of the register encoding
    pub fn low3(self: Register) u3 {
        return @truncate(@intFromEnum(self));
    }
};

/// Register assignments for JIT code
pub const Regs = struct {
    /// Context pointer - always in rdi (first argument)
    pub const ctx = Register.rdi;
    /// JS value stack pointer (callee-saved)
    pub const js_sp = Register.rbx;
    /// Primary accumulator for return values and arithmetic
    pub const acc = Register.rax;
    /// Secondary scratch register
    pub const tmp1 = Register.rcx;
    /// Third scratch register
    pub const tmp2 = Register.rdx;
    /// Fourth scratch register
    pub const tmp3 = Register.r8;
    /// Fifth scratch register
    pub const tmp4 = Register.r9;
};

/// x86-64 instruction emitter
pub const X86Emitter = struct {
    buffer: std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) X86Emitter {
        return .{
            .buffer = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *X86Emitter) void {
        self.buffer.deinit(self.allocator);
    }

    /// Get the emitted code as a slice
    pub fn getCode(self: *const X86Emitter) []const u8 {
        return self.buffer.items;
    }

    /// Get current code offset (for patching)
    pub fn offset(self: *const X86Emitter) usize {
        return self.buffer.items.len;
    }

    /// Reserve space and return offset for later patching
    pub fn reserve(self: *X86Emitter, n: usize) !usize {
        const off = self.offset();
        try self.buffer.appendNTimes(self.allocator, 0, n);
        return off;
    }

    /// Patch a 32-bit value at the given offset
    pub fn patch32(self: *X86Emitter, off: usize, value: i32) void {
        const bytes = @as([4]u8, @bitCast(value));
        @memcpy(self.buffer.items[off..][0..4], &bytes);
    }

    // ========================================
    // REX prefix helpers
    // ========================================

    /// Emit REX prefix if needed
    /// W: 64-bit operand size
    /// R: extends ModRM.reg
    /// X: extends SIB.index
    /// B: extends ModRM.rm or SIB.base
    fn emitRex(self: *X86Emitter, w: bool, r: bool, x: bool, b: bool) !void {
        const rex: u8 = 0x40 |
            (@as(u8, @intFromBool(w)) << 3) |
            (@as(u8, @intFromBool(r)) << 2) |
            (@as(u8, @intFromBool(x)) << 1) |
            @as(u8, @intFromBool(b));
        if (rex != 0x40) {
            try self.buffer.append(self.allocator, rex);
        }
    }

    /// Emit REX.W prefix for 64-bit operations
    fn emitRexW(self: *X86Emitter, reg: Register, rm: Register) !void {
        try self.emitRex(true, reg.isExtended(), false, rm.isExtended());
    }

    // ========================================
    // ModRM and SIB encoding
    // ========================================

    /// Emit ModRM byte
    /// mod: addressing mode (0=indirect, 1=disp8, 2=disp32, 3=register)
    /// reg: register or opcode extension
    /// rm: register or memory operand
    fn emitModRM(self: *X86Emitter, mod: u2, reg: u3, rm: u3) !void {
        try self.buffer.append(self.allocator, (@as(u8, mod) << 6) | (@as(u8, reg) << 3) | rm);
    }

    /// Emit SIB byte (Scale-Index-Base)
    fn emitSIB(self: *X86Emitter, scale: u2, index: u3, base: u3) !void {
        try self.buffer.append(self.allocator, (@as(u8, scale) << 6) | (@as(u8, index) << 3) | base);
    }

    // ========================================
    // Data movement instructions
    // ========================================

    /// MOV r64, imm64 (absolute 64-bit immediate)
    pub fn movRegImm64(self: *X86Emitter, dst: Register, imm: u64) !void {
        // REX.W + B8+rd + imm64
        try self.emitRex(true, false, false, dst.isExtended());
        try self.buffer.append(self.allocator, 0xB8 + @as(u8, dst.low3()));
        try self.buffer.appendSlice(self.allocator, &@as([8]u8, @bitCast(imm)));
    }

    /// MOV r64, imm32 (sign-extended)
    pub fn movRegImm32(self: *X86Emitter, dst: Register, imm: i32) !void {
        // REX.W + C7 /0 + imm32
        try self.emitRex(true, false, false, dst.isExtended());
        try self.buffer.append(self.allocator, 0xC7);
        try self.emitModRM(0b11, 0, dst.low3());
        try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(imm)));
    }

    /// MOV r64, r64
    pub fn movRegReg(self: *X86Emitter, dst: Register, src: Register) !void {
        // REX.W + 89 /r (MOV r/m64, r64)
        try self.emitRexW(src, dst);
        try self.buffer.append(self.allocator, 0x89);
        try self.emitModRM(0b11, src.low3(), dst.low3());
    }

    /// MOV r32, r32 (zero-extends into r64)
    pub fn movRegReg32(self: *X86Emitter, dst: Register, src: Register) !void {
        try self.emitRex(false, src.isExtended(), false, dst.isExtended());
        try self.buffer.append(self.allocator, 0x89);
        try self.emitModRM(0b11, src.low3(), dst.low3());
    }

    /// MOVSXD r64, r/m32 (sign-extend 32-bit to 64-bit)
    pub fn movsxdRegReg(self: *X86Emitter, dst: Register, src: Register) !void {
        try self.emitRex(true, dst.isExtended(), false, src.isExtended());
        try self.buffer.append(self.allocator, 0x63);
        try self.emitModRM(0b11, dst.low3(), src.low3());
    }

    /// MOV r64, [base + offset]
    pub fn movRegMem(self: *X86Emitter, dst: Register, base: Register, disp: i32) !void {
        try self.emitRexW(dst, base);
        try self.buffer.append(self.allocator, 0x8B); // MOV r64, r/m64

        if (base == .rsp or base == .r12) {
            // RSP/R12 require SIB byte
            if (disp == 0 and base != .rbp and base != .r13) {
                try self.emitModRM(0b00, dst.low3(), 0b100);
                try self.emitSIB(0, 0b100, base.low3());
            } else if (disp >= -128 and disp <= 127) {
                try self.emitModRM(0b01, dst.low3(), 0b100);
                try self.emitSIB(0, 0b100, base.low3());
                try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(disp))));
            } else {
                try self.emitModRM(0b10, dst.low3(), 0b100);
                try self.emitSIB(0, 0b100, base.low3());
                try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
            }
        } else if (base == .rbp or base == .r13) {
            // RBP/R13 always need displacement
            if (disp >= -128 and disp <= 127) {
                try self.emitModRM(0b01, dst.low3(), base.low3());
                try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(disp))));
            } else {
                try self.emitModRM(0b10, dst.low3(), base.low3());
                try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
            }
        } else {
            if (disp == 0) {
                try self.emitModRM(0b00, dst.low3(), base.low3());
            } else if (disp >= -128 and disp <= 127) {
                try self.emitModRM(0b01, dst.low3(), base.low3());
                try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(disp))));
            } else {
                try self.emitModRM(0b10, dst.low3(), base.low3());
                try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
            }
        }
    }

    /// MOV r32, [base + offset] (32-bit load, zero-extends to 64-bit)
    /// No REX.W prefix = 32-bit operand size
    pub fn movRegMem32(self: *X86Emitter, dst: Register, base: Register, disp: i32) !void {
        // No REX.W for 32-bit, but need REX for extended registers
        if (dst.isExtended() or base.isExtended()) {
            const rex: u8 = 0x40 | (if (dst.isExtended()) @as(u8, 4) else 0) | (if (base.isExtended()) @as(u8, 1) else 0);
            try self.buffer.append(self.allocator, rex);
        }
        try self.buffer.append(self.allocator, 0x8B); // MOV r32, r/m32

        // ModRM/SIB/disp encoding same as movRegMem
        if (base == .rsp or base == .r12) {
            if (disp == 0 and base != .rbp and base != .r13) {
                try self.emitModRM(0b00, dst.low3(), 0b100);
                try self.emitSIB(0, 0b100, base.low3());
            } else if (disp >= -128 and disp <= 127) {
                try self.emitModRM(0b01, dst.low3(), 0b100);
                try self.emitSIB(0, 0b100, base.low3());
                try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(disp))));
            } else {
                try self.emitModRM(0b10, dst.low3(), 0b100);
                try self.emitSIB(0, 0b100, base.low3());
                try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
            }
        } else if (base == .rbp or base == .r13) {
            if (disp >= -128 and disp <= 127) {
                try self.emitModRM(0b01, dst.low3(), base.low3());
                try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(disp))));
            } else {
                try self.emitModRM(0b10, dst.low3(), base.low3());
                try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
            }
        } else {
            if (disp == 0) {
                try self.emitModRM(0b00, dst.low3(), base.low3());
            } else if (disp >= -128 and disp <= 127) {
                try self.emitModRM(0b01, dst.low3(), base.low3());
                try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(disp))));
            } else {
                try self.emitModRM(0b10, dst.low3(), base.low3());
                try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
            }
        }
    }

    /// MOVZX r64, word [base + offset] (zero-extend 16-bit load to 64-bit)
    /// Used for loading slot_offset from PIC entries
    pub fn movzxRegMem16(self: *X86Emitter, dst: Register, base: Register, disp: i32) !void {
        // REX.W for 64-bit destination, plus R/B for extended registers
        try self.emitRex(true, dst.isExtended(), false, base.isExtended());
        try self.buffer.append(self.allocator, 0x0F);
        try self.buffer.append(self.allocator, 0xB7); // MOVZX r64, r/m16

        // ModRM/SIB/disp encoding same pattern as movRegMem
        if (base == .rsp or base == .r12) {
            if (disp == 0 and base != .rbp and base != .r13) {
                try self.emitModRM(0b00, dst.low3(), 0b100);
                try self.emitSIB(0, 0b100, base.low3());
            } else if (disp >= -128 and disp <= 127) {
                try self.emitModRM(0b01, dst.low3(), 0b100);
                try self.emitSIB(0, 0b100, base.low3());
                try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(disp))));
            } else {
                try self.emitModRM(0b10, dst.low3(), 0b100);
                try self.emitSIB(0, 0b100, base.low3());
                try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
            }
        } else if (base == .rbp or base == .r13) {
            if (disp >= -128 and disp <= 127) {
                try self.emitModRM(0b01, dst.low3(), base.low3());
                try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(disp))));
            } else {
                try self.emitModRM(0b10, dst.low3(), base.low3());
                try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
            }
        } else {
            if (disp == 0) {
                try self.emitModRM(0b00, dst.low3(), base.low3());
            } else if (disp >= -128 and disp <= 127) {
                try self.emitModRM(0b01, dst.low3(), base.low3());
                try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(disp))));
            } else {
                try self.emitModRM(0b10, dst.low3(), base.low3());
                try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
            }
        }
    }

    /// MOV [base + offset], r64
    pub fn movMemReg(self: *X86Emitter, base: Register, disp: i32, src: Register) !void {
        try self.emitRexW(src, base);
        try self.buffer.append(self.allocator, 0x89); // MOV r/m64, r64

        if (base == .rsp or base == .r12) {
            if (disp == 0 and base != .rbp and base != .r13) {
                try self.emitModRM(0b00, src.low3(), 0b100);
                try self.emitSIB(0, 0b100, base.low3());
            } else if (disp >= -128 and disp <= 127) {
                try self.emitModRM(0b01, src.low3(), 0b100);
                try self.emitSIB(0, 0b100, base.low3());
                try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(disp))));
            } else {
                try self.emitModRM(0b10, src.low3(), 0b100);
                try self.emitSIB(0, 0b100, base.low3());
                try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
            }
        } else if (base == .rbp or base == .r13) {
            if (disp >= -128 and disp <= 127) {
                try self.emitModRM(0b01, src.low3(), base.low3());
                try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(disp))));
            } else {
                try self.emitModRM(0b10, src.low3(), base.low3());
                try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
            }
        } else {
            if (disp == 0) {
                try self.emitModRM(0b00, src.low3(), base.low3());
            } else if (disp >= -128 and disp <= 127) {
                try self.emitModRM(0b01, src.low3(), base.low3());
                try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(disp))));
            } else {
                try self.emitModRM(0b10, src.low3(), base.low3());
                try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
            }
        }
    }

    /// LEA r64, [base + index*scale + disp]
    pub fn leaRegMem(self: *X86Emitter, dst: Register, base: Register, index: Register, scale: u2, disp: i32) !void {
        // Use SIB addressing. Index cannot be RSP.
        std.debug.assert(index != .rsp);

        try self.emitRex(true, dst.isExtended(), index.isExtended(), base.isExtended());
        try self.buffer.append(self.allocator, 0x8D); // LEA

        const needs_disp = disp != 0 or base == .rbp or base == .r13;
        const disp_fits_i8 = disp >= -128 and disp <= 127;
        const mod: u2 = if (!needs_disp) 0b00 else if (disp_fits_i8) 0b01 else 0b10;

        // ModRM: reg = dst, rm = SIB
        try self.emitModRM(mod, dst.low3(), 0b100);
        // SIB: scale, index, base
        try self.emitSIB(scale, index.low3(), base.low3());

        if (mod == 0b01) {
            try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(disp))));
        } else if (mod == 0b10 or (mod == 0b00 and needs_disp)) {
            // For base=rbp/r13 with mod=00, force disp32=0
            try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
        }
    }

    // ========================================
    // Arithmetic instructions
    // ========================================

    /// ADD r64, r64
    pub fn addRegReg(self: *X86Emitter, dst: Register, src: Register) !void {
        try self.emitRexW(src, dst);
        try self.buffer.append(self.allocator, 0x01); // ADD r/m64, r64
        try self.emitModRM(0b11, src.low3(), dst.low3());
    }

    /// ADD r64, imm32 (sign-extended)
    pub fn addRegImm32(self: *X86Emitter, dst: Register, imm: i32) !void {
        try self.emitRex(true, false, false, dst.isExtended());
        if (imm >= -128 and imm <= 127) {
            try self.buffer.append(self.allocator, 0x83); // ADD r/m64, imm8
            try self.emitModRM(0b11, 0, dst.low3());
            try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(imm))));
        } else {
            try self.buffer.append(self.allocator, 0x81); // ADD r/m64, imm32
            try self.emitModRM(0b11, 0, dst.low3());
            try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(imm)));
        }
    }

    /// SUB r64, r64
    pub fn subRegReg(self: *X86Emitter, dst: Register, src: Register) !void {
        try self.emitRexW(src, dst);
        try self.buffer.append(self.allocator, 0x29); // SUB r/m64, r64
        try self.emitModRM(0b11, src.low3(), dst.low3());
    }

    /// SUB r64, imm32
    pub fn subRegImm32(self: *X86Emitter, dst: Register, imm: i32) !void {
        try self.emitRex(true, false, false, dst.isExtended());
        if (imm >= -128 and imm <= 127) {
            try self.buffer.append(self.allocator, 0x83); // SUB r/m64, imm8
            try self.emitModRM(0b11, 5, dst.low3());
            try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(imm))));
        } else {
            try self.buffer.append(self.allocator, 0x81); // SUB r/m64, imm32
            try self.emitModRM(0b11, 5, dst.low3());
            try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(imm)));
        }
    }

    /// IMUL r64, r64
    pub fn imulRegReg(self: *X86Emitter, dst: Register, src: Register) !void {
        try self.emitRexW(dst, src);
        try self.buffer.append(self.allocator, 0x0F);
        try self.buffer.append(self.allocator, 0xAF); // IMUL r64, r/m64
        try self.emitModRM(0b11, dst.low3(), src.low3());
    }

    /// NEG r64
    pub fn negReg(self: *X86Emitter, reg: Register) !void {
        try self.emitRex(true, false, false, reg.isExtended());
        try self.buffer.append(self.allocator, 0xF7); // NEG r/m64
        try self.emitModRM(0b11, 3, reg.low3());
    }

    // ========================================
    // Comparison and test
    // ========================================

    /// CMP r64, r64
    pub fn cmpRegReg(self: *X86Emitter, a: Register, b: Register) !void {
        try self.emitRexW(b, a);
        try self.buffer.append(self.allocator, 0x39); // CMP r/m64, r64
        try self.emitModRM(0b11, b.low3(), a.low3());
    }

    /// CMP r32, r32 (32-bit compare, useful for hidden class index comparison)
    pub fn cmpRegReg32(self: *X86Emitter, a: Register, b: Register) !void {
        // No REX.W for 32-bit, but need REX for extended registers
        if (a.isExtended() or b.isExtended()) {
            const rex: u8 = 0x40 | (if (b.isExtended()) @as(u8, 4) else 0) | (if (a.isExtended()) @as(u8, 1) else 0);
            try self.buffer.append(self.allocator, rex);
        }
        try self.buffer.append(self.allocator, 0x39); // CMP r/m32, r32
        try self.emitModRM(0b11, b.low3(), a.low3());
    }

    /// CMP r64, imm32
    pub fn cmpRegImm32(self: *X86Emitter, reg: Register, imm: i32) !void {
        try self.emitRex(true, false, false, reg.isExtended());
        if (imm >= -128 and imm <= 127) {
            try self.buffer.append(self.allocator, 0x83); // CMP r/m64, imm8
            try self.emitModRM(0b11, 7, reg.low3());
            try self.buffer.append(self.allocator, @bitCast(@as(i8, @intCast(imm))));
        } else {
            try self.buffer.append(self.allocator, 0x81); // CMP r/m64, imm32
            try self.emitModRM(0b11, 7, reg.low3());
            try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(imm)));
        }
    }

    /// TEST r64, r64
    pub fn testRegReg(self: *X86Emitter, a: Register, b: Register) !void {
        try self.emitRexW(b, a);
        try self.buffer.append(self.allocator, 0x85); // TEST r/m64, r64
        try self.emitModRM(0b11, b.low3(), a.low3());
    }

    // ========================================
    // Bitwise operations
    // ========================================

    /// AND r64, r64
    pub fn andRegReg(self: *X86Emitter, dst: Register, src: Register) !void {
        try self.emitRexW(src, dst);
        try self.buffer.append(self.allocator, 0x21); // AND r/m64, r64
        try self.emitModRM(0b11, src.low3(), dst.low3());
    }

    /// OR r64, r64
    pub fn orRegReg(self: *X86Emitter, dst: Register, src: Register) !void {
        try self.emitRexW(src, dst);
        try self.buffer.append(self.allocator, 0x09); // OR r/m64, r64
        try self.emitModRM(0b11, src.low3(), dst.low3());
    }

    /// XOR r64, r64
    pub fn xorRegReg(self: *X86Emitter, dst: Register, src: Register) !void {
        try self.emitRexW(src, dst);
        try self.buffer.append(self.allocator, 0x31); // XOR r/m64, r64
        try self.emitModRM(0b11, src.low3(), dst.low3());
    }

    /// NOT r64
    pub fn notReg(self: *X86Emitter, reg: Register) !void {
        try self.emitRex(true, false, false, reg.isExtended());
        try self.buffer.append(self.allocator, 0xF7); // NOT r/m64
        try self.emitModRM(0b11, 2, reg.low3());
    }

    /// SHL r64, imm8
    pub fn shlRegImm(self: *X86Emitter, reg: Register, imm: u8) !void {
        try self.emitRex(true, false, false, reg.isExtended());
        if (imm == 1) {
            try self.buffer.append(self.allocator, 0xD1); // SHL r/m64, 1
            try self.emitModRM(0b11, 4, reg.low3());
        } else {
            try self.buffer.append(self.allocator, 0xC1); // SHL r/m64, imm8
            try self.emitModRM(0b11, 4, reg.low3());
            try self.buffer.append(self.allocator, imm);
        }
    }

    /// SHR r64, imm8 (logical shift right)
    pub fn shrRegImm(self: *X86Emitter, reg: Register, imm: u8) !void {
        try self.emitRex(true, false, false, reg.isExtended());
        if (imm == 1) {
            try self.buffer.append(self.allocator, 0xD1);
            try self.emitModRM(0b11, 5, reg.low3());
        } else {
            try self.buffer.append(self.allocator, 0xC1);
            try self.emitModRM(0b11, 5, reg.low3());
            try self.buffer.append(self.allocator, imm);
        }
    }

    /// SHR r32, imm8 (32-bit logical shift right, zero-extends to 64-bit)
    pub fn shrRegImm32(self: *X86Emitter, reg: Register, imm: u8) !void {
        // No REX.W for 32-bit, but need REX.B for extended registers
        if (reg.isExtended()) {
            try self.buffer.append(self.allocator, 0x41); // REX.B
        }
        if (imm == 1) {
            try self.buffer.append(self.allocator, 0xD1);
            try self.emitModRM(0b11, 5, reg.low3());
        } else {
            try self.buffer.append(self.allocator, 0xC1);
            try self.emitModRM(0b11, 5, reg.low3());
            try self.buffer.append(self.allocator, imm);
        }
    }

    /// SAR r64, imm8 (arithmetic shift right)
    pub fn sarRegImm(self: *X86Emitter, reg: Register, imm: u8) !void {
        try self.emitRex(true, false, false, reg.isExtended());
        if (imm == 1) {
            try self.buffer.append(self.allocator, 0xD1);
            try self.emitModRM(0b11, 7, reg.low3());
        } else {
            try self.buffer.append(self.allocator, 0xC1);
            try self.emitModRM(0b11, 7, reg.low3());
            try self.buffer.append(self.allocator, imm);
        }
    }

    /// SHL r64, CL (shift left by CL register)
    pub fn shlRegCl(self: *X86Emitter, reg: Register) !void {
        try self.emitRex(true, false, false, reg.isExtended());
        try self.buffer.append(self.allocator, 0xD3); // SHL r/m64, CL
        try self.emitModRM(0b11, 4, reg.low3());
    }

    /// SHR r64, CL (logical shift right by CL register)
    pub fn shrRegCl(self: *X86Emitter, reg: Register) !void {
        try self.emitRex(true, false, false, reg.isExtended());
        try self.buffer.append(self.allocator, 0xD3); // SHR r/m64, CL
        try self.emitModRM(0b11, 5, reg.low3());
    }

    /// SAR r64, CL (arithmetic shift right by CL register)
    pub fn sarRegCl(self: *X86Emitter, reg: Register) !void {
        try self.emitRex(true, false, false, reg.isExtended());
        try self.buffer.append(self.allocator, 0xD3); // SAR r/m64, CL
        try self.emitModRM(0b11, 7, reg.low3());
    }

    /// AND r64, imm32 (sign-extended)
    pub fn andRegImm32(self: *X86Emitter, reg: Register, imm: i32) !void {
        try self.emitRex(true, false, false, reg.isExtended());
        if (reg == .rax) {
            try self.buffer.append(self.allocator, 0x25); // AND RAX, imm32
        } else {
            try self.buffer.append(self.allocator, 0x81); // AND r/m64, imm32
            try self.emitModRM(0b11, 4, reg.low3());
        }
        try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(imm)));
    }

    // ========================================
    // Control flow
    // ========================================

    /// JMP rel32
    pub fn jmp(self: *X86Emitter, target: i32) !void {
        try self.buffer.append(self.allocator, 0xE9);
        try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(target)));
    }

    /// JMP rel8 (short jump)
    pub fn jmpShort(self: *X86Emitter, target: i8) !void {
        try self.buffer.append(self.allocator, 0xEB);
        try self.buffer.append(self.allocator, @bitCast(target));
    }

    /// Conditional jump opcodes
    pub const Condition = enum(u4) {
        o = 0, // overflow
        no = 1, // not overflow
        b = 2, // below (unsigned <)
        ae = 3, // above or equal (unsigned >=)
        e = 4, // equal
        ne = 5, // not equal
        be = 6, // below or equal (unsigned <=)
        a = 7, // above (unsigned >)
        s = 8, // sign
        ns = 9, // not sign
        p = 10, // parity
        np = 11, // not parity
        l = 12, // less (signed <)
        ge = 13, // greater or equal (signed >=)
        le = 14, // less or equal (signed <=)
        g = 15, // greater (signed >)
    };

    /// Jcc rel32 (conditional jump)
    pub fn jcc(self: *X86Emitter, cond: Condition, target: i32) !void {
        try self.buffer.append(self.allocator, 0x0F);
        try self.buffer.append(self.allocator, 0x80 + @as(u8, @intFromEnum(cond)));
        try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(target)));
    }

    /// Jcc rel8 (short conditional jump)
    pub fn jccShort(self: *X86Emitter, cond: Condition, target: i8) !void {
        try self.buffer.append(self.allocator, 0x70 + @as(u8, @intFromEnum(cond)));
        try self.buffer.append(self.allocator, @bitCast(target));
    }

    /// CMOVcc r64, r64 (conditional move)
    pub fn cmovcc(self: *X86Emitter, cond: Condition, dst: Register, src: Register) !void {
        try self.emitRex(true, dst.isExtended(), false, src.isExtended());
        try self.buffer.append(self.allocator, 0x0F);
        try self.buffer.append(self.allocator, 0x40 + @as(u8, @intFromEnum(cond)));
        try self.emitModRM(0b11, dst.low3(), src.low3());
    }

    /// CALL rel32
    pub fn call(self: *X86Emitter, target: i32) !void {
        try self.buffer.append(self.allocator, 0xE8);
        try self.buffer.appendSlice(self.allocator, &@as([4]u8, @bitCast(target)));
    }

    /// CALL r64 (indirect call)
    pub fn callReg(self: *X86Emitter, reg: Register) !void {
        if (reg.isExtended()) {
            try self.emitRex(false, false, false, true);
        }
        try self.buffer.append(self.allocator, 0xFF);
        try self.emitModRM(0b11, 2, reg.low3());
    }

    /// RET
    pub fn ret(self: *X86Emitter) !void {
        try self.buffer.append(self.allocator, 0xC3);
    }

    // ========================================
    // Stack operations
    // ========================================

    /// PUSH r64
    pub fn pushReg(self: *X86Emitter, reg: Register) !void {
        if (reg.isExtended()) {
            try self.emitRex(false, false, false, true);
        }
        try self.buffer.append(self.allocator, 0x50 + @as(u8, reg.low3()));
    }

    /// POP r64
    pub fn popReg(self: *X86Emitter, reg: Register) !void {
        if (reg.isExtended()) {
            try self.emitRex(false, false, false, true);
        }
        try self.buffer.append(self.allocator, 0x58 + @as(u8, reg.low3()));
    }

    // ========================================
    // Miscellaneous
    // ========================================

    /// NOP
    pub fn nop(self: *X86Emitter) !void {
        try self.buffer.append(self.allocator, 0x90);
    }

    /// INT3 (breakpoint)
    pub fn int3(self: *X86Emitter) !void {
        try self.buffer.append(self.allocator, 0xCC);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "X86Emitter: mov reg, imm64" {
    var emit = X86Emitter.init(std.testing.allocator);
    defer emit.deinit();

    try emit.movRegImm64(.rax, 0x123456789ABCDEF0);
    const code = emit.getCode();

    // REX.W (48) + B8 + imm64
    try std.testing.expectEqual(@as(usize, 10), code.len);
    try std.testing.expectEqual(@as(u8, 0x48), code[0]); // REX.W
    try std.testing.expectEqual(@as(u8, 0xB8), code[1]); // MOV rax, imm64
}

test "X86Emitter: mov reg, reg" {
    var emit = X86Emitter.init(std.testing.allocator);
    defer emit.deinit();

    try emit.movRegReg(.rax, .rbx);
    const code = emit.getCode();

    // REX.W (48) + 89 + ModRM
    try std.testing.expectEqual(@as(usize, 3), code.len);
    try std.testing.expectEqual(@as(u8, 0x48), code[0]);
    try std.testing.expectEqual(@as(u8, 0x89), code[1]);
}

test "X86Emitter: add reg, reg" {
    var emit = X86Emitter.init(std.testing.allocator);
    defer emit.deinit();

    try emit.addRegReg(.rax, .rcx);
    const code = emit.getCode();

    try std.testing.expectEqual(@as(usize, 3), code.len);
    try std.testing.expectEqual(@as(u8, 0x48), code[0]); // REX.W
    try std.testing.expectEqual(@as(u8, 0x01), code[1]); // ADD
}

test "X86Emitter: extended registers" {
    var emit = X86Emitter.init(std.testing.allocator);
    defer emit.deinit();

    try emit.movRegReg(.r8, .r9);
    const code = emit.getCode();

    // REX.WRB (4D) for r8, r9
    try std.testing.expectEqual(@as(u8, 0x4D), code[0]);
}

test "X86Emitter: jmp and ret" {
    var emit = X86Emitter.init(std.testing.allocator);
    defer emit.deinit();

    try emit.jmp(0x12345678);
    try emit.ret();
    const code = emit.getCode();

    try std.testing.expectEqual(@as(usize, 6), code.len);
    try std.testing.expectEqual(@as(u8, 0xE9), code[0]); // JMP
    try std.testing.expectEqual(@as(u8, 0xC3), code[5]); // RET
}

test "X86Emitter: conditional jump" {
    var emit = X86Emitter.init(std.testing.allocator);
    defer emit.deinit();

    try emit.jcc(.e, 0x100); // JE
    const code = emit.getCode();

    try std.testing.expectEqual(@as(usize, 6), code.len);
    try std.testing.expectEqual(@as(u8, 0x0F), code[0]);
    try std.testing.expectEqual(@as(u8, 0x84), code[1]); // JE
}

test "X86Emitter: push and pop" {
    var emit = X86Emitter.init(std.testing.allocator);
    defer emit.deinit();

    try emit.pushReg(.rbx);
    try emit.popReg(.rbx);
    const code = emit.getCode();

    try std.testing.expectEqual(@as(usize, 2), code.len);
    try std.testing.expectEqual(@as(u8, 0x53), code[0]); // PUSH rbx
    try std.testing.expectEqual(@as(u8, 0x5B), code[1]); // POP rbx
}

test "X86Emitter: mov memory" {
    var emit = X86Emitter.init(std.testing.allocator);
    defer emit.deinit();

    try emit.movRegMem(.rax, .rbx, 8);
    const code = emit.getCode();

    // REX.W + 8B + ModRM (01 000 011 = disp8) + disp8
    try std.testing.expectEqual(@as(usize, 4), code.len);
    try std.testing.expectEqual(@as(u8, 0x48), code[0]); // REX.W
    try std.testing.expectEqual(@as(u8, 0x8B), code[1]); // MOV
    try std.testing.expectEqual(@as(u8, 0x43), code[2]); // ModRM: [rbx+disp8], rax
    try std.testing.expectEqual(@as(u8, 8), code[3]); // disp8
}
