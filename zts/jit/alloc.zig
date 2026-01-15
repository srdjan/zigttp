//! Executable Memory Allocator for JIT Compilation
//!
//! Handles platform-specific requirements for executable memory:
//! - macOS: Uses MAP_JIT and pthread_jit_write_protect_np for W^X compliance
//! - Linux: Uses standard mmap with PROT_EXEC
//!
//! Memory is allocated in pages and can be toggled between writable and executable.

const std = @import("std");
const builtin = @import("builtin");

/// Page size (typically 4KB, but 16KB on Apple Silicon)
pub const PAGE_SIZE: usize = if (builtin.os.tag == .macos and builtin.cpu.arch == .aarch64)
    16384
else
    4096;

/// Error types for code allocation
pub const AllocError = error{
    MmapFailed,
    MprotectFailed,
    OutOfMemory,
};

/// A single page of executable memory
pub const CodePage = struct {
    /// Base address of the page
    memory: []align(PAGE_SIZE) u8,
    /// Current write offset within the page
    offset: usize,
    /// Whether the page is currently writable (vs executable)
    writable: bool,

    /// Allocate a new code page
    pub fn init() AllocError!CodePage {
        const memory = allocExecutablePage() catch return AllocError.MmapFailed;
        return .{
            .memory = memory,
            .offset = 0,
            .writable = true,
        };
    }

    /// Free the code page
    pub fn deinit(self: *CodePage) void {
        freeExecutablePage(self.memory);
    }

    /// Get remaining space in this page
    pub fn remaining(self: *const CodePage) usize {
        return self.memory.len - self.offset;
    }

    /// Allocate space from this page, returns slice to write code into
    pub fn alloc(self: *CodePage, size: usize) ?[]u8 {
        if (self.offset + size > self.memory.len) return null;
        const slice = self.memory[self.offset..][0..size];
        self.offset += size;
        return slice;
    }

    /// Make the page executable (and non-writable)
    pub fn makeExecutable(self: *CodePage) AllocError!void {
        if (!self.writable) return;
        makePageExecutable(self.memory) catch return AllocError.MprotectFailed;
        self.writable = false;
    }

    /// Make the page writable (and non-executable)
    pub fn makeWritable(self: *CodePage) AllocError!void {
        if (self.writable) return;
        makePageWritable(self.memory) catch return AllocError.MprotectFailed;
        self.writable = true;
    }
};

/// Manages multiple pages of executable memory
pub const CodeAllocator = struct {
    /// List of allocated pages
    pages: std.ArrayListUnmanaged(CodePage),
    /// Backing allocator for the page list
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) CodeAllocator {
        return .{
            .pages = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *CodeAllocator) void {
        for (self.pages.items) |*page| {
            page.deinit();
        }
        self.pages.deinit(self.allocator);
    }

    /// Allocate space for code, returns writable slice
    /// After writing, call makeExecutable() on the returned slice's page
    pub fn alloc(self: *CodeAllocator, size: usize) AllocError![]u8 {
        // Try to allocate from existing pages
        for (self.pages.items) |*page| {
            if (page.writable) {
                if (page.alloc(size)) |slice| {
                    return slice;
                }
            }
        }

        // Need a new page
        var page = try CodePage.init();
        errdefer page.deinit();

        const slice = page.alloc(size) orelse return AllocError.OutOfMemory;
        self.pages.append(self.allocator, page) catch return AllocError.OutOfMemory;
        return slice;
    }

    /// Make all pages executable
    pub fn makeAllExecutable(self: *CodeAllocator) AllocError!void {
        for (self.pages.items) |*page| {
            try page.makeExecutable();
        }
    }
};

// ============================================================================
// Platform-specific implementation
// ============================================================================

fn allocExecutablePage() ![]align(PAGE_SIZE) u8 {
    if (builtin.os.tag == .macos) {
        return allocExecutablePageMacOS();
    } else if (builtin.os.tag == .linux) {
        return allocExecutablePageLinux();
    } else {
        @compileError("Unsupported platform for JIT compilation");
    }
}

fn freeExecutablePage(memory: []align(PAGE_SIZE) u8) void {
    if (builtin.os.tag == .macos or builtin.os.tag == .linux) {
        _ = std.posix.munmap(memory);
    }
}

fn makePageExecutable(memory: []align(PAGE_SIZE) u8) !void {
    const prot_rx: std.c.PROT = .{ .READ = true, .EXEC = true };
    if (builtin.os.tag == .macos) {
        // On macOS with MAP_JIT, we need to use pthread_jit_write_protect_np
        // to toggle between write and execute permissions
        if (builtin.cpu.arch == .aarch64) {
            // Apple Silicon: use JIT write protection
            pthread_jit_write_protect_np(true);
        }
        // Also update via mprotect for x86_64 or as fallback
        if (std.c.mprotect(@ptrCast(memory.ptr), memory.len, prot_rx) != 0) return error.MprotectFailed;
    } else {
        if (std.c.mprotect(@ptrCast(memory.ptr), memory.len, prot_rx) != 0) return error.MprotectFailed;
    }
}

fn makePageWritable(memory: []align(PAGE_SIZE) u8) !void {
    const prot_rw: std.c.PROT = .{ .READ = true, .WRITE = true };
    if (builtin.os.tag == .macos) {
        if (builtin.cpu.arch == .aarch64) {
            pthread_jit_write_protect_np(false);
        }
        if (std.c.mprotect(@ptrCast(memory.ptr), memory.len, prot_rw) != 0) return error.MprotectFailed;
    } else {
        if (std.c.mprotect(@ptrCast(memory.ptr), memory.len, prot_rw) != 0) return error.MprotectFailed;
    }
}

fn allocExecutablePageMacOS() ![]align(PAGE_SIZE) u8 {
    // On macOS, we use MAP_JIT which allows JIT code to be written and executed
    // with proper W^X handling via pthread_jit_write_protect_np
    const flags: std.posix.MAP = .{
        .TYPE = .PRIVATE,
        .ANONYMOUS = true,
    };

    // MAP_JIT is 0x0800 on macOS
    const MAP_JIT: u32 = 0x0800;
    const flags_with_jit = @as(u32, @bitCast(flags)) | MAP_JIT;

    const prot_rw: std.c.PROT = .{ .READ = true, .WRITE = true };
    const result = std.posix.mmap(
        null,
        PAGE_SIZE,
        prot_rw,
        @bitCast(flags_with_jit),
        -1,
        0,
    ) catch return error.MmapFailed;

    return @alignCast(result[0..PAGE_SIZE]);
}

fn allocExecutablePageLinux() ![]align(PAGE_SIZE) u8 {
    const prot_rw: std.c.PROT = .{ .READ = true, .WRITE = true };
    const result = std.posix.mmap(
        null,
        PAGE_SIZE,
        prot_rw,
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    ) catch return error.MmapFailed;

    return @alignCast(result[0..PAGE_SIZE]);
}

// External declaration for macOS JIT write protection
extern "c" fn pthread_jit_write_protect_np(enabled: bool) void;

// ============================================================================
// Compiled code structure
// ============================================================================

/// Compiled function entry point signature
/// Takes context pointer, returns JS value (u64)
pub const CompiledFn = *const fn (ctx: *anyopaque) callconv(.c) u64;

/// Represents a compiled function that can be executed
pub const CompiledCode = struct {
    /// Pointer to the machine code
    code: []const u8,
    /// Entry point function pointer (takes context, returns JSValue as u64)
    entry: CompiledFn,

    /// Create from a code slice (must already be executable)
    pub fn fromSlice(code: []const u8) CompiledCode {
        return .{
            .code = code,
            .entry = @ptrCast(@alignCast(code.ptr)),
        };
    }

    /// Execute the compiled code with context
    pub fn execute(self: *const CompiledCode, ctx: *anyopaque) u64 {
        return self.entry(ctx);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "CodePage: basic allocation" {
    var page = try CodePage.init();
    defer page.deinit();

    try std.testing.expect(page.writable);
    try std.testing.expectEqual(@as(usize, 0), page.offset);
    try std.testing.expectEqual(PAGE_SIZE, page.remaining());

    const slice = page.alloc(100);
    try std.testing.expect(slice != null);
    try std.testing.expectEqual(@as(usize, 100), slice.?.len);
    try std.testing.expectEqual(@as(usize, 100), page.offset);
}

test "CodePage: make executable and writable" {
    var page = try CodePage.init();
    defer page.deinit();

    // Write some code (NOP sled + RET)
    const slice = page.alloc(10).?;
    @memset(slice[0..9], 0x90); // NOP
    slice[9] = 0xC3; // RET

    // Make executable
    try page.makeExecutable();
    try std.testing.expect(!page.writable);

    // Make writable again
    try page.makeWritable();
    try std.testing.expect(page.writable);
}

test "CodeAllocator: basic usage" {
    var alloc_inst = CodeAllocator.init(std.testing.allocator);
    defer alloc_inst.deinit();

    const code = try alloc_inst.alloc(64);
    try std.testing.expectEqual(@as(usize, 64), code.len);
}

test "CodeAllocator: multiple allocations" {
    var alloc_inst = CodeAllocator.init(std.testing.allocator);
    defer alloc_inst.deinit();

    const code1 = try alloc_inst.alloc(100);
    const code2 = try alloc_inst.alloc(200);
    const code3 = try alloc_inst.alloc(50);

    try std.testing.expectEqual(@as(usize, 100), code1.len);
    try std.testing.expectEqual(@as(usize, 200), code2.len);
    try std.testing.expectEqual(@as(usize, 50), code3.len);

    // Should all be from the same page
    try std.testing.expectEqual(@as(usize, 1), alloc_inst.pages.items.len);
}
