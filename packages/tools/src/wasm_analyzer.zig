//! WebAssembly host interface for the zigts static analyzer.
//!
//! Compiled to wasm64-freestanding via `zig build wasm`, this module exposes
//! the exact `zigts check --json` analysis pipeline to a browser. The host
//! (the marketing-site proof playground) writes handler source into linear
//! memory and calls `analyze`; the returned JSON is the same envelope the
//! native `zigts check --json` produces, so the in-browser proof card mirrors
//! the real compiler instead of approximating it.
//!
//! ABI:
//!   alloc(len)                 -> ptr   allocate `len` bytes for host input
//!   free(ptr, len)                      release a buffer from `alloc`
//!   analyze(ptr, len, is_tsx)  -> ptr   run the pipeline; [u32 LE len][JSON]
//!
//! The `analyze` result stays valid until the next `analyze` call.

const std = @import("std");
const zigts = @import("zigts");
const precompile = @import("precompile.zig");
const json_diag = precompile.json_diag;

/// Freestanding/wasm has no stderr. `std.log`'s default sink pulls a threaded
/// IO stack that does not compile here, so route every log record to a no-op.
/// The analyzer surfaces findings through the returned JSON, never the log.
pub const std_options: std.Options = .{
    .logFn = noopLog,
};

fn noopLog(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    _ = level;
    _ = scope;
    _ = format;
    _ = args;
}

/// Persistent allocator for host-visible buffers (input source, result JSON).
/// wasm_allocator grows linear memory on demand and needs no libc.
const host_alloc = std.heap.wasm_allocator;

/// The most recent `analyze` result, kept alive until the next call so the
/// host has a stable pointer to read. Layout: [u32 LE length][JSON bytes].
var result_buf: []u8 = &[_]u8{};

/// Allocate `len` bytes of linear memory for the host to fill with UTF-8
/// handler source. Returns null on allocation failure.
export fn alloc(len: usize) ?[*]u8 {
    if (len == 0) return null;
    const slice = host_alloc.alloc(u8, len) catch return null;
    return slice.ptr;
}

/// Release a buffer previously returned by `alloc`.
export fn free(ptr: [*]u8, len: usize) void {
    if (len == 0) return;
    host_alloc.free(ptr[0..len]);
}

/// Run the full single-handler analysis pipeline over the UTF-8 source at
/// `ptr[0..len]`. `is_tsx` selects JSX parsing (pass 1 for `.tsx` handlers,
/// 0 for plain `.ts`). Returns a pointer to a length-prefixed JSON buffer
/// (`[u32 LE length][bytes]`) valid until the next `analyze` call, or null
/// on catastrophic allocation failure.
export fn analyze(ptr: [*]const u8, len: usize, is_tsx: u32) ?[*]const u8 {
    if (result_buf.len != 0) {
        host_alloc.free(result_buf);
        result_buf = &[_]u8{};
    }
    const source: []const u8 = if (len == 0) "" else ptr[0..len];

    var arena = std.heap.ArenaAllocator.init(host_alloc);
    defer arena.deinit();
    const a = arena.allocator();

    var json: std.ArrayList(u8) = .empty;
    var aw: std.Io.Writer.Allocating = .fromArrayList(a, &json);
    runAnalysis(a, source, is_tsx != 0, &aw.writer) catch {
        // runAnalysis only fails on allocation failure; surface a minimal
        // but well-formed envelope so the host renderer never sees garbage.
        return emit(stable_oom_json);
    };
    json = aw.toArrayList();
    return emit(json.items);
}

const stable_oom_json = "{\"success\":false,\"diagnostics\":[],\"error\":\"analyzer_oom\"}";

/// Copy `body` into a freshly allocated length-prefixed buffer and record it
/// as the live result. Returns the buffer pointer, or null on failure.
fn emit(body: []const u8) ?[*]const u8 {
    const out = host_alloc.alloc(u8, 4 + body.len) catch return null;
    std.mem.writeInt(u32, out[0..4], @intCast(body.len), .little);
    @memcpy(out[4..], body);
    result_buf = out;
    return out.ptr;
}

fn runAnalysis(a: std.mem.Allocator, source: []const u8, is_tsx: bool, w: *std.Io.Writer) !void {
    // The handler path is synthetic; only its extension matters - the pipeline
    // keys JSX parsing off a `.tsx` suffix.
    const handler_path: []const u8 = if (is_tsx) "handler.tsx" else "handler.ts";
    var result = try precompile.runCheckOnlyFromSource(
        a,
        source,
        handler_path,
        null, // sql_schema_path: no cross-handler SQL schema in the browser
        true, // json_mode: collect structured diagnostics
        null, // system_path: single-handler analysis only
        false, // skip_contract: run the full contract + proof pipeline
    );
    const contract_ptr: ?*const zigts.handler_contract.HandlerContract =
        if (result.contract) |*c| c else null;
    if (result.totalErrors() > 0) {
        try json_diag.writeErrorJson(w, contract_ptr, result.json_diagnostics.items, null);
    } else {
        try json_diag.writeSuccessJson(w, contract_ptr, result.json_diagnostics.items, null);
    }
}
