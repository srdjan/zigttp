//! Module-level consistency checks for WebSocket handlers.
//!
//! These checks straddle the scope the existing handler_verifier can
//! see — they need a full view of which virtual modules the handler
//! imports and which top-level functions it exports, not just the
//! body of one handler function. `handler_contract.ContractBuilder`
//! already collects both, so the checks live in their own module and
//! consume whatever the builder decided.
//!
//! Emits two diagnostics:
//!   - ZTS320 `websocket_import_without_events`
//!   - ZTS321 `websocket_events_without_import`

const std = @import("std");
const handler_verifier = @import("handler_verifier.zig");

/// Minimal shape the caller provides. Kept deliberately import-free
/// (no NodeIndex, no HandlerContract) so this checker stays a pure
/// function and is trivially testable without setting up the full
/// contract-builder pipeline.
pub const Inputs = struct {
    imports_websocket_module: bool,
    exports_on_open: bool,
    exports_on_message: bool,
    exports_on_close: bool,
    exports_on_error: bool,

    pub fn exportsAnyEvent(self: Inputs) bool {
        return self.exports_on_open or self.exports_on_message or
            self.exports_on_close or self.exports_on_error;
    }
};

pub const Finding = struct {
    kind: handler_verifier.DiagnosticKind,
    severity: handler_verifier.Severity,
    message: []const u8,
    help: []const u8,
};

/// Run the checks and append any findings to `out`. `out` is caller-
/// owned; the appended slices are static string literals.
pub fn check(
    allocator: std.mem.Allocator,
    inputs: Inputs,
    out: *std.ArrayList(Finding),
) !void {
    if (inputs.imports_websocket_module and !inputs.exports_on_message) {
        try out.append(allocator, .{
            .kind = .websocket_import_without_events,
            .severity = .warning,
            .message = "zigttp:websocket is imported but no onMessage handler is exported",
            .help = "Export `onMessage(ws, data, room)` to receive inbound frames, or drop the import if it was speculative.",
        });
    }

    if (!inputs.imports_websocket_module and inputs.exportsAnyEvent()) {
        try out.append(allocator, .{
            .kind = .websocket_events_without_import,
            .severity = .err,
            .message = "WebSocket event handlers are exported but zigttp:websocket is not imported",
            .help = "Add `import { send, close } from 'zigttp:websocket';` so event handlers can reply to frames.",
        });
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "clean handler produces no findings" {
    var out: std.ArrayList(Finding) = .empty;
    defer out.deinit(testing.allocator);
    try check(testing.allocator, .{
        .imports_websocket_module = true,
        .exports_on_open = true,
        .exports_on_message = true,
        .exports_on_close = true,
        .exports_on_error = false,
    }, &out);
    try testing.expectEqual(@as(usize, 0), out.items.len);
}

test "handler with no events is also clean (no WS module imported)" {
    var out: std.ArrayList(Finding) = .empty;
    defer out.deinit(testing.allocator);
    try check(testing.allocator, .{
        .imports_websocket_module = false,
        .exports_on_open = false,
        .exports_on_message = false,
        .exports_on_close = false,
        .exports_on_error = false,
    }, &out);
    try testing.expectEqual(@as(usize, 0), out.items.len);
}

test "import without onMessage warns ZTS320" {
    var out: std.ArrayList(Finding) = .empty;
    defer out.deinit(testing.allocator);
    try check(testing.allocator, .{
        .imports_websocket_module = true,
        .exports_on_open = false,
        .exports_on_message = false,
        .exports_on_close = false,
        .exports_on_error = false,
    }, &out);
    try testing.expectEqual(@as(usize, 1), out.items.len);
    try testing.expectEqual(
        handler_verifier.DiagnosticKind.websocket_import_without_events,
        out.items[0].kind,
    );
    try testing.expectEqual(handler_verifier.Severity.warning, out.items[0].severity);
}

test "import with onOpen only still warns because no onMessage" {
    var out: std.ArrayList(Finding) = .empty;
    defer out.deinit(testing.allocator);
    try check(testing.allocator, .{
        .imports_websocket_module = true,
        .exports_on_open = true,
        .exports_on_message = false,
        .exports_on_close = false,
        .exports_on_error = false,
    }, &out);
    try testing.expectEqual(@as(usize, 1), out.items.len);
    try testing.expectEqual(
        handler_verifier.DiagnosticKind.websocket_import_without_events,
        out.items[0].kind,
    );
}

test "events without import errors ZTS321" {
    var out: std.ArrayList(Finding) = .empty;
    defer out.deinit(testing.allocator);
    try check(testing.allocator, .{
        .imports_websocket_module = false,
        .exports_on_open = false,
        .exports_on_message = true,
        .exports_on_close = false,
        .exports_on_error = false,
    }, &out);
    try testing.expectEqual(@as(usize, 1), out.items.len);
    try testing.expectEqual(
        handler_verifier.DiagnosticKind.websocket_events_without_import,
        out.items[0].kind,
    );
    try testing.expectEqual(handler_verifier.Severity.err, out.items[0].severity);
}

test "onClose alone without import still errors" {
    var out: std.ArrayList(Finding) = .empty;
    defer out.deinit(testing.allocator);
    try check(testing.allocator, .{
        .imports_websocket_module = false,
        .exports_on_open = false,
        .exports_on_message = false,
        .exports_on_close = true,
        .exports_on_error = false,
    }, &out);
    try testing.expectEqual(@as(usize, 1), out.items.len);
    try testing.expectEqual(
        handler_verifier.DiagnosticKind.websocket_events_without_import,
        out.items[0].kind,
    );
}
