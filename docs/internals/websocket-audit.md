# WebSocket Frame Validation Audit

RFC 6455 compliance review of the runtime's WebSocket handling. Frame-level parsing is delegated to `std.http.Server.WebSocket.readSmallMessage` (see `packages/runtime/src/websocket_codec.zig:34` re-exports). The runtime layer maps stdlib errors to wire-correct close codes.

## RFC 6455 checklist

| Concern | RFC ref | Status | Evidence |
|---------|---------|--------|----------|
| Client-mask-bit required, reject unmasked | §5.3 | OK | `ws_frame_loop.zig:114` maps `error.MissingMaskBit` to close 1002 (protocol error) |
| Reserved close codes (0-999, 1004-1006, 1015) | §7.4 | **GAP** | Peer-sent close frames are not parsed; the `.connection_close` arm at `ws_frame_loop.zig:152` returns without code validation. `onClose` always receives 1000. Documented as W2 work in the source comments at `ws_frame_loop.zig:262-266` |
| Continuation frames | §5.4 | OK | `readSmallMessage` rejects fragmented frames as `MessageOversize`, mapped to close 1009 (`ws_frame_loop.zig:110-113`). The `.continuation` arm at line 153 is defensive and also maps to 1002 |
| Max frame size enforced | §5.2 | OK | `max_message_bytes = 64 * 1024` at `ws_frame_loop.zig:41`, enforced by stdlib reader. Oversize raises `error.MessageOversize` |
| Unknown opcode rejected | §5.2 | OK | `error.UnexpectedOpCode` mapped to close 1002 at `ws_frame_loop.zig:114`. The `_ =>` arm at line 159 catches anything stdlib lets through with close 1002 |
| Server-to-client frames unmasked | §5.3 | OK (delegated) | Handled by stdlib's `writeMessage` |
| Ping payload echoed in pong | §5.5.3 | OK | `ws_frame_loop.zig:124-127` |
| Handshake: Upgrade, Connection, version 13, key present | §4.1, §4.2 | OK | `websocket_codec.zig:76-109` with eight tests covering valid, missing, wrong-version, multi-token, case-insensitive |
| Handshake: accept-key derivation | §4.2.2 | OK | `computeAcceptKey` at `websocket_codec.zig:49`, RFC test vector at `websocket_codec.zig:140` |

## The one substantive gap

Peer-sent close-frame code validation. The current code treats every close as a graceful exit. If a peer sends a close frame with a reserved code (0-999, 1004-1006, 1015), RFC 6455 §7.4 requires the server to treat it as a protocol error and close with 1002. Today we do not. This is acknowledged inline at `ws_frame_loop.zig:262-266`:

> `reason` is empty for W1 because the peer-sent close payload isn't surfaced by `readSmallMessage` (std returns `error.ConnectionClose` without the body). W2 will parse the close frame explicitly to thread the reason through.

The fix lands naturally when W2 starts surfacing the close payload to `onClose(ws, code, reason)`. Until then, the impact is limited: a misbehaving peer can close with a reserved code and the server treats it as a normal close. No security boundary is crossed because the connection is closing anyway.

## What is already covered by tests

- `websocket_codec.zig` has 9 tests covering handshake validation, accept-key derivation, the Opcode enum shape (regression guard if stdlib renumbers), and tokenized Connection-header parsing.
- `ws_frame_loop.zig` has 1 test asserting buffer-size invariants.

## What is NOT covered by tests

- Black-box frame injection: feeding `readSmallMessage` malformed frames via a pipe-pair and asserting the close-code response. Would require ~80 lines of test plumbing for two assertions. The stdlib already has these tests; the runtime's only job is mapping errors to close codes, which is a 5-line `switch`. Reading the switch is faster than a black-box test.
- The W2 close-code gap (above). Test lands with the fix.

## No code changes from this audit

The implementation correctly delegates frame parsing to the stdlib and maps the relevant error paths to the right close codes. The W2 gap is acknowledged in source comments and waits on broader work (close-frame body surfacing). Tests already cover the handshake path comprehensively.
