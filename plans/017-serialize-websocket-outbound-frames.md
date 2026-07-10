# Plan 017: Serialize and validate all outbound WebSocket frames

> **Executor instructions**: Follow this plan step by step. Run every verification command. Preserve connection-local concurrency; do not hold the pool's coarse index lock during socket I/O. Update `plans/README.md` when complete.
>
> **Drift check, run first**: `git diff --name-only b00eae29 -- packages/runtime/src/websocket_pool.zig packages/runtime/src/ws_runtime_callbacks.zig packages/runtime/src/ws_frame_loop.zig packages/runtime/src/websocket_codec.zig`

## Status

- **Priority**: P1
- **Effort**: M
- **Risk**: MED
- **Depends on**: 016 recommended, not strictly required
- **Category**: runtime protocol correctness
- **Planned at**: `b00eae29` on 2026-07-10

## Why this matters

Broadcast handlers can have multiple runtime threads send to the same peer concurrently. The current gather-write narrows the interleave window but explicitly admits that a short write can interleave the remainder of two frames. That corrupts the RFC 6455 byte stream and disconnects players under load or backpressure. Outbound close frames also accept codes the inbound codec rejects and truncate UTF-8 reasons at an arbitrary byte.

For games, ordered, valid connection-local delivery is more important than maximizing parallel writes to one slow client.

## Current evidence

- `packages/runtime/src/ws_runtime_callbacks.zig:54-62` documents the remaining short-write interleave and names a per-connection send lock as the robust fix.
- `packages/runtime/src/ws_runtime_callbacks.zig:105-115` duplicates the fd, then writes without a stable connection-local serialization lease.
- `packages/runtime/src/ws_frame_loop.zig:134-224` also writes pongs, auto-responses, close frames, and echo frames through its private writer, so callback-only locking would remain incomplete.
- `packages/runtime/src/ws_runtime_callbacks.zig:133-166` permits any code in `[1000,4999]` and truncates the reason to 123 raw bytes.
- `packages/runtime/src/websocket_codec.zig:193-210` already has stricter close-code and UTF-8 validation for peer frames.

## Scope

In scope:

- per-connection outbound serialization with lifetime-safe leasing;
- one outbound frame writer shared by callbacks and the frame loop;
- shared close-code validation and UTF-8-safe reason truncation;
- concurrency/protocol regression tests.

Out of scope:

- application-level message acknowledgement/replay;
- global broadcast ordering across different peers;
- an unbounded outbound queue;
- WebSocket worker shutdown ownership (plan 016).

## Steps

### 1. Add a stable outbound lease

Give each registered connection a separately owned outbound state containing a mutex and the minimum fd/lifetime data required for a send. Acquiring a lease under the pool lock must pin that outbound state until the send releases it, even if `unregister` races concurrently.

Prefer explicit refcounted ownership or another mechanism that proves the state cannot be freed while a sender waits on/holds its mutex. Do not retain a raw `Connection*` after dropping the pool lock, and do not hold the pool's coarse lock while performing socket writes.

### 2. Route every frame through one serializer

Move raw frame construction/writing to a shared helper used by:

- handler `send`;
- handler `close`;
- frame-loop ping/pong and close replies;
- auto-responses;
- any echo/test path.

Hold the connection's outbound mutex from the first header byte through the last payload byte, including all short-write retries. Preserve ordering among frames issued to the same peer; writes to different peers must remain independent.

### 3. Share protocol validation

Expose/reuse the codec's valid-close-code predicate for outbound close calls. Reject reserved/non-transmittable codes rather than merely range-checking.

Validate the supplied reason as UTF-8. When it exceeds 123 bytes, truncate at the largest valid code-point boundary at or below 123 bytes. Add a small pure helper and table-driven tests.

### 4. Add adversarial tests

Use a socket pair or injectable short-write sink to prove:

- two concurrent large sends to one peer decode as two complete frames with no interleaving;
- frames to different peers are not serialized by a global lock;
- unregister racing an acquired/waiting send cannot use freed state or a recycled fd;
- reserved close codes are rejected;
- a multibyte reason crossing byte 123 remains valid UTF-8;
- frame-loop replies and callback sends share the same ordering boundary.

## Verification

```sh
zig fmt --check build.zig packages/
zig build test-server test-zruntime
bash scripts/verify.sh
git diff --check
git status --short
```

## Done criteria

- [x] All bytes for one frame are serialized per connection through short-write completion.
- [x] No pool-global lock is held during network I/O.
- [x] The lease remains valid across concurrent unregister.
- [x] All outbound frame sources use the same serialization primitive.
- [x] Outbound close codes/reasons obey the same protocol rules as inbound frames.
- [x] Concurrency and UTF-8 regression tests pass.

## STOP conditions

- The design depends on a raw pointer whose lifetime is not mechanically pinned across unregister.
- The only proposed fix serializes every connection behind the pool's coarse lock.
- A queue is introduced without a finite byte/message bound and slow-client policy.
- A focused gate fails twice after reasonable correction.
