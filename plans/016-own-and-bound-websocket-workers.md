# Plan 016: Own and bound WebSocket workers through shutdown

> **Executor instructions**: Follow this plan step by step. Run every verification command. Keep connection ownership explicit; do not preserve detached workers with sleeps or best-effort lifetime assumptions. Update `plans/README.md` when complete.
>
> **Drift check, run first**: `git diff --name-only b00eae29 -- packages/runtime/src/server.zig packages/runtime/src/ws_frame_loop.zig packages/runtime/src/websocket_pool.zig packages/runtime/src/server_test.zig docs/roadmap.md docs/reliability.md`

## Status

- **Priority**: P0
- **Effort**: M-L
- **Risk**: MED-HIGH
- **Depends on**: none
- **Category**: runtime lifecycle / availability
- **Planned at**: `b00eae29` on 2026-07-10

## Why this matters

Each accepted WebSocket spawns and detaches a kernel thread. Its config borrows the server's WebSocket pool, handler pool, reload lock/flag, and I/O backend. `Server.deinit` destroys those objects without first stopping or joining live frame loops. A live connection during graceful shutdown can therefore dereference freed server state. The same unbounded thread-per-upgrade path lets remote clients exhaust threads and memory.

This is the highest-risk blocker to presenting the WebSocket gateway as a dependable real-time game transport.

## Current evidence

- `packages/runtime/src/ws_frame_loop.zig:8-13` explicitly documents one dedicated kernel thread per connection.
- `packages/runtime/src/ws_frame_loop.zig:43-71` stores raw pointers to server-owned pools and reload state in `Config`.
- `packages/runtime/src/ws_frame_loop.zig:89-131` can block in the read loop and touches the borrowed pools again during deferred `onClose`/unregister cleanup.
- `packages/runtime/src/server.zig:1045-1054` documents the detached W1 lifecycle.
- `packages/runtime/src/server.zig:1109-1137` spawns and detaches the worker after passing server-owned pointers.
- `packages/runtime/src/server.zig:1641-1668` deinitializes the handler pool, WebSocket pool, and I/O backend without joining WebSocket workers.
- `packages/runtime/src/server.zig:2262-2288` drains active handler executions only; it does not stop or wait for frame loops.
- `ServerConfig` has HTTP pool/request bounds but no WebSocket connection limit.

## Scope

In scope:

- a server-owned WebSocket worker registry/lifecycle type (prefer a focused new runtime module);
- `ServerConfig` and CLI/config wiring for a maximum live WebSocket count;
- upgrade admission, shutdown, deinit, and frame-loop stop behavior;
- focused unit tests and a live-socket shutdown integration test;
- reliability/limitation documentation.

Out of scope:

- replacing the threaded HTTP backend;
- adopting experimental `std.Io.Evented` as part of this fix;
- WebSocket send serialization (plan 017);
- cross-process rooms or distributed game state.

## Steps

### 1. Introduce explicit worker ownership

Create a server-owned worker registry that records each live connection's stable id, a shutdown-capable socket handle, and its joinable `std.Thread`. Define lifecycle as an exhaustive state machine such as `accepting -> stopping -> joined`.

The registry must close/shutdown every registered socket to wake blocked reads, then join every thread before any borrowed server object is deinitialized. Handle the spawn/register race explicitly: either reserve a slot before spawn and roll it back on failure, or transfer a fully initialized record atomically.

Do not detach worker threads.

### 2. Bound admission and expose pressure

Add `max_websocket_connections` to `ServerConfig` with a conservative finite default and explicit zero/disabled semantics. Reserve capacity before completing the 101 upgrade; when full, return a documented HTTP rejection (prefer 503) without transferring fd ownership.

Track at least live, rejected-at-capacity, and peak-live counters so game load tests can verify behavior rather than infer it from process RSS.

### 3. Order shutdown around ownership

Shutdown order should be:

1. stop accepting HTTP/upgrades;
2. mark the WebSocket registry non-accepting;
3. shutdown all live WebSocket sockets;
4. join all WebSocket workers, allowing their `onClose` and unregister defers to finish;
5. drain remaining handler executions;
6. deinitialize connection, handler, WebSocket, contract, and I/O state.

Make repeated shutdown/deinit calls idempotent where existing server semantics require it.

### 4. Prove the lifecycle

Add tests for:

- admission stops exactly at the configured cap and failed upgrades do not leak slots/fds;
- a server with a live, idle WebSocket returns from shutdown within the grace bound;
- `onClose` completes before the handler/WebSocket pools are destroyed;
- repeated stop/deinit is safe;
- spawn failure rolls back capacity and ownership;
- ThreadSanitizer/Debug runtime-safety runs show no access after deinit where supported.

## Verification

```sh
zig fmt --check build.zig packages/
zig build test-server test-zruntime
zig build smoke-v1 test-panic-isolation
bash scripts/verify.sh
git diff --check
git status --short
```

## Done criteria

- [x] No WebSocket worker is detached.
- [x] Shutdown joins every worker before destroying borrowed state.
- [x] A finite configurable connection cap is enforced before upgrade ownership transfer.
- [x] Spawn, upgrade, shutdown, and repeated-deinit failure paths are tested.
- [x] Reliability docs state the supported concurrency model and limit.
- [x] All verification commands pass.

## STOP conditions

- The chosen Zig socket API cannot reliably wake a blocked read by shutting down the registered handle; isolate and prove an alternative before proceeding.
- Clean joining requires a change to public shutdown timing/semantics beyond the existing grace-period contract; report that contract decision first.
- The cap cannot be enforced before the 101 response without restructuring handshake ownership; do not accept then immediately drop as a silent workaround.
- A focused gate fails twice after reasonable correction.
