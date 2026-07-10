# Plan 019: Align the WebSocket module contract with executable behavior

> **Executor instructions**: Follow this plan step by step. Run every verification command. This plan intentionally uses a direct beta cutover: remove an advertised export that has no semantics instead of keeping a permanent throwing stub. Ask the operator before execution if compatibility must be preserved. Update `plans/README.md` when complete.
>
> **Drift check, run first**: `git diff --name-only b00eae29 -- packages/modules/src/net/websocket.zig packages/modules/module-specs/net/websocket.json packages/runtime/src/ws_runtime_callbacks.zig packages/runtime/src/websocket_pool.zig packages/runtime/src/ws_gateway.zig packages/zigts/src/builtin_modules.zig docs/virtual-modules/README.md examples/websocket/chat.ts`

## Status

- **Priority**: P1
- **Effort**: M
- **Risk**: MED
- **Depends on**: 016 recommended for a finite all-room allocation bound
- **Category**: public module correctness / game scalability
- **Planned at**: `b00eae29` on 2026-07-10

## Why this matters

The WebSocket registry, generated module metadata, compiler type environment, expert prompt, and runtime callbacks are intended to describe one API. Today they disagree: one export always throws, another advertises two arguments while its callback requires three, and room enumeration silently returns only the first 256 peers. A chat/game handler can compile against the advertised surface yet fail or miss players at runtime.

## Current evidence

- `packages/modules/src/net/websocket.zig:34-44` advertises seven exports.
- `packages/modules/src/net/websocket.zig:43` declares `setAutoResponse` with `arg_count = 2` and two string parameter types.
- `packages/runtime/src/ws_runtime_callbacks.zig:274-301` requires `setAutoResponse(ws, request, response)` and reads three arguments.
- `packages/runtime/src/ws_runtime_callbacks.zig:264-272` implements `roomFromPath` only as an unconditional error.
- `packages/runtime/src/ws_gateway.zig:96-98` describes `roomFromPath` as a later override despite the public registry already presenting it as available.
- `packages/runtime/src/ws_runtime_callbacks.zig:248-260` caps `getWebSockets(room)` to 256 ids and returns no overflow signal.
- `packages/runtime/src/websocket_pool.zig:660-672` tests truncation as the current collection behavior.
- `docs/virtual-modules/README.md:35` lists the unsupported export without a limitation.
- `examples/websocket/chat.ts:24-39` demonstrates broadcast but has no executable WebSocket integration test.

## Scope

In scope:

- WebSocket binding/spec/generated registry parity;
- `setAutoResponse` signature and executable tests;
- removal of the unsupported `roomFromPath` export and stale callback/wiring unless product semantics are supplied before execution;
- complete or explicitly fallible room enumeration;
- one end-to-end WebSocket example test.

Out of scope:

- inventing undocumented `roomFromPath` matching semantics;
- distributed room membership;
- binary frames/attachments;
- worker lifecycle and send serialization (plans 016/017).

## Steps

### 1. Make unsupported surface honest

Remove `roomFromPath` from the SDK binding, module spec, runtime callback table, install wiring, generated docs, and expert-facing module lists. Remove gateway comments that imply the override exists today. Because the live implementation can only throw, this is a direct beta cleanup rather than a loss of working behavior.

If the operator supplies concrete semantics and requests implementation instead, stop and re-plan that behavior as an external contract change with tests; do not guess what the two parameters mean.

### 2. Correct `setAutoResponse`

Declare the live signature consistently as `setAutoResponse(ws, request, response)` with three parameter types (`object`, `string`, `string`) and `arg_count = 3`. Confirm the compiler, generated types, runtime native registration, module spec, and user-facing docs all agree.

Add a callback-level test and a real WebSocket test proving a registered request gets its response without invoking the JS handler.

### 3. Eliminate silent room truncation

Replace the fixed `[256]ConnectionId` snapshot with a bounded allocator-owned snapshot or iterator that returns every live member. With plan 016 complete, bound the worst case by `max_websocket_connections`. Without plan 016, introduce an explicit configurable bound and return a visible error/overflow result; do not silently omit peers.

Keep pool locking limited to snapshot construction. Do not hold the pool lock while creating JS values or sending frames.

### 4. Turn the example into a game-transport gate

Add an integration test around `examples/websocket/chat.ts` (or a smaller fixture) that covers:

- upgrade and `onOpen`;
- room broadcast to more than one peer;
- auto-response without handler dispatch;
- close metadata;
- the configured room/connection limit behavior;
- module-governance/docs drift.

## Verification

```sh
zig fmt --check build.zig packages/
zig build test-modules test-module-governance test-docs-drift test-doc-links
zig build test-server test-zruntime test-zigts
bash scripts/test-examples.sh
bash scripts/verify.sh
git diff --check
git status --short
```

## Done criteria

- [x] Every advertised WebSocket export has executable runtime behavior.
- [x] `setAutoResponse` has one consistent three-argument signature end to end.
- [x] Room enumeration never silently omits live peers.
- [x] The chat/game fixture exercises a real multi-peer gateway path.
- [x] Registry, spec, generated types, docs, and runtime callbacks agree.
- [x] All verification commands pass.

## STOP conditions

- Backward compatibility for the throwing `roomFromPath` symbol is required; obtain explicit semantics/versioning direction before proceeding.
- Full room snapshots are unbounded because plan 016 or another finite connection policy is absent; choose an explicit overflow contract rather than allocating without limit.
- The only test approach mocks the callback without exercising a real frame/upgrade path; keep the unit test but add an integration path before declaring done.
- A focused gate fails twice after reasonable correction.
