# zigttp unreleased

Curated release notes. Paste the sections below above the auto-generated
commit changelog when editing the next GitHub Release. Mirrors `CHANGELOG.md`
under `[Unreleased]`.

## Highlights

A runtime fault no longer returns a bare 500: the body names the proof chip that
guards the fault's class, plus the faulting source line, and a fault on a fully
proven path is flagged as a soundness incident. Durable workflows now fail closed
on unproven automatic retry or reuse. The workflow queue also has
operator-visible recovery paths for dead-lettered children and interrupted lease
reclaim, and durable fetch stops retrying once the enclosing step deadline
passes.

## Changed

- **Breaking:** `zigttp:websocket` removes `roomFromPath`. Use the normalized
  room passed to WebSocket callbacks and pass that value to
  `getWebSockets(room)`; there is no beta compatibility stub.
- **Breaking:** generic `tools.invoke` RPC calls can discover and invoke only
  analysis and workspace-read tools. Process execution, agent-state
  persistence, and workspace writes require their trusted/model surfaces.
- **Breaking:** durable `run()` no longer trusts automatic retry or
  duplicate-response replay by default. Reusing a completed durable response, or
  retrying a run that a crash left incomplete, now requires either a workflow
  proof (`idempotent` for response reuse, `retry_safe` for retries) or a
  matching client-supplied `Idempotency-Key` recorded in the on-disk ledger.
  Without one of those guarantees, the handler receives a soft `599` JSON error
  (`DurableIdempotencyUnproven` or `DurableRetryUnproven`) instead of silently
  re-running or replaying a side effect.

## Added

- Proof-explained failures: the runtime maps a fault to the proof chip that
  guards its class (type error to `optional_safe`/`result_safe`, non-Response
  return to `exhaustive_returns`) and names the unproven chip as the predicted
  cause. A fault on a path where every guarding chip was proven is reported as a
  possible soundness incident.
- Source-mapped runtime traps: the engine carries a per-function line table from
  IR through bytecode and the bytecode cache (format v4), so an interpreter-tier
  type fault reports the faulting `line:column` in the 500 body and in the
  soundness-incident record.
- `--incident-log <FILE>`: opt-in JSONL sink for soundness incidents, appended
  through a shared `O_APPEND` fd. Off by default.
- `--max-websocket-connections <N>`: caps live WebSocket connections (default
  1024; `0` disables upgrades). Upgrades past the cap get a `503` before
  protocol ownership transfers.
- `--workflow-queue` recovers `.reclaim-*` files left by a crashed lease-reclaim
  attempt instead of leaving them invisible to future claims.
- A dead-lettered workflow-queue child request now suspends the parent durable
  step and can be resolved through queue replay or discard rather than becoming
  a terminal cached parent error.
- Durable fetch (`zigttp:fetch`'s `fetch()`) stops its retry/backoff loop as soon
  as the enclosing durable step deadline passes.

## Breaking changes

- See the WebSocket, agent RPC policy, and durable `run()` entries under
  Changed.
