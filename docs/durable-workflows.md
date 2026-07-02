# Durable Workflows

Durable workflows combine `zigttp:durable` with optional `zigttp:workflow`
dispatch. They give a handler a stable run key, an oplog for replay, and
operator-visible artifacts that say which replay guarantees the compiler proved.

## Runtime Model

Enable the oplog with `--durable <dir>`:

```bash
zigttp serve examples/workflow/durable-orchestrator.ts \
  --system examples/workflow/system.json \
  --durable ./.durable
```

`run(key, fn)` owns one durable run. `step(name, fn)` records a JSON snapshot of
the step result. `stepWithTimeout(name, timeoutMs, fn)` returns a Result; on
timeout the result is `{ ok: false, error: "timeout" }`. `sleep()` and
`waitSignal()` park a run and return `202` until the timer or signal can resume.

Use `Idempotency-Key` when the client supplies one. The runtime records a small
ledger entry for a matching header and run key. If the compiler cannot prove a
workflow retry-safe or idempotent, a matching idempotency key lets the same
caller retry or reuse the completed response without opening that replay to
other callers.

## Proofs Vs Runtime Guarantees

The analyzer emits `durable.workflow.properties` in `contract.json`:

```json
{
  "durable": {
    "workflow": {
      "proofLevel": "complete",
      "properties": {
        "retrySafe": true,
        "idempotent": true,
        "faultCovered": true,
        "reasons": ["complete durable workflow graph uses stable keys, stable names, and modeled recovery nodes"]
      }
    }
  }
}
```

At runtime, validated contracts copy those properties into the durable executor.
When enforcement is active:

- Incomplete replay requires `retrySafe` or a matching `Idempotency-Key`.
- Completed response reuse requires `idempotent` or a matching
  `Idempotency-Key`.
- Unproven replay returns a normal `599` JSON response with
  `DurableRetryUnproven` or `DurableIdempotencyUnproven`.

Proof receipts and deploy manifests expose the same status:

- `zigttp verify --json` includes `durableWorkflowProofLevel`,
  `durableWorkflowRetrySafe`, `durableWorkflowIdempotent`, and
  `durableWorkflowFaultCovered`.
- AWS and Cloudflare deploy metadata include the same durable workflow proof
  snapshot.
- `proofTrace.durable_workflow_retry_safe`,
  `proofTrace.durable_workflow_idempotent`, and
  `proofTrace.durable_workflow_fault_covered` explain why a guarantee is
  present or absent.

Two compile-time checks reject a handler outright rather than letting it
ship with a guarantee that only looks proven:

- **ZTS509** - `workflow.call`, `saga`, `fanout`, or `follow` used inside a
  `durable.step()` callback. These exports only durably record at step
  depth 0; nested inside a `step()` they would otherwise silently lose
  durability at runtime with no error. Move the call to the same
  `durable.run()` scope it's already recorded in.
- **ZTS510** - a statically-analyzable `saga([...])` has a non-last step
  with no `compensate`, the structural signature of a partial-rollback
  hole: if a later step fails, that step's already-completed side effect
  is never undone. The last step may omit `compensate` (it never completed
  if it's the one that failed). A saga built from anything other than a
  fully static array of step object literals (a spread, a computed step,
  an externally-referenced step array) is not analyzable and stays
  unproven rather than guessed at - `contract.json`'s `sagas[].dynamic`
  reports this, and `sagas[].compensationProven` is only ever `true` for a
  fully static, fully covered saga.

The most common ZTS509 mistake is putting a child dispatch behind a user
`step()` boundary:

```ts
import { run, step } from "zigttp:durable";
import { call } from "zigttp:workflow";

function handler(req: Request): Response {
  const key = req.headers.get("idempotency-key") ?? "bad";
  return run(key, () =>
    step("charge", () => call("greet", { path: "/charge" })),
  );
}
```

That is invalid because the workflow queue can only persist top-level child
dispatch inside the `run()` body. Move the child call to durable depth 0:

```ts
import { run } from "zigttp:durable";
import { call } from "zigttp:workflow";

function handler(req: Request): Response {
  const key = req.headers.get("idempotency-key") ?? "good";
  return run(key, () => {
    const res = call("greet", { path: "/charge" });
    return Response.json({ status: res.status });
  });
}
```

## Durable-Run Recovery and Dead-Letters

`durable_recovery.zig`'s background scheduler retries an incomplete oplog
with jittered exponential backoff. A run that fails ten consecutive times is
quarantined: it stops retrying, and a dead-run record is written next to the
oplog (never inside it) at `<durable>/dead-runs/<id>.json`, with the run key,
the last failure reason, and timestamps.

```bash
zigttp durable dead-runs list --durable ./.durable
zigttp durable dead-runs show --durable ./.durable <id>
zigttp durable dead-runs replay --durable ./.durable <id>
zigttp durable dead-runs discard --durable ./.durable <id>
```

A restarted process honors a standing dead-run record instead of forgetting
the quarantine: the in-memory retry count resets on every restart, but the
persisted record does not, so a quarantined run stays quarantined across
restarts until an operator acts on it. `replay` deletes the record so the
next recovery poll retries the run from its untouched oplog; `discard`
instead rewrites the record with `state: "discarded"` (kept on disk for
`show`, excluded from `list`) so the run stays permanently unretried without
silently becoming eligible again on the next poll.

## Workflow Queue

`zigttp:workflow` dispatches to co-located handlers from `--system <file>`.
Inside durable `run()`, completed `call`, `follow`, and `fanout` child
responses are recorded so replay does not re-dispatch completed children.

`fanout()` dispatches its calls one at a time, not concurrently - the name
describes the shape of the request (many targets), not parallel execution.
Results are ordered by declaration index regardless of dispatch order. This
keeps the durable oplog boundary simple (the whole fan-out replays as one
step) at the cost of wall-clock speedup; use `zigttp:io`'s `parallel()` for
actual concurrent I/O outside a durable workflow context.

There are two queue surfaces with different reliability models:

- `--workflow-queue` persists workflow child dispatch under the durable
  directory. Use it for durable `workflow.call`, `workflow.follow`, and
  ordered `workflow.fanout` boundaries inside `run()`.
- `zigttp:queue` is the opt-in actor mailbox module. It isolates handlers and
  passes messages through process-local actor queues; it is not a durable
  workflow queue substitute.

Add `--workflow-queue` to persist top-level child dispatch before it runs:

```bash
zigttp serve examples/workflow/dsl-orchestrator.ts \
  --system examples/workflow/system.json \
  --durable ./.durable \
  --workflow-queue
```

The queue lives under `<durable>/workflow-queue`. Items move through
`pending/`, `leased/`, `done/`, and `dead/`. Leases expire and can be reclaimed.
After the attempt cap, the item moves to a dead letter. Dead letters block new
enqueue for the same item id until an operator replays or discards them. The
parent workflow suspends (`202`) while its child is dead-lettered rather than
completing with a terminal error, so the queue item's state and the parent
run's state are separate: `replay`/`discard` only change the queue item, and
the parent request must be retried with the same `Idempotency-Key` afterward
to actually resolve:

```bash
zigttp workflow-queue list --durable ./.durable
zigttp workflow-queue show --durable ./.durable <item-id>
zigttp workflow-queue replay --durable ./.durable <item-id>
zigttp workflow-queue discard --durable ./.durable <item-id>
```

`saga()` is intentionally rejected with `--workflow-queue`. Saga step closures
hide dispatch behind a nested callback, while the durable workflow queue tracks
top-level `call`, `follow`, and `fanout` boundaries.

## Examples

Run the example gate:

```bash
bash scripts/test-examples.sh
```

Workflow fixtures live in `examples/workflow/`:

- `orchestrator.ts` - plain `call`.
- `fanout-orchestrator.ts` - declaration-order `fanout`.
- `follow-orchestrator.ts` - HAL affordance `follow`.
- `durable-orchestrator.ts` - durable child call replay.
- `dsl-orchestrator.ts` - the canonical embedded workflow DSL path: proof
  first, one `Idempotency-Key` run key, one queued child boundary, and one
  inspectable recovery rail.
- `queued-orchestrator.ts` - queued durable child dispatch.
- `queued-fanout-orchestrator.ts` - `fanout()` inside a durable `run()` with
  `--workflow-queue`, so each child call is persisted before it runs, the
  same crash-tolerance a queued top-level `call()` already gets.
- `wait-signal-orchestrator.ts` - signal park/resume, including `signalAt`'s
  scheduled (rather than immediately-delivered) signal.
- `timeout-orchestrator.ts` - deterministic `stepWithTimeout`.
- `scope-orchestrator.ts` - `zigttp:scope`'s `using`/`ensure` resource
  cleanup and `scope`'s named nested-scope unwind-before-continuing.
- `saga-orchestrator.ts` - reverse-order saga compensation, including the
  `/compensation-fails` path where the `compensate` thunk itself fails: a
  terminal 500 requiring manual intervention, with no automatic retry.

For a guided crash drill, see
[First Durable Workflow](tutorials/first-durable-workflow.md).
