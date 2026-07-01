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

## Workflow Queue

`zigttp:workflow` dispatches to co-located handlers from `--system <file>`.
Inside durable `run()`, completed `call`, `follow`, and `fanout` child
responses are recorded so replay does not re-dispatch completed children.

Add `--workflow-queue` to persist top-level child dispatch before it runs:

```bash
zigttp serve examples/workflow/queued-orchestrator.ts \
  --system examples/workflow/system.json \
  --durable ./.durable \
  --workflow-queue
```

The queue lives under `<durable>/workflow-queue`. Items move through
`pending/`, `leased/`, `done/`, and `dead/`. Leases expire and can be reclaimed.
After the attempt cap, the item moves to a dead letter. Dead letters block new
enqueue for the same item id until an operator replays or discards them:

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
- `queued-orchestrator.ts` - queued durable child dispatch.
- `wait-signal-orchestrator.ts` - signal park/resume.
- `timeout-orchestrator.ts` - deterministic `stepWithTimeout`.

For a guided crash drill, see
[First Durable Workflow](tutorials/first-durable-workflow.md).
