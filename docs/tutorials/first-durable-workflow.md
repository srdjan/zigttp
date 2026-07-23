# First Durable Workflow

This tutorial runs the durable workflow examples from a clean checkout and shows
where to inspect replay state.

## Build

```bash
zig build
```

Use the built binary in the checkout:

```bash
export ZTTP=./zig-out/bin/zttp
```

## Read The Proof First

Start with the compiler receipt before running the server:

```bash
$ZTTP check examples/workflow/dsl-orchestrator.ts --json
$ZTTP check examples/workflow/dsl-orchestrator.ts --contract
```

The first command emits the proof receipt to stdout. The second command writes
`contract.json`. Together they show the authoring receipt that the example
creates. Because the run key comes from the request, the durable workflow graph
can stay partial even when the handler's narrower `Spec` passes:

- `durable.workflow.proofLevel` describes whether the workflow graph is complete.
- `durable.workflow.properties.retrySafe` tracks whether automatic replay is safe.
- `durable.workflow.properties.idempotent` tracks whether completed response reuse is safe.
- `durable.workflow.properties.faultCovered` tracks whether the workflow has modeled recovery nodes.
- `proofTrace.durable_workflow_*` explains why the durable workflow guarantees
  are proven or still partial.

The example's embedded DSL is just disciplined naming around existing ZigTS
primitives: the `Idempotency-Key` becomes the run key, `durable.run()` owns the
parent workflow, and one top-level `workflow.call()` owns the queued child
boundary.

## Run The Canonical Workflow

```bash
rm -rf ./.durable-demo
$ZTTP serve examples/workflow/dsl-orchestrator.ts \
  --system examples/workflow/system.json \
  --durable ./.durable-demo \
  --workflow-queue \
  -p 3000
```

In another terminal:

```bash
curl -H 'Idempotency-Key: order-1' http://127.0.0.1:3000/
curl -H 'Idempotency-Key: order-1' http://127.0.0.1:3000/
```

The response names the child boundary (`workflow.call:greet`) and the child
path (`/workflow-dsl`). The second request reuses the persisted completed
response for `order-1`.

Inspect the durable directory:

```bash
find ./.durable-demo -maxdepth 3 -type f | sort
```

## Inspect The Queued Child

The canonical workflow already runs with the workflow queue. Trigger the parent:

```bash
curl -H 'Idempotency-Key: queued-1' http://127.0.0.1:3000/
```

Inspect queue state:

```bash
find ./.durable-demo/workflow-queue -maxdepth 2 -type f | sort
```

Completed child responses appear under `done/`. If a child queue item exceeds
the attempt cap or its envelope is invalid, it moves to `dead/` and stays there
until an operator acts. The parent workflow suspends (returning `202`) while
its child is dead-lettered rather than caching a terminal error, so replaying
or discarding the item and then retrying the *parent* request with the same
`Idempotency-Key` is what actually resolves the workflow -- `replay`/`discard`
alone only change the queue item's own state:

```bash
$ZTTP workflow-queue list --durable ./.durable-demo
$ZTTP workflow-queue show --durable ./.durable-demo <item-id>
$ZTTP workflow-queue replay --durable ./.durable-demo <item-id>
$ZTTP workflow-queue discard --durable ./.durable-demo <item-id>
curl -H 'Idempotency-Key: queued-1' http://127.0.0.1:3000/
```

## Artifact Matrix

| State | Trigger | Artifact | Caller result | Operator action | Proof/idempotency condition |
|---|---|---|---|---|---|
| Completed run | Repeat the same request key | Durable oplog plus workflow queue `done/` entry | Completed response reuses the recorded result | None | `idempotent` proof or matching `Idempotency-Key` |
| Queued child dead letter | Attempt cap or invalid queue envelope | `<durable>/workflow-queue/dead/<item-id>.json` | Parent stays suspended with `202` | `workflow-queue replay` or `discard`, then retry parent request | `retrySafe` proof or matching `Idempotency-Key` |
| Durable run dead letter | Recovery fails repeatedly | `<durable>/dead-runs/<id>.json` | Run stays quarantined | `durable dead-runs replay` or `discard` | Operator action chooses retry or permanent discard |

## Signal Drill

```bash
rm -rf ./.durable-demo
$ZTTP serve examples/workflow/wait-signal-orchestrator.ts \
  --durable ./.durable-demo \
  -p 3000
```

In another terminal:

```bash
curl -i -H 'Idempotency-Key: approval-1' http://127.0.0.1:3000/wait
curl -H 'Idempotency-Key: approval-1' http://127.0.0.1:3000/signal
curl -H 'Idempotency-Key: approval-1' http://127.0.0.1:3000/wait
```

The first call returns `202` with a signal wait. The signal call persists the
payload. The final wait call returns `200` with the approved payload.

## Crash Drill

Use the same signal drill and stop the server after the first `202`, before
sending `/signal`. Restart with the same `--durable` directory, send `/signal`,
then call `/wait` again. The run resumes from the durable signal claim and
returns the signal payload once.

For workflow queue recovery, stop the process while a child is leased, restart
with the same flags, and retry the parent request. The queue either completes
the child once, reclaims the expired lease, or leaves a dead letter for explicit
operator replay/discard.

## Verify Artifacts

Compile or deploy a durable handler, then inspect proof surfaces:

```bash
$ZTTP check examples/workflow/dsl-orchestrator.ts --json
$ZTTP check examples/workflow/dsl-orchestrator.ts --contract
$ZTTP verify http://127.0.0.1:3000 --json
```

Look for `durableWorkflowProofLevel`, `durableWorkflowRetrySafe`,
`durableWorkflowIdempotent`, `durableWorkflowFaultCovered`, and
`proofTrace.durable_workflow_*`.
