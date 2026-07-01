# First Durable Workflow

This tutorial runs the durable workflow examples from a clean checkout and shows
where to inspect replay state.

## Build

```bash
zig build
```

Use the built binary in the checkout:

```bash
export ZIGTTP=./zig-out/bin/zigttp
```

## Start A Durable Workflow

```bash
rm -rf ./.durable-demo
$ZIGTTP serve examples/workflow/durable-orchestrator.ts \
  --system examples/workflow/system.json \
  --durable ./.durable-demo \
  -p 3000
```

In another terminal:

```bash
curl -H 'Idempotency-Key: order-1' http://127.0.0.1:3000/
curl -H 'Idempotency-Key: order-1' http://127.0.0.1:3000/
```

The second request reuses the persisted completed response for `order-1`.

Inspect the durable directory:

```bash
find ./.durable-demo -maxdepth 3 -type f | sort
```

## Queue A Child Dispatch

Restart with the workflow queue:

```bash
rm -rf ./.durable-demo
$ZIGTTP serve examples/workflow/queued-orchestrator.ts \
  --system examples/workflow/system.json \
  --durable ./.durable-demo \
  --workflow-queue \
  -p 3000
```

Trigger the parent:

```bash
curl -H 'Idempotency-Key: queued-1' http://127.0.0.1:3000/
```

Inspect queue state:

```bash
find ./.durable-demo/workflow-queue -maxdepth 2 -type f | sort
```

Completed child responses appear under `done/`. If a child queue item exceeds
the attempt cap or its envelope is invalid, it moves to `dead/` and stays there
until an operator acts:

```bash
$ZIGTTP workflow-queue list --durable ./.durable-demo
$ZIGTTP workflow-queue show --durable ./.durable-demo <item-id>
$ZIGTTP workflow-queue replay --durable ./.durable-demo <item-id>
$ZIGTTP workflow-queue discard --durable ./.durable-demo <item-id>
```

## Signal Drill

```bash
rm -rf ./.durable-demo
$ZIGTTP serve examples/workflow/wait-signal-orchestrator.ts \
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
$ZIGTTP check examples/workflow/queued-orchestrator.ts --contract --json
$ZIGTTP verify http://127.0.0.1:3000 --json
```

Look for `durableWorkflowProofLevel`, `durableWorkflowRetrySafe`,
`durableWorkflowIdempotent`, `durableWorkflowFaultCovered`, and
`proofTrace.durable_workflow_*`.
