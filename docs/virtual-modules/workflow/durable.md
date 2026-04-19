# `zigttp:durable`

Crash-safe, replay-safe durable execution backed by a write-ahead
oplog. Wrap work in `run(key, fn)`; each `step`, `sleep`, and
`waitSignal` inside it persists to disk before returning control to
the handler.

## Summary

```ts
import { run, step, waitSignal } from "zigttp:durable";

function handler(req) {
  if (req.path !== "/orders/42") {
    return Response.json({ error: "not found" }, { status: 404 });
  }

  return run("order:42", () => {
    const draft = step("createDraft", () => ({
      id: 42,
      status: "draft",
      totalCents: 1999,
    }));

    const approval = waitSignal("approved");

    const confirmed = step("confirmOrder", () => ({
      id: draft.id,
      status: "confirmed",
      totalCents: draft.totalCents,
      approval,
    }));

    return Response.json(confirmed);
  });
}
```

Start the server with `--durable <dir>`:

```bash
zigttp serve examples/durable/approval.ts --durable ./oplogs
```

A suspended run returns `202 Accepted` with a JSON body describing
the wait. Delivering the signal (via `signal()` from another request
or the sibling `zigttp-admin`) resumes the run from its last
persisted step.

## API

| Export | Signature | Purpose |
|---|---|---|
| `run` | `run(key, fn): Response` | Wrap a unit of work with an idempotency key. |
| `step` | `step(name, fn): unknown` | Persist a step result before returning. |
| `stepWithTimeout` | `stepWithTimeout(name, timeoutMs, fn): Result<unknown, "timeout">` | Step with a deadline. |
| `sleep` | `sleep(ms): undefined` | Suspend the run for `ms` milliseconds. |
| `sleepUntil` | `sleepUntil(unixMs): undefined` | Suspend until a wall-clock deadline. |
| `waitSignal` | `waitSignal(name): unknown` | Suspend until a signal arrives. |
| `signal` | `signal(key, name, payload): boolean` | Deliver a signal to a waiting run. |
| `signalAt` | `signalAt(key, name, unixMs, payload): boolean` | Schedule a signal for future delivery. |

`run()` takes a stable idempotency key. Completed runs are
deduplicated: a second call with the same key returns the recorded
response without re-executing the body.

`step()` persists its return value under `name` before returning. On
crash recovery, recorded results replay without re-running the step
body, so any fetch, SQL write, or other side effect runs exactly
once per key.

`stepWithTimeout()` returns `{ ok: true, value }` on completion or
`{ ok: false, error: "timeout" }` when the deadline is exceeded.
Check `.ok` before reading `.value`.

`signal()` returns `true` when the payload was enqueued for a
waiting run, `false` when the key is unknown. `signalAt()` accepts
a Unix-ms timestamp; the background scheduler delivers the signal
when the timer fires.

## Oplog layout

The oplog lives under the directory passed to `--durable`:

```
<durable>/
  runs/<key>/oplog.jsonl
  signals/<key>/<name>.json
  scheduled/<unix_ms>-<key>-<name>.json
  ws/<id>.att            (WebSocket attachments)
  fetch/<hash>.step      (zigttp:fetch durable cache)
```

Every entry is line-delimited JSON so recovery is a streaming scan.
The background scheduler polls `scheduled/` for ready timers and
`signals/` for ready deliveries using the same replay-safe path as
recovery.

## Compile-time proof

Handlers that use `zigttp:durable` emit a `durable` section in
`contract.json` with:

- `workflowId` derived from the handler and `run()` key.
- `proofLevel` for the workflow graph.
- `nodes` and `edges` covering every `step`, `sleep`, and
  `waitSignal` reachable from the run body.

Durable handlers refuse `--watch --prove` live swap because replay
state depends on handler identity. Swap them by restarting the
process.

## Inspection

The runtime does not expose admin routes. Use the sibling
`zigttp-admin` service against the same durable directory:

```bash
cd ../zigttp-admin
deno task start --durable-dir ../zigttp/.zigttp-durable \
  --contract ../zigttp/contract.json
```

Exposed routes:

- `GET /_zigttp/durable/contract` - workflow graph from the
  embedded contract.
- `GET /_zigttp/durable/runs` - list every persisted run.
- `GET /_zigttp/durable/runs/:key` - run state and step history.
- `POST /_zigttp/durable/runs/:key/signals/:name` - deliver a
  signal with a JSON body.

```bash
curl -X POST \
  http://127.0.0.1:8787/_zigttp/durable/runs/order%3A42/signals/approved \
  -H 'content-type: application/json' \
  -d '{"approvedBy":"ops"}'
```

## Runtime failures

Every export throws synchronously when `--durable` is not set. Other
failure modes are reported as exceptions the handler cannot catch
(zigts has no `try/catch`); the server converts them into `500`
responses with a structured body:

| Cause | Message |
|---|---|
| Missing `--durable` | `"run() requires --durable <DIR>"` (and equivalents). |
| Wrong argument types | `"step() name must be a string"`, etc. |
| Oplog write failure | `"durable oplog write failed"` with disk error detail. |

## Requirements

- Start the server with `--durable <dir>`. The directory is created
  on first use.
- For cross-host or cross-process signal delivery, point both sides
  at the same filesystem path (or a shared volume).
- `run()` callbacks must be deterministic given the recorded step
  and signal results. Non-deterministic reads outside `step()` break
  replay.

## Not yet

- Multi-process concurrent writers. The oplog assumes one writer
  per run.
- Distributed signal delivery across hosts without a shared
  filesystem.
- Cross-handler durable composition (one `run()` spawning another).

## Related

- [`zigttp:fetch`](../net/fetch.md) - durable HTTP cache
  (`<durable>/fetch/`) uses the same oplog layout.
- [`zigttp:websocket`](../net/websocket.md) - attachments
  (`<durable>/ws/`) use the same directory root.
- `zigttp-admin` - out-of-band inspection and signal delivery for
  durable runs.
