# Runtime Limits and Failure Behavior

Reference for limits enforced by the current runtime and the way failures are
reported.

## Request And Resource Limits

| Limit | Default / Value | Notes |
|---|---:|---|
| Request body size | 1 MB | Set with `--max-body-size`. Oversized requests return `413 Payload Too Large`. |
| Request read timeout | 30 seconds | Set with `ServerConfig.timeout_ms` or edge `timeoutMs`. Slow client reads return `408 Request Timeout`. |
| Handler execution timeout | 30 seconds | Set with `ServerConfig.timeout_ms` or edge `timeoutMs`. Slow handlers return `504 Gateway Timeout` and invalidate the pool slot. |
| JavaScript memory | no explicit limit | Set per runtime with `-m` / `--memory`. |
| Runtime pool size | `cpu_count * 2`, clamped 8-128 | Set with `-n` / `--pool`. |
| Live WebSocket connections | 1,024 | Set with `--max-websocket-connections`; `0` disables upgrades. Capacity is reserved before the `101` response and excess upgrades return `503`. |
| Value stack | 1 MB | 131,072 JSValue slots. |
| Call-stack depth | 1,024 frames | Deep recursion raises a typed error. |
| Saved-state depth | 1,024 | Exceeding the cap raises a typed error. |

Memory sizes accept suffixes: `64k`, `1m`, `256mb`, `1g`; a bare number is
bytes.

## Rate Limiting

The server does not enforce a global or per-IP rate limit. Put standalone
servers behind a reverse proxy or load balancer for network-layer limits. Use
`zigttp:ratelimit` inside handlers for application-level limits such as per API
key, user, or route.

## Failure Behavior

- Handler exceptions and engine runtime errors return `500`; the server process
  and other workers keep running. The body is proof-explained rather than a bare
  `Internal Server Error` (see [Proof-Explained 500s](#proof-explained-500s)).
- Allocation failure under an explicit `-m` ceiling returns `500`.
- Stack overflow and call-depth overflow return typed engine errors that fold
  into `500`.
- Request bodies above the fixed limit return `413` before the body is buffered.
- Slow request reads return `408`.
- Handler execution deadlines return `504` and the runtime slot is replaced
  before it is reused.
- Durable `stepWithTimeout()` returns `{ ok: false, error: "timeout" }` when
  the step deadline expires. Timers and signal waits inside that step obey the
  same deadline.
- Exhausted runtime pools return `503`.
- WebSocket upgrades above the configured live-connection cap return `503`
  before protocol ownership transfers. Each accepted connection currently owns
  one joinable kernel thread; graceful shutdown wakes and joins those workers
  before destroying handler or room state.
- A handler panic is caught by a setjmp/longjmp boundary around each handler
  invocation, returns `500`, and quarantines the pool slot. The server and other
  workers keep running. A panic in server infrastructure outside that boundary
  still aborts the process.

## Proof-Explained 500s

The runtime maps a fault to the proof chip that guards its class, then attributes
it against what the handler proved.

| Fault class | Trigger | Guarding chips |
|---|---|---|
| Type fault | `TypeError` / `NotCallable`: an un-narrowed optional dereferenced or called, or `.value` read off an unchecked `Result`. | `optional_safe`, `result_safe` |
| Non-Response return | The handler returned something that is not a `Response`. | `exhaustive_returns` |
| Unmapped | Everything else. | none |

When a guarding chip was not proven, that chip is the predicted cause:

```text
This response path was not proven optional_safe. A value was used without narrowing its type. at 12:9
```

When every guarding chip was proven and the handler still faulted, the fault
contradicts the proof, and the body says so:

```text
Possible soundness incident: the handler faulted on a path proven optional_safe/result_safe. This should be impossible — please report it.
```

An unmapped fault returns the plain `Internal Server Error` body. The trailing
`line:column` is appended only when the interpreter resolved a source location;
JIT-tier faults and JS-thrown exceptions often carry none.

`--incident-log <file>` is an opt-in JSONL sink for confirmed soundness
incidents. Each is appended as one line through a shared `O_APPEND` fd:

```json
{"kind":"soundness_incident","method":"GET","path":"/users/1","proven":["optional_safe","result_safe"],"detail":"TypeError"}
```

Writing is best-effort and never fails the request path. The always-on error log
is unaffected.

## Durable Replay And Workflow Queue

Durable replay is conservative when a validated contract is present:

- incomplete replay requires a proven `durable.workflow.properties.retrySafe`
  claim or a matching `Idempotency-Key`;
- completed response reuse requires proven
  `durable.workflow.properties.idempotent` or a matching `Idempotency-Key`;
- unproven replay returns a `599` JSON response with
  `DurableRetryUnproven` or `DurableIdempotencyUnproven`.

`--workflow-queue` persists durable workflow child dispatch under
`<durable>/workflow-queue`. Dead letters are visible operator state, not hidden
cleanup: list, inspect, replay, or discard them with `zigttp workflow-queue`.
`saga()` remains unsupported with `--workflow-queue`.

## Access Logs

Request logging is enabled by default and can be disabled with `-q` /
`--quiet` in `serve` and `dev`. Each completed request writes one structured
line to the Zig logger:

```text
access method=GET path=/api status=200 duration_ms=4 request_id=abc-123
```

`request_id` comes from the `X-Request-Id` request header only when it is a
short token-like value; malformed, empty, or oversized values are logged as `-`.

## Health And Readiness Probes

The server answers two built-in paths before the handler and before WebSocket
upgrade:

| Path | Meaning |
|---|---|
| `/_health` | `200 OK` with body `ok`; always answers if the process is running. |
| `/_readiness` | `200 OK` when the runtime pool has at least one free slot; `503` when all slots are in use. |

Orchestrators (Kubernetes, ECS, Fly.io) should use `/_health` for liveness and
`/_readiness` for readiness checks.

## Graceful Shutdown

On `SIGTERM` or `SIGINT`, the server stops accepting new connections and waits
for in-flight requests to finish, up to the configured request timeout. A second
signal terminates immediately.

Container stop (`docker stop`, `systemctl stop`, ECS task drain) will complete
in-flight requests before the process exits.

## Exit Codes

| Code | Meaning |
|---:|---|
| 0 | Success |
| 1 | Command failed or analyzer found errors |
| 2 | Bad input or analyzer warnings, where the command distinguishes warnings |

Common cases:

- `zigttp check` / `zigts check`: `0` clean, `1` errors, `2` warning-only.
- `zigttp build`: `0` success, `1` build failure.
- `zigttp test`: `0` all tests passed, `1` test failure, `2` invalid fixture.

Security-relevant crashes or bypasses should be reported through
[SECURITY.md](../SECURITY.md).
