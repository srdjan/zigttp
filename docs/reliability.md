# Runtime Limits and Failure Behavior

Reference for the limits the zigttp runtime enforces and how it behaves when a
handler, the engine, or a request hits an edge. Defaults are stated; where a
limit is fixed in the binary that is called out.

## Request and resource limits

### Request body size

The HTTP server caps request bodies at **1 MB** (`max_body_size`, 1,048,576
bytes). The limit is fixed in the binary - there is no CLI flag to change it.

When a request body exceeds the cap the server **closes the connection without
sending an HTTP response**. The accept path logs the rejected request to
stderr. Clients see a dropped connection, not a `413 Payload Too Large`.
Returning `413` is the conventional behavior and is tracked as a gap; until it
lands, treat a closed connection on a large upload as the body-size limit.

### JavaScript runtime memory - `-m` / `--memory`

`zigttp serve -m <size>` sets a per-handler memory ceiling for the JavaScript
runtime. **The default is `0`, meaning no limit.** Sizes take a unit suffix:
`64k`, `1m`, `256mb`, `1g` (`k`/`kb` = 1024, `m`/`mb` = 1,048,576,
`g`/`gb` = 1,073,741,824); a bare number is bytes.

When `-m` is set and a handler allocates past the ceiling, the engine allocator
fails the allocation. The request returns `500` (see Failure behavior below)
and the server stays up.

### Runtime pool size - `-n` / `--pool`

`zigttp serve -n <count>` sets the number of isolated runtimes in the handler
pool. The default is derived from the host - `cpu_count * 2`, clamped to the
range **8 to 128**. A 14-core machine defaults to 28.

### Engine stack limits

The `zigts` engine enforces three fixed caps, none configurable:

| Limit | Value |
|-------|------:|
| Value stack | 1 MB (1,048,576 slots) |
| Call-stack depth | 1,024 frames |
| Saved-state depth | 1,024 |

Exceeding any of them raises a typed error rather than corrupting memory. Deep
or unbounded recursion in a handler hits the 1,024-frame call cap.

## Failure behavior

### Handler errors at runtime

When a handler throws, returns an error, or the engine raises a runtime fault
mid-request, the runtime catches it, folds every engine error into a single
`HandlerError`, and the HTTP server responds **`500 Internal Server Error`**.
The worker and the server process stay up and keep serving other requests - one
failed request never takes down the pool.

If the handler raised a JavaScript exception without a hard engine error, the
`500` body carries the exception message; otherwise the body is
`Internal Server Error`.

### Engine faults

`StackOverflow` (value stack), `CallStackOverflow` (call or saved-state depth),
and allocation failure under an `-m` ceiling all surface as typed errors, fold
into `HandlerError`, and produce a `500`. None of them panic or abort the
process.

### Process exit codes

The CLIs follow a consistent convention:

| Code | Meaning |
|-----:|---------|
| 0 | Success |
| 1 | Error - the command failed |
| 2 | Bad input - analyzer warnings, or an invalid test fixture |

Per command:

- `zigttp check` / `zigts check` - `0` clean, `1` on analyzer errors, `2` when
  only warnings were found.
- `zigttp build` - `0` on success, `1` on any build failure.
- `zigttp test` - `0` if all tests passed, `1` if one or more failed, `2` if
  the test fixture was invalid.

CI and scripts can rely on `0` versus non-zero, and can distinguish `1`
(failure) from `2` (bad input) when that matters.

### Panics

zigttp installs no custom panic handler, so a panic uses Zig's default - the
panic message and a stack trace print to stderr and the process aborts with a
non-zero status. A panic is always a bug: handler-level and engine-level faults
are handled as typed errors (above) and never panic. Report a panic through
[SECURITY.md](../SECURITY.md) if it looks security-relevant, otherwise as a
normal bug.
