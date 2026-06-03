# Runtime Limits and Failure Behavior

Reference for limits enforced by the current runtime and the way failures are
reported.

## Request And Resource Limits

| Limit | Default / Value | Notes |
|---|---:|---|
| Request body size | 1 MB | Fixed in the binary. Oversized requests return `413 Payload Too Large`. |
| JavaScript memory | no explicit limit | Set per runtime with `-m` / `--memory`. |
| Runtime pool size | `cpu_count * 2`, clamped 8-128 | Set with `-n` / `--pool`. |
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

- Handler exceptions and engine runtime errors return `500 Internal Server
  Error`; the server process and other workers keep running.
- Allocation failure under an explicit `-m` ceiling returns `500`.
- Stack overflow and call-depth overflow return typed engine errors that fold
  into `500`.
- Request bodies above the fixed limit return `413` before the body is buffered.
- A panic is a bug. Zig's default panic handler prints a message and stack trace
  to stderr, then aborts the process.

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
