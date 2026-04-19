# `zigttp:ratelimit`

Per-key sliding-window rate limiting backed by the cache layer.
Returns a Result so handlers can branch on exceedance without
exceptions.

## Summary

```ts
import { rateCheck } from "zigttp:ratelimit";

function handler(req) {
  const ip = req.headers["x-forwarded-for"] ?? "anon";
  const check = rateCheck(ip, 60, 100);
  if (!check.ok) {
    return Response.json(
      { error: "rate limit", resetMs: check.error.resetMs },
      { status: 429 },
    );
  }
  return Response.json({ ok: true });
}
```

## API

| Export | Signature | Effect | Purpose |
|---|---|---|---|
| `rateCheck` | `rateCheck(key, windowSeconds, limit): Result<{ remaining, resetMs }, { resetMs, retryAfter }>` | write | Atomically increment counter for `key` and return `ok: false` when the window exceeded `limit`. |
| `rateReset` | `rateReset(key): boolean` | write | Clear the counter for `key`. Returns `true` when a counter existed. |

`rateCheck` is last-write-wins atomic, so concurrent handlers sharing
a key converge on the same counter value.

## Compile-time proof

- The literal first argument to `rateCheck()` becomes the handler's
  `rate_limit_namespace` in `contract.json`. Deploy tooling and the
  OWASP A04 coverage line read this field to confirm a rate-limit
  guard exists.
- `rateCheck()` is marked `failure_severity = critical`. A handler
  that ignores `.ok` fails the handler verifier.

## Runtime failures

- `{ ok: false, error }` when the window is exceeded. Never throws
  on normal use.
- Non-string `key`, non-numeric window or limit throw synchronously.

## Requirements

- The cache layer is always available in-process. For cross-process
  rate limiting, run a shared cache or move the counter into
  [`zigttp:sql`](./sql.md).

## Related

- [`zigttp:cache`](./cache.md) - `cacheIncr()` is the primitive
  `rateCheck` builds on; use it directly for non-time-windowed
  counters.
