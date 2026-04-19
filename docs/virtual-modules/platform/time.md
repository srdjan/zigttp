# `zigttp:time`

Time formatting and arithmetic on Unix milliseconds. Pure functions
that operate on caller-supplied timestamps; the module does not read
the clock itself (use `Date.now()` for that).

## Summary

```ts
import { formatIso, formatHttp, parseIso, addSeconds } from "zigttp:time";

function handler(req) {
  const now = Date.now();
  const expiresAt = addSeconds(now, 3600);

  return Response.json({
    iso: formatIso(now),
    http: formatHttp(now),
    expiresIso: formatIso(expiresAt),
    parsedBack: parseIso(formatIso(now)),
  });
}
```

## API

| Export | Signature | Returns | Purpose |
|---|---|---|---|
| `formatIso` | `formatIso(unixMs): string` | ISO 8601 string | `"2026-04-19T12:34:56.789Z"`. |
| `formatHttp` | `formatHttp(unixMs): string` | HTTP date string | `"Sun, 19 Apr 2026 12:34:56 GMT"` for `Last-Modified`, `Expires`, etc. |
| `parseIso` | `parseIso(input): number` | Unix milliseconds | Parse an ISO 8601 string to Unix ms. Throws on malformed input. |
| `addSeconds` | `addSeconds(unixMs, seconds): number` | Unix milliseconds | Shift a timestamp by a signed integer number of seconds. |

All four are pure; canonicalization may collapse repeated calls on
literal arguments.

## Compile-time proof

- No contract extractions. Time formatting does not declare egress,
  env, or routes.
- Because none of the calls reads the clock, handlers using only
  this module (and no `Date.now()` or `Math.random()`) stay
  `deterministic`.

## Runtime failures

- `parseIso()` throws on malformed input (missing `T`, bad offset,
  non-numeric fields).
- Non-numeric arguments throw a TypeError.

## Related

- [`Date.now()`](../../user-guide.md) - built-in JavaScript primitive
  for the current wall clock in milliseconds.
- [`zigttp:log`](./log.md) - log entries carry an ISO timestamp
  automatically; `formatIso()` is for timestamps inside handler
  payloads.
