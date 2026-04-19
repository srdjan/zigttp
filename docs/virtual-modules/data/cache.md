# `zigttp:cache`

In-process key-value cache with TTL, LRU eviction, and per-namespace
counters. Proven namespaces feed runtime sandboxing.

## Summary

```ts
import { cacheGet, cacheSet, cacheIncr } from "zigttp:cache";

function handler(req) {
  const cached = cacheGet("pages", req.url);
  if (cached !== undefined) return Response.json(JSON.parse(cached));

  cacheIncr("metrics", "requests", 1, 0);

  const page = { path: req.url, ts: Date.now() };
  cacheSet("pages", req.url, JSON.stringify(page), 60);
  return Response.json(page);
}
```

## API

| Export | Signature | Effect | Purpose |
|---|---|---|---|
| `cacheGet` | `cacheGet(namespace, key): string \| undefined` | read | Read a cached value. |
| `cacheSet` | `cacheSet(namespace, key, value, ttlSeconds): boolean` | write | Store a value with a TTL. |
| `cacheDelete` | `cacheDelete(namespace, key): boolean` | write | Remove a key. |
| `cacheIncr` | `cacheIncr(namespace, key, delta, ttlSeconds): number` | write | Atomic increment; creates the key if absent. |
| `cacheStats` | `cacheStats(namespace): { hits, misses, size }` | read | Per-namespace counters. |

TTLs are in seconds. Pass `0` to skip TTL (entries still subject to
LRU eviction when the namespace capacity is reached).

## Compile-time proof

- Every literal namespace feeds the `cache.namespaces` contract
  section. `cache.dynamic` is `false` when all calls use string
  literals.
- With a proven namespace list, the runtime sandbox restricts cache
  access to exactly those names. A handler calling
  `cacheGet(userInput, ...)` reports `cache.dynamic: true` and the
  namespace set stays open.

## Runtime failures

- `cacheSet()` returns `false` when the namespace policy blocks the
  write. `cacheGet()` returns `undefined` for missing or policy-
  denied reads.
- `cacheIncr()` returns the new value. It throws on non-numeric
  deltas.

## Requirements

- No external store; data lives in the server process. Cache does
  not survive restarts.
- Multi-process deployments do not share cache state. Use
  [`zigttp:sql`](./sql.md) for cross-process shared state.

## Related

- [`zigttp:ratelimit`](./ratelimit.md) - sliding-window rate limiting
  built on top of `cacheIncr`.
- [`zigttp:sql`](./sql.md) - durable relational storage when cache
  eviction is unacceptable.
