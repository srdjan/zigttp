# `zigttp:id`

ID generation: UUID v4, ULID, and nanoid. All three draw from the
runtime's random source and wall clock.

## Summary

```ts
import { uuid, ulid, nanoid } from "zigttp:id";

function handler(req) {
  return Response.json({
    requestId: uuid(),
    sortKey: ulid(),
    shortId: nanoid(10),
  });
}
```

## API

| Export | Signature | Returns | Purpose |
|---|---|---|---|
| `uuid` | `uuid(): string` | 36-char string | RFC 4122 v4 UUID (random). |
| `ulid` | `ulid(): string` | 26-char string | Timestamp-prefixed lexicographically sortable ID. |
| `nanoid` | `nanoid(length): string` | string | URL-safe random ID of the given length. |

## Compile-time proof

- No contract extractions. ID calls do not declare egress, env, or
  routes.
- All three are `effect = read` and labeled `internal` (not
  user-derived), so they pass flow analysis into response bodies
  without sanitization.
- None are declared pure: each call draws fresh randomness. The
  canonicalizer does not collapse repeated calls.

## Runtime failures

- `nanoid(length)` throws on non-integer or non-positive length.
- `uuid()` and `ulid()` take no arguments; extra arguments throw
  with a TypeError.

## Requirements

- The runtime provides the random and clock sources.
- IDs are not guaranteed unique across processes. For cross-process
  coordination, prefer [`zigttp:sql`](../data/sql.md) with a
  database-assigned primary key.

## Related

- [`zigttp:crypto`](../security/crypto.md) - SHA-256 is the right
  primitive for content-addressed IDs derived from a payload.
