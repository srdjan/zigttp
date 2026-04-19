# `zigttp:fetch`

Web-standard outbound HTTP client with optional durable replay.

## Summary

```ts
import { fetch } from "zigttp:fetch";

export function handler(req) {
  // Non-durable: ordinary outbound call.
  const pong = fetch("https://example.com/ping");

  // Durable: replayable across crashes, keyed by idempotency header.
  const receipt = fetch("https://billing.example/charge", {
    method: "POST",
    headers: { "Idempotency-Key": req.headers.get("idempotency-key") },
    body: req.text(),
    durable: { key: req.headers.get("idempotency-key"), retries: 3 },
  });

  if (!receipt.ok) return Response.text("retry later", { status: 503 });
  return Response.json({ charged: true });
}
```

## API

```ts
fetch(url: string, init?: RequestInit): Response
```

`RequestInit` extends the WHATWG shape:

| Field | Type | Required | Notes |
|---|---|---|---|
| `method` | string | no | `"GET"`, `"POST"`, ... Default `"GET"`. |
| `headers` | `Record<string,string>` | no | Header names/values must be string. |
| `body` | string | no | Request body. Currently string-only. |
| `maxResponseBytes` | number | no | Cap on response read. |
| `durable` | `DurableOpts` | no | Enables oplog replay. See below. |

`DurableOpts`:

| Field | Type | Required | Default | Notes |
|---|---|---|---|---|
| `key` | string | yes | — | Stable idempotency key. Must be non-empty. |
| `retries` | integer | no | `0` | `[0, 16]`. Applies only to 5xx responses. |
| `backoff` | `"none" \| "exponential"` | no | `"none"` | Exponential starts at 100 ms, doubles per attempt, capped at ~6.4 s. |
| `ttl_s` | integer | no | `3600` | `(0, 604800]`. Cache lifetime in seconds. |

## Durable path

Durable fetches are wrapped in an oplog step persisted under
`<--durable>/fetch/<hash>.step`. The hash is
`sha256(key || method || url || body)`, so the same idempotency key with
different endpoints or bodies never collides.

Sequence on every call:

1. Parse `init.durable`. If absent, the call is ephemeral.
2. Compute the hash.
3. Read `<durable>/fetch/<hash>.step`. If it exists and
   `at_ms + ttl_s*1000 > now_ms`, reconstruct the `Response` from the
   cached JSON and return without touching the network.
4. On cache miss, execute the request. On status ≥ 500, sleep per
   the backoff policy and retry up to `retries` times.
5. Persist the final response if it is not 5xx. 5xx (including our
   own 599 transport errors) are **not** cached so a transient
   upstream failure doesn't freeze the caller on a stale response.

Cache entry schema (v1):

```json
{
  "v": 1,
  "at_ms": 1700000000000,
  "status": 200,
  "status_text": "OK",
  "content_type": "application/json",
  "body_b64": "…"
}
```

The body is base64-encoded so binary payloads round-trip unchanged.

## Requirements

- The server must be started with `--durable <dir>` for the durable
  path. Durable calls without `--durable` return an
  `"DurableNotConfigured"` error response.
- Outbound HTTP must be allowed: pass `--outbound-http` or
  `--outbound-host <host>` when starting the binary.
- The egress host extracted from the URL literal is recorded in the
  handler's contract and subject to any configured host allow-list.

## Runtime validation

Malformed `durable` options return a 599 error response synchronously,
not an exception:

| Error code | Cause |
|---|---|
| `InvalidDurable` | `durable.key` missing / non-string / empty |
| `InvalidDurable` | `durable.retries` not an integer, or outside `[0, 16]` |
| `InvalidDurable` | `durable.backoff` not `"none"` or `"exponential"` |
| `InvalidDurable` | `durable.ttl_s` not an integer, or outside `(0, 604800]` |
| `DurableNotConfigured` | `durable` set but server not started with `--durable` |

## Non-durable path

Without `durable`, `fetch` is a thin wrapper over the runtime's
`fetchSync` primitive: same egress-host policy, trace recording,
replay-verification, and connection pooling.

## Not yet

- Full header preservation in the cache entry (currently only
  `content-type` is restored).
- Streaming request/response bodies in the durable path (bounded
  bodies only, so the hash is stable).
- HTTP/2 and HTTP/3 (HTTP/1.1 only).
- Per-handler durable-fetch compile-time diagnostics (ZTSF00x). The
  runtime rejects malformed options at call time; compile-time is
  Phase T follow-up.

## Related

- [`zigttp:service`](./service.md) — named inter-handler RPC validated
  against `system.json` at build time. Distinct role; `fetch` is the
  general web-standard outbound client.
- [`zigttp:durable`](./durable.md) — durable workflow primitive
  (`run` / `step` / `waitSignal`). Shares the oplog storage layer
  with durable fetch.
