# `zigttp:service`

Named internal service-to-service calls resolved through `system.json`
at build time and at runtime.

## Summary

```ts
import { serviceCall } from "zigttp:service";

function handler(req) {
  const user = serviceCall("users", "GET /api/users/:id", {
    params: { id: "123" },
  });

  if (user.status !== 200) {
    return Response.json({ error: "user service unavailable" }, { status: 502 });
  }

  return Response.json(user.json());
}
```

Start the server with `--system <FILE>` so the service registry is
loaded before the handler runs:

```bash
zigttp serve --system examples/system/system.json examples/system/gateway.ts
```

## API

```ts
serviceCall(service: string, route: string, init?: RequestInit): Response
```

| Argument | Type | Required | Notes |
|---|---|---|---|
| `service` | string | yes | Name from `system.json#handlers[].name`. |
| `route` | string | yes | `"METHOD /path"`. Path may contain `:param` placeholders. |
| `init` | object | no | Per-call overrides (see below). |

`init` fields:

| Field | Type | Purpose |
|---|---|---|
| `params` | `Record<string,string>` | Substitutes `:path` placeholders. |
| `query` | `Record<string,string>` | Appended as a query string. |
| `headers` | `Record<string,string>` | Request headers. |
| `body` | string | String request body. |

`serviceCall()` returns the same response shape as `fetchSync()`:
`status`, `ok`, `headers.get(name)`, `text()`, `json()`.

## `system.json`

Each handler in the system declares a stable `name` and a `baseUrl`.
The name is what `serviceCall()` resolves; the base URL is prepended
to the route.

```json
{
  "version": 1,
  "handlers": [
    { "name": "gateway", "path": "examples/system/gateway.ts", "baseUrl": "https://gateway.internal" },
    { "name": "users",   "path": "examples/system/users.ts",   "baseUrl": "https://users.internal" },
    { "name": "orders",  "path": "examples/system/orders.ts",  "baseUrl": "https://orders.internal" }
  ]
}
```

Duplicate names abort startup; unknown names throw at the call site.

## Compile-time proof

With `zigts check --system <FILE>` or `zigts compile --system <FILE>`,
literal `serviceCall()` sites become payload-aware:

- `status` narrows to the target route's proven status codes.
- `.json()` returns the target route's proven JSON type when a single
  response schema matches.
- When different status codes produce different schemas, narrow on
  `resp.status` before calling `.json()`.
- Required path, query, header, and body inputs are validated against
  the target route contract.

Non-literal service names or routes fall back to a dynamic call with
no payload narrowing. The contract honestly reports them as dynamic
so proof downgrades are visible in `contract.json`.

## Contract extraction

Every literal `serviceCall()` is captured under the `service_call`
contract section: service name, route signature, and statically
proven `params` / `query` / `headers` / `body` keys. This feeds
`zigts link <system.json>`, which verifies the full internal call
graph and emits `system-contract.json` plus `system-report.txt`
with explicit payload fields (`payloadProven`, `payloadCompatible`,
`payloadDetail`).

`zigts rollout <old-system.json> <new-system.json>` extends the proof
across time: it compiles and links both systems, checks mixed-version
states, and emits `rollout-plan.json` with the smallest rollout phases
it can prove.

## Runtime failures

`serviceCall()` throws synchronously on:

- Missing `--system` flag (`"serviceCall() requires --system <FILE>"`).
- Unknown service name.
- Non-string `service` or `route`.
- `init` present but not an object.

Transport-level failures (timeout, connection refused, oversized
response) surface the same way `fetchSync()` surfaces them: a `599`
response whose body contains `{ error, details }`. Handlers check
`status` rather than catching.

## Related

- [`zigttp:fetch`](./fetch.md) - general web-standard outbound client.
  Distinct role; `serviceCall()` is for named zigttp-to-zigttp edges
  backed by `system.json`.
- `zigts link <system.json>` - build-time proof of the full internal
  service graph.
- `zigts rollout` - multi-handler rollout planner built on the same
  proof pipeline.
