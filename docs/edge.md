# Edge runtime

`zigttp edge` runs an in-process edge that loads multiple handler pools
behind a single listener and routes incoming requests to a named target
based on host, method, and path prefix. Useful when several handlers
share one bind address - multitenant routing, internal request fan-out,
or A/B routing during a migration.

```bash
zigttp edge --config zigttp.edge.json    # explicit path
zigttp edge -c zigttp.edge.json          # short form
zigttp edge zigttp.edge.json             # positional
zigttp edge                              # defaults to ./zigttp.edge.json
```

## Config

The config file is JSON with three required sections - `listener`,
`handlers`, `routes`:

```json
{
    "listener": {
        "host": "127.0.0.1",
        "port": 8080,
        "protocol": "http"
    },
    "handlers": [
        { "name": "api", "entry": "src/handler.ts", "pool": 8 },
        { "name": "admin", "entry": "src/admin.ts", "pool": 2 }
    ],
    "routes": [
        { "host": "*", "method": "*", "pathPrefix": "/admin", "target": "admin" },
        { "host": "*", "method": "*", "pathPrefix": "/", "target": "api" }
    ]
}
```

### `listener`

| Field      | Type     | Default       | Notes                                            |
|------------|----------|---------------|--------------------------------------------------|
| `host`     | string   | required      | IPv4 bind address                                |
| `port`     | integer  | required      | Port to listen on                                |
| `protocol` | string   | `"http"`      | `"http"` or `"https"` - HTTPS is parsed but TLS termination is post-v1, returns an error today |

For HTTPS today, run zigttp edge behind a trusted front proxy (nginx,
Caddy, a load balancer) that terminates TLS upstream.

### `handlers`

An array of named handler pools. Each entry verifies its handler at
load time using the same pipeline as `zigttp build`.

| Field                  | Type   | Default | Notes                                          |
|------------------------|--------|---------|------------------------------------------------|
| `name`                 | string | required| Referenced by routes via `target` or `targets` |
| `entry`                | string | required| Path to handler `.ts`/`.tsx`/`.js`             |
| `pool`                 | integer| auto    | Runtime pool size for this handler             |
| `poolWaitTimeoutMs`    | integer| `5000`  | Max wait for a pool slot before 503            |
| `lifecycle`            | string | inferred| `ephemeral` \| `bounded` \| `reuse`            |
| `runtimeConfig`        | object | `{}`    | Per-handler runtime overrides (sqlite, durable, outbound) |

### `routes`

Match rules in declaration order; first match wins. Each route either
points at a single named handler via `target` or distributes across
multiple handlers via `targets` with a load policy.

| Field         | Type    | Default      | Notes                                          |
|---------------|---------|--------------|------------------------------------------------|
| `host`        | string  | `"*"`        | Exact host match or `"*"` for any              |
| `method`      | string  | `"*"`        | HTTP method or `"*"` for any                   |
| `pathPrefix`  | string  | `"/"`        | Match if the request path starts with this     |
| `target`      | string  | (one form)   | Single handler name. Mutually exclusive with `targets`. |
| `targets`     | array   | (one form)   | Multiple `{ handler, weight }` entries         |
| `policy`      | string  | `"least_busy"`| `"least_busy"` or `"round_robin"`             |

A multi-target route example:

```json
{
    "host": "*", "method": "GET", "pathPrefix": "/",
    "targets": [
        { "handler": "api-v1", "weight": 9 },
        { "handler": "api-v2", "weight": 1 }
    ],
    "policy": "round_robin"
}
```

`weight` only matters for `round_robin`; `least_busy` ignores it and
picks the target with the fewest in-flight requests.

### Top-level options

| Field          | Type    | Default            | Notes                                           |
|----------------|---------|--------------------|-------------------------------------------------|
| `maxBodySize`  | integer | `1048576` (1 MiB)  | Reject larger bodies with 413                   |
| `maxHeaders`   | integer | `64`               | Reject larger header counts with 400            |
| `timeoutMs`    | integer | `30000`            | Per-request timeout including handler execution |

## Per-handler verification

Each `handlers[].entry` is verified before the listener binds. A failure
in any handler aborts edge startup with a non-zero exit code; no partial
loads. The edge only listens after every handler in the config is
provably safe.

## Response headers

The edge adds one header to every response:

```
X-Zigttp-Edge-Target: <handler name>
```

This identifies which named pool handled the request, for debugging or
log correlation.

## Limitations (v1)

- TLS termination is not implemented. Setting `protocol: "https"` parses
  but exits with an error at startup. Front-proxy for now.
- No upstream health checks. A handler that crashes during compile will
  fail the whole edge startup; a handler that fails during runtime
  surfaces as a 5xx for that route's requests until the runtime recovers
  via the existing pool replacement path.
- Per-route rate limiting and circuit breakers are not built in. Compose
  with `zigttp:ratelimit` inside individual handlers, or front the edge
  with a proxy that does.

These are tracked as post-v1 roadmap items in `docs/roadmap/frontier.md`.
