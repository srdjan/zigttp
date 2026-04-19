# Virtual Modules

Virtual modules are native Zig APIs exposed to handlers as
`import { ... } from "zigttp:*"`. Calls have zero interpretation
overhead, carry effect annotations the compiler reads for handler
property classification, and feed contract extraction for runtime
sandboxing.

The modules are grouped by role. Each page documents exports,
compile-time proof interactions, runtime failures, and related
modules.

## Security

Trust boundary: input validation, credential handling, cryptographic
primitives.

- [`zigttp:auth`](./security/auth.md) - JWT signing/verification,
  Bearer parsing, webhook signatures, constant-time comparison.
- [`zigttp:crypto`](./security/crypto.md) - SHA-256, HMAC-SHA256,
  base64 encode/decode.
- [`zigttp:validate`](./security/validate.md) - JSON Schema registry
  with `validateJson` / `validateObject` / `coerceJson`.
- [`zigttp:decode`](./security/decode.md) - schema-backed ingress
  (`decodeJson` / `decodeForm` / `decodeQuery`).

## Data

Persistent or quasi-persistent state.

- [`zigttp:cache`](./data/cache.md) - in-process key-value cache
  with TTL and LRU.
- [`zigttp:sql`](./data/sql.md) - registered SQLite queries with
  build-time schema validation.
- [`zigttp:ratelimit`](./data/ratelimit.md) - sliding-window rate
  limiting backed by the cache layer.

## Net

Inbound and outbound traffic across process boundaries.

- [`zigttp:fetch`](./net/fetch.md) - web-standard outbound HTTP with
  optional durable replay.
- [`zigttp:service`](./net/service.md) - named internal
  service-to-service calls resolved via `system.json`.
- [`zigttp:websocket`](./net/websocket.md) - WebSocket protocol
  termination with rooms, attachments, and codec auto-replies.

## HTTP

HTTP-shaped utilities with no transport of their own.

- [`zigttp:router`](./http/router.md) - pattern-matching router with
  captured params.
- [`zigttp:http`](./http/http.md) - cookies, content negotiation,
  content-type parsing, CORS headers.
- [`zigttp:url`](./http/url.md) - URL parsing, query string handling,
  percent-encoding.

## Workflow

Control-flow primitives: durable runs, pipe guards, scoped cleanup,
structured concurrency.

- [`zigttp:durable`](./workflow/durable.md) - crash-safe execution
  with write-ahead oplog, timers, and signals.
- [`zigttp:compose`](./workflow/compose.md) - compile-time handler
  composition with `guard()` and the pipe operator.
- [`zigttp:scope`](./workflow/scope.md) - structured lifecycle
  management with deterministic LIFO cleanup.
- [`zigttp:io`](./workflow/io.md) - `parallel()` and `race()` for
  overlapping outbound I/O without async.

## Platform

Runtime primitives with no domain coupling.

- [`zigttp:env`](./platform/env.md) - environment variable access
  (literal keys feed runtime sandboxing).
- [`zigttp:id`](./platform/id.md) - UUID v4, ULID, nanoid.
- [`zigttp:log`](./platform/log.md) - structured line-delimited JSON
  logging to stderr.
- [`zigttp:text`](./platform/text.md) - HTML escape, slugify,
  truncate, mask.
- [`zigttp:time`](./platform/time.md) - ISO 8601 / HTTP date
  formatting, Unix-ms arithmetic.

## Effect classification

Every export carries an effect annotation used for handler property
classification:

- **Read**: all of `security/*`, `platform/env`, `cacheGet`,
  `cacheStats`, `sql`, `sqlOne`, `sqlMany`, `id/*`, `platform/time`.
- **Write**: `cacheSet`, `cacheDelete`, `cacheIncr`, `sqlExec`,
  `serviceCall`, `parallel`, `race`, `scope`, `using`, `ensure`,
  every durable export, `log/*`, `ratelimit/*`.
- **None**: `compose/*`, all of `platform/text`, `http/*`, `url/*`,
  `router/*`.

See `README.md` in the project root for the full feature matrix and
the runtime capability table per module.
