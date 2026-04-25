# Virtual Modules

Virtual modules are native Zig APIs exposed to handlers as
`import { ... } from "zigttp:*"`. Calls have zero interpretation
overhead, carry effect annotations the compiler reads for handler
property classification, and feed contract extraction for runtime
sandboxing.

The modules are grouped by role. Each page documents exports,
compile-time proof interactions, runtime failures, and related
modules. The implementation registry is
`packages/zigts/src/builtin_modules.zig`; public module specs live under
`packages/modules/module-specs/`.

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

- **Read**: `env`, `cacheGet`, `cacheStats`, `id/*`,
  `websocket.deserializeAttachment`, `websocket.getWebSockets`, and
  `websocket.roomFromPath`.
- **Write**: `cacheSet`, `cacheDelete`, `cacheIncr`, `sqlExec`, `fetch`,
  `serviceCall`, `parallel`, `race`, `scope`, `using`, `ensure`, every
  durable export, `websocket.send`, `websocket.close`,
  `websocket.serializeAttachment`, `websocket.setAutoResponse`, `log/*`, and
  `ratelimit/*`.
- **None**: `auth/*`, `crypto/*`, `decode/*`, `validate/*`, `compose/*`,
  `platform/text`, `platform/time`, `http/*`, `url/*`, `router/*`, `sql`,
  `sqlOne`, and `sqlMany`.

For internal capability governance, see
[`../internals/capabilities.md`](../internals/capabilities.md).
