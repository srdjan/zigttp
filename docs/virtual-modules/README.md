# Virtual Modules

Virtual modules are native Zig APIs imported from `zttp:*` specifiers. The
current built-in set is registered in `packages/zts/src/builtin_modules.zig`;
public module specs live in `packages/modules/module-specs/`.

Use `zttp modules --json` for the live export list from the built binary.

## Module Catalog

| Module | Exports | Capabilities |
|---|---|---|
| `zttp:auth` | `parseBearer`, `jwtVerify`, `jwtSign`, `verifyWebhookSignature`, `timingSafeEqual` | `crypto`, `clock` |
| `zttp:cache` | `cacheGet`, `cacheSet`, `cacheDelete`, `cacheIncr`, `cacheStats` | `clock`, `policy_check` |
| `zttp:compose` | `guard`, `pipe` | none |
| `zttp:crypto` | `sha256`, `hmacSha256`, `base64Encode`, `base64Decode` | `crypto` |
| `zttp:decode` | `decodeJson`, `decodeForm`, `decodeQuery`, `decodeFormMultipart` | none |
| `zttp:durable` | `run`, `step`, `stepWithTimeout`, `sleep`, `sleepUntil`, `waitSignal`, `signal`, `signalAt` | `runtime_callback` |
| `zttp:env` | `env` | `env`, `policy_check` |
| `zttp:fetch` | `fetch`, `fetchWithRetry` | `network`, `runtime_callback` |
| `zttp:http` | `parseCookies`, `setCookie`, `negotiate`, `parseContentType`, `cors` | none |
| `zttp:id` | `uuid`, `ulid`, `nanoid` | `clock`, `random` |
| `zttp:io` | `parallel`, `race` | `runtime_callback` |
| `zttp:log` | `logDebug`, `logInfo`, `logWarn`, `logError` | `clock`, `stderr` |
| `zttp:queue` | `send`, `request`, `receive`, `ack`, `nack`, `reply` | `runtime_callback` |
| `zttp:ratelimit` | `rateCheck`, `rateReset` | `clock` |
| `zttp:router` | `routerMatch` | none |
| `zttp:scope` | `scope`, `using`, `ensure` | `runtime_callback` |
| `zttp:service` | `serviceCall` | `network`, `filesystem`, `runtime_callback` |
| `zttp:sql` | `sql`, `sqlOne`, `sqlMany`, `sqlExec` | `sqlite`, `policy_check` |
| `zttp:text` | `escapeHtml`, `unescapeHtml`, `slugify`, `truncate`, `mask` | none |
| `zttp:time` | `formatIso`, `formatHttp`, `parseIso`, `addSeconds` | none |
| `zttp:url` | `urlParse`, `urlSearchParams`, `urlEncode`, `urlDecode` | none |
| `zttp:validate` | `schemaCompile`, `validateJson`, `validateObject`, `coerceJson`, `schemaDrop` | none |
| `zttp:websocket` | `send`, `close`, `serializeAttachment`, `deserializeAttachment`, `getWebSockets`, `setAutoResponse` | `clock`, `runtime_callback`, `network`, `filesystem`, `policy_check`, `websocket` |
| `zttp:workflow` | `call`, `saga`, `fanout`, `follow` | `runtime_callback` |

## Common Usage

```ts
import { env } from "zttp:env";
import { routerMatch } from "zttp:router";
import { sha256 } from "zttp:crypto";

function handler(req: Request): Response {
    const routes = { "GET /users/:id": true };
    const match = routerMatch(routes, req);
    if (match) {
        return Response.json({
            id: match.params.id,
            tokenHash: sha256(env("API_TOKEN") ?? ""),
        });
    }
    return Response.text("Not Found", { status: 404 });
}
```

## Runtime Requirements

Most modules work without extra flags. Modules that cross a process, disk, or
durability boundary need configuration:

| Module | Requirement |
|---|---|
| `zttp:env` | Proven literal env vars are checked at startup unless `--no-env-check` is passed. |
| `zttp:sql` | Run with `--sqlite <file>` for execution. Use `--sql-schema <schema.sql>` or `-Dsql-schema=<schema.sql>` for schema validation. |
| `zttp:fetch` | Enable outbound HTTP with `--outbound-http` or one or more `--outbound-host <host>` flags. Durable fetch also needs `--durable <dir>`. |
| `zttp:service` | Run with `--system <file>` or set `"system"` in `zttp.json`. |
| `zttp:workflow` | Run with `--system <file>`: the orchestrator dispatches to co-located sub-handlers in-process. Every local handler path in the manifest must be readable at startup. Durable `call`/`saga`/`fanout`/`follow` replay also needs `--durable <dir>`. Add `--workflow-queue` to force durable top-level `call`, `follow`, and `fanout` child dispatch through the persisted queue; `saga()` is rejected in that mode because step closures would hide direct dispatch inside the flat oplog. Dead letters stay under `<durable>/workflow-queue/dead` until `zttp workflow-queue replay` or `discard`. |
| `zttp:queue` | Run with `--actor-queue` for server-owned in-memory actor mailboxes. Exports `send`, `request`, `receive`, `ack`, `nack`, and `reply`; all return `Result` objects. Messages own JSON snapshots of payloads and remain retained until ack, requeue, or dead-lettering, but the current backend is not durable across process restart. This is separate from the persisted workflow queue used by `--workflow-queue`. |
| `zttp:durable` | Run with `--durable <dir>`. Durable workflow proof properties appear under `durable.workflow.properties` in `contract.json` and under `durableWorkflow*` fields in proof receipts. |
| `zttp:websocket` | Run through the server WebSocket gateway. `setAutoResponse(ws, request, response)` installs a codec-level reply; room keys are request paths, and `getWebSockets(room)` returns every live peer in that room. Peer closes dispatch `onClose(ws, code, reason)` with parsed close metadata. |
| `zttp:log` | Writes structured lines to stderr. Do not log raw secrets, tokens, or PII. |

## Effects

The compiler reads each export's effect annotation when deriving handler
properties and runtime policy.

| Effect | Meaning | Examples |
|---|---|---|
| `none` | Pure or analysis-only operation. | `sha256`, `routerMatch`, `parseCookies`, `decodeJson` |
| `read` | Reads runtime state without mutating it. | `env`, `cacheGet`, `cacheStats`, `deserializeAttachment` |
| `write` | Mutates state, performs I/O, or schedules runtime callbacks. | `fetch`, `serviceCall`, `sqlExec`, `cacheSet`, `parallel`, `run`, `send`, `logInfo` |

`fetchWithRetry(url, init?, retryOptions?)` accepts `maxRetries`,
`baseDelayMs`, `maxDelayMs`, and `retryOn`. To keep synchronous retry sleeps
bounded, `maxRetries` must be at most 10, `baseDelayMs` at most 5000, and
`maxDelayMs` at most 30000. Negative retry or delay values are treated as 0.

For capability governance internals, see
[Module Capabilities](../internals/capabilities.md).

## Type-Only Imports

`zttp:types` is stripped before runtime and is not in the native module
registry. It provides proof annotation aliases:

```ts
import type { Spec, Proof, Effects } from "zttp:types";
```

See [TypeScript](../typescript.md) and
[Contracts and Auto-Sandboxing](../contracts-and-sandboxing.md) for active
specs and helper capsules.
