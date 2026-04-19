# `zigttp:websocket`

WebSocket protocol termination with per-connection state, room-scoped
broadcast, attachment persistence, and codec-level auto-replies.

## Handler shape

WebSocket handlers export three top-level functions plus the usual
`handler(req)` for HTTP fallbacks on the same route:

```ts
// examples/websocket/chat.ts
import { send, getWebSockets, serializeAttachment, deserializeAttachment }
  from "zigttp:websocket";

export function onOpen(ws, url) {}

export function onMessage(ws, data, room) {
  const name = deserializeAttachment(ws);
  if (name === undefined) {
    serializeAttachment(ws, data);
    send(ws, "hello " + data);
    return;
  }
  for (const peer of getWebSockets(room)) {
    send(peer, name + ": " + data);
  }
}

export function onClose(ws, code, reason) {}

export function handler(req) {
  return Response.text("ws endpoint", { status: 404 });
}
```

The runtime upgrades any incoming HTTP request with `Upgrade: websocket`
whose path matches the handler's WS routes. The room key is derived
from the upgrade URL (full path by default).

## API

| Export | Signature | Effect | Purpose |
|---|---|---|---|
| `send` | `send(ws, data)` | io | Enqueue outbound frame |
| `close` | `close(ws, code?, reason?)` | io | Graceful close with RFC 6455 §5.5.1 payload |
| `serializeAttachment` | `serializeAttachment(ws, value)` | state | Persist per-connection bytes |
| `deserializeAttachment` | `deserializeAttachment(ws): value \| undefined` | state | Retrieve after attachment was set |
| `getWebSockets` | `getWebSockets(roomKey): ws[]` | io | Iterate peers for broadcast |
| `roomFromPath` | `roomFromPath(ws, param): string` | pure | Extract `:param` from opening URL |
| `setAutoResponse` | `setAutoResponse(ws, request, response)` | state | Register codec-level auto-reply |

Attachments are currently **strings only**. Handlers that need
structured data should `JSON.stringify` before storing.

## Event dispatch

- `onOpen(ws, url)` — fires once after the 101 handshake; `url` is
  the full opening request path.
- `onMessage(ws, data, room)` — fires for every inbound text/binary
  frame. `room` is the room key the connection registered under;
  pass it to `getWebSockets(room)` for broadcast.
- `onClose(ws, code, reason)` — fires on peer close or server-
  initiated close. `code` is the RFC 6455 status; `reason` is
  currently always empty (the close-frame body is not yet parsed).

Each dispatch borrows a runtime from the handler pool, installs the
WS callback table, invokes the named global, and releases the runtime.
No state survives between invocations on the JS side — the
`serializeAttachment` API is the supported path for per-connection
state.

## Attachment persistence

With `--durable <dir>`, every `serializeAttachment` write lands
atomically at `<dir>/ws/<id>.att` via tmp + rename. A clean
`unregister` (peer-initiated close) unlinks the file; a process
crash leaves the file for recovery.

Pool-level helpers for operators:

- `listPersistedAttachmentIds(allocator)` — returns every
  crash-leftover id on disk.
- `adoptPersistedAttachment(new_id, prior_id)` — migrate a
  crash-leftover attachment onto a freshly registered connection.
  Caller is expected to validate that the client genuinely owns
  `prior_id` (signed cookie or similar) before calling.

## Auto-responses

`setAutoResponse(ws, request, response)` registers a single byte
pair per connection. When an inbound text/binary frame's bytes
match `request` exactly, the frame loop writes `response` back
without dispatching to JS (no runtime borrow, no attachment read).
Pass two empty strings to clear the registration.

```ts
export function onOpen(ws, url) {
  setAutoResponse(ws, "heartbeat", "alive");
}
```

Any frame whose payload is not exactly `"heartbeat"` still flows
into `onMessage` as normal.

## Connection state machine

Internally each connection moves through three states:

- `live` — a JS handler is currently executing
- `parked` — idle, waiting for the next frame (the default between
  messages)
- `dormant` — parked past the configured idle threshold

Today only the observational labels are used: the runtime is already
released between every frame dispatch, so a `parked` connection
holds no runtime. The remaining resource held per connection is a
kernel thread (`ws_frame_loop` runs thread-per-connection). Thread
release awaits an evented-loop refactor; `scanIdle` is the hook for
when that lands.

## Compile-time checks

Two diagnostics fire when the handler's exports and imports don't
line up:

- **ZTS320 `websocket_import_without_events`** (warning) — imports
  `zigttp:websocket` but no `onMessage` export.
- **ZTS321 `websocket_events_without_import`** (error) — exports
  WS event functions but no `zigttp:websocket` import.

Run `zigts describe-rule ZTS320 --json` for the full rule record.

## Runtime flags

- `--durable <dir>` — enables attachment persistence. Connections
  function without this, but attachments are memory-only.

## Not yet

- Full peer-sent close code/reason propagation to `onClose` (the
  close frame body is currently dropped before dispatch).
- Binary typed-array attachments (string-only today).
- Evented-loop replacement for thread-per-connection; true
  resource-savings hibernation.
- Per-message deflate (permessage-deflate compression).

## Related

- [`zigttp:durable`](./durable.md) — shares the durable-store subtree
  machinery. WS attachments live under `<durable>/ws/`; durable
  signals and scheduled signals live under `<durable>/signals/` and
  `<durable>/scheduled/`.
