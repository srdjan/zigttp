# User Guide

zigttp runs JavaScript, TypeScript, and TSX HTTP handlers from a single Zig
binary. The language surface is intentionally restricted so the compiler can
prove handler properties before and during local development.

## Install

Install a release build:

```bash
curl -fsSL https://raw.githubusercontent.com/srdjan/zigttp/main/install.sh | sh
zigttp --help
```

Build from source with Zig `0.16.0`:

```bash
git clone https://github.com/srdjan/zigttp.git
cd zigttp
zig build -Doptimize=ReleaseFast
./zig-out/bin/zigttp --help
```

## First Project

```bash
zigttp init my-app
cd my-app
zigttp dev
```

The scaffold writes `zigttp.json`, `src/handler.ts` or `src/handler.tsx`, and
starter tests. `zigttp dev` reads the project config, starts the local server,
watches the handler, and prints a proof card on every save.

Run tests and build a local deploy artifact:

```bash
zigttp test
zigttp deploy
./.zigttp/deploy/my-app
```

`deploy` verifies the handler, emits a self-contained binary, writes
`.zigttp/proofs.jsonl`, and signs a proof receipt unless `--no-attest` is
passed.

For a one-file experiment:

```bash
zigttp serve -e "function handler(req) { return Response.json({ ok: true }) }"
zigttp serve examples/handler/handler.ts -p 3000
```

## Handler Shape

A handler is a function named `handler` that receives a request and returns a
`Response`.

```ts
function handler(req: Request): Response {
    if (req.path === "/health") {
        return Response.json({ ok: true });
    }
    return Response.text("Not Found", { status: 404 });
}
```

Request fields used by examples:

| Field | Meaning |
|---|---|
| `req.method` | HTTP method, for example `GET` or `POST`. |
| `req.url` | Raw URL path and query as received by the server. |
| `req.path` | Path without query string. |
| `req.query` | Query string object when available. |
| `req.headers` | Lowercase header map. |
| `req.body` | Decoded request body as a string. `Content-Length` and HTTP/1.1 chunked request bodies are accepted. |

Response helpers:

```ts
Response.text("ok")
Response.json({ ok: true }, { status: 201 })
Response.html("<h1>Hello</h1>")
```

## Routing

Small handlers can branch directly:

```ts
function handler(req: Request): Response {
    if (req.method === "GET" && req.path === "/todos") {
        return Response.json({ items: [] });
    }
    if (req.method === "POST" && req.path === "/todos") {
        const body = JSON.parse(req.body);
        return Response.json({ title: body.title }, { status: 201 });
    }
    return Response.text("Not Found", { status: 404 });
}
```

Use `zigttp:router` when you need path parameters:

```ts
import { routerMatch } from "zigttp:router";

function handler(req: Request): Response {
    const routes = { "GET /users/:id": true };
    const match = routerMatch(routes, req);
    if (match) {
        return Response.json({ id: match.params.id });
    }
    return Response.text("Not Found", { status: 404 });
}
```

For multi-handler routing behind one listener, see [Edge Runtime](edge.md).

## JSON And Validation

Use normal `JSON.parse` and `Response.json` for basic JSON. Use
`zigttp:validate` or `zigttp:decode` when the handler needs schema-backed
validation.

```ts
import { schemaCompile, validateJson } from "zigttp:validate";

schemaCompile("todo", '{"type":"object","required":["title"]}');

function handler(req: Request): Response {
    const parsed = validateJson("todo", req.body);
    if (!parsed.ok) {
        return Response.json({ error: "invalid body" }, { status: 400 });
    }
    return Response.json(parsed.value, { status: 201 });
}
```

Result-producing virtual-module calls must be checked before `.value` access.
Optional-producing calls must be narrowed before use. The verifier enforces both
patterns.

## JavaScript And TypeScript

zigts supports a practical server-side JS/TS subset and rejects constructs that
weaken analysis. Commonly rejected constructs include `var`, `while`, `class`,
`try/catch`, implicit globals, and unsupported module forms.

Use:

- `const` by default, `let` only when reassigned.
- `for...of` loops.
- `if`/`else` and `match` for branching.
- Explicit Result and optional checks.
- Type-only imports from `zigttp:types` for proof annotations.

### Author-Declared Specs

```ts
import type { Spec } from "zigttp:types";

type Safe = Spec<"deterministic" | "state_isolated">;

function handler(req: Request): Response & Safe {
    return Response.json({ ok: true });
}
```

References:

- [TypeScript](typescript.md)
- [Feature Detection](feature-detection.md)
- [Restrictions to Proofs](restrictions-to-proofs.md)
- [Canonical Profile](canonical-profile.md)
- [Sound Mode](sound-mode.md)

## JSX And TSX

TSX handlers can render server-side HTML without a build step.

```tsx
function Page(props) {
    return (
        <html>
            <body><h1>{props.title}</h1></body>
        </html>
    );
}

function handler(req: Request): Response {
    return Response.html(renderToString(<Page title="Hello" />));
}
```

Use the `htmx` template for an HTML-first scaffold:

```bash
zigttp init htmx-app --template htmx
cd htmx-app
zigttp dev
```

See `examples/jsx/` and `examples/handler/handler-full.tsx`.

## Hypermedia Resources

`resource(data, affordances)` is a global that returns one value the runtime
renders two ways, chosen by the request's `Accept` header. Services that ask for
`application/hal+json` (or `application/json`) receive a HAL document; browsers
that ask for `text/html` receive HTML. This is the v1 beta hypermedia primitive:
one affordance declaration drives both HAL for services and HTMX controls for
users.

```ts
function handler(req: Request): Response {
    const order = { id: 42, total: 1999, status: "pending" };
    return resource(order, {
        self: { href: "/orders/42" },
        pay: { href: "/orders/42/pay", method: "POST", title: "Pay now",
               target: "#order", swap: "outerHTML" },
        cancel: { href: "/orders/42", method: "DELETE", title: "Cancel" },
    });
}
```

Each affordance is keyed by its link relation. `href` is required; `method`,
`title`, `target`, `swap`, and `fields` are optional. An affordance with no
`method` (or `GET`/`HEAD`) is a navigation link; any other method is a write
action.

The rendering follows from the affordance shape:

| Accept | Output |
|---|---|
| `application/hal+json` or `application/json` | HAL: data plus `_links` (navigation) and `_templates` (write actions, with `fields` as `properties`). |
| `text/html` with `HX-Request: true` | An HTMX fragment: a `<dl>` of the scalar data, then `<a>` links and `<button hx-post>`/`hx-delete` controls carrying `hx-target`/`hx-swap`. |
| `text/html` without `HX-Request` | The same fragment wrapped in a full page that loads htmx. |

```bash
zigttp serve examples/hypermedia/order.ts -p 3000
curl -H 'Accept: application/hal+json' http://127.0.0.1:3000/orders/42
curl -H 'Accept: text/html' -H 'HX-Request: true' http://127.0.0.1:3000/orders/42
```

See `examples/hypermedia/order.ts`. For an interactive walkthrough of HATEOAS
and HAL with this resource (a live order workflow whose affordances change with
state), open [hypermedia-explainer.html](hypermedia-explainer.html) in a browser.

## Workflow Orchestration

`zigttp:workflow` dispatches from a top-level orchestrator to co-located
handlers loaded from `--system <file>`. The system manifest is strict for
workflow use: every local handler `path` must be readable at startup, and
relative paths use the same rule as the proof tools: a path that exists from the
current working directory is used as-is, otherwise it is resolved from the
manifest directory.

```ts
import { call, fanout, follow, saga } from "zigttp:workflow";
```

`call(name, init?)` dispatches by handler name. `follow(resource, rel, init?)`
resolves a `resource()` affordance by relation and dispatches by the proven
bundle route. `fanout(calls)` aggregates multiple co-located calls in declaration
order. `saga(steps)` runs durable do/compensate steps.

Inside `run()` from `zigttp:durable`, workflow dispatches snapshot plain
`{status, headers, body}` records into the oplog and rebuild real `Response`
objects during replay, so completed child calls are not re-dispatched. Existing
oplogs use the internal `workflow.parallel#N` key for `fanout()` records for
compatibility.

Queue-mediated workflow dispatch is opt-in with `--workflow-queue` and requires
both `--durable <dir>` and `--system <file>`. In that mode, durable top-level
`call`, `follow`, and `fanout` write child requests to `<durable>/workflow-queue`
before dispatch, lease queued items while they run, and write completed response
parts under `done/` before the parent durable step result is persisted. If the
process stops before the parent step completes, durable recovery re-enters the
run and resumes from the queue result or retries the leased item after its lease
expires. Child handlers should be idempotent because a process crash after a
child side effect but before its queue result is written can re-run that child.
`saga()` is not supported with `--workflow-queue`; keep queue-mediated handler
communication at top-level durable `call`, `follow`, or `fanout` boundaries.

The analyzer records durable workflow proof properties separately from the
handler-wide property chips. Receipts, deploy manifests, and proof traces expose
`durableWorkflowProofLevel`, `durableWorkflowRetrySafe`,
`durableWorkflowIdempotent`, `durableWorkflowFaultCovered`, and
`proofTrace.durable_workflow_*` so replay guarantees are visible in the same
artifacts you already inspect. See [Durable Workflows](durable-workflows.md) and
[First Durable Workflow](tutorials/first-durable-workflow.md).

## Actor Queues

`zigttp:queue` provides opt-in in-process actor mailboxes for handlers that need
to exchange work without sharing JS runtime state. Enable the server-owned
in-memory queue with `--actor-queue`.

```ts
import { send, request, receive, ack, nack, reply } from "zigttp:queue";

function handler(req) {
  const sent = send("worker", { kind: "resize", image: "hero.jpg" });
  if (!sent.ok) return Response.json({ error: sent.error }, { status: 503 });

  const inbox = receive("worker");
  if (!inbox.ok) return Response.json({ error: inbox.error }, { status: 503 });
  if (!inbox.value) return Response.json({ queued: sent.value });

  const msg = inbox.value;
  const done = ack(msg.id);
  return Response.json({ id: msg.id, payload: msg.payload, acked: done.ok });
}
```

`send(target, payload)` stores a JSON snapshot of `payload` and returns
`Result<string>` with the message id. `request(target, payload)` also sets the
current actor as the reply target. `receive(actor?)` leases one message and
returns a `Result` whose `.value` is falsy when no message is available; the default actor is `main`. A leased message
stays retained until `ack(id)` deletes it or `nack(id, reason?)` requeues it.
After the configured attempt limit, `nack()` moves the message to the in-memory
dead-letter set and releases the actor mailbox slot; dead letters are retained
outside mailbox capacity. `reply(id, payload)` sends a high-priority response to
the original message's reply actor and sets `correlationId`.

The current backend is process-local memory. It survives handler VM reset,
timeout invalidation, and panic quarantine because payloads are owned by the
queue, not the JS heap. It does not survive process restart; use
`--workflow-queue` for the existing durable workflow child-dispatch queue.

## Virtual Modules

Virtual modules are native Zig APIs exposed through `import { ... } from
"zigttp:*"`. The current module list and runtime requirements are in
[Virtual Modules](virtual-modules/README.md).

Common runtime flags:

| Need | Flag |
|---|---|
| SQLite queries | `--sqlite <file>` |
| Outbound HTTP | `--outbound-http` or `--outbound-host <host>` |
| Durable workflows | `--durable <dir>` |
| Service registry and in-process workflow bundle | `--system <file>` |
| Queue-mediated durable workflow dispatch | `--workflow-queue` |
| In-memory actor mailboxes | `--actor-queue` |
| Skip env startup check in development | `--no-env-check` |

## Compile-Time Proofs

`zigttp dev`, `zigttp test`, `zigttp check`, and build-time precompile paths run
the analyzer. It checks:

- every path returns a `Response`;
- Result and optional values are checked before access;
- unreachable code and unused values are reported;
- module-scope mutations that can leak request state are rejected;
- declared `Spec<...>` obligations are discharged;
- virtual-module imports derive a least-privilege runtime policy;
- flow checks catch secret, credential, validation, injection, and PII issues
  where enough structure is visible.

The proof card shows the current verdict and the property chips. See
[Proof Card](proof-card.md), [Verification](verification.md), and
[Contracts and Auto-Sandboxing](contracts-and-sandboxing.md).

## Tests And Replay

Declarative tests are JSONL files. A scaffolded project writes a starter file
under `tests/`.

```bash
zigttp test
zigttp serve --test tests/handler.test.jsonl src/handler.ts
zig build -Dhandler=src/handler.ts -Dtest-file=tests/handler.test.jsonl
```

Record and replay handler I/O:

```bash
zigttp serve --trace traces.jsonl src/handler.ts
zigttp serve --replay traces.jsonl src/handler.ts
zig build -Dhandler=src/handler.ts -Dreplay=traces.jsonl
```

Persisted counterexamples live in the witness corpus and can be inspected with
`zigttp witnesses`. See [Witnesses](witnesses.md).

## Deploy And Verify

```bash
zigttp deploy
./.zigttp/deploy/my-app -p 8080
zigttp verify http://127.0.0.1:8080
```

The running server emits `Zigttp-Proofs` and `Zigttp-Attest` headers and serves
`/.well-known/zigttp-attest`. `zigttp verify <url>` validates the signed
attestation from another machine. Use `zigttp proofs` to inspect local ledger
entries and `zigttp proofs gate` for pull-request checks.

## Expert Mode

`zigttp expert` is the compiler-in-the-loop coding agent. It uses a configured
Anthropic or OpenAI key, proposes edits, and routes edits through the same
compiler checks before they land.

```bash
zigttp auth claude
zigttp expert
zigttp expert --yes                                # apply edits without prompting
zigttp expert --no-edit                            # read-only analysis, no writes
zigttp expert --resume                             # continue last session
zigttp expert --model claude-sonnet-4-6            # user-selected override
zigttp expert --print "add a GET /health route"
zigttp expert --handler src/handler.ts --goal no_secret_leakage
```

Pi defaults to `claude-haiku-4-5-20251001`. Pass `--model <id>` to start on a
specific provider model, or switch mid-session with the `/model` command. Pass
`--yes` to apply every verified edit without a confirmation prompt; the approval
policy is persisted through `--resume`.
Pass `--no-edit` to allow analysis and file reads while blocking all writes.

Keys are stored in `~/.zigttp/providers.json` with mode `0600`. A shell-set
`ANTHROPIC_API_KEY` or `OPENAI_API_KEY` overrides the stored value.

When your request is ambiguous - the right edit depends on a choice you have not
made - the agent asks one clarifying question instead of guessing. Answer it on
the next line and it proceeds with your choice. Type `/ledger` at any point to
see the session metrics: turns, model round-trips, verified edits, round-trips
to the first proven edit, and the share of proof guarantees the final handler
discharges.

## Troubleshooting

**The server says no handler was provided.**

Run inside a project with `zigttp.json`, pass a handler path, or use `-e`:

```bash
zigttp serve src/handler.ts
zigttp serve -e "function handler(req) { return Response.text('ok') }"
```

**An env var check fails at startup.**

Set the required variable, remove the literal `env("NAME")` use, or pass
`--no-env-check` for local development.

**A Result or optional access fails verification.**

Check `.ok` before `.value`, or narrow the optional before use:

```ts
const token = parseBearer(req.headers.authorization ?? "");
if (!token) return Response.text("Unauthorized", { status: 401 });
```

**A language feature is rejected.**

Run `zigttp restrictions` or see [Restrictions to Proofs](restrictions-to-proofs.md)
for the reason and supported replacement.

**A request returns 500.**

The process stays up. Read the response body first: a type fault or a
non-Response return names the proof chip that guards it and, when the
interpreter resolved one, the faulting `line:column`. If no chip is named, check
stderr, memory limits, stack depth, and any virtual-module runtime requirements.
See [Reliability](reliability.md#proof-explained-500s).

**A port is already in use.**

Choose another port:

```bash
zigttp dev -p 3001
zigttp serve -p 8081 src/handler.ts
```
