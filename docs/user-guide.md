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
| `req.body` | Request body as a string. |

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
    const match = routerMatch("GET /users/:id", req.method, req.path);
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
| Internal service registry | `--system <file>` |
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
zigttp expert --model claude-sonnet-4-6
zigttp expert --print "add a GET /health route"
zigttp expert --handler src/handler.ts --goal no_secret_leakage
```

Pass `--model <id>` to start on a specific provider model, or switch mid-session
with the `/model` command.

Keys are stored in `~/.zigttp/providers.json` with mode `0600`. A shell-set
`ANTHROPIC_API_KEY` or `OPENAI_API_KEY` overrides the stored value.

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

The process stays up. Check stderr, memory limits, stack depth, and any
virtual-module runtime requirements. See [Reliability](reliability.md).

**A port is already in use.**

Choose another port:

```bash
zigttp dev -p 3001
zigttp serve -p 8081 src/handler.ts
```
