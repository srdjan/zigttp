# Examples

Handler files grouped by what they demonstrate. The compiler treats every
example as a complete program: parse, contract, verify, then run. Each
`.test.jsonl` file beside a handler is replayable through `zigttp mock`;
build-time replay uses `zig build -Dhandler=<handler> -Dtest-file=<tests>`.
Workflow examples use live server checks in `scripts/test-examples.sh` because
they exercise real durable state, signals, and the persisted workflow queue.

## Start here

These three handlers, in order, are the shortest path to seeing what makes zigttp different from a Node runtime.

1. **[handler/spec-guardrails.ts](handler/spec-guardrails.ts)** - the magnet, in 24 lines. The handler declares `Spec<"deterministic" | "idempotent" | "no_secret_leakage" | "injection_safe">` on its return type and the compiler discharges all four. Try editing it: drop a `Date.now()` into the body and watch `-deterministic` light up in the proof card with a `Why:` row. Wrap it in `step("ts", () => Date.now())` from `zigttp:durable` and the chip flips back green.

2. **[handler/handler-full.tsx](handler/handler-full.tsx)** - the same shape, with branches. JSX rendering, multiple routes, and Result handling across a fuller handler. It returns a plain `Response` (no `Spec<...>`), so it does not declare proof obligations; read it after the guardrails example to see the routing surface scale up, then compare with [handler/handler.ts](handler/handler.ts), which carries the `Spec<...>` declaration.

3. **[handler/secret-leak.ts](handler/secret-leak.ts)** - see a proof reject your code. Reads `env("SECRET_KEY")` and tries to ship it back in the response body. The flow analyzer catches it before runtime and emits ZTS400. This is the most concrete demo of "the compiler proves what your code is."

After those, the shortest durable workflow path is
**[workflow/dsl-orchestrator.ts](workflow/dsl-orchestrator.ts)**. Read its
proof receipt first, then run it with `--durable` and `--workflow-queue` to see
one stable run key, one queued child boundary, and one inspectable recovery
surface.

## By category

### handler/

The core shape of a zigttp handler. Start with the three above, then:

- [handler.ts](handler/handler.ts) - the canonical TS handler with `Spec<...>`.
- [handler.tsx](handler/handler.tsx) - the same shape in TSX.
- [handler-with-imports.ts](handler/handler-with-imports.ts) - importing multiple virtual modules.
- [sugar.ts](handler/sugar.ts) - the small syntactic conveniences (pipe, match, `assert`) the parser permits.
- [feature-probes.ts](handler/feature-probes.ts) - exact-output probes for runtime language features tracked in the feature matrix.
- [spec-fails-idempotent.ts](handler/spec-fails-idempotent.ts) - a deliberately failing `Spec<...>` for the discharge diagnostics path.

### jsx/

JSX without a build step. Use `renderToString(<Component />)` to produce HTML.

- [jsx-simple.tsx](jsx/jsx-simple.tsx) - one component, one route.
- [jsx-component.tsx](jsx/jsx-component.tsx) - components composing other components.
- [jsx-ssr.tsx](jsx/jsx-ssr.tsx) - full server-side rendering pattern.

### routing/

Branch on `req.method` and `req.path`. No external router needed.

- [router.ts](routing/router.ts) - the bare branching style.
- [match-handler.ts](routing/match-handler.ts) - `match` expression for cleaner exhaustiveness.
- [guard-compose.ts](routing/guard-compose.ts) - pre and post guards composed with `|>` from `zigttp:compose`.
- [api-surface.ts](routing/api-surface.ts) - declaring a larger API as a flat object.

### modules/

Calling into the virtual modules (the in-binary stdlib that replaces npm).

- [modules.ts](modules/modules.ts) - imports a handful of modules and uses them in one handler.
- [modules_all.ts](modules/modules_all.ts) - touches several representative virtual modules so the contract extractor exercises auth, validation, cache, and crypto bindings.

### fetch/

Outbound HTTP through the `zigttp:fetch` module. Start these examples
with an explicit outbound host allow-list.

- [weather-forecasts.ts](fetch/weather-forecasts.ts) - calls the keyless Open-Meteo API and parses the JSON response.
- [webhook.ts](fetch/webhook.ts) - durable POST forwarding with idempotency-key replay.

### Advanced surfaces

- **durable/** - `run`, `step`, `waitSignal` from `zigttp:durable`. Replay-safe execution. See [approval.ts](durable/approval.ts).
- **workflow/** - start with [dsl-orchestrator.ts](workflow/dsl-orchestrator.ts) for the embedded workflow DSL path, then use the primitive fixtures for `call`, `fanout`, `follow`, durable workflow queue, signal resume, timeout, and dead-letter replay. See [../docs/durable-workflows.md](../docs/durable-workflows.md).
- **parallel/** - `parallel` and `race` from `zigttp:io`.
- **websocket/** - WebSocket events with `serializeAttachment` and rooms.
- **sql/** - the `sql` tagged template from `zigttp:sql`.
- **system/** - the cross-handler linking story (`zigts link`).
- **autoloop/** - the agent autoloop demo (`zigttp expert`).

## Running an example

```bash
zig build run -- examples/handler/spec-guardrails.ts -p 3000             # serve
zig build cli -- serve examples/handler/spec-guardrails.ts -p 3000 --watch --prove
zigts check examples/handler/spec-guardrails.ts                          # verify once
```

### A note on `zigttp check` and these examples

`zigttp check` (equivalently `zigts check`) runs the strict analyzer. Its
default is to demand a *fully discharged* handler: when a handler declares no
`Spec<...>` on its return type, the verifier must prove the entire default
profile (`read_only`, `retry_safe`, `idempotent`, `pure`) and emits **ZTS500**
if any member does not hold. Most examples here are intentionally minimal -
they exist to show one feature (a route, a module import, a JSX component) and
deliberately do *not* carry a `Spec<...>`, so they exit non-zero under `check`.
That is expected, not a bug: the example test harness
(`scripts/test-examples.sh`) replays each `.test.jsonl` for observable
behavior, which is a separate gate from the strict `check` discharge.
Workflow fixtures in `examples/workflow/` run as live server checks because the
JSONL replay runner intentionally stubs virtual-module I/O and creates a fresh
runtime per test case.

The canonical example that declares and fully discharges a `Spec<...>` - and so
passes strict `check` cleanly - is [handler/handler.ts](handler/handler.ts).
[handler/spec-guardrails.ts](handler/spec-guardrails.ts) also declares a
`Spec<...>` and is the magnet to edit interactively under `zigttp dev`.
[handler/spec-fails-idempotent.ts](handler/spec-fails-idempotent.ts) declares a
`Spec<...>` it cannot hold *on purpose*, to exercise the ZTS500 discharge
diagnostic.

## Diagnosing failures

Every `.test.jsonl` next to a handler is a replayable assertion. Run them with the mock server:

```bash
zigttp mock examples/handler/handler.test.jsonl --port 3001
```

When verification fails on an unsupported language feature, the terminal shows the framed restriction block (`why` / `buys` / `try`). See `zigts restrictions` for the full table of language cuts and the proofs they unlock.
