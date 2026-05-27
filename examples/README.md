# Examples

Handler files grouped by what they demonstrate. The compiler treats every example as a complete program: parse, contract, verify, then run. Each `.test.jsonl` file beside a handler is replayable through `zigttp mock` or `zigts check --test-file`.

## Start here

These three handlers, in order, are the shortest path to seeing what makes zigttp different from a Node runtime.

1. **[handler/spec-guardrails.ts](handler/spec-guardrails.ts)** - the magnet, in 24 lines. The handler declares `Spec<"deterministic" | "idempotent" | "no_secret_leakage" | "injection_safe">` on its return type and the compiler discharges all four. Try editing it: drop a `Date.now()` into the body and watch `-deterministic` light up in the proof card with a `Why:` row. Wrap it in `step("ts", () => Date.now())` from `zigttp:durable` and the chip flips back green.

2. **[handler/handler-full.tsx](handler/handler-full.tsx)** - the same magnet, with branches. JSX rendering, multiple routes, Result handling, and a non-trivial `Spec<...>` declaration. Read it after the guardrails example to see how the proof obligations scale across a real handler.

3. **[handler/secret-leak.ts](handler/secret-leak.ts)** - see a proof reject your code. Reads `env("SECRET_KEY")` and tries to ship it back in the response body. The flow analyzer catches it before runtime and emits ZTS400. This is the most concrete demo of "the compiler proves what your code is."

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
- [modules_all.ts](modules/modules_all.ts) - touches every shipped virtual module so the contract extractor exercises each binding.

### Advanced surfaces

- **durable/** - `run`, `step`, `waitSignal` from `zigttp:durable`. Replay-safe execution. See [approval.ts](durable/approval.ts).
- **fetch/** - the `fetch` web-standard binding from `zigttp:fetch`.
- **parallel/** - `parallel` and `race` from `zigttp:io`.
- **websocket/** - WebSocket events with `serializeAttachment` and rooms.
- **sql/** - the `sql` tagged template from `zigttp:sql`.
- **htmx-todo/**, **shopping-cart/**, **url-shortener/** - small full apps wiring several modules together.
- **system/** - the cross-handler linking story (`zigts link`).
- **autoloop/** - the agent autoloop demo (`zigts expert`).

## Running an example

```bash
zig build run -- examples/handler/spec-guardrails.ts -p 3000        # serve
zig build run -- examples/handler/spec-guardrails.ts --watch --prove # proven live reload
zigts check examples/handler/spec-guardrails.ts                     # verify once
```

## Diagnosing failures

Every `.test.jsonl` next to a handler is a replayable assertion. Run them with the mock server:

```bash
zigttp mock examples/handler/handler.test.jsonl --port 3001
```

When verification fails on an unsupported language feature, the terminal shows the framed restriction block (`why` / `buys` / `try`). See `zigts restrictions` for the full table of language cuts and the proofs they unlock.
