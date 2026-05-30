# CLAUDE.md

## What This Is

Serverless JavaScript runtime for FaaS, powered by zigts (pure Zig JS engine). Targets AWS Lambda, Azure Functions, Cloudflare Workers, edge. Design goals: instant cold starts, small binary, request isolation, zero dependencies.

## Build & Run

Validated on Zig 0.16.0 stable.

The build produces three binaries:

- `zigttp` — the primary developer CLI and local runtime entrypoint. `zigttp --help` advertises only the five core commands: `init`, `dev`, `test`, `expert`, `deploy`. Everything else is advanced and listed under `zigttp help --all`: the analyzer commands `check`, `prove`, `mock`, `link`, `gen-tests`, `rollout`, plus `serve`, `compile`, `build`, `verify`, `doctor`, `demo`, `edge`, `proofs` (with `list | show | diff | watch | export | badge | bundle | verify` subcommands), `proof` (with the `replay <capsule>` subcommand — replays a recorded Proof Flight Recorder capsule against the current handler; distinct from `proofs`, the ledger viewer), `ratchet` (with `show | check` subcommands), `witnesses`. A "Machine tools" section in `help --all` lists the JSON-output commands `features`, `modules`, `restrictions`, `meta`, `describe-rule`, `search`, `verify-paths`, `verify-modules`, `edit-simulate`, `review-patch`, and `prove-behavior` — all reachable as `zigttp <command>` via thin delegations into the same code path the `zigts` binary uses. The top-level `zigttp verify <url>` is the proof-receipt verifier and is distinct from `zigttp proofs verify <bundle-dir>` (bundle integrity). The browser proof workbench (`zigttp studio`) is built but hidden from help in v0.1.0-beta. Hosted cloud deploy is deferred too: `deploy --cloud` still parses and rejects with a "not in this beta" message; the related account verbs (`login`, `logout`, `review`, `grants`, `revoke-grant`) are not dispatched and treated as unknown commands. Hosted control-plane, provider, registry, and OCI image orchestration code is intentionally out of core.
- `zigttp-runtime` — the internal runtime template used for self-contained outputs and direct runtime tests. Invoked automatically by `zigttp`; users never type its name.
- `zigts` — the pi-free engine/compiler CLI installed alongside `zigttp` for IDE and CI integrations that prefer calling the analyzer directly. Every `zigts <command>` is also reachable as `zigttp <command>` with identical surface and output. The interactive `expert` agent and session `ledger` commands are the exception: they live only in `zigttp`, so the ~37 KLOC agent (and its provider HTTP clients and API-key handling) is compiled exactly once and never linked into `zigts` or the deployed `zigttp-runtime`. `zigts expert`/`zigts ledger` print a one-line pointer to `zigttp` and exit non-zero.

```bash
zig build                                      # Debug build (all three binaries)
zig build -Doptimize=ReleaseFast              # Release build
zig build -Dhandler=handler.jsx               # Precompile handler into zigttp
zig build -Dhandler=handler.jsx -Dverify      # Verify at compile time
zig build -Dhandler=handler.jsx -Dcontract    # Emit contract.json
zig build -Dhandler=handler.jsx -Dreplay=traces.jsonl   # Replay-verify
zig build -Dhandler=handler.jsx -Dtest-file=tests.jsonl  # Handler tests at build time

zig build run -- examples/handler/handler.ts -p 3000       # Run zigttp
zig build run -- examples/handler/handler.ts --watch --prove  # Proven live reload
zig build run -- -e "function handler(req) { return Response.json({ok:true}); }"
zig build cli -- --help                        # Run zigttp

zig build wasm                     # Build zigts analyzer as a wasm module (web playground)
bash scripts/build-wasm-playground.sh  # Build wasm + publish to zigttp-website/static

zig build test                     # All tests (runtime + CLI + engine + zruntime)
zig build test-zigts               # Engine tests only
zig build test-zruntime            # Runtime tests only
zig build test-cli                 # Developer CLI tests only
zig build test -- --test-filter "name"  # Single test
bash scripts/test-examples.sh      # All example handler tests

zig build bench                    # Zig-native benchmarks (packages/runtime/src/benchmark.zig)
zigttp prove old.json new.json  # Compare contracts (0=safe, 1=breaking)
zigttp mock tests.jsonl --port 3001  # Mock server from test cases
zigttp link system.json         # Cross-handler contract linking
```

## Architecture

Monorepo with packages under `packages/`. Runtime (`packages/runtime/`): HTTP, CLI, request routing, static files, live reload. Two entry points after the split:

- `main.zig` → `runtime_cli.zig` — the `zigttp-runtime` runtime template binary (serve, attest, self-extract startup, version, help).
- `cli_main.zig` → `dev_cli.zig` — the `zigttp` developer binary (init, dev, test, expert, deploy, and the advanced commands).
- `cli_shared.zig` — arg parsing, watch sets, size parsing shared by both.

HTTP: `server.zig`, runtime management: `zruntime.zig`, live reload: `live_reload.zig`. Engine (`packages/zigts/`): JS engine with two-pass compilation (parse to IR, then bytecode). Parser in `packages/zigts/src/parser/`, VM dispatch loop in `interpreter.zig` (Zig switch the compiler lowers to computed-goto on x86_64 and aarch64). Profile counters trigger tiered JIT compilation through `interpreter/jit_compile.zig`: a baseline tier in `jit/baseline.zig` (bytecode to native, no specialization) and an optimized tier in `jit/optimized.zig` (type specialization, monomorphic call inlining). JIT is on by default and shares the interpreter's polymorphic inline cache and capability-sandbox dispatch; the `analyzer_only` build flag disables JIT for freestanding and WASM targets. Values use NaN-boxing (`value.zig`, `object.zig`), memory management in `gc.zig`/`heap.zig`/`arena.zig`/`pool.zig`, TypeScript stripping in `stripper.zig`. Tools (`packages/tools/`): build-time precompilation, CLI, analysis.

Request flow: accept connection, check proven route table (contract-aware pre-filter), check proof cache for deterministic+read_only handlers (`proof_adapter.zig`), acquire isolated runtime from HandlerPool (LockFreePool-backed), convert to JS Request, invoke handler, extract Response, release runtime. Self-extracting binaries parse the embedded contract at startup for env var validation, route pre-filtering, proof cache activation, and property logging (`contract_runtime.zig`).

Key patterns: `Result(T)` for error handling (ok/err variants), hidden classes for inline caching, request-scoped arena allocation, guard composition via pipe operator (`packages/modules/src/workflow/compose.zig`).

For detailed architecture: [docs/internals/architecture.md](docs/internals/architecture.md). For performance internals: [docs/performance.md](docs/performance.md).

## Virtual Modules

Import via `import { fn } from "zigttp:module"`. Implementations in `packages/modules/src/` (peer package), where each module owns its `pub const binding = sdk.ModuleBinding{...}` declaration next to the implementation file (one per `data/`, `http/`, `net/`, `platform/`, `security/`, `workflow/` module). The `ModuleBinding` type and the shared capability-enforcement helpers live in `packages/zigts/src/module_binding.zig`. Bindings declare `required_capabilities` (clock, crypto, random, stderr, etc.) enforced at call time by those helpers; modules with no capabilities skip the enforcement wrapper at compile time.

| Module | Key Exports |
|--------|-------------|
| `zigttp:env` | `env` |
| `zigttp:crypto` | `sha256`, `hmacSha256`, `base64Encode`, `base64Decode` |
| `zigttp:router` | `routerMatch` |
| `zigttp:auth` | `parseBearer`, `jwtVerify`, `jwtSign`, `verifyWebhookSignature`, `timingSafeEqual` |
| `zigttp:validate` | `schemaCompile`, `validateJson`, `validateObject`, `coerceJson`, `schemaDrop` |
| `zigttp:decode` | `decodeJson`, `decodeForm`, `decodeQuery` |
| `zigttp:cache` | `cacheGet`, `cacheSet`, `cacheDelete`, `cacheIncr`, `cacheStats` |
| `zigttp:sql` | `sql`, `sqlOne`, `sqlMany`, `sqlExec` |
| `zigttp:service` | `serviceCall` |
| `zigttp:fetch` | `fetch` (web-standard `fetch(url, init?) -> Response`) |
| `zigttp:websocket` | `send`, `close`, `serializeAttachment`, `deserializeAttachment`, `getWebSockets`, `roomFromPath`, `setAutoResponse` |
| `zigttp:io` | `parallel`, `race` |
| `zigttp:durable` | `run`, `step`, `sleep`, `sleepUntil`, `waitSignal`, `signal`, `signalAt` |
| `zigttp:compose` | `guard`, `pipe` |
| `zigttp:scope` | `scope`, `using`, `ensure` |
| `zigttp:url` | `urlParse`, `urlSearchParams`, `urlEncode`, `urlDecode` |
| `zigttp:id` | `uuid`, `ulid`, `nanoid` |
| `zigttp:http` | `parseCookies`, `setCookie`, `negotiate`, `parseContentType`, `cors` |
| `zigttp:log` | `logDebug`, `logInfo`, `logWarn`, `logError` |
| `zigttp:text` | `escapeHtml`, `unescapeHtml`, `slugify`, `truncate`, `mask` |
| `zigttp:time` | `formatIso`, `formatHttp`, `parseIso`, `addSeconds` |
| `zigttp:ratelimit` | `rateCheck`, `rateReset` |

## JavaScript Subset

ES5 + arrow functions, template literals, destructuring, spread, `for...of` (arrays), optional chaining, nullish coalescing, `match` expression, `assert` statement, pipe operator, typed arrays, compound assignments, array HOFs, `Object.keys/values/entries`, `range()`.

Not supported (detected at parse time with suggestions): classes, async/await, Promises, `var`, `while`, `switch`, `this`, `new`, `try/catch`, `null`, regex, `==`, `++`. Use `undefined` as sole absent-value sentinel. See [docs/feature-detection.md](docs/feature-detection.md).

Response helpers: `Response.json()`, `Response.text()`, `Response.html()`, `Response.redirect()`.

## Compile-Time Systems

All documented in detail in their source files and in `docs/`:

- **Verification** (`-Dverify`): Proves Response returns, Result checking, state isolation. See [docs/verification.md](docs/verification.md).
- **Contracts** (`-Dcontract`): Extracts imports, env vars, routes, egress hosts, handler properties, and author-declared intent assertions. See `packages/zigts/src/handler_contract.zig`. Intent extraction is strict-literal: a top-level `export const intent = { assertions: [...] }` populates `contract.intent.assertions[]`; any non-literal form sets `intent.dynamic = true`. See `packages/zigts/src/intent_extractor.zig`.
- **Sound mode**: Type-directed analysis across operators. See [docs/sound-mode.md](docs/sound-mode.md).
- **Type checking**: Full TS annotation checking. See `packages/zigts/src/type_checker.zig`, `packages/zigts/src/type_map.zig`.
- **Flow analysis**: Data label tracking (secret, credential, user_input). See `packages/zigts/src/flow_checker.zig`.
- **Fault coverage**: Path enumeration, failure severity. See `packages/zigts/src/fault_coverage.zig`.
- **Replay/durable**: Deterministic trace recording and crash recovery. See `packages/zigts/src/trace.zig`, `packages/runtime/src/durable_recovery.zig`.
- **Deploy manifests**: Platform-specific configs from contracts. See `packages/tools/src/deploy_manifest.zig`.
- **System linking**: Cross-handler verification. See `packages/zigts/src/system_linker.zig`.

## TypeScript/JSX

TS/TSX files work directly (native type stripper). JSX parsed by zigts parser, rendered via `h()`/`renderToString()` in `packages/zigts/src/http.zig`. `comptime()` evaluates expressions at load time. See [docs/typescript.md](docs/typescript.md), [docs/jsx-guide.md](docs/jsx-guide.md).

## CLI Options

### zigttp (server)

`-p PORT`, `-h HOST`, `-e CODE`, `-m SIZE` (memory limit), `-n N` (pool size), `--cors`, `--static DIR`, `--watch` (live reload), `--prove` (contract-diff before swap), `--force-swap` (apply breaking changes), `--trace FILE`, `--replay FILE`, `--test FILE`, `--durable DIR`, `--no-env-check`. `zigttp dev --record-proof` desugars to `--trace` aimed inside a capsule (`.zigttp/capsules/default/`) and writes the capsule manifest up front (the manifest pins the handler/contract/policy hashes; it is written before the session so Ctrl+C, which signals the whole process group, cannot skip it). Replay the capsule against a later edit with `zigttp proof replay default` (exit 0 reproduced, 1 regression; fails closed on schema/policy-hash drift, `--allow-version-mismatch` overrides).

### zigttp auth

`zigttp auth claude` prompts (with hidden input) for an Anthropic API key
and stores it at `~/.zigttp/providers.json` with mode 0600. `zigttp auth
openai` does the same for OpenAI. `zigttp auth status` prints which keys
are configured (shell vs file, masked). `zigttp auth revoke <provider>`
removes a stored key. The runtime calls `cli_auth.injectStoredProvidersIntoEnv`
before dispatching `dev`/`serve`/`expert`, so stored values populate
`ANTHROPIC_API_KEY`/`OPENAI_API_KEY` automatically. Shell-set variables
always win; the file only fills gaps. Listed under `zigttp help --all`
(Credentials section), not in the core five.

### zigttp deploy

`zigttp deploy` takes no arguments. It auto-detects the handler file and project name in the current directory, verifies the handler, and emits a self-contained binary at `.zigttp/deploy/<project-name>` with a `kind=deploy` row appended to `.zigttp/proofs.jsonl`. No credentials, Docker, or network. `zigttp deploy --local` and `--target local` are explicit aliases. Hosted cloud deploy (`--cloud`) is deferred from v0.1.0-beta. See [docs/deploy-tutorial.md](docs/deploy-tutorial.md).

Proof receipts ship default-on: every fresh `compile`, `build`, or `deploy --local` signs the contract, bytecode, and rule-registry hashes with the persistent Ed25519 identity at `~/.zigttp/attest/keypair.bin` (generated on first use, mode 0600) and embeds the JWS in the self-extracting binary. The running server emits `Zigttp-Proofs` and `Zigttp-Attest` response headers on every request and serves `GET /.well-known/zigttp-attest` with the full attestation envelope plus the JWK public key (ETag, `Cache-Control: max-age=3600`, 304 on `If-None-Match`). `zigttp verify <url>` validates the signature from any third-party machine. Opt out for a specific build with `--no-attest`.

### Machine tools (analyzer surface)

Every analyzer command is reachable as `zigttp <command>`. The standalone `zigts` binary is also installed for IDE and CI integrations; surface and output formats are identical.

```bash
zigttp check [handler.ts] [--json] [--contract] [--types] [--sql-schema path] [--system path] [--require-export-capsules]
zigttp compile [flags] <handler.ts> <output.zig>
zigttp prove <old.json> <new.json> [output-dir/]
zigttp mock <tests.jsonl> [--port PORT]
zigttp link <system.json> [--output-dir <dir>]
zigttp features [--json]
zigttp modules [--json]
zigttp restrictions [--json] [--by proof|class]
zigttp meta [--json]
zigttp verify-paths <file>... [--json]
zigttp verify-modules <file>... [--strict] [--json]
zigttp verify-modules --builtins [--strict] [--json]
zigttp edit-simulate [handler.ts] [--before old.ts] [--stdin-json]
zigttp describe-rule [rule-name|code] [--json] [--hash]
zigttp search <keyword> [--json]
zigttp review-patch <file> [--before <old>] [--diff-only] [--json] [--stdin-json]
zigttp prove-behavior <before.ts> <after.ts> [--json]
zigttp expert
```

`--json` emits structured diagnostics to stdout with error codes (ZTS0xx-ZTS3xx), source locations, and suggestion fields. `zigttp features` and `zigttp modules` list language rules and virtual module exports. `zigttp restrictions` projects every blocked feature into the failure class it eliminates and the proof it unlocks (see [docs/restrictions-to-proofs.md](docs/restrictions-to-proofs.md) for the table). `zigttp expert` is the canonical interactive compiler-in-the-loop workflow.

`zigttp edit-simulate` runs the analysis pipeline on a handler file and reports violations as JSON. With `--before`, it marks violations introduced by the edit vs pre-existing. `zigttp describe-rule` lists all diagnostic rules; `--hash` outputs the policy hash for CI assertions. `zigttp search` finds rules by keyword. `zigttp review-patch` combines edit-simulate with `--diff-only` filtering to show only new violations. `zigttp prove-behavior <before.ts> <after.ts>` compiles both handler versions and reports the behavioral-equivalence verdict (equivalent / equivalent_modulo_laws / additive / breaking) with the per-path behavior delta; a changed or removed response path reads as breaking even when the route surface is unchanged. Exit 0 = safe, 1 = breaking, 2 = error. It differs from `zigttp prove`, which diffs two pre-extracted contract.json files. The expert loop emits the same verdict as a signed `kind=equivalence` proof receipt after each applied edit (opt out with `--no-equivalence-receipt`).

`zigttp meta`, `zigttp verify-paths`, and `zigttp verify-modules` are the machine-facing verification surface. `zigttp expert` is the interactive agent that uses the same underlying analyzers in-process.

## Conventions

- All Zig. New code in Zig unless editing existing JS/TS handler examples.
- Tests live alongside code in `test "..."` blocks. Run relevant `zig build test*` after changes.
- `errdefer` on all allocations. `orelse` instead of `?` unwrap.
- Benchmark before optimizing. If targets already met, stop.
- Benchmarks live in `../zigttp-bench`. Do not create benchmark scripts here.
- Draft structure first before deep codebase exploration.

# CLAUDE.md

Behavioral guidelines to reduce common LLM coding mistakes. Merge with project-specific instructions as needed.

**Tradeoff:** These guidelines bias toward caution over speed. For trivial tasks, use judgment.

## 1. Think Before Coding

**Don't assume. Don't hide confusion. Surface tradeoffs.**

Before implementing:
- State your assumptions explicitly. If uncertain, ask.
- If multiple interpretations exist, present them - don't pick silently.
- If a simpler approach exists, say so. Push back when warranted.
- If something is unclear, stop. Name what's confusing. Ask.

## 2. Simplicity First

**Minimum code that solves the problem. Nothing speculative.**

- No features beyond what was asked.
- No abstractions for single-use code.
- No "flexibility" or "configurability" that wasn't requested.
- No error handling for impossible scenarios.
- If you write 200 lines and it could be 50, rewrite it.

Ask yourself: "Would a senior engineer say this is overcomplicated?" If yes, simplify.

## 3. Surgical Changes

**Touch only what you must. Clean up only your own mess.**

When editing existing code:
- Don't "improve" adjacent code, comments, or formatting.
- Don't refactor things that aren't broken.
- Match existing style, even if you'd do it differently.
- If you notice unrelated dead code, mention it - don't delete it.

When your changes create orphans:
- Remove imports/variables/functions that YOUR changes made unused.
- Don't remove pre-existing dead code unless asked.

The test: Every changed line should trace directly to the user's request.

## 4. Goal-Driven Execution

**Define success criteria. Loop until verified.**

Transform tasks into verifiable goals:
- "Add validation" → "Write tests for invalid inputs, then make them pass"
- "Fix the bug" → "Write a test that reproduces it, then make it pass"
- "Refactor X" → "Ensure tests pass before and after"

For multi-step tasks, state a brief plan:
```
1. [Step] → verify: [check]
2. [Step] → verify: [check]
3. [Step] → verify: [check]
```

Strong success criteria let you loop independently. Weak criteria ("make it work") require constant clarification.

---

**These guidelines are working if:** fewer unnecessary changes in diffs, fewer rewrites due to overcomplication, and clarifying questions come before implementation rather than after mistakes.
