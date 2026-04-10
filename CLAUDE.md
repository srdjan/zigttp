# CLAUDE.md

## What This Is

Serverless JavaScript runtime for FaaS, powered by zigts (pure Zig JS engine). Targets AWS Lambda, Azure Functions, Cloudflare Workers, edge. Design goals: instant cold starts, small binary, request isolation, zero dependencies.

## Build & Run

Validated on Zig 0.16.0-dev.3073+28ae5d415. Newer nightlies are best-effort until revalidated.

```bash
zig build                                      # Debug build
zig build -Doptimize=ReleaseFast              # Release build
zig build -Dhandler=handler.jsx               # Precompile handler
zig build -Dhandler=handler.jsx -Dverify      # Verify at compile time
zig build -Dhandler=handler.jsx -Dcontract    # Emit contract.json
zig build -Dhandler=handler.jsx -Ddeploy=aws  # Proven deploy manifest
zig build -Dhandler=handler.jsx -Dreplay=traces.jsonl   # Replay-verify
zig build -Dhandler=handler.jsx -Dtest-file=tests.jsonl  # Handler tests at build time

zig build run -- examples/handler/handler.ts -p 3000
zig build run -- examples/handler/handler.ts --watch --prove  # Proven live reload
zig build run -- -e "function handler(req) { return Response.json({ok:true}); }"

zig build test                     # All tests
zig build test-zigts                 # Engine tests only
zig build test-zruntime            # Runtime tests only
zig build test -- --test-filter "name"  # Single test
bash scripts/test-examples.sh      # All example handler tests

zig build bench                    # Zig-native benchmarks (packages/runtime/src/benchmark.zig)
zig build prove -- old.json new.json  # Compare contracts (0=safe, 1=breaking)
zig build mock -- tests.jsonl --port 3001  # Mock server from test cases
zigts link system.json               # Cross-handler contract linking
```

## Architecture

Monorepo with packages under `packages/`. Runtime (`packages/runtime/`): HTTP, CLI, request routing, static files, live reload. Entry: `main.zig`, HTTP: `server.zig`, runtime management: `zruntime.zig`, live reload: `live_reload.zig`. Engine (`packages/zigts/`): JS engine with two-pass compilation (parse to IR, then bytecode). Parser in `packages/zigts/src/parser/`, VM in `interpreter.zig`, values use NaN-boxing (`value.zig`, `object.zig`), memory management in `gc.zig`/`heap.zig`/`arena.zig`/`pool.zig`, TypeScript stripping in `stripper.zig`. Tools (`packages/tools/`): build-time precompilation, CLI, analysis.

Request flow: accept connection, check proven route table (contract-aware pre-filter), check proof cache for deterministic+read_only handlers (`proof_adapter.zig`), acquire isolated runtime from HandlerPool (LockFreePool-backed), convert to JS Request, invoke handler, extract Response, release runtime. Self-extracting binaries parse the embedded contract at startup for env var validation, route pre-filtering, proof cache activation, and property logging (`contract_runtime.zig`).

Key patterns: `Result(T)` for error handling (ok/err variants), hidden classes for inline caching, request-scoped arena allocation, guard composition via pipe operator (`packages/zigts/src/modules/compose.zig`).

For detailed architecture: [docs/architecture.md](docs/architecture.md). For performance internals: [docs/performance.md](docs/performance.md).

## Virtual Modules

Import via `import { fn } from "zigttp:module"`. Implementations in `packages/zigts/src/modules/`. Each module declares a `ModuleBinding` in `module_binding.zig` as single source of truth. Bindings also declare `required_capabilities` (clock, crypto, random, stderr, etc.) enforced at call time by shared checked helpers in `module_binding.zig`. Modules with no capabilities skip the enforcement wrapper at compile time.

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
| `zigttp:io` | `parallel`, `race` |
| `zigttp:durable` | `run`, `step`, `sleep`, `sleepUntil`, `waitSignal`, `signal`, `signalAt` |
| `zigttp:compose` | `guard`, `pipe` |
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
- **Contracts** (`-Dcontract`): Extracts imports, env vars, routes, egress hosts, handler properties. See `packages/zigts/src/handler_contract.zig`.
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

`-p PORT`, `-h HOST`, `-e CODE`, `-m SIZE` (memory limit), `-n N` (pool size), `--cors`, `--static DIR`, `--watch` (live reload), `--prove` (contract-diff before swap), `--force-swap` (apply breaking changes), `--trace FILE`, `--replay FILE`, `--test FILE`, `--durable DIR`, `--no-env-check`.

### zigts (compiler/analyzer)

```bash
zigts check [handler.ts] [--json] [--contract] [--types] [--sql-schema path]
zigts compile [flags] <handler.ts> <output.zig>
zigts prove <old.json> <new.json> [output-dir/]
zigts mock <tests.jsonl> [--port PORT]
zigts link <system.json> [--output-dir <dir>]
zigts features [--json]
zigts modules [--json]
zigts init
zigts edit-simulate [handler.ts] [--before old.ts] [--stdin-json]
zigts describe-rule [rule-name|code] [--json] [--hash]
zigts search <keyword> [--json]
zigts review-patch <file> [--before <old>] [--diff-only] [--json] [--stdin-json]
zigts expert meta [--json]
zigts expert verify-paths <file>... [--json]
zigts expert <subcommand> [options]
```

`--json` emits structured diagnostics to stdout with error codes (ZTS0xx-ZTS3xx), source locations, and suggestion fields. `zigts features` and `zigts modules` list language rules and virtual module exports. `zigts init` writes zigts-expert skill files and hook scripts into the current project so Claude Code picks up the compiler-in-the-loop workflow.

`zigts edit-simulate` runs the analysis pipeline on a handler file and reports violations as JSON. With `--before`, it marks violations introduced by the edit vs pre-existing. `zigts describe-rule` lists all diagnostic rules; `--hash` outputs the policy hash for CI assertions. `zigts search` finds rules by keyword. `zigts review-patch` combines edit-simulate with `--diff-only` filtering to show only new violations.

`zigts expert` is the hook-oriented interface. `expert meta` emits policy metadata (version, hash, rule count). `expert verify-paths` runs analysis on specified files. Other expert subcommands delegate to their top-level equivalents.

## Conventions

- All Zig. New code in Zig unless editing existing JS/TS handler examples.
- Tests live alongside code in `test "..."` blocks. Run relevant `zig build test*` after changes.
- `errdefer` on all allocations. `orelse` instead of `?` unwrap.
- Benchmark before optimizing. If targets already met, stop.
- Benchmarks live in `../zigttp-bench`. Do not create benchmark scripts here.
- Draft structure first before deep codebase exploration.
