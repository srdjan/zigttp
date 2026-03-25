# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

zigttp-server is a **serverless JavaScript runtime** for FaaS (Function-as-a-Service) use cases, powered by **zts** - a pure Zig JavaScript engine. Target deployments: AWS Lambda, Azure Functions, Cloudflare Workers, edge computing.

**Design goals**: Instant cold starts, small deployment package, request isolation, zero external dependencies.

**Trade-offs for FaaS optimization**: Single-threaded sequential processing (one request per instance), compile-time configuration, ES5 JavaScript subset (with some ES6).

## Build Commands

Requires Zig 0.16.0+ (nightly).

```bash
# Build
zig build                           # Debug build
zig build -Doptimize=ReleaseFast   # Optimized build
zig build -Dhandler=handler.jsx    # Precompile handler at build time
zig build -Dhandler=handler.jsx -Dverify  # Verify handler at compile time
zig build -Dhandler=handler.jsx -Dcontract  # Emit contract.json manifest
zig build -Dhandler=handler.jsx -Ddeploy=aws  # Generate proven deployment manifest
zig build -Dhandler=handler.jsx -Dreplay=traces.jsonl  # Replay-verify before embedding
zig build -Dhandler=handler.jsx -Dtest-file=tests.jsonl  # Run handler tests at build time

# Run
zig build run -- -e "function handler(req) { return Response.json({ok:true}); }"
zig build run -- examples/handler.jsx -p 3000
zig build run -- examples/handler.ts       # TypeScript handler
zig build run -- examples/handler.tsx      # TSX handler
zig build run -- examples/handler.jsx --test examples/handler.test.jsonl  # Run handler tests

# Test
zig build test                      # All src/ tests
zig build test-zts                  # zts engine tests only
zig build test-zruntime             # Native Zig runtime tests only

# Run a single test by name
zig build test 2>&1 | head -50      # See available tests
zig build test -- --test-filter "runtime init"  # Filter by name

# Benchmark
zig build bench                     # Build and run benchmarks (src/benchmark.zig)
```

## Architecture

### Two-Layer Design

**Server layer (src/)**: HTTP listener, CLI, request routing, static file serving. Entry point is `main.zig`, HTTP handling in `server.zig`, JS runtime management in `zruntime.zig`.

**Engine layer (zts/)**: Pure Zig JavaScript engine with two-pass compilation (parse to IR, then bytecode). Key components:
- Parser (`zts/parser/`): Pratt parser, tokenizer, IR, bytecode codegen, scope tracking
- VM (`interpreter.zig`): Stack-based bytecode interpreter with JIT baseline compiler
- Values (`value.zig`, `object.zig`): Type-prefix NaN-boxing, hidden classes, inline caching
- Memory (`gc.zig`, `heap.zig`, `arena.zig`, `pool.zig`): Generational GC, arena allocator, lock-free pooling
- TypeScript (`stripper.zig`, `comptime.zig`): Type stripping and compile-time evaluation

### Request Flow

1. Server accepts HTTP connection
2. HandlerPool acquires an isolated runtime (LockFreePool-backed)
3. Request parsed and converted to JS Request object
4. Handler function invoked with Request
5. JS Response extracted and sent as HTTP response
6. Runtime released back to pool

### Key Patterns

**Handler Pool**: Pre-allocates JS contexts for per-request isolation using `LockFreePool`. Critical for FaaS warm instance reuse.

**Result Type**: Functional error handling throughout - `Result(T)` union with `ok`/`err` variants. Maps JS exceptions to Zig errors.

**NaN-Boxing**: Type-prefix NaN-boxing - each type owns a unique upper 16-bit prefix (pointers 0xFFFC, integers 0xFFFD, specials 0xFFFE, extern 0xFFFF). Raw f64 doubles stored inline when prefix < 0xFFFC. Every type check is a single shift-and-compare.

**Hidden Classes**: V8-style hidden class transitions for inline caching. Index-based `HiddenClassPool` with SoA layout stores shapes as compact u32 indices (50% vs pointers). O(1) transition lookups via hash map keyed on `(from_class << 32 | atom)`.

**Hybrid Arena Allocation**: Request-scoped arena with O(1) bulk reset. Write barriers detect arena escape. GC disabled in hybrid mode.

**Guard Composition** (`zts/parser/parse.zig`, `zts/modules/compose.zig`): Compile-time handler composition via `guard()` + pipe operator. The parser collects pipe chains containing `guard()` calls and desugars them into a single flat arrow function with sequential if-checks. Pre-guards (before the handler) receive `req` and short-circuit on non-undefined return. Post-guards (after the handler) receive the response and can replace it. The desugared form uses standard IR nodes (var_decl, if_stmt, return_stmt) so the verifier, contract extractor, and precompiler handle it transparently. Errors: exactly one non-guard handler required, `guard()` must be imported from `zigttp:compose`.

### Performance Optimizations

The request pipeline includes several optimizations for low-latency FaaS workloads:

#### Property Access Optimizations

**Shape Preallocation** (`zts/context.zig:352-434`): HTTP Request and Response objects use preallocated hidden class shapes, eliminating hidden class transitions. Direct slot writes via `setSlot()` bypass property lookup entirely.
- Request shape: method, url, path, query, body, headers (6 props)
- Response shape: body, status, statusText, ok, headers (5 props)
- Response headers shape: content-type, content-length, cache-control (3 props)
- Request headers shape: authorization, content-type, accept, host, user-agent, accept-encoding, connection (7 props)

**Polymorphic Inline Cache (PIC)** (`zts/interpreter.zig:259-335`): 8-entry cache per property access site with last-hit optimization for O(1) monomorphic lookups. Megamorphic transition after 9th distinct shape.

**Binary Search for Large Objects** (`zts/object.zig:751, 831-835`): Objects with 8+ properties use binary search on sorted property arrays. Threshold: `BINARY_SEARCH_THRESHOLD = 8`.

**JIT Baseline IC Integration** (`zts/jit/baseline.zig:1604-1765`): x86-64 and ARM64 JIT fast paths check PIC entry[0] inline, falling back to helper only on cache miss.

**JIT Object Literal Shapes** (`zts/context.zig:746-779`, `zts/jit/baseline.zig:3646-3670`): Object literals with static keys use pre-compiled hidden class shapes. The `new_object_literal` opcode creates objects with the final hidden class directly (no transitions), and `set_slot` writes property values to inline slots without lookup overhead. Fast path uses arena bump allocation with minimal slot initialization (`createWithArenaFast`). Supports both x86-64 and ARM64.

**Type Feedback** (`zts/type_feedback.zig`): Call site and value type profiling for JIT optimization decisions. Early exits when sites become megamorphic avoid redundant type classification. Inlining threshold lowered to 5 calls (from 10) for faster FaaS warmup.

#### String Optimizations

**Lazy String Hashing** (`zts/string.zig:18-24, 44-54`): Hash computation deferred until actually needed. Both `JSString` and `SliceString` use a `hash_computed` flag to track state; `getHash()`/`getHashConst()` compute on first access. Reduces overhead for strings never used as hash keys.

**Pre-interned HTTP Atoms** (`zts/object.zig:237-264`): 27 common headers with O(1) lookup:
- Basic: content-type, content-length, accept, host, user-agent, authorization
- Caching: cache-control, if-modified-since, if-none-match, etag, last-modified, expires, pragma
- CORS: origin, access-control-allow-origin, access-control-allow-methods, access-control-allow-headers, access-control-allow-credentials, access-control-max-age
- Other: connection, accept-encoding, cookie, x-forwarded-for, x-request-id, content-encoding, transfer-encoding, vary

**HTTP String Cache** (`zts/context.zig:111-135, 462+`): Pre-allocated status texts (OK, Created, Not Found, etc.), content-type strings (application/json, text/plain, text/html), and HTTP method strings (GET, POST, PUT, etc.).

#### Pool and Request Optimizations

**Pool Slot Hint** (`zts/pool.zig`): `free_hint` atomic reduces slot acquisition from O(N) linear scan to O(1) in the common case.

**Relaxed Atomic Ordering** (`src/zruntime.zig`): The `in_use` counter uses `.monotonic` ordering since it's only for metrics/limits, not synchronization.

**LRU Static Cache** (`src/server.zig`): Static file cache uses doubly-linked list LRU eviction instead of clear-all, eliminating latency spikes.

**Adaptive Backoff** (`src/zruntime.zig`): Three-phase backoff for pool contention:
- Phase 1: 10 spin iterations using `spinLoopHint`
- Phase 2: Sleep 10us-1ms with jitter (prevents thundering herd)
- Phase 3: Circuit breaker fails fast after 100 retries

**Zero-Copy Response** (`src/zruntime.zig`): Borrowed mode for both body and headers avoids memcpy when arena lifetime is guaranteed.

#### Build-Time Precompilation

**Handler Precompilation** (`tools/precompile.zig`, `build.zig`): The `-Dhandler=<path>` build option compiles JavaScript handlers at build time. Bytecode is embedded directly into the binary, eliminating runtime parsing entirely. This provides the fastest possible cold start for production deployments.

Build flow: `precompile.zig` uses full zts engine to compile, serialize bytecode with atoms and shapes, and generate `src/generated/embedded_handler.zig`. The server loads this bytecode directly via `loadFromCachedBytecode()`.

**Handler Verification** (`zts/handler_verifier.zig`): The `-Dverify` build option enables compile-time correctness verification. The verifier statically proves: (1) every code path returns a Response, (2) Result values from virtual modules are checked before access, (3) no unreachable code after returns, (4) unused variables detected with scope-aware tracking (warnings, underscore-prefix suppresses), (5) match expressions have default arms, (6) optional values from `env()`, `cacheGet()`, `parseBearer()`, and `routerMatch()` are checked before use. Check 6 tracks optional bindings through the control flow tree, recognizing narrowing via truthiness guards (`if (val)`), negated early returns (`if (!val) return`), explicit undefined checks (`val !== undefined`), nullish coalescing (`val ?? default`), and reassignment. This is possible because zigttp's JS subset eliminates most non-trivial control flow (`break`/`continue` are allowed within `for-of` as forward jumps only) - the IR tree IS the control flow graph. See [docs/verification.md](docs/verification.md).

**Handler Contract Manifest** (`zts/handler_contract.zig`): Contract extraction runs automatically during every precompilation, scanning the handler's IR for virtual module imports, literal env var names, outbound hosts from `fetchSync` URL arguments, cache namespace strings, route patterns, and durable usage (keys, step names, timer usage, signal names, producer keys). The `-Dcontract` build option additionally emits `contract.json` (v6) alongside the embedded bytecode. Non-literal arguments set `dynamic: true` as an honest signal that static analysis cannot enumerate all values.

**Handler Effect Classification** (`zts/modules/resolver.zig`, `zts/handler_contract.zig`): Each virtual module export carries an `EffectClass` annotation (read/write/none) defined in `ModuleExport`. During contract extraction, the `ContractBuilder.computeProperties()` method aggregates these effects across all imported functions to derive handler-level `HandlerProperties`: `pure` (no virtual module calls or egress), `read_only` (only read-classified functions), `stateless` (read_only and no cacheGet), `retry_safe` (read_only, or all writes within durable steps), `deterministic` (no Date.now() or Math.random()), `has_egress` (uses fetchSync). The `hasBareWrites()` helper distinguishes durable-wrapped writes from bare writes to correctly classify handlers that use durable execution as retry-safe. Properties appear in contract.json, the build report (PROVEN/--- labels), deploy manifest tags (zigttp:retrySafe, zigttp:readOnly), and OpenAPI specs (x-zigttp-properties extension).

**Compile-Time Fault Coverage** (`zts/fault_coverage.zig`, `zts/path_generator.zig`): The `-Dgenerate-tests=true` build option enables exhaustive path enumeration and fault coverage analysis. The `PathGenerator` walks the handler's IR tree, forking at every branch point (if/match/switch) and I/O success/failure boundary, producing test cases for each execution path. The `FaultCoverageChecker` then analyzes these paths against `FailureSeverity` annotations on each virtual module function (`critical` for auth/validation, `expected` for cache/env, `upstream` for fetchSync). It warns when a critical I/O failure path (e.g., jwtVerify returning `{ok: false}`) produces a 2xx response - a structural pattern overwhelmingly correlated with bugs. Cache misses returning 200 are correctly classified as graceful degradation (no warning). Results flow into `contract.json` (`faultCoverage` section), `HandlerProperties.fault_covered`, deployment manifest tags (`zigttp:faultCovered`), and proven upgrade diffs.

**Compiler-Derived Runtime Sandboxing** (`zts/handler_policy.zig`, `tools/precompile.zig`): Every precompiled handler is automatically sandboxed based on its contract. The `RuntimePolicy` embedded in the generated code restricts env vars, outbound hosts, and cache namespaces to exactly what the compiler can prove the code uses. When a contract section has `dynamic: false`, the sandbox restricts to the proven literal values. When `dynamic: true`, that section remains permissive. An explicit `--policy` file overrides auto-derived sandboxing. The `contractToRuntimePolicy()` function in `handler_policy.zig` implements the conversion. The build output includes a sandbox report showing what was proven and restricted. Non-precompiled handlers (dev mode via `zig build run`) run without sandboxing.

**Proven Deployment Manifests** (`tools/deploy_manifest.zig`): The `-Ddeploy=<target>` build option generates platform-specific deployment configurations from compiler-proven contracts. Implies `-Dcontract`. The system extracts `ProvenFacts` (platform-agnostic) from the contract, then dispatches to a `DeployTarget` renderer. Architecture is pluggable via a `DeployTarget` enum - add new backends by adding an enum variant and render function.

Currently supported targets:
- `aws`: Generates AWS SAM `template.json` with proven env vars as parameters, routes as HttpApi events, egress hosts as tags, and proof level metadata. Output: `src/generated/deploy/template.json` + `src/generated/deploy/deploy-report.txt`.

Proof levels: `complete` (all checks pass, no dynamic flags), `partial` (some verification but dynamic access detected), `none` (no verification ran). The deploy report shows PROVEN vs NEEDS MANUAL REVIEW sections.

**Sound Mode** (`zts/bool_checker.zig`): The BoolChecker walks the IR tree inferring expression types and applies type-directed analysis across all operators. See [docs/sound-mode.md](docs/sound-mode.md).

Type-directed truthiness (TDT) in boolean contexts: types with unambiguous falsy states are accepted (boolean, number, string, optional variants). Objects and functions are rejected (always truthy). undefined emits a warning (always false).

Type-directed arithmetic safety: arithmetic operators (`-`, `*`, `/`, `%`, `**`) require numeric operands. Non-numeric types (string, boolean, object, function, undefined, optional) are compile-time errors. Type-directed `+` safety: mixed-type `+` (number + string) is a compile-time error - use template literals. Non-addable types (boolean, object, function, undefined, optional) in `+` are also errors. Tautological comparison detection: `typeof x === "T"` on a provably-typed value, or `x === undefined` on a provably non-optional value, emits warnings.

Type-directed codegen: when the BoolChecker proves both operands of an arithmetic or comparison are numbers, it populates a `NodeTypeMap`. The CodeGen emits specialized opcodes (`add_num`, `sub_num`, `mul_num`, `div_num`, `lt_num`, `gt_num`, `lte_num`, `gte_num`, `concat_2`) that skip runtime type dispatch. These opcodes omit the string-concatenation check (for `add_num`) and type feedback recording, providing faster cold-start execution before JIT warmup.

Progressive type inference and automatic narrowing:
- **Virtual module return types**: Imported functions from `zigttp:*` modules have known return types (boolean, number, string, object, optional variants). `if (cacheIncr("ns", "key"))` works directly (number truthiness: != 0).
- **Match expression types**: When all arms return the same type, the match expression inherits that type.
- **Optional types**: Functions like `env()` and `cacheGet()` return `optional_string` (`T | undefined`). These work directly in boolean contexts: `if (val)` narrows to string in then-branch. The `??` operator resolves optionals: `env("KEY") ?? "default"` infers as `string`.
- **Truthiness narrowing**: `if (x)` where x is optional automatically narrows to non-optional in then-branch. `if (!x)` narrows to undefined. Same semantics as `x !== undefined` narrowing, triggered automatically.
- **Result property access**: `result.ok` on objects from `jwtVerify`/`validateJson`/`validateObject`/`coerceJson` infers as `boolean`. `result.error` infers as `string`.
- **Undefined equality narrowing**: `x !== undefined` guards narrow optional types to their non-optional variants in the then-branch. `x === undefined` narrows to `undefined`. Both operand orderings supported (`undefined !== x`).

**Bytecode Verifier** (`zts/bytecode_verifier.zig`): Validates bytecode structural integrity before execution. Checks: (1) opcode validity - every instruction boundary byte is a recognized opcode, (2) operand bounds - jump targets within bytecode range and on instruction boundaries, (3) constant pool indices valid, (4) stack discipline - height never negative and within limits, (5) local/upvalue indices within declared counts. Called automatically at runtime and during precompilation.

**Type Checking** (`zts/type_map.zig`, `zts/type_pool.zig`, `zts/type_env.zig`, `zts/type_checker.zig`): Full type annotation checking for TypeScript handlers. The stripper extracts a TypeMap of all type annotations (type aliases, interfaces, variable/param/return annotations, generics) as sideband data without changing the IR or parser. The TypePool stores structured types (primitives, records, unions, functions, arrays, tuples, nullable, generics) as a flat indexed array with structural subtyping via `isAssignableTo`. The TypeEnv resolves type aliases, interfaces, and annotations by walking the TypeMap. The TypeChecker walks the IR tree checking: variable declaration type mismatches, function argument types, property access on known records, return type mismatches. Virtual module types (`zts/modules/types.zig`) provide full function signatures for all 27 zigttp:* exports. Interface declarations with all-function members are marked nominal to prevent structural forgery of capability objects. Two-arg handler support (`handler(req, caps)`) detects handler arity at runtime for capability injection.

**Deterministic Replay** (`zts/trace.zig`, `src/replay_runner.zig`): Handler trace recording and replay system that exploits zigttp's restricted JS subset to provide provable handler equivalence. Since virtual modules are the ONLY I/O boundary, recording their inputs/outputs captures ALL external state a handler depends on - making handlers deterministic pure functions of (Request, VirtualModuleResponses).

Recording: `--trace traces.jsonl` captures every request, virtual module call (with args and return values), `fetchSync` response, `Date.now()` timestamp, and handler response as JSONL. Uses `TraceRecorder` in module_state slot 7 with comptime-generated tracing wrappers (`makeTracingWrapper`) around each NativeFn.

Replay: `--replay traces.jsonl handler.ts` replays recorded traces against a handler. Registers comptime-generated stub functions (`makeReplayStub`) via `registerVirtualModuleReplay` that return recorded values from `ReplayState` in module_state slot 3. Compares actual vs expected Response (status + body). Reports identical count, status changes, body changes with structured diffs.

Build integration: `-Dreplay=traces.jsonl` in the precompile pipeline. The precompile tool creates a lightweight zts context, registers replay stubs, executes the handler against each trace, and fails the build if regressions are detected.

Non-determinism sources handled: `env()`, `fetchSync`, `Date.now()` (intercepted in builtins.zig), `Math.random()` (intercepted in builtins.zig), `jwtVerify` (captured by result recording), `cacheGet/Set/Delete/Incr` (per-call recording sidesteps state), `parallel`/`race` (individual fetch results recorded).

**Declarative Handler Testing** (`src/test_runner.zig`, `tools/precompile.zig`): User-authored test cases in JSONL format, leveraging the same replay stub infrastructure. Because handlers are pure functions of (Request, VirtualModuleResponses), testing requires no mocking frameworks or infrastructure - just declare inputs and expected outputs.

Test file format: each test is a group of JSONL lines starting with `{"type":"test","name":"..."}`, followed by `{"type":"request",...}` (same format as trace recording), optional `{"type":"io",...}` lines (virtual module stubs), and `{"type":"expect","status":N,"bodyContains":"..."}` (assertions). Runtime mode: `--test tests.jsonl`. Build-time mode: `-Dtest-file=tests.jsonl`. Exit code 1 on any failure.

**Durable Execution** (`zts/trace.zig:DurableState`, `src/durable_recovery.zig`, `src/durable_store.zig`, `src/durable_scheduler.zig`): Write-ahead oplog mode built on top of the deterministic replay system. `--durable <dir>` enables crash recovery and long-running workflows for FaaS handlers.

Per-request lifecycle: (1) create oplog file in dir, (2) persist request line with fsync, (3) execute handler - each I/O call's result is persisted to oplog before returning to handler, (4) persist response and "complete" marker, (5) delete oplog.

Recovery: on startup, scans oplog dir for incomplete files (request entry but no "complete" marker), re-executes handlers with `DurableState` loaded from recorded I/O entries. Replayed I/O returns recorded results (zero real I/O). When oplog is exhausted, transitions to live execution for remaining calls.

Durable waits: `sleep(ms)`/`sleepUntil(unixMs)` persist a `wait_timer` entry to the oplog and return `error.DurableSuspended`, which the runtime catches and converts to a `202 Accepted` response with a JSON body describing the pending wait. `waitSignal(name)` works the same way but suspends until a named signal arrives. `signal(key, name, payload?)` and `signalAt(key, name, unixMs, payload?)` enqueue signals for waiting runs via the `DurableStore` filesystem-backed queue (`signals/` and `scheduled/` directories beside the oplog dir). A background `DurableScheduler` thread polls incomplete oplogs every second via the existing recovery path, letting each run decide locally whether its pending timer or signal is ready.

Key types: `DurableState` (hybrid replay/record state, module_state slot 0), `PendingDurableWait` (timer or signal wait stored on `ActiveDurableRun`), `DurableStore` (union-tagged filesystem backend for signal persistence), `DurableScheduler` (background thread polling incomplete runs), `makeDurableWrapper` (comptime wrapper combining replay and write-ahead recording), `registerVirtualModuleDurable` (registers durable wrappers for all virtual modules), `isIncompleteOplog`/`parseIncompleteOplog` (recovery helpers).

The durable wrapper (`makeDurableWrapper`) combines both `makeReplayStub` (replay) and `makeTracingWrapper` (record) behavior in a single comptime-generated function. `fetchSync` and `Date.now()` also support durable mode via their own DurableState checks.

## TypeScript/TSX Support

zts includes a native TypeScript/TSX stripper that removes type annotations at load time. Use `.ts` or `.tsx` files directly.

### Compile-Time Evaluation

The `comptime()` function evaluates expressions at load time:

```typescript
const x = comptime(1 + 2 * 3);                 // -> const x = 7;
const upper = comptime("hello".toUpperCase()); // -> const upper = "HELLO";
const etag = comptime(hash("content-v1"));     // -> const etag = "a1b2c3d4";
```

**Supported**: Literals, arithmetic, bitwise, comparison, logical, ternary, Math, string methods, `parseInt`, `parseFloat`, `JSON.parse`, `hash`.

**Disallowed**: Variables, `Date.now()`, `Math.random()`, closures, assignments.

See [docs/typescript.md](docs/typescript.md) for full specification.

## Unsupported Feature Detection

zigttp uses a two-layer fail-fast validation system to detect unsupported JavaScript and TypeScript features as early as possible:

1. **TypeScript Stripper** ([zts/stripper.zig](zts/stripper.zig)): Catches TypeScript-specific type-position syntax before parsing
   - Detects `any` type (in annotations, assertions, nested positions)
   - Rejects `as` and `satisfies` type assertions as errors
   - Only runs for .ts/.tsx files

2. **Parser** ([zts/parser/parse.zig](zts/parser/parse.zig)): Catches all other unsupported features
   - 53 features including class, enum, namespace, while, throw, try/catch, var, ==, ++, compound assignments, decorators, access modifiers, etc.
   - Runs for all files (after stripping for TS files)
   - Provides consistent error messages regardless of file type

See [feature-detection.md](docs/feature-detection.md) for the complete matrix of detected features.

All error messages follow the pattern: "'feature' is not supported; use X instead"

**Example errors:**
- `'class' is not supported; use plain objects and functions instead`
- `'enum' is not supported; use object literals or discriminated unions instead`
- `'while' is not supported; use 'for-of' with a finite collection instead`

## JavaScript Runtime

**Supported**: ES5 + arrow functions, template literals, destructuring, spread operator, `for...of` (arrays) with `break`/`continue`, optional chaining, nullish coalescing, typed arrays, `**` operator, `globalThis`, string methods (replaceAll, trimStart/End), Math extensions, `range(end)` / `range(start, end)` / `range(start, end, step)`, `match` expression (pattern matching), compound assignments (`+=`, `-=`, `*=`, `/=`, `%=`, `**=`, `&=`, `|=`, `^=`, `<<=`, `>>=`, `>>>=`), pipe operator (`|>`), array HOFs (`.map()`, `.filter()`, `.reduce()`, `.find()`, `.findIndex()`, `.some()`, `.every()`, `.forEach()`), `Object.keys()`, `Object.values()`, `Object.entries()`.

**Limitations**: Strict mode only, no `with`, no array holes, no `new Number()`/`new String()`, only `Date.now()` from Date API. Classes, async/await, Promises, `var`, `while`/`do-while`, `this`, `new`, `try/catch`, `null`, regular expressions are all detected at parse time with helpful error messages. Use `undefined` as the sole absent-value sentinel.

**Response helpers**:
- `Response.json(data, init?)`
- `Response.text(text, init?)`
- `Response.html(html, init?)`
- `Response.redirect(url, status?)`

## Virtual Modules

Native `zigttp:*` modules provide common FaaS functionality with zero JS interpretation overhead. Import via `import { fn } from "zigttp:module"`.

| Module | Exports | Notes |
|--------|---------|-------|
| `zigttp:env` | `env` | Environment variable access |
| `zigttp:crypto` | `sha256`, `hmacSha256`, `base64Encode`, `base64Decode` | Stateless crypto |
| `zigttp:router` | `routerMatch` | Pattern-matching HTTP router with path params |
| `zigttp:auth` | `parseBearer`, `jwtVerify`, `jwtSign`, `verifyWebhookSignature`, `timingSafeEqual` | HS256 JWT, webhook HMAC-SHA256 |
| `zigttp:validate` | `schemaCompile`, `validateJson`, `validateObject`, `coerceJson`, `schemaDrop` | JSON Schema subset (type, required, properties, min/maxLength, min/max, enum, items). Per-runtime SchemaRegistry via module_state. |
| `zigttp:cache` | `cacheGet`, `cacheSet`, `cacheDelete`, `cacheIncr`, `cacheStats` | In-memory KV cache with namespace isolation, LRU eviction, lazy TTL. Per-runtime CacheStore via module_state. Persists across requests in same pool slot. |
| `zigttp:io` | `parallel`, `race` | Structured concurrent I/O. `parallel(thunks)` executes fetchSync calls concurrently using threads, returns results in declaration order. `race(thunks)` returns the first successful result. Requires outbound HTTP enabled. IoCallbacks via module_state slot 6, set by runtime layer. |
| `zigttp:durable` | `run`, `step`, `sleep`, `sleepUntil`, `waitSignal`, `signal`, `signalAt` | Durable execution with crash recovery, timers, and signals. `run(key, fn)` wraps idempotent work; `step(name, fn)` persists sub-results. `sleep(ms)`/`sleepUntil(unixMs)` suspend until a timer fires. `waitSignal(name)` suspends until a signal arrives. `signal(key, name, payload?)`/`signalAt(key, name, unixMs, payload?)` deliver signals to waiting runs. Pending waits return 202. Requires `--durable <dir>`. Per-run oplog via DurableState in module_state slot 0. |
| `zigttp:compose` | `guard` | Compile-time handler composition. `guard(fn)` marks a function as a guard in a pipe chain. The parser desugars `guard(g1) \|> guard(g2) \|> handler \|> guard(post)` into a single flat function with sequential if-checks. Pre-guards `(req) => Response \| undefined` short-circuit; post-guards `(res) => Response \| undefined` transform. No runtime overhead - pure compile-time macro. |

Module implementations: `zts/modules/{auth,validate,cache,env,crypto,router,io,compose,durable,util}.zig`. Shared helpers in `util.zig`. Resolver/wiring in `resolver.zig` and `root.zig`.

**Module Binding System** (`zts/module_binding.zig`, `zts/builtin_modules.zig`): Each module declares a `pub const binding: ModuleBinding` as the single source of truth for all consumers. The `FunctionBinding` struct captures per-function metadata: `effect` (read/write/none), `returns` (ReturnKind for verifier/type checker/bool checker), `param_types` (for type checking), `traceable` (for trace/replay wrapper generation), `contract_extractions` (declarative argument tracking rules), and `contract_flags` (boolean flags like `sets_durable_used`). The `builtin_modules.zig` registry lists all 10 built-in bindings and runs comptime validation (unique specifiers, unique function names, state lifecycle consistency). The type checker (`types.zig`), handler verifier, bool checker, and contract builder all read from this registry instead of maintaining separate hardcoded tables.

**Capability Sandbox for Third-Party Modules**: Third-party modules use `ModuleFn` (receives `*ModuleHandle` opaque type) instead of `NativeFn` (receives `*anyopaque`). The `ModuleHandle` cannot be dereferenced - all runtime interaction goes through free functions in `module_binding.zig` (`createString`, `extractString`, `resultOk`, `resultErr`, `throwError`, `getState`, `setState`, `getAllocator`). Build-path isolation (third-party modules depend on `zigttp-sdk` package, not the full `zts`) prevents importing `context.zig`. The `wrapModuleFn` comptime function generates a zero-cost NativeFn wrapper from a ModuleFn.

Stateful modules (validate, cache, io) use `Context.module_state` - a fixed-size array of opaque pointers with deinit callbacks, indexed by VirtualModule enum ordinal. Lazy-initialized on first use (validate, cache) or set by runtime during init (io).

## CLI Options

```
-p, --port <PORT>      Port (default: 8080)
-h, --host <HOST>      Host (default: 127.0.0.1)
-e, --eval <CODE>      Inline JavaScript code
-m, --memory <SIZE>    JS runtime memory limit (default: 0 = no limit)
-n, --pool <N>         Runtime pool size (default: auto = 2 * cpu, min 8)
--cors                 Enable CORS headers
--static <DIR>         Serve static files
--trace <FILE>         Record handler I/O traces to JSONL file
--replay <FILE>        Replay recorded traces and verify handler output
--test <FILE>          Run declarative handler tests from JSONL file
--durable <DIR>        Enable durable execution with write-ahead oplog
```

## JSX Support

JSX/TSX is parsed directly by the zts parser when JSX mode is enabled (based on `.jsx`/`.tsx` file extensions). No separate transformer.

### JSX Runtime (in zts/http.zig)

- `h(tag, props, ...children)` - Creates virtual DOM nodes
- `renderToString(node)` - Renders to HTML string
- `Fragment` - For grouping without wrapper element

## Testing Guidelines

Tests live alongside code using Zig `test "..."` blocks (no separate test directory). Run the relevant `zig build test*` step after changes.

## Code Quality Patterns

**Memory Safety**: All allocations use `errdefer` for cleanup on failure paths. Header strings are duplicated to avoid use-after-free from GC.

**Safe Optional Handling**: Pool and listener access uses `orelse` pattern instead of `?` unwrap.

**Path Traversal Prevention**: `isPathSafe()` in server.zig validates static file paths.

## Language & Style

Primary language: **Zig**. All new code should be written in Zig unless the existing file is JavaScript/TypeScript (handler examples, JSX). Consult `/mnt/skills/user/zig-expert/SKILL.md` for Zig-specific patterns (allocators, error unions, comptime, data-oriented design).

## Workflow Conventions

When asked to create a plan, outline, or document, produce a draft structure/skeleton FIRST before deep-diving into codebase exploration. Show incremental output early.

## Performance Work

Before implementing an optimization, always benchmark the current state first and compare against the target metrics. If targets are already met, report that immediately rather than proceeding with implementation.

## Benchmarks

JS/TS benchmark scripts and historical results live in the separate `../zigttp-bench` repository. The Zig-native benchmark harness remains in `src/benchmark.zig` and runs via `zig build bench`.

When performance benchmarks are needed (profiling, regression detection, comparative analysis), use the `../zigttp-bench` repo. Do not create benchmark scripts in this repository.
