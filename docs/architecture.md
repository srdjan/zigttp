# zigttp Architecture

This document describes the architecture of zigttp, a serverless JavaScript runtime powered by the zigts JavaScript engine.

For the strategic direction beyond the current implementation, see
[Frontier](frontier.md).

## Design Philosophy

**Goal**: Instant cold starts, small deployment package, request isolation, zero external dependencies.

**Trade-offs for FaaS**: Single-threaded sequential processing (one request per instance), compile-time configuration, ES5 JavaScript subset with select ES6 features.

## System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        zigttp (Zig)                         │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │ HTTP Server │──│ HandlerPool │──│  Builtins/HTTP     │  │
│  │  (std.net)  │  │  (contexts) │  │  (Response, h())   │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
├─────────────────────────────────────────────────────────────┤
│                    zigts (Pure Zig)                      │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │   Parser    │──│  Bytecode   │──│  Generational GC    │  │
│  │             │  │     VM      │  │ (Nursery + Tenured) │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## Two-Layer Design

### Server Layer (packages/runtime/src/)

HTTP listener, CLI, request routing, static file serving, contract-aware startup.

**Two binaries built from this package**:
- `zigttp` — the runtime. Ships with deployed FaaS apps. Subcommands: `serve`, `attest`. Entry: `main.zig` → `runtime_cli.zig`.
- `zigttp-cli` — the developer CLI. Subcommands: `init`, `dev`, `check`, `compile`, `expert`, `deploy`, `login`, `logout`, `review`, `grants`, `revoke-grant`, `doctor`, plus pass-through to `zigts` for `check`/`prove`/`mock`/`link`. Entry: `cli_main.zig` → `dev_cli.zig`.

Both binaries share the engine (`zigts`), core server/runtime code (`server.zig`, `zruntime.zig`, `contract_runtime.zig`), `self_extract.zig`, and `cli_shared.zig` (arg parsing, watch sets, size helpers). Only `zigttp-cli` links `pi_app` (the `expert` interactive agent) and the `deploy/` subtree (OCI push, control plane, provider adapter).

**Key Components**:
- `main.zig` - Runtime entry trampoline
- `cli_main.zig` - Dev CLI entry trampoline
- `runtime_cli.zig` - Runtime subcommand dispatch
- `dev_cli.zig` - Dev CLI subcommand dispatch
- `cli_shared.zig` - Shared arg parsing, watch sets
- `server.zig` - HTTP request/response handling, static file cache, route pre-filtering
- `zruntime.zig` - HandlerPool management, JS context lifecycle
- `contract_runtime.zig` - Runtime contract parser for startup env validation, route pre-filtering, and property logging
- `live_reload.zig` - Proven live reload: file watching, contract diffing, atomic handler hot-swap
- `self_extract.zig` - Self-extracting binary format (bytecode, contract JSON, runtime policy)

### Engine Layer (packages/zigts/src/)

Pure Zig JavaScript engine with two-pass compilation (parse to IR, then bytecode).

**Key Components**:

#### Parser (`packages/zigts/src/parser/`)
- `parse.zig` - Pratt parser for JavaScript/TypeScript
- `tokenizer.zig` - Lexical analysis
- `codegen.zig` - Bytecode generation from IR
- `ir.zig` - Intermediate representation

#### VM and Runtime
- `interpreter.zig` - Stack-based bytecode interpreter with JIT baseline compiler
- `value.zig` - Type-prefix NaN-boxing value representation
- `object.zig` - Hidden classes, object system, property access
- `builtins.zig` - Built-in JavaScript functions and APIs
- `http.zig` - HTTP runtime (Request, Response) and JSX runtime (h, renderToString)

#### Memory Management
- `gc.zig` - Generational GC (nursery + tenured spaces)
- `heap.zig` - Size-class segregated allocator
- `arena.zig` - Request-scoped arena allocator
- `pool.zig` - Lock-free runtime pooling

#### Language Extensions
- `stripper.zig` - TypeScript/TSX type annotation stripper
- `comptime.zig` - Compile-time expression evaluator
- `bool_checker.zig` - Sound mode type-directed analysis (TDT, arithmetic safety)
- `type_map.zig`, `type_pool.zig`, `type_env.zig`, `type_checker.zig` - TypeScript type checking

#### Compile-Time Analysis
- `handler_verifier.zig` - Seven-check compile-time verification
- `handler_contract.zig` - Contract extraction from IR
- `handler_policy.zig` - Runtime policy derivation from contracts
- `bytecode_verifier.zig` - Bytecode structural integrity validation
- `path_generator.zig` - Exhaustive execution path enumeration and behavioral contract generation
- `fault_coverage.zig` - Fault coverage analysis against FailureSeverity annotations

#### JIT Compilation
- `jit/baseline.zig` - Baseline JIT compiler for x86-64 and ARM64
- `type_feedback.zig` - Call site and value type profiling

## Request Flow

1. Server accepts HTTP connection (`packages/runtime/src/server.zig`)
2. Request parsed from HTTP bytes
3. If contract is loaded, request method+path checked against proven routes - non-matching requests return 404 without entering JS (`packages/runtime/src/contract_runtime.zig`)
4. If handler is proven deterministic+read_only, check proof cache by request hash - cache hit returns the memoized response with `X-Zigttp-Proof-Cache: hit` header, skipping JS entirely (`packages/runtime/src/proof_adapter.zig`)
5. HandlerPool acquires an isolated runtime from LockFreePool (`packages/runtime/src/zruntime.zig`)
6. Request converted to JS Request object
7. Handler function invoked with Request
8. JS Response extracted, stored in proof cache on miss, and sent as HTTP response
9. Runtime released back to pool

## Key Patterns

### Handler Pool

Pre-allocates JS contexts for per-request isolation using `LockFreePool`. Critical for FaaS warm instance reuse.

**Configuration**:
- Default pool size: `2 * cpu_count` (minimum 8)
- Configurable via `-n` flag or `ServerConfig.pool_size`
- Each context is pre-warmed and reusable

### Result Type

Functional error handling throughout using `Result(T)` union with `ok`/`err` variants. Maps JS exceptions to Zig errors.

```zig
const Result = union(enum) {
    ok: T,
    err: Error,
};
```

### NaN-Boxing

Type-prefix NaN-boxing: each type owns a unique upper 16-bit prefix, enabling single-instruction type checks.

**Layout** (upper 16 bits of raw u64):
- Raw doubles: prefix < 0xFFFC - IEEE 754 f64 stored inline, no heap allocation
- Pointers: prefix 0xFFFC - 8-byte aligned address in lower 48 bits
- Integers: prefix 0xFFFD - signed i32 in lower 32 bits
- Specials: prefix 0xFFFE - null (0), undefined (1), true (2), false (3), exception (4)
- Extern pointers: prefix 0xFFFF - non-GC pointer in lower 48 bits

Quiet NaN patterns with prefix >= 0xFFFC are canonicalized to 0x7FF8 on storage.

### Hidden Classes

V8-style hidden class transitions for inline caching. The index-based `HiddenClassPool` (`packages/zigts/src/object.zig`) stores shapes as compact u32 indices with Structure-of-Arrays (SoA) layout for cache-friendly property lookups. Transitions use a hash map keyed on `(from_class << 32 | atom)` for O(1) lookup.

**Shape Preallocation**: HTTP Request and Response objects use preallocated hidden class shapes (`packages/zigts/src/context.zig:352-434`), eliminating hidden class transitions. Direct slot writes via `setSlot()` bypass property lookup entirely.

### Hybrid Arena Allocation

Request-scoped arena with O(1) bulk reset. Write barriers detect arena escape. GC disabled in hybrid mode.

**Benefits**:
- No per-object deallocation overhead
- Predictable memory usage per request
- Zero GC pauses during request handling
- Write barriers prevent arena objects from leaking to persistent storage

### Structured Concurrent I/O

zigttp avoids async/await, Promises, and event loops. Handler code is always
synchronous and linear. When a handler needs concurrent outbound HTTP, it uses
`parallel()` or `race()` from `zigttp:io`. Concurrency happens entirely in the
I/O layer, invisible to the JavaScript execution model.

**Three-phase collect-execute-join model** (`packages/zigts/src/modules/io.zig`,
`packages/runtime/src/zruntime.zig`):

1. **Collect**: Each thunk is called sequentially on the main thread. A
   threadlocal `parallel_collector` intercepts `fetchSync` calls during thunk
   execution - instead of performing HTTP I/O, `fetchSync` records a
   `FetchDescriptor` (URL, method, body, headers) into the collector and returns
   immediately.

2. **Execute**: All collected descriptors are dispatched to OS threads. Each
   worker thread creates its own `std.Io.Threaded` backend and `std.http.Client`
   for full isolation. HTTP requests run truly in parallel with no shared mutable
   state between workers.

3. **Join**: The main thread joins all worker threads, builds JS Response objects
   from the results, and returns them. `parallel()` returns an array in
   declaration order. `race()` returns the first successful response.

**Design constraints**:
- At most 8 concurrent operations per call (`MAX_PARALLEL`).
- Single fetch runs inline without thread overhead.
- Thread spawn failure falls back to inline execution per slot.
- Each `fetchSync` inside a thunk is individually checked against egress
  policies and capability sandboxing.
- The JS heap is never touched from worker threads - only the main thread
  interacts with the VM.

**Why not async/await**: zigttp targets FaaS workloads where handlers process a
single request and exit. Async/await adds complexity (event loop, microtask
queue, Promise machinery) for a concurrency model that only benefits long-lived
servers. The collect-execute-join approach gives the same I/O overlap benefit
with a fraction of the runtime complexity, and the handler code remains a
straight-line function that the verifier and type checker can fully analyze.

### Durable Execution

zigttp extends the same straight-line model to crash recovery through
`zigttp:durable`. A handler opts in with `run(key, fn)` and can bracket
idempotent subcomputations with `step(name, fn)`. The same oplog now also
captures durable waits: `sleep()` / `sleepUntil()` record timer waits, and
`waitSignal()` records named signal waits. The runtime writes a JSONL oplog
under `--durable`, replays deterministic effects from that oplog after a
restart, and resumes live execution at the first incomplete step or wait.
Completed logs stay on disk so duplicate keys can return the previously
recorded `Response` without repeating side effects.

Signals are stored beside the oplog directory in a small filesystem-backed
queue. A background scheduler thread reuses the existing durable recovery path:
it polls incomplete runs, lets the runtime decide whether a timer is due or a
signal is now available, and resumes the run through the same replay logic used
for crash recovery.

## Runtime Model

zigts uses a **generational garbage collector** with:

1. **Type-prefix NaN-boxing** for efficient value representation (single-instruction type checks)
2. **Hidden classes** for inline caching (index-based `HiddenClassPool` with SoA layout)
3. **LockFreePool-backed handler pool** for request isolation in FaaS environments
4. **Hybrid arena allocation** for request-scoped memory with O(1) reset

The Result<T> pattern throughout makes error handling explicit and prevents silent failures.

## Project Structure

```
zigttp/
├── build.zig                    # Zig build configuration
├── packages/
│   ├── zigts/                   # Pure Zig JavaScript engine
│   │   ├── src/
│   │   │   ├── parser/          # Two-pass parser with IR
│   │   │   │   ├── parse.zig    # Main parser (Pratt parser)
│   │   │   │   ├── tokenizer.zig # Tokenizer
│   │   │   │   ├── codegen.zig  # Bytecode generation
│   │   │   │   └── ir.zig       # Intermediate representation
│   │   │   ├── interpreter.zig  # Stack-based bytecode VM
│   │   │   ├── value.zig        # Type-prefix NaN-boxing value representation
│   │   │   ├── object.zig       # Hidden classes, object system
│   │   │   ├── gc.zig           # Generational GC (nursery + tenured)
│   │   │   ├── heap.zig         # Size-class segregated allocator
│   │   │   ├── arena.zig        # Request-scoped arena allocator
│   │   │   ├── http.zig         # HTTP/JSX runtime for SSR
│   │   │   ├── pool.zig         # Lock-free runtime pooling
│   │   │   ├── builtins.zig     # Built-in JavaScript functions
│   │   │   ├── stripper.zig     # TypeScript/TSX type stripper
│   │   │   ├── comptime.zig     # Compile-time expression evaluator
│   │   │   ├── handler_contract.zig # Contract extraction from IR
│   │   │   ├── handler_policy.zig # Runtime policy + contract-to-policy conversion
│   │   │   ├── modules/
│   │   │   │   ├── io.zig       # Structured concurrent I/O (parallel, race)
│   │   │   │   ├── auth.zig     # JWT, HMAC, webhook verification
│   │   │   │   ├── cache.zig    # In-memory KV cache with LRU
│   │   │   │   ├── validate.zig # JSON Schema validation
│   │   │   │   ├── env.zig      # Environment variable access
│   │   │   │   ├── crypto.zig   # SHA-256, HMAC, base64
│   │   │   │   ├── router.zig   # Pattern-matching HTTP router
│   │   │   │   ├── sql.zig      # SQLite query execution with allowlisting
│   │   │   │   ├── compose.zig  # Guard-based handler composition
│   │   │   │   ├── durable.zig  # Durable execution (run, step, sleep, signal)
│   │   │   │   ├── resolver.zig # Module resolver and wiring
│   │   │   │   └── root.zig     # Module registry
│   │   │   ├── jit/
│   │   │   │   └── baseline.zig # Baseline JIT compiler (x86-64, ARM64)
│   │   │   ├── type_feedback.zig    # Call site profiling
│   │   │   ├── handler_verifier.zig # Compile-time handler verification
│   │   │   ├── path_generator.zig   # Exhaustive path enumeration
│   │   │   ├── fault_coverage.zig   # Fault coverage analysis
│   │   │   ├── bool_checker.zig     # Sound mode type-directed analysis
│   │   │   ├── type_map.zig         # TypeScript type annotation map
│   │   │   ├── type_checker.zig     # TypeScript type checking
│   │   │   ├── trace.zig            # Deterministic trace recording/replay
│   │   │   ├── module_binding.zig   # ModuleBinding, FunctionBinding, ModuleHandle
│   │   │   └── builtin_modules.zig  # Registry of all built-in module bindings
│   │   └── deps/                # Engine dependencies (SQLite)
│   ├── runtime/                 # HTTP server, CLI, request routing
│   │   └── src/
│   │       ├── main.zig             # CLI entry point
│   │       ├── zruntime.zig         # HandlerPool, JS context management
│   │       ├── server.zig           # HTTP server implementation
│   │       ├── contract_runtime.zig # Runtime contract parser (env validation, route pre-filter)
│   │       ├── live_reload.zig      # Proven live reload (watch, contract diff, hot-swap)
│   │       ├── proof_adapter.zig    # Proof-driven response cache (deterministic+read_only handlers)
│   │       ├── self_extract.zig     # Self-extracting binary payload (bytecode + contract + policy)
│   │       ├── test_runner.zig      # Declarative handler test runner
│   │       ├── replay_runner.zig    # Deterministic replay runner
│   │       ├── durable_recovery.zig # Durable execution crash recovery
│   │       ├── durable_store.zig    # Signal persistence backend
│   │       └── durable_scheduler.zig # Background scheduler for durable waits
│   └── tools/                   # CLI tools and build-time utilities
│       └── src/
│           ├── precompile.zig       # Build-time bytecode compiler
│           ├── zigts_cli.zig        # CLI dispatcher for zigts subcommands
│           ├── expert.zig           # zigts expert namespace (meta, verify-paths, delegation)
│           ├── edit_simulate.zig    # Diff-aware violation analysis
│           ├── review_patch.zig     # Patch review with --diff-only filtering
│           ├── deploy_manifest.zig  # Proven deployment manifest generator
│           └── openapi_manifest.zig # OpenAPI spec generator
└── examples/
    ├── handler/           # Basic handler variants (TS, TSX)
    ├── jsx/               # JSX rendering demos
    ├── modules/           # Virtual module usage
    ├── routing/           # Router, match, guard composition
    ├── parallel/          # Concurrent I/O demos
    ├── shopping-cart/     # Full shopping cart app
    ├── htmx-todo/         # HTMX Todo app
    └── sql/               # SQL schema + CRUD handler
```

## Memory Safety

All allocations use `errdefer` for cleanup on failure paths. Header strings are duplicated to avoid use-after-free from GC.

**Safe Optional Handling**: Pool and listener access uses `orelse` pattern instead of `?` unwrap.

**Path Traversal Prevention**: `isPathSafe()` in `packages/runtime/src/server.zig` validates static file paths.

## Performance Architecture

### Property Access Optimizations

**Polymorphic Inline Cache (PIC)** (`packages/zigts/src/interpreter.zig:259-335`): 8-entry cache per property access site with last-hit optimization for O(1) monomorphic lookups. Megamorphic transition after 9th distinct shape.

**Binary Search for Large Objects** (`packages/zigts/src/object.zig:751, 831-835`): Objects with 8+ properties use binary search on sorted property arrays. Threshold: `BINARY_SEARCH_THRESHOLD = 8`.

**JIT Baseline IC Integration** (`packages/zigts/src/jit/baseline.zig:1604-1765`): x86-64 and ARM64 JIT fast paths check PIC entry[0] inline, falling back to helper only on cache miss.

**JIT Object Literal Shapes** (`packages/zigts/src/context.zig:746-779`, `packages/zigts/src/jit/baseline.zig:3646-3670`): Object literals with static keys use pre-compiled hidden class shapes. The `new_object_literal` opcode creates objects with the final hidden class directly (no transitions), and `set_slot` writes property values to inline slots without lookup overhead.

### String Optimizations

**Lazy String Hashing** (`packages/zigts/src/string.zig:18-24, 44-54`): Hash computation deferred until needed. `hash_computed` flag tracks state; `getHash()`/`getHashConst()` compute on first access.

**Pre-interned HTTP Atoms** (`packages/zigts/src/object.zig:237-264`): 27 common headers with O(1) lookup (content-type, content-length, accept, host, user-agent, authorization, cache-control, CORS headers, etc.).

**HTTP String Cache** (`packages/zigts/src/context.zig:111-135, 462+`): Pre-allocated status texts (OK, Created, Not Found, etc.), content-type strings (application/json, text/plain, text/html), and HTTP method strings (GET, POST, PUT, etc.).

### Pool and Request Optimizations

**Pool Slot Hint** (`packages/zigts/src/pool.zig`): `free_hint` atomic reduces slot acquisition from O(N) linear scan to O(1) in the common case.

**Relaxed Atomic Ordering** (`packages/runtime/src/zruntime.zig`): The `in_use` counter uses `.monotonic` ordering since it's only for metrics/limits, not synchronization.

**LRU Static Cache** (`packages/runtime/src/server.zig`): Static file cache uses doubly-linked list LRU eviction instead of clear-all, eliminating latency spikes.

**Adaptive Backoff** (`packages/runtime/src/zruntime.zig`): Three-phase backoff for pool contention:
- Phase 1: 10 spin iterations using `spinLoopHint`
- Phase 2: Sleep 10us-1ms with jitter (prevents thundering herd)
- Phase 3: Circuit breaker fails fast after 100 retries

**Zero-Copy Response** (`packages/runtime/src/zruntime.zig`): Borrowed mode for both body and headers avoids memcpy when arena lifetime is guaranteed.

### Build-Time Precompilation

**Handler Precompilation** (`packages/tools/src/precompile.zig`, `build.zig`): The `-Dhandler=<path>` build option compiles JavaScript handlers at build time. Bytecode is embedded directly into the binary, eliminating runtime parsing entirely.

Build flow: `packages/tools/src/precompile.zig` uses full zigts engine to compile, serialize bytecode with atoms and shapes, and generate `packages/runtime/src/generated/embedded_handler.zig`. The server loads this bytecode directly via `loadFromCachedBytecode()`.

### Compiler-Derived Sandboxing

Every precompilation extracts a handler contract by walking the IR for virtual module imports and call sites. The contract records which env vars, outbound hosts, cache namespaces, and SQL queries the handler accesses, and whether each section uses only literal strings (`dynamic: false`) or includes computed access (`dynamic: true`).

When no explicit `--policy` file is provided, the precompiler auto-derives a `RuntimePolicy` from the contract and embeds it in the generated code. Static sections are restricted to exactly the proven literals. Dynamic sections remain permissive. The result is zero-configuration least-privilege sandboxing for every precompiled handler.

**Key files**:
- `packages/zigts/src/handler_contract.zig` - `ContractBuilder` extracts proven facts from IR
- `packages/zigts/src/handler_policy.zig` - `contractToRuntimePolicy()` converts contract to policy; `RuntimePolicy` enforces at runtime
- `packages/tools/src/precompile.zig` - `validateSqlContract()` proves registered SQL against a schema snapshot and embeds the derived policy in generated code
- `packages/zigts/src/modules/sql.zig` / `packages/zigts/src/sqlite.zig` - runtime SQL execution over SQLite with named-query allowlisting

**Enforcement points** (activated by the embedded policy and contract):
- `packages/zigts/src/modules/env.zig` - `allowsEnv()` check on env var access
- `packages/zigts/src/modules/cache.zig` - `allowsCacheNamespace()` on cache operations
- `packages/zigts/src/modules/sql.zig` - `allowsSqlQuery()` on registered query execution
- `packages/runtime/src/zruntime.zig` - `allowsEgressHost()` on outbound HTTP
- `packages/runtime/src/contract_runtime.zig` - startup env var validation, route pre-filtering
- `packages/runtime/src/server.zig` - route pre-filter rejects unproven method+path at HTTP layer

### Module Binding System

Each virtual module declares a `pub const binding: ModuleBinding` struct - the single source of truth for all compile-time consumers. The `FunctionBinding` struct captures effect class, return kind (for verification/type checking), param types, traceability, and declarative contract extraction rules. `ModuleBinding` also carries `required_capabilities`, which records the runtime capabilities consumed by the module's Zig implementation for governance and auditability. These declarations are distinct from handler-facing `effect` metadata and do not feed into `RuntimePolicy`. The `packages/zigts/src/builtin_modules.zig` registry lists all bindings and runs comptime validation (unique specifiers, unique function names, state lifecycle consistency, duplicate capability declarations).

For the planned redesign of third-party virtual modules, see [Extension Model](extension-model.md).

Consumers that previously maintained separate hardcoded tables now read from the registry:
- **Type checker** (`packages/zigts/src/types.zig`): maps `ReturnKind` to `TypeIndex` via `mapReturnKind()`
- **Handler verifier**: looks up result/optional producers via `builtin_modules.findFunction()`
- **Bool checker**: maps `ReturnKind` to `ExprType` via `returnKindToExprType()`
- **Contract builder**: uses `GenericBinding` entries populated from `FunctionBinding.contract_extractions` and `contract_flags`

Third-party modules use `ModuleFn` (opaque `*ModuleHandle`) instead of `NativeFn` (raw `*anyopaque`). The opaque handle prevents dereferencing - all interaction goes through SDK free functions. Capability-checked SDK helpers (for example clock/random/stderr access) run under the module's declared `required_capabilities`, so extension modules can only use that helper surface when the binding metadata allows it. Built-in modules now route the same sensitive operations through shared checked helpers for clock/random/crypto/stderr, policy checks, filesystem, runtime callbacks, and SQLite access, failing fast if implementation behavior drifts from the declaration. Build-path isolation via a separate `zigttp-sdk` package prevents importing runtime internals.

### Handler Effect Classification

Each `FunctionBinding` carries an `effect` annotation (read, write, or none). During contract extraction, `computeEffectSummary()` first reduces call facts into a compact internal summary: whether the handler makes any virtual-module calls, whether it performs reads or writes, whether any write is outside durable handling, whether it reads cache state, whether it uses egress, and whether nondeterministic builtins appear. `computeProperties()` then derives handler-level properties from that summary:

- **pure** - no virtual module calls and no fetchSync; handler is a function of the request only
- **read_only** - all imported functions are read-classified; no state mutations through virtual modules
- **stateless** - read_only and no cacheGet; handler does not depend on mutable external state
- **retry_safe** - read_only, or writes are confined to durable-managed operations with no proven bare writes
- **deterministic** - no `Date.now()` or `Math.random()` calls detected in the IR
- **has_egress** - handler uses fetchSync (conservatively classified as write)

Properties appear in contract.json, the build report (PROVEN/--- labels), AWS SAM tags (zigttp:retrySafe, zigttp:readOnly), and OpenAPI specs (x-zigttp-properties extension). At runtime, `pure` or `deterministic`+`read_only` handlers get automatic response memoization: GET/HEAD responses are cached and served from Zig memory without entering JS (`packages/runtime/src/proof_adapter.zig`).

**Key files**:
- `packages/zigts/src/module_binding.zig` - `ModuleBinding`, `FunctionBinding`, `ModuleHandle`, `ModuleCapability`, `validateBindings()`, capability-checked helpers (`clockNowMsChecked`, `fillRandomChecked`, `hmacSha256Checked`, `writeStderrChecked`, `readFileChecked`, etc.), threadlocal `ActiveModuleContext` push/pop for call-scoped enforcement
- `packages/zigts/src/builtin_modules.zig` - registry of all built-in bindings with comptime validation (unique specifiers, unique function names, duplicate capability detection)
- `packages/zigts/src/modules/resolver.zig` - `wrappedExportFn()` injects the capability context wrapper per export, with a comptime short-circuit for modules declaring no capabilities
- `packages/zigts/src/module_binding_adapter.zig` - adapts SDK `ModuleBinding` to internal types with ordinal-alignment comptime assertions for `ModuleCapability`
- `packages/zigts/src/handler_contract.zig` - `GenericBinding`, `getCategoryTarget()`, `computeEffectSummary()`, `computeProperties()`
- `packages/tools/src/deploy_manifest.zig` - `ProvenFacts.retry_safe`/`read_only`, AWS tag emission
- `packages/tools/src/openapi_manifest.zig` - `x-zigttp-properties` extension

### Compile-Time Path Analysis and Behavioral Contract

`PathGenerator` (`packages/zigts/src/path_generator.zig`) walks the handler's IR tree, forking at every branch point (`if`/`match`) and I/O success/failure boundary. It produces one test case per execution path. PathGenerator runs when `-Dgenerate-tests=true` or `-Dcontract` is specified.

Each enumerated path becomes a `BehaviorPath` (`packages/zigts/src/handler_contract.zig`) in the contract's `behaviors` section. A `BehaviorPath` records route method/pattern, branching conditions (which I/O calls succeed or fail), the I/O call sequence, response status, I/O depth, and whether it represents a failure path. `behaviors_exhaustive` is true when PathGenerator did not hit the 1024-path cap.

`FaultCoverageChecker` (`packages/zigts/src/fault_coverage.zig`) analyzes these paths against `FailureSeverity` annotations on each virtual module function (`critical` for auth/validation, `expected` for cache/env, `upstream` for fetchSync). It warns when a critical I/O failure path produces a 2xx response. Results flow into `contract.json`, `HandlerProperties.fault_covered`, and deployment manifest tags.

### Upgrade Verification

`upgrade_verifier` (`packages/tools/src/upgrade_verifier.zig`) combines surface diff, behavioral diff, property regressions, and coverage gap analysis into a four-value `UpgradeVerdict`:

- **safe** - all behavioral paths preserved, no property regressions
- **safe_with_additions** - new paths added, existing behavior preserved
- **breaking** - paths removed, responses changed, or critical property lost (retry_safe, injection_safe, state_isolated, fault_covered, no_secret_leakage, no_credential_leakage)
- **needs_review** - structurally OK but warning-level property regressions (deterministic, idempotent, read_only) or significant coverage gaps

The behavioral diff (`contract_diff.diffBehaviors`) matches paths by (method, pattern, conditions) tuple. Two paths match when they have the same route and I/O success/failure conditions. A matched path with a different response status is `response_changed` (breaking).

Output: `upgrade-manifest.json` with verdict, justification, surface summary, behavioral summary, property regressions/gains with severity, and coverage gap metrics. Both `zigts prove` and `-Dprove` produce this artifact.

### Proven Live Reload

`--watch --prove` feeds the upgrade verifier into the edit-save cycle. A background thread polls handler files (250ms, recursive directory walking). On change:

1. Full analysis pipeline (`runCheckOnly`): parse, type check, verify, extract contract
2. Diff new contract against running contract via `diffContracts` + `analyzeUpgrade`
3. Verdict `safe` or `safe_with_additions`: swap the handler pool atomically
4. Verdict `breaking` or `needs_review`: block the swap, keep old handler serving

`HandlerPool.reloadHandler` locks the init and cache mutexes, updates the handler source, clears the bytecode cache, then invalidates idle runtimes outside the lock. In-flight requests finish with old code; their runtimes recompile from new source on next acquire. The server's `RuntimeContract` and proof cache update from the new contract's properties.

Without `--prove`, `--watch` recompiles and swaps without contract diffing. Compilation errors keep the old handler running. Handlers using `zigttp:durable` refuse live swap because replay state depends on handler identity.

**Key files**: `packages/runtime/src/live_reload.zig` (watch loop, contract diffing, swap orchestration), `HandlerPool.reloadHandler()` in `packages/runtime/src/zruntime.zig` (atomic pool swap), `Server.updateContract()` in `packages/runtime/src/server.zig` (contract + proof cache reconfiguration).

### Guard Composition

The parser recognizes `guard()` calls within pipe operator chains and desugars the entire chain into a single flat arrow function at compile time. `guard(g1) |> guard(g2) |> handler |> guard(post)` becomes sequential if-checks: pre-guards receive `req` and short-circuit on non-undefined return, the main handler runs if all pre-guards pass, and post-guards receive the response and can replace it. Zero runtime overhead - pure compile-time macro. Implementation: `packages/zigts/src/parser/parse.zig` (pipe chain collection), `packages/zigts/src/modules/compose.zig` (guard marker).

### Native Deploy

`zigttp deploy` turns the compiler-proven contract into a running service in one command. The pipeline auto-detects the handler file, service name, and `.env` from the current directory, prompts for a Zigttp access token when credentials are missing (with browser-based device login as fallback), and stores credentials at `~/.zigttp/credentials`. In the hosted flow, users create that token in `zigttp-admin`, the web control plane. The CLI then fetches a short-lived `DeploySession` from the zigttp control plane. The session carries OCI registry credentials, a Northflank scope and plan, a provider API token, and an optional expiry. `precompile.compileHandler()` runs in-process as a verification pre-check; on pass the orchestrator shells out to `zig build -Dhandler=... -Dtarget=x86_64-linux-musl` for the cross-compiled binary, packages it as an OCI image in Zig (`deploy/oci/{tar,layer,config,manifest,image}.zig`), pushes through the distribution protocol over `std.http.Client` (`deploy/oci/registry.zig`), and hands the digest to `deploy/northflank_adapter.zig` to create or update the service. The only external tool invoked is `zig` itself.

The orchestrator (`packages/runtime/src/deploy.zig`) wraps the registry push and the provider call in retry-once-on-401 helpers (`pushWithAuthRetry`, `executeWithAuthRetry`). Each helper proactively calls `control_plane.sessionRefreshIfExpiring` with a 120-second skew window before its phase, then catches `error.RegistryUnauthorized` or `error.ProviderUnauthorized` and force-refreshes the session. Refreshes go through `control_plane.swapRefreshedSession`, which asserts the new session carries the same `namespace` and `registry_host` as the old one; a mismatch frees the fresh session and returns `error.SessionIdentityChanged`, because the OCI image and registry ref are derived from those fields at the start of the run.

Reconciliation is driven by `.zigttp/deploy-state.json`, which stores only non-secret identifiers (`service_id`, `scope_id`, `plan_id`, `region`, `managed_env_keys`, `last_image_digest`). The orchestrator loads state before fetching the session so it can resolve the effective region from `--region`, then the previous deploy's region, then `default_region`. A second run for the same service reuses the stored service id and patches in place. A change to scope, region, plan, or removal of a previously managed env var raises a `replace_requires_confirm` action that prints a drift warning and exits with code 2 unless `--confirm` is passed. Even with `--confirm`, the old service is rebound and updated, never deleted.

After the push the orchestrator polls `northflank_adapter.waitForReadyDefault` until the service reports running (default deadline 120s, exit code 3 on timeout, exit code 4 on hard failure). `--no-wait` skips the poll. The poll loop owns a single threaded-IO backend via `RealSleeper` and drives a status-fetch + sleep cycle through injectable seams so tests run without network or real sleep. Output flows through `deploy/printer.zig`, which wraps stdout and stderr `std.Io.Writer` handles in a small `Printer` struct; rate-limited progress lines for the OCI push and provider call go through a `Progress` wrapper around the same printer.

Proof facts from the contract (proof level, env var names, egress hosts, cache namespaces, routes, handler properties, OWASP Top 10 coverage) are encoded as JSON arrays in OCI image labels on the config blob, so provenance survives in the registry. The portable extraction logic is reused from `packages/tools/src/deploy_manifest.zig`; the live orchestration lives under `packages/runtime/src/deploy/`. The legacy `DeployPlan` value type and `--dry-run --json` output have been removed; the runtime CLI is the only supported deployment path.

## Deployment Patterns

### Single Instance (Lambda-style)

```bash
./zigttp serve handler.js
```

Each instance handles one request at a time for isolation.

### Multiple Instances Behind Load Balancer

For high-throughput scenarios, deploy multiple instances. The small binary size (~500KB) and instant cold starts make horizontal scaling efficient.

### Container Deployment

```dockerfile
FROM scratch
COPY zig-out/bin/zigttp /zigttp
COPY handler.js /handler.js
EXPOSE 8080
ENTRYPOINT ["/zigttp", "serve", "-q", "-h", "0.0.0.0", "/handler.js"]
```

### FaaS Deployment

Build for target platform:

```bash
# AWS Lambda (x86-64)
zig build -Doptimize=ReleaseFast -Dtarget=x86_64-linux

# AWS Lambda (ARM64)
zig build -Doptimize=ReleaseFast -Dtarget=aarch64-linux

# With precompiled handler (fastest)
zig build -Doptimize=ReleaseFast -Dtarget=x86_64-linux -Dhandler=handler.js
```

Package as Lambda deployment:

```bash
zip function.zip bootstrap handler.js
aws lambda create-function --function-name my-function \
  --zip-file fileb://function.zip --runtime provided.al2 \
  --handler handler.handler --role arn:aws:iam::...
```
