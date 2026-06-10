# zigttp Technical Audit -> Multi-Phase Improvement Plan

> Trimmed 2026-06-10. This plan came out of the 2026-06-05 12-agent audit.
> Completed work has been removed: Phase 0a (GC correctness fixes: IC-store
> write barrier, float-constant-pool rooting, `heap_ptr` validation) and the
> Phase 0b `test-server` integration suite are landed and green. What follows
> is only the live remainder, re-checked against source on 2026-06-10.

## Remaining Phase 0b - Measurement Gates

- **Binary-size baseline + CI gate** - measure per-component size (`bloaty`/`nm`)
  for all three binaries, record the breakdown, fail CI on >2% drift. Reuse the
  `check-runtime-purity.sh` gating pattern already wired into `build.zig`.
- **Performance receipts** - `zig build bench`/`bench-check` exist and compare a
  latency corpus against a checked-in baseline, but req/s, process-launch
  cold-start, and RSS are still not measured in-repo, so the headline doc claims
  remain unreproduced and ungated. (Connects to the deferred "performance as
  proof" idea - signed reproducible perf receipts via `zigttp bench`.)

---

## Track A - Engine Boundary & Code Quality

### A1. Complete the engine<->runtime facade
Packages are already split and `engine_adapter.zig` exists - **extend it, don't
start over.**
- Promote `engine_adapter.zig` into a strict `packages/runtime/src/engine_facade.zig`
  that is the **only** runtime entry point into the engine; eliminate the direct
  reaches into engine internals from `zruntime.zig`.
- Add `packages/zigts/src/runtime_contract.zig` exposing **stable public types**
  (opaque `Runtime`, `PrecompiledBytecode`, a bytecode-cache interface) separate
  from analyzer-only internals; audit which `root.zig` exports are runtime-safe
  vs analyzer-only and document the split.
- Move JIT-knob manipulation behind a `RuntimeTuning` interface on the facade.

### A2. Split `zruntime.zig` by concern
A first extraction pass moved ~1,150 LOC into sibling modules, but the file is
back to ~8,100 LOC as of 2026-06-10. Extract `runtime_engine`
(compile/dispatch/bytecode-validate), `request_handler` (HTTP/fetch/response),
and `durable_adapter` (trace/durable state) into sibling modules. **Preserve
the exact call ABI and ownership** - per the critic, inverting
`Runtime`/`HandlerPool` ownership risks reference cycles in the lock-free pool,
so change visibility/placement, not lifecycle. Do **not** relocate server-owned
fields into the pool context in this pass.

### A3. Idiomatic-Zig cleanup (Zig 0.16 / Zen-of-Zig)
- Replace the production-path `catch unreachable` sites (~31 non-test sites as
  of 2026-06-10) with explicit error propagation or a documented
  `@panic(@errorName(err))`.
- `const`-by-default and allocator-field-reduction passes over the grep-identified
  candidates (treat as candidates - most `var`/stored-allocator uses are
  legitimate; only change the ones that are genuinely unreassigned / aware-container
  shaped).
- Consolidate scattered error sets into semantically-grouped sets; document the
  raised comptime branch quotas (`interpreter.zig`, `module_binding.zig`).

### A4. Binary-size / dependency management
- **`-Druntime_sqlite` build flag** to exclude `deps/sqlite/sqlite3.c` from the
  deployed runtime. Reuse the `-Dstudio`/`-Dedge` + `runtime_features.zig`
  compile-out pattern. Pair with **compile-time capability gating** (A-cross-B):
  if SQLite is excluded, a handler importing `zigttp:sql` must fail at compile
  time with a clear message via `module_binding.zig`, not a link error. CI builds
  both variants and asserts the size reduction.
- Optional `proof_review` feature gate in the dev CLI, same pattern.
- Document binary composition per package. (Defer the tools-package modularization
  - XL effort, ~3-5% return.)

---

## Track B - Production FaaS Table Stakes & Hardening

### B1. Production-safety hardening (the urgent runtime gaps)
- **Per-request deadline** - cooperative checks at interpreter back-edges/
  safepoints (the loop already has JIT/GC safepoints) plus a **watchdog that
  abandons the runtime slot** (tear down the isolate) on timeout. No JS-level
  cancellation token (no `try/catch` to unwind). Wire the still-unread
  `keep_alive_timeout_ms` (`server.zig:1027`) as the per-request idle timeout.
- **Graceful shutdown** - SIGTERM/SIGINT handler, set `running=false`, stop
  accepting, cooperatively drain in-flight requests, force-exit after a grace
  timeout (Zig threads aren't cancellable - drain is best-effort + bounded).
- **`/_health` + `/_readiness`** built-in probes (readiness returns 503 on pool
  exhaustion).
- **Structured JSON logs** to stderr (request/response, pool metrics, errors,
  request IDs) with a configurable level.
- **Panic isolation** - wrap `executeHandlerBorrowed` so a handler panic returns
  500 and recycles the slot instead of killing the worker thread
  (`runtime_pool.zig`).
- **Per-handler memory cap + OOM handler** via the arena size ceiling (`-m`).

### B2. Table-stakes modules (confirmed in scope)
- **`decodeFormMultipart`** in `zigttp:decode` (`packages/modules/src/security/
  decode.zig`) - closes the documented multipart gap; reuse the `ModuleBinding`
  pattern.
- **Fetch resilience wrapper** - `fetchWithRetry` (exponential backoff, per-try
  timeout, circuit breaker) wrapping `zigttp:fetch`
  (`packages/modules/src/net/fetch.zig`).

### B3. Compile-time capability verification
Lift capability enforcement from runtime-only (`module_binding.zig`) to a
compile-time check in the verifier/type-checker, surfacing capability
mismatches (and SQLite-excluded `zigttp:sql` use, see A4) as compile errors
with suggestions.

### B4. JIT compile-time DoS guard
A per-context native JIT code cap with full-context eviction exists. Still
missing: a per-function compile-time budget for baseline/optimized JIT and a
per-function deopt-count cap + recompilation backoff (today only the optimized
tier checks deopt-storm suppression; baseline can promote->deopt->promote in a
loop). Files: `interpreter/jit_compile.zig`, `jit/deopt.zig`.

### B5. Spec-compliance correctness (within the supported subset)
Low-risk conformance fixes, no new language features:
- `String.prototype.charCodeAt` should return UTF-16 code units, not bytes
  (`builtins/string_builtins.zig:44`).
- Integer-key canonicalization in `Object.keys/values/entries` enumeration order
  (`object.zig`, `builtins/object_builtins.zig`).
- Inline-cache slot-exhaustion graceful fallback (`IC_CACHE_SIZE`).
- Document sparse-array non-compliance + regression test; `Array.prototype.reduce`
  empty-array-without-initial guard; sign-of-zero observability. (All low
  priority.)

---

## Deferred Work

VM-loop dedupe is not part of this near-term plan. It lives in
[Deferred VM Loop Dedupe Plan](DEFERRED_VM_LOOP_DEDUPE_PLAN.md) and starts only
after the production table stakes, engine facade, and measurement gates are
stable.

Per the table-stakes-only decision, these are evaluated FaaS gaps that need cloud
adapters / a hosted-control-plane decision and are also out of scope here:
`zigttp:secrets` (Secrets Manager / Key Vault), `zigttp:storage` (S3/GCS/Blob),
`zigttp:queue` (SQS/SNS/PubSub/Service Bus), `zigttp:schedule` (cron), edge KV
(Durable Objects / CF KV), streaming request/response, distributed tracing
(OpenTelemetry). Also deferred: Windows JIT fallback, x86-64 loop-register
expansion (1 reg vs ARM64's 6), monomorphic call inlining in the optimized tier.

## Follow-up audit needed (coverage the workflow did not reach)

Schedule a second targeted audit for: the WebSocket module surface; attestation
signature + certificate-chain verification depth; durable-recovery correctness
(flock concurrency); contract validation completeness; module-binding cross-
handler isolation side-channels; proof-cache `Vary`/multi-variant edge cases.

---

## Verification

- **Per change:** `zig build test-zigts`, `zig build test-zruntime`,
  `zig build test-cli`, plus `zig build test-server`. Run per-target (not
  the combined step - it can stall in the test-runner IPC).
- **Measurement gates stay green** after every Track A/B change: `test-server`,
  binary-size within 2% once the gate lands, perf receipts not regressed.
- **Facade refactor:** `test-server` green + `zig build bench` shows no hot-path
  regression; confirm no runtime file imports engine internals outside
  `engine_facade.zig` (grep gate).
- **SQLite exclusion:** build + test both `-Druntime_sqlite` on/off; assert the
  size reduction and the compile-time error for `zigttp:sql` when excluded.
- **Module governance:** `zig build test-module-governance`,
  `zig build test-capability-audit`, `bash scripts/test-examples.sh` for new
  modules.
- **Hardening:** SIGTERM drain test; `curl /_health` -> 200, `/_readiness` -> 503
  on pool exhaustion; panic-handler test handler returns 500 and the worker
  survives; per-request timeout abandons the slot under a slow handler.
