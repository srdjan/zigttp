# zigttp Technical Audit -> Multi-Phase Improvement Plan

## Context

This plan is the output of a comprehensive read-only technical and architectural
audit of `zigttp` (230K LOC of Zig across 7 packages), run as a 12-agent
workflow covering the runtime/server, the `zigts` engine (IR/bytecode, two-tier
JIT, generational GC), the `zigttp:*` modules, engine<->runtime coupling,
binary-size/dependency management, and idiomatic-Zig quality. The audit also
identified VM-loop duplication; that work is now tracked in
[Deferred VM Loop Dedupe Plan](DEFERRED_VM_LOOP_DEDUPE_PLAN.md). Findings were
cross-checked against source by an adversarial critic and by direct
verification.

**Why this work matters.** The architecture is fundamentally sound - the
generational GC is real (nursery + tenured + remembered set + tri-color
marking, `gc.zig`), the three-binary split is enforced (pi/expert kept out of
`zigts`/runtime via `check-runtime-purity.sh`, gated 9x in `build.zig`), and the
engine/runtime boundary already routes through a public `zigts` module plus
`engine_adapter.zig`. But the audit surfaced three classes of issue that block
production-grade FaaS use:

1. **Confirmed correctness bugs** - most importantly, the property-store
   inline-cache fast path (`interpreter.zig:1145,1156`) calls `obj.setSlot()`
   directly, bypassing the write barrier that the slow path uses via
   `setPropertyChecked`. A tenured->nursery pointer written this way never
   enters the remembered set: a latent use-after-free, today masked by the
   per-request arena model. Two sibling GC findings (unrooted float-constant-pool
   boxes; `heap_ptr` init in non-hybrid mode) are in the same family.
2. **Production-readiness gaps** - no per-request timeout (`keep_alive_timeout_ms`
   is defined at `server.zig:1013` but never read), no graceful shutdown/signal
   handling, no health/readiness probes, no structured logs, no panic isolation,
   no per-handler memory cap. And the headline performance claims (4.8MB binary,
   112k req/s, 3.5ms cold start, 13MB RSS) are documented but **not** measured by
   the in-repo benchmark (`benchmark.zig` only runs a latency corpus) and not
   CI-gated.
3. **Maintainability debt** - `zruntime.zig` is 7,846 LOC conflating
   engine-adapter and server concerns; runtime tests and wiring still reach into
   engine internals; SQLite (`deps/sqlite/sqlite3.c`) links into the deployed
   runtime unconditionally.

**Intended outcome.** A sequenced near-term plan that lands a foundation of
correctness fixes and safety nets first, then runs two interleaved tracks:
engine-boundary work and production FaaS hardening. Scope decisions confirmed
with the user: directory work is facade completion (packages are already split),
and new modules are table-stakes only (multipart + fetch resilience), with
cloud-adapter modules documented as a separate evaluated track. VM-loop dedupe is
deferred until the production table stakes and engine facade gates are green.

> Constraints honored throughout: never add the deliberately-excluded JS features
> (classes/async/while/switch/try-catch/regex); preserve the small-binary +
> zero-runtime-dependency goal; never weaken the attestation/proof or
> request-isolation guarantees. JavaScript here has no `try/catch`, so timeouts
> and shutdown are **cooperative + watchdog-enforced at the isolate level**, never
> exception-based unwinding inside a handler.

---

## Phase 0 - Foundation: Correctness Fixes + Test/Bench Gates

Lands before any high-blast-radius refactor. These are the safety nets that make
Tracks A and B safe to run in parallel.

### 0a. Confirmed GC correctness bugs (memory safety)
- **Write barrier on the IC store fast path.** Route `put_field_ic` slot writes
  through the barrier-enforcing path (replicate the `setPropertyChecked`/
  `setIndexChecked` barrier, or call a `writeBarrierIfCrossGen` before
  `obj.setSlot`). Then audit every `setSlot` site (37 today vs 9 `writeBarrier`
  sites) and every STORE-family opcode in both `interpreter.zig` and
  `jit/baseline.zig`. Add a debug-build assertion that records tenured->nursery
  edges. Files: `packages/zigts/src/interpreter.zig:1001,1145,1156`,
  `packages/zigts/src/jit/baseline.zig`, `packages/zigts/src/context.zig`,
  `packages/zigts/src/gc.zig`.
- **Root the float-constant-pool boxes.** `FloatConstantPool` boxes are
  GC-allocated (`gc.zig` `allocFloat`) but not in the root set; minor GC can
  relocate/reclaim them. Register them as roots (or move to a non-collected
  arena). Files: `packages/zigts/src/gc.zig` (`FloatConstantPool`),
  `context.zig`.
- **Validate `heap_ptr` initialization in non-hybrid mode.** Confirm `setHeap()`
  is always called on the persistent-server path before tenured objects
  accumulate; add an assertion + bounded major-GC threshold. Files: `gc.zig`,
  `pool.zig`, `packages/runtime/src/zruntime.zig`.
- Also from the GC audit: prevent `registerObject` duplicates, cap the major-GC
  threshold growth, harden `writeBarrier` OOM handling, add pointer-alignment
  validation in `value.zig` NaN-boxing.

### 0b. Test + measurement gates (unblock the refactors)
- **`test-server` integration suite** - handler execution, error paths,
  timeouts, keep-alive, graceful shutdown; the gate for the engine/runtime
  boundary refactor. (The coupling audit notes JIT/interpreter internals are
  poked directly from `zruntime.zig` tests at `:7515-7526` - these move behind
  the facade.)
- **Binary-size baseline + CI gate** - measure per-component size (`bloaty`/`nm`)
  for all three binaries, record the breakdown, fail CI on >2% drift. Reuse the
  `check-runtime-purity.sh` gating pattern already wired into `build.zig`.
- **Performance receipts** - extend `packages/runtime/src/benchmark.zig`
  (`runHandlerCorpus`) and `zig build bench`/`bench-check` to actually measure
  req/s, process-launch cold-start, and RSS, so the doc claims become reproducible
  and gated. (Connects to the deferred "performance as proof" idea - signed
  reproducible perf receipts via `zigttp bench`.)

**Verify:** `zig build test-zigts`, new `zig build test-server`, SIGTERM drains
in the integration suite; binary-size + perf baselines recorded in CI.

---

## Track A - Engine Boundary & Code Quality (runs after Phase 0, interleaved with Track B)

### A1. Complete the engine<->runtime facade
Packages are already split and `engine_adapter.zig` exists - **extend it, don't
start over.**
- Promote `engine_adapter.zig` into a strict `packages/runtime/src/engine_facade.zig`
  that is the **only** runtime entry point into the engine; eliminate the direct
  reaches into engine internals from `zruntime.zig` (`:606,838,914-925,1118,1215,
  1288,2455,7515-7526`).
- Add `packages/zigts/src/runtime_contract.zig` exposing **stable public types**
  (opaque `Runtime`, `PrecompiledBytecode`, a bytecode-cache interface) separate
  from analyzer-only internals; audit which `root.zig` exports are runtime-safe
  vs analyzer-only and document the split.
- Move JIT-knob manipulation behind a `RuntimeTuning` interface on the facade.

### A2. Split `zruntime.zig` (7,846 LOC) by concern
Extract `runtime_engine` (compile/dispatch/bytecode-validate), `request_handler`
(HTTP/fetch/response), and `durable_adapter` (trace/durable state) into sibling
modules. **Preserve the exact call ABI and ownership** - per the critic,
inverting `Runtime`/`HandlerPool` ownership risks reference cycles in the
lock-free pool, so change visibility/placement, not lifecycle. Do **not** relocate
server-owned fields into the pool context in this pass.

### A3. Idiomatic-Zig cleanup (Zig 0.16 / Zen-of-Zig)
- Replace the production-path `catch unreachable` sites (25 total; e.g.
  `precompile.zig:1443`, `cli_help.zig:215`) with explicit error propagation or a
  documented `@panic(@errorName(err))`.
- `const`-by-default and allocator-field-reduction passes over the grep-identified
  candidates (treat as candidates - most `var`/stored-allocator uses are
  legitimate; only change the ones that are genuinely unreassigned / aware-container
  shaped). Files incl. `context.zig`, `pool.zig`, `gc.zig:85`.
- Consolidate scattered error sets into semantically-grouped sets; document the
  raised comptime branch quotas (`interpreter.zig:212`, `module_binding.zig:1457`).

### A4. Binary-size / dependency management
- **`-Druntime_sqlite` build flag** to exclude `deps/sqlite/sqlite3.c` from the
  deployed runtime. Reuse the `-Dstudio`/`-Dedge` + `runtime_features.zig`
  compile-out pattern. Pair with **compile-time capability gating** (A-cross-B):
  if SQLite is excluded, a handler importing `zigttp:sql` must fail at compile
  time with a clear message via `module_binding.zig`, not a link error. CI builds
  both variants and asserts the size reduction.
- Optional `proof_review` feature gate in the dev CLI (2,669 LOC), same pattern.
- Document binary composition per package. (Defer the tools-package modularization
  - XL effort, ~3-5% return.)

---

## Track B - Production FaaS Table Stakes & Hardening (runs interleaved with Track A)

### B1. Production-safety hardening (the urgent runtime gaps)
- **Per-request deadline** - cooperative checks at interpreter back-edges/
  safepoints (the loop already has JIT/GC safepoints) plus a **watchdog that
  abandons the runtime slot** (tear down the isolate) on timeout. No JS-level
  cancellation token (no `try/catch` to unwind). Wire the unused
  `keep_alive_timeout_ms` (`server.zig:1013`) as the per-request idle timeout.
- **Graceful shutdown** - SIGTERM/SIGINT handler, set `running=false`, stop
  accepting, cooperatively drain in-flight requests, force-exit after a grace
  timeout (Zig threads aren't cancellable - drain is best-effort + bounded).
  Files: `server.zig:1682,1710-1729`.
- **`/_health` + `/_readiness`** built-in probes (readiness returns 503 on pool
  exhaustion). `server.zig:1684`.
- **Structured JSON logs** to stderr (request/response, pool metrics, errors,
  request IDs) with a configurable level. `server.zig:461,558,612,1021`.
- **Panic isolation** - wrap `executeHandlerBorrowed` so a handler panic returns
  500 and recycles the slot instead of killing the worker thread.
  `runtime_pool.zig:293-326`.
- **Per-handler memory cap + OOM handler** via the arena size ceiling (`-m`).
  `server.zig:772`.

### B2. Table-stakes modules (confirmed in scope)
- **`decodeFormMultipart`** in `zigttp:decode` (`packages/modules/src/security/
  decode.zig`) - closes the documented multipart gap; reuse the `ModuleBinding`
  pattern.
- **Fetch resilience wrapper** - `fetchWithRetry` (exponential backoff, per-try
  timeout, circuit breaker) wrapping `zigttp:fetch`
  (`packages/modules/src/net/fetch.zig`).

### B3. Compile-time capability verification
Lift capability enforcement from runtime-only (`module_binding.zig:126-129,
235-238`) to a compile-time check in the verifier/type-checker, surfacing
capability mismatches (and SQLite-excluded `zigttp:sql` use, see A4) as
compile errors with suggestions.

### B4. JIT compile-time DoS guard
Add a per-function compile-time budget for baseline/optimized JIT and a per-
function deopt-count cap + recompilation backoff (today only the optimized tier
checks deopt-storm suppression; baseline can promote->deopt->promote in a loop).
Files: `interpreter/jit_compile.zig:310-362`, `jit/deopt.zig`.

### B5. Spec-compliance correctness (within the supported subset)
Low-risk conformance fixes, no new language features:
- `String.prototype.charCodeAt` should return UTF-16 code units, not bytes
  (`builtins/string_builtins.zig:44`).
- Integer-key canonicalization in `Object.keys/values/entries` enumeration order
  (`object.zig:2056`, `builtins/object_builtins.zig`).
- Inline-cache slot-exhaustion graceful fallback (IC_CACHE_SIZE=512,
  `codegen.zig:92`).
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
  `zig build test-cli`, plus the new `zig build test-server`. Run per-target (not
  the combined step - it can stall in the test-runner IPC).
- **Phase 0 gates stay green** after every Track A/B change: `test-server`,
  binary-size within 2%, perf receipts not regressed.
- **GC fixes:** debug-build tenured->nursery edge assertion + GC stress test under
  the leak-detecting allocator (`std.testing.allocator` / `FailingAllocator`).
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
