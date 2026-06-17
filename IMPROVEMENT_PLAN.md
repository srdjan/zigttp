# zigttp Fresh-Eyes Improvement Plan

Refreshed on 2026-06-17 against HEAD `13df867`. The worktree also had an
existing user-owned modification in `packages/zigts/src/jit/optimized.zig`, so
any JIT implementation phase must re-open that file before editing it.

This is an advisory plan only. Do not execute it as one large change. Each phase
below should be a small, reviewable patch with the listed verification. Before
starting any phase, re-open the cited files and drop items that no longer match
the source.

## Refresh Method

- Re-read `AGENTS.md`, `CLAUDE.md`, this plan, and
  `DEFERRED_VM_LOOP_DEDUPE_PLAN.md`.
- Audited docs, runtime, engine/JIT semantics, analyzer tooling, build flags,
  CI, examples, and security-sensitive docs directly from source.
- Ran four independent read-only critique passes over runtime hardening,
  analyzer tooling, engine semantics, and docs/build/capability surfacing.
- Kept only items backed by direct source evidence or by critique findings that
  were rechecked against the code.

## Recently Completed Fixes

These are already landed and should not be reintroduced as plan work:

- `String.prototype.charCodeAt` returns UTF-16 code units instead of UTF-8 bytes
  in `packages/zigts/src/builtins/string_builtins.zig`.
- `Array.prototype.reduce` handles empty arrays correctly:
  `[].reduce(fn)` throws a TypeError, and `[].reduce(fn, initial)` returns the
  initial value without calling the callback.

Verification that was run for those fixes:

- `zig build test-zigts`

## Completed In The 2026-06-17 Implementation Slice

- Repaired Phase 0 docs drift for virtual-module exports, capability inventory,
  threat-model chunked/multipart wording, roadmap multipart/runtime-hardening
  wording, and the `examples/modules/modules_all.ts` coverage claim.
- Strengthened `scripts/check-docs-drift.sh` so it compares documented module
  exports and capabilities against `packages/modules/module-specs/*.json`, not
  just module counts and specifier presence.
- Updated stale B1 wording in `packages/runtime/src/server_test.zig`.
- Added `zig build test-panic-isolation` and a CI step for it. The smoke script
  now builds against the emitted `zigttp` binary, creates a proof-friendly
  temporary handler, injects `HandlerPanicked` through the internal
  `--_debug-panic-path` serve flag, verifies HTTP 500, then verifies a
  subsequent `/ok` request succeeds.

Verification run for this implementation slice:

- `zig build test-cli test-docs-drift test-doc-links test-module-governance test-capability-audit test-server test-zruntime test-panic-isolation`

## Phase 0 - Repair Docs Drift Before More Feature Work

Goal: make public docs match shipped behavior and make drift detectable.

1. Update virtual-module export docs to match shipped specs.
   - Evidence: `packages/modules/src/security/decode.zig` and
     `packages/modules/module-specs/security/decode.json` export
     `decodeFormMultipart`, but `docs/virtual-modules/README.md` lists only
     `decodeJson`, `decodeForm`, and `decodeQuery`.
   - Evidence: `packages/modules/src/net/fetch.zig` and
     `packages/modules/module-specs/net/fetch.json` export `fetchWithRetry`,
     but `docs/virtual-modules/README.md` lists only `fetch`.
   - Keep this as documentation sync unless source has changed since this
     refresh.

2. Strengthen `scripts/check-docs-drift.sh`.
   - Current gate counts module rows and specifier presence only. It can pass
     while export lists or capability-bearing surfaces are stale.
   - Compare documented exports and capability entries against
     `packages/modules/module-specs/*.json` or a generated module catalog.

3. Fix capability and threat-model drift.
   - `docs/internals/capabilities.md` omits `zigttp:fetch`, while the fetch spec
     and binding require `network` and `runtime_callback`.
   - `docs/threat-model.md` says chunked transfer is rejected, but
     `packages/runtime/src/server.zig` decodes chunked request bodies and
     `docs/roadmap.md` already says chunked bodies are supported.
   - `docs/roadmap.md` and `docs/threat-model.md` still imply multipart parsing
     has no virtual-module support. The runtime still receives raw bodies, but
     `zigttp:decode` now exposes `decodeFormMultipart`.

4. Fix example coverage claims.
   - `examples/README.md` claims `examples/modules/modules_all.ts` touches every
     shipped virtual module, but that example imports only auth, validate,
     cache, and crypto today.
   - Either narrow the claim or add stable smoke coverage for the missing
     modules. Avoid fragile examples for runtime-flag-dependent modules unless
     the test setup supplies those flags.

Exit gate:

- `zig build test-docs-drift test-doc-links`
- `zig build test-module-governance test-capability-audit`
- `bash scripts/test-examples.sh` if examples change

## Phase 1 - Close Runtime Hardening Verification Gaps

Goal: treat landed B1 work as landed, then test and document the remaining
runtime lifecycle gaps.

Already landed and should not be reimplemented: request timeout plumbing,
keep-alive idle timeout, health/readiness routes, shutdown signal handling,
panic recovery wiring, and the `-m` memory limit path.

1. Replace stale B1 test wording with accept-path coverage.
   - `packages/runtime/src/server_test.zig` still has header text saying several
     B1 features do not exist.
   - Timeout coverage is real, but shutdown/readiness/panic tests are still
     partly structural. Replace comments/placeholders with tests that exercise
     the public accept path, probe routes, and process-survival behavior.

2. Wire panic-isolation verification.
   - `scripts/test-panic-isolation.sh` exists and checks the important behavior:
     `/crash` returns 500, the process survives, and the next request succeeds.
   - `.github/workflows/ci.yml` does not run that script, and `build.zig` has no
     equivalent build step.

3. Split the request-timeout contract before adding CLI flags.
   - `ServerConfig.timeout_ms` currently feeds socket read/write behavior,
     request body budget, handler deadline, and shutdown grace.
   - `runtime_cli.zig` exposes `--outbound-timeout-ms`, but no handler/request
     timeout flag.
   - A naive `--request-timeout-ms` mapped to `ServerConfig.timeout_ms` would
     change multiple lifecycle semantics. Decide whether to document the fixed
     behavior, add separate knobs, or intentionally keep one knob, then update
     help text/docs/tests in the same patch.

4. Finish or remove access logging.
   - `ServerConfig.log_requests` exists and `-q` toggles it, but request
     completion paths do not emit structured access logs.
   - If implemented, log method, path, status, duration, and request id only.
     Do not log raw bodies, raw headers, secrets, or PII.
   - If not implemented yet, adjust CLI/help/docs so `-q` is not misleading.

5. Prove request deadlines across compiled code.
   - Interpreter back-edges and frame entry check deadlines; JIT loop/back-edge
     emitters do not show an equivalent deadline poll.
   - The current timeout test disables JIT, so it does not prove hot compiled
     loops respect deadlines.
   - First add a failing-or-passing hot-loop timeout test. If compiled loops can
     outrun the deadline, either add JIT safepoints or inhibit JIT when active
     request deadlines are configured.

6. Audit shutdown's thread-safety contract.
   - `Server.shutdown` is documented as safe from any thread, while
     `Server.running` is a plain `bool` read in the accept loop and written by
     shutdown.
   - Either make the contract true with synchronization and tests, or narrow the
     documented contract.

Exit gate:

- `zig build test-server test-zruntime`
- `zig build && bash scripts/test-panic-isolation.sh` until represented by a
  build/CI step
- Focused CLI parse/help tests if timeout or logging flags change

## Phase 2 - Measurement Gates Before More Optimization

Goal: make performance and size claims reproducible before changing hot paths.

1. Extend measurement beyond the current microbench gate.
   - `zig build bench-check` compares selected `ops_per_sec` values against
     `benchmarks/perf-baseline.json`.
   - `docs/performance.md` also claims HTTP req/s, process-launch cold start,
     RSS, and release binary size. Add receipts for those values before relying
     on the claims for release decisions.

2. Add CI coverage gradually.
   - CI runs tests, release build, smoke, examples, policy hash checks, and
     module/doc governance, but not `bench-check`, binary-size drift,
     cold-start, RSS, or HTTP throughput receipts.
   - Start as artifacts/warnings; fail CI only after baselines are stable on
     macOS and Linux.

Exit gate:

- `zig build bench-check`
- Release artifact sizes recorded for `zigttp`, `zigttp-runtime`, and `zigts`
- Cold-start, RSS, and HTTP throughput receipts recorded on at least one
  stable runner before becoming blocking

## Phase 3 - Build Features and Capability Surfacing

Goal: reduce deployed surface area and make unsupported capability use fail
early.

1. Design `-Druntime_sqlite` as a build contract before coding.
   - Root `build.zig` and `packages/zigts/build.zig` include SQLite in
     non-analyzer builds; only `analyzer_only` strips it.
   - Normal builtins always register `zigttp:sql`.
   - Keep the current default unless the phase explicitly changes it. A disabled
     variant must remove `sqlite3.c`, the runtime SQL bridge, and any misleading
     capability surface.

2. Gate SQLite capability at compile/check time.
   - If SQLite is compiled out, importing `zigttp:sql` should fail with a clear
     diagnostic, not a link error or late runtime failure.
   - Existing `Effects<...>` and capability inference make this a
     build-feature/policy surfacing task, not a broad verifier rewrite.

3. Decide `fetchWithRetry` semantics before adding behavior.
   - `fetchWithRetry` already exists with retry/backoff.
   - Per-try timeout and circuit-breaker behavior are separate API contract
     decisions. Document the contract first, then add tests and implementation
     only for the chosen behavior.

Exit gate:

- Build and test SQLite-enabled and SQLite-disabled variants
- Clear diagnostic for `zigttp:sql` when SQLite is excluded
- Binary-size delta recorded
- Fetch retry docs/tests updated only after semantics are decided

## Phase 4 - Analyzer and CLI Guardrails

Goal: fix integration-facing issues that can hide diagnostics, exhaust tool
processes, or mislead machine callers.

1. Bound `--stdin-json` input.
   - File reads are capped, but `packages/tools/src/edit_simulate.zig` reads
     stdin JSON until EOF with append-only growth.
   - `review-patch --stdin-json` uses the same path.
   - Reject oversize stdin before parse and return a structured error.

2. Fix `review-patch --diff-only` duplicate-diagnostic hiding.
   - `ViolationKey` hashes diagnostic code and message while intentionally
     excluding location.
   - A newly introduced violation with the same code/message as an existing
     violation can be hidden.
   - Replace or augment the key with a stable diagnostic identity that
     distinguishes independent occurrences without being fragile to unrelated
     line movement.

3. Emit structured JSON for missing SQL schema.
   - `zigts check --json` exits directly on `MissingSqlSchema` in
     `packages/tools/src/zigts_cli.zig`.
   - `zigttp check --json` delegates through the same analyzer path.
   - Machine callers should receive JSON diagnostics when JSON mode was
     requested.

4. Thread SQL schema paths through analyzer workflows.
   - `edit-simulate` discovers schema paths, but `review-patch` and
     `prove-behavior` currently call the shared simulation path without a
     schema.
   - SQL handlers should work in these review/proof workflows when the schema is
     available.

5. Correct `prove-behavior` receipt wording or implementation.
   - Source comments/help describe signed proof receipts, but the shared command
     path emits only a verdict today.
   - Low-risk path: correct docs/help. Higher-risk path: implement signing and
     ledger append with tests.

Exit gate:

- `zig build test-cli test-zigts`
- `zig build test-expert-golden` after adding direct golden/fixture coverage for
  changed `check`, `edit-simulate`, `review-patch`, and `prove-behavior`
  surfaces

## Phase 5 - Remaining Supported-Subset Correctness

Goal: fix real JS-subset correctness issues without expanding the language.

1. Implement integer-key ordering for `Object.keys/values/entries`.
   - `Object.keys`, `Object.values`, and `Object.entries` delegate to
     hidden-class enumeration order today.
   - JS ordinary own-property order requires array-index keys first in numeric
     order, then remaining string keys in insertion order.
   - Keep the fix local to enumerable own properties supported by the current
     object model.

2. Decide and fix sparse-array elision behavior.
   - The parser accepts array holes, and codegen materializes `null_node` holes
     as `undefined`.
   - Higher-order array methods then call callbacks for those positions with
     `undefined`, diverging from JS hole-skipping behavior for
     `[ , 1].map/forEach/reduce`.
   - Either reject sparse elisions at parse/check time as unsupported, or model
     holes explicitly and update array builtins. Do not silently keep the current
     half-supported behavior without docs/tests.

3. Preserve negative zero where it is observable.
   - `JSValue.fromFloat` can represent `-0`, but `Math.sign(-0)` returns integer
     `0`, and `JSON.parse("-0")` routes through integer parsing and returns
     `+0`.
   - Add focused tests that observe the sign through division, then fix only the
     affected builtin/parser paths.

4. Reject trailing garbage in `JSON.parse` and `JSON.tryParse`.
   - `parseJsonValue` returns after parsing one value and does not check that
     only whitespace remains.
   - `JSON.parse("1 trailing")` should fail instead of accepting the numeric
     prefix.
   - This is boundary validation, not a broad JSON rewrite.

5. Refresh the deferred VM-loop dedupe plan before code work.
   - The old residual post-fault side-effect claim is now narrower: current
     frame boundaries reconcile pending JIT exceptions, and parity coverage
     exists for post-fault stores/calls.
   - Before adding emitter-level exception checks, reproduce any remaining
     single-frame side-effect problem with a failing parity test.

Exit gate:

- `zig build test-zigts`
- Focused filters while developing:
  `zig build test-zigts -- --test-filter "Object.keys"`
  `zig build test-zigts -- --test-filter "sparse array"`
  `zig build test-zigts -- --test-filter "negative zero"`
  `zig build test-zigts -- --test-filter "JSON.parse"`

## Phase 6 - Architecture and Idiomatic-Zig Cleanup

Goal: reduce coupling after verification gates exist.

1. Scope the engine facade to request-path boundaries first.
   - `engine_adapter.zig` exists, but `zruntime.zig` and `runtime_pool.zig` still
     import engine internals directly.
   - Do not apply a blanket ban to analyzer/dev/proof tooling until those
     boundaries are separated.

2. Split `zruntime.zig` by concern in stop-after-each steps.
   - Candidate slices: compile/load/bytecode validation, request/fetch/response,
     durable state, websocket callbacks.
   - Preserve `Runtime` and `HandlerPool` ownership/lifetime.
   - Do this only after Phase 1 and Phase 2 gates exist, because this is broad
     request-path movement.

3. Re-audit `catch unreachable` before adding a cleanup phase.
   - The runtime critique found no high-confidence production request-lifecycle
     `catch unreachable` target beyond comments in the inspected slice.
   - Do not keep this as a broad cleanup item unless a fresh repo-wide
     `rg -n "catch unreachable" packages/` pass produces real production
     targets.

Exit gate:

- `zig build test test-server test-zruntime test-runtime-purity`
- `zig build bench-check` for request-path or JIT-adjacent movement

## Dropped Or Narrowed From The Old Plan

- Dropped: implement `decodeFormMultipart`; it already exists.
- Dropped: implement `fetchWithRetry`; the wrapper already exists.
- Dropped: inline-cache slot-exhaustion fallback; codegen already falls back.
- Dropped: proof-cache `Vary` follow-up; non-empty `Vary` is already excluded
  and covered by tests.
- Dropped for now: broad production `catch unreachable` cleanup. Re-add only
  after a fresh categorized list identifies real production targets.
- Narrowed: compile-time capability verification; remaining work is build
  feature/policy error surfacing.
- Narrowed: broad `const` and allocator cleanup sweeps.
- Narrowed: VM-loop dedupe remains deferred, but its plan must be refreshed to
  reflect current post-fault coverage before emitter work starts.

## Verification Menu

- Docs: `zig build test-docs-drift test-doc-links`
- Server/runtime hardening: `zig build test-server test-zruntime`
- Engine/parser/JIT: `zig build test-zigts`
- CLI/analyzer tools: `zig build test-cli test-expert-golden`
- Modules/governance: `zig build test-modules test-module-governance test-capability-audit`
- SDK/runtime purity: `zig build test-sdk test-runtime-purity`
- Examples: `bash scripts/test-examples.sh`
- Performance: `zig build bench-check`
- Aggregate: `zig build test`
