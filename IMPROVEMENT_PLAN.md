# zigttp Fresh-Eyes Improvement Plan

Refreshed on 2026-06-16 at `a44cf60`. This replaces the stale 2026-06-10
audit plan, which still listed several features as missing after they had
landed.

Do not execute this as one large change. Each phase below should be a small,
reviewable patch with the listed verification. Before starting any phase,
re-open the cited files and drop items that no longer match the source.

## Completed In This Refresh Pass

- Fixed `String.prototype.charCodeAt` to return UTF-16 code units instead of
  UTF-8 bytes in `packages/zigts/src/builtins/string_builtins.zig`.
  Regression coverage: BMP and surrogate-pair cases.
- Fixed `Array.prototype.reduce` empty-array behavior in
  `packages/zigts/src/builtins/array.zig`: `[].reduce(fn)` now throws a
  TypeError instead of returning `undefined`, and `[].reduce(fn, initial)`
  returns the initial value without requiring the callback trampoline.

Verification run for those fixes:

- `zig build test-zigts`

## Phase 0 - Repair Docs Drift Before More Feature Work

Goal: make public docs match shipped module behavior and make drift detectable.

1. Update virtual-module docs to match shipped exports.
   - Evidence: `packages/modules/src/security/decode.zig` and
     `packages/modules/module-specs/security/decode.json` export
     `decodeFormMultipart`, but `docs/virtual-modules/README.md` still lists
     only `decodeJson`, `decodeForm`, and `decodeQuery`.
   - Evidence: `packages/modules/src/net/fetch.zig` and
     `packages/modules/module-specs/net/fetch.json` export `fetchWithRetry`, but
     `docs/virtual-modules/README.md` still lists only `fetch`.
   - Also clean stale multipart/module-work claims in `docs/roadmap.md` and
     `docs/threat-model.md`.

2. Strengthen `scripts/check-docs-drift.sh`.
   - Current gate counts module rows and specifiers only. It passes while export
     lists are stale.
   - Compare documented exports/capabilities against
     `packages/modules/module-specs/*.json` or a generated module catalog.

3. Fix example coverage claims.
   - `examples/README.md` claims `examples/modules/modules_all.ts` covers every
     virtual module; verify that claim or narrow the wording.
   - Add targeted smoke coverage for durable, parallel, websocket, and
     system-style examples only where stable.

Exit gate:

- `zig build test-docs-drift test-doc-links`
- `bash scripts/test-examples.sh` if examples change

## Phase 1 - Close Runtime Hardening Verification Gaps

Goal: treat landed B1 work as landed, then test the remaining seams.

Already landed and should not be reimplemented: request timeout plumbing,
keep-alive idle timeout, health/readiness routes, shutdown signal handling,
panic recovery wiring, and the `-m` memory limit path.

1. Replace stale B1 comments/placeholders in `packages/runtime/src/server_test.zig`.
   - The file header still says timeout, shutdown drain, probes, and panic
     isolation do not exist.
   - Some tests are still structural rather than exercising the HTTP accept path
     or probe routes.

2. Wire panic-isolation verification.
   - `scripts/test-panic-isolation.sh` exists, but `.github/workflows/ci.yml`
     does not run it and `build.zig` has no equivalent build step.

3. Expose request timeout through the runtime CLI or document why it is fixed.
   - `ServerConfig.timeout_ms` is wired to handler deadlines, but
     `runtime_cli.zig` exposes only `--outbound-timeout-ms`.
   - This is a contract change; define flag name, help text, docs, and tests in
     the same patch if implemented.

4. Finish access logging.
   - `ServerConfig.log_requests` exists and `-q` toggles it, but request/response
     paths still lack structured access logs. Keep raw headers/bodies out.

5. Prove request deadlines across compiled code.
   - Deadline checks are visible in interpreter back-edges/frame entry. No
     deadline references are present in `packages/zigts/src/jit`.
   - Add tests for already-hot compiled loops. If compiled loops can outrun the
     deadline, add JIT safepoints or inhibit JIT when request deadlines are
     active.

6. Audit shutdown's thread-safety contract.
   - `Server.shutdown` is documented as safe from any thread, while
     `Server.running` is a plain `bool` read in the accept loop and written by
     shutdown.
   - Either make it atomic with tests or narrow the documented contract.

Exit gate:

- `zig build test-server test-zruntime`
- `bash scripts/test-panic-isolation.sh` until represented by a build/CI step

## Phase 2 - Measurement Gates Before More Optimization

Goal: make performance and size claims reproducible before changing hot paths.

1. Extend measurement beyond the current microbench gate.
   - `zig build bench-check` compares selected `ops_per_sec` values against
     `benchmarks/perf-baseline.json`.
   - Add receipts for HTTP req/s, process-launch cold start, RSS, and release
     binary size because `docs/performance.md` discusses those numbers.

2. Add CI coverage gradually.
   - CI currently runs tests, release build, smoke, examples, and policy hash
     checks, but not `bench-check`, binary-size drift, cold-start, or RSS checks.
   - Start as artifacts/warnings; fail CI only after baselines are stable on
     macOS and Linux.

Exit gate:

- `zig build bench-check`
- Release artifact sizes recorded for `zigttp`, `zigttp-runtime`, and `zigts`

## Phase 3 - Build Features and Capability Surfacing

Goal: reduce deployed surface area and make unsupported capability use fail
early.

1. Add `-Druntime_sqlite`.
   - Root `build.zig` and `packages/zigts/build.zig` still include SQLite in
     non-analyzer builds; only `analyzer_only` strips it.
   - Excluding SQLite must remove `sqlite3.c` and the runtime SQL bridge from
     deployed builds.

2. Gate SQLite capability at compile/check time.
   - If SQLite is compiled out, importing `zigttp:sql` should fail with a clear
     diagnostic, not a link error or late runtime failure.
   - Existing `Effects<...>` and capability inference mean this is a
     build-feature/policy surfacing task, not a broad verifier rewrite.

3. Decide remaining `fetchWithRetry` semantics before coding.
   - `fetchWithRetry` exists with retry/backoff. Per-try timeout and
     circuit-breaker behavior are separate contract decisions.

Exit gate:

- Build and test SQLite-enabled and SQLite-disabled variants
- Clear diagnostic for `zigttp:sql` when SQLite is excluded
- Binary-size delta recorded

## Phase 4 - Analyzer and CLI Guardrails

Goal: fix integration-facing issues that can hide diagnostics or exhaust tool
processes.

1. Bound `--stdin-json` input.
   - File reads are capped, but `packages/tools/src/edit_simulate.zig` reads
     stdin JSON until EOF with append-only growth. `review-patch --stdin-json`
     uses the same path.

2. Fix `review-patch --diff-only` duplicate-diagnostic hiding.
   - `ViolationKey` excludes locations, so a newly introduced diagnostic with
     the same code/message as an existing diagnostic can be hidden.

3. Emit structured JSON for missing SQL schema.
   - `zigts check --json` exits directly on `MissingSqlSchema` in
     `packages/tools/src/zigts_cli.zig`, unlike other check errors.

Exit gate:

- `zig build test-cli test-zigts`
- Golden JSON/error-output tests for all changed paths

## Phase 5 - Remaining Supported-Subset Correctness

Goal: fix real JS-subset correctness issues without expanding the language.

1. Implement integer-key ordering for `Object.keys/values/entries`.
   - Current implementation delegates to hidden-class enumeration order in
     `packages/zigts/src/builtins/object_builtins.zig`.

2. Decide and test sparse-array behavior.
   - The object layer has sparse-hole safety notes, but sparse-array semantics
     are still not documented as supported or unsupported at the user-facing
     level.

3. Add sign-of-zero regression coverage where observable.
   - Keep this narrow; do not introduce a broad numeric semantics rewrite.

Exit gate:

- `zig build test-zigts`
- Focused regression tests near touched builtin/numeric modules

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

3. Replace only production-relevant `catch unreachable` sites.
   - The old count mixed tests, bounded buffers, and production paths. First
     produce a categorized list; then change only sites where propagation or an
     explicit panic improves correctness.

Exit gate:

- `zig build test test-server test-zruntime test-runtime-purity`
- `zig build bench-check` for request-path or JIT-adjacent movement

## Dropped From The Old Plan

- Dropped: implement `decodeFormMultipart`; it already exists.
- Dropped: implement `fetchWithRetry`; the wrapper already exists.
- Dropped: inline-cache slot-exhaustion fallback; codegen already falls back.
- Dropped: proof-cache `Vary` follow-up; non-empty `Vary` is already excluded
  and covered by tests.
- Narrowed: compile-time capability verification; remaining work is build
  feature/policy error surfacing.
- Narrowed: broad `catch unreachable`, `const`, and allocator cleanup sweeps.
- Deferred: VM-loop dedupe remains in `DEFERRED_VM_LOOP_DEDUPE_PLAN.md`.

## Verification Menu

- Docs: `zig build test-docs-drift test-doc-links`
- Server/runtime hardening: `zig build test-server test-zruntime`
- Engine/parser/JIT: `zig build test-zigts`
- CLI/analyzer tools: `zig build test-cli`
- Modules/governance: `zig build test-modules test-module-governance test-capability-audit`
- SDK/runtime purity: `zig build test-sdk test-runtime-purity`
- Examples: `bash scripts/test-examples.sh`
- Performance: `zig build bench-check`
- Aggregate: `zig build test`
