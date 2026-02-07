# zigttp Codebase Audit (2026-02)

Date: 2026-02-07  
Scope: `src/` runtime/server layer and `zts/` engine layer  
Reference docs reviewed: `docs/performance.md`, `docs/architecture.md`, `docs/cold-start-optimization.md`, `docs/oop-removal-plan.md`, `docs/ts-to-zig-transpile.md`

## Executive Summary

The codebase has strong foundations (pool isolation model, shape caching, two-pass parser pipeline, hybrid allocation), but several high-severity correctness and safety issues are present in production paths:

1. An unsafe pointer cast in builtins can reinterpret non-object pointers as `JSObject`.
2. Static file serving is effectively disabled on the active threaded backend path.
3. HTTP status and integer serialization paths contain unchecked casts/overflow edge cases.

The highest ROI path is to fix these first, then address performance inconsistencies (PIC width mismatch, disabled thread-local pooling), and finally reduce structural drift (duplicated request paths, dual hidden-class systems, stale docs/tests).

## Implementation Status (2026-02-07)

Completed in this pass:

- Phase 1 critical fixes
  - Hardened builtin object extraction (`zts/builtins.zig`): `getObject` now requires `isObject()`.
  - Restored threaded static-file serving (`src/server.zig`): removed 404 stub and implemented real sync static response path with conditional ETag handling.
  - Normalized response status coercion (`zts/http.zig`) across `Response.json/rawJson/text/html/redirect` and `new Response(...)`.
  - Fixed `i32` min-value integer serialization overflow in JSON writer (`zts/http.zig:writeInt`).
- Phase 3 quality/stability improvements
  - Fixed leak in interpreter test setup (`zts/interpreter.zig`).
  - Added stripper option to suppress expected-error logging in tests (`zts/stripper.zig`), preserving runtime diagnostics.
  - Hardened object teardown against arena/double-destroy pitfalls (`zts/object.zig`).
- Phase 4 hardening
  - Added canonical-path static root validation to block symlink escapes (`src/server.zig` async and sync static serving paths).

Validation status after implementation:

- `zig build test` passed
- `zig build test-zts` passed
- `zig build test-zruntime` passed
- `zig build bench` passed

## Alignment With Existing Initiatives

This audit intentionally avoids duplicating:

- OOP-removal work already tracked in `docs/oop-removal-plan.md`.
- TS->Zig transpilation roadmap in `docs/ts-to-zig-transpile.md`.
- Existing cold-start work centered on embedded bytecode in `docs/cold-start-optimization.md`.

Recommendations below focus on uncovered correctness/security/performance gaps that are not already prioritized there.

## Findings by Severity

## Critical

### C1. Unsafe object extraction can miscast non-object pointers
- Category: Security, Correctness
- Evidence:
  - `zts/builtins.zig:76`
  - `zts/builtins.zig:77`
  - `zts/builtins.zig:79`
  - `zts/value.zig:131`
  - `zts/value.zig:138`
- Problem:
  - `getObject()` accepts any `isPtr()` value and casts to `*JSObject`.
  - `isPtr()` is generic pointer-tag detection, not an object-type check.
  - Many builtins use this helper (`zts/builtins.zig:1759`, `zts/builtins.zig:1772`, `zts/builtins.zig:1786`, `zts/builtins.zig:1831`, etc.), so non-object pointers (for example strings/rope/slice pointers) can be reinterpreted as objects.
- Recommendation:
  - Replace `if (!val.isPtr())` with `if (!val.isObject())`.
  - Add defensive memtag validation in debug builds before cast.
  - Add regression tests that pass non-object pointers to Array/Object builtins and verify safe behavior.
- Estimated impact:
  - Risk reduction: high (eliminates memory corruption/crash class).
  - Maintainability: medium (consistent value-type discipline across native bindings).

## High

### H1. Static file serving is broken on the active server backend
- Category: Correctness, Architecture
- Evidence:
  - `src/server.zig:2076`
  - `src/server.zig:2082`
  - `src/server.zig:263`
  - `src/server.zig:285`
  - `src/server.zig:486`
  - `src/server.zig:492`
- Problem:
  - `useEventedBackend()` currently hard-returns `false`, so the threaded/sync path is always used.
  - In that path, `serveStaticFileSync()` is a stub that always returns 404.
  - This contradicts CLI/docs expectations (`--static`) and silently disables static serving.
- Recommendation:
  - Implement real sync static serving parity (reuse shared helper logic to avoid a second divergent implementation).
  - Add integration tests covering static success, 404, ETag/304, and traversal rejection on the threaded path.
- Estimated impact:
  - Correctness improvement: very high (restores advertised feature).
  - Maintainability: high (eliminates backend divergence).

### H2. Response status handling uses unchecked integer casts
- Category: Correctness, Security hardening
- Evidence:
  - `zts/http.zig:293`
  - `zts/http.zig:318`
  - `zts/http.zig:340`
  - `zts/http.zig:362`
  - `zts/http.zig:380`
  - `zts/http.zig:425`
  - `src/zruntime.zig:927`
  - `src/zruntime.zig:970`
- Problem:
  - Response helper constructors cast user-provided status directly into `u16` with no range normalization.
  - Runtime response extraction already clamps status to `100..599`; helper path should match this.
  - In safety-on modes this can panic; in safety-off it can produce invalid codes.
- Recommendation:
  - Centralize a `parseHttpStatusOrDefault` helper in `zts/http.zig`.
  - Clamp to valid HTTP range (`100..599`), defaulting to `500` on invalid values.
  - Add tests for negative, zero, >599, and large integer status values.
- Estimated impact:
  - Risk reduction: high (prevents malformed responses/runtime traps).
  - Behavioral consistency: high (aligns helper path with runtime extractor).

### H3. JSON integer writer overflows on `i32` minimum value
- Category: Correctness
- Evidence:
  - `zts/http.zig:906`
  - `zts/http.zig:911`
  - `zts/http.zig:913`
- Problem:
  - `writeInt()` negates `i32` when negative; `-2147483648` cannot be negated in `i32`.
  - This can overflow/trap and break JSON serialization for a valid integer.
- Recommendation:
  - Convert to `i64` (or unsigned magnitude math) before negation and digit emission.
  - Add regression test for `-2147483648` via `valueToJson`.
- Estimated impact:
  - Risk reduction: high for edge-case correctness.
  - Runtime overhead: negligible.

## Medium

### M1. JIT PIC fast path checks only 4 entries while interpreter tracks 8
- Category: Performance
- Evidence:
  - `zts/interpreter.zig:261`
  - `zts/jit/baseline.zig:90`
  - `zts/jit/baseline.zig:4666`
  - `zts/jit/baseline.zig:4703`
- Problem:
  - Interpreter PIC capacity is 8 entries; JIT inline check path hardcodes first 4.
  - 5th-8th polymorphic shapes force helper slow path despite interpreter cache hit potential.
- Recommendation:
  - Either generate checks for all 8 entries, or make `PIC_CHECK_COUNT` configurable and explicitly tied to a benchmarked target.
  - Ensure docs/comments match actual behavior.
- Estimated impact:
  - Throughput gain: medium on polymorphic property-access workloads.
  - Code size increase: low-to-medium depending on emitted checks.

### M2. Runtime request handling has duplicated sync/evented code paths
- Category: Maintainability, Correctness
- Evidence:
  - `src/server.zig:263`
  - `src/server.zig:897`
  - `src/server.zig:1031`
  - `src/server.zig:1101`
- Problem:
  - `handleSingleRequestSync` and `handleSingleRequest` duplicate parsing/dispatch logic.
  - Divergence already occurred (static serving parity issue above).
- Recommendation:
  - Consolidate into shared request-processing core with transport adapters.
  - Keep one parser/dispatcher code path where possible.
- Estimated impact:
  - Maintainability improvement: high.
  - Defect-rate reduction: medium-to-high.

### M3. Hidden-class migration is incomplete; two systems are active in context
- Category: Architecture, Maintainability
- Evidence:
  - `zts/object.zig:918`
  - `zts/object.zig:1296`
  - `zts/context.zig:254`
  - `zts/context.zig:261`
  - `zts/context.zig:778`
  - `zts/context.zig:779`
- Problem:
  - Index-based `HiddenClassPool` exists, but pointer-based legacy `HiddenClass` is still initialized/deinitialized.
  - Mixed model increases cognitive load and deinit risk surface.
- Recommendation:
  - Define a concrete migration checkpoint plan:
    - remove legacy root class initialization from `Context`,
    - switch all remaining pointer-based references,
    - remove dead legacy paths and tests.
  - Gate with perf/compat tests to avoid regression in IC behavior.
- Estimated impact:
  - Maintainability improvement: high.
  - Memory footprint reduction: low-to-medium.

### M4. Thread-local runtime cache is defined but disabled
- Category: Performance
- Evidence:
  - `zts/pool.zig:307`
  - `zts/pool.zig:332`
  - `zts/pool.zig:334`
  - `zts/pool.zig:341`
- Problem:
  - TLS cache infrastructure exists but acquire/release bypasses it due race concerns.
  - This leaves pool contention optimizations partially implemented.
- Recommendation:
  - Either:
    - remove dead TLS cache paths for clarity, or
    - re-enable with correct atomic ownership protocol and stress tests.
- Estimated impact:
  - Throughput gain potential: medium in high-concurrency workloads.
  - Complexity risk: medium (must be validated with race/concurrency tests).

### M5. Path traversal checks do not address symlink escape scenarios
- Category: Security hardening
- Evidence:
  - `src/server.zig:2011`
  - `src/server.zig:2022`
  - `src/server.zig:1298`
  - `src/server.zig:1302`
- Problem:
  - `isPathSafe()` blocks `..`, absolute paths, and drive prefixes, but static file open still follows filesystem links.
  - A symlink within `static_dir` can expose files outside the intended root.
- Recommendation:
  - Resolve/validate against canonical static root (`realpath`) or use directory-fd constrained open strategy where possible.
  - Add tests for symlink traversal attempts.
- Estimated impact:
  - Security hardening: medium.
  - Runtime overhead: low (one-time canonicalization + bounded checks).

## Low

### L1. Comment and implementation drift around PIC sizing and JIT behavior
- Category: Documentation, Maintainability
- Evidence:
  - `zts/interpreter.zig:259`
  - `zts/interpreter.zig:264`
  - `zts/jit/baseline.zig:90`
- Problem:
  - Comments still refer to 4-entry assumptions in some places, while interpreter is 8-entry.
- Recommendation:
  - Align comments/docs to implementation, then update once final PIC policy is chosen.
- Estimated impact:
  - Maintainability improvement: low.

### L2. Documentation drift on backend behavior and defaults
- Category: Documentation
- Evidence:
  - `docs/http-server-architecture.md:14`
  - `src/server.zig:2076`
  - `src/server.zig:2082`
  - `src/server.zig:609`
- Problem:
  - Docs describe evented I/O on non-Apple targets and historical assumptions that no longer match current code state.
  - Some cache/default values in docs diverge from current code defaults.
- Recommendation:
  - Add a “current state” vs “target state” note in architecture/performance docs.
  - Regenerate line references and defaults from source.
- Estimated impact:
  - Maintainer onboarding improvement: low-to-medium.

## Remaining Test Coverage Gaps (Cross-Cutting)

- No end-to-end static file serving test for active threaded path (`serveStaticFileSync` behavior).
- No symlink traversal test for static file serving.

## Phased Implementation Plan

## Phase 1: Critical Fixes (Security + Correctness)

Target: eliminate crash/memory-safety vectors and restore broken runtime behavior.

| Task | Files / Functions | Effort | Dependencies | Success Criteria | Verification |
|---|---|---:|---|---|---|
| Fix unsafe builtins object cast | `zts/builtins.zig:getObject` and all callsites | 0.5-1 day | None | Non-object pointer inputs never reinterpret as `JSObject`; no crashes in builtin calls | `zig build test-zts`; add targeted tests in `zts/builtins.zig` |
| Restore static serving on threaded path | `src/server.zig:serveStaticFileSync`, `handleSingleRequestSync` | 1-2 days | None | `--static` works in current backend mode; parity with async behavior (200/304/404/403) | `zig build test`; add static-serving integration tests |
| Normalize status parsing in response helpers | `zts/http.zig:responseJson/RawJson/Text/Html/Redirect/Constructor` | 0.5 day | None | Invalid statuses map to safe default (or clamped range) consistently | `zig build test-zts`; new status coercion tests |
| Fix `writeInt` min-int overflow | `zts/http.zig:writeInt` | 0.25 day | None | `-2147483648` serializes correctly | `zig build test-zts`; dedicated regression test |

Phase 1 validation commands:
- `zig build test`
- `zig build test-zts`
- `zig build test-zruntime`
- Manual smoke:
  - `zig build run -- --static ./examples`
  - request existing file, missing file, traversal path, and conditional `If-None-Match` flow

## Phase 2: High-Impact Performance Optimizations

Target: improve hot-path throughput without regressing cold start.

| Task | Files / Functions | Effort | Dependencies | Success Criteria | Verification |
|---|---|---:|---|---|---|
| Harmonize JIT PIC check depth with interpreter PIC policy | `zts/jit/baseline.zig`, `zts/interpreter.zig` | 1-2 days | Phase 1 complete recommended | Reduced helper fallback rate for polymorphic property access; no JIT correctness regressions | `zig build test-zts`; `zig build bench`; compare PIC hit/miss and p95 latency |
| Re-enable or remove TLS cache path decisively | `zts/pool.zig`, `src/zruntime.zig` | 2-4 days | Phase 1 complete recommended | If enabled: no hangs/races under stress; measurable contention reduction. If removed: simpler, dead-code-free pool path | `zig build test-zts`; stress loop with concurrent requests; bench before/after |
| Reduce request-path duplication cost in parser/dispatcher | `src/server.zig` shared parse/dispatch core | 2-3 days | Phase 1 static fix | One logic path for static/handler dispatch to prevent divergence | `zig build test`; throughput comparison via `zig build bench` |

Phase 2 validation commands:
- `zig build test`
- `zig build test-zts`
- `zig build test-zruntime`
- `zig build bench` (track p50/p95, RPS, PIC metrics)

## Phase 3: Code Quality and Maintainability

Target: reduce long-term technical debt and improve auditability.

| Task | Files / Functions | Effort | Dependencies | Success Criteria | Verification |
|---|---|---:|---|---|---|
| Remove hidden-class dual-path ambiguity | `zts/object.zig`, `zts/context.zig`, related IC code | 3-5 days | Phase 1 done; coordinate with perf tests | Single hidden-class model in active runtime path | `zig build test-zts`; property access microbench and hidden-class transition tests |
| Break up large orchestration functions | `src/zruntime.zig:executeHandlerInternal`, `src/server.zig` request handlers | 1-2 days | None | Functions split by responsibility (dispatch/AOT/extract/send) with unchanged API behavior | `zig build test`; `zig build test-zruntime` |
| Expand regression coverage for discovered edge cases | `src/server.zig`, `zts/http.zig`, `zts/builtins.zig` | 1-2 days | Phase 1 fixes | New tests prevent recurrence of C1/H1/H2/H3 issues | `zig build test*` suite |
| Update docs to reflect current behavior | `docs/architecture.md`, `docs/http-server-architecture.md`, `docs/performance.md`, `docs/api-reference.md` | 0.5-1 day | After code changes | Docs reflect actual backend mode/defaults and verified capabilities | Manual doc review + command examples sanity check |

Phase 3 validation commands:
- `zig build test`
- `zig build test-zts`
- `zig build test-zruntime`
- manual verification of CLI examples in docs

## Phase 4: Nice-to-Have Enhancements

Target: incremental hardening and polish after critical/perf debt is retired.

| Task | Files / Functions | Effort | Dependencies | Success Criteria | Verification |
|---|---|---:|---|---|---|
| Harden static root confinement against symlink escapes | `src/server.zig` static open logic | 1-2 days | Phase 1 static fix | Access constrained to static root under symlink tricks | Added security tests + manual symlink checks |
| Add structured benchmark gates to CI | `build.zig`, benchmark scripts/docs | 1-2 days | Phase 2 metrics baseline | Regressions in RPS/latency surfaced automatically | `zig build bench` in CI mode, threshold checks |
| Improve observability for pool/JIT internals | `src/zruntime.zig`, `zts/interpreter.zig` | 1 day | Phase 2 | Easy diagnosis of PIC misses/pool exhaustion/backoff behavior | Metrics log checks under load |

Phase 4 validation commands:
- `zig build test`
- `zig build test-zts`
- `zig build test-zruntime`
- `zig build bench`

## Notes on Constraints

- API compatibility preserved in recommendations:
  - handler signature unchanged
  - `Response.*` helper surface unchanged
  - CLI flags unchanged
- Example compatibility maintained:
  - recommendations do not break `examples/` handler model
- Cold start target compatibility:
  - no recommendation conflicts with embedded-bytecode strategy in `docs/cold-start-optimization.md`
  - major changes prioritize warm-path correctness/perf while keeping startup path simple
