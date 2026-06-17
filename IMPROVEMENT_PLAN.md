# zigttp Fresh-Eyes Implementation Plan

Refreshed on 2026-06-18 against HEAD `c2141bf`.

The worktree already contained a user-owned modification in
`packages/zigts/src/jit/optimized.zig` before this implementation pass. Preserve
that change unless a JIT-specific phase explicitly re-opens and owns it.

This is the current implementation checklist for the confirmed fresh-eyes
findings. Keep each phase small and reviewable. Before starting a phase, re-open
the cited source and drop the item if it no longer matches current code.

## Shared Decisions

- Safety fixes come first; broad architecture cleanup waits for verification
  gates.
- PI session IDs and cassette sidecars are safe path segments, not arbitrary
  relative paths.
- Module binding metadata in Zig is authoritative; specs and docs must match the
  binding table.
- `review-patch --diff-only` compares diagnostic multisets, including duplicate
  occurrences.
- `fetchWithRetry` stays conservative: reject more than 10 retries, base delay
  over 5000 ms, or max delay over 30000 ms.
- Edge `timeoutMs` must cover handler execution, matching public docs.
- Access logs, when enabled, contain method, path, status, duration, and request
  id only.
- `Server.shutdown` is a same-thread/cooperative API unless a later phase adds
  synchronization and tests.
- Sparse array elisions are unsupported syntax and should be rejected, not
  modeled as JavaScript holes.
- JSON parsing must validate the whole input and reject invalid escapes, raw
  control bytes, invalid numbers, and trailing garbage.
- Performance and binary-size direction work is limited to measurement gates and
  documented boundaries.

## Completed Before This Pass

- `String.prototype.charCodeAt` returns UTF-16 code units.
- `Array.prototype.reduce` handles empty arrays correctly.
- Phase 0 documentation drift for virtual modules, capabilities, multipart
  support, roadmap wording, and example coverage was repaired.
- `scripts/check-docs-drift.sh` checks documented module exports and
  capabilities against module specs.
- Panic-isolation verification is wired through `zig build test-panic-isolation`
  and CI.

## Phase 1 - PI Path Containment

Goal: keep replay/session paths inside the intended repository-owned roots.

1. Validate session IDs before joining them into
   `$ROOT/<cwd_hash>/<session_id>`.
2. Skip invalid session directory names during session listing.
3. Validate cassette `sse_path` as a sibling basename before reading a sidecar.

Exit gate:

- `zig build test-cassette`
- `zig build test-expert-app`

## Phase 2 - Module Metadata And Capability Governance

Goal: make virtual-module policy metadata machine-checkable and aligned.

1. Add explicit `.effect = .none` metadata to pure `ModuleBinding` helpers where
   the default `.read` effect is inaccurate.
2. Align module specs to the binding table, especially SQL read effects and
   helpers with callback/runtime behavior.
3. Extend `module_audit` to compare exported function effects, return shapes,
   and all capability flags represented in specs.
4. Extend `scripts/check-capability-helpers.sh` for websocket and SQL-write
   helper coverage.
5. Bound and document `fetchWithRetry` retry/delay options.

Exit gate:

- `zig build test-modules test-module-governance test-capability-audit`
- Focused tests for `fetchWithRetry`

## Phase 3 - Analyzer And Machine-Contract Guardrails

Goal: keep CLI/analyzer workflows bounded and truthful for automation.

1. Cap stdin JSON accepted by `edit-simulate` and `review-patch`.
2. Compare duplicate diagnostics as multisets in `review-patch --diff-only`.
3. Return structured JSON when `zigts check --json` needs an SQL schema.
4. Thread discovered SQL schema paths through `review-patch` and
   `prove-behavior`.
5. Correct `prove-behavior` receipt wording unless this pass also implements
   signed receipts.

Exit gate:

- `zig build test-cli`
- `zig build test-expert-golden`

## Phase 4 - Runtime And Edge Hardening

Goal: make documented lifecycle behavior true or narrow the contract.

1. Wire edge `timeoutMs` into handler request deadlines.
2. Implement structured access logs behind `log_requests`.
3. Close queued keep-alive connection file descriptors during pool deinit.
4. Narrow the `Server.shutdown` doc contract to cooperative same-thread use.
5. Add a hot-loop deadline regression. If compiled loops outrun deadlines,
   inhibit JIT while request deadlines are active.

Exit gate:

- `zig build test-server test-zruntime`
- Focused edge/runtime timeout tests

## Phase 5 - Supported-Subset Engine Correctness

Goal: fix observable bugs in the currently supported JavaScript subset without
expanding the language surface.

1. Enumerate array-index own properties before other strings in
   `Object.keys`, `Object.values`, and `Object.entries`.
2. Reject sparse array elisions during parse/check.
3. Preserve negative zero in `Math.sign(-0)` and `JSON.parse("-0")`.
4. Make JSON parsing strict for trailing garbage, control bytes, escapes, and
   number grammar.

Exit gate:

- `zig build test-zigts`
- Focused filters for Object enumeration, sparse arrays, negative zero, and
  JSON parsing

## Phase 6 - Direction Gates Only

Goal: prevent broad optimization or architecture movement without receipts.

1. Keep architecture cleanup out of this pass unless required by a verified bug.
2. Add or update measurement receipts before changing performance-sensitive
   paths.
3. Use `bench-check`, binary-size receipts, and runtime smoke checks as gates
   before optimization claims become blocking.

Exit gate:

- `zig build bench-check` when performance-adjacent code changes
- `zig build test` before declaring the implementation pass complete

## Verification Menu

- PI: `zig build test-cassette test-expert-app`
- Modules/governance: `zig build test-modules test-module-governance test-capability-audit`
- Analyzer tools: `zig build test-cli test-expert-golden`
- Runtime: `zig build test-server test-zruntime`
- Engine/parser/JIT: `zig build test-zigts`
- Docs: `zig build test-docs-drift test-doc-links`
- Examples: `bash scripts/test-examples.sh`
- Aggregate: `zig build test`
