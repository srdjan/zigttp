# zigttp Fresh-Eyes Implementation Plan

Refreshed on 2026-06-18 against HEAD `aa14d66`.

This plan is the current backlog after a whole-repo fresh-eyes pass. The
previous checklist was reconciled against source first; items already landed are
not carried forward as active work.

## Audit Method

- Five read-only subsystem agents reviewed engine/compiler/JIT, runtime/proof,
  modules/SDK, PI/tools, and docs/release/CI.
- Findings were accepted only when the main pass could verify them against live
  source or a local command.
- CodeGraph is not useful for this Zig-heavy repository yet, so source reads and
  build gates are the primary evidence.
- Changes that alter runtime behavior, CLI contracts, proof semantics, module
  semantics, or JavaScript-visible behavior require an explicit implementation
  decision before patching.

## Implemented In This Pass

- CI and release workflows no longer run the aggregate `test` root and
  `test-zruntime` in the same Zig build invocation. `build.zig` documents that
  duplicate pool-heavy roots have caused intermittent macOS teardown traps, and
  the same combined shape reproduced a local segfault once during this pass.
- The proof-gate workflow now fails when `zigttp proofs gate` exits with an
  infrastructure/usage error or produces no verdict, after still posting the PR
  comment.
- The release workflow now runs `test-panic-isolation` and checks the tag
  version against both `build.zig.zon` and `packages/zigts/src/root.zig`.
- The docs link gate now validates local Markdown anchors for README/docs and
  `SECURITY.md`.
- Router examples now use the current `routerMatch(routes, req)` API.
- Witness docs and PI/tool comments no longer advertise the unsupported
  `zigttp mock --replay` command.

Exit gate:

- `zig build test-docs-drift test-doc-links`
- `zig build test-cli`
- `git diff --check`

## Landed Before This Pass

The prior plan's active items are now source-confirmed as landed or superseded:

- PI session IDs and cassette sidecars are validated as safe path segments.
- Module capability governance, helper coverage, and bounded `fetchWithRetry`
  options are in place.
- Analyzer/tool guardrails cover bounded stdin JSON, duplicate diagnostics,
  SQL-schema JSON errors, and proof-behavior schema threading.
- Runtime hardening covers edge timeouts, access logs, cooperative shutdown
  docs, deadline-aware JIT inhibition, and panic-isolation smoke coverage.
- Engine correctness work covers UTF-16 `charCodeAt`, empty-array `reduce`,
  object enumeration order, sparse-array syntax rejection, strict JSON parsing,
  and negative zero cases.

## Phase 1 - Workflow And Release Gates

Goal: make automation fail for real infrastructure failures without reintroducing
duplicate-root instability.

1. Keep `zig build test` and `zig build test-zruntime` as separate workflow
   commands.
2. Keep `test-doc-links` separate from aggregate `test` unless it is added to
   the aggregate root intentionally.
3. Preserve proof-gate comment posting, then fail on verdict `error`, missing
   verdict, or non-overridden `breaking`.
4. Keep release tag, package version, and binary version aligned before
   packaging.

Exit gate:

- `zig build test`
- `zig build test-zruntime`
- `zig build test-docs-drift test-doc-links`
- `zig build test-panic-isolation`

## Phase 2 - Documentation And Example Truthfulness

Goal: make advertised examples and links executable or clearly scoped.

1. Extend docs drift checks to compile/check selected documentation snippets,
   starting with virtual-module examples.
2. Audit advertised advanced examples in `examples/README.md`; either add them
   to the example gate or mark them as non-gated sketches.
3. Fix stale advanced examples identified in this pass:
   `examples/routing/api-surface.ts`,
   `examples/parallel/parallel-simple.ts`,
   `examples/parallel/parallel.ts`, and `examples/system/users.ts`.
4. Keep witness-corpus docs aligned with the actual replay surface.

Exit gate:

- `bash scripts/test-examples.sh`
- New focused snippet/example checks from this phase

## Phase 3 - Runtime And Proof Behavior

Goal: fix confirmed runtime/proof correctness issues after approving the visible
behavior changes.

1. Make `HandlerPool.acquireForRequest` wait until elapsed
   `poolWaitTimeoutMs` rather than failing early due to retry-count exhaustion.
2. Populate proof-card/ledger intent and behavior-path summaries in
   `factsFromContract`.
3. Replace placeholder-style public-path server tests with actual accept-path
   coverage, or rename them so the gate does not overclaim.

Exit gate:

- `zig build test-server test-zruntime test-panic-isolation test-proof-review`

## Phase 4 - Engine Semantics

Goal: fix JavaScript-visible supported-subset bugs deliberately, with focused
regressions and no silent surface expansion.

1. Make array callback builtins skip holes where JavaScript requires present
   elements.
2. Fail closed or dynamically store oversized type-expression members instead
   of silently truncating unions, intersections, records, tuples, and generic
   arguments.
3. Bring string `length`, `charAt`, `slice`, and `substring` under UTF-16
   code-unit semantics.
4. Decode `\uXXXX` surrogate pairs consistently in parser strings and JSON,
   rejecting invalid JSON surrogate forms.

Exit gate:

- `zig build test-zigts`
- Focused sparse-array, type-expression, Unicode string, and JSON escape tests

## Phase 5 - Modules, SDK, And Input Validation

Goal: align virtual-module docs, metadata, and validators with implementation
truth.

1. Fix `zigttp:io` return metadata for `parallel` and `race`, including SDK
   `ReturnKind` support if needed.
2. Give `fetchWithRetry` the same structured response type as `fetch`.
3. Tighten `zigttp:validate` ISO date/datetime validation beyond shape checks.
4. Tighten `zigttp:time` `parseIso` timezone parsing so malformed suffixes fail.

Exit gate:

- `zig build test-modules test-module-governance test-capability-audit test-sdk`

## Phase 6 - PI And Analyzer Contracts

Goal: keep machine-facing analyzer and expert tooling explicit.

1. Decide whether `ZIGTTP_CASSETTE_MODE=replay` should prevent live provider
   network calls even when API keys are present; if yes, route provider
   selection through cassette replay and add a fake-key no-socket test.
2. Reject unknown flags for stable analyzer commands such as `meta`,
   `features`, and `modules`.

Exit gate:

- `zig build test-cli test-expert test-expert-app test-cassette test-expert-golden test-proof-review`

## Deferred Direction Work

Keep VM loop semantic dedupe in `DEFERRED_VM_LOOP_DEDUPE_PLAN.md`. Do not fold
it into this plan until the current correctness gates are green.

## Verification Menu

- Aggregate: `zig build test`
- Runtime: `zig build test-zruntime test-server test-panic-isolation`
- Engine: `zig build test-zigts`
- Modules/governance: `zig build test-modules test-module-governance test-capability-audit test-sdk`
- Analyzer/tools: `zig build test-cli test-expert test-expert-app test-cassette test-expert-golden test-proof-review`
- Docs: `zig build test-docs-drift test-doc-links`
- Examples: `bash scripts/test-examples.sh`
- Release-adjacent: `zig build -Doptimize=ReleaseFast`
