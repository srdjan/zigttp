---
title: Close Workflow & Fault-Tolerance Gaps - Plan
type: feat
topic: workflow-fault-tolerance-gaps
artifact_contract: ce-unified-plan/v1
artifact_readiness: implementation-ready
product_contract_source: ce-plan-bootstrap
execution: code
date: 2026-07-02
origin: docs/plans/2026-07-02-001-decision-workflow-fault-tolerance-gaps.md
---

# Close Workflow & Fault-Tolerance Gaps - Plan

## Goal Capsule

Close every gap identified in
`docs/plans/2026-07-02-001-decision-workflow-fault-tolerance-gaps.md` so the
workflow/fault-tolerance surface landed by the 2026-06-30 plan has no known
silent-failure or misleading-semantics hole left before further work builds
on top of it.

Execution profile:

- **Depth**: Deep.
- **Mode**: Implementation-ready code plan.
- **Primary surface**: durable-run crash recovery, the analyzer's
  effect/diagnostic pipeline, `system_linker`'s cross-handler proof phases,
  workflow examples and docs.
- **Stop conditions**: do not weaken proof soundness, do not break persisted
  replay names or oplog formats, no automatic destructive retention, do not
  attempt to lift the existing saga vs. `--workflow-queue` rejection, do not
  expand `fanout()`/`race()` into genuine concurrency in this pass
  (deferred — see Scope Boundaries).
- **Tail ownership**: the final phase closes examples and docs so nothing
  from this plan ships undocumented.

## Product Contract

### Summary

This plan formalizes the three ranked gaps and six minor findings from the
2026-07-02 decision record into concrete requirements and implementation
units, sequenced by risk: documentation/consistency fixes first, the
self-contained analyzer diagnostic second, the crash-recovery-touching
dead-letter work third, the new saga proof surface fourth, and
affordance-proof-coverage plus docs close-out last.

### Problem Frame

Three user-confirmed design forks resolved scope before planning began:

- Durable-run dead-letter parity gets its **own `durable dead-runs` CLI
  namespace**, not a merge with `workflow-queue`'s dead-letter surface.
- `workflow.call`/`saga`/`fanout`/`follow` nested inside `step()` gets a
  **hard compile-time rejection**, not new runtime support for nested
  durability.
- The saga compensation-exhaustiveness proof covers **static do/undo sets
  only**; dynamically-constructed sagas fall back to unproven, matching the
  existing `intent.dynamic = true` escape hatch.

### Requirements

- **R1 - Fanout/race semantics are documented accurately**: `fanout()`'s
  sequential-but-order-deterministic dispatch and `race()`'s
  first-in-declaration-order (not lowest-latency) selection must be stated
  explicitly in user-facing docs, not just internal code comments.
- **R2 - Workflow-queue lease reclaim uses bounded jitter**: reclaim retries
  must back off the same way durable-run recovery already does, instead of
  retrying with no delay.
- **R3 - Nested workflow calls fail closed**: `workflow.call`, `saga`,
  `fanout`, and `follow` used inside a `durable.step()` callback must fail
  the build with a diagnostic instead of silently downgrading durability.
- **R4 - Durable-run failures are inspectable**: a durable run that
  permanently fails recovery must be persisted to an inspectable dead-run
  record with `list`/`replay`/`discard` operations, mirroring
  `workflow_queue`'s existing dead-letter API, without mutating the
  original oplog.
- **R5 - Saga compensation coverage is proven where decidable**: a
  statically-constructed `saga([...])` array must be proven at compile time
  to have a `compensate` on every step except possibly the last; a
  dynamically-constructed saga is marked unproven, never guessed.
- **R6 - HATEOAS affordance links get the same proof coverage as ordinary
  links**: payload-compatibility, cross-boundary/injection, and
  failure-cascade/retry-safety checks must also run over `affordance_links`,
  not only `links`.
- **R7 - Examples and docs stay current**: `examples/workflow/` gains
  coverage for `scope`/`using`/`ensure`, `signalAt`, queued `fanout`, and a
  saga-compensation-failure path; docs describe every new CLI verb,
  diagnostic, and contract property this plan adds.

---

## Planning Contract

### Product Contract Preservation

This plan carries forward Options A, B, and C from
`docs/plans/2026-07-02-001-decision-workflow-fault-tolerance-gaps.md`
without narrowing them, plus the six minor findings that decision record
left unscored. The three user-confirmed scope forks above are additions,
not conflicts, with that record.

### Key Technical Decisions

- **KTD1 - Dead-run record is the first durable representation of
  quarantine state**: `durable_recovery.zig`'s `RetryTracker` is in-memory
  only (`std.StringHashMapUnmanaged`), so today a process restart silently
  resets a run's failure count and re-attempts it. Writing a persisted
  record at the moment `quarantine_threshold` (10) is crossed makes
  quarantine durable across restarts for the first time: the recovery
  poller must check for an existing dead-run record before attempting a
  run, not rely on the in-memory tracker alone.
- **KTD2 - Dead-run records live alongside, not inside, the oplog**: new
  records go in a sibling `dead-runs/` directory keyed by the oplog
  filename, mirroring `workflow_queue`'s `<durable_dir>/workflow-queue/dead/`
  layout. The oplog file itself is never renamed, mutated, or deleted by
  this plan — required by the hypothesis canvas's kill criterion and the
  2026-06-30 plan's replay-compatibility stop condition.
- **KTD3 - New small module, not a bigger existing file**: dead-run
  persistence gets its own `durable_dead_runs.zig` (record shape, atomic
  write, list/read/replay/discard) and `durable_dead_runs_cli.zig` (CLI
  verbs), mirroring how `workflow_queue.zig` and `workflow_queue_cli.zig`
  are already split, rather than growing `durable_recovery.zig` further.
- **KTD4 - Diagnostic codes**: the highest ZTS5xx code in use is ZTS508.
  The new call-in-step diagnostic takes **ZTS509**; the new saga
  compensation diagnostic takes **ZTS510**.
- **KTD5 - Call-in-step detection reuses existing state**:
  `effect_inference.zig` already tracks `durable_callback_depth` to know
  when the walk is inside a `durable.step()` callback (used today only to
  suppress a non-determinism check). ZTS509 reuses that counter — no new
  "inside step()" tracking is needed.
- **KTD6 - Saga proof scope is structural, not effect-level**: the proof
  checks only whether a `compensate` key is *present* on every non-last
  static step, not whether an existing `compensate` correctly undoes its
  `run`. Full side-effect analysis of step bodies is out of scope for this
  pass (see Scope Boundaries). The "last step may omit `compensate`" rule
  matches the existing `saga-orchestrator.ts` example (`ship` is last and
  has no compensate, by design) and avoids false positives against that
  legitimate pattern.

### High-Level Technical Design

```mermaid
flowchart TD
  subgraph Phase1[Phase 1: Consistency and Docs]
    U1[U1 Fanout/race doc accuracy]
    U2[U2 Queue lease-reclaim backoff]
  end
  subgraph Phase2[Phase 2: Call-in-step Diagnostic]
    U3[U3 ZTS509 rule and detection]
    U4[U4 ZTS509 tests]
  end
  subgraph Phase3[Phase 3: Durable-Run Dead-Letter Parity]
    U5[U5 Dead-run record and quarantine hook]
    U6[U6 replayDeadRun and startup skip]
    U7[U7 durable dead-runs CLI]
    U8[U8 Crash-recovery tests]
  end
  subgraph Phase4[Phase 4: Saga Compensation Proof]
    U9[U9 saga_extractor.zig]
    U10[U10 ZTS510 rule in system_linker]
    U11[U11 Contract property]
    U12[U12 Saga proof tests]
  end
  subgraph Phase5[Phase 5: Affordance Coverage and Close-Out]
    U13[U13 Extend proof phases to affordance_links]
    U14[U14 Examples and docs]
  end
  Phase1 --> Phase2 --> Phase3 --> Phase4 --> Phase5
  U5 --> U6 --> U7
  U9 --> U10 --> U11
```

Phases are sequenced by risk, not by strict code dependency: Phase 1 and
Phase 2 have no dependency on Phase 3 or 4 and could run in either order,
but documentation/consistency fixes are cheapest to land first and the
self-contained analyzer diagnostic (Phase 2) is lower-risk than the
crash-recovery-touching work (Phase 3). Phase 4 depends on nothing from
Phase 3 but is sequenced after it because it is the largest, most novel
unit of work. Phase 5's U13 is independent of Phases 1-4; U14 documents the
cumulative result of all prior phases, so it must go last.

### System-Wide Impact

- `durable_recovery.zig` gains a new persisted side channel (dead-run
  records) alongside its existing in-memory `RetryTracker`.
- The analyzer's `effect_inference.zig` / `rule_registry.zig` /
  `contract_types.zig` gain two new diagnostics (ZTS509, ZTS510) and a new
  `SagaInfo` contract field.
- `system_linker.zig`'s proof phases (payload, cross-boundary, failure
  cascade) extend to a second link list (`affordance_links`).
- `contract.json` gains a new saga compensation-coverage property,
  following the existing `durable.workflow.properties` convention.
- The CLI gains a new `durable dead-runs` command group.
- `examples/workflow/` and `docs/durable-workflows.md` gain new coverage;
  existing docs get corrected concurrency-semantics language for
  `fanout()`/`race()`.

### Risks

- **Crash-recovery fragility**: `durable_recovery.zig` has a documented
  prior double-free bug that was invisible under `test-zruntime`'s
  arena-wrapped allocator and only caught under `test-cli`'s fresh-GPA
  runner. Phase 3 tests must run under the latter.
- **False proof positives (saga)**: an overly aggressive static check could
  flag legitimate patterns (e.g., a trailing no-compensate step). Mitigated
  by KTD6's explicit last-step exception and a negative test matching the
  existing `saga-orchestrator.ts` example.
- **Restart semantics change**: KTD1 means a restarted process no longer
  silently forgets quarantine state. This is an intentional behavior
  improvement, but it changes observable behavior after a crash and should
  be called out in docs (U14), not just shipped silently.
- **Diagnostic false positives (ZTS509)**: the check must not fire for
  top-level `call()`/`saga()`/`fanout()`/`follow()` usage outside any
  `step()`, nor for `workflow.call` nested inside an unrelated, non-durable
  helper function. Covered by U4's negative test cases.

---

## Implementation Units

### Phase 1: Consistency and Documentation Fixes

#### U1 - Accurate fanout()/race() concurrency semantics

**Goal**

Eliminate the naming/behavior mismatch by documenting what `fanout()` and
`race()` actually do, and cross-reference the two mismatched concurrency
caps, without changing runtime behavior in this unit.

**Requirements**

- Covers R1.

**Files**

- `packages/runtime/src/runtime_workflow.zig`
- `packages/zigts/src/modules/workflow/io.zig`
- `docs/durable-workflows.md`

**Approach**

- Expand the existing internal comments at `runtime_workflow.zig:835`
  (`MAX_PARALLEL_CALLS = 16`, sequential dispatch) and `io.zig:16`/`:27`
  (`MAX_PARALLEL = 8`, `race()` join-then-scan) into doc comments that state
  the user-visible behavior plainly: `fanout()` dispatches sequentially and
  is order-deterministic by declaration index, not concurrent; `race()`
  returns the first success in declaration order, not the lowest-latency
  response.
- Add a one-line cross-reference in each constant's comment noting the
  other constant and why they differ: `MAX_PARALLEL_CALLS` bounds a
  sequential dispatch loop (no concurrent OS threads), `MAX_PARALLEL` bounds
  actual concurrent fetches — they are not required to match.
- Add the same plain-language behavior notes to `docs/durable-workflows.md`
  wherever `fanout`/`race` are documented, with a pointer to the Scope
  Boundaries section below for the deferred concurrency work.

**Tests**

- Test expectation: none — documentation and comment changes only, no
  behavior change.

#### U2 - Workflow-queue reclaim jitter (scope corrected during implementation)

> **Amendment**: this unit's original framing — "lease reclaim retries
> immediately with no delay/backoff" — turned out to be inaccurate. Tracing
> the full path before writing code showed `handleLeasedFile`'s `.busy`
> result is already durably persisted as a `wait_timer` event
> (`suspendQueuedWorkflowDispatch` in `runtime_workflow.zig`) and cheaply
> gated on each 1-second `DurableScheduler` tick
> (`durable_recovery.zig:186-187`) — there is no tight retry loop on the
> reachable path. The real, smaller gap: the computed retry time is a
> deterministic `lease_until_ms` with no jitter, so items whose leases
> expire in the same second all become re-eligible in the same tick. The
> goal, files, and approach below reflect what was actually implemented.

**Goal**

Add bounded jitter to the workflow-queue reclaim retry time so items whose
leases expire around the same wall-clock moment don't all become
re-eligible in the same scheduler tick, without ever retrying before the
lease has actually expired.

**Requirements**

- Covers R2.

**Files**

- `packages/runtime/src/runtime_workflow.zig` (not `workflow_queue.zig` —
  the retry-time computation lives in the caller,
  `workflowQueuedDispatchParts`'s `.busy` branch, not in
  `handleLeasedFile`)
- `packages/runtime/src/retry_backoff.zig` (reused unchanged)

**Approach**

- Added a `jitteredRetryAtMs(base_retry_ms, item_id)` helper that adds
  `retry_backoff.boundedJitterMs(WORKFLOW_QUEUE_RETRY_JITTER_CAP_MS, seed)`
  on top of the existing `base_retry_ms` computation, seeded via
  `retry_backoff.seedBytes(item_id, ...)` so different items jitter
  differently. Jitter is strictly additive, so the result is never earlier
  than `base_retry_ms`/`lease_until_ms`.
- Used `boundedJitterMs` directly (not the full exponential
  `retryDelayMs`) since there is no failure/attempt count here — just a
  fixed lease deadline that needs spreading out, not exponential growth.
- `WORKFLOW_QUEUE_RETRY_JITTER_CAP_MS` is a small, separate constant
  (1000ms) from `WORKFLOW_QUEUE_RETRY_DELAY_MS` — it only needs to spread
  items apart, not delay them meaningfully.

**Tests**

- `jitteredRetryAtMs never retries before base_retry_ms`
- `jitteredRetryAtMs stays within the bounded jitter ceiling`
- `jitteredRetryAtMs spreads distinct item ids apart`

All three colocated with `runtime_workflow.zig`'s existing test blocks;
full `test-zruntime` suite (392 tests) passes with zero failures.

---

### Phase 2: Compile-Time Rejection of Nested Workflow Calls

#### U3 - ZTS509 rule definition and detection (construction site amended)

> **Amendment**: the plan called for `spec_discharge.zig`'s existing
> two-pass pipeline (an `EffectRow` flag consumed by a second pass) to
> construct the diagnostic. Reading that pipeline before writing code
> showed it is specifically the *opt-in Effects<...> ceiling diff*
> mechanism: `dischargeEffects` explicitly skips any function with no
> `Effects<...>` annotation ("no declared ceiling, so no check").
> `workflow.call`/`saga`/`fanout`/`follow` nested in `step()` must be
> flagged unconditionally, regardless of whether the function carries any
> capsule annotation — routing it through the opt-in pipeline would have
> silently limited the check to annotated functions only. Construction was
> implemented in `contract_builder.zig` instead, alongside the (also
> unconditional) `dischargeCapabilityBudget` handler-budget check, as a new
> "Phase 4d". Detection itself landed exactly as planned, reusing
> `effect_inference.zig`'s existing `durable_callback_depth` counter.

**Goal**

Fail the build when `workflow.call`, `saga`, `fanout`, or `follow` is used
inside a `durable.step()` callback, instead of silently losing durability
at runtime.

**Requirements**

- Covers R3.

**Files**

- `packages/zigts/src/rule_registry.zig`
- `packages/zigts/src/contract_types.zig`
- `packages/zigts/src/effect_inference.zig`
- `packages/zigts/src/contract_builder.zig` (not `spec_discharge.zig` — see
  Amendment above)
- `packages/tools/src/precompile_check.zig`,
  `packages/proof-review/src/spec_diagnostic.zig` (exhaustive switches over
  `SpecDiagnostic.Kind` that needed a new arm; caught immediately by the
  compiler, not discovered by inspection)

**Approach**

- Added a `SpecDiagnostic.Kind.workflow_call_in_step` variant
  (`contract_types.zig`, code `ZTS509`) and a matching `capsule_meta` entry
  (`rule_registry.zig`, `.category = .verifier` like every other ZTS5xx/
  ZTS6xx rule) — description, example, help pointing at moving the call
  outside `step()`, no automatic `repair` (restructuring requires developer
  intent).
- `effect_inference.Analyzer` gained a new unconditional collection field,
  `nested_workflow_calls: std.ArrayListUnmanaged(NestedWorkflowCall)`
  (`owner` function index + borrowed `workflow_fn` name), populated in
  `handleCall` right where the existing import-resolution branch already
  checks `imported.module`/`imported.name` — when
  `durable_callback_depth > 0` and the callee is `zigttp:workflow`'s
  `call`/`saga`/`fanout`/`follow`, append a violation. No new "inside
  step()" tracking was needed, as planned.
- `contract_builder.zig`'s `build()` gained a new unconditional "Phase 4d"
  (`emitNestedWorkflowCallDiagnostics`) right after the existing budget
  discharge phase, walking `effects.nested_workflow_calls` and appending a
  ZTS509 `SpecDiagnostic` per violation — unconditionally, for every
  function the analyzer collected, matching the severity default (`.err`).

**Tests**

- `contract_builder.zig`: `"ZTS509 fires for call/saga/fanout/follow nested
  inside step()"` (table-driven over all four exports), `"ZTS509 does not
  fire for top-level workflow.call"`, `"ZTS509 fires through an inline
  closure nested in step()"`, `"ZTS509 does not fire for workflow.call in a
  function never reached from step()"`.
- Zero `ZTS509` hits across every `examples/workflow/*.ts` file, confirmed
  via `zigttp check --json` (no false positives on legitimate top-level
  usage).

#### U4 - ZTS509 negative/positive test matrix (scope corrected)

> **Amendment**: the planned "nested two levels deep inside a `step()`
> whose callback itself contains a nested non-durable helper" scenario
> turned out not to be a real boundary of this check — it's a boundary of
> the *existing* `durable_callback_depth` mechanism ZTS509 reuses. A call
> to a **named** local/helper function (`const wrapped = () => call(...);
> wrapped()`, or a separately-declared function) resolves as neither an
> import nor a tracked top-level function at that call site, so its body is
> never walked with the caller's depth — the exact same blind spot the
> pre-existing non-determinism suppression check
> (`effect_inference.zig`'s sibling `durable_callback_depth == 0` check)
> already has. Extending depth-tracking across named-function calls would
> be interprocedural analysis, well beyond this unit's S-M scope, and
> would make ZTS509 inconsistently stricter than the mechanism it reuses.
> The test matrix instead asserts the boundary that genuinely holds:
> depth survives nesting through **inline anonymous closures** (e.g. a
> `.map()` callback), which effect_inference.zig's walk does inline by
> design ("anonymous closures inherit their enclosing function's effect
> row").

**Goal**

Lock in the exact boundary of the new rule so future analyzer changes
cannot silently widen or narrow it.

**Requirements**

- Covers R3.

**Dependencies**

- U3.

**Files**

- Test blocks colocated with `contract_builder.zig` (not
  `effect_inference.zig`/`spec_discharge.zig` as originally planned — see
  U3's amendment on where construction actually landed; testing at the
  `HandlerContract` level via the existing `buildTestContract` helper
  exercises detection, construction, and the diagnostic list together).

**Approach**

- Table-driven test covering: each of the four nested-call forms fails;
  top-level usage passes; nesting through an inline closure (`.map()`
  callback) still fails; a `workflow.call` inside a function never reached
  from a `step()` callback does not fire.

**Tests**

- See U3's Tests list — the two units share one test group in
  `contract_builder.zig`.

---

### Phase 3: Durable-Run Dead-Letter Parity

#### U5 - Dead-run record persistence and quarantine hook

> **Amendment (spans U5-U8)**: three real corrections surfaced during
> implementation, all kept since they make the feature's core promise
> (surviving a restart, and surviving a separate-process `replay`) actually
> true rather than assumed:
>
> 1. **The gate logic must check the persisted record unconditionally, not
>    only when the in-memory tracker already suspects quarantine.** The
>    first implementation only re-checked `hasDeadRun` when
>    `RetryTracker.isQuarantined` was already true — which is never true
>    right after a restart (the map is empty), so it silently skipped the
>    exact scenario (KTD1) this feature exists to fix. Fixed by checking
>    `hasDeadRun` first, every poll, for every oplog — one cheap `access()`
>    syscall on the common (no-record) case, not a read.
> 2. **`discardDeadRun` rewrites the record (`state: "discarded"`) instead
>    of deleting it.** The plan's original text ("deletes the persisted
>    record only") would have made a discarded run eligible for retry again
>    on the very next poll (no record → gate lets it through), contradicting
>    the plan's own Definition of Done ("discard... run stays permanently
>    unretried"). The record now stays on disk permanently (still
>    inspectable via `show`, excluded from `list`), and `replay` on an
>    already-discarded record fails closed (`error.DeadRunAlreadyDiscarded`)
>    rather than silently un-discarding it.
> 3. **`replayDeadRun` cannot call `tracker.clear()` directly** — it has no
>    reference to a live `RetryTracker`, and in the real cross-process case
>    (operator CLI vs. running server) there is no such reference to have.
>    It only deletes the file; the running server's in-memory tracker
>    resyncs itself on its next poll tick when it sees the record is gone
>    (point 1's unconditional check, plus a new `RetryTracker.isQuarantined`
>    used only to decide whether a resync is needed).
>
> A fourth, process-level finding: this repo's Zig test builds do not
> transitively discover `test` blocks in every `@import`ed file — files
> reached only through a used-but-not-otherwise-referenced import need an
> explicit `test { _ = @import(...); }` hook (an established pattern already
> used for `retry_backoff.zig` in `zruntime.zig`, and for `workflow_queue_cli.zig`
> in `dev_cli.zig`). `durable_dead_runs.zig` and `durable_dead_runs_cli.zig`
> needed the same treatment in both files, or their tests silently never ran
> despite the build succeeding — caught by injecting a deliberately-failing
> assertion and observing the test count not move.

**Goal**

Persist a durable run's terminal failure state the moment it crosses
`RetryTracker.quarantine_threshold`, instead of only existing as ephemeral
in-process tracker state.

**Requirements**

- Covers R4.

**Files**

- `packages/runtime/src/durable_dead_runs.zig` (new)
- `packages/runtime/src/durable_recovery.zig`

**Approach**

- New file `durable_dead_runs.zig` defines a dead-run record (oplog
  filename, run key, quarantined-at timestamp, consecutive-failure count,
  and the concrete failure reason) and a `writeDeadRun` function using a
  local atomic tmp-write + fsync + rename, mirroring
  `workflow_queue.zig`'s private `writeFileAtomic` shape (KTD3).
- Records live at `<durable_dir>/dead-runs/<name-derived-from-oplog>.json`,
  a sibling of the oplog directory (KTD2) — never touching the oplog file.
- In `durable_recovery.zig`'s `recoverIncompleteOplogsTracked`, the
  `.failed` branch currently swallows the concrete error into the enum tag
  before calling `tracker.recordFailure`. Thread the actual error/reason
  string through so it can be captured in the record.
- Call `writeDeadRun` exactly once, at the transition where
  `recordFailure` first causes `shouldAttempt` to become false for that
  name (not on every subsequent poll) — a re-quarantine of an already
  dead-lettered run must be a no-op, not a duplicate write.

**Tests**

- Crossing `quarantine_threshold` writes exactly one dead-run record with
  the correct reason, run key, and timestamp.
- A run that fails 9 times then recovers never gets a record.
- Polling an already-quarantined run again does not rewrite or duplicate
  its record.
- The oplog file is byte-for-byte unchanged after quarantine.

#### U6 - replayDeadRun and startup skip-if-dead

**Goal**

Let an operator clear a dead-run record and have the run picked back up by
recovery from its existing oplog; make recovery-on-startup honor an
existing dead-run record even though the in-memory tracker resets on
restart (KTD1).

**Requirements**

- Covers R4.

**Dependencies**

- U5.

**Files**

- `packages/runtime/src/durable_dead_runs.zig`
- `packages/runtime/src/durable_recovery.zig`

**Approach**

- `replayDeadRun` deletes the persisted record and calls
  `tracker.clear(name)`, then leaves the untouched oplog for the next
  recovery poll to pick up naturally — no oplog mutation.
- `discardDeadRun` deletes the persisted record only, without touching the
  oplog. The run stays permanently unretried (matching today's silent
  behavior) but the operator's decision is now recorded and the record
  list stops showing it — consistent with the "explicit, inspectable
  cleanup" convention from the 2026-06-30 plan.
- `recoverIncompleteOplogsTracked` must check for an existing dead-run
  record before attempting a run — not just consult the in-memory
  `RetryTracker`, which is empty after a restart (KTD1). A run with a
  standing dead-run record is skipped until `replayDeadRun` removes it.

**Tests**

- `replayDeadRun` removes the record, resets the tracker, and the next
  recovery poll retries the run from the untouched oplog.
- `discardDeadRun` removes the record without touching the oplog; the run
  is not retried on the next poll.
- Simulated restart (fresh `RetryTracker`, existing dead-run record on
  disk) does not retry the quarantined run.
- `discardDead`/`replayDeadRun` on a non-existent record surfaces a clear
  error, not a silent no-op.

#### U7 - `durable dead-runs` CLI

**Goal**

Give operators `list`/`show`/`replay`/`discard` access to dead-run records,
mirroring `workflow-queue`'s existing dead-letter CLI exactly.

**Requirements**

- Covers R4.

**Dependencies**

- U5, U6.

**Files**

- `packages/runtime/src/durable_dead_runs_cli.zig` (new)
- `packages/runtime/src/dev_cli.zig` (dispatch + test-collection hook — see
  U5's amendment)
- `packages/runtime/src/runtime_cli.zig`
- `packages/runtime/src/cli_help.zig` (added `durable dead-runs` to
  `help --all` and its anti-drift test, matching `workflow-queue`'s
  existing entry — not in the original Files list, but the two dead-letter
  CLIs would otherwise have inconsistent discoverability)

**Approach**

- New `durable_dead_runs_cli.zig` mirrors `workflow_queue_cli.zig`'s exact
  shape: a `Command` enum (`list`, `show`, `replay`, `discard`, `help`),
  `Options`/`parseOptions` (`--durable <DIR>` plus positional id), and a
  `runWith(allocator, argv, stdout, stderr)` entry point.
- Wire a `durable` top-level command with a `dead-runs` sub-verb into
  `dev_cli.zig`/`runtime_cli.zig` following the identical
  `if (eql(command, "workflow-queue"))` dispatch pattern already used for
  `workflow-queue`.
- Match `workflow_queue_cli.zig`'s exit-code convention
  (`isExpectedUserError` → exit 1) so both dead-letter CLIs behave
  identically from a caller's perspective.

**Tests**

- `list` shows quarantined run IDs sorted, matching `listDeadIds`'s
  contract.
- `show` prints the record's reason/timestamp/failure-count.
- `replay`/`discard` route to U6's functions and surface their errors with
  the same formatting `workflow_queue_cli.zig` uses.

#### U8 - Crash-recovery regression tests under the fresh-GPA runner

**Goal**

Guarantee the new dead-run code path is exercised somewhere that would
actually catch a double-free or use-after-free, given the documented
history of exactly that bug class in this file.

**Requirements**

- Covers R4.

**Dependencies**

- U5, U6, U7.

**Execution note**: run these specific tests under `test-cli`'s
per-test-fresh GPA allocator, not `test-zruntime`'s arena-wrapped `Runtime`
fixture — the latter is documented to mask a double-free as a silent no-op.

**Files**

- Test blocks in `durable_recovery.zig`, `durable_dead_runs.zig`, and
  `durable_dead_runs_cli.zig` — reachable from `test-cli` via the
  `zruntime.zig`/`dev_cli.zig` explicit test-collection hooks (U5's
  amendment), not merely "reachable from the runtime_cli.zig root" as
  originally assumed.

**Approach**

- Reused the existing crash-recovery test fixture shape from the
  `durable_recovery.zig` timeout-recovery test (deliberately-nonexistent
  handler path, so `recoverOne` fails before ever touching the JS
  runtime — staying clear of the separate, pre-existing recoverOne
  double-free bug on real JS execution). Drove 9 failures directly via
  `RetryTracker.recordFailure` (mirroring the pre-existing quarantine
  test) to avoid racing real wall-clock backoff timing, then crossed the
  10th through the real `recoverIncompleteOplogsTracked` path so the
  actual `writeDeadRunRecord` call site is what's under test.

**Tests**

- `"durable recovery: full dead-run lifecycle - quarantine writes a
  record, replay clears it"`: 10 consecutive failures → record written
  with the real run key and failure reason → oplog byte-for-byte
  unchanged → a further poll does not duplicate the record →
  `replayDeadRun` → next poll resyncs the in-memory tracker and retries.
- `test-cli` run: 654 passed, 0 failed, **0 leaked** — the leak count is
  the actual signal this unit exists to produce, given the documented
  double-free history in this file.

---

### Phase 4: Proven-Exhaustive Saga Compensation Coverage

#### U9 - `saga_extractor.zig`

**Goal**

Give the analyzer a compile-time model of a saga's steps for the first
time, reusing the literal-walk-or-bail discipline `intent_extractor.zig`
already established.

**Requirements**

- Covers R5.

**Files**

- `packages/zigts/src/saga_extractor.zig` (new)
- `packages/zigts/src/contract_types.zig`
- `packages/zigts/src/handler_contract.zig`
- `packages/zigts/src/contract_builder.zig` (wires the extractor into
  `build()` and finds `saga`'s import binding slot; not in the original
  Files list, but extraction needs a call site)

**Approach**

- Mirrored `intent_extractor.zig`'s pattern exactly: a `Deps`/`AtomResolver`
  struct, a linear scan over every IR node for `.call` tags whose callee
  resolves (via a small self-contained import scan for `zigttp:workflow`'s
  `saga`) to the tracked binding slot, then a literal walk of the first
  argument as an array of object literals (`name` literal string required,
  `run`/`compensate` presence-only, unknown sibling key or non-literal
  shape marks the call dynamic).
- Added `SagaStep {name, has_compensate}` and `SagaCallInfo {steps,
  dynamic, source_line, source_column}` to `contract_types.zig`, plus a
  `sagas: std.ArrayList(SagaCallInfo)` field on `HandlerContract` (bumped
  `version` 15 → 16). `SagaCallInfo.compensationProven()` is a method on
  the struct itself (not a separate derived field), computing "every
  non-last step has `compensate`" on demand.
- Reused `contract_builder.zig`'s existing `intentAtomResolver` as the
  resolver callback (same function-pointer type as `intent_extractor`'s),
  rather than writing a second identical resolver.

**Tests**

- `"saga extractor collects steps and has_compensate flags from a static
  saga"`, `"saga extractor marks a spread-constructed saga dynamic"` (both
  in `contract_builder.zig`, via `buildTestContract`).

#### U10 - ZTS510 rule (construction site amended: `contract_builder.zig`, not `system_linker.zig`)

> **Amendment**: the plan called for `system_linker.zig` because saga
> steps typically dispatch cross-handler via `call(name, ...)`. Once
> `SagaCallInfo` actually existed (from U9), the check itself turned out to
> need none of that: "does a non-last step have a `compensate` key" is
> fully decidable from one handler's own `contract.sagas` — no cross-handler
> resolution, no `system.json`, nothing `system_linker.zig` uniquely
> provides. `system_linker.zig` remains genuinely necessary for Phase 5's
> affordance-link proof extension (that one *does* need cross-handler
> route resolution), so this correction doesn't erase its role — it just
> means this particular check doesn't belong there. Implemented as a new
> unconditional "Phase 4e" in `contract_builder.zig`, immediately following
> ZTS509's "Phase 4d", for the same reason ZTS509 landed there: no
> `Spec<...>`/`Effects<...>` gating.

**Goal**

Fail the build when a statically-analyzable saga has a non-last step
without a `compensate`, closing the partial-rollback hole Workstream B2
originally scoped.

**Requirements**

- Covers R5.

**Dependencies**

- U9.

**Files**

- `packages/zigts/src/rule_registry.zig`
- `packages/zigts/src/contract_types.zig`
- `packages/zigts/src/contract_builder.zig` (not `system_linker.zig` — see
  Amendment above)
- `packages/tools/src/precompile_check.zig`,
  `packages/proof-review/src/spec_diagnostic.zig` (exhaustive
  `SpecDiagnostic.Kind` switches, same as ZTS509 in Phase 2)

**Approach**

- Added `SpecDiagnostic.Kind.saga_step_missing_compensate` (ZTS510) and a
  matching `rule_registry.zig` entry.
- `emitSagaCompensationDiagnostics` walks `contract.sagas`, skips
  `dynamic` sagas and empty step lists entirely (KTD6/R5 — unproven, not
  failed), and flags any non-last step where `has_compensate == false`.

**Tests**

- `"ZTS510 fires for a non-last saga step missing compensate"`, `"ZTS510
  does not fire when only the last saga step omits compensate"`, `"ZTS510
  does not fire for a fully-covered static saga"`, `"ZTS510 never fires
  for a dynamically-constructed saga"` (all in `contract_builder.zig`).
- Zero `ZTS510` hits across every `examples/workflow/*.ts` file, confirmed
  via `zigttp check --json` — `saga-orchestrator.ts`'s real
  `ship`-is-last-with-no-compensate pattern is correctly proven, not
  flagged.

#### U11 - Saga compensation-coverage contract property

**Goal**

Surface the proof verdict to receipts, `contract.json`, and `zigttp
verify`, the same way existing durable-workflow properties are surfaced.

**Requirements**

- Covers R5.

**Dependencies**

- U9, U10.

**Files**

- `packages/zigts/src/contract_json_writer.zig`
- `packages/zigts/src/contract_json_parser.zig`
- `packages/zigts/src/handler_contract.zig` (test only)

**Approach**

- `contract.json` gained a top-level `"sagas"` array (one entry per
  `saga([...])` call site, not a single collapsed boolean — a handler can
  have more than one saga), each with `dynamic`, `compensationProven`
  (computed, not stored), `steps: [{name, hasCompensate}]`, `sourceLine`,
  `sourceColumn`. `compensationProven` is written for convenience but not
  parsed back on read (it's derived from `dynamic`+`steps`, which are).
  Mirrors `intent`'s existing write/parse pair in both files.

**Tests**

- `"parseFromJson roundtrip preserves sagas"` (`handler_contract.zig`):
  two saga call sites (one static + proven via the last-step exception,
  one dynamic) survive a full write → parse round trip.

#### U12 - Saga proof end-to-end tests

**Goal**

Verify the extractor, diagnostic, and contract property agree with each
other across the full saga shape space.

**Requirements**

- Covers R5.

**Dependencies**

- U9, U10, U11.

**Files**

- Test blocks in `contract_builder.zig` (extraction + diagnostic, via
  `buildTestContract` — not `saga_extractor.zig`/`system_linker.zig` as
  originally planned; see U10's amendment for why the diagnostic moved)
  and `handler_contract.zig` (JSON round-trip).

**Approach**

- Table-style coverage across `contract_builder.zig`: fully covered static
  saga, non-last-step gap (fails), last-step-only gap (passes, matches
  `saga-orchestrator.ts`), and dynamic construction (never fires,
  regardless of shape) — six tests total across U9/U10, plus U11's
  round-trip test.

**Tests**

- See U9/U10/U11's Tests lists — this unit's matrix is the union of all
  three, run together as `test-zigts` (1466 passed, 1 skipped, 0 failed).

---

### Phase 5: Affordance Coverage and Close-Out

#### U13 - Extend proof phases to `affordance_links`

**Goal**

Give HATEOAS affordance links the same payload/injection/retry-safety
proof coverage ordinary links already get.

**Requirements**

- Covers R6.

**Files**

- `packages/zigts/src/system_linker.zig`

**Approach**

- Phases C2 (payload, `analyzePayloadProof`), D (cross-boundary/injection),
  and E (failure cascade/retry-safety) currently iterate `links.items`
  only. Add a parallel iteration over `affordance_links.items` in each
  phase, reusing the same per-phase analysis functions rather than
  duplicating their logic.
- Sequence this after Phase 4 (U9-U12) since both touch
  `system_linker.zig` — implementing U13 after U10 minimizes diff overlap
  in the same file.

**Tests**

- A HATEOAS affordance link with an incompatible payload shape now fails
  the same payload-proof check an equivalent ordinary link would.
- A HATEOAS affordance link into a non-retry-safe target now fails the
  same failure-cascade check an equivalent ordinary link would.
- Existing `affordance_responses_covered` behavior (Phase A2b) is
  unchanged.

#### U14 - Examples and documentation close-out

**Goal**

Leave no new surface from this plan undocumented, and fill the
previously-identified example gaps.

**Requirements**

- Covers R7.

**Dependencies**

- U1-U13 (documents their cumulative result).

**Files**

- `examples/workflow/` (new example files)
- `docs/durable-workflows.md`

**Approach**

- Add example coverage for: `scope`/`using`/`ensure` usage, `signalAt`
  (existing `wait-signal-orchestrator.ts` only uses `signal`), queued
  `fanout` (the `--workflow-queue` + `fanout()` combination has no example
  today), and a saga-compensation-failure path (an example where the
  `compensate` thunk itself throws, distinct from the existing
  `saga-orchestrator.ts` success-then-rollback path).
- Document: the new `durable dead-runs` CLI (U5-U8), ZTS509 and ZTS510
  (U3, U10), the `saga.compensationProven` contract property (U11), the
  corrected `fanout()`/`race()` semantics (U1), and the restart-behavior
  change from KTD1 (a restarted process now honors standing dead-run
  records instead of silently forgetting quarantine state).

**Tests**

- Test expectation: none for docs. New example files must pass
  `bash scripts/test-examples.sh`, including the new saga-compensation-
  failure example asserting the compensation failure surfaces as the
  documented terminal 500.

---

## Scope Boundaries

### Deferred to Follow-Up Work

- **Genuine `fanout()` concurrency**: blocked on per-call runtime/
  interpreter context isolation — today's sequential dispatch relies on
  shared globals (`zruntime.current_runtime`, `interpreter.current_interpreter`,
  `http.call_function_callback`) that are swapped per call and are not
  safe to share across concurrent calls without a larger runtime change.
- **Genuine `race()` latency-based selection**: blocked on
  `execute_fetches_fn` exposing per-descriptor completion signaling
  (select/poll or a completion callback) instead of joining all threads
  before scanning for the first success.
- **Saga proof for dynamically-constructed do/undo sets**: only static
  arrays are proven in this pass (R5/KTD6); a saga built from spreads,
  conditionals, or externally-referenced step arrays stays unproven.
- **Saga effect-body analysis**: this pass proves structural presence of
  `compensate`, not whether an existing `compensate` correctly undoes its
  paired `run`. Verifying that would require full side-effect analysis of
  step bodies.
- **Saga vs. `--workflow-queue` reconciliation**: unchanged from the
  2026-06-30 plan's guardrail (KTD carried forward: R9 there, still out of
  scope here).
- **Hosted/cloud orchestration**: unchanged strategic exclusion
  (`STRATEGY.md`, `docs/roadmap.md`).

---

## Verification Contract

| Gate | Command |
|---|---|
| Format | `zig fmt --check build.zig packages/` |
| Runtime workflow tests | `zig build test-zruntime` |
| ZigTS/proof tests | `zig build test-zigts` |
| CLI tests (Phase 3, 7's `durable dead-runs`) | `zig build test-cli` |
| Examples | `bash scripts/test-examples.sh` |
| Docs | `zig build test-docs-drift test-doc-links` |
| Full verification | `bash scripts/verify.sh` |
| Diff hygiene | `git diff --check` |

Manual drills to keep with the implementation branch:

- Quarantine a durable run (force 10 consecutive recovery failures), kill
  and restart the process, confirm it stays quarantined until
  `zigttp durable dead-runs replay <id>` is run.
- Compile a handler with `step(() => call("x", {}))` and confirm ZTS509
  fails the build with an actionable message.
- Compile the existing `saga-orchestrator.ts` example unmodified and
  confirm it passes ZTS510 (last-step-no-compensate is allowed); then
  remove `compensate` from a non-last step and confirm ZTS510 fires.

If a full gate is too slow during an intermediate unit, run the focused
command for the touched package first and leave `bash scripts/verify.sh`
for the closing unit (U14).

## Definition of Done

- R1-R7 are implemented, tested, and documented; only items listed under
  Scope Boundaries may remain deferred.
- U1-U14 focused tests pass, with U8's tests specifically confirmed to run
  under `test-cli`'s fresh-GPA allocator, not `test-zruntime`'s
  arena-wrapped fixture.
- `bash scripts/verify.sh`, `zig build test-docs-drift test-doc-links`, and
  `git diff --check` pass before final commit.
- No persisted replay key, oplog format, or `system_hash` behavior is
  changed without a compatibility note (KTD2/stop condition).
- `durable dead-runs discard`/`replay` remain explicit operator actions;
  no automatic destructive retention is introduced (KTD2, carried from the
  2026-06-30 plan).
- Saga under `--workflow-queue` remains rejected, tested, and documented,
  unchanged from the 2026-06-30 plan.
- The restart-behavior change from KTD1 (standing dead-run records survive
  a process restart) is called out explicitly in docs (U14), not shipped
  silently.
- Every new CLI verb, diagnostic code, and contract property this plan
  introduces is documented in `docs/durable-workflows.md`.

## Sources & Research

- `docs/plans/2026-07-02-001-decision-workflow-fault-tolerance-gaps.md` —
  origin decision record; Options A/B/C and the six minor findings this
  plan formalizes.
- `docs/plans/2026-06-30-001-feat-workflow-fault-tolerance-plan.md` —
  predecessor plan; source of the stop conditions and KTD9-equivalent
  guardrails carried forward here.
- `docs/durable-workflows.md`, `docs/reliability.md` — existing
  proofs-vs-runtime-guarantees documentation convention followed by U11.
- `~/.claude/plans/zany-sniffing-pearl.md` (external, non-repo planning
  artifact, cited for provenance only — not a file to open during
  implementation) — Workstream B2's original one-line sketch of the saga
  compensation-coverage idea this plan's Phase 4 formalizes and implements.
