---
title: Workflow & Fault-Tolerance Gap Analysis - Decision Record
type: decision
topic: workflow-fault-tolerance-gaps
date: 2026-07-02
status: proposed
---

# Workflow & Fault-Tolerance Gap Analysis - Decision Record

**PDR-1**: Which missing workflow/fault-tolerance capability is worth
building next
**Date**: 2026-07-02
**Status**: Proposed
**Stakeholders**: srdjan

## Context

### Background
The workflow/fault-tolerance surface (durable `run/step/stepWithTimeout/
sleep/waitSignal`, crash recovery, workflow queue with dead-lettering,
`workflow.call/saga/fanout/follow`, proof-gated retry/idempotency) landed
through commit `82d4494b` per
`docs/plans/2026-06-30-001-feat-workflow-fault-tolerance-plan.md`. A parallel
hardening pass (atomic signal writes, tightened file permissions, JSON
surrogate-pair correctness) is in flight uncommitted as of this analysis.

### Problem Statement
Confirm, from the code as it exists today (not from memory or docs alone),
whether any capability gap in this surface is severe enough to warrant
scoping as the next unit of work.

### Constraints
- Must not weaken proof soundness or existing replay-name compatibility
  (stop conditions carried over from the 2026-06-30 plan).
- Must not introduce automatic destructive retention.
- Saga + workflow-queue reconciliation and hosted/cloud orchestration are
  out of scope by prior strategic decision (STRATEGY.md, `docs/roadmap.md`)
  — not reconsidered here.

## Options Considered

### Option A: Durable-run dead-letter parity
**Description**: `durable_recovery.zig`'s `RetryTracker` quarantines a
durable run after 10 consecutive recovery failures (`durable_recovery.zig`
around lines 38-99) with no persisted record and no CLI surface — the run is
silently abandoned forever. `workflow_queue.zig` already solved the same
problem for queued dispatch: `listDeadIds`/`readDead`/`replayDead`/
`discardDead`. The durable-run engine underneath everything else in this
surface never got the same treatment.
**Pros**: closes a genuine silent-data-loss hole; the pattern to copy
already exists verbatim in the same codebase; reuses `durable_store.zig`'s
atomic-write helpers.
**Cons**: touches the crash-recovery path, which memory flags as having had
at least one prior subtle double-free bug and "fragile test coverage...
invisible under test-zruntime's arena-wrapped allocator."
**Effort**: M
**Risk**: Low-Medium
**User Value**: High

### Option B: Compile-time rejection of `workflow.call` nested in `step()`
**Description**: `runtime_workflow.zig:86-92` — `workflow.call` is only
durable-recorded at step depth 0; nested inside a user `step()` it silently
becomes non-durable. No compile error, no runtime error. This contradicts
the project's own fail-closed-on-ambiguity design (elsewhere, proof-gated
retry/idempotency return a 599 rather than silently guess).
**Pros**: on-strategy (extends "compiler proves it's safe" to an ambiguity
the analyzer currently misses); likely a build-time diagnostic, not new
runtime plumbing, so probably lower effort than Option A.
**Cons**: purely preventive (no reported incident yet); scope of the
analyzer check needs care to avoid false positives on legitimate top-level
`call()` usage.
**Effort**: S-M
**Risk**: Low
**User Value**: Medium-High

### Option C: Proven-exhaustive saga compensation coverage
**Description**: A prior design (Workstream B2, referenced in
`~/.claude/plans/zany-sniffing-pearl.md`) scoped compile-time proof that a
saga's `undo:` set covers every `do:` step. It was never implemented — today
a saga compensation failure is a terminal 500 "manual intervention" with no
compile-time backing.
**Pros**: most strategically differentiated of the three (directly extends
the proof engine into the one workflow primitive that currently fails open
to human intervention).
**Cons**: largest effort of the three (new analyzer proof surface, not a
runtime fix); needs a documented escape hatch for dynamic/conditional
compensation (pattern already exists: `intent.dynamic = true`).
**Effort**: L
**Risk**: Medium
**User Value**: High (strategic)

*(Lower-severity gaps not scored as options — naming/consistency issues,
not correctness: `fanout()` dispatches sequentially despite the name
(`runtime_workflow.zig:835`); `race()` picks first-success-in-declaration-
order, not lowest latency (`io.zig:16`, a documented future-version note);
concurrency caps are inconsistent (`fanout`=16 vs `parallel`/`race`=8);
`system_linker.zig:158-160` skips payload/injection/retry-safety proofs for
HATEOAS affordance links that ordinary links get; `workflow_queue.zig`'s
lease reclaim has no backoff/jitter unlike durable-run recovery's
exponential backoff; `examples/workflow/` has no coverage for `scope`/
`using`/`ensure`, `signalAt`, queued `fanout`, or the saga-compensation-
failure path.)*

## Decision

**Selected**: Option A — Durable-run dead-letter parity, formalized as a
Hypothesis Canvas below rather than committed to a PRD directly, since it
has not yet been user-validated as the right next slice of work.

### Rationale
Option A is the only one of the three that is a live correctness gap today
(silent, unrecoverable data loss) rather than a preventable-but-unreported
risk (B) or a strategic-but-large investment (C). It also has the lowest
implementation risk of translating an existing, already-proven pattern
(`workflow_queue`'s dead-letter API) onto a sibling subsystem.

### Trade-offs Accepted
- Option B (silent durability downgrade) and Option C (unproven saga
  compensation) remain open; not being acted on in this decision.
- This decision record does not commit to building Option A — it only
  elevates it to a validated hypothesis, per product-god's mode ladder.

### Success Metrics
- 30 days: hypothesis below is validated or explicitly killed.
- 60 days: if validated, Option A has a PRD.
- 90 days: if shipped, 0 durable runs are quarantined without an
  inspectable record.

## Implementation Plan

### Immediate Actions
- [ ] Review the Hypothesis Canvas below.
- [ ] If accepted, graduate to a `standalone_prd` for Option A.
- [ ] Separately decide whether Option B or C is worth a hypothesis of its
      own (not blocked on Option A).

### Risks & Mitigations
| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Crash-recovery path is fragile (per memory: prior double-free bug, weak test isolation) | Medium | High | Add regression tests under test-cli's per-test-fresh GPA runner, not test-zruntime's arena-wrapped allocator (the one that hid the prior bug) |
| Persisting failure state changes the oplog format in a replay-incompatible way | Low | High | Stop condition carried from the 2026-06-30 plan: do not break persisted replay names or formats; store dead-run records alongside, not inside, the oplog |

---

## Appendix: Hypothesis Canvas — Durable-Run Dead-Letter Parity

### Hypothesis Statement

We believe **operators running zttp handlers with `--durable`**

Will **need to see and act on durable runs that permanently fail recovery**

Because **`durable_recovery.zig`'s `RetryTracker` currently quarantines a
run after 10 consecutive recovery failures with no persisted record, no CLI
surface, and no way to inspect or replay it — unlike `workflow_queue.zig`,
which already solved this exact problem with `listDeadIds`/`readDead`/
`replayDead`/`discardDead`**

Which will result in **zero silent data loss for durable executions, and
parity between the two fault-tolerance surfaces the runtime ships (durable
runs vs. queued workflow dispatch)**

We'll know this is true when **100% of permanently-failed durable runs are
recorded to an inspectable dead-run store, and a `zttp durable dead-runs
list|replay|discard` surface exists, mirroring `workflow queue`'s existing
CLI**

---

### Supporting Evidence

**User Research**
- Interview quotes: none collected — this is an internal infra-symmetry
  finding, not an externally reported complaint.
- Analytics insights: n/a (no production telemetry on quarantine events
  exists yet — itself part of the gap).

**Market Validation**
- Competitor features: Temporal, AWS Step Functions, and Cadence all expose
  failed-workflow visibility (terminated/failed execution lists, retry from
  history) as baseline expectations for a durable-execution product.
- Customer requests: none yet; this is a proactive gap-closure, not a
  reactive one.

**Technical Feasibility**
- Effort estimate: Medium. `durable_recovery.zig` already has the
  `RetryTracker` and the quarantine call site (~lines 38-99); the exact
  pattern to reuse (persisted record + list/read/replay/discard) exists
  verbatim in `workflow_queue.zig`.
- Risk assessment: Low-Medium — mostly additive, but the crash-recovery path
  has a documented history of a subtle double-free bug and test coverage
  that memory flags as "invisible under test-zruntime's arena-wrapped
  allocator."
- Dependencies: reuse `durable_store.zig`'s existing atomic tmp-write +
  fsync + rename helpers rather than adding new ones.

---

### Experiment Design

**MVP Scope**
On quarantine, write the durable run's terminal state + failure reason to a
new `dead-runs/` directory (same durable dir, same `0600` permissions as the
in-flight hardening pass) instead of abandoning it in place. Add
`zttp durable dead-runs list|replay|discard`, mirroring the four
`workflow_queue` verbs.

**Success Criteria**
- Primary metric: % of quarantined durable runs with an inspectable record
  (target: 100%, current baseline: 0%).
- Target: land as one focused implementation unit, not a multi-week effort.
- Timeline: next slice after this decision record is accepted.

**Learning Goals**
1. Does the `workflow_queue` dead-letter code shape transplant cleanly onto
   `durable_recovery`'s different failure trigger (recovery exhaustion vs.
   lease/attempt exhaustion)?
2. Do real crash-recovery test fixtures reveal edge cases the queue's tests
   didn't (e.g., partially-replayed steps at quarantine time)?
3. Is CLI verb parity (`durable dead-runs` vs `workflow-queue dead`) the
   right UX, or should the two dead-letter surfaces be unified?

**Kill Criteria**
- If capturing durable-run failure state turns out to require changing the
  oplog format in a way that breaks existing replay compatibility (a stop
  condition carried from the 2026-06-30 plan), stop and re-scope as an
  explicit breaking-change proposal instead of shipping this as-is.

---

### Post-Launch Plan

**If Successful**
- Next iteration: decide whether to formalize Option B (compile-time
  rejection of `workflow.call` nested in `step()`) and/or Option C
  (proven-exhaustive saga compensation) as follow-on hypotheses.
- Resource allocation: n/a (solo project).

**If Failed**
- Alternative approach: ship a cheaper, lower-fidelity signal instead — a
  log-level warning plus a metrics counter for quarantined runs, without a
  full CLI dead-letter surface.
- Lessons learned: capture in `docs/solutions/` per this repo's compounding
  convention if the kill criteria above is what triggers the stop.

---

## Critical files referenced (evidence trail)

- `packages/runtime/src/durable_recovery.zig` (RetryTracker/quarantine
  ~lines 38-99, `signalDefinitelyAbsent` ~232-236, recovery double-free note
  ~513-573)
- `packages/runtime/src/workflow_queue.zig` (dead-letter API: `listDeadIds`,
  `readDead`, `replayDead`, `discardDead`; lease/attempt limits ~lines 16-18,
  302-339)
- `packages/runtime/src/durable_store.zig` (atomic tmp-write+fsync+rename
  pattern ~lines 3-6, 405, 643, 740-761)
- `packages/runtime/src/runtime_workflow.zig` (`workflow.call` step-depth
  gating ~86-92, `saga` vs `--workflow-queue` rejection ~653-655, `fanout`
  sequential dispatch note ~798, 835)
- `packages/zts/src/modules/workflow/io.zig` (`race()` latency note line
  16, `MAX_PARALLEL` = 8)
- `packages/zts/src/system_linker.zig` (affordance proof-coverage scope
  note ~158-160)
- `docs/plans/2026-06-30-001-feat-workflow-fault-tolerance-plan.md` (landed
  scope, stop conditions, explicit deferrals)
- `docs/durable-workflows.md`, `docs/reliability.md`, `docs/roadmap.md`,
  `STRATEGY.md` (shipped-vs-deferred confirmation)
