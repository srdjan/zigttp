---
artifact_contract: ce-unified-plan/v1
artifact_readiness: requirements-only
product_contract_source: ce-brainstorm
date: 2026-06-30
status: deferred-post-beta
---

# Complete Workflow & Fault Tolerance - Plan

## Context

zigttp's workflow orchestration and durable-execution surface is *implemented* but not
*complete* in the senses that ship trust: advanced paths are untested, the durability
guarantees are undocumented, a handful of behaviors are surprising, and the
fault-tolerance primitives a real durable-workflow user reaches for are missing. Two
independent code-mapping passes both concluded the surface is "production-ready, no
stubs" - and that is exactly the trap. The code runs; the *feature* is unfinished
where it matters for someone betting a payment flow on it.

The product thesis is "the AI writes your handler, the compiler proves it's safe."
Durable-execution correctness is mostly a *runtime* property, which sits in tension with
a compile-time proof story. This plan resolves that tension on purpose: wherever a
fault-tolerance property is decidable on the restricted JS subset, we **prove it and let
the runtime trust the proof**; where it is inherently runtime (wall-clock deadlines,
backoff timing, external non-idempotent effects), we build a solid runtime floor. That
turns this work from an orphan runtime feature into one that *compounds* the named
Proof-engine and Expert-agent tracks.

### Decisions locked (from brainstorm dialogue)

1. **Scope bar: Capability completion, in full.** Ship-integrity (test + document +
   fix the existing surface) *and* the missing fault-tolerance primitives. Durable
   workflows treated as a first-class product surface, not an experiment.
2. **Philosophy: Proof-first where decidable.** Enforce the `idempotent` / `retry_safe`
   properties the contract system already extracts; the runtime acts on the proof. A
   step that can't be proven gets a veto/warning the expert loop teaches the user to
   fix. Runtime mechanism is the fallback for the genuinely-undecidable cases only.
3. **Timing: deferred post-beta.** v0.1.0-beta ships on the expert-first core. This plan
   is authored in full now and executed as the next milestone after the beta.

### Goal Capsule

- **Objective:** A solo developer can author a durable, multi-step workflow, have the
  compiler prove its idempotency / retry-safety where decidable, and trust the runtime
  to recover, retry, time-out, and dead-letter correctly for everything else - with
  documented guarantees and tested crash paths.
- **Product authority:** Decisions 1-3 above are settled. The proof-first interlock
  (below) is the spine; do not silently substitute a runtime-mechanism-first design.
- **Open blockers:** None blocking authorship. Sequencing is post-beta. Two design
  questions (deadline cancellation depth, compensation ordering semantics) are flagged
  in Open Questions and resolved at execution time, not now.

---

## Current state: the three-part gap

**Implementation (done):** `call` / `saga` / `fanout` / `follow`
(`packages/runtime/src/runtime_workflow.zig`), durable's 8 exports
(`packages/zigts/src/modules/workflow/durable.zig`, runtime in
`packages/runtime/src/durable_executor.zig`), `io` / `scope` / `compose`, crash recovery
(`packages/runtime/src/durable_recovery.zig`), trace journaling with `fsync`
(`packages/zigts/src/trace.zig`), and the new durable lease-based async queue
(`packages/runtime/src/workflow_queue.zig`, currently uncommitted).

**Correctness surprises (latent):** `stepWithTimeout` checks the deadline *after* the
step finishes; durable fetch retries 5xx but not connection errors (599); no backoff
jitter; saga compensation failure is terminal; signals are last-write-wins (no queue);
idempotency is a *marked* contract property but not runtime-enforced; saga is
deliberately unsupported with `--workflow-queue`.

**Testing gaps:** `stepWithTimeout` has zero tests; `workflow_queue` has happy-path only
(no crash/recovery, no `FailingAllocator`, no corrupted-file quarantine); all five
`examples/workflow/*.ts` have no `.test.jsonl` and aren't in `scripts/test-examples.sh`;
saga-compensation idempotency, signal races, and nested workflows are untested.

**Docs gaps:** no dedicated durable-workflows guide; durability guarantees (exactly-once
vs at-least-once, oplog retention) unspecified; `stepWithTimeout`, idempotency,
crash-recovery behavior, and "why saga != queue" undocumented; no first-workflow
tutorial.

---

## The proof-first interlock (design spine)

The differentiated mechanism that ties the three workstreams together. The contract
system already extracts `idempotent` and `retry_safe` per handler/step
(`packages/zigts/src/handler_contract.zig`; surfaced today in
`packages/tools/src/deploy_manifest.zig` and proof review). Today they are advisory.
This plan makes them load-bearing:

- **Prove → enforce.** A step whose effects are provably idempotent (read-only, or only
  idempotent module calls, or keyed/upsert writes) is *proven idempotent*; the runtime
  then skips the idempotency-key ledger for it. A step proven `retry_safe` is eligible
  for **automatic** retry.
- **Can't prove → expert teaches, runtime floors.** An unprovable step does not silently
  get weaker guarantees. In `expert` mode it surfaces as a teachable veto ("this step
  isn't provably idempotent because it POSTs to a non-idempotent egress host; wrap it in
  an idempotency key or a keyed write"). Outside expert mode, the developer opts into the
  runtime idempotency-key ledger (WS-C3) explicitly - the ledger is the *fallback*, not
  the default.
- **Auto-retry gating.** Step-level auto-retry (WS-C1) is enabled by default *only* for
  proven-`retry_safe` steps. For unproven steps, retries require an explicit opt-in, so
  the runtime never silently double-executes a non-idempotent effect.
- **Receipts.** The proven fault-tolerance properties extend the existing signed
  `kind=workflow` receipt, so the guarantee is portable, not just printed.

This is why "proof-first" is not a slogan here: it is the gate that decides whether a
runtime mechanism (ledger, auto-retry) runs at all.

---

## Workstream A - Ship-integrity (make the existing surface honest)

Goal: the durable surface that already exists is tested on its crash paths, documents
its real guarantees, and has no surprising behavior. This is the part that most directly
serves the trust thesis.

### A1. Fix the correctness surprises
- **Real deadline enforcement for `stepWithTimeout`** - thread the step deadline into
  the `io` / fetch timeout used inside the step and add a cooperative deadline check at
  durable suspension points, so a long fetch is interrupted rather than the deadline
  being noticed only after the step returns.
  Files: `packages/runtime/src/durable_executor.zig` (the post-execution check ~L145-192),
  `packages/runtime/src/runtime_http.zig` (fetch timeout plumbing). See Open Question Q1
  for cancellation depth.
- **Retry connection errors, not just 5xx** - durable fetch treats 599 connection
  failures as retryable under the same `retries` budget. File:
  `packages/runtime/src/runtime_http.zig` (~L1275-1289), `durable_fetch.zig`.
- **Backoff jitter** - add full jitter to the exponential backoff to avoid thundering
  herd. Files: `runtime_http.zig` retry loop, `durable_recovery.zig` `RetryTracker`.

### A2. Test the untested paths
- **`stepWithTimeout` tests** (currently zero): timeout fires, error shape, nested-call
  rejection, millisecond semantics. File: `packages/runtime/src/zruntime.zig` test blocks.
- **`workflow_queue` crash/recovery + failure injection**: partial write + crash →
  recovery, lease-expiry + concurrent re-claim race, corrupted-file quarantine,
  `FailingAllocator` on every queue op (mirror the existing `HandlerPool` stress test).
  Files: `packages/runtime/src/workflow_queue.zig`, `zruntime.zig`.
- **Example test fixtures**: add `.test.jsonl` for all five
  `examples/workflow/*.ts` and wire `workflow/` into `scripts/test-examples.sh`
  (it currently skips that directory).
- **Saga + signal edge cases**: compensation idempotency under replay, partial
  compensation (step 3 of 5 fails), signal sent mid-step (buffered vs lost), nested
  `workflow.call`.

### A3. Document the guarantees
- New `docs/durable-workflows.md`: the durability-guarantee contract
  (exactly-once-effects vs at-least-once, what oplog replay does and does not protect),
  oplog retention/pruning, crash-recovery behavior (retry backoff, the 10-failure
  quarantine), `stepWithTimeout` semantics, "why saga != `--workflow-queue`."
- New `docs/tutorials/first-durable-workflow.md`: end-to-end payment/reservation
  example with tests and a crash-recovery walkthrough.
- Expand the workflow section of `docs/user-guide.md` (currently ~2 paragraphs,
  L253-287) and link out; update `docs/reliability.md` with the recovery/quarantine
  section.

---

## Workstream B - Proof-first property layer (the differentiator)

Goal: lift fault-tolerance correctness into the compile-time boundary wherever the
restricted subset makes it decidable, and make the runtime + expert loop act on it.

### B1. Prove and enforce `idempotent` / `retry_safe`
- **Sound extraction** - derive the properties from the existing effect/flow analysis
  (`packages/zigts/src/flow_checker.zig`, `handler_contract.zig`), not from a declared
  annotation. Conservative decidable slice: a step is idempotent if its effects are
  read-only or restricted to idempotent module calls / keyed writes; `retry_safe`
  follows similarly. Anything outside the slice is `unproven`, never silently `true`.
- **Runtime trust** - `durable_executor.zig` reads the proven property and (a) skips the
  idempotency-key ledger for proven-idempotent steps, (b) permits auto-retry for proven-
  `retry_safe` steps.
- **Expert teaching** - when a step is `unproven`, the `expert` agent surfaces the
  specific reason and the fix (idempotency key, keyed write, split effect). This is the
  on-brand loop: the agent authors code the compiler can prove.
- **Receipt extension** - add the proven properties to the signed `kind=workflow` receipt.

### B2. Proven-exhaustive compensation
- Where decidable, prove a saga's compensation set covers every forward step (every
  `do:` has a reachable `undo:`), so a partial-rollback hole is caught at compile time
  rather than discovered at 2am. Builds on the existing affordance/contract extraction in
  `runtime_workflow.zig` saga handling and `system_linker.zig`.

---

## Workstream C - Runtime primitives (the inherently-runtime floor)

Goal: build the conventional fault-tolerance machinery for the cases proof can't reach.
Each primitive is gated by, or interlocks with, the WS-B proof layer.

- **C1. Step-level retries with backoff/jitter.** `step(name, fn, {retries, backoff})`.
  Auto-enabled for proven-`retry_safe` steps (WS-B1); explicit opt-in otherwise. Backoff
  shared with A1 jitter. File: `durable_executor.zig`.
- **C2. True deadline enforcement.** Generalizes A1's `stepWithTimeout` fix into an
  optional workflow-level deadline for `durable.run(...)`, with cooperative cancellation
  at suspension/io points.
- **C3. Idempotency-key ledger (fallback floor).** `<durable>/idempotency/<handler>/<key>.json`
  state machine (arrived → processing → complete) for steps that call genuinely
  non-idempotent external effects and are therefore `unproven`. Off by default for proven-
  idempotent steps (WS-B1). Files: new `durable` store path, `durable_store.zig`.
- **C4. Signal queues + scheduled-signal TTL.** Replace last-write-wins signal files with
  FIFO enqueue/consume so multiple `signal(key, name)` deliveries survive; add TTL cleanup
  for expired `signalAt` files. Files: `durable_store.zig` (`scanSignals`, signal write),
  `durable_executor.zig` (`waitSignal`).
- **C5. Dead-letter queue + max-attempts policy.** `workflow_queue.zig` already has a
  `dead/` lane and `markDead`, but no attempt counter or max-attempts → dead transition,
  and nothing reads the DLQ. Add an attempt counter to the queue envelope, a configurable
  max-attempts policy, and a `zigttp` inspection path (list/replay dead items). Files:
  `workflow_queue.zig`, a CLI surface in the runtime.
- **C6. Reconcile saga with `--workflow-queue`.** Today saga rejects under the queue
  (`runtime_workflow.zig` ~L634) because compensation order emerges from oplog replay and
  closures don't survive JSON flattening. Either (a) lift the limitation by representing
  compensation as queue-addressable durable steps, or (b) make the rejection a documented,
  first-class "use top-level call/follow/fanout" guardrail. Resolve at execution (Q3).
- **C7. Operational ergonomics.** Configurable recovery poll interval (currently fixed
  1s in `durable_recovery.zig`); queue-depth / pending-work visibility for operators.

---

## Success criteria

- A developer can build a durable workflow from the new tutorial, kill the process
  mid-run, restart, and observe correct resume - with a test that proves it.
- `zig build test`, `zig build test-zruntime`, `zig build test-cli`, and
  `scripts/test-examples.sh` (now including `workflow/`) are green; `scripts/verify.sh`
  passes.
- `stepWithTimeout`, `workflow_queue` crash recovery, saga-compensation idempotency,
  signal queues, and the idempotency ledger each have failing-path + `FailingAllocator`
  coverage.
- A proven-idempotent step runs with no idempotency ledger; an unprovable one is either
  taught-and-fixed in `expert` mode or explicitly ledger-backed - never silently weaker.
- The signed `kind=workflow` receipt carries the proven fault-tolerance properties.
- `docs/durable-workflows.md` documents the guarantee contract precisely enough that a
  user can answer "is this exactly-once?" without reading Zig.

## Verification

1. Unit/integration: the new tests in `zruntime.zig` and `workflow_queue.zig`
   (crash-injection, `FailingAllocator`, lease races).
2. Example E2E: `scripts/test-examples.sh` runs the five workflow fixtures.
3. Manual crash drill: `zigttp dev --durable .zigttp/durable` a multi-step handler,
   `kill -9` mid-step, restart, curl, confirm resume and no double-effect.
4. Proof drill: author an idempotent and a non-idempotent step; confirm `check` /
   `expert` report `proven` vs `unproven` and that the runtime ledger engages only for
   the latter.
5. Full gate: `bash scripts/verify.sh` plus `zig fmt --check` (run separately per repo
   convention).

## Sequencing (all post-beta)

1. **WS-A** first - it is bounded, de-risks the surface already in users' hands, and the
   tests it adds are the safety net for B and C.
2. **WS-B1** next - the proof interlock that gates C1/C3; do it before the runtime
   primitives so they can read the proven properties from day one.
3. **WS-C** - retries (C1) and idempotency ledger (C3) right after B1 (they depend on
   it), then C4/C5/C2, then B2 and C6/C7.

## Open questions (resolve at execution, not now)

- **Q1. Deadline cancellation depth.** True mid-step cancellation needs cooperative
  checkpoints in the interpreter/io. How deep do we go - interrupt only at fetch/io
  boundaries (cheaper, covers the common case) or add general cancellation points?
- **Q2. Idempotency decidable slice.** Exact boundary of what counts as "provably
  idempotent" on the subset - confirm the conservative rule (read-only ∪ idempotent
  module calls ∪ keyed writes) is both sound and useful enough to not flag everything.
- **Q3. saga × queue (C6).** Lift the limitation or formalize the guardrail? Lifting is
  more work and more value; formalizing is honest and cheap.
- **Q4. Oplog/queue retention policy.** Pruning of completed oplogs and dead-letter items
  is currently unbounded-on-disk; pick a default retention and whether it's configurable.
