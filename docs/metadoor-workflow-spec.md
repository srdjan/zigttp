# Metadoor Workflow Module Story + Spec (`zigttp:workflow`)

## 1) Product Story

Metadoor teams want to define long-running, human-in-the-loop business workflows in a HAL-typed format and execute them with the same reliability properties as infrastructure-grade orchestration systems.

Today, many teams split concerns across:
- API handlers for ingress,
- ad-hoc queue workers for background steps,
- vendor-specific workflow engines for retries and compensation.

This proposal lifts the Metadoor workflow model into a first-class `zigttp` virtual module (`zigttp:workflow`) so workflow definitions can stay close to handlers and ship with normal `zigttp` deployments.

### Problem Statement

We need a workflow runtime that:
1. Preserves deterministic progress across crashes/restarts.
2. Offers typed contracts between steps (HAL-typed).
3. Supports async waits (timers, signals, approvals, external callbacks).
4. Gives replay-safe side effects (idempotent activities).
5. Works with existing `zigttp` durable execution primitives instead of introducing a second scheduler.

### Desired User Experience

A handler author should be able to:
- import the workflow virtual module,
- register a HAL-typed workflow definition,
- start and signal instances,
- observe instance history/state,
- evolve versions safely.

## 2) Goals / Non-goals

### Goals
- Expose workflow APIs through `zigttp:workflow`.
- Compile and validate HAL-typed definitions at load/start time.
- Execute workflows on top of zigttp durable primitives (log, checkpoints, timers, signals, task queues).
- Ensure at-least-once activity dispatch with idempotency keys.
- Provide deterministic replay for workflow logic.

### Non-goals (v1)
- General distributed transactions across arbitrary third-party systems.
- Visual workflow designer.
- Cross-region active/active guarantees beyond existing zigttp durability envelope.

## 3) Module Surface (Draft)

```ts
import {
  defineWorkflow,
  startWorkflow,
  signalWorkflow,
  queryWorkflow,
  cancelWorkflow,
  continueAsNew,
  WorkflowError,
} from "zigttp:workflow";
```

### Core API Sketch

```ts
type HalTypeRef = string; // e.g. "metadoor.order.v2"

type WorkflowDefinition<TInput, TState, TResult> = {
  name: string;
  version: number;
  inputType: HalTypeRef;
  stateType: HalTypeRef;
  resultType: HalTypeRef;
  steps: WorkflowStep[];
};

type StartOptions = {
  workflowId?: string;
  idempotencyKey?: string;
  startAt?: string; // RFC3339
  ttlSeconds?: number;
  tags?: Record<string, string>;
};

defineWorkflow(def: WorkflowDefinition<any, any, any>): void;
startWorkflow(name: string, input: unknown, opts?: StartOptions): Promise<{ workflowId: string; runId: string }>;
signalWorkflow(workflowId: string, signal: string, payload?: unknown): Promise<void>;
queryWorkflow(workflowId: string): Promise<WorkflowSnapshot>;
cancelWorkflow(workflowId: string, reason?: string): Promise<void>;
continueAsNew(workflowId: string, nextInput: unknown): Promise<never>;
```

## 4) HAL-Typed Model

`zigttp:workflow` consumes HAL-typed contracts from Metadoor schema packages.

### Validation Rules
- `inputType`, `stateType`, `resultType` must resolve to known HAL schemas.
- Step I/O must be assignable according to HAL type graph.
- Signals must declare typed payload schemas.
- Version upgrade checks must reject incompatible changes for in-flight instances unless migration hooks are supplied.

### Suggested HAL Binding Strategy
- Keep HAL schema registry immutable per deployment artifact.
- Persist `schemaDigest` alongside workflow events.
- On replay, enforce same digest or run explicit migration path.

## 5) Durable Execution Mapping

Workflow execution is implemented as a thin deterministic layer over zigttp durable primitives:

- **Event log**: append-only workflow history (start, step-scheduled, step-completed, signal-received, timer-fired, completed, failed, canceled).
- **Checkpoint**: compacted materialized state for fast resume.
- **Timer wheel / durable timer**: wake suspended workflows.
- **Signal inbox**: external or internal events delivered in-order per workflow instance.
- **Activity queue**: side-effecting work dispatched with idempotency token.

### Determinism Contract
Workflow code must not read nondeterministic values directly (`Date.now()`, random, external I/O). Instead it uses runtime intrinsics that are recorded in history.

## 6) State Machine

States:
`CREATED -> RUNNING -> WAITING | RETRYING -> RUNNING -> COMPLETED | FAILED | CANCELED | TIMED_OUT`

Rules:
- Terminal states are immutable.
- Cancellation is cooperative: long activities receive cancel token and heartbeat deadline.
- Retry policy belongs to each step with exponential backoff + jitter seed persisted for replay.

## 7) Activity Model

Activity types:
1. **Pure step** (deterministic transform, runs in workflow isolate).
2. **Durable activity** (external side effects; must be idempotent).
3. **Human task** (wait for approval/reject signal).

Required metadata for durable activities:
- `activityType`
- `activityId` (stable per scheduling decision)
- `idempotencyKey`
- timeout + retry policy

## 8) Versioning + Migration

Each workflow definition is identified by `(name, version)`.

Upgrade paths:
- **Compatible patch**: same version, additive metadata only.
- **Minor evolution**: new version for new starts; old runs finish on pinned version.
- **Breaking migration**: `migrate(run, fromVersion, toVersion)` hook to rewrite state/scheduled steps.

`continueAsNew` is the preferred strategy for unbounded histories and major reshaping.

## 9) Observability

Emit structured events + metrics:
- counters: starts, completions, failures, cancellations, retries
- histograms: end-to-end duration, activity latency, queue lag
- gauges: active workflows, waiting workflows, stuck retries

Query APIs should expose:
- current status,
- last checkpoint version,
- pending timers/signals,
- recent event history window.

## 10) Security + Multi-tenancy

- Namespace workflows by tenant/project.
- Enforce per-tenant quotas (active instances, signals/sec, activity concurrency).
- Sign external callback URLs and verify replay-protection nonce.
- Redact sensitive payload fields in logs via schema annotations.

## 11) Failure Semantics

- Worker crash: resume from last committed event.
- Duplicate delivery: dedupe by `(workflowId, eventId)` and activity idempotency key.
- Poison workflow: quarantine after max deterministic replay failures.
- Dead letter signals/activities with operator tooling.

## 12) Example: Order Fulfillment (HAL-Typed)

```ts
const OrderFulfillment = defineWorkflow({
  name: "order-fulfillment",
  version: 2,
  inputType: "metadoor.order.fulfillment.input.v2",
  stateType: "metadoor.order.fulfillment.state.v2",
  resultType: "metadoor.order.fulfillment.result.v2",
  steps: [
    { type: "activity", activityType: "reserve-inventory", retry: "standard" },
    { type: "activity", activityType: "authorize-payment", retry: "aggressive" },
    { type: "wait-signal", signal: "manual-review-approved" },
    { type: "activity", activityType: "create-shipment", retry: "standard" },
  ],
});
```

## 13) Acceptance Criteria (v1)

1. Can register and start a HAL-typed workflow from a handler.
2. Workflow survives process restart and resumes deterministically.
3. Signals and timers unblock waiting workflows reliably.
4. Durable activities are replay-safe through idempotency keys.
5. Query endpoint returns status + recent history.
6. Version pinning prevents in-flight breakage.

## 14) Implementation Plan (Phased)

### Phase 0: Foundations
- Add `zigttp:workflow` module scaffold.
- Add schema-resolution hooks to HAL registry.
- Define durable event types and serialization format.

### Phase 1: Execution Core
- Deterministic interpreter and replay engine.
- Start/signal/query/cancel APIs.
- Durable timers and signal inbox integration.

### Phase 2: Activities + Retries
- Activity dispatcher, idempotency store, retry policies.
- Heartbeats + cancellation tokens.

### Phase 3: Versioning + Ops
- Version pinning, migration hooks, continue-as-new.
- Metrics, tracing, dead-letter/quarantine tooling.

## 15) Open Design Questions

1. Should HAL type checking happen at module init, first start, or build-time precompile?
2. How much history is retained hot vs. archived?
3. Do we expose direct event-stream subscriptions to user code?
4. What is the default deterministic time source granularity?
5. Which activity transport is default (in-proc queue vs remote worker)?
