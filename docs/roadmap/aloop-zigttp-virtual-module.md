# Feasibility: extracting the existing `zigts expert` loop into a reusable virtual-module library

## Scope and intent

This document updates the earlier `aloop`-inspired exploration with the key
project reality: Zigttp already has a production agentic loop in `zigts expert`.

The goal here is to assess how to **extract that loop core** into a reusable,
native virtual-module library so it can be used by:

- the CLI (`zigts expert`),
- handlers (`zigttp:*` virtual modules),
- future adapters (ACP, service mode, editor integrations).

The target remains **not** a line-for-line Python compatibility clone.
Instead, we preserve the useful operational properties associated with modern
agent loops:

- model/provider abstraction,
- tool execution with permission controls,
- session/fork orchestration,
- event streaming for observability,
- embeddable API surface for handlers and runtime integrations.

## Quick verdict

Extraction is feasible and likely lower-risk than building a fresh loop from
scratch because core behavior already exists in `zigts expert`.

- **Feasible now (low/medium risk):** define a loop engine boundary, expose
  `run/stream` APIs, reuse existing session/event machinery, and provide a
  virtual-module façade.
- **Feasible later (medium/high risk):** full multi-agent hierarchy semantics,
  deep adapter parity (ACP/etc.), and complete compatibility layer for
  third-party loop configs.

Recommended approach: treat `zigts expert` as the reference implementation and
perform a staged extraction into `packages/zigts/src/agent/` plus virtual-module
bindings.

## Extraction-first mapping

Current `zigts expert` concern | Extracted reusable unit
---|---
Session lifecycle + events | `AgentSessionStore` interface (open, append, fork, compact)
Planner/repair orchestration | `AgentLoopEngine` with explicit step contracts
Tool invocation + gating | `ToolRegistry` + `CapabilityPolicy` dispatcher
Provider/model transport | `ProviderAdapter` interface (`complete`, `stream`, retries)
CLI UX/rendering | thin adapter over engine events (keep outside core)
Resume/autoloop wiring | orchestration adapter layer over core engine

## Candidate module surface

Primary module:

- `zigttp:agent`
  - `complete(input, opts)` one-shot provider call (no loop state required)
  - `run(input, opts)` tool-using loop execution
  - `stream(input, opts)` structured event stream (`token`, `tool_call`, `tool_result`, `patch`, `done`, `error`)

Supporting module (optional in phase 2+):

- `zigttp:agent/session`
  - `open(id?)`, `append(event)`, `fork(parentId)`, `compact(policy)`

Permissions reuse existing primitives where possible:

- `zigttp:scope` for execution scope,
- `zigttp:io` for bounded filesystem actions,
- `zigttp:service` for provider transport wrappers,
- `zigttp:auth` for API-key/material handling.

## Where extraction should start in practice

`zigts expert` already has loop-specific behavior that should migrate into a
new reusable core package with stable interfaces:

1. Session/event persistence and replay metadata.
2. Iteration control (budget, stall detection, outcome finalization).
3. Tool-call contracts and deterministic application order.
4. Patch/witness bookkeeping and structured outcomes.

The CLI then becomes a consumer of this core, rather than its owner.

## Key design constraints (important)

1. **Determinism and replayability**
   - Tool calls should produce structured, replay-safe event records.
   - Session forks should capture exact parent snapshot hashes.

2. **No hidden ambient authority**
   - Every tool must declare capabilities (fs read/write, net, env, subprocess).
   - The dispatcher denies by default.

3. **Allocator-safe long-lived sessions**
   - Avoid unbounded heap growth in event-heavy runs.
   - Use chunked logs + compaction checkpoints.

4. **Compatibility with existing expert workflows**
   - Preserve current `zigts expert` semantics during extraction.
   - Keep wire/event formats stable unless a migration path is provided.

## Proposed phased delivery

### Phase 0: design spike (1-2 weeks)

- Inventory current `zigts expert` loop boundaries and identify seams.
- Define core interfaces (`AgentLoopEngine`, `ProviderAdapter`,
  `ToolRegistry`, `AgentSessionStore`).
- Add no-op/mock provider and golden tests for deterministic event ordering.

Exit criteria:

- A minimal engine can drive one iteration via mock adapter without CLI code.

### Phase 1: MVP module (2-4 weeks)

- Move `zigts expert` orchestration to consume extracted engine.
- Expose `zigttp:agent.run` + `zigttp:agent.stream`.
- Add one real provider adapter (OpenAI-compatible endpoint via `fetch`).
- Add static tool registry and permission gate.

Exit criteria:

- Existing `zigts expert` flows still pass and handler-level module demos run.

### Phase 2: orchestration hardening (3-6 weeks)

- Add forked child sessions with explicit inheritance modes.
- Add compaction/summarization hooks.
- Add policy presets (readonly, workspace-write, restricted-net).

Exit criteria:

- Multi-agent fanout demos + regression tests for escalation prevention.

### Phase 3: protocol adapters (optional)

- Add ACP-compatible adapter process over core engine.
- Add migration helper for common third-party loop config patterns.

Exit criteria:

- External editor/tooling can drive Zigttp agent loops without Python runtime.

## Risk register

- **Extraction regression risk:** behavior changes while moving code out of CLI.
  - Mitigation: freeze event fixtures and run dual-path comparison tests.
- **Cost/latency variance across providers:** can destabilize tool loop behavior.
  - Mitigation: budget guards and explicit retry/circuit-breaker policy.
- **Complex permission UX:** too many knobs can reduce adoption.
  - Mitigation: opinionated presets + explicit override path.

## Recommendation

Proceed with an **extraction-first** plan: lift the existing `zigts expert`
engine into a reusable core, then wrap it as `zigttp:agent`.

Start with an MVP that proves:

- `zigts expert` remains stable after refactor,
- handlers can invoke the same loop via virtual module APIs,
- permissions and session durability are preserved.

If this MVP proves stable, invest in ACP compatibility and deeper multi-agent
parity as adapter layers over the same core engine.
