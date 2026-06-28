---
artifact_contract: ce-unified-plan/v1
artifact_readiness: requirements-only
product_contract_source: ce-brainstorm
canonical_path: docs/plans/2026-06-28-002-feat-expert-clarifying-and-metrics-plan.md
last_updated: 2026-06-28
---

# Expert First-Run: Clarifying Questions + Session Metric Capture - Plan

## Context

zigttp v0.1.0-beta is expert-first for solo developers (`STRATEGY.md`): the AI writes
the handler, the compiler proves it. The launch-critical moment is "English in ->
certified-safe handler out." Grounding the current expert flow surfaced exactly one
unsolved, user-facing friction on that path: ambiguous or complex asks do not reliably
converge in a turn. The loop spends its scarce budget (5 veto attempts, 18 model
round-trips, 60s wall time per turn) having the model **guess** the dev's intent and
then discover via the compiler that it guessed wrong. When the ask is ambiguous, that
whole budget burns resolving ambiguity the dev could have settled in one sentence.

Separately, `STRATEGY.md` stakes the release on three metrics - expert success rate,
round-trips to first green proof, proven-path ratio - and the ledger does **not** record
any of them today (`proofs.jsonl` has no per-session row; `events.jsonl` `Meta` has no
turn/round-trip count). The release would launch blind against its own scorecard.

This plan rounds up the first release with two reinforcing additions: let the agent ask
one clarifying question when intent is materially ambiguous (converting an expensive
compiler-mediated guess-loop into a cheap human round-trip), and capture per-session
metrics so the launch is measurable. The questions are themselves new ledger signal, and
the instrumentation is how you prove the questions actually cut round-trips.

Product authority: Srdjan (sole owner). Open blockers: none.

## Product Contract

### In scope

1. **Clarifying questions (persona-first).** The expert agent asks *one* focused question
   when the dev's intent is materially ambiguous (e.g. "add auth" without a method,
   "make it safe" without naming the property), instead of guessing and spending veto /
   round-trip budget on a misread.
2. **Per-session metric capture.** Every expert session writes one summary record at
   session close carrying the three strategy signals plus turn/round-trip counts, read
   back via `/ledger`.

### Out of scope (deliberate boundaries)

- Mid-turn pause/resume turn-machine surgery (a clarifying question simply ends the turn;
  the dev answers on the next line - the REPL is already a multi-turn conversation with
  retained transcript).
- Clarifying negotiation, preference memory, or multi-question trees - one question, used
  sparingly, never for details inferable from context.
- Analytics phone-home or hosted dashboards (cross-session retention is already marked
  aspirational in `STRATEGY.md`).
- Cross-session median/rate aggregation - defer to a thin reader; this plan writes the
  per-session row and surfaces the latest one.
- New templates or demo recording (separate launch task).
- No new proof machinery - proven-path ratio reuses existing contract data.

### Success criteria

- A deliberately ambiguous first ask ("add auth") makes the agent ask one clarifying
  question rather than guess-and-veto; after the dev answers, it converges to a proven edit.
- A clear ask ("add a GET /health route to src/handler.ts") still edits directly with no
  question - the fast path is preserved.
- Every expert session writes a `session_summary`; round-trips-to-first-green is computable;
  the three `STRATEGY.md` metrics become readable from the ledger.

## Recommended Approach

### Feature 1 - Clarifying questions (persona-first)

The model can already end a turn with plain text: `AssistantReply.Response.final_text`
(`packages/pi/src/turn.zig:57-66`) is rendered to the user (`packages/pi/src/loop.zig:218`)
and ends the turn cleanly with `end_reason = .approved` (`packages/pi/src/loop.zig:485-489`).
The REPL retains the transcript across turns (`packages/pi/src/repl.zig:720-791`), so the
dev's next line answers the question with full context. **No turn-machine changes.**

- **Primary change:** add an operational rule to the persona at
  `packages/pi/src/expert_persona.zig:66-90` instructing the model that when intent is
  materially ambiguous it may ask one clarifying question (as its normal terminal text
  response) instead of proposing an edit - sparingly, only when the ambiguity changes the
  edit strategy. The current rules ("Inspect before editing... Every edit goes through
  compiler veto") bias the model toward immediate action; this adds the explicit ask path.
- **Metric correctness consequence:** because a text-only turn is stamped `.approved` just
  like a successful edit, the metric layer must key "edit applied / handler advanced" on
  the `verified_patch` event (`packages/pi/src/session/events.zig:14-25`), **not** on
  `turn_end = approved`. No new `TurnEndReason` variant is required.

### Feature 2 - Per-session metric capture (lightweight)

- **New event.** Add `session_summary` to the `EventKind` enum
  (`packages/pi/src/session/events.zig:14-25`) with a `SessionSummary` struct:
  `turn_count`, `total_roundtrips`, `verified_patch_count`, `reached_proof: bool`,
  `round_trips_to_first_green: u32`, `proven_path_ratio: f32`, `final_outcome`. Extend the
  `appendEvent` serializer (`packages/pi/src/session/events.zig:111-134`).
- **Count round-trips/turns.** The turn loop already counts `model_roundtrips`
  (`packages/pi/src/loop.zig:239,286,304`). Surface it by adding `roundtrips: u8` to
  `TurnResult` (`packages/pi/src/loop.zig:175-180`), then accumulate per session in
  `runOneTurn` (`packages/pi/src/agent.zig:586-635`), tracking `round_trips_to_first_green`
  until the first `verified_patch`.
- **Proven-path ratio.** Reuse the contract data the check already produces:
  `HandlerContract.behaviors` / `behaviors_exhaustive` (`packages/zigts/src/contract_types.zig:1369`,
  `BehaviorPath` at `:965`). Capture the latest ratio at the post-apply check
  (`packages/pi/src/loop.zig` ~`:413`, where `verified_patch` is emitted). **Verify during
  implementation** how per-path "proven vs unproven" is exposed in the check JSON
  (`packages/zigts/src/contract_json_writer.zig:682`) - if only `behaviors.len` +
  `behaviors_exhaustive` are available, define the ratio against those and note the
  approximation.
- **Write at session close.** Append the `session_summary` once at session teardown -
  `deinit` (`packages/pi/src/agent.zig:199-205`) or REPL loop exit
  (`packages/pi/src/repl.zig:791`).
- **Surface.** Extend `/ledger` (`renderLedgerGuidance`, `packages/pi/src/repl.zig:444-454`)
  to read and show the latest `session_summary`. Optionally extend the CLI export
  (`exportSessionLedger`, `packages/pi/src/ledger.zig:92-114`) to include the row.

### Reuse (do not rebuild)

- `verified_patch` event as the "edit applied / handler advanced" signal.
- `model_roundtrips` counter in `loop.zig` - already counted, just surfaced.
- `behaviors` / `behaviors_exhaustive` in `contract_types.zig` - already computed by check.
- `appendEvent` in `events.zig` for writing the new event.

## Verification

- **Tests alongside code** (`test "..."` blocks; run under `testing.allocator`):
  - `session_summary` serialization round-trips (append + parse).
  - With a stub `ModelClient`: a session of one clarifying *text* turn followed by one
    *edit* turn produces a summary with `verified_patch_count == 1`, `turn_count == 2`, and
    `round_trips_to_first_green` reflecting both turns - confirming text turns are counted
    but not mistaken for edits.
  - `proven_path_ratio` computed from a fixed contract with known `behaviors`.
- **Manual end-to-end** (needs `ANTHROPIC_API_KEY` or `zigttp auth claude`):
  1. `zigttp init demo --expert`
  2. Ambiguous ask: `add auth` -> expect one clarifying question, no edit. Answer it ->
     expect convergence to a proven edit.
  3. Clear ask in a fresh project: `add a GET /health route to src/handler.ts` -> expect a
     direct edit, no question (fast path preserved).
  4. `quit`, then inspect `.zigttp/.../events.jsonl` for the `session_summary` row and run
     `/ledger` to see the metrics.
- **Gate:** run `zig build test-zigts` and `zig build test-cli`; then `bash scripts/verify.sh`
  for the full local gate (run `zig fmt --check` separately per repo convention).

## Notes for execution

- This file is the plan-mode artifact. The canonical brainstorm/handoff copy belongs at
  `docs/plans/2026-06-28-002-feat-expert-clarifying-and-metrics-plan.md`; copy it there
  once out of plan mode, then `ce-plan` can enrich the HOW.
- The single implementation uncertainty to resolve first is the per-path proven/unproven
  granularity for `proven_path_ratio` (see Feature 2). Everything else is wired to
  confirmed touchpoints.
