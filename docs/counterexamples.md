# Counterexamples in the Live HUD

When `zigttp dev --watch --prove` recompiles your handler and a proven
property regresses between the previous build and the new one, the proof
card surfaces a Counterexample block. The block is the compiler's way of
saying not just "you broke it" but "here is the construct that broke it,
here is what to do, and here is what fails."

Counterexamples land in two places at once. The terminal HUD writes them
into the proof card frame stderr already streams. The studio at
`http://localhost:3000/_zigttp/studio` carries the same payload as a
`counterexample` object in the SSE state, so a browser tab open beside
your editor reflects the same regression without a refresh.

## What the block looks like

For the `deterministic` property today:

```
| Counterexample: -deterministic at src/handler.ts:14: Date.now()        |
|   why: remove Date.now() / Math.random() or move the call inside       |
|        a `durable.step`.                                               |
|   [r] replay live   [s] pin as regression test   [a] ask expert to fix |
```

Three sources fold into one frame: the per-property cause that the
verifier records on `HandlerContract.property_provenance`, the
actionable hint from the spec discharge catalog, and the keystroke hints
that name the three follow-on actions. Cause and hint are always present
when a Counterexample renders. The keystrokes are advertised even before
their handlers ship: the agent integration and the live replay
short-cuts are wired in the same release as the frame itself.

## When the block does not render

The Counterexample block stays empty in three cases that read like a no
news / good news boundary: a save that broke nothing leaves `delta`
without demoted properties; a save that demoted a property the verifier
does not yet capture a cause for renders a Why row but no block; a fresh
`seedInitialProof` pass has no baseline to diff against, so demotions
are not yet a concept. The block is opt-in by silence; nothing is forced
on a clean session.

## Cause-only versus flow-driven properties

Two classes of property feed Counterexamples differently:

The cause-only set (`deterministic`, `read_only`, `retry_safe`,
`idempotent`, `state_isolated`, `fault_covered`) is structural. The
verifier classifies the property from the IR and, when it demotes the
property, records a `PropertyCause` with the file line, column, and
snippet that produced the demotion. The Counterexample frame pairs that
cause with the hint catalog in `spec_discharge.suggestionFor` and
renders. No witness, no replay, no request: the snippet is the
counterexample.

The flow-driven set (`no_secret_leakage`, `no_credential_leakage`,
`input_validated`, `pii_contained`, `injection_safe`) has executable
witnesses. The witness solver in `zigts.counterexample.solve` turns a
flow violation's constraint chain into a concrete `Request` plus the
virtual-module stub sequence needed to drive the handler down the
witnessing path. When the live-reload pipeline produces a witness for a
demoted flow property, the frame extends with the request line and two
replay rows showing the previous build's response and the current
build's response. The witness corpus described in
[`witnesses.md`](witnesses.md) is the system of record for these.

v1 ships the cause-only path end to end. The flow-driven extension
reuses the `CounterexamplePreview.failing_request`,
`previous_response`, and `current_response` slots that are already on
the data type; the live-reload integration with `replay_runner` lands as
a focused follow-on. The shape does not change.

## Acting on the block

The three keystrokes hold for both classes:

`[r]` replays the failing request live against the running server. For
cause-only Counterexamples this is a no-op today because the cause is a
construct rather than a runnable request. For flow-driven Counterexamples
this hits the dev server with the synthesised request and the replay
stub responses, so a developer can see the regression reproduce in the
same audit ring the rest of the HUD streams.

`[s]` pins the underlying witness into the corpus as a regression test.
The pinned witness re-fires on every subsequent analysis pass, so a fix
that re-introduces the same construct demotes the property again with
the original counterexample call-out. Pinning is the moment a one-time
catch becomes durable evidence.

`[a]` hands the Counterexample to the `zigttp expert` agent through the
`pi_counterexample_current` tool. The agent's `proof_enrichment` and
`property_goals` channels consume it, so its first response is a fix
proposal grounded in the exact construct the verifier flagged, not a
clarifying question.

## Where it lives in the code

- `packages/runtime/src/counterexample_pipeline.zig` builds the preview
  from the contract and the diff. Single-file orchestrator. Owns
  nothing; borrows from contract, label table, and suggestion catalog.
- `packages/proof-review/src/review.zig` defines
  `CounterexamplePreview` and the optional `counterexample` field on
  `ProofCard`. The data type carries the flow-driven extension slots.
- `packages/runtime/src/proof_card_tui.zig` renders the block under the
  Why row using the same full-width primitive. Three to five rows tall
  depending on whether a witness is present.
- `packages/runtime/src/live_reload.zig` calls `buildFromDelta` after the
  upgrade verifier produces a verdict, before the swap decision. The
  studio receives the preview through the existing `updateFacts` channel.
- `packages/runtime/src/studio.zig` serialises the preview into the
  SSE `counterexample` field of the state JSON so the browser surface
  stays in lockstep with the terminal frame.

## Related reading

[`witnesses.md`](witnesses.md) covers the corpus, the CLI for pinning
and pruning, and the persisted-on-disk layout that the `[s]` keystroke
ultimately writes into.
[`verification.md`](verification.md) describes the property classifier
that produces the demotions in the first place.
[`sound-mode.md`](sound-mode.md) covers the type-directed analyses that
sit upstream of the flow-driven property set.
