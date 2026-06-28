---
name: zigttp
last_updated: 2026-06-28
---

# zigttp Strategy

## Target problem

Solo developers increasingly let an AI write their serverless handlers but have no
way to know the generated code is correct or safe before it ships. Today's runtimes
run it fast and prove nothing, so "it worked when I tried it" is the only guarantee -
now for code the developer didn't even write - and proving correctness of generated
code normally costs more than writing it by hand.

## Our approach

Restrict the JavaScript surface on purpose (no classes, async, try/catch, regex,
`==`, ...) so that correctness and safety properties become decidable and provable by
the compiler in milliseconds, then put the AI agent *inside* that proof boundary: it
can only author code the compiler can prove, and the developer sees a verdict, not a
vibe. Constraint is what makes both the proofs cheap and the agent trustworthy. The
explicit non-goal is full Node/V8 language fidelity, which is exactly what makes these
proofs intractable elsewhere.

## Who it's for

**Primary:** Solo developer / indie builder shipping serverless or edge functions, with
no PM, QA, or SRE to vouch for a deploy. They're hiring zigttp to let the AI write the
handler and have the compiler vouch for it - shipping something they can trust in
production without a review process they don't have and without becoming a verification
expert themselves. Expert-first: the agent is the main interface, the compiler is the
proof underneath.

## Key metrics

- **Expert success rate** - share of `expert` sessions that end with a handler passing
  all proofs and the user keeping or deploying it. Measured from the session ledger /
  `.zigttp/proofs.jsonl`. The core "did the agent produce provable code" signal.
- **Round-trips to first green proof** - median agent iterations to a fully-proven
  handler. Measured from the session ledger. Lower is better; regresses if the agent
  degrades.
- **Proven-path ratio** - fraction of a shipped handler's response paths covered by a
  proof rather than `unproven`. Measured from the contract / proofs ledger. Measures the
  strength of the guarantee, not just that it ran.
- **Second-handler retention** (aspirational, no measurement home yet) - developers who
  author and ship a second proven handler within 14-30 days. A local OSS CLI doesn't
  phone home, so this has no honest signal today.
- **Cold-start p50** (guardrail) - kept as a performance floor, not a headline; lives in
  `zigttp-bench`.

## Tracks

### Proof engine

The analyzer and the proof mechanisms that make properties sound and fast: semantics
registry, contracts, spec-check/SMT, the restriction-to-proof mapping.

_Why it serves the approach:_ It is the trunk - cheap, sound proofs are what every other
part of the product leans on.

### Expert agent

The compiler-in-the-loop authoring experience: reliably producing provable code, cutting
round-trips, surfacing module signatures, veto and off-ramps.

_Why it serves the approach:_ It is the launch interface for the expert-first persona -
the agent is how a solo dev actually reaches a proven handler.

### Runtime & deploy footprint

Fast cold start, small self-contained binary, `deploy --local`, edge/Lambda/Workers
targets.

_Why it serves the approach:_ The execution floor plus the last mile - a proof is worth
nothing if the proven handler can't ship fast and small.

## Milestones

- **v0.1.0-beta** (undated) - first public release; expert-first authoring with
  compiler-proven handlers.

## Not working on

- Portable trust artifacts as a headline (signed proof receipts, `verify <url>`,
  `/.well-known/zigttp-attest`) - built, but deferred from this release; it serves the
  team/platform-owner need, not the launch persona.
- Hosted cloud deploy (`deploy --cloud` rejected; `studio` and `edge` compiled out).
- The team / platform-owner persona - whoever has to vouch that a deploy is safe on
  someone else's behalf.
- Full Node/V8 language fidelity - the permanent non-goal; it is what makes the proofs
  intractable.

## Marketing

**One-liner:** The AI writes your handler. The compiler proves it's safe.
