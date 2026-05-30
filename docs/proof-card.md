# Reading the Proof Card

Every time you save a handler, zigttp re-runs its analyzers and prints a proof
card: a verdict, the proven surface, and - when something fails - the exact
construct that broke a proof. It is the screen you read on every save. This
guide explains how to read it.

The card comes from one analysis, shown in three places:

- `zigttp dev` redraws it on every save, above the live-reload HUD, and
  `zigttp test` prints it as the pre-test check.
- `zigttp check` prints it once to the terminal for a one-shot run.
- `zigttp studio` mirrors it in the browser workbench.

The in-browser playground on the zigttp website runs the same analyzer
compiled to WebAssembly, so the card there matches the card you get locally.

## The Stages

The top of the card is a sequence of pass/fail gates. Each must pass before
the next runs:

- `Parse` - the handler is valid ZigTS syntax.
- `Types` - TypeScript annotations check (TypeScript handlers only).
- `Sound mode` - type-directed analysis across operators: no non-numeric
  arithmetic, no mixed-type `+`, no tautological comparisons.
- `Strict ZigTS` - the strict profile, on by default.

A `FAIL` here names the error count. The diagnostics print above the card
with a ZTS code, a source location, and a suggested fix. Fix the first one
and re-save; later stages often clear on their own.

## The Proven Surface

Below the stages, the card lists the properties the compiler proved. Each is
a pill: `[+]` proven, `[-]` not proven. They are grouped.

**Verification** - structural guarantees about control flow:

| Chip | Proven when |
|---|---|
| `exhaustive_returns` | every path returns a Response |
| `results_safe` | every `Result.ok` is guarded before the value is used |
| `optionals_safe` | every optional is narrowed before use |
| `state_isolated` | no module-scope variable is mutated in the handler body |
| `no_unreachable` | the handler has no dead code |

**Properties** - behavioral guarantees:

| Chip | Proven when |
|---|---|
| `pure` | the handler calls no virtual modules: it is a pure function of the request |
| `read_only` | every virtual-module call is read-classified: no state is mutated |
| `deterministic` | no `Date.now()` or `Math.random()` on any path: every run is identical |
| `retry_safe` | read-only, or every write sits inside a durable step |
| `idempotent` | deterministic and retry-safe: safe under at-least-once delivery |

**Security** - data-flow guarantees:

| Chip | Proven when |
|---|---|
| `injection_safe` | no unvalidated user input reaches a SQL or HTML sink |
| `no_secret_leakage` | no secret-labelled value reaches a response, header, or egress call |
| `no_credential_leak` | no credential-labelled value reaches a response body or log |
| `input_validated` | all user input passes a validation step before any egress call |

A `[-]` pill is not an error. It means the compiler could not prove that
property for this handler, often because the handler legitimately does the
thing the property forbids. A handler that writes to a cache will not be
`read_only`, and that is correct.

## Reading A Failing Chip

When a save demotes a property that was proven on the previous save, the card
adds a `Why:` row naming the regression:

```
Why: -deterministic at src/handler.ts:14: Date.now()
```

It reads as: the `deterministic` property was lost (`-`), the construct that
broke it is at `src/handler.ts:14`, and that construct is `Date.now()`. Remove
or relocate it and the property returns on the next save.

## Counterexamples

For properties backed by path or flow analysis, a `[-]` comes with a
counterexample: the concrete input that breaks the proof. It takes one of two
shapes.

An **offending node** is a single source location: the line and column, a
snippet of the breaking construct, and a one-line fix. This is what you get
for `deterministic` and similar path properties.

A **flow chain** is an ordered walk of a tainted value from the call that
produced it to the sink that leaked it, plus the request method and URL that
drives that path. This is what you get for the security properties
(`no_secret_leakage`, `injection_safe`, and the rest).

The live HUD renders a full Counterexample block when a property regresses;
see [Counterexamples](counterexamples.md) for that block in detail.

## The Proof Trace

Each property carries a proof trace: not just whether it holds, but how the
compiler decided. Expand a chip in the playground or Studio, or read the
auto-rendered block in the terminal HUD, to see:

- A `summary` in one plain sentence: what was checked and what was found.
- The `kind` of proof. `structural` is a single-pass scan of the IR.
  `path-enumeration` is exhaustive symbolic enumeration of execution paths.
  `flow-trace` is a source-to-sink data-flow taint trace.
- The `counterexample`, when the property does not hold.
- The `resisted` evidence, when a data-flow property *does* hold. This is the
  green-proof mirror of the counterexample: a representative attack the prover
  considered (`attackInput`), the source -> guard -> sink `chain` that defeats
  it, and a one-line `conclusion`. For example, a held `injection_safe` shows
  the SQL/HTML payload it tried, the validator that cleared it (e.g.
  `escapeHtml()`), and the sink it safely reached. A contained secret shows
  that it was read but never reached a response, log, or egress sink. The
  evidence is derived from what the prover observed, never invented: a named
  guard appears only when the validator binding is recovered from the source.

The trace is the same data on every surface. `zigttp check --json` emits it as
a `proof.proofTrace` object keyed by property name, so agents and CI read the
exact reasoning the HUD shows.

## The Three Lenses

In the `zigttp dev` HUD, press `Tab` to rotate the card's left pane through
three lenses:

- `Properties` - the default `[+]`/`[-]` pills.
- `Trade` - each proof paired with the substrate restriction that earned it:
  which rejected JavaScript feature bought which guarantee.
- `Handover` - a copy-pasteable proof certificate for an AI agent or a
  reviewer.

Studio mirrors the same three views with a tab bar. The rotation is described
in the [User Guide](user-guide.md); the restriction-to-proof map it draws on
is [Restrictions to Proofs](restrictions-to-proofs.md).
