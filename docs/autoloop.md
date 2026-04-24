# Property-goal autoloop

The autoloop turns `zigts expert` into a property-convergence engine. You
state the properties you want proven on a handler; the compiler drives
the session until the properties flip green or a budget trips. The model
is not in the loop — the compiler is.

## Invocation

```
zigts expert --handler <path> --goal <csv> [--max-iters <n>]
```

`--goal` takes a comma-separated list of property tags. Entries are
validated at parse time against the `PropertiesSnapshot` field set, so a
typo like `retry_saff` exits immediately rather than burning the full
iteration budget. `--handler` is required whenever `--goal` is set.
`--max-iters` overrides the default of 8.

The flag is mutually exclusive with `--print` and `--mode rpc`.

## What a run does

One iteration is three tool calls:

1. `pi_goal_check` loads the handler, runs the flow checker, and
   synthesises executable counterexample witnesses for every requested
   goal that fails. If no goals fail, the verdict is `achieved` and the
   run exits 0.
2. `pi_repair_plan` reads the same diagnostics and emits typed edit
   intents — a plan carries the repair kind, a line/column target, and a
   template.
3. For each plan, `pi_apply_repair_plan` dry-runs the candidate edit and
   runs the compiler against the proposed content. When the candidate
   verifies, the autoloop writes the file and emits a chained
   `verified_patch` event (parent hash lookup via `session_state`
   carries the chain forward).

Iteration stops when the goals are met, the iteration budget is
exhausted, the wall clock runs out (2 minutes by default), or three
patches land without flipping any new goal green (`stalled`).

Every exit path emits a durable `autoloop_outcome` event with the
verdict, iteration count, per-goal met/unmet lists, and the final patch
hash when one exists.

## Supported goals

The v1 surface checks the flow-oriented goals the counterexample solver
models:

- `no_secret_leakage`
- `no_credential_leakage`
- `input_validated`
- `pii_contained`
- `injection_safe`

The full property lattice (17 booleans on `HandlerProperties`) is
validated at parse time — you can also pass `retry_safe`, `pure`,
`idempotent`, `state_isolated`, `fault_covered`, and the rest. These
are evaluated but have no counterexample witness today, so a run
whose goals include them relies on the handler contract reporting
`true` rather than on witness-driven repair.

## Smoke example

An intentionally insecure handler lives at `examples/autoloop/handler.ts`:

```ts
import { env } from "zigttp:env";

function handler(req: Request): Response {
    const secret = env("SECRET_KEY");
    return Response.json({ secret: secret });
}
```

Run:

```
zigts expert --handler examples/autoloop/handler.ts \
             --goal no_secret_leakage --max-iters 4
```

Sample output:

```
autoloop verdict: stalled
iterations: 3
goals:
  [ ] no_secret_leakage
final_patch_hash: f70c608c...
```

`stalled` is the honest verdict here: `pi_apply_repair_plan` v1 only
supports deterministic line insertions (`insert_guard_before_line`,
`add_trailing_return`). Secret redaction is a mutation rather than an
insertion, so the autoloop reaches its patches-without-progress ceiling
and exits. When `redact_sensitive_sink` lands as an insertion-friendly
intent, the same handler will converge to `achieved`.

The run mutates the handler file. Reset it before another attempt:

```
git checkout -- examples/autoloop/handler.ts
```

## Exit codes

- `0` — `achieved`
- `1` — any other verdict (`exhausted_iters`, `exhausted_time`,
  `stalled`, `regression_blocked`, `tool_failed`) or any unexpected
  error
- `2` — flag parse error

## What it does not do yet

- No block-and-revert on property regression. A patch that closes one
  witness while regressing a different property still lands; the
  `patches_without_progress` counter catches pathological loops but
  cannot roll back.
- No session persistence of the autoloop run. `--no-session` is
  implicit; the TUI ledger view (coming) will be the durable surface.
- The orchestrator drives one plan per turn to stay compatible with the
  `apply_edit must be the only tool call` invariant elsewhere in the
  loop. Batching is possible but would change the veto ordering.
