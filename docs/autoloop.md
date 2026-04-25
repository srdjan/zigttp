# Property-goal autoloop

The autoloop turns `zigts expert` into a property-convergence engine. You
state the properties you want proven on a handler; the compiler drives
the session until the properties flip green or a budget trips. The model
is not in the loop; the compiler is.

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
   intents; a plan carries the repair kind, a line/column target, and a
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

The v1 surface tracks the five flow-oriented goals the counterexample
solver models. The wider PropertiesSnapshot lattice (17 booleans) shows
up in `zigts_check` and contract proof output, but the autoloop only
accepts goals the witness and repair tools can actually drive.

- `no_secret_leakage`
- `no_credential_leakage`
- `input_validated`
- `pii_contained`
- `injection_safe`

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

`stalled` is the honest verdict here. The handler produces two
compile-time diagnostics on the same line (`unchecked_optional_use` for
the `env(...)` result, `secret_in_response` for the flow into the body).
`pi_repair_plan` emits a plan for each; `pi_apply_repair_plan` applies
them in order, and the first apply shifts the line numbers so the
second plan targets the wrong line. That's the v1 ordering limitation.
`redact_sensitive_sink` now emits an `insert_guard_before_line` edit
intent - the apply tool accepts it - but the demo still stalls on the
two-diagnostics-one-line pattern until repair-plan ordering learns to
cope with line-number shifts or to batch cooperative plans.

On a handler where only the flow diagnostic fires (for example, one
where the optional is narrowed explicitly before the sink), the same
redact intent drives the verdict to `achieved`.

The run mutates the handler file. Reset it before another attempt:

```
git checkout -- examples/autoloop/handler.ts
```

## Exit codes

- `0`: `achieved`
- `1`: any other verdict (`exhausted_iters`, `exhausted_time`,
  `stalled`, `regression_blocked`, `tool_failed`) or any unexpected
  error
- `2`: flag parse error

## What it does not do yet

- No session persistence of the autoloop run itself. `--no-session` is
  implicit; the chained `verified_patch` and `autoloop_outcome` events
  are emitted to the events log when an `events_path` is configured,
  but the autoloop CLI does not open one today.
- One plan per turn. The orchestrator stays compatible with the
  `apply_edit must be the only tool call` invariant elsewhere in the
  loop; batching is possible but would change the veto ordering.
- Multiple diagnostics on the same line break ordering: applying the
  first plan shifts line numbers and invalidates the second plan's
  target. Fixed when repair-plan ordering learns line-delta tracking,
  or when plans emit byte offsets instead of line/column targets.

## Regression guard

A patch that flips any previously-green boolean property to false is
rolled back. The orchestrator writes the pre-patch snapshot back to
disk, keeps the `verified_patch` event in the transcript as an audit
record of the attempt, appends a `system_note` explaining the revert,
and exits with verdict `regression_blocked` on the next finalize.
